;;; copilot-org-process.el --- AI-powered org inbox processor -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edd Wilder-James
;; Author: Edd Wilder-James <https://github.com/ewilderj>
;; Keywords: tools, ai, org

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; AI-assisted GTD inbox processor.  Point it at any org buffer and it
;; walks through top-level entries, using Copilot to classify, enrich,
;; tag, and suggest refiling.
;;
;; Usage:
;;   Open an org buffer with rough notes, then:
;;   M-x copilot-org-process
;;
;; In the review buffer:
;;   a  accept suggestion (applies changes to the org entry)
;;   e  edit the suggestion before applying
;;   s  skip this entry
;;   r  regenerate suggestion
;;   q  quit processing
;;
;; The command operates on whatever org buffer you invoke it from.
;; It does NOT modify entries until you accept.

;;; Code:

(require 'copilot-sdk)
(require 'org)
(require 'org-element)
(require 'seq)

(defcustom copilot-org-process-model "claude-sonnet-4-5"
  "Model to use for inbox processing."
  :type 'string
  :group 'copilot-org-process)

(defcustom copilot-org-process-refile-targets nil
  "List of refile target descriptions for the AI.
Each entry is a string like \"~/org/work.org :: Projects\".
If nil, the AI will suggest but not refile."
  :type '(repeat string)
  :group 'copilot-org-process)

(defvar copilot-org-process--session-id nil
  "Session for the current processing run.")

(defvar copilot-org-process--source-buffer nil
  "The org buffer being processed.")

(defvar copilot-org-process--entries nil
  "List of (marker . raw-text) for entries to process.")

(defvar copilot-org-process--current-index 0
  "Index of the current entry being reviewed.")

(defvar copilot-org-process--current-suggestion nil
  "Plist of the current AI suggestion.")

(defconst copilot-org-process--system-prompt
  "You are an org-mode inbox processor helping with a GTD workflow.

For each inbox entry, analyze it and respond with EXACTLY this format
(no markdown fences, no extra text):

HEADING: <cleaned up heading, imperative if TODO>
TYPE: <one of: TODO, REFERENCE, EVENT, PROJECT, SOMEDAY>
TAGS: <comma-separated tags, e.g. @person, topic, context>
PRIORITY: <A, B, C, or empty>
SCHEDULED: <YYYY-MM-DD if time-sensitive, or empty>
DEADLINE: <YYYY-MM-DD if has a hard deadline, or empty>
REFILE: <suggested category/file, or KEEP>
NOTES: <optional one-line note about why you classified it this way>

Rules:
- Clean up grammar and spelling in the heading
- Use imperative mood for TODOs (\"Fix\" not \"fix the\")
- Add @ prefix for person tags
- Only suggest SCHEDULED/DEADLINE if the text implies urgency
- REFERENCE is for notes/links to file away
- SOMEDAY is for ideas without immediate action
- Be concise"
  "System prompt for inbox processing.")

;;;; --- Entry Extraction ---

(defun copilot-org-process--collect-entries ()
  "Collect top-level org entries from the source buffer.
Return a list of (MARKER . RAW-TEXT) cons cells."
  (with-current-buffer copilot-org-process--source-buffer
    (let ((entries nil))
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward "^\\*\\s-" nil t)
         (let* ((beg (line-beginning-position))
                (marker (copy-marker beg))
                (end (line-end-position))
                (text (buffer-substring-no-properties beg end)))
           (push (cons marker text) entries))))
      (nreverse entries))))

(defun copilot-org-process--entry-heading (entry-text)
  "Extract the heading line from ENTRY-TEXT."
  (car (split-string entry-text "\n")))

;;;; --- AI Interaction ---

(defun copilot-org-process--ensure-session ()
  "Get or create the processing session."
  (copilot-sdk-ensure-connected)
  (unless (and copilot-org-process--session-id
               (condition-case nil
                   (copilot-sdk-resume-session
                    copilot-org-process--session-id)
                 (error nil)))
    (let ((refile-info
           (if copilot-org-process-refile-targets
               (format "\n\nAvailable refile targets:\n%s"
                       (mapconcat (lambda (x) (concat "- " x))
                                  copilot-org-process-refile-targets "\n"))
             "")))
      (setq copilot-org-process--session-id
            (copilot-sdk-create-session
             :model copilot-org-process-model
             :system-message
             (concat copilot-org-process--system-prompt refile-info)))))
  copilot-org-process--session-id)

(defun copilot-org-process--parse-suggestion (response)
  "Parse the AI RESPONSE into a suggestion plist."
  (let ((result nil))
    (dolist (line (split-string response "\n"))
      (when (string-match "^\\([A-Z]+\\):\\s-*\\(.*\\)" line)
        (let ((key (match-string 1 line))
              (val (string-trim (match-string 2 line))))
          (unless (string-empty-p val)
            (pcase key
              ("HEADING"   (setq result (plist-put result :heading val)))
              ("TYPE"      (setq result (plist-put result :type val)))
              ("TAGS"      (setq result (plist-put result :tags val)))
              ("PRIORITY"  (setq result (plist-put result :priority val)))
              ("SCHEDULED" (setq result (plist-put result :scheduled val)))
              ("DEADLINE"  (setq result (plist-put result :deadline val)))
              ("REFILE"    (setq result (plist-put result :refile val)))
              ("NOTES"     (setq result (plist-put result :notes val))))))))
    result))

(defun copilot-org-process--get-suggestion (entry-text)
  "Get an AI suggestion for ENTRY-TEXT.  Return a parsed plist."
  (let* ((session (copilot-org-process--ensure-session))
         (prompt (format "Process this inbox entry:\n\n%s" entry-text))
         (response (copilot-sdk-send-and-wait session prompt 30)))
    (when response
      (copilot-org-process--parse-suggestion response))))

;;;; --- Applying Suggestions ---

(defun copilot-org-process--apply-suggestion (marker suggestion)
  "Apply SUGGESTION to the org entry at MARKER."
  (with-current-buffer (marker-buffer marker)
    (org-with-wide-buffer
     (goto-char marker)
     (when (org-at-heading-p)
       ;; Update heading
       (let* ((type (plist-get suggestion :type))
              (heading (plist-get suggestion :heading))
              (priority (plist-get suggestion :priority))
              (tags-str (plist-get suggestion :tags))
              (scheduled (plist-get suggestion :scheduled))
              (deadline (plist-get suggestion :deadline))
              (todo-kw (pcase type
                         ("TODO" "TODO")
                         ("EVENT" "TODO")
                         ("PROJECT" "TODO")
                         ("SOMEDAY" "TODO")
                         ("REFERENCE" nil)
                         (_ nil)))
              (extra-tags (when (string= type "SOMEDAY") "someday")))
         ;; Set the heading text
         (org-edit-headline heading)
         ;; Set TODO keyword
         (when todo-kw
           (org-todo todo-kw))
         ;; Set priority
         (when (and priority (not (string-empty-p priority)))
           (let ((pri-char (string-to-char priority)))
             (when (and (>= pri-char ?A) (<= pri-char ?C))
               (org-priority pri-char))))
         ;; Set tags
         (when (or tags-str extra-tags)
           (let ((tags (when (and tags-str (not (string-empty-p tags-str)))
                         (mapcar #'string-trim
                                 (split-string tags-str ",")))))
             (when extra-tags
               (push extra-tags tags))
             (org-set-tags (mapcar (lambda (tag)
                                    (replace-regexp-in-string
                                     "\\s-" "_" tag))
                                  tags))))
         ;; Set scheduled
         (when (and scheduled
                    (not (string-empty-p scheduled))
                    (not (string= scheduled "empty")))
           (org-schedule nil scheduled))
         ;; Set deadline
         (when (and deadline
                    (not (string-empty-p deadline))
                    (not (string= deadline "empty")))
           (org-deadline nil deadline)))))))

;;;; --- Review Buffer UI ---

(defvar copilot-org-process-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'copilot-org-process-accept)
    (define-key map (kbd "e") #'copilot-org-process-edit-and-accept)
    (define-key map (kbd "s") #'copilot-org-process-skip)
    (define-key map (kbd "r") #'copilot-org-process-regenerate)
    (define-key map (kbd "q") #'copilot-org-process-quit)
    (define-key map (kbd "n") #'copilot-org-process-skip)
    map)
  "Keymap for `copilot-org-process-mode'.")

(define-derived-mode copilot-org-process-mode special-mode "Org-Process"
  "Mode for reviewing AI suggestions on org inbox entries."
  (setq-local header-line-format
              "[a]ccept [e]dit [s]kip [r]egenerate [q]uit"))

(defun copilot-org-process--render ()
  "Render the current entry and suggestion in the review buffer."
  (let* ((buf (get-buffer-create "*org-process*"))
         (entry (nth copilot-org-process--current-index
                     copilot-org-process--entries))
         (entry-text (cdr entry))
         (suggestion copilot-org-process--current-suggestion)
         (total (length copilot-org-process--entries))
         (idx (1+ copilot-org-process--current-index)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (copilot-org-process-mode)
        ;; Title
        (insert (propertize (format "── Inbox item %d/%d " idx total)
                            'face 'bold))
        (insert (propertize
                 (format "(%s)"
                         (buffer-name copilot-org-process--source-buffer))
                 'face 'shadow))
        (insert "\n\n")
        ;; Original
        (insert (propertize "Original:\n" 'face 'font-lock-comment-face))
        (insert (propertize (string-trim entry-text)
                            'face 'shadow))
        (insert "\n\n")
        ;; Suggestion
        (if suggestion
            (progn
              (insert (propertize "Suggestion:\n"
                                  'face 'font-lock-comment-face))
              (let ((type (or (plist-get suggestion :type) ""))
                    (heading (or (plist-get suggestion :heading) ""))
                    (tags (plist-get suggestion :tags))
                    (priority (plist-get suggestion :priority))
                    (scheduled (plist-get suggestion :scheduled))
                    (deadline (plist-get suggestion :deadline))
                    (refile (plist-get suggestion :refile))
                    (notes (plist-get suggestion :notes)))
                ;; Show the reconstructed heading
                (insert "  ")
                (when (and type (member type '("TODO" "EVENT" "PROJECT")))
                  (insert (propertize (concat type " ")
                                      'face 'org-todo)))
                (when (and type (string= type "SOMEDAY"))
                  (insert (propertize "SOMEDAY "
                                      'face 'org-agenda-dimmed-todo-face)))
                (when (and priority (not (string-empty-p priority)))
                  (insert (propertize (format "[#%s] " priority)
                                      'face 'org-priority)))
                (insert (propertize heading 'face 'org-level-1))
                (when (and tags (not (string-empty-p tags)))
                  (insert (propertize
                           (concat "  :"
                                   (mapconcat
                                    #'string-trim
                                    (split-string tags ",") ":")
                                   ":")
                           'face 'org-tag)))
                (insert "\n")
                ;; Dates
                (when (and scheduled
                           (not (string-empty-p scheduled))
                           (not (string= scheduled "empty")))
                  (insert (propertize
                           (format "  SCHEDULED: <%s>\n" scheduled)
                           'face 'org-scheduled))
                  )
                (when (and deadline
                           (not (string-empty-p deadline))
                           (not (string= deadline "empty")))
                  (insert (propertize
                           (format "  DEADLINE: <%s>\n" deadline)
                           'face 'org-warning)))
                ;; Refile
                (when (and refile
                           (not (string-empty-p refile))
                           (not (string= refile "KEEP")))
                  (insert (propertize
                           (format "  Refile → %s\n" refile)
                           'face 'font-lock-function-name-face)))
                ;; Notes
                (when (and notes (not (string-empty-p notes)))
                  (insert "\n"
                          (propertize (concat "  ℹ " notes "\n")
                                      'face 'font-lock-doc-face)))))
          ;; No suggestion yet
          (insert (propertize "Analyzing...\n"
                              'face 'font-lock-comment-face)))
        (goto-char (point-min))))
    (pop-to-buffer buf)))

(defun copilot-org-process--process-current ()
  "Get AI suggestion for the current entry and render it."
  (let* ((entry (nth copilot-org-process--current-index
                     copilot-org-process--entries))
         (entry-text (cdr entry)))
    (setq copilot-org-process--current-suggestion nil)
    (copilot-org-process--render)
    (let ((suggestion (copilot-org-process--get-suggestion entry-text)))
      (setq copilot-org-process--current-suggestion suggestion)
      (copilot-org-process--render))))

(defun copilot-org-process--advance ()
  "Move to the next entry, or finish if done."
  (cl-incf copilot-org-process--current-index)
  (if (>= copilot-org-process--current-index
          (length copilot-org-process--entries))
      (progn
        (message "All inbox entries processed!")
        (when (get-buffer "*org-process*")
          (kill-buffer "*org-process*")))
    (copilot-org-process--process-current)))

(defun copilot-org-process-accept ()
  "Accept the current suggestion and apply it."
  (interactive)
  (let ((entry (nth copilot-org-process--current-index
                    copilot-org-process--entries))
        (suggestion copilot-org-process--current-suggestion))
    (unless suggestion (user-error "No suggestion to accept"))
    (copilot-org-process--apply-suggestion (car entry) suggestion)
    (message "Applied: %s" (plist-get suggestion :heading))
    (copilot-org-process--advance)))

(defun copilot-org-process-edit-and-accept ()
  "Edit the suggested heading before accepting."
  (interactive)
  (let ((suggestion copilot-org-process--current-suggestion))
    (unless suggestion (user-error "No suggestion to edit"))
    (let ((new-heading (read-string "Heading: "
                                    (plist-get suggestion :heading))))
      (setq copilot-org-process--current-suggestion
            (plist-put suggestion :heading new-heading))
      (copilot-org-process-accept))))

(defun copilot-org-process-skip ()
  "Skip the current entry without changes."
  (interactive)
  (message "Skipped.")
  (copilot-org-process--advance))

(defun copilot-org-process-regenerate ()
  "Regenerate the suggestion for the current entry."
  (interactive)
  (message "Regenerating...")
  (copilot-org-process--process-current))

(defun copilot-org-process-quit ()
  "Quit processing."
  (interactive)
  (when copilot-org-process--session-id
    (ignore-errors
      (copilot-sdk-destroy-session copilot-org-process--session-id))
    (setq copilot-org-process--session-id nil))
  (kill-buffer)
  (message "Inbox processing stopped."))

;;;; --- Entry Point ---

;;;###autoload
(defun copilot-org-process ()
  "Process the current org buffer as an inbox.
Walk through each top-level entry and get AI suggestions for
classification, tagging, scheduling, and refiling."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (setq copilot-org-process--source-buffer (current-buffer))
  (setq copilot-org-process--entries
        (copilot-org-process--collect-entries))
  (when (null copilot-org-process--entries)
    (user-error "No entries found in buffer"))
  (setq copilot-org-process--current-index 0)
  (setq copilot-org-process--session-id nil)
  (message "Found %d entries. Processing..."
           (length copilot-org-process--entries))
  (copilot-org-process--process-current))

(provide 'copilot-org-process)
;;; copilot-org-process.el ends here

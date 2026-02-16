;;; copilot-tool-agent.el --- Tool-using agent with permission model -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edd Wilder-James
;; Author: Edd Wilder-James <https://github.com/ewilderj>
;; Keywords: tools, ai

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Demonstrates tool registration, dispatch, and a tiered permission
;; model.  The agent has tools for reading files, listing directories,
;; and getting the current time.
;;
;; Each tool has a risk level:
;;   :safe     — runs without asking (e.g. current time)
;;   :ask      — prompts the user: [y]es this turn, [a]lways, [n]o
;;
;; "Yes" grants approval for the remainder of the current turn,
;; so the user isn't re-prompted when a tool is called repeatedly.
;; "Always" grants a session-scoped blanket approval per tool.
;;
;; Usage:
;;   (require 'copilot-tool-agent)
;;   M-x copilot-tool-agent

;;; Code:

(require 'copilot-sdk)

(defgroup copilot-tool-agent nil
  "Tool-using agent with permission model."
  :group 'copilot
  :prefix "copilot-tool-agent-")

(defcustom copilot-tool-agent-model "claude-sonnet-4-5"
  "Model to use for the tool agent."
  :type 'string
  :group 'copilot-tool-agent)

;;;; --- Permission Model ---

(defvar copilot-tool-agent--always-allowed nil
  "List of tool names that have been granted session-wide approval.")

(defvar copilot-tool-agent--turn-allowed nil
  "List of tool names approved for the current turn.")

(defvar copilot-tool-agent--prompting nil
  "Non-nil while a permission prompt is active.
Prevents nested prompts from `read-char-choice' re-entering
the process filter.")

(defvar copilot-tool-agent--tool-risk (make-hash-table :test #'equal)
  "Map of tool-name → risk level (:safe or :ask).")

(defun copilot-tool-agent--check-permission (tool-name)
  "Check if TOOL-NAME is allowed to run.
Return non-nil if approved.  For :ask tools, prompt the user."
  (let ((risk (gethash tool-name copilot-tool-agent--tool-risk)))
    (cond
     ((eq risk :safe) t)
     ((member tool-name copilot-tool-agent--always-allowed) t)
     ((member tool-name copilot-tool-agent--turn-allowed) t)
     ((not risk) nil)
     ;; If we're already prompting, auto-approve (same turn)
     (copilot-tool-agent--prompting
      (push tool-name copilot-tool-agent--turn-allowed)
      t)
     (t
      (unwind-protect
          (progn
            (setq copilot-tool-agent--prompting t)
            (let ((answer (read-char-choice
                           (format "'%s': [y]es (this turn), [a]lways, [n]o? "
                                   tool-name)
                           '(?y ?a ?n))))
              (pcase answer
                (?a (push tool-name copilot-tool-agent--always-allowed) t)
                (?y (push tool-name copilot-tool-agent--turn-allowed) t)
                (?n nil))))
        (setq copilot-tool-agent--prompting nil))))))

(defun copilot-tool-agent--define (name description risk handler &optional schema)
  "Define a tool with NAME, DESCRIPTION, RISK level, HANDLER, and optional SCHEMA.
RISK is :safe or :ask.  The handler is wrapped with permission checking."
  (puthash name risk copilot-tool-agent--tool-risk)
  (copilot-sdk-define-tool
   name description
   (lambda (args)
     (if (copilot-tool-agent--check-permission name)
         (funcall handler args)
       `(:textResultForLlm
         ,(format "Permission denied by user for tool '%s'" name)
         :resultType "rejected")))
   schema))

;;;; --- Tool Implementations ---

(defun copilot-tool-agent--tool-current-time (_args)
  "Return the current date and time."
  `(:textResultForLlm
    ,(format-time-string "%A, %B %d, %Y at %I:%M %p %Z")
    :resultType "success"))

(defun copilot-tool-agent--tool-read-file (args)
  "Read a file's contents.  ARGS: path."
  (let ((path (plist-get args :path)))
    (if (not (file-exists-p path))
        `(:textResultForLlm ,(format "File not found: %s" path)
          :resultType "failure")
      (let* ((content (with-temp-buffer
                        (insert-file-contents path)
                        (buffer-string)))
             (truncated (if (> (length content) 10000)
                            (concat (substring content 0 10000)
                                    "\n\n[truncated]")
                          content)))
        `(:textResultForLlm ,truncated :resultType "success")))))

(defun copilot-tool-agent--tool-list-directory (args)
  "List files in a directory.  ARGS: path, pattern."
  (let* ((dir (or (plist-get args :path) default-directory))
         (pattern (or (plist-get args :pattern) "*"))
         (expanded (expand-file-name dir)))
    (if (not (file-directory-p expanded))
        `(:textResultForLlm ,(format "Not a directory: %s" dir)
          :resultType "failure")
      (let ((files (directory-files expanded nil pattern)))
        `(:textResultForLlm
          ,(mapconcat #'identity files "\n")
          :resultType "success")))))

(defun copilot-tool-agent--tool-file-info (args)
  "Get metadata about a file.  ARGS: path."
  (let ((path (plist-get args :path)))
    (if (not (file-exists-p path))
        `(:textResultForLlm ,(format "File not found: %s" path)
          :resultType "failure")
      (let ((attrs (file-attributes path)))
        `(:textResultForLlm
          ,(format "path: %s\nsize: %d bytes\nmodified: %s\ntype: %s"
                   path
                   (file-attribute-size attrs)
                   (format-time-string "%Y-%m-%d %H:%M:%S"
                                       (file-attribute-modification-time attrs))
                   (cond ((file-directory-p path) "directory")
                         ((file-symlink-p path) "symlink")
                         (t "file")))
          :resultType "success")))))

;;;; --- Tool Registration ---

(defun copilot-tool-agent--register-tools ()
  "Register all tools with their risk levels."
  (copilot-sdk-clear-tools)

  ;; Safe tools — no permission needed
  (copilot-tool-agent--define
   "get_current_time"
   "Get the current date and time."
   :safe
   #'copilot-tool-agent--tool-current-time)

  ;; Filesystem tools — require permission
  (copilot-tool-agent--define
   "read_file"
   "Read the contents of a file.  Returns the file text."
   :ask
   #'copilot-tool-agent--tool-read-file
   '(:type "object"
     :properties (:path (:type "string"
                         :description "Absolute or relative file path"))
     :required ["path"]))

  (copilot-tool-agent--define
   "list_directory"
   "List files in a directory, optionally filtered by pattern."
   :ask
   #'copilot-tool-agent--tool-list-directory
   '(:type "object"
     :properties (:path (:type "string"
                         :description "Directory path. Defaults to cwd.")
                  :pattern (:type "string"
                            :description "Glob pattern filter, e.g. *.el"))))

  (copilot-tool-agent--define
   "file_info"
   "Get metadata about a file: size, modification time, type."
   :ask
   #'copilot-tool-agent--tool-file-info
   '(:type "object"
     :properties (:path (:type "string"
                         :description "File path"))
     :required ["path"])))

;;;; --- Agent UI ---

(defvar copilot-tool-agent--session-id nil
  "Current agent session.")

(defvar-local copilot-tool-agent--input-start nil
  "Marker for start of user input area.")

(defvar copilot-tool-agent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'copilot-tool-agent-send)
    (define-key map (kbd "C-c C-k") #'copilot-tool-agent-quit)
    map)
  "Keymap for `copilot-tool-agent-mode'.")

(define-derived-mode copilot-tool-agent-mode text-mode "Tool-Agent"
  "Mode for the tool agent chat.
Inherits self-insert from `text-mode' so all keys type normally."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local header-line-format
              "Tool Agent.  RET send | C-c C-k quit"))

(defun copilot-tool-agent--insert-prompt ()
  "Insert a prompt and set up the input region."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (let ((prompt-start (point)))
      (insert "> ")
      (add-text-properties prompt-start (point)
                           '(read-only t rear-nonsticky t
                             font-lock-face font-lock-keyword-face
                             front-sticky (read-only))))
    (setq copilot-tool-agent--input-start (point-marker))
    (goto-char (point-max))))

(defun copilot-tool-agent--append (text &optional face)
  "Append TEXT at end of agent buffer with optional FACE."
  (let ((buf (get-buffer "*tool-agent*")))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (let ((start (point)))
            (insert text "\n")
            (when face
              (add-text-properties start (point)
                                   (list 'font-lock-face face)))))))))

(defun copilot-tool-agent-send ()
  "Send the current input to the agent."
  (interactive)
  (setq copilot-tool-agent--turn-allowed nil)
  (let ((input (buffer-substring-no-properties
                copilot-tool-agent--input-start (point-max))))
    (when (string-blank-p input)
      (user-error "Nothing to send"))
    (let ((inhibit-read-only t))
      ;; Remove prompt and input, re-insert as styled user text
      (delete-region (save-excursion
                       (goto-char copilot-tool-agent--input-start)
                       (beginning-of-line)
                       (point))
                     (point-max))
      (goto-char (point-max))
      (insert (propertize (concat "> " input "\n")
                          'font-lock-face 'bold
                          'read-only t 'front-sticky '(read-only))))
    ;; No prompt yet — it appears when the reply finishes
    (copilot-sdk-send copilot-tool-agent--session-id input
                      :on-message
                      (lambda (text)
                        (copilot-tool-agent--append text 'font-lock-doc-face))
                      :on-tool
                      (lambda (name)
                        (let ((risk (gethash name copilot-tool-agent--tool-risk)))
                          (when risk
                            (copilot-tool-agent--append
                             (format "🔧 %s %s" name
                                     (cond
                                      ((eq risk :safe) "(safe)")
                                      ((member name
                                               copilot-tool-agent--always-allowed)
                                       "(always allowed)")
                                      ((member name
                                               copilot-tool-agent--turn-allowed)
                                       "(approved)")
                                      (t "(asking...)")))
                             'font-lock-comment-face))))
                      :on-error
                      (lambda (msg)
                        (copilot-tool-agent--append
                         (concat "❌ " msg) 'error))
                      :on-idle
                      (lambda ()
                        (with-current-buffer (get-buffer "*tool-agent*")
                          (copilot-tool-agent--insert-prompt))))))

(defun copilot-tool-agent-quit ()
  "Quit the agent."
  (interactive)
  (when copilot-tool-agent--session-id
    (ignore-errors
      (copilot-sdk-destroy-session copilot-tool-agent--session-id))
    (setq copilot-tool-agent--session-id nil))
  (setq copilot-tool-agent--always-allowed nil)
  (setq copilot-tool-agent--turn-allowed nil)
  (setq copilot-tool-agent--prompting nil)
  (kill-buffer)
  (message "Tool agent stopped."))

;;;###autoload
(defun copilot-tool-agent ()
  "Start the tool agent."
  (interactive)
  (copilot-sdk-ensure-connected)
  (copilot-tool-agent--register-tools)
  (setq copilot-tool-agent--always-allowed nil)
  ;; Approve SDK-level permissions — our tool wrappers handle user consent
  (copilot-sdk-set-permission-handler
   (lambda (_kind _req) '(:kind "approved")))
  (let ((buf (get-buffer-create "*tool-agent*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (copilot-tool-agent-mode)
      (let ((inhibit-read-only t))
        (insert (propertize
                 (format "Tool Agent — %s\n" default-directory)
                 'face 'bold
                 'read-only t 'front-sticky '(read-only)))
        (insert (propertize
                 (concat "Tools: get_current_time (safe), "
                         "read_file (ask), list_directory (ask), "
                         "file_info (ask)\n\n")
                 'face 'shadow
                 'read-only t 'front-sticky '(read-only))))
      (copilot-tool-agent--insert-prompt))
    (setq copilot-tool-agent--session-id
          (copilot-sdk-create-session
           :model copilot-tool-agent-model
           :tools (copilot-sdk-tool-defs)
           :system-message
           (format
            "You are a helpful assistant with access to the local filesystem.
You can read files, list directories, get file metadata, and tell the time.
Use your tools to answer questions. Be concise.
The current working directory is: %s
When asked about 'files here' or 'current directory', use that path.
The user may deny permission for filesystem tools — if so, explain
what you tried and ask how they'd like to proceed."
            default-directory)
           :working-directory default-directory))
    (pop-to-buffer buf)))

(provide 'copilot-tool-agent)
;;; copilot-tool-agent.el ends here

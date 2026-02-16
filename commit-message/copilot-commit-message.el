;;; copilot-commit-message.el --- Generate commit messages with Copilot -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edd Wilder-James
;; Author: Edd Wilder-James <edd@me.com>
;; Keywords: tools, ai, vc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Reads `git diff --staged', asks Copilot to write a commit message,
;; opens a vc-style popup buffer for editing, then commits with C-c C-c.
;;
;; Usage:
;;   Stage your changes, then M-x copilot-commit
;;
;; In the commit buffer:
;;   C-c C-c  commit
;;   C-c C-k  cancel
;;   C-c C-r  regenerate message

;;; Code:

(require 'copilot-sdk)
(require 'seq)

(defcustom copilot-commit-model "claude-haiku-4.5"
  "Model to use for commit message generation."
  :type 'string
  :group 'copilot-commit)

(defvar copilot-commit--session-id nil
  "Reusable session for commit message generation.")

(defconst copilot-commit--system-prompt
  "You are a git commit message generator. Given a diff, write a commit
message following Conventional Commits format:

  type(scope): subject

  Optional body explaining what and why.

Rules:
- First line under 72 characters
- Use imperative mood (\"add\" not \"added\")
- Types: feat, fix, docs, refactor, test, chore, style, perf, ci, build
- Scope is optional but helpful
- Body is optional; use for non-obvious changes
- Output ONLY the commit message, no explanation, no markdown fences"
  "System prompt for commit message generation.")

(defun copilot-commit--ensure-session ()
  "Get or create a session for commit message generation."
  (copilot-sdk-ensure-connected)
  (unless (and copilot-commit--session-id
               (condition-case nil
                   (copilot-sdk-resume-session copilot-commit--session-id)
                 (error nil)))
    (setq copilot-commit--session-id
          (copilot-sdk-create-session
           :model copilot-commit-model
           :system-message copilot-commit--system-prompt)))
  copilot-commit--session-id)

(defun copilot-commit--staged-diff ()
  "Return the staged diff as a string, or nil if nothing staged."
  (let ((diff (with-temp-buffer
                (call-process "git" nil t nil "diff" "--staged")
                (buffer-string))))
    (if (string-empty-p (string-trim diff)) nil diff)))

(defun copilot-commit--staged-stat ()
  "Return a short stat summary of staged changes."
  (with-temp-buffer
    (call-process "git" nil t nil "diff" "--staged" "--stat")
    (buffer-string)))

(defun copilot-commit--generate-message (diff)
  "Generate a commit message for DIFF."
  (let* ((session (copilot-commit--ensure-session))
         (stat (copilot-commit--staged-stat))
         (diff-truncated (if (> (length diff) 12000)
                             (concat (substring diff 0 12000)
                                     "\n\n[diff truncated]")
                           diff))
         (prompt (format "Files changed:\n%s\nDiff:\n%s"
                         stat diff-truncated))
         (msg (copilot-sdk-send-and-wait session prompt 60)))
    (when msg (string-trim msg))))

;;;; --- Commit Buffer ---

(defvar copilot-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'copilot-commit-finish)
    (define-key map (kbd "C-c C-k") #'copilot-commit-cancel)
    (define-key map (kbd "C-c C-r") #'copilot-commit-regenerate)
    map)
  "Keymap for `copilot-commit-mode'.")

(define-derived-mode copilot-commit-mode text-mode "Copilot-Commit"
  "Mode for editing a Copilot-generated commit message.
\\<copilot-commit-mode-map>
\\[copilot-commit-finish] to commit, \\[copilot-commit-cancel] to abort, \
\\[copilot-commit-regenerate] to regenerate."
  (setq-local header-line-format
              (substitute-command-keys
               "Edit message.  \\[copilot-commit-finish] commit \
| \\[copilot-commit-cancel] cancel \
| \\[copilot-commit-regenerate] regenerate")))

(defvar-local copilot-commit--repo-dir nil
  "Repository root for the pending commit.")

(defvar-local copilot-commit--amend nil
  "Non-nil if this is an amend operation.")

(defun copilot-commit--setup-buffer (message-text stat repo-dir amend)
  "Populate commit buffer with MESSAGE-TEXT, STAT, REPO-DIR, AMEND flag."
  (let ((buf (get-buffer-create "*copilot-commit*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (copilot-commit-mode)
        (insert message-text)
        (insert "\n\n"
                (propertize
                 (concat "# ---\n# "
                         (replace-regexp-in-string
                          "\n" "\n# " (string-trim stat))
                         "\n# ---\n"
                         "# Lines starting with # are ignored.\n"
                         (if amend "# Amending last commit.\n" ""))
                 'face 'font-lock-comment-face))
        (setq copilot-commit--repo-dir repo-dir)
        (setq copilot-commit--amend amend)
        (goto-char (point-min))))
    (pop-to-buffer buf)))

(defun copilot-commit--extract-message ()
  "Extract the commit message, stripping comment lines."
  (let ((lines (split-string (buffer-string) "\n")))
    (string-trim
     (mapconcat #'identity
                (seq-remove (lambda (l) (string-prefix-p "#" l)) lines)
                "\n"))))

(defun copilot-commit-finish ()
  "Commit with the message in the buffer."
  (interactive)
  (let ((msg (copilot-commit--extract-message)))
    (when (string-empty-p msg)
      (user-error "Empty commit message, aborting"))
    (let* ((default-directory copilot-commit--repo-dir)
           (args (if copilot-commit--amend
                     (list "commit" "--amend" "-m" msg)
                   (list "commit" "-m" msg)))
           (exit-code (apply #'call-process "git" nil nil nil args)))
      (if (= exit-code 0)
          (progn
            (kill-buffer)
            (message "%s: %s"
                     (if copilot-commit--amend "Amended" "Committed")
                     (car (split-string msg "\n"))))
        (user-error "git commit failed (exit %d)" exit-code)))))

(defun copilot-commit-cancel ()
  "Cancel the commit."
  (interactive)
  (kill-buffer)
  (message "Commit cancelled."))

(defun copilot-commit-regenerate ()
  "Regenerate the commit message."
  (interactive)
  (let* ((default-directory copilot-commit--repo-dir)
         (diff (copilot-commit--staged-diff)))
    (unless diff (user-error "Nothing staged"))
    (message "Regenerating...")
    (let ((msg (copilot-commit--generate-message diff)))
      (unless msg (user-error "No response from Copilot"))
      (copilot-commit--setup-buffer msg (copilot-commit--staged-stat)
                                    copilot-commit--repo-dir
                                    copilot-commit--amend))))

;;;###autoload
(defun copilot-commit ()
  "Generate a commit message for staged changes in a popup buffer."
  (interactive)
  (let ((default-directory (or (vc-root-dir) default-directory)))
    (let ((diff (copilot-commit--staged-diff)))
      (unless diff
        (user-error "Nothing staged.  Stage changes first with `git add'"))
      (message "Generating commit message...")
      (let ((msg (copilot-commit--generate-message diff)))
        (unless msg (user-error "No response from Copilot"))
        (copilot-commit--setup-buffer msg (copilot-commit--staged-stat)
                                      default-directory nil)))))

;;;###autoload
(defun copilot-commit-amend ()
  "Generate a new commit message and amend the last commit."
  (interactive)
  (let* ((default-directory (or (vc-root-dir) default-directory))
         (diff (with-temp-buffer
                 (call-process "git" nil t nil "diff" "HEAD~1" "--staged")
                 (buffer-string))))
    (when (string-empty-p (string-trim diff))
      (setq diff (with-temp-buffer
                   (call-process "git" nil t nil "show" "--format=" "HEAD")
                   (buffer-string))))
    (message "Generating commit message...")
    (let ((msg (copilot-commit--generate-message diff)))
      (unless msg (user-error "No response from Copilot"))
      (copilot-commit--setup-buffer msg (copilot-commit--staged-stat)
                                    default-directory t))))

(provide 'copilot-commit-message)
;;; copilot-commit-message.el ends here

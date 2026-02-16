;;; copilot-docstring.el --- Generate docstrings with Copilot -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edd Wilder-James
;; Author: Edd Wilder-James <edd@me.com>
;; Keywords: tools, ai

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Single command: M-x copilot-docstring generates a docstring for the
;; function at point and inserts it.  No session persistence, no chat
;; buffer — just a single send-and-wait call.
;;
;; Supports Emacs Lisp, Python, JavaScript/TypeScript, Go, Rust, and
;; other languages.  The model infers the right docstring format from
;; the code and major mode.
;;
;; Usage:
;;   Place point inside a function, then M-x copilot-docstring

;;; Code:

(require 'copilot-sdk)

(defcustom copilot-docstring-model "claude-haiku-4.5"
  "Model to use for docstring generation."
  :type 'string
  :group 'copilot-docstring)

(defvar copilot-docstring--session-id nil
  "Reusable session for docstring generation.")

(defconst copilot-docstring--system-prompt
  "You are a docstring generator. Given a function, write ONLY the
docstring — no explanation, no code, no markdown fences.

For Emacs Lisp: return the docstring text (no quotes).
For Python: return the docstring including triple quotes.
For JS/TS: return a JSDoc comment block.
For Go: return a godoc comment.
For other languages: use the idiomatic format.

Be concise but complete. Document parameters and return values."
  "System prompt for docstring generation.")

(defun copilot-docstring--ensure-session ()
  "Get or create a session for docstring generation."
  (copilot-sdk-ensure-connected)
  (unless (and copilot-docstring--session-id
               (condition-case nil
                   (copilot-sdk-resume-session copilot-docstring--session-id)
                 (error nil)))
    (setq copilot-docstring--session-id
          (copilot-sdk-create-session
           :model copilot-docstring-model
           :system-message copilot-docstring--system-prompt)))
  copilot-docstring--session-id)

(defun copilot-docstring--extract-function ()
  "Extract the function at point as a string and its bounds.
Return (CODE . START-POS) or signal an error."
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (unless bounds
      (user-error "No function at point"))
    (cons (buffer-substring-no-properties (car bounds) (cdr bounds))
          (car bounds))))

(defun copilot-docstring--insert-elisp (docstring defun-start)
  "Insert DOCSTRING into the elisp defun starting at DEFUN-START."
  (save-excursion
    (goto-char defun-start)
    ;; Move past (defun name args
    (down-list)
    (forward-sexp 3)
    (let ((existing-doc (looking-at-p "\\s-*\"")))
      (when existing-doc
        ;; Remove existing docstring
        (skip-chars-forward " \t\n")
        (let ((doc-start (point)))
          (forward-sexp)
          (delete-region doc-start (point))))
      (insert "\n  \"" (replace-regexp-in-string "\"" "\\\\\"" docstring) "\""))))

(defun copilot-docstring--insert-generic (docstring defun-start)
  "Insert DOCSTRING after the first line of the function at DEFUN-START."
  (save-excursion
    (goto-char defun-start)
    (end-of-line)
    ;; For languages with opening braces, go past the brace
    (when (re-search-forward "{\\|:" (line-end-position 3) t)
      (goto-char (match-end 0)))
    (insert "\n" docstring)))

;;;###autoload
(defun copilot-docstring ()
  "Generate and insert a docstring for the function at point."
  (interactive)
  (let* ((func-data (copilot-docstring--extract-function))
         (code (car func-data))
         (start-pos (cdr func-data))
         (mode (symbol-name major-mode))
         (session (copilot-docstring--ensure-session))
         (prompt (format "Language/mode: %s\n\nFunction:\n%s" mode code))
         (docstring (copilot-sdk-send-and-wait session prompt 30)))
    (unless docstring
      (user-error "No response from Copilot"))
    ;; Trim whitespace and strip markdown fences
    (setq docstring (string-trim docstring))
    (when (string-match "^```[a-z]*\n" docstring)
      (setq docstring (replace-regexp-in-string "^```[a-z]*\n" "" docstring))
      (setq docstring (replace-regexp-in-string "\n```\\'" "" docstring))
      (setq docstring (string-trim docstring)))
    (if (derived-mode-p 'emacs-lisp-mode 'lisp-interaction-mode)
        (copilot-docstring--insert-elisp docstring start-pos)
      (copilot-docstring--insert-generic docstring start-pos))
    (message "Docstring inserted.")))

(provide 'copilot-docstring)
;;; copilot-docstring.el ends here

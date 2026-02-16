;;; copilot-echo-bot.el --- Minimal Copilot SDK demo -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edd Wilder-James
;; Author: Edd Wilder-James <edd@me.com>
;; Keywords: tools, ai

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Bare-minimum Copilot SDK example.  Connect, send one message,
;; print the response, disconnect.  Proves the SDK works end-to-end.
;;
;; Usage:
;;   M-x copilot-echo-bot

;;; Code:

(require 'copilot-sdk)

;;;###autoload
(defun copilot-echo-bot (prompt)
  "Send PROMPT to Copilot and display the response."
  (interactive "sAsk Copilot: ")
  (copilot-sdk-ensure-connected)
  (let* ((session-id (copilot-sdk-create-session
                      :system-message "You are a helpful assistant. Be concise."))
         (response (copilot-sdk-send-and-wait session-id prompt 30)))
    (copilot-sdk-destroy-session session-id)
    (message "%s" (or response "[no response]"))))

(provide 'copilot-echo-bot)
;;; copilot-echo-bot.el ends here

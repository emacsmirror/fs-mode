;;; root-cmd.el --- execute command and get the output with root privilege

;; Copyright (C) 2013  zxsu

;; Author: zxsu <suzp1984@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'comint)

(defvar root-cmd-proc-name "*root-proc*"
  "Default root cmd proc name.")

(defvar root-cmd-buffer-name "*root-proc*"
  "Default root cmd buffer name.")

(defvar root-cmd-output-filter-hooks '(root-cmd-output-keep-hook root-cmd-password-prompt-hook)
  "Function hooks used to process output.")

(defvar root-cmd-command nil
  "record the command.")

(defvar root-cmd-output-keep nil
  "keep cmd output.")

(defun root-cmd-send-password (prompt)
  "send password to process"
  ;(message "begin root-cmd-send-password")
  (let ((proc (get-process "*root-proc*")))
    (if (processp proc)
        (let ((str (read-passwd prompt)))
          (process-send-string proc (concat str "\n"))
          (sit-for 1)
          (process-send-string proc (concat root-cmd-command "\n"))))))

(defun root-cmd-output-keep-hook (string)
  "keep all cmd output."
  ;(message "root-cmd-output-keep-hook: %s" string)
  (setq root-cmd-output-keep (cons string root-cmd-output-keep)))

(defun root-cmd-password-prompt-hook (string)
  "prompt for password input"
  (when (string-match comint-password-prompt-regexp string)
    (when (string-match "^[ \n\r\t\v\f\b\a]+" string)
      (setq string (replace-match "" t t string)))
    (root-cmd-send-password string)))

(defun root-cmd-filter (proc string)
  "filter"
  (let ((buffer (get-buffer-create root-cmd-buffer-name))) 
    (with-current-buffer buffer
      (insert string)
      (run-hook-with-args 'root-cmd-output-filter-hooks string)))
  )

(defun root-cmd-sentinel (process event)
  "root process sentinel"
  (message "Process %s received event %s." process event)
  )

(defun root-cmd (command)
  "output command as root process."
;;  (interactive)
  (let* ((name (or root-cmd-proc-name "*root-proc*"))
         (buffer (get-buffer-create (or root-cmd-buffer-name "*root-proc*")))
         (proc (get-process (or root-cmd-proc-name "*root-proc*")))
         )
    (setq root-cmd-output-keep nil)
    (when (processp proc)
      (root-cmd-stop)
      (sit-for 1)
      ;(delete-process proc)
      )
    (setq proc (start-file-process name buffer shell-file-name "-i"))
    (setq root-cmd-command command)
    (set-process-filter proc 'root-cmd-filter)
    (set-process-sentinel proc 'root-cmd-sentinel)
    (message "process-send-string output: %s" (process-send-string proc "su\n"))
    ))

(defun root-cmd-stop ()
  "Stop root cmd process."
  (interactive)
  (let ((proc (get-process (or root-cmd-proc-name "*root-proc*")))
        (buffer (get-buffer (or root-cmd-buffer-name "*root-proc*"))))
    (if (processp proc)
        (delete-process proc))
    (if (bufferp buffer)
        (kill-buffer buffer))))

(defun root-cmd-output ()
  "return the cmd output."
  (interactive)
  (if (> (length root-cmd-output-keep) 2)
      (nth 1 root-cmd-output-keep)))

(provide 'root-cmd)
;;; root-cmd.el ends here

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

(defvar root-cmd-output-filter-hooks '(root-cmd-password-prompt)
  "Function hooks used to process output.")

(defvar root-cmd-command nil
  "record the command.")

(defun root-cmd-send-password (prompt)
  "send password to process"
  (message "begin root-cmd-send-password")
  (let ((proc (get-process "*root-proc*")))
    (if (processp proc)
        (let ((str (read-passwd prompt)))
          (process-send-string proc (concat str "\n"))
          (process-send-string proc (concat root-cmd-command "\n"))))))

(defun root-cmd-password-prompt (string)
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

(defun root-cmd-to-string (command)
  "output command as root process."
;;  (interactive)
  (let* ((name (or root-cmd-proc-name "*root-proc*"))
         (buffer (get-buffer-create (or root-cmd-buffer-name "*root-proc*")))
         (proc (get-process (or root-cmd-proc-name "*root-proc*")))
         )
    (unless (processp proc)
      (setq proc (start-file-process name buffer shell-file-name "-i")))
    (setq root-cmd-command command)
    (set-process-filter proc 'root-cmd-filter)
    (process-send-string proc "su\n")
    ))


(provide 'root-cmd)
;;; root-cmd.el ends here

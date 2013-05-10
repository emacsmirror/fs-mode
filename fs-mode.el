;;; fs-mode.el --- linux file system menagement

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

(require 'tabulated-list)

(defgroup fs-mode nil
  "File System mode"
  :tag "The Linux file system management."
  :version "0.1"
  :group 'applications)

(defvar fs-mode-buffer-name "*fs-mode*"
  "Default fs-mode buffer name.")

(defvar fs-mode-parser nil
  "fsm parser for fs-mode.")

(defvar fs-mode-all-list nil
  "All result after parser.")

(defvar fs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'fs-mount)
    (define-key map (kbd "u") 'fs-umount)
    map)
  "fs-mode keymap.")

(defun fs-mount ()
  "Mount file system"
  )

(defun fs-umount ()
  ""
  )

(defun fs-mode-refresh ()
  "Refresh fs-mode buffer."
  (setq tabulated-list-entries nil)
  (let ((output (shell-command-to-string "df -h")))
    ;;(setq fs-mode-parser (start-fs-mode-parser output))
    (dolist (line (split-string output "\n" t))
      (cond ((string-match "^Filesystem" line)
             t)
            (t 
             (let ((res (split-string line " " t)))
               (message "%s" (length res))
               (when (= 6 (length res))
                 (message "Filesystem: %s" (nth 0 res))
                 (message "Size: %s" (nth 1 res))
                 (message "Used: %s" (nth 2 res))
                 (message "Avail: %s" (nth 3 res))
                 (message "Use: %s" (nth 4 res))
                 (message "Mount on: %s" (nth 5 res))
                 (add-to-list 'fs-mode-all-list 
                              (list :filesystem (nth 0 res)
                                    :size (nth 1 res)
                                    :used (nth 2 res)
                                    :avail (nth 3 res)
                                    :use% (nth 4 res)
                                    :mounton (nth 5 res)))
                 )))
          ))
    (dolist (p fs-mode-all-list)
      (push (list nil
                  (vector 
                   (plist-get p :filesystem)
                   (plist-get p :size)
                   (plist-get p :used)
                   (plist-get p :avail)
                   (plist-get p :use%)
                   (plist-get p :mounton)))
            tabulated-list-entries))))

;;;###autoload
(define-derived-mode fs-mode tabulated-list-mode "File System mode"
  "Major mode for Linux File System management."
  (setq tabulated-list-format [("Filesystem" 10 t)
                               ("Size" 5 t)
                               ("Used" 5 t)
                               ("Avail" 5 t)
                               ("Use%" 5 t)
                               ("Mounted on" 10 t)])
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook 'fs-mode-refresh nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun list-fs-usage ()
  "list all file system usage"
  (interactive)
  (let ((buffer (get-buffer-create 
                 (or fs-mode-buffer-name "*fs-mode*"))))
    (with-current-buffer buffer
      (fs-mode)
      (fs-mode-refresh)
      (tabulated-list-print))
    (pop-to-buffer buffer)))

(provide 'fs-mode)
;;; fs-mode.el ends here

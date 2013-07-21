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
(require 'root-cmd)

(defgroup fs-mode nil
  "File System mode"
  :tag "The Linux file system management."
  :version "0.1"
  :group 'applications)

(defcustom fs-mode-warning-threshold 90
  "Up threshold to warning user, when the disk is full."
  :type 'integer
  :group 'fs-mode)

(defface fs-mode-face-1
  '((t (:inherit font-lock-type-face)))
  "First fs face")

(defface fs-mode-face-2 
  '((t (:inherit font-lock-constant-face)))
  "Second fs face")

(defface fs-mode-face-3
  '((t (:inherit font-lock-variable-name-face)))
  "fs face")

(defface fs-mode-face-warning
  '((t (:inherit font-lock-warning-face)))
  "fs warning face")

(defface fs-mode-face-4 
  '((t (:inherit font-lock-comment-delimiter-face)))
  "fs face")

(defface fs-mode-face-5
  '((t (:inherit font-lock-function-name-face)))
  "fs face")

(defface fs-mode-face-6
  '((t (:inherit font-lock-keyword-face)))
  "fs face")

(defvar fs-mode-buffer-name "*fs-mode*"
  "Default fs-mode buffer name.")

(defvar fs-mode-parser nil
  "fsm parser for fs-mode.")

(defvar fs-mode-all-list nil
  "All result after parser.")

(defvar fs-mode-ecryptfs-regexp-1
  "Select key type\\(.*\\)\n.*\\(1).*\\)\n.*\\(2).*\\)\n.*\\(3).*\\)\n.*\\(4).*\\)\n"
  "Regexp matching prompts for excryptfs.")

(defvar fs-mode-ecryptfs-regexp-2
  "PKCS.*Serialized ID:"
  "Regexp matching prompts for excryptfs.")

(defvar fs-mode-ecryptfs-regexp-3
  "PEM key file \\[\\(.*\\)\\]:"
  "Regexp matching prompts for excryptfs.")

(defvar fs-mode-ecryptfs-regexp-4 
  (regexp-opt '("tspi_uuid:"))
  "Regexp matching prompts for excryptfs.")

(defvar fs-mode-ecryptfs-regexp-5
  "Select cipher\\(.*\\)\n.*\\(1).*\\)\n.*\\(2).*\\)\n.*\\(3).*\\)\n.*\\(4).*\\)\n.*\\(5).*\\)\n.*\\(6).*\\)\n"
  "Regexp matching prompts for excryptfs."
  )

(defvar fs-mode-ecryptfs-regexp-6
  "Select key bytes\\(.*\\)\n.*\\(1).*\\)\n.*\\(2).*\\)\n.*\\(3).*\\)\n"
  "Regexp matching prompts for excryptfs."
  )

(defvar fs-mode-ecryptfs-regexp-7
  "Enable plaintext passthrough (y/n)"
  "Regexp matching prompts for excryptfs.")

(defvar fs-mode-ecryptfs-regexp-8
  "Enable filename encryption (y/n)"
  "Regexp matching prompts for excryptfs.")

(defvar fs-mode-ecryptfs-regexp-9 
  "Filename Encryption Key (FNEK) Signature \\[\\(.*\\)\\]:"
  "Regexp matching prompts for excryptfs.")

(defvar fs-mode-fs-type-collection
  '("ecryptfs" "ext4" "ext3" "fat")
  "Collection of file system types used for completing-read.")

(defvar fs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'fs-mount)
    (define-key map (kbd "u") 'fs-umount)
    (define-key map (kbd "f") 'fs-refresh)
    (define-key map (kbd "RET") 'fs-goto-directory)
    (define-key map (kbd "q") 'fs-quit)
    map)
  "fs-mode keymap.")

(defun fs-goto-directory (directory)
  "goto directory in eshell mode."
  (interactive (let ((id (tabulated-list-get-id))
                     default)
                 (setq default (read-directory-name (concat "Default [" id "]: ") 
                                                    id id t))
                 (list default)))
  (require 'eshell)
  (let ((buffer (get-buffer eshell-buffer-name))
        )
    (unless buffer
      (eshell)
      (setq buffer (get-buffer eshell-buffer-name)))
    (with-current-buffer buffer
      (setq default-directory directory)
      (eshell-send-input))
    (pop-to-buffer buffer)
    )
  )

(defun fs-quit ()
  "quit fs mode"
  (interactive)
  (root-cmd-stop)
  (quit-window))

(defun fs-refresh ()
  "Refresh fs-mode display."
  (interactive)
  (root-cmd-stop)
  (let ((buffer (get-buffer
                 (or fs-mode-buffer-name "*fs-mode*"))))
    ;;(message "get buffer name: %s" buffer)
    (when (bufferp buffer) 
      (with-current-buffer buffer
        (fs-mode-refresh)
        (tabulated-list-print))
      (pop-to-buffer buffer)
      )
    ) 
)

(defun fs-mount (device path &optional type)
  "Mount file system"
  (interactive (let ((id (tabulated-list-get-id))
                     device
                     path
                     type)
                 (setq device (read-file-name
                           "Device name: " id id t))
                 (setq path (read-directory-name
                             (concat "Default mount point [" id "]: ")
                             id id t))
                 (setq type (completing-read
                             "Default filesystem type: "
                             fs-mode-fs-type-collection
                             ))
                 (list device path type)))
  (message "%s %s %s" device path type)
  (let ((cmd (if (> (string-width type) 0) 
                 (concat "mount -t " type " " device " " path)
               (concat "mount " device " " path))))
    (message "%s" cmd)
    (root-cmd cmd)
    )
  )

(defun fs-mount-ecryptfs-hook (str)
  "hook for mount ecryptfs."
  (cond ((string-match fs-mode-ecryptfs-regexp-1 str)
         (let ((select1 (match-string 2 str))
               (select2 (match-string 3 str))
               (select3 (match-string 4 str))
               (select4 (match-string 5 str))
               (proc (get-process (or root-cmd-proc-name "*root-proc*")))
               result)
           (message "before while null.")
           (while (null result)
             (message "null result.")
             (setq result (completing-read "Select key type to use: " `(,select1 ,select2 ,select3 ,select4)
                                           nil nil "4"))
             (unless (string-match "^\\([1-4]\\)$" result)
               (setq result nil)))
           (if (processp proc) 
               (process-send-string proc (concat result "\n")))
           ))
        ((string-match fs-mode-ecryptfs-regexp-2 str)
         (let ((proc (get-process (or root-cmd-proc-name "*root-proc*")))
               result)
           (while (null result)
             (setq result (read-string str)))
           (if (processp proc)
               (process-send-string proc (concat result "\n")))))
        ((string-match fs-mode-ecryptfs-regexp-3 str)
         (let ((default (match-string 1 str))
               (proc (get-process (or root-cmd-proc-name "*root-proc*")))
               result)
           (while (null result)
             (setq result (read-file-name "PEM key file " 
                                          default default t))
             )
           (if (processp proc)
               (process-send-string proc (concat result "\n")))))
        ((string-match fs-mode-ecryptfs-regexp-4 str)
         (let ((proc (get-process (or root-cmd-proc-name "*root-proc*")))
               result)
           (while (null result)
             (setq result (read-string str)))
           (if (processp proc)
               (process-send-string proc (concat result "\n")))
           ))
        ((string-match fs-mode-ecryptfs-regexp-5 str)
         (let ((select1 (match-string 2 str))
               (select2 (match-string 3 str))
               (select3 (match-string 4 str))
               (select4 (match-string 5 str))
               (select5 (match-string 6 str))
               (select6 (match-string 7 str))
               (proc (get-process (or root-cmd-proc-name "*root-proc*")))
               result)
           (while (null result)
             (setq result (completing-read "Select cipher: " 
                                          `(,select1 ,select2 ,select3 ,select4 ,select5 ,select6)
                                          nil nil "1"))
             (unless (string-match "^\\([1-6]\\)$" result)
               (setq result nil)))
           (if (processp proc)
               (process-send-string proc (concat result "\n"))))
         )
        ((string-match fs-mode-ecryptfs-regexp-6 str)
         (let ((select1 (match-string 2 str))
               (select2 (match-string 3 str))
               (select3 (match-string 4 str))
               (proc (get-process (or root-cmd-proc-name "*root-proc*")))
               result)
           (while (null result)
             (setq result (completing-read "Select key bytes: " 
                                          `(,select1 ,select2 ,select3)
                                          nil nil "1"))
             (unless (string-match "^\\([1-3]\\)$" result)
               (setq result nil)))
           (if (processp proc)
               (process-send-string proc (concat result "\n")))))
        ((string-match fs-mode-ecryptfs-regexp-7 str)
         (let ((proc (get-process (or root-cmd-proc-name "*root-proc*")))
               result)
           (while (null result)
             (setq result (completing-read str 
                                           '("y" "n") nil t "y" nil "y")))
           (if (processp proc)
               (process-send-string proc (concat result "\n")))))
        ((string-match fs-mode-ecryptfs-regexp-8 str)
         (let ((proc (get-process (or root-cmd-proc-name "*root-proc*")))
               result)
           (while (null result)
             (setq result (completing-read str
                                           '("y" "n") nil t "y" nil "y")))
           (if (processp proc)
               (process-send-string proc (concat result "\n")))))
        ((string-match fs-mode-ecryptfs-regexp-9 str)
         (let ((default (match-string 1 str))
               (proc (get-process (or root-cmd-proc-name "*root-proc*")))
               result)
           (while (null result)
             (setq result (read-string str default nil default)))
           (if (processp proc)
               (process-send-string proc (concat result "\n")))))
        )
  )

(defun fs-umount (path)
  "Umount a mount point of file system."
  (interactive (let ((id (tabulated-list-get-id))
                     result)
                 (setq result (completing-read 
                               (concat "Default umonut point [" id "]: ")
                               tabulated-list-entries nil t nil nil id))
                 (list result)))
  (message "%s" path)
  (root-cmd (concat "umount " path))
  ;(sit-for 2)
  ;(root-cmd-stop)
  ;(fs-refresh)
  )

(defun fs-warning-p (percent)
  "Return t if percent is greater than `fs-mode-warning-threshold'."
  (when (string-match "\\([0-9]+\\)" percent)
    (let ((int (string-to-int (match-string 1 percent)))
          (threshold (if (integerp fs-mode-warning-threshold)
                         fs-mode-warning-threshold
                       (string-to-int fs-mode-warning-threshold))))
      (if (> int threshold)
          t
        nil)))
  )

(defun fs-mode-refresh ()
  "Refresh fs-mode buffer."
  (setq tabulated-list-entries nil)
  (setq fs-mode-all-list nil)
  (let ((output (shell-command-to-string "df -h")))
    ;;(setq fs-mode-parser (start-fs-mode-parser output))
    (dolist (line (split-string output "\n" t))
      (cond ((string-match "^Filesystem" line)
             t)
            (t 
             (let ((res (split-string line " " t)))
               (when (= 6 (length res))
                 (let ((filesystem (nth 0 res))
                       (size (nth 1 res))
                       (used (nth 2 res))
                       (avail (nth 3 res))
                       (use (nth 4 res))
                       (mounton (nth 5 res)))
                   (add-to-list 'fs-mode-all-list 
                              (list :filesystem (propertize filesystem 'face 'fs-mode-face-1)
                                    :size (propertize size 'face 'fs-mode-face-2)
                                    :used (propertize used 'face 'fs-mode-face-3)
                                    :avail (propertize avail 'face 'fs-mode-face-4)
                                    :use% (propertize use 'face (if (fs-warning-p use)
                                                                            'fs-mode-face-warning
                                                                          'fs-mode-face-5))
                                    :mounton (propertize mounton 'face 'fs-mode-face-6))))
                 
                 )))
          ))
    (dolist (p fs-mode-all-list)
      (push (list (plist-get p :mounton)
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
  (setq tabulated-list-format [("Filesystem" 15 t)
                               ("Size" 10 t)
                               ("Used" 10 t)
                               ("Avail" 5 t)
                               ("Use%%" 10 t)
                               ("Mounted on" 20 t)])
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook 'fs-mode-refresh nil t)
  (tabulated-list-init-header)
  (make-local-variable 'root-cmd-output-filter-hooks)
  (add-to-list 'root-cmd-output-filter-hooks 'fs-mount-ecryptfs-hook))

;;;###autoload
(defun list-fs ()
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

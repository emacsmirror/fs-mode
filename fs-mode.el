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

(defvar fs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'fs-mount-at)
    (define-key map (kbd "u") 'fs-umount-at)
    map)
  "fs-mode keymap.")

(defun fs-mode-refresh ()
  "Refresh fs-mode buffer."
  )

;;;###autoload
(define-derived-mode fs-mode tabulated-list-mode "File System mode"
  "Major mode for Linux File System management."
  (setq tabulated-list-format [("Filesystem" 10 t)
                               ("Size" 5 t)
                               ("Used" 5 t)
                               ("Avail" 5 t)
                               ("Use%" 5 t)
                               ("Mounted on" 10 t)])
  (add-hook 'tabulated-list-revert-hook 'fs-mode-refresh nil t)
  (tabulated-list-init-header))

(provide 'fs-mode)
;;; fs-mode.el ends here

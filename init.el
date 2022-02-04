;;; My emacs config
;;; Code:

;;; My emacs config
;;; Code:
(show-paren-mode 1)
(global-hl-line-mode 1)
(column-number-mode)

(tool-bar-mode -1)

(menu-bar-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq backup-by-copying t)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-linum-mode 1)

(setq dired-listing-switches "-alh")


(setq inhibit-startup-screen t)

(prefer-coding-system 'utf-8)

(setq create-lockfiles nil)

(setq custom-file (make-temp-file "emacs-custom"))

;;;
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(global-auto-revert-mode t)

;; Initialize package sources
(require 'package)
;; avoid package-check-package-signature
(setq package-check-signature t)
;;
(setq package-archives '(("gnu" .   "http://localhost:8080/elpa/")
			 ("melpa" . "http://localhost:8080/melpa/")))



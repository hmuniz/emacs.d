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


(package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))


(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-solarized-light   t)
  
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; (use-package dracula-theme
;;   :config
;;   (load-theme 'dracula t))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(defun my-eval-buffer ()
  (interactive)
  (eval-buffer)
  (message "Tudo top!"))

(use-package emacs-lisp-mode
  :ensure nil
  :bind (("C-c C-c" . my-eval-buffer)))


(use-package yaml-mode
  :ensure t)

(use-package deadgrep
  :ensure t)


(use-package multiple-cursors
  :ensure t)


(use-package restclient
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
  :custom
  (restclient-log-request nil))


(use-package ace-window
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package magit-todos
  ;; :config
  ;; (magit-todos-mode)
  :custom
  (magit-todos-exclude-globs
   '(".git/" "submodules/" "test/" "Jenkins/" ".idea/" "*.egg-info/"))
  (magit-todos-update t)
  )


(use-package hydra
  :defer t)

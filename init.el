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

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package markdown-mode)

(use-package dockerfile-mode)

(use-package general
  :config
  (general-def
    "<ESC> <down>" 'mc/mark-next-like-this
    "<ESC> <up>" 'mc/mark-previous-like-this))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . efs/lsp-mode-setup)
	 (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-mode . (lambda ()
                       (let ((lsp-keymap-prefix "C-c l"))
                         (lsp-enable-which-key-integration)))))
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  ;;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold 100000000)
  (lsp-enable-which-key-integration t)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :custom
  (lsp-pyright-typechecking-mode "off")
  (lsp-enable-snippet nil)
  (lsp-file-watch-ignored-directories
   '("[/\\\\]\\.git\\'" "[/\\\\]\\.github\\'" "[/\\\\]\\.circleci\\'" "[/\\\\]\\.hg\\'" "[/\\\\]\\.bzr\\'" "[/\\\\]_darcs\\'" "[/\\\\]\\.svn\\'" "[/\\\\]_FOSSIL_\\'" "[/\\\\]\\.idea\\'" "[/\\\\]\\.ensime_cache\\'" "[/\\\\]\\.eunit\\'" "[/\\\\]node_modules" "[/\\\\]\\.yarn\\'" "[/\\\\]\\.fslckout\\'" "[/\\\\]\\.tox\\'" "[/\\\\]dist\\'" "[/\\\\]dist-newstyle\\'" "[/\\\\]\\.stack-work\\'" "[/\\\\]\\.bloop\\'" "[/\\\\]\\.metals\\'" "[/\\\\]target\\'" "[/\\\\]\\.ccls-cache\\'" "[/\\\\]\\.vscode\\'" "[/\\\\]\\.venv\\'" "[/\\\\]\\.deps\\'" "[/\\\\]build-aux\\'" "[/\\\\]autom4te.cache\\'" "[/\\\\]\\.reference\\'" "[/\\\\]\\.lsp\\'" "[/\\\\]\\.clj-kondo\\'" "[/\\\\]\\.shadow-cljs\\'" "[/\\\\]\\.babel_cache\\'" "[/\\\\]\\.cpcache\\'" "[/\\\\]\\checkouts\\'" "[/\\\\]\\.m2\\'" "[/\\\\]bin/Debug\\'" "[/\\\\]obj\\'" "[/\\\\]_opam\\'" "[/\\\\]_build\\'" "[/\\\\]\\.direnv\\'" "[/\\\\]workers\\'")))

(require 'lsp-mode)

(setq lsp-keymap-prefix "C-c l")

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  :custom
  (lsp-enable-dap-auto-configure nil)
  ;; :config
  :commands dap-debug
  :config
  ;; Set up Node debugging
  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (dap-ui-mode 1)
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "d" '(dap-hydra t :wk "debugger"))
  (defun breackpoint-on-exception ()
    (interactive)
    (if dap-exception-breakpoints
	(setq dap-exception-breakpoints nil)
      (setq dap-exception-breakpoints '(("python" ("raised" . t)
					 ("uncaught" . t)))))
    )
  :custom
  (dap-debug-restart-keep-session nil)
  (dap-utils-vscode-ext-url
   "http://localhost:8080/_apis/public/gallery/publishers/%s/vsextensions/%s/%s/vspackage")
  )
;;
(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets)
;;
(use-package pyvenv)
(use-package highlight-indentation)
;; (use-package lsp-pyright)  ; or lsp-deferred
;; (use-package lsp-python-ms
;;   :ensure t
;;   :init
;;   (setq lsp-python-ms-executable (executable-find "python-language-server")))
(use-package sphinx-doc
  :config
  (setq sphinx-doc-include-types t))
(use-package python-mode
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))  
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "ipython")
  (python-shell-font-lock-enable nil)
  (python-shell-interpreter-args
   "--simple-prompt -i --ipython-dir=/home/henrmun/utils/ipython")
  (dap-python-debugger 'ptvsd)
  :config
  (require 'dap-python)
  ;; `general-define-key' is comparable to `define-key' when :keymaps is specified
  (general-define-key
   :keymaps 'python-mode-map
   "C-c C-r" 'python-shell-send-region))

(add-hook 'python-mode-hook (lambda ()
                              (require 'sphinx-doc)
                              (sphinx-doc-mode t)))

;; (use-package importmagic
;;     :ensure t
;;     :config
;;     (add-hook 'python-mode-hook 'importmagic-mode))

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))


(use-package company
  :after lsp-mode
  :hook ((lsp-mode . company-mode)
	 (racket-mode . company-mode)
	 (inferior-python-mode . company-mode))
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :config

  (defun company-yasnippet-or-completion ()
    "Solve company yasnippet conflicts."
    (interactive)
    (let ((yas-fallback-behavior
           (apply 'company-complete-common nil)))
      (yas-expand)))

  (add-hook 'company-mode-hook
            (lambda ()
              (substitute-key-definition
               'company-complete-common
               'company-yasnippet-or-completion
               company-active-map))))

(use-package company-box
  :hook (company-mode . company-box-mode))
;;

(defun my-inhibit-global-linum-mode ()
  "Counter-act `global-linum-mode'."
  (add-hook 'after-change-major-mode-hook
            (lambda () (linum-mode 0))
            :append :local))

(use-package multi-term
  :ensure t

  :config
  (setq multi-term-program "/bin/zsh")
  (add-hook 'term-mode-hook 'my-inhibit-global-linum-mode)
  (defalias 'mt #'multi-term))


(use-package helpful
  :ensure t
  :init
  (global-set-key (kbd "C-h .") #'helpful-at-point)
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))
;;

(use-package smartparens
  :init
  (progn
    (use-package smartparens-config :ensure nil)
    (use-package smartparens-python :ensure nil)
    (use-package smartparens-html :ensure nil)
    (use-package smartparens-racket :ensure nil)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1)
    (smartparens-global-strict-mode))
  :config
  (progn
    (setq smartparens-strict-mode t)
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p)))
  :bind
  (("C-M-k" . sp-kill-sexp-with-a-twist-of-lime)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-n" . sp-up-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-p" . sp-backward-down-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("M-s" . sp-splice-sexp)
   ("M-r" . sp-splice-sexp-killing-around)
   ("C-0" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-9" . sp-backward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)
   ("M-S" . sp-split-sexp)
   ("M-J" . sp-join-sexp)
   ("C-M-t" . sp-transpose-sexp)))

(use-package py-autopep8
  ;; :config
  ;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  )

;; wsl
(defun wsl-copy (start end)
  (interactive "r")
  (shell-command-on-region start end "clip.exe"))

(global-set-key
 (kbd "M-w")
 'wsl-copy)

(use-package 2048-game)

;;
(use-package  move-text
  :config
  (require 'move-text)
  (move-text-default-bindings))
;;

;; Persistent scratch

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))

(use-package origami
  :bind ("C-c l o" . hydra-origami/body)
  :config
  (defhydra hydra-origami (:color red
                                  :hint nil)
    "
_t_: toggle    _o_: open     _r_: redo    _p_: prev        _C_: close all
_u_: undo      _c_: close    _n_: next    _O_: open all    _q_: quit
"
    ("t" origami-recursively-toggle-node)
    ("u" origami-undo)
    ("r" origami-redo)
    ("p" origami-previous-fold)
    ("n" origami-next-fold)
    ("o" origami-open-node)
    ("c" origami-close-node)
    ("O" origami-open-all-nodes)
    ("C" origami-close-all-nodes)
    ("q" nil "Quit" :color blue))

  (global-origami-mode))

;; lsp-origami provides support for origami.el using language server protocol’s
;; textDocument/foldingRange functionality.
;; https://github.com/emacs-lsp/lsp-origami/
(use-package lsp-origami
  :hook ((lsp-after-open . lsp-origami-mode)))

(provide 'setup-origami)


(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode 1))

(use-package racket-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))


(use-package sicp)


;;
(defun search-selection (beg end)
  "search for selected text"
  (interactive "r")
  (let ((selection (buffer-substring-no-properties beg end)))
    (deactivate-mark)
    (isearch-mode t nil nil nil)
    (isearch-yank-string selection)))

(use-package selected
  :commands selected-minor-mode
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("u" . upcase-region)
              ("l" . downcase-region)
              ("w" . count-words-region)
	      ("m" . mc/mark-next-like-this)
	      ("n" . search-selection)
	      (";" . comment-dwim)
	      ("e" . mc/edit-lines)
	      ("<DEL>" . delete-region)))

(selected-global-mode 1)










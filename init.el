(load (expand-file-name "elpaca.el" user-emacs-directory))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package emacs
  :elpaca nil
  :init
  (set-face-attribute 'default nil
    :font "CaskaydiaCove Nerd Font"
    :height 180)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (pixel-scroll-precision-mode)
  (blink-cursor-mode 0)
  (setq inhibit-splash-screen t
	use-file-dialog nil
	initial-scratch-message nil)
  (setq-default indent-tabs-mode nil
		tab-width 2))

(use-package doom-themes
  :demand
  :config
  (load-theme 'doom-challenger-deep t))
(elpaca-wait)

(use-package emacs
  :elpaca nil
  :init
  (setq make-backup-files nil
        auto-save-default nil
        create-lockfiles nil))

(use-package emacs
  :elpaca nil
  :hook (prog-mode . display-line-numbers-mode)
  :init
  (setq ring-bell-function 'ignore)
  (setq display-line-numbers-type 'relative)
  ;; utf-8 everywhere
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))

(use-package dired
  :elpaca nil
  :init
  (setq dired-dwim-target t))


(use-package evil
  :demand
  :config
  (add-to-list 'evil-emacs-state-modes 'elpaca-log-mode)
  (add-to-list 'evil-emacs-state-modes 'elpaca-ui-mode)
  ; (evil-set-leader nil (kbd "<SPC>"))
  ; (evil-define-key 'normal 'global (kbd "<leader>f") 'find-file)
  ; (evil-define-key 'normal 'global (kbd "<leader>r") 'recentf)
  (evil-mode 1))

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-auto-unbind-keys)
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "f" 'find-file
   "o" 'other-window
   "r" 'recentf
   "g" 'magit-status
   "1" 'delete-other-windows))

(use-package which-key
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

;; minibuffer

(use-package marginalia
  :init
  (setq marginalia-align 'left)
  (marginalia-mode))

(use-package vertico
  :config
  (vertico-mode))

(use-package vertico-mouse
  :elpaca nil :after vertico :load-path "elpaca/builds/vertico/extensions/"
  :config
  (vertico-mouse-mode))

(use-package orderless
  :after vertico
  :config
  (setq
   completion-styles '(orderless)
   orderless-completion-styles '(orderless basic)
   completion-category-defaults nil
   completion-category-overrides
   '((file (styles basic-remote ; For `tramp' hostname completion with `vertico'
                   orderless)))
   orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-flex
     orderless-regexp
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     )))

;; end minibuffer

(use-package recentf
  :elpaca nil
  :config
  (recentf-mode))

(use-package vterm)

(use-package pdf-tools
  :config
  (pdf-tools-install))

(elpaca-wait)

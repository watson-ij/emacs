(load (expand-file-name "elpaca.el" user-emacs-directory))

(use-package emacs
  :elpaca nil
  :init
  (set-face-attribute 'default nil
    :font "CaskaydiaCove Nerd Font"
    :height 160)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
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
  (setq display-line-numbers 'relative)
  (global-display-line-numbers-mode))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package evil
  :demand
  :config
  (evil-mode 1))

(setq warning-minimum-level :emergency)
(load (expand-file-name "elpaca.el" user-emacs-directory))

(use-package magit
  :bind ("C-x g" . magit-status)
  (:map magit-mode-map ("/" . evil-search-forward))
  :config
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

(use-package emacs
  :if (display-graphic-p)
  :elpaca nil
  :init
  (pixel-scroll-precision-mode)
  (scroll-bar-mode -1))

(use-package emacs
  :if (string= (system-name) "ArchBeasty")
  :elpaca nil
  :init
  (setq epg-gpg-home-directory "/home/iyan/.gnupg/yubikey")
  (setenv "SSH_AUTH_SOCK" (substring
                           (shell-command-to-string
                            "gpgconf --list-dirs agent-ssh-socket") 0 -1))
  (setenv "GNUPGHOME" "/home/iyan/.gnupg/yubikey"))

(use-package emacs
  :elpaca nil
  :init
  (setq size 180)
  (setq font "CaskaydiaCove Nerd Font")
  (when (string= (system-name) "ArchBeasty") (setq size 140))
  (when (string= (system-name) "ArchDesktop") (setq size 160))
  (when (string= (system-name) "GuixBeauty") (setq size 160))
  (when (string= (system-name) "GuixBeauty") (setq font "Cascadia Code"))
  (set-face-attribute 'default nil
    :font font
    :height size)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (if (functionp 'tool-bar-mode) (tool-bar-mode -1))
  (menu-bar-mode -1)
  (blink-cursor-mode 0)
  (setq tramp-ssh-controlmaster-options "") 
  (defun my-vc-off-if-remote ()
    (if (file-remote-p (buffer-file-name))
      (setq-local vc-handled-backends nil)))
  (add-hook 'find-file-hook 'my-vc-off-if-remote)
  (setq inhibit-splash-screen t
	use-file-dialog nil
	initial-scratch-message nil)
  (setq-default indent-tabs-mode nil
		tab-width 2))

(use-package proced
  :elpaca nil :init
  (setq proced-auto-update-flag t
        proced-enable-color-flag t ; Emacs 29
        proced-auto-update-interval 5
        proced-descend t
        proced-filter 'user))

(use-package doom-themes
  :demand
  :config
  (load-theme 'doom-opera t))
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


(use-package evil
  :demand
  :config
  (setq evil-want-keybinding nil)
  (add-to-list 'evil-emacs-state-modes 'elpaca-log-mode)
  (add-to-list 'evil-emacs-state-modes 'elpaca-ui-mode)
  ; (evil-set-leader nil (kbd "<SPC>"))
  ; (evil-define-key 'normal 'global (kbd "<leader>f") 'find-file)
  ; (evil-define-key 'normal 'global (kbd "<leader>r") 'recentf)
  (evil-define-key 'insert 'global (kbd "C-<tab>") 'completion-at-point)
  (evil-mode 1))

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-auto-unbind-keys)
  (general-override-mode)
  (general-create-definer localleader-def
    :states '(normal visual insert emacs)
    :prefix ","
    :non-normal-prefix "C-,")
  (general-create-definer leader-def
    :keymaps 'override
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (leader-def
   "SPC" 'execute-extended-command
   "TAB" 'switch-to-previous-buffer
   "f" 'find-file
   "b" 'switch-to-buffer
   "w" 'save-buffer
   "o" 'other-window
   "k" 'kill-buffer
   "e" 'eval-last-sexp
   "r" 'recentf
   "g" 'magit-status
   "v" 'split-window-right
   "2" 'split-window-right
   "1" 'delete-other-windows
   "0" 'delete-window
   ";" 'eval-expression
   "c" `(,(lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))) :which-key "config")
   "-" `(,(lambda () (interactive) (dired default-directory)) :which-key "dired")
   "h" '(:ignore t :which-key "help")
   "hm" 'describe-mode
   "hk" 'describe-key
   "hf" 'describe-function
   "hv" 'describe-variable
   "hi" 'info
   "hd" 'apropos-documentation
   "hb" 'describe-binding
   ))

(use-package consult
  :bind
  ("C-s" . consult-line)
  ("M-g" . consult-goto-line)
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (leader-def
    "p" '(:ignore t :which-key "project")
    "pp" 'consult-line-multi
    "pi" 'consult-imenu-multi
    "pf" 'consult-find
    "ps" 'consult-ripgrep
    "s" 'consult-ripgrep
    "y" 'consult-yank-from-kill-ring
    "x" 'xref-find-references
    "i" 'consult-outline
    "b" 'consult-buffer))

(use-package affe
  :config
  (leader-def "a" 'affe-find))

(use-package dired
  :elpaca nil
  :custom
  (dired-listing-switches "-alh")
  :init
  (setq dired-dwim-target t))

(use-package dired-subtree
  :custom
  (dired-subtree-cycle-depth 2)
  :bind (:map dired-mode-map
              ("<backtab>" . dired-subtree-cycle)
              ("<tab>" . dired-subtree-toggle)))

(use-package peep-dired
  :after dired
  :bind (:map dired-mode-map ("I" . peep-dired)))

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

(use-package embark
  :bind
  (("C-;" . embark-act)))

(use-package vertico
  :demand t
  ; https://systemcrafters.net/live-streams/may-21-2021/
  :bind (:map minibuffer-local-map
         ("<backspace>" . dw/minibuffer-backward-kill)
         ("DEL" . dw/minibuffer-backward-kill) ;; make it work in wezterm remote
         ("<del>" . dw/minibuffer-backward-kill))
  :init
  (defun dw/minibuffer-backward-kill (arg)
    "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a character backward"
    (interactive "p")
    (if minibuffer-completing-file-name
        ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
        (if (and (string-match-p "/\\'" (minibuffer-contents))
                 (string-match-p "/." (minibuffer-contents)))
            (zap-up-to-char (- arg) ?/)
          (delete-backward-char arg)
          ;(delete-minibuffer-contents)
          )
      (delete-backward-char arg)))
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
   ; completion-category-overrides
   ; '((file (styles basic-remote ; For `tramp' hostname completion with `vertico'
   ;                 orderless)))
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

(use-package corfu
  :config
  ;; Enable Corfu more generally for every minibuffer, as long as no other
  ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
  ;; completion UI. From
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  (global-corfu-mode))

(use-package corfu-popupinfo
  :elpaca nil :after corfu :load-path "elpaca/builds/corfu/extensions/"
  :config
  (setq corfu-popupinfo-delay 0.5)
  (corfu-popupinfo-mode))

(use-package cape
  :init
  ;; order matters, first wins
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package recentf
  :elpaca nil
  :config
  (recentf-mode))

(use-package s)
(use-package vterm
  :after s
  ; :defines ijw-shell
  :bind
  ("C-c v" . ijw-shell)
  :init
  (defun ijw-shell ()
    (interactive)
    (let* ((buf-shorts
            (seq-map (lambda (n) (s-replace ">" "" (s-replace "*vterm*<" "" n)))
                     (seq-filter (lambda (n) (s-starts-with? "*vterm*" n))
                                 (seq-map #'buffer-name (buffer-list)))))
           (buf (completing-read "vterm name: " buf-shorts))
           (name (concat "*vterm*<" buf ">")))
      (if (get-buffer name)
          (switch-to-buffer name)
        (vterm name))))
  (leader-def "t" 'ijw-shell)
  (setq vterm-max-scrollback 5000)
  :config
  (general-define-key :states 'insert :keymaps 'vterm-mode-map
                      "C-a" 'vterm--self-insert
                      "C-d" 'vterm--self-insert
                      "C-n" 'vterm--self-insert
                      "C-k" 'vterm--self-insert
                      "C-p" 'vterm--self-insert
                      "C-e" 'vterm--self-insert
                      "C-z" 'vterm--self-insert
                      "C-r" 'vterm--self-insert))

(use-package pdf-tools
  :if (not (or (string= (system-name) "gate")
               (string-prefix-p "lxplus" (system-name))))
  :config
  (pdf-tools-install))

(use-package tree-sitter)
(use-package tree-sitter-langs :after tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


;;; ORG

(use-package org
  :custom
  (org-agenda-files `(,(expand-file-name "~/org-roam")
                      ,(expand-file-name "~/org-roam/daily")))
  (org-startup-folded nil)
  (org-agenda-scheduled-leaders '("S:" "S.%2dx:"))
  (org-support-shift-select 'always)
  (org-latex-images-centered nil)
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-adapt-indentation nil)
  :init
  (general-define-key :states 'normal :keymaps 'org-agenda-mode-map
                      "q" 'org-agenda-quit
                      "g" 'org-agenda-redo-all
                      "RET" 'org-agenda-switch-to)
  (localleader-def :keymaps 'org-mode-map
    "." 'org-ctrl-c-ctrl-c
    "," 'org-todo)
  (leader-def
   "n" '(:ignore t :which-key "org")
   "nl" 'org-store-link
   "ni" 'org-insert-link
   "nc" 'org-capture
   "nb" 'org-iswitchb
   "nv" 'org-open-at-point
   "no" 'org-open-at-point
   "nw" 'org-todo-list
   "nn" 'org-agenda-list))

(use-package org-journal
  :after org
  :ensure t
  :custom
  (org-journal-dir (expand-file-name "~/org-roam/daily"))
  (org-journal-carryover-items "")
  (org-journal-time-prefix "** ")
  (org-journal-date-prefix "* ")
  (org-journal-date-format "%A, %x")
  (org-journal-time-format " ")
  (org-journal-file-type 'weekly)
  (org-journal-file-format "%Y_%m_%d.org")
  (org-journal-enable-agenda-integration nil)
  (org-journal-find-file #'find-file)
  :config
  ;; workaround problem when journal files aren't org-mode but org-journal-mode e.g. with org-ql
  ;; https://github.com/bastibe/org-journal/issues/298
  (defun org-journal-is-journal () nil)
  :init
  (leader-def
    "nj" 'org-journal-new-entry
    "nd" 'org-journal-new-date-entry
    "nt" 'org-journal-open-current-journal-file))

(use-package org-ql
  :after org
  :init
  (leader-def "ns" 'org-ql-find-in-agenda))

(use-package jupyter
  :config
  (require 'ob-jupyter)
  (require 'jupyter-python)
  ;(use-package ob-mermaid)
  (add-to-list 'org-src-lang-modes '("jupyter" . python))
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (ditaa . t)
     (dot . t)
     (plantuml . t)
     (gnuplot . t)
     (org . t)
     (scheme . t)
     (latex . t)
     ;(mermaid . t)
     (jupyter . t))))
;; workaround for jupyter nonsense...
(setenv "PYDEVD_DISABLE_FILE_VALIDATION" "1")

;;; END ORG

(use-package emms
  :config
  (leader-def
  "m m" 'emms-pause
  "m s" 'emms-play-or-stop
  "m i" 'emms-show
  "m n" 'emms-next
  "m p" 'emms-previous
  "m d" 'emms-add-directory
  "m f" 'emms-add-file
  "m +" 'emms-volume-mode-plus
  "m =" 'emms-volume-mode-plus
  "m -" 'emms-volume-mode-minus
  "m <" 'emms-seek-backward
  "m >" 'emms-seek-forward
  "m b" 'emms) ;; go to emms *b*uffer
  (require 'emms-setup)
  (defun emms-play-or-stop () (interactive)
    (if emms-player-playing-p
        (emms-stop) (emms-start)))
  (emms-all)
  (setq emms-volume-change-function 'emms-volume-pulse-change)
  (setq emms-volume-mode-timeout 2)
  (setq emms-player-list '(emms-player-vlc)
        emms-info-functions '(emms-info-native)))

(use-package evil-surround
  :ensure t
  :config
  (general-def 'visual evil-surround-mode-map
    "s" 'evil-surround-region
    "S" 'evil-substitute)
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (evil-collection-init))

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces)
  (custom-set-faces
   '(evil-goggles-default-face ((t (:inherit 'pulse-highlight-face))))))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package nix-mode
  :mode "\\.nix\\'")

(elpaca-wait)

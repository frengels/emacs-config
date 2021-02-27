;; -*- lexical-binding: t; -*-

(setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq custom-file (concat user-emacs-directory "custom-file.el"))
(when (file-exists-p custom-file) (load-file custom-file))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(use-package use-package-hydra)

(use-package paredit
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  (eval-expression-minibuffer-setup . enable-paredit-mode)
  (ielm-mode . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  (lisp-interaction-mode . enable-paredit-mode)
  (scheme-mode . enable-paredit-mode))

(use-package gitignore-mode)

(use-package gitconfig-mode)

(use-package gitattributes-mode)

(use-package magit
  :bind ("C-x g" . magit-status)
  ("C-c g" . magit-file-dispatch))

(use-package company
  :defer 3
  :bind (:map company-active-map
					; not sure about these bindings, they could interfere with movement
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous))
  :custom
  (company-idle-delay 0.3)
  (company-minimum-prefix-length 1)
  :config
  (global-company-mode t))

(use-package swiper
  :bind (("C-s" . swiper-isearch)
	 ("C-r" . swiper-isearch-backward)
	 ("C-c s s" . swiper)))

(use-package ivy
  :bind (("C-c C-r" . ivy-resume))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  :config
  (ivy-mode))

(use-package counsel
  :config
  (counsel-mode))

(use-package hydra)

(use-package avy
  :bind (("C-c w" . avy-goto-word-1)))

(use-package which-key
  :defer 5
  :diminish which-key-mode
  :init
  (which-key-mode))

(use-package doom-modeline
  :init
  (setq doom-modeline-indent-info t)
  :hook
  (after-init . doom-modeline-mode))

(use-package treemacs
  :bind (("C-c t" . treemacs))
  :init
  (setq treemacs-user-mode-line-format 'none))

(use-package ace-window
  :after (hydra)
  :bind (("C-;" . hydra-window/body))
  :hydra (hydra-window (:color blue :hint nil)
		       "
    Movement^   ^Split^       ^Switch^     ^^^Resize^
    -------------------------------------------------
    _h_ left    _/_ vertical   ^_b_uffer     _<left>_ left
    _j_ down    _-_ horizontal ^_f_ind files _<down>_ down
    _k_ up      _u_ undo       ^_a_ce window _<up>_ up
    _l_ right   _r_ reset      ^_s_wap       _<right>_ right

    _x_ M-x     _q_ quit
    "
		       ("h" windmove-left)
		       ("j" windmove-down)
		       ("k" windmove-up)
		       ("l" windmove-right)
		       ("/" (lambda ()
			      (interactive)
			      (split-window-right)
			      (windmove-right)))
		       ("-" (lambda ()
			      (interactive)
			      (split-window-below)
			      (windmove-down)))
		       ("u" (progn
			      (winner-undo)
			      (setq this-command 'winner-undo)))
		       ("r" winner-redo)
		       ("x" counsel-M-x)
		       ("q" nil)
		       ("b" switch-to-buffer)
		       ("f" find-file)
		       ("a" ace-window)
		       ("s" ace-swap-window)
		       ("<left>" nil)
		       ("<down>" nil)
		       ("<up>" nil)
		       ("<right>" nil)))

(use-package markdown-mode)

(use-package ripgrep)

(use-package projectile
  :after ripgrep
  :custom
  (projectile-indexing-method 'alien)
  (projectile-require-project-root t)
  (projectile-completion-system 'ivy)
  :bind (:map projectile-mode-map
	      (("C-c p" . projectile-command-map)))
  :config
  (projectile-mode +1))

(use-package flycheck)

(use-package lsp-treemacs
  :after (lsp-mode treemacs))

(use-package lsp-ivy
  :after (lsp-mode ivy))

(use-package lsp-ui
  :after lsp-mode)

(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-keymap-prefix "C-c l"))

(use-package dap-mode)

(use-package format-all
  :commands format-all-buffer
  :bind (("C-c f" . format-all-buffer)))

;; org section

(use-package org
  :custom
  (org-directory (expand-file-name "~/Documents/org")))

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory (expand-file-name "~/Documents/org-roam")))

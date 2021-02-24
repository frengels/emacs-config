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

(use-package use-package-hydra
  :straight t)

(use-package paredit
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  (eval-expression-minibuffer-setup . enable-paredit-mode)
  (ielm-mode . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  (lisp-interaction-mode . enable-paredit-mode)
  (scheme-mode . enable-paredit-mode))

(use-package gitignore-mode
  :straight t)

(use-package gitconfig-mode
  :straight t)

(use-package gitattributes-mode
  :straight t)

(use-package magit
  :straight t
  :bind ("C-x g" . magit-status))

(use-package company
  :straight t
  :defer 3
  :bind (:map company-active-map
	      ; not sure about these bindings, they could interfere with movement
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous))
  :init
  (setq company-idle-delay 0.3)
  :config
  (global-company-mode t))

(use-package swiper
  :straight t
  :bind (("C-s" . swiper-isearch)
	 ("C-r" . swiper-isearch-backward)))

(use-package ivy
  :straight t
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  :config
  (ivy-mode))

(use-package counsel
  :straight t
  :config
  (counsel-mode))

(use-package hydra
  :straight t)

(use-package avy
  :straight t
  :bind (("M-g g" . avy-goto-line)
	 ("M-g w" . avy-goto-word-1)))

(use-package which-key
  :straight t
  :diminish which-key-mode
  :init
  (which-key-mode))

(use-package doom-modeline
  :straight t
  :init
  (setq doom-modeline-indent-info t)
  :hook
  (after-init . doom-modeline-mode))

(use-package treemacs
  :straight t
  :init
  (setq treemacs-user-mode-line-format 'none))

(use-package ace-window
  :straight t
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

(use-package markdown-mode
  :straight t)

(use-package projectile
  :straight t)

;; org section

(use-package org
  :straight t)

(use-package org-roam
  :straight t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory (expand-file-name "~/Documents/org-roam")))

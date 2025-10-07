;; -*- lexical-binding: t; -*-

(set-language-environment "UTF-8")

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)
(setq-default indent-tabs-mode nil)
(setq pop-up-windows nil)

(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(blink-cursor-mode 0)

(auto-save-visited-mode)
;; avoid #files#
(setq create-lockfiles nil)
;; avoid files~
(setq make-backup-files nil)

(show-paren-mode)

(global-hl-line-mode) ; I enjoy seeing the current active line

(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file) (load-file custom-file))

(require 'package)

(setq package-archives
      '(
	("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package use-package-hydra)

(use-package doom-themes
  :custom
  (doom-theme-enable-bold t)
  (doom-theme-enable-italic t)
  :config
  (load-theme 'doom-one t))

(use-package evil
  :disabled
  :bind (:map evil-motion-state-map
	      (";" . evil-ex)
	      (":" . evil-repeat-find-char))
  :custom
  (evil-want-keybinding nil)
  :init
  (evil-mode))

(use-package evil-surround
  :disabled
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :disabled
  :after evil
  :config
  (evil-collection-init))

(use-package evil-escape
  :disabled
  :after evil
  :custom
  (evil-escape-key-sequence "fd")
  (evil-escape-delay 0.125)
  :config
  (evil-escape-mode))

(use-package paredit
  :disabled
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  (eval-expression-minibuffer-setup . enable-paredit-mode)
  (ielm-mode . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  (lisp-interaction-mode . enable-paredit-mode)
  (scheme-mode . enable-paredit-mode))

(use-package git-modes)

(use-package magit
  :bind ("C-x g" . magit-status)
  ("C-c g" . magit-file-dispatch))

(use-package company
  :defer 3
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 1)
  :config
  (global-company-mode t))

(use-package company-box
  :disabled t
  :hook (company-mode . company-box-mode))

(use-package swiper
  :bind (("C-s" . swiper)
	 ("C-r" . swiper-isearch-backward)
	 ("C-c s s" . swiper)))

(use-package ivy
  :bind (("C-c C-r" . ivy-resume)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view))
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
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-;" . hydra-window/body)
         ("<M-return>" . ace-window))
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

(use-package modern-cpp-font-lock
  :hook
  (c++-mode . modern-c++-font-lock-mode))

(use-package projectile
  :custom
  (projectile-indexing-method 'alien)
  (projectile-require-project-root t)
  (projectile-completion-system 'ivy)
  (projectile-project-search-path '("~/projects"))
  (projectile-enable-cmake-presets 't)
  :bind (:map projectile-mode-map
	      (("C-c p" . projectile-command-map)))
  :init
  (projectile-global-mode))

(use-package flycheck)

(use-package lsp-treemacs)

(use-package lsp-ivy)

(use-package lsp-ui
  :after lsp-mode)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-which-key-integration t))

(use-package dap-mode)

(use-package format-all
  :commands format-all-buffer
  :bind (("C-c f" . format-all-buffer))
  :config
  (define-format-all-formatter clang-format-9
    (:executable "clang-format-9")
    (:install)
    (:languages "C" "C#" "C++" "GLSL" "Java" "Objective-C" "Protocol Buffer")
    (:format
     (format-all--buffer-easy
      executable))))


(use-package cmake-mode)

(use-package rmsbolt)

;; org section

(use-package org
  :bind (("C-c o a" . org-agenda)
         ("C-c o c" . org-capture))
  :custom
  (org-directory (expand-file-name "~/org"))
  (org-default-notes-file "~/org/tasks.org")
  (org-agenda-files `(,org-directory))
  :config
  (add-to-list 'org-capture-templates
               '("t" "Personal Task" entry
                 (file "~/org/tasks.org")
                 "* TODO %?" :empty-lines 1))
  (add-to-list 'org-capture-templates
               '("w" "Work-related Task" entry
                 (file "~/org/roam/work/tasks.org")
                 "* TODO %?" :empty-lines 1)))

(use-package evil-org
  :disabled
  :after org
  :hook
  (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-journal
  :after org
  :bind
  (("C-c o j n" . org-journal-new-entry))
  :custom
  (org-journal-dir (expand-file-name "~/org/journal"))
  (org-journal-file-type 'weekly)
  (org-journal-enable-agenda-integration t)
  (org-journal-file-format "%Y-%m-%d.org"))

(use-package org-roam
  :after org
  :bind
  (("C-c o r f" . org-roam-node-find)
   ("C-c o r r" . org-roam-node-random)
   (:map org-mode-map
         (("C-c o r l" . org-roam-buffer-toggle)
          ("C-c o r i" . org-roam-node-insert)
          ("C-c o r f" . org-roam-node-find)
          ("C-c o r c" . org-roam-capture)
          ("C-c o r a" . org-roam-alias-add))))
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-v2-ack t)
  (org-roam-directory (expand-file-name "~/org/roam"))
  (org-roam-prefer-id-links t)
                                        ;(org-roam-capture-templates
                                        ; `(("d" "default" plain (function org-roam-capture--get-point)
                                        ;    "%?"
                                        ;    :file-name "%<%Y%m%d>-${slug}"
                                        ;    :head "#+title: ${title}\n"
                                        ;    :unnarrows t)
                                        ;   ("p" "people" plain (function org-roam-capture--get-point)
                                        ;    "%?"
                                        ;    :file-name "${slug}"
                                        ;    :head "#+title: ${title}\n"
                                        ;    :unnarrows t)))
  (org-roam-v2-ack t)
  :config
  (require 'org-roam-dailies)
  (require 'org-protocol)
  (require 'org-roam-protocol)
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package deft
  :custom
  (deft-default-extension "org"))

(use-package org-bullets
  :disabled t
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

(use-package olivetti
  :disabled t
  :custom
  (olivetti-body-width 80)
  :hook
  (text-mode . olivetti-mode))


(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package all-the-icons
  :if (display-graphic-p))


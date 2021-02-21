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

(straight-use-package 'use-package)

(use-package magit
  :straight t)

(use-package company
  :straight t)

(use-package swiper
  :straight t)

(use-package ivy
  :straight t)

(use-package hydra
  :straight t)

(use-package avy
  :straight t)

(use-package which-key
  :straight t)

(use-package doom-modeline
  :straight t
  :hook
  (after-init . doom-modeline-mode))

(use-package treemacs
  :straight t)

(use-package ace-window
  :straight t)

(use-package markdown-mode
  :straight t)

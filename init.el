;;------------------------------------------------------------------------------
;; Package Manager
;;
;; Uses straight.el + use-package.
;;
;; Use (use-package) when adding new packages
;;
;; If there is an error with (url-retrieve-synchronously) related to a proxy. Try
;; Cloning straight.el manually first:
;;
;;   $ git clone https://github.com/raxod502/straight.el.git ~/.emacs.d/straight/repos/straight.el
;;
;; Then try starting emacs again
;;------------------------------------------------------------------------------
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

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
             :custom (straight-use-package-by-default t))

;;------------------------------------------------------------------------------
;; Packages
;;------------------------------------------------------------------------------
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  (mkdir "~/roam-notes" t)
  (setq org-roam-directory (file-truename "~/roam-notes"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode)
  )

(use-package evil
             :init (setq evil-want-C-i-jump nil)
             :config (evil-mode))

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :init
  (marginalia-mode))


(use-package magit)

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))

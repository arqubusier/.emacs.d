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

(require 'use-package-ensure)
(setq use-package-always-ensure t)



;;
;; evil
;;
(use-package undo-fu)
(use-package evil
             :init
	     (setq evil-want-C-i-jump nil)
	     (setq evil-undo-system 'undo-fu)
             :config
	     (setcdr evil-insert-state-map nil)
	     (define-key evil-insert-state-map [escape] 'evil-normal-state)
	     (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
	     (evil-mode)
	     (evil-global-set-key 'normal (kbd "SPC f") #'find-file)
	     (evil-global-set-key 'normal (kbd "SPC j") #'evil-window-next)
	     (evil-global-set-key 'normal (kbd "SPC k") #'evil-window-prev)
             (evil-global-set-key 'normal (kbd "SPC SPC") #'execute-extended-command)
             (evil-global-set-key 'normal (kbd "SPC h") #'dired-jump)
             (evil-global-set-key 'normal (kbd "SPC s") #'save-buffer)
	     (add-hook 'dired-mode-hook
		       (lambda()
			 (local-unset-key (kbd "SPC"))))
	     )
(require 'evil)

;;------------------------------------------------------------------------------
;; Org
;;------------------------------------------------------------------------------
(add-hook 'org-mode-hook #'(lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(use-package org-roam
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
  (evil-global-set-key 'normal (kbd "SPC n f") 'org-roam-node-find)
  (evil-global-set-key 'normal (kbd "SPC n i") 'org-roam-node-insert)
  )
(require 'org-roam)

;;------------------------------------------------------------------------------
;; completion
;;------------------------------------------------------------------------------
(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))

  (evil-global-set-key 'normal (kbd "SPC b") 'consult-buffer)
  (evil-global-set-key 'normal (kbd "SPC d") 'consult-find)
  (evil-global-set-key 'normal (kbd "SPC g") 'consult-grep)
  (evil-global-set-key 'normal (kbd "SPC r") 'consult-ripgrep)
)
(require 'consult)

(use-package orderless
  :custom (completion-styles '(orderless)))

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (corfu-global-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))



;;------------------------------------------------------------------------------
;; Coding
;;------------------------------------------------------------------------------


;; Add this to .dir-locals.el of your project
;; ((c++-mode
;;  (eval add-hook 'before-save-hook #'clang-format-buffer nil t)))

(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (interactive)
  (add-hook 'before-save-hook
            (lambda ()
	      (message "running clang-format")
              (when (locate-dominating-file "." ".clang-format")
                (clang-format-buffer))
              ;; Continue to save.
              nil)
            nil
            ;; Buffer local hook.
            t))

(use-package clang-format
  :config
)
(require 'clang-format)

;; On ubuntu:
;;   # snap install universal-ctags
;; Gnu Global uses ctags (universal-ctags)
;;   # apt install global
;;   $ pip install pygments
(use-package ggtags
  :config
  (add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))
  )
  

(use-package magit
  :config
  (evil-global-set-key 'normal (kbd "SPC v") 'consult-buffer)
  )
(use-package solarized-theme
  :config
  (load-theme 'solarized-light t))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  ;;(setq lsp-clangd-binary-path "/usr/bin/clangd")
  (setq lsp-clangd-binary-path "lsp-clients-clangd-args")
  (setq lsp-log-io t)
  (setq lsp-clients-clangd-args '("--compile-commands-dir=/home/lundkhe/work/vihalp/1846/profile_linux_gcc8_preinstalled_debug"))
  :hook (
         (c++-mode . lsp)
	 )
  :commands lsp)

;;(use-package eglot
;;  :config
;;  (setq eglot-connect-timeout 60)
;;  (add-to-list 'eglot-server-programs
;;	       '(c++-mode . ("clangd"
;;			     "-log=verbose"
;;                           "-j=4")))
;;  
;; (add-hook 'c++-mode-hook 'eglot-ensure))

(use-package project) ;; For eglot
(use-package ag)
(use-package xterm-color
  :config
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'my/advice-compilation-filter))
(use-package realgud)
(use-package rust-mode)
(use-package cmake-mode)

;;------------------------------------------------------------------------------
;; Misc
;;------------------------------------------------------------------------------

(global-set-key (kbd "<mouse-7>") #'(lambda ()
                                     (interactive)
                                     (scroll-left 7)))
(global-set-key (kbd "<mouse-6>") #'(lambda ()
                                     (interactive)
                                     (scroll-right 7)))

(use-package multi-term)
(use-package hideshow 
  :config
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
		 "<!--\\|<[^/>]*[^/]>"
		 "-->\\|</[^/>]*[^/]>"

		 "<!--"
		 sgml-skip-tag-forward
		 nil)))

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval add-hook 'before-save-hook #'clang-format-buffer nil t)))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package dired-hacks-utils)
(use-package dired-subtree
  :bind (:map dired-mode-map
	 ("b" . dired-subtree-toggle)
         ))

(require 'dired-subtree)

(recentf-mode 1)
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 100)
(run-at-time nil (* 5 60) 'recentf-save-list)

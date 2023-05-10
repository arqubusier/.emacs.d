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
(defvar native-comp-deferred-compilation-deny-list nil) 
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

(straight-use-package 'org)

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
(add-hook 'org-mode-hook 'org-display-inline-images)
(setq org-startup-with-inline-images t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))
  
(use-package org-download
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (buffer-file-name)
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "import" nil nil nil filename)
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

(use-package sqlite3)
(use-package emacsql)
(use-package emacsql-sqlite3)
(use-package emacsql-sqlite)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/roam-notes"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)

  (evil-global-set-key 'normal (kbd "SPC n f") 'org-roam-node-find)
  (evil-global-set-key 'normal (kbd "SPC n i") 'org-roam-node-insert)
  )

(use-package org-journal
  :init
  (setq org-element-use-cache nil)
  :config
  (evil-global-set-key 'normal (kbd "SPC n d") 'org-journal-new-entry)
  (evil-global-set-key 'normal (kbd "SPC n j") 'org-journal-next-entry)
  (evil-global-set-key 'normal (kbd "SPC n k") 'org-journal-previous-entry)
  (evil-global-set-key 'normal (kbd "SPC n s") 'org-journal-search-entry)
  )


(require 'iso-transl)
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
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key '(:debounce 0.2 any)
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  ; (setq consult-narrow-key "<") ;; (kbd "C-+")

  (setq consult-project-function #'consult--default-project-function)

  (evil-global-set-key 'normal (kbd "SPC b") 'consult-buffer)
  (evil-global-set-key 'normal (kbd "SPC d") 'consult-find)
  (evil-global-set-key 'normal (kbd "SPC g") 'consult-grep)
  (evil-global-set-key 'normal (kbd "SPC r") 'consult-ripgrep)
  (evil-global-set-key 'normal (kbd "SPC x c") #'project-compile)
  (evil-global-set-key 'normal (kbd "SPC x b") #'conan-build)
  (evil-global-set-key 'normal (kbd "SPC x i") #'conan-install)
)
(require 'consult)

(use-package orderless
  :custom (completion-styles '(orderless)))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; You may want to enable Corfu only for certain modes.
  :config
  :hook ((python-mode . corfu-mode)
	 (c-mode . corfu-mode)
	 (lisp-mode . corfu-mode)
	 (c++-mode . corfu-mode)
	 )

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  )

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
  (setq tab-always-indent 'complete)
  ;; First indent then complete
  ;; Force tab to use emacs generic tab function instead of language mode specific
  ;; tab functions
  (global-set-key (kbd "<f5>")
		  #'(lambda()
		      (interactive)
		      (pop-to-buffer "*terminal<1>*")))
  (global-set-key (kbd "<f6>")
		  #'(lambda()
		      (interactive)
		      (pop-to-buffer "*terminal<2>*")))
  (global-set-key (kbd "<f7>")
		  #'(lambda()
		      (interactive)
		      (pop-to-buffer "*terminal<3>*")))
  (global-set-key (kbd "<f8>")
		  #'(lambda()
		      (interactive)
		      (pop-to-buffer "*terminal<4>*")))
  (global-set-key (kbd "<f9>")
		  #'(lambda()
		      (interactive)
		      (pop-to-buffer "*compilation*")))
  ;;:bind (
  ;;	 :map c-mode-base-map
  ;;	 ("TAB" . indent-for-tab-command)
  ;;	 )
  )



;;------------------------------------------------------------------------------
;; Coding
;;------------------------------------------------------------------------------
(defun project-debug ()
  (interactive)
  (let ((default-directory (vc-root-dir)))
  (realgud:gdb)
  )
  )

(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(use-package flyspell
  :config
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'org-mode-hook 'turn-on-flyspell)
  )
(require 'flyspell)

(defun my-c-common-mode-hook ()
 (define-key evil-normal-state-local-map
 (kbd "SPC o") 'ff-find-other-file)
 (setq indent-tabs-mode nil)
 (setq display-fill-column-indicator-column 120)
 (display-fill-column-indicator-mode)
 (google-set-c-style)
 (google-make-newline-indent)
  )
(use-package google-c-style)
(setq c-default-style "google")
(add-hook 'c-mode-common-hook #'my-c-common-mode-hook)

;; Add this to .dir-locals.el of your project
;; ((c++-mode
;;  (eval add-hook 'before-save-hook #'clang-format-buffer nil t)))

(defun clang-format-save-hook-for-this-buffer ()
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
  

(use-package magit)
(use-package solarized-theme
  :config
  (load-theme 'solarized-light t))

;; Symlink compile-commands.json to project root
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  ;;(setq lsp-clangd-binary-path "/usr/bin/clangd")
  (setq lsp-clangd-binary-path "lsp-clients-clangd-args")
  (setq lsp-log-io t)
  ;;(setq lsp-clients-clangd-args '("--compile-commands-dir=profile_linux_gcc8_preinstalled_debug"))
  :hook (
         (c++-mode . lsp)
	 )
  :commands lsp
  :config
  (org-add-hook 'lsp-mode-hook
		(lambda ()
		  (define-key evil-normal-state-local-map
                    (kbd "SPC l d") 'lsp-find-definition)
		  (define-key evil-normal-state-local-map
		    (kbd "SPC l r") 'lsp-find-references)
		  ))
  )
(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  )

(use-package treemacs)
(use-package lsp-treemacs)

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
(use-package rust-mode)

(use-package xterm-color
  :config
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'my/advice-compilation-filter))
(use-package realgud)
(use-package rust-mode)
(use-package cmake-mode)
(use-package yaml-mode)
(use-package yasnippet
  :config
(add-hook 'prog-mode-hook #'yas-minor-mode)
(setq yas-snippet-dirs '( "~/.emacs.d/snippets" ))
(yas-reload-all)
  )

(defun compilation-done-notify (buffer desc)
  (notifications-notify
   :title "Compilation Done!"
   :body (message "%s %s" buffer desc)
   )
  )
(add-hook 'compilation-finish-functions #'compilation-done-notify)
;;------------------------------------------------------------------------------
;; Misc
;;------------------------------------------------------------------------------
(global-visual-line-mode 1)
;; (setq desktop-path '("~/.emacs.d/"))
;; (setq desktop-dirname "~/.emacs.d/")
;; (setq desktop-base-file-name "emacs-desktop")
;; (desktop-save-mode)
;; (setq desktop-buffers-not-to-save
;;      (concat "\\("
;;              "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
;;              "\\|\\.emacs.*\\|\\.bbdb"
;;         "\\)$"))
;; (add-to-list 'desktop-modes-not-to-save 'dired-mode)
;; (add-to-list 'desktop-modes-not-to-save 'Info-mode)
;; (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
;; (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

(global-set-key (kbd "<mouse-7>") #'(lambda ()
                                     (interactive)
                                     (scroll-left 7)))
(global-set-key (kbd "<mouse-6>") #'(lambda ()
                                     (interactive)
                                     (scroll-right 7)))

(use-package visual-fill-column
  :config
 (setq visual-fill-column-width 120)
 (setq-default visual-fill-column-center-text t)
 )

(defun my/org-present-start ()
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (org-display-inline-images)
  (org-hide-block-all)
  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defun my/org-present-end ()
  (menu-bar-mode 1)
  (tool-bar-mode 1)
  (scroll-bar-mode 1)
  (org-show-block-all)
  ;; Stop centering the document
  (visual-fill-column-mode 0)
  (visual-line-mode 0))

(use-package org-present
:config
  (add-hook 'org-present-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map
                (kbd "SPC n") 'org-present-next)
              (define-key evil-normal-state-local-map
		(kbd "SPC p") 'org-present-prev)
              (define-key evil-normal-state-local-map
		(kbd "SPC q") 'org-present-quit)
	      ))
  (add-hook 'org-present-mode-hook 'my/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'my/org-present-end)
  )

(use-package smartparens)

(setq term-buffer-maximum-size 20000) 
(use-package multi-term
  :config
  (add-hook 'term-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map
                (kbd "SPC l") 'term-line-mode)
              (define-key evil-normal-state-local-map
		(kbd "SPC c") 'term-char-mode)
	    )))

(require 'multi-term)

(straight-use-package
'(aweshell :type git :host github :repo "manateelazycat/aweshell"))
(require 'aweshell)

(use-package tramp)

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
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(display-buffer-alist
   '(("\\(?:.*shell\\)\\|\\(?:\\*grep\\)\\|\\(?:\\*compilation\\*\\)\\|\\(?:\\*terminal<[0-9]+>\\*\\)" display-buffer-in-side-window
      (side . bottom)
      (slot . 0)
      (window-height . 10))
     ("\\*help\\*" display-buffer-in-side-window
      (side . right)
      (slot . 0)
      (window-width . 80))))
 '(safe-local-variable-values
   '((eval setq-local conan-install-command #'conan-install-poetry)
     (eval setq-local conan-build-command #'conan-build-poetry)
     (eval setq-local conan-build-format
	   (lambda "build with conan + poetry"
	     (profile)
	     (message "hej")))
     (eval add-hook 'before-save-hook #'clang-format-buffer nil t)))
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

(use-package pikchr-mode)

(defun show-image-dimensions-in-mode-line ()
  (interactive)
  (let* ((image-dimensions (image-size (image-get-display-property) :pixels))
         (width (car image-dimensions))
         (height (cdr image-dimensions)))
    (setq mode-line-buffer-identification
          (format "%s %dx%d" (propertized-buffer-identification "%12b") width height))))

(add-hook 'image-mode-hook #'show-image-dimensions-in-mode-line)

(setq make-backup-files nil)

(use-package bookmark+
  :config 
  (evil-global-set-key 'normal (kbd "SPC x x") 'bookmark-jump)
  (evil-global-set-key 'normal (kbd "SPC x s") 'bookmark-set)
  (evil-global-set-key 'normal (kbd "SPC x l") 'bookmark-set)
  (evil-global-set-key 'normal (kbd "SPC x t") 'bookmark-tag)
  )

(setq dired-mouse-drag-files t)

(setq switch-to-buffer-obey-display-actions t)

(use-package which-key)
(require 'which-key)
(which-key-mode)

(use-package markdown-mode
  :config
  (setq makrdown-command "/usr/bin/pandoc"))


(require 'notifications)
(use-package alert)
(require 'alert)

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
	elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-x w" . elfeed ))

;; Used for configuration of elfeed using org-files
(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

(use-package rg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sick
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun conan-build-poetry (profile)
(format "cd %s && poetry run conan build .. " profile)
    )
(defun conan-install-poetry (profile)
    (format "mkdir -p %s && cd %s && poetry run conan install -pr %s --update --build missing ..  -e CMAKE_EXPORT_COMPILE_COMMANDS=ON -o create_and_run_unit_tests=True" profile profile profile))

(defun conan-build-default (profile)
(format "euler devshell --commands='cd %s && conan build .. ; exit'" profile)
    )
(defun conan-install-default (profile)
(format "euler devshell --commands='mkdir -p %s && cd %s &&  conan install -pr %s --update --build missing .. -e CMAKE_EXPORT_COMPILE_COMMANDS=ON ; exit'" profile profile profile))

(defvar conan-build-command #'conan-build-default)
(defvar conan-install-command #'conan-install-default)

(defun conan-profiles ()
 (split-string (shell-command-to-string "conan profile list")))

(defun conan-build (arg)
  (interactive
   (list
    (completing-read "conan profile " (conan-profiles))))
  (message arg)
  (let ((default-directory (vc-root-dir)))
  (compile (funcall conan-build-command arg)))
  )

(defun conan-install (arg)
  (interactive
   (list
    (completing-read "conan profile " (conan-profiles))))
  (message arg)
  (let ((default-directory (vc-root-dir)))
  (compile (funcall conan-install-command arg)))
  )

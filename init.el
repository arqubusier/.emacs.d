(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

(setq-default word-wrap t)


;; Spelling

(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                c-mode-common-hook
                python-mode-hook
                shell-mode-hook
                LaTeX-mode-hook))
  (add-hook hook 'flyspell-prog-mode))

                                        ;
;; Save session
(desktop-save-mode 1)
(savehist-mode 1)

;; Copy paste
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)
(setq mouse-yank-at-point t)

;; Gui elements
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Which function
(which-function-mode)
(setq which-func-unknown "n/a")
;; Show the current function name in the header line
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
            ;; We remove Which Function Mode from the mode line, because it's mostly
            ;; invisible here anyway.
            (assq-delete-all 'which-func-mode mode-line-misc-info))


;; Solarized
;(load-theme 'solarized-light t)
(load-theme 'solarized-dark t)

;; Clang format
(defun format-and-save()
  "Format current buffer using clang-format, then save."
  (interactive)
  (clang-format-buffer)
  (save-buffer))

;; Compilation
; only move between errors.
(setq compilation-skip-threshold 2)
;(add-to-list 'compilation-error-regexp-alist 'build-helper-error0)
;(add-to-list 'compilation-error-regexp-alist-alist
;         '(build-helper-error0 "\\([^ ]+\\):\\([0-9]+\\):\\([0-9]+\\): error:" 1 2 3 2))


;
; C/C++
;
(setq-default indent-tabs-mode nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(custom-safe-themes
   (quote
    ("c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" default)))
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(package-selected-packages
   (quote
    (dap-mode solarized-theme flycheck lsp-ui helm-lsp helm-xref lsp-mode magit clang-format helm-projectile zzz-to-char projectile fill-column-indicator yasnippet volatile-highlights helm-gtags evil company clojure-mode)))
 '(safe-local-variable-values
   (quote
    ((reftex-default-bibliography "../bibliography.bib")
     (TeX-master . "../TechnicalReference_VectorAdaptiveCommonLibrary")
     (gud-gdb-command-name . "/home/vsarchelu/amsr-mono/adaptive-microsar/builds/native/amsr-vector-fs-libvac/test/gtest_libvac_test")))))

(defconst helu-style
  '("gnu"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "helu" helu-style)
(setq c-default-style "helu")


(defun my-c-hook ()
  (setq fill-column 120))

(defun my-c++-hook ()
  (setq fill-column 120))


(add-hook 'c-mode-hook 'my-c-hook)
(add-hook 'c++-mode-hook 'my-c++-hook)


;
; Org Mode
;
;(defun org-config () ((make-local-variable 'evil-auto-indent) (setq evil-auto-indent nil)))
;(remove-hook 'org-mode-hook 'org-config)

(setq org-todo-keywords
      '((sequence "TODO" "REVIEW" "|" "DONE")))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/notes/todo.org" "Tasks")
         "* TODO %?\n  \n")
        ("j" "Job" entry (file+headline "~/notes/job.org" "Tasks")
         "* TODO %?\n  \n")
        ("k" "Job Note" entry (file+headline "~/notes/job-notes.org" "Job Notes")
                "* %?\n  \n")
        ))
;
; Evil
;
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(require 'helm-config)
(helm-mode 1)
;(semantic-mode 1)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

; bindings and functions
(defun arm-gdb () "Run arm-none-eabi-gdb." (interactive (gdb "arm-none-eabi-gdb -i=mi")))
(defun gdb-custom () "Run gdb with 'gdb-args'." (interactive (gdb (compile-command))))

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x e") 'eval-buffer)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h x") 'helm-register)

(defun myprevious-window ()
    (interactive)
    (other-window -1))

(with-eval-after-load 'evil-maps
    (define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "C-,") 'company-complete)
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd ",") nil)
    (define-key evil-motion-state-map (kbd "SPC SPC") 'execute-extended-command)

    (define-key evil-motion-state-map (kbd "SPC a") 'compile)
    (define-key evil-motion-state-map (kbd "SPC b") 'helm-mini)
    (define-key evil-motion-state-map (kbd "SPC c") 'format-and-save)
    (define-key evil-motion-state-map (kbd "SPC d") 'lsp-find-definition)
    (define-key evil-motion-state-map (kbd "SPC e") 'eval-buffer)
    (define-key evil-motion-state-map (kbd "SPC f") 'next-error)
    (define-key evil-motion-state-map (kbd "SPC g") 'magit-file-dispatch)
    (define-key evil-motion-state-map (kbd "SPC j") 'other-window)
    (define-key evil-motion-state-map (kbd "SPC k") 'myprevious-window)
    (define-key evil-motion-state-map (kbd "SPC m n") 'smerge-next)
    (define-key evil-motion-state-map (kbd "SPC m p") 'smerge-previous)
    (define-key evil-motion-state-map (kbd "SPC m RET") 'smerge-keep-current)
    (define-key evil-motion-state-map (kbd "SPC m m") 'smerge-keep-mine)
    (define-key evil-motion-state-map (kbd "SPC m o") 'smerge-keep-other)
    (define-key evil-motion-state-map (kbd "SPC o") 'org-capture)
    (define-key evil-motion-state-map (kbd "SPC p") 'projectile-command-map)
    (define-key evil-motion-state-map (kbd "SPC q") 'magit-blame-quit)
    (define-key evil-motion-state-map (kbd "SPC r") 'lsp-find-references)
    (define-key evil-motion-state-map (kbd "SPC s") 'save-buffer)
    (define-key evil-motion-state-map (kbd "SPC v") 'magit-status)
    (define-key evil-motion-state-map (kbd "SPC x") 'xref-find-apropos)
    (define-key evil-motion-state-map (kbd "SPC y") 'xah-copy-file-path)
)
;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'fill-column-indicator)
(setq fci-rule-column 120)


;;
;; Projectile
;;
(require 'projectile)
(global-set-key (kbd "C-x p") 'projectile-command-map)
(projectile-mode +1)

;;
;; Helm-projectile
;;
(require 'helm-projectile)
(helm-projectile-on)
;; helm-xref
(require 'helm-xref)
(require 'helm-lsp)

;;
;; Line numbers
;;
(global-display-line-numbers-mode)

;;
;; Company
;;
(add-hook 'after-init-hook 'global-company-mode)


;; Helm-gtags
;(add-hook 'c-mode-hook 'helm-gtags-mode)
;(add-hook 'c++-mode-hook 'helm-gtags-mode)


;; lsp
(require 'lsp-mode)
  :config
    ;; `-background-index' requires clangd v8+!
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))
(setq lsp-clients-clangd-executable (if (file-exists-p "/usr/bin/clangd-9")
                                        "/usr/bin/clangd-9"
                                      "/usr/bin/clangd"))
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)

;; Company lsp
(require 'company-lsp)
  :config
  (push 'company-lsp company-backends)

   ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil)

;; (setq lsp-enable-on-type-formatting nil)
(setq clang-format-style "file")

;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)


;; org2anki
(defun print-headings ()
  "print all headings in current buffer (of org mode).
2019-01-14"
  (interactive)
  (with-output-to-temp-buffer "*xah temp out*"
    (org-element-map (org-element-parse-buffer) 'paragraph
      (lambda (x)
        (princ (org-element-interpret-data x))
        (terpri )))))

        ;(princ (org-element-property :raw x))

(defun org-test ()
  "print all headings in current buffer (of org mode).
2019-01-14"
  (interactive)
  (with-output-to-temp-buffer "*xah temp out*"
    (org-element-parse-buffer)
  )
)


;; Copy current buffer path
(defun xah-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not dired, copy value of `default-directory' (which is usually the “current” dir when that buffer was created)

URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2017-09-01"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: 「%s」" $fpath)
         $fpath )))))

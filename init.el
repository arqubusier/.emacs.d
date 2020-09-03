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
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(package-selected-packages
   (quote
    (evil-numbers flycheck lsp-ui helm-lsp helm-xref lsp-mode magit clang-format helm-projectile zzz-to-char projectile fill-column-indicator yasnippet volatile-highlights helm-gtags evil company clojure-mode))))

(defconst helu-style
  '("gnu"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "helu" helu-style)
(setq c-default-style "helu")

;
; Org Mode
;
(defun org-config () ((make-local-variable 'evil-auto-indent) (setq evil-auto-indent nil)))
(add-hook 'org-mode-hook 'org-config)

(setq org-todo-keywords
      '((sequence "TODO" "REVIEW" "|" "DONE")))


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

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x e") 'eval-buffer)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h x") 'helm-register)

(with-eval-after-load 'evil-maps
    (define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "C-,") 'company-complete)
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd ",") nil)
    (define-key evil-motion-state-map (kbd "SPC SPC") 'execute-extended-command)

    (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

    (define-key evil-motion-state-map (kbd "SPC a") 'compile)
    (define-key evil-motion-state-map (kbd "SPC b") 'helm-mini)
    (define-key evil-motion-state-map (kbd "SPC c") 'format-and-save)
    (define-key evil-motion-state-map (kbd "SPC d") 'lsp-find-definition)
    (define-key evil-motion-state-map (kbd "SPC e") 'eval-buffer)
    (define-key evil-motion-state-map (kbd "SPC f") 'next-error)
    (define-key evil-motion-state-map (kbd "SPC g") 'magit-file-dispatch)
    (define-key evil-motion-state-map (kbd "SPC m n") 'smerge-next)
    (define-key evil-motion-state-map (kbd "SPC m p") 'smerge-previous)
    (define-key evil-motion-state-map (kbd "SPC m RET") 'smerge-keep-current)
    (define-key evil-motion-state-map (kbd "SPC m m") 'smerge-keep-mine)
    (define-key evil-motion-state-map (kbd "SPC m o") 'smerge-keep-other)
    (define-key evil-motion-state-map (kbd "SPC p") 'projectile-command-map)
    (define-key evil-motion-state-map (kbd "SPC q") 'magit-blame-quit)
    (define-key evil-motion-state-map (kbd "SPC r") 'lsp-find-references)
    (define-key evil-motion-state-map (kbd "SPC s") 'save-buffer)
    (define-key evil-motion-state-map (kbd "SPC v") 'magit-status)
    (define-key evil-motion-state-map (kbd "SPC x") 'xref-find-apropos)
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

;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)


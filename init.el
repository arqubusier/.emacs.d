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
 '(package-selected-packages
   (quote
    (magit clang-format helm-projectile zzz-to-char projectile fill-column-indicator yasnippet volatile-highlights helm-gtags evil company clojure-mode))))

(defconst helu-style
  '("gnu"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "helu" helu-style)
(setq c-default-style "helu")

;
; Org Mode
;
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
    (define-key evil-motion-state-map (kbd "SPC e") 'eval-buffer)
    (define-key evil-motion-state-map (kbd "SPC SPC") 'execute-extended-command)
    ; helm
    (define-key evil-motion-state-map (kbd "SPC b") 'helm-mini)
    ; projectile
    (define-key evil-motion-state-map (kbd "SPC p") 'projectile-command-map)
    ; helm gtags
    (define-key evil-motion-state-map (kbd "SPC g C") 'helm-gtags-create-tags)
    (define-key evil-motion-state-map (kbd "SPC g t") 'helm-gtags-find-tag)
    (define-key evil-motion-state-map (kbd "SPC g r") 'helm-gtags-find-rtag)
    (define-key evil-motion-state-map (kbd "SPC g s") 'helm-gtags-find-symbol)
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

;;
;; Clang format
;;
(add-hook 'c-mode-common-hook
          (function (lambda ()
                    (add-hook 'before-save-hook
                              'clang-format-buffer))))

;;
;; Line numbers
;;
(global-display-line-numbers-mode)

;;
;; Company
;;
(add-hook 'after-init-hook 'global-company-mode)


;; Helm-gtags
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(custom-set-variables
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t))

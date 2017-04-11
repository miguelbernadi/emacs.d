;;; package --- Summary
;;; Commentary:
;;; Code:

;;; Set up melpa-stable as package source
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;; Install packages I want
(eval-when-compile
  (require 'use-package))

(use-package helm
	     :ensure t)
(use-package helm-config)
;(use-package helm-dash
;	     :ensure t)

;;; Setup Org-mode
(use-package org
  :config
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)
  ;; Languages available in SRC blocks
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . nil)
     (sh . t)))
  ;; Set agenda files
  (setq org-agenda-files (list "~/org/agenda/test.org"))
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(org-agenda-files (quote ("~/org/agenda/newgtd.org"))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   ))

(use-package flycheck
  :ensure t
  :config
  ;;; Enable flycheck for syntax checking
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package go-mode
  :ensure t
  :config
  ;;; Get PATH environment
  ;;; (https://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/)
  (defun set-exec-path-from-shell-PATH ()
    (let ((path-from-shell (replace-regexp-in-string
			    "[ \t\n]*?"
			    ""
			    (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
      (setq eshell-path-env path-from-shell) ; for eshell
      (setq exec-path (split-string path-from-shell path-separator))))

  (when window-system (set-exec-path-from-shell-PATH))

  ;;; Set go related paths
  (setenv "GOPATH" "/Users/mbernabe/go")
  (add-to-list 'exec-path "/Users/mbernabe/go/bin")

  (defun my-go-mode-hook ()
    ;; Use goimports instead of go-fmt
    (setq gofmt-command "goimports")
    ;; Call gofmt before saving
    (add-hook 'before-save-hook 'gofmt-before-save)
    ;; Customize compile command to run go build
    (if (not (string-match "go" compile-command))
	(set (make-local-variable 'compile-command)
	     "go build -v && go test -v && go vet"))
    )
  (add-hook 'go-mode-hook 'my-go-mode-hook)

  (defun auto-complete-for-go ()
    (auto-complete-mode 1))
  (add-hook 'go-mode-hook 'auto-complete-for-go))

(use-package exec-path-from-shell
	     :ensure t)
(use-package go-errcheck
	     :ensure t)
(use-package go-eldoc
	     :ensure t)
(use-package go-dlv
             :ensure t)

(use-package magit
  :ensure t
  :config
  :bind ("C-x g" . magit-status))
  
(use-package auto-complete
	    :ensure t)
(use-package go-autocomplete
            :ensure t)
(use-package dockerfile-mode
            :ensure t)

(use-package perspective
  :ensure t)

(use-package exec-path-from-shell
  :ensure t)

(use-package smtpmail
  :config
  (setq message-send-mail-function 'smtpmail-send-it
	smtpmail-auth-credentials
	'(("smtp.gmail.com" 587 "miguel.bernabeu@gmail.com" nil))
	smtpmail-stream-type 'starttls
	smtpmail-default-smtp-server "smtp.gmail.com"
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 587)
  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t))

(load-theme 'misterioso)

;;; Load configurations

;; show a clock for full screen
(display-time-mode 1)


;;; Remove tool-bar
(tool-bar-mode -1)

;; Display line and column numbers in Mode line
(setq line-number-mode t)
(setq column-number-mode t)

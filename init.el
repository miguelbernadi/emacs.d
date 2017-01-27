;;; package --- Summary
;;; Commentary:
;;; Code:

;;; Install packages I want
(load "~/.emacs.d/init-packages")

;;; Setup Org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;; Enable flycheck for syntax checking
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; Remove tool-bar
(tool-bar-mode -1)

;;; Set agenda files
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
 )

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
(add-hook 'go-mode-hook 'auto-complete-for-go)

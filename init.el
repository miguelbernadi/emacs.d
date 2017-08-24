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

;; Add locally installed package paths here
(add-to-list 'load-path "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e/")

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
    (setq gofmt-command "goreturns")
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

(use-package magit-gh-pulls
  :ensure t
  :config
  (defun endless/add-PR-fetch ()
    "If refs/pull is not defined on a GH repo, define it."
    (let ((fetch-address
	   "+refs/pull/*/head:refs/pull/origin/*")
	  (magit-remotes
	   (magit-get-all "remote" "origin" "fetch")))
      (unless (or (not magit-remotes)
		  (member fetch-address magit-remotes))
	(when (string-match
	       "github" (magit-get "remote" "origin" "url"))
	  (magit-git-string
	   "config" "--add" "remote.origin.fetch"
	   fetch-address)))))
  (add-hook 'magit-mode-hook #'endless/add-PR-fetch)
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

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

(use-package markdown-mode
  :ensure t)

(use-package mu4e
  :config
  ;; Set mu4e as default mail client for emacs
  (setq mail-user-agent 'mu4e-user-agent)
  ;; Mu4e program settings
  (setq mu4e-mu-binary "/usr/local/bin/mu"
	mu4e-get-mail-command "offlineimap"
	mu4e-update-interval 300) ;; seconds between auto-update

  ;; Folder settings and behaviour
  (setq	mu4e-maildir "~/Mail"
	mu4e-drafts-folder "/[Gmail].Drafts"
	mu4e-sent-folder   "/[Gmail].Sent Mail"
	mu4e-trash-folder  "/[Gmail].Trash"
	;; don't save message to Sent Messages, Gmail/IMAP takes care
	;; of this (See the documentation for
	;; `mu4e-sent-messages-behavior' if you have additional
	;; non-Gmail addresses and want assign them different
	;; behavior.)
	mu4e-sent-messages-behavior 'delete)

  ;; Navigation settings
  
  ;; setup some handy shortcuts you can quickly
  ;; switch to your Inbox -- press ``ji'' then, when you want archive
  ;; some messages, move them to the 'All Mail' folder by pressing
  ;; ``ma''.
  (setq mu4e-maildir-shortcuts
	'( ("/INBOX"               . ?i)
	   ("/[Gmail].Sent Mail"   . ?s)
	   ("/[Gmail].Trash"       . ?t)
	   ("/[Gmail].All Mail"    . ?a)))

  ;; This variable can be used to configure the bookmarks in the main view
  (setq mu4e-bookmarks
	`( ,(make-mu4e-bookmark
	     :name  "Unread messages"
	     :query "flag:unread AND NOT flag:trashed"
	     :key ?u)
	   ,(make-mu4e-bookmark
	     :name "Today's messages"
	     :query "date:today..now"
	     :key ?t)
	   ,(make-mu4e-bookmark
	     :name "Last 7 days"
	     :query "date:7d..now"
	     :key ?w)
	   ,(make-mu4e-bookmark
	     :name "Flagged in INBOX"
	     :query "maildir:\"/INBOX\" and flag:flagged"
	     :key ?f)))

  ;; Editor settings
  ;; every new email composition gets its own frame! (window)
  (setq mu4e-compose-in-new-frame t
	;; Avoid self replies in Reply to all
	mu4e-compose-dont-reply-to-self t)

  ;; User personal information
  (setq mu4e-user-mail-address-list '("miguel.bernabeu@devex.com")
	user-mail-address "miguel.bernabeu@gmail.com"
	user-full-name  "Miguel Bernabeu"
	mu4e-compose-signature
	(concat
	 "Miguel Bernabeu\n"
	 "Infrastructure Engineer @ Devex\n"))

  ;; Headers View settings
  ;; Set date format
  (setq mu4e-headers-date-format "%Y-%m-%d %H:%M"
  ;; Set fields in Headers view
	mu4e-headers-fields
	'( (:human-date     .  22)
	   (:flags          .   6)
	   (:mailing-list   .  10)
	   (:from-or-to     .  22)
	   (:thread-subject .  nil)))

  ;; Message View settings
  ;; show full addresses instead of just names
  ;; toggle per name with M-RET
  (setq mu4e-view-show-addresses 't)

  ;; Enable org-mode capture for messages
  (require 'org-mu4e))

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
  (setq message-kill-buffer-on-exit t)
  ;; Configure queue mode for offline mail queueing
  (setq smtpmail-queue-mail nil  ;; start in queuing mode
	smtpmail-queue-dir   "~/Mail/queue/cur")
  )

(load-theme 'misterioso)

;;; Load configurations

;; show a clock for full screen
(display-time-mode 1)


;;; Remove tool-bar
(tool-bar-mode -1)

;; Display line and column numbers in Mode line
(setq line-number-mode t)
(setq column-number-mode t)

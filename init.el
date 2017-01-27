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
(setq org-agenda-files (list "~/org/test.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/org/newgtd.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

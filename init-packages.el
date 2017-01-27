;;; init-packages - Install packages in Emacs

;;; Set up melpa-stable as package source
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
;;; list packages to be installed
(setq package-list
      '(;;; syntax highlighting
	flycheck

	;;; golang
	go-mode
        exec-path-from-shell

	;;; version control
	magit

	))

;;; activate all the packages
(package-initialize)

;;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


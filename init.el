(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(require 'cl)

;; Guarantee all packages are installed on start
(defvar packages-list
  '(ac-nrepl
    auto-complete
    color-theme-sanityinc-tomorrow
    clojure-mode
    nrepl
    starter-kit)
  "List of packages to be installed at launch.")

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))

(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

;; Auto-Complete
(require 'auto-complete)

;; Clojure
(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))

;; Emacs Lisp
(defun ac-sources-elisp ()
  (setq ac-sources '(ac-source-symbols
		     ac-source-variables
		     ac-source-functions
		     ac-source-filename
		     ac-source-words-in-buffer)))
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)
(add-hook 'emacs-lisp-mode-hook 'ac-sources-elisp)

;; Emacs Starter Kit
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(remove-hook 'prog-mode-hook 'esk-pretty-lambdas)

;; Settings
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq temporary-file-directory "~/.emacs-backup")
(color-theme-sanityinc-tomorrow-bright)
(global-set-key "\C-x\C-b" 'ibuffer)

;; Winner Mode
(winner-mode 1)
(global-set-key "\C-c\C-_" 'winner-undo)

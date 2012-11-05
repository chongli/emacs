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
(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)
(setq ac-delay 0.5)
(setq ac-menu-height 15)
(setq ac-use-fuzzy nil)

;; Clojure
(add-hook 'clojure-mode-hook
          (lambda () (paredit-mode +1)))
(add-hook 'nrepl-mode-hook
          (lambda () (progn
                       (paredit-mode +1)
                       (enlarge-window -25)
                       (ac-nrepl-setup)
                       (auto-complete-mode))))
(add-hook 'nrepl-interaction-mode-hook
          (lambda () (progn
                       (ac-nrepl-setup)
                       (auto-complete-mode))))


;; Emacs Lisp
(defun ac-sources-elisp ()
  (setq ac-sources '(ac-source-symbols
		     ac-source-variables
		     ac-source-functions
		     ac-source-filename
		     ac-source-words-in-buffer)))
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))
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
(setq temporary-file-directory "~/.emacs.d/temp")
(color-theme-sanityinc-tomorrow-bright)
(global-set-key "\C-x\C-b" 'ibuffer)

;; Winner Mode
(winner-mode 1)
(global-set-key "\C-c\C-_" 'winner-undo)

;; XSel Support
;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; I prefer using the "clipboard" selection (the one the
;; typically is used by c-c/c-v) before the primary selection
;; (that uses mouse-select/middle-button-click)
(setq x-select-enable-clipboard t)

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
(unless window-system
  (when (getenv "DISPLAY")
    ;; Callback for when user cuts
    (defun xsel-cut-function (text &optional push)
      ;; Insert text to temp-buffer, and "send" content to xsel stdin
      (with-temp-buffer
        (insert text)
        ;; I prefer using the "clipboard" selection (the one the
        ;; typically is used by c-c/c-v) before the primary selection
        ;; (that uses mouse-select/middle-button-click)
        (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
    ;; Call back for when user pastes
    (defun xsel-paste-function ()
      ;; Find out what is current selection by xsel. If it is different
      ;; from the top of the kill-ring (car kill-ring), then return
      ;; it. Else, nil is returned, so whatever is in the top of the
      ;; kill-ring will be used.
      (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
        (unless (string= (car kill-ring) xsel-output)
          xsel-output)))
    ;; Attach callbacks to hooks
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)))

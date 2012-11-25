(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(require 'cl)

;; Guarantee all packages are installed on start
(defvar packages-list
  '(ac-nrepl
    auto-complete
    color-theme-sanityinc-tomorrow
    clojure-mode
    glsl-mode
    haskell-mode
    nrepl
    rainbow-delimiters
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

;; Utility
(defun add-hooks (hook fns)
  (mapcar (apply-partially 'add-hook hook) fns))
(defun append-to-list (list elems)
  (mapcar (apply-partially 'add-to-list list) elems))
(defun turn-on-paredit-mode ()
  (paredit-mode 1))
(defun turn-on-rainbow-delimiters ()
  (rainbow-delimiters-mode 1))

;; Auto-Complete
(require 'auto-complete)
(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)
(setq ac-delay 0.5)
(setq ac-menu-height 15)
(setq ac-use-fuzzy nil)

;; Clojure
(defun set-nrepl-window-size ()
  (enlarge-window -25))
(add-hooks 'clojure-mode-hook '(turn-on-paredit-mode
                                turn-on-rainbow-delimiters))
(add-hooks 'nrepl-mode-hook '(turn-on-paredit-mode
                              set-nrepl-window-size
                              ac-nrepl-setup
                              auto-complete-mode))
(add-hooks 'nrepl-interaction-mode-hook '(ac-nrepl-setup
                                          auto-complete-mode))


;; Emacs Lisp
(defun ac-sources-elisp ()
  (setq ac-sources '(ac-source-symbols
		     ac-source-variables
		     ac-source-functions
		     ac-source-filename
		     ac-source-words-in-buffer)))
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hooks 'emacs-lisp-mode-hook '(set-newline-and-indent
                                   turn-on-eldoc-mode
                                   turn-on-paredit-mode
                                   turn-on-rainbow-delimiters
                                   auto-complete-mode
                                   ac-sources-elisp))
;; GLSL
(autoload 'glsl-mode "glsl-mode" nil t)
(append-to-list 'auto-mode-alist '(("\\.glsl\\'" . glsl-mode)
                                   ("\\.vert\\'" . glsl-mode)
                                   ("\\.frag\\'" . glsl-mode)
                                   ("\\.geom\\'" . glsl-mode)))
;; Haskell
(add-hooks 'haskell-mode-hook '(turn-on-haskell-indentation
                                turn-on-haskell-doc-mode))

;; ZSH
(defun ac-sources-zsh ()
  (setq ac-sources '(ac-source-words-in-buffer
                     ac-source-filename)))
(add-hooks 'sh-mode-hook '(auto-complete-mode
                           ac-sources-zsh))
(append-to-list 'auto-mode-alist '(("zshecl" . sh-mode)
                                         ("\\.zshrc(.local)?" . sh-mode)))

;; Settings
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(color-theme-sanityinc-tomorrow-bright)
(global-set-key "\C-x\C-b" 'ibuffer)

;; Emacs Starter Kit
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(remove-hook 'prog-mode-hook 'esk-pretty-lambdas)

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

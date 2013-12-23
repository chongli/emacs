(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'load-path
             "~/.emacs.d/plugin")
(require 'cl)

;; Guarantee all packages are installed on start
(defvar packages-list
  '(ac-nrepl
    auto-complete
    color-theme-sanityinc-tomorrow
    clojure-mode
    clojurescript-mode
    flycheck
    glsl-mode
    ghc
    haskell-mode
    js2-mode
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
(defun remove-hooks (hook fns)
  (mapcar (apply-partially 'remove-hook hook) fns))
(defun append-to-list (list elems)
  (mapcar (apply-partially 'add-to-list list) elems))
(defun turn-on-paredit-mode ()
  (paredit-mode 1))
(defun turn-on-rainbow-delimiters ()
  (rainbow-delimiters-mode 1))
(defun turn-on-electric-indentation ()
  (electric-indent-mode 1))
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))

;; Auto-Complete
(require 'auto-complete)
(setq ac-delay 0.5)
(setq ac-menu-height 15)
(setq ac-use-fuzzy 1)

;; Clojure
(defun set-nrepl-window-size ()
  (enlarge-window -5))
(defun set-nrepl-popup-stacktraces ()
  (setq nrepl-popup-stacktraces nil))
(defun remove-clojurescript-start-cljs-repl ()
  (remove-hook 'inferior-lisp-mode-hook 'clojurescript-start-cljs-repl))
(defun rhino-repl ()
  (interactive)
  (run-lisp "lein trampoline cljsbuild repl-rhino"))
(defun node-repl ()
  (interactive)
  (clojure-display-inferior-lisp-buffer)
  (comint-send-string (inferior-lisp-proc) "(require 'cljs.repl 'cljs.repl.node)\n")
  (comint-send-string (inferior-lisp-proc) "(cljs.repl/repl (cljs.repl.node/repl-env))\n"))
(defun browser-repl ()
  (interactive)
  (run-lisp "lein trampoline cljsbuild repl-launch aurora http://localhost:3000/"))
(add-hook 'clojurescript-mode-hook 'remove-clojurescript-start-cljs-repl)
(add-hooks 'clojure-mode-hook '(set-newline-and-indent
                                turn-on-paredit-mode
                                turn-on-rainbow-delimiters))
(add-hooks 'nrepl-mode-hook '(turn-on-paredit-mode
                              set-nrepl-window-size
                              set-nrepl-popup-stacktraces
                              ac-nrepl-setup
                              auto-complete-mode))
(add-hooks 'nrepl-interaction-mode-hook '(ac-nrepl-setup
                                          auto-complete-mode))

;; Ediff
(setq ediff-split-window-function 'split-window-horizontally)

;; Emacs Lisp
(defun ac-sources-elisp ()
  (setq ac-sources '(ac-source-symbols
		     ac-source-variables
		     ac-source-functions
		     ac-source-filename
		     ac-source-words-in-buffer)))
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
(add-hook 'glsl-mode-hook 'turn-on-electric-indentation)

;; Haskell
(eval-after-load 'flycheck
  '(require 'flycheck-ghcmod))
(defun turn-on-ghc-mod ()
  (ghc-init))
(defun turn-on-flycheck ()
  (flycheck-mode 1))
(add-hooks 'haskell-mode-hook '(set-newline-and-indent
                                turn-on-haskell-indent
                                turn-on-eldoc-mode
                                turn-on-haskell-doc-mode
                                turn-on-haskell-decl-scan
                                turn-on-flycheck
                                turn-on-ghc-mod))

;; ZSH
(defun ac-sources-zsh ()
  (setq ac-sources '(ac-source-words-in-buffer
                     ac-source-filename)))
(add-hooks 'sh-mode-hook '(auto-complete-mode
                           ac-sources-zsh))
(append-to-list 'auto-mode-alist '(("zshecl" . sh-mode)
                                   ("\\.zshrc(.local)?\\'" . sh-mode)))

;; Settings
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(delete-selection-mode 1)
(global-set-key "\C-x\C-b" 'ibuffer)
(set-default-font "Inconsolata-16")
(setq visible-bell nil)
(transient-mark-mode -1)
(setq set-mark-command-repeat-pop 1)

;; Emacs Starter Kit
(remove-hooks 'prog-mode-hook '(esk-turn-on-hl-line-mode
                                idle-highlight-mode
                                esk-pretty-lambdas))
;; Ido Mode
(setq ido-cannot-complete-command 'ido-next-match)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
 '(custom-safe-themes (quote ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

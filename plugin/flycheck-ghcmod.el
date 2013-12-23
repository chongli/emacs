;;; flycheck-ghcmod.el --- A flycheck checker for Haskell using ghc-mod

;; Copyright (C) 2013 Chong Li

;; Author: Chong Li <chongli@gmail.com>
;; URL: https://github.com/chongli/flycheck-ghcmod
;; Keywords: convenience languages tools
;; Package-Requires: ((flycheck "0.15"))
;; Version: DEV

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds a Flycheck syntax checker for Haskell based on ghc-mod.

;;;; Setup

;; (eval-after-load 'flycheck '(require 'flycheck-ghcmod))

;;; Code:

(require 'flycheck)

(flycheck-define-checker haskell-ghcmod
  "A Haskell syntax and type checker using ghcmod.

See URL `https://github.com/bitc/ghcmod'."
  :command ("ghc-mod" "--boundary=' '" "check" "-g" "-Wall" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            (or " " "\n ") "Warning:" (optional "\n")
            (one-or-more " ")
            (message (one-or-more not-newline)
                     (zero-or-more "\n"
                                   (one-or-more " ")
                                   (one-or-more not-newline)))
            line-end)
   (error line-start (file-name) ":" line ":" column ":"
          (or (message (one-or-more not-newline))
              (and "\n" (one-or-more " ")
                   (message (one-or-more not-newline)
                            (zero-or-more "\n"
                                          (one-or-more " ")
                                          (one-or-more not-newline)))))
          line-end))
  :modes haskell-mode
  :next-checkers ((warnings-only . haskell-hlint)))


(add-to-list 'flycheck-checkers 'haskell-ghcmod)


(provide 'flycheck-ghcmod)
;;; flycheck-hdevtools.el ends here

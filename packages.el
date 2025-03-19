;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; avy
(package! avy)

;; paredit
;; (when (package! paredit)
;;   (package! paredit-everywhere))

;; realgud-lldb
(package! realgud-lldb)

;; nginx-mode
(package! nginx-mode)

;; javascript
(disable-packages! rjsx-mode)

;; shellscript
(disable-packages! company-shell)

;; highlights the current cursor line after major movements
(package! beacon)

;; focus
(package! focus)

;; dotenv
(package! dotenv-mode)

;; magit-todos
(when (string-equal emacs-version "29.1")
  (package! magit-todos
    :recipe (:host github :repo "unipro/magit-todos" :branch "wip/obsolete_buffer-local-value")
    :pin "c24518ef461393e1d4e43f15b7c3d528cb0c89bf"))

;; copilot
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el"))
(package! copilot-chat
  :recipe (:host github :repo "chep/copilot-chat.el"))

;; claude-code
;; (if (>= emacs-major-version 30)
;;     (package! claude-code
;;       :recipe (:host github :repo "stevemolitor/claude-code.el" :branch "main"
;;                :files ("*.el" (:exclude "demo.gif")))))

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

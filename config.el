;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Bootstrap config
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-win-nt* (eq system-type 'windows-nt))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Byungwan Jun"
      user-mail-address "unipro.kr@gmail.com")

;; Default frame size
(add-to-list 'default-frame-alist '(height . 70))
(add-to-list 'default-frame-alist '(width . 204))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(when (display-graphic-p)
  (let ((fontset "fontset-default"))
    (cond ((member "Droid Sans Mono" (font-family-list))
           (setq doom-font (font-spec :family "Droid Sans Mono")))
          ((member "DejaVu Sans Mono" (font-family-list))
           (setq doom-font (font-spec :family "DejaVu Sans Mono")))
          (t
           (message "'Droid Sans Mono' or 'DejaVu Sans Mono' are not installed")))
    (cond ((member "D2Coding" (font-family-list))
           (set-fontset-font fontset 'hangul
                             '("D2Coding" . "unicode-bmp")))
          ((member "NanumGothicCoding" (font-family-list))
           (set-fontset-font fontset 'hangul
                             '("NanumGothicCoding" . "unicode-bmp")))
          ((member "나눔고딕코딩" (font-family-list))
           (set-fontset-font fontset 'hangul
                             '("나눔고딕코딩" . "unicode-bmp")))
          ((member "나눔고딕코딩" (font-family-list))
           (set-fontset-font fontset 'hangul
                             '("나눔고딕코딩" . "unicode-bmp")))
          (t
           (message "'D2Coding' or 'NanumGothicCoding' are not installed")))))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-zenburn)
(if (display-graphic-p)
    (setq doom-theme 'doom-solarized-light)
  (setq doom-theme nil))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; ;; exec-path, PATH
;; (defun add-to-path (path)
;;   "Add the path directory to the `exec-path' and `PATH' variables."
;;   (when (file-directory-p path)
;;     (let ((path-env (getenv "PATH")))
;;         (when (not (cl-search path path-env))
;;        (setenv "PATH" (concat path ":" path-env))))
;;     (add-to-list 'exec-path path)))

;; (defconst home-bin-path (expand-file-name "bin" "~"))
;; (defconst home-local-bin-path (expand-file-name ".local/bin" "~"))

;; (add-to-path home-bin-path)
;; (add-to-path home-local-bin-path)

;; Fix issues after installing Emacs 27.1 on macOS
(setq default-directory "~/")
(setq command-line-default-directory "~/")

;; Enable korean input
(setq default-input-method "korean-hangul")
(global-set-key (kbd "S-SPC") 'toggle-input-method)

;; neotree
;; (use-package! neotree
;;   :bind
;;   (:map global-map
;;    ("M-0" . neotree-show))
;;   :init
;;   (setq projectile-switch-project-action 'neotree-projectile-action))

;; treemacs
(use-package! treemacs
  :bind
  (:map global-map
   ("M-0" . treemacs-select-window))
  :config
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))

;; js2-mode
(use-package! js2-mode
  :init
  (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 4))))

;; realgud-lldb
(use-package! realgud-lldb)

;; paredit
(use-package! paredit
  :init
  (use-package! paredit-everywhere))

(add-hook! lisp-mode (paredit-mode t))
(add-hook! lisp-interaction-mode (paredit-mode t))

(add-hook! emacs-lisp-mode (paredit-mode t))
(add-hook! ielm-mode (paredit-mode t))

;; unset the backends for a sh mode
(after! sh-script
  (set-company-backend! 'sh-mode nil))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

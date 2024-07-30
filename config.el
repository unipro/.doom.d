;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Byungwan Jun"
      user-mail-address "unipro.kr@gmail.com")

;; basic settings
(setq-default window-combination-resize t ; Take new window space from all other windows
              x-stretch-cursor t)         ; Stretch cursor to the glyph width
(setq undo-limit 80000000                 ; Raise undo-limit to 80Mb
      evil-want-fine-undo t               ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                 ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"        ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil           ; I can trust my computers ... can't I?
      ;; scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2                     ; It's nice to maintain a little
      display-time-default-load-average nil)
(display-time-mode 1)                     ; Enable time in the mode-line
(global-subword-mode 1)                   ; Iterate through CamelCase words

;; Default frame size
(add-to-list 'default-frame-alist '(height . 80))
(add-to-list 'default-frame-alist '(width . 120))

;; Default buffer mode
(setq-default major-mode 'org-mode)

(defvar +default-want-RET-continue-comments t
  "If non-nil, RET will continue commented lines.")

(defvar +default-minibuffer-maps
  (append '(minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map
            read-expression-map)
          (cond ((modulep! :completion ivy)
                 '(ivy-minibuffer-map
                   ivy-switch-buffer-map))
                ((modulep! :completion helm)
                 '(helm-map
                   helm-rg-map
                   helm-read-file-map))))
  "A list of all the keymaps used for the minibuffer.")

;; OS specific fixes
(when (featurep :system 'macos)
  ;; Fix MacOS shift+tab
  (define-key key-translation-map [S-iso-lefttab] [backtab])
  ;; Keybinding settings for macOS
  (setq mac-right-option-modifier 'meta
        ns-right-option-modifier  'meta)
  ;; Fix conventional OS keys in Emacs
  (map! "s-`" #'other-frame  ; fix frame-switching
        ;; fix OS window/frame navigation/manipulation keys
        "s-w" #'delete-window
        "s-W" #'delete-frame
        "s-n" #'+default/new-buffer
        "s-N" #'make-frame
        "s-q" (if (daemonp) #'delete-frame #'save-buffers-kill-terminal)
        "C-s-f" #'toggle-frame-fullscreen
        ;; Restore somewhat common navigation
        "s-l" #'goto-line
        ;; Restore OS undo, save, copy, & paste keys (without cua-mode, because
        ;; it imposes some other functionality and overhead we don't need)
        "s-f" (if (modulep! :completion vertico) #'consult-line #'swiper)
        "s-z" #'undo
        "s-Z" #'redo
        "s-c" (if (featurep 'evil) #'evil-yank #'copy-region-as-kill)
        "s-v" #'yank
        "s-s" #'save-buffer
        "s-x" #'execute-extended-command
        :v "s-x" #'kill-region
        ;; Buffer-local font scaling
        "s-+" #'doom/reset-font-size
        "s-=" #'doom/increase-font-size
        "s--" #'doom/decrease-font-size
        ;; Conventional text-editing keys & motions
        "s-a" #'mark-whole-buffer
        "s-/" (cmd! (save-excursion (comment-line 1)))
        :n "s-/" #'evilnc-comment-or-uncomment-lines
        :v "s-/" #'evilnc-comment-operator
        :gi  [s-backspace] #'doom/backward-kill-to-bol-and-indent
        :gi  [s-left]      #'doom/backward-to-bol-or-indent
        :gi  [s-right]     #'doom/forward-to-last-non-comment-or-eol
        :gi  [M-backspace] #'backward-kill-word
        :gi  [M-left]      #'backward-word
        :gi  [M-right]     #'forward-word))

;; Changing the leader prefixes
;; (setq doom-leader-alt-key "M-m"
;;       doom-localleader-alt-key "M-m m")

;; disable C-z
(global-unset-key (kbd "C-z"))

;; Default fonts
(defun my-init-fonts ()
  (when (display-graphic-p)
    ;; Default Latin font
    (cond ((member "Droid Sans Mono" (font-family-list))
           (set-face-attribute 'default nil :family "Droid Sans Mono"))
          ((member "DejaVu Sans Mono" (font-family-list))
           (set-face-attribute 'default nil :family "DejaVu Sans Mono"))
          (t
           (message "'Droid Sans Mono' or 'DejaVu Sans Mono' are not installed")))
    ;; Font size
    (cond ((featurep :system 'macos)
           (set-face-attribute 'default nil :height 150))
          ((featurep :system 'windows)
           (set-face-attribute 'default nil :height 110))
          (t
           (set-face-attribute 'default nil :height 135)))
    ;; Default Korean font
    (cond ((member "D2Coding" (font-family-list))
           (set-fontset-font t 'hangul (font-spec :name "D2Coding")))
          ((member "NanumGothicCoding" (font-family-list))
           (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding")))
          (t
           (message "'D2Coding' or 'NanumGothicCoding' are not installed")))))
(add-hook 'after-setting-font-hook #'my-init-fonts)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-zenburn)
(if (display-graphic-p)
    (setq doom-theme 'doom-solarized-light)
  (setq doom-theme nil))

;; In Emacs 28 you can tell Emacs to jump to the first or last match in the buffer,
;; with M-< and M->; or the next or previous match not currently visible
;; with C-v and M-v.
(when (>= emacs-major-version 28)
  (setq isearch-allow-motion t))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; coding-system
(require 'ucs-normalize)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq-default coding-system-for-write 'utf-8)
(cond ((featurep :system 'windows)
       (setq-default coding-system-for-read 'utf-8) ; XXX 'utf-16-le
       (set-clipboard-coding-system 'utf-16-le)
       (set-selection-coding-system 'utf-16-le))
      ((featurep :system 'macos)
       (setq-default coding-system-for-read 'utf-8-hfs)
       (set-clipboard-coding-system 'utf-8-hfs)
       (set-selection-coding-system 'utf-8-hfs)
       (set-file-name-coding-system 'utf-8-hfs)
       (setq default-process-coding-system '(utf-8-hfs . utf-8-hfs)))
      (t  ; linux
       (setq-default coding-system-for-read 'utf-8)
       (setq x-select-request-type
             '(UTF8_STRING COMPOUND_TEXT TEXT STRING))))

;; Enable korean input
(setq default-input-method "korean-hangul")
(global-set-key (kbd "S-SPC") 'toggle-input-method)

;; Hippie expand
(global-set-key [remap dabbrev-expand] #'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-list
        try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-all-abbrevs
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev-from-kill
        try-expand-whole-kill
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; avy
(setq avy-all-windows nil
      avy-all-windows-alt t
      avy-background t
      ;; the unpredictability of this (when enabled) makes it a poor default
      avy-single-candidate-jump nil)
(global-set-key (kbd "C-'") 'avy-goto-char)
(global-set-key (kbd "C-\"") 'avy-goto-char-2)
(global-set-key (kbd "M-g c") 'avy-goto-char)
(global-set-key (kbd "M-g C") 'avy-goto-char-2)
(global-set-key (kbd "M-g l") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)

;; smartparens
(use-package! smartparens
  :config
  ;; like paredit keybinding
  (map! :map lisp-mode-map
        "M-(" #'sp-wrap-round
        "M-s" #'sp-splice-sexp
        "C-)" #'sp-forward-slurp-sexp
        "C-}" #'sp-forward-barf-sexp
        "C-(" #'sp-backward-slurp-sexp
        "C-{" #'sp-backward-barf-sexp))

;; ;; paredit
;; (use-package! paredit
;;   :init
;;   (use-package! paredit-everywhere))

;; (add-hook! lisp-mode (paredit-mode t))
;; (add-hook! lisp-interaction-mode (paredit-mode t))

;; (add-hook! emacs-lisp-mode (paredit-mode t))
;; (add-hook! ielm-mode (paredit-mode t))

;; man
(after! woman
  ;; The woman-manpath default value does not necessarily match man. If we have
  ;; man available but aren't using it for performance reasons, we can extract
  ;; it's manpath.
  (when (executable-find "man")
    (setq woman-manpath
          (split-string (cdr (doom-call-process "man" "--path"))
                        path-separator t))))

;; treemacs
(use-package! treemacs
  :bind
  (:map global-map
   ("M-o" . treemacs-select-window))
  :config
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))

(after! (treemacs projectile)
  (treemacs-project-follow-mode 1))

;; js2-mode
(use-package! js2-mode
  :mode "\\.js\\'"
  :config
  (setq js-chain-indent t
        js2-basic-offset 4
        ;; Don't mishighlight shebang lines
        js2-skip-preprocessor-directives t
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-missing-semi-warning nil
        ;; maximum fontification
        js2-highlight-level 3
        js2-idle-timer-delay 0.15))

(use-package! xref-js2
  :when (modulep! :tools lookup)
  :init
  (setq xref-js2-search-program 'rg)
  (set-lookup-handlers! 'rjsx-mode
    :xref-backend #'xref-js2-xref-backend))

;; python
(after! python
  (setq python-shell-interpreter "python3"
        flycheck-python-pycompile-executable "python3"
        doom-modeline-env-python-executable "python3"))

(after! projectile
  (projectile-register-project-type 'python-poetry '("poetry.lock")
                                    :project-file "poetry.lock"
                                    :compile "poetry build"
                                    :test "poetry run pytest"
                                    :test-prefix "test_"
                                    :test-suffix "_test"))

;; realgud-lldb
(use-package! realgud-lldb)

;; unset the backends for a sh mode
(after! sh-script
  (set-company-backend! 'sh-mode nil))

(use-package! focus)

;; global beacon minor-mode
(use-package! beacon)
(after! beacon (beacon-mode 1))

;; dotenv-mode
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]shaka-packager/packager/third_part\\'"))

;; Auto-customisations
(setq-default custom-file (expand-file-name ".custom.el" doom-user-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

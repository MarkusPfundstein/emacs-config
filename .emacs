;;; EMACS INIT

(global-auto-revert-mode t)

(defvar my-packages)
(setq my-packages '(exec-path-from-shell
                    flycheck
                    lsp-ui
                    lsp-mode
                    slime
                    alect-themes
                    exotica-theme
                    pyenv-mode
                    dap-mode
                    doom-themes
                    rainbow-delimiters
                    eglot
                    terraform-mode
                    terraform-doc
                    projectile
                    plz))


;; PACKAGING
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/dev/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))


;; ORG MODE
(org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-startup-with-inline-images t)
(eval-after-load "org"
  '(require 'ox-md nil t))

;; DIRED
(defun my-dired-init ()
  (dired-hide-details-mode 1))

(setq-default doc-view-continuous t)

(add-hook 'dired-mode-hook 'my-dired-init)

;; GENERAL SETTINGS
(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq colon-double-space t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-mode-hook 'turn-off-auto-fill)

(auto-image-file-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
;(tooltip-mode nil)

(set-frame-font "Monospace 18" t t)



;; BACKUP DIR
(defvar --user-backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --user-backup-directory))
    (make-directory --user-backup-directory t))
(setq backup-directory-alist `(("." . ,--user-backup-directory)))
(setq make-backup-files t)

(windmove-default-keybindings 'meta)

;; THEMES
(defvar my-theme)
;; (setf my-theme 'exotica)
(setf my-theme 'doom-challenger-deep)
(load-theme my-theme t)

;; COMMON LISP

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/sbcl")

(setq show-paren-delay 0)
(show-paren-mode)

;; (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook 'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook 'enable-paredit-mode)
;; (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
;; (require 'paredit)
;; (defun override-slime-del-key ()
;;   (define-key slime-repl-mode-map
;;     (read-kbd-macro paredit-backward-delete-key) nil))
;; (add-hook 'slime-repl-mode-hook 'override-slime-del-key)

;; Enable Rainbow Delimiters.
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)

;; Customize Rainbow Delimiters.
(require 'rainbow-delimiters)
(set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
(set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
(set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
(set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
(set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
(set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
(set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
(set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
(set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray

;; CUSTOM LOAD PATHS

(add-to-list 'load-path "~/.emacs.d/emacs-async")
(add-to-list 'load-path "~/.emacs.d/sr-speedbar")

;; SPEEDBAR
(require 'sr-speedbar)
(setq speedbar-show-unknown-files t)
(setq speedbar-use-images nil)
(setq sr-speedbar-right-side nil)
(setq speedbar-directory-unshown-regexp
      "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'")
(sr-speedbar-refresh-turn-off)

;; PROJECTILE

(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '("~/clients/fermioniq"))

;; TREEMACS

(require 'treemacs)
(use-package treemacs
  :defer t
  :hook ((treemacs-mode . aorst/treemacs-setup-title))
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  (setq treemacs-collapse-dirs                   0
        treemacs-deferred-git-apply-delay        0.5
        treemacs-directory-name-transformer      #'identity
        treemacs-display-in-side-window          t
        treemacs-eldoc-display                   'simple
        treemacs-file-event-delay                1000
        treemacs-file-follow-delay               0.2
        treemacs-file-name-transformer           #'identity
        treemacs-follow-after-init               t
        treemacs-expand-after-init               t
        treemacs-find-workspace-method           'find-for-file-or-pick-first
        treemacs-git-command-pipe                ""
        treemacs-goto-tag-strategy               'refetch-index
        treemacs-header-scroll-indicators        '(nil . "^^^^^^")
        treemacs-hide-dot-git-directory          t
        treemacs-indentation                     2
        treemacs-indentation-string              " "
        treemacs-is-never-other-window           nil
        treemacs-max-git-entries                 5000
        treemacs-missing-project-action          'ask
        treemacs-move-forward-on-expand          nil
        treemacs-no-png-images                   nil
        treemacs-no-delete-other-windows         t
        treemacs-project-follow-cleanup          nil
        treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-position                        'left
        treemacs-read-string-input               'from-child-frame
        treemacs-recenter-distance               0.1
        treemacs-recenter-after-file-follow      nil
        treemacs-recenter-after-tag-follow       nil
        treemacs-recenter-after-project-jump     'always
        treemacs-recenter-after-project-expand   'on-distance
        treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
        treemacs-project-follow-into-home        nil
        treemacs-show-cursor                     nil
        treemacs-show-hidden-files               t
        treemacs-silent-filewatch                nil
        treemacs-silent-refresh                  nil
        treemacs-sorting                         'alphabetic-asc
        treemacs-select-when-already-in-treemacs 'move-back
        treemacs-space-between-root-nodes        t
        treemacs-tag-follow-cleanup              t
        treemacs-tag-follow-delay                1.5
        treemacs-text-scale                      nil
        treemacs-user-mode-line-format           nil
        treemacs-user-header-line-format         nil
        treemacs-wide-toggle-width               70
        treemacs-width                           35
        treemacs-width-increment                 1
        treemacs-width-is-initially-locked       t
        treemacs-workspace-switch-cleanup        nil)

    (treemacs-follow-mode t)
;    (treemacs-project-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-hide-gitignored-files-mode nil)
    (defun aorst/treemacs-setup-title ()
      (let ((bg (face-attribute 'default :background))
            (fg (face-attribute 'default :foreground)))
        (face-remap-add-relative 'header-line
                                 :background bg :foreground fg
                                 :box `(:line-width ,(/ (line-pixel-height) 2) :color ,bg)))
      (setq header-line-format
            '((:eval
               (let* ((text (treemacs-workspace->name (treemacs-current-workspace)))
                      (extra-align (+ (/ (length text) 2) 1))
                      (width (- (/ (window-width) 2) extra-align)))
                 (concat (make-string width ?\s) text))))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; PYTHON VENV
;(venv-initialize-interactive-shells)
;(setq venv-location "~/.virtualenvs")
(require 'pyenv-mode)
;(use-package pyvenv
;  :diminish
;  :config (progn
;            (setq pyvenv-mode-line-indicator
;                  '(pyvenv-virtual-env-name ("[pyenv:" pyvenv-virtual-env-name "] ")))
;            (pyvenv-mode t)))

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (progn
          (message "switch pyenv"))
      (message "unswitch pyenv"))))
;          (pyenv-mode-set project))
   ;       (lsp-workspace-restart (lsp--read-workspace)))
;;      (pyenv-mode-unset))))

(add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)

(defun python-custom-settings ()
  "Set custom python settings."
  (setq python-indent-offset 4)
  (setq tab-width 4)
  (setq python-indent-guess-indent-offset nil))

(add-hook 'python-mode-hook 'python-custom-settings)

;; FLYCHECK
(use-package flycheck
 :ensure t
 :init (progn
         (global-flycheck-mode t)))

(setq-default flycheck-emacs-lisp-load-path 'inherit)

;; LSP SETUP
(setq-default lsp-pylsp-server-command "~/.pyenv/shims/pylsp")
(use-package lsp-mode
  :config (progn
            (setq lsp-idle-delay 0.25
                  lsp-enable-symbol-highlighting t
                  lsp-enable-snippet nil
                  lsp-semantic-tokens-enable t
                  lsp-pylsp-plugins-flake8-enabled t
                  lsp-pylsp-plugins-flake8-ignore '(D100
                                                    D101
                                                    D102
                                                    D103
                                                    D104
                                                    D105
                                                    D106))
            (lsp-register-client
             (make-lsp-client :new-connection
                              (lsp-stdio-connection '("terraform-ls" "serve"))
                              :major-modes '(terraform-mode)
                              :server-id 'terraform-ls))
                                           
            ;; (lsp-register-client
            ;;  (make-lsp-client :new-connection
            ;;                   (lsp-tramp-connection "/var/lib/cloud-user/.local/bin/pylsp")
            ;;                   :major-modes '(python-mode)
            ;;                   :remote? t
            ;;                   :server-id 'pylsp-remote))
            (lsp-register-custom-settings
             '(("pylsp.plugins.pylsp_mypy.enabled" t t)
               ("pylsp.plugins.pyls_mypy.live_mode" t t)
               ("pylsp.plugins.pyls_black.enabled" t t)
               ("pylsp.plugins.pyls_isort.enabled" t t)
     
               ;; Disable these as they're duplicated by flake8
               ("pylsp.plugins.pycodestyle.enabled" t t)
               ("pylsp.plugins.mccabe.enabled" nil t)
               ("pylsp.plugins.pyflakes.enabled" nil t))))
  :hook ((python-mode . lsp-deferred)
         (terraform-mode .lsp-deferred)))

;;(add-to-list 'lsp-language-id-configuration '(terraform-mode . "terraform"))

(use-package lsp-ui
  :commands lsp-ui-mode)

;; tramp

(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
;;(setq tramp-shell-prompt-pattern "[\W] $")
;(setenv "ESHELL" "sh")


;; DAP

(use-package dap-mode)

;; Exec path

(exec-path-from-shell-initialize)

;; CUSTOM FUNCTIONS

(defvar my--sh-eval-region-beg)
(defvar my--sh-eval-region-end)

(defun my--sh-eval-region-reset ()
  "Reset internal sh-eval region variables."
  (interactive)
  (setq my--sh-eval-region-beg nil)
  (setq my--sh-eval-region-end nil))

(defun my-sh-eval-region (beg end)
  "Eval sh command on region from BEG END.
Saves the region to global var"
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list my--sh-eval-region-beg my--sh-eval-region-end)))
  (if (and (not (null beg)) (not (null end)))
      (progn
        (setq my--sh-eval-region-beg beg)
        (setq my--sh-eval-region-end end)
        (let ((text (buffer-substring-no-properties beg end)))
          (message "Run shell command sh with: '%s'" text)
          (shell-command-on-region beg end "sh")))))

  
(defun my-clear-elisp-from-functions-with-prefix (prefix)
  "Clear elisp of all functions with PREFIX.
E.g. when PREFIX equals ferm, all functions starting with ferm-
will be deleted."
  (interactive "sprefix:")
  (if (yes-or-no-p (format "delete all symbols starting with %s-?" prefix))
      (mapatoms (lambda (symbol)
                  (if (string-prefix-p (format "%s-" prefix) (symbol-name symbol))
                      (unintern symbol nil))))))

(require 'lsp)
(defun my-pylsp-init (pyenv-name)
  "Init pylsp and activates pyenv with PYENV-NAME."
  (interactive (list (pyenv-mode-read-version)))
  (pyenv-mode-unset)
  (pyenv-mode-set pyenv-name)
  (lsp-workspace-restart (lsp--read-workspace)))

(load "~/.emacs.d/sysmon/systemctl.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(terraform-doc terraform-mode dap-mode pyenv-mode pyenv plz doom-themes exotica-theme alect-themes exec-path-from-shell flycheck lsp-ui lsp-mode dired-sidebar elpy slime)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; .emacs ends here


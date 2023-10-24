;;; EMACS INIT

(global-auto-revert-mode t)

(defvar my-packages '(exec-path-from-shell
                      flycheck
                      lsp-ui
                      lsp-mode
                      slime
                      alect-themes
                      exotica-theme
                      pyenv-mode
                      doom-themes
                      rainbow-delimiters
                      plz))

;; ORG MODE
(org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-startup-with-inline-images t)
(eval-after-load "org"
  '(require 'ox-md nil t))

;; DIRED
(defun my-dired-init ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))

(add-hook 'dired-mode-hook 'my-dired-init)

;; GENERAL SETTINGS
(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq colon-double-space t)
(setq-default indent-tabs-mode nil)

(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-mode-hook 'turn-off-auto-fill)

(auto-image-file-mode t)
(tool-bar-mode -1)
;(tooltip-mode nil)

(set-frame-font "Monospace 18" t t)

;; BACKUP DIR
(defvar --user-backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --user-backup-directory))
    (make-directory --user-backup-directory t))
(setq backup-directory-alist `(("." . ,--user-backup-directory)))
(setq make-backup-files t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doc-view-continuous t)
 '(package-selected-packages
   '(pyenv-mode pyenv plz doom-themes exotica-theme alect-themes exec-path-from-shell flycheck lsp-ui lsp-mode evil treemacs-tab-bar treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs dired-sidebar elpy slime)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; PACKAGING
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

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


;; PYTHON VENV
;(venv-initialize-interactive-shells)
;(setq venv-location "~/.virtualenvs")
(require 'pyenv-mode)
(use-package pyvenv
  :diminish
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[pyenv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1))

;; FLYCHECK
(add-hook 'after-init-hook #'global-flycheck-mode)

(setq-default flycheck-emacs-lisp-load-path 'inherit)

;; LSP
(setq lsp-pylsp-server-command "~/.pyenv/shims/pylsp")
(use-package lsp-mode
  :config
  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil
        lsp-pyls-plugins-flake8-enabled t)
  (lsp-register-custom-settings
   '(("pylsp.plugins.pyls_mypy.enabled" t t)
     ("pylsp.plugins.pyls_mypy.live_mode" nil t)
     ("pylsp.plugins.pyls_black.enabled" t t)
     ("pylsp.plugins.pyls_isort.enabled" t t)

     ;; Disable these as they're duplicated by flake8
     ("pylsp.plugins.pycodestyle.enabled" nil t)
     ("pylsp.plugins.mccabe.enabled" nil t)
     ("pylsp.plugins.pyflakes.enabled" nil t)))
  :hook ((python-mode . lsp)))

(use-package lsp-ui
  :commands lsp-ui-mode)

(setq python-flymake-command '("pylint" "--from-stdin" "stdin"))

(exec-path-from-shell-initialize)

(defun clear-elisp-from-functions-with-prefix (prefix)
  "Clear elisp of all functions with PREFIX.
E.g. when PREFIX equals ferm, all functions starting with ferm-
will be deleted."
  (interactive "sprefix:")
  (if (yes-or-no-p (format "delete all symbols starting with %s-?" prefix))
      (mapatoms (lambda (symbol)
                  (if (string-prefix-p (format "%s-" prefix) (symbol-name symbol))
                      (unintern symbol nil))))))

(load "~/.emacs.d/sysmon/systemctl.el")

;;; .emacs ends here

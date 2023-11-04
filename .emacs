;;; EMACS INIT
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(global-auto-revert-mode t)
(setq x-select-enable-clipboard t)

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
                    treemacs
                    treemacs-projectile
                    treemacs-all-the-icons
                    treemacs-icons-dired
                    treemacs-magit
                    which-key
                    avy
                    helm
                    yasnippet
                    hydra
                    vterm
                    notmuch
                    leuven-theme
                    rust-mode
                    gptel
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

(which-key-mode)

;; GNUS
(setq browse-url-browser-function 'eww)

(require 'notmuch)
(setq-default notmuch-search-oldest-first nil)

;; ORG MODE
(org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-startup-with-inline-images t)
(eval-after-load "org"
  '(require 'ox-md nil t))


(setq mail-user-agent 'message-user-agent)
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'ssl
      smtpmail-smtp-user "markus@life-electronic.nl"
      smtpmail-smtp-server "smtp.transip.email"
      smtpmail-smtp-service 465)

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


51

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq tab-width 2)
            (setq js-indent-level 2)))


;; BACKUP DIR
(defvar --user-backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --user-backup-directory))
    (make-directory --user-backup-directory t))
(setq backup-directory-alist `(("." . ,--user-backup-directory)))
(setq make-backup-files t)

(windmove-default-keybindings 'meta)

;; THEMES
(defvar my-theme)
(progn 
  ;;  (setf my-theme 'exotica)
  (setf my-theme 'leuven-dark)
  (load-theme my-theme t)
  (setf my-theme 'doom-challenger-deep)
  (load-theme my-theme t))


(when (display-graphic-p)
       (require 'treemacs-all-the-icons)
       (require 'all-the-icons))


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
(add-to-list 'load-path "~/.emacs.d/org-ref")

(use-package org-ref)

;; CHATGPT
(require 'gptel)
(setq-default gptel-default-mode 'org-mode)
(setq-default gptel-model 'gpt-4)

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
        treemacs-show-hidden-files               nil
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
        ;; only for dap debug
        ("C-x t o"   . treemacs-expand-extension-node)
        ;; only for dap debug
        ("C-x t c"   . treemacs-collapse-extension-node)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t ."   . treemacs-toggle-show-dotfiles)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t w"   . treemacs-switch-workspace)
        ("C-x t M-t" . treemacs-find-tag)))

(treemacs-load-theme 'all-the-icons)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

(treemacs-icons-dired-mode t)

;; PYTHON VENV
(require 'pyenv-mode)

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (progn
          (message "switch pyenv"))
      (message "unswitch pyenv"))))

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
  :config 
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
     ("pylsp.plugins.pylsp_mypy.live_mode" t t)
     ("pylsp.plugins.pylsp_mypy.report_progress" t t)
     ("pylsp.plugins.pylsp_black.enabled" t t)
               ("pylsp.plugins.pylsp_isort.enabled" t t)
               
               ;; Disable these as they're duplicated by flake8
               ("pylsp.plugins.pycodestyle.enabled" t t)
               ("pylsp.plugins.mccabe.enabled" nil t)
               ("pylsp.plugins.pyflakes.enabled" nil t)))
  :hook ((python-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (rust-mode . lsp)
         (terraform-mode . lsp)))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 10 1024 1024)) ;; 10mb

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

;; DAP

(use-package dap-mode)
(require 'dap-gdb-lldb)

;(dap-cpptools-setup t)

(with-eval-after-load 'dap-cpptools
  ;; Add a template specific for debugging Rust programs.
  ;; It is used for new projects, where I can M-x dap-edit-debug-template
  (dap-register-debug-template "Rust::CppTools Run Configuration"
                               (list :type "cppdbg"
                                     :request "launch"
                                     :name "Rust::Run"
                                     :MIMode "gdb"
                                     :miDebuggerPath "rust-gdb"
                                     :environment []
                                     :program "${workspaceFolder}/target/debug/hello / replace with binary"
                                     :cwd "${workspaceFolder}"
                                     :console "external"
                                     :dap-compilation "cargo build"
                                     :dap-compilation-dir "${workspaceFolder}")))

;;(add-to-list 'lsp-language-id-configuration '(terraform-mode . "terraform"))

(with-eval-after-load 'dap-mode
  (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
  (dap-auto-configure-mode +1))

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-ignore-duplicate t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))
;  :commands lsp-ui-mode)

;; tramp

(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
;;(setq tramp-shell-prompt-pattern "[\W] $")
;(setenv "ESHELL" "sh")



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

(defun my-insert-current-date () (interactive)
    (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(require 'lsp)
(defun my-pylsp-init (pyenv-name)
  "Init pylsp and activates pyenv with PYENV-NAME."
  (interactive (list (pyenv-mode-read-version)))
  (pyenv-mode-unset)
  (pyenv-mode-set pyenv-name)
  (lsp-workspace-restart (lsp--read-workspace)))

(global-set-key (kbd "C-c mi") 'my-pylsp-init)
(global-set-key (kbd "C-c me") 'my-sh-eval-region)

(defun my-email-fetch ()
  "Fetch new email with offlineimap."
  (interactive)
  (let* ((output-buf (get-buffer-create "*update email*"))
         (offlineimap-p (start-process-shell-command
                         "my-email-fetch-offlineimap"
                         output-buf
                         "offlineimap")))
    (set-process-sentinel offlineimap-p 'my--email-fetch-offlineimap-sentinel)))

(defun my--email-fetch-offlineimap-sentinel (process event-name)
  "Callback method from my-email-fetch.
PROCESS, EVENT-NAME."
  (let* ((event (string-trim-right event-name))
         (success (string-equal "finished" event)))
    (if success
        (progn
          (message "New email fetched.  Start notmuch new for indexing!")
          (let* ((output-buf (get-buffer-create "*update email*"))
                 (notmuch-p (start-process-shell-command
                             "my-email-fetch-notmuch"
                             output-buf
                             "notmuch new")))
            (set-process-sentinel notmuch-p 'my--email-fetch-notmuch-sentinel)))
      (error "ERROR UPDATING EMAIL DURING offlineimap"))))

(defun my--email-fetch-notmuch-sentinel (process event-name)
  "Callback method from my-email-fetch.
PROCESS, EVENT-NAME."
  (let* ((event (string-trim-right event-name))
         (success (string-equal "finished" event)))
    (if success
        (progn
          (message "EMAIL UPDATED AND INDEXED")
          (notmuch))
      (error "ERROR UPDATING EMAIL DURING notmuch new"))))

(defun my-notmuch-show-jump-to-latest ()
  "Jump to the message in the current thread with the latest timestamp."
  (interactive)
  (let ((timestamp 0)
	    latest)
    (notmuch-show-mapc
     (lambda () (let ((ts (notmuch-show-get-prop :timestamp)))
		      (when (> ts timestamp)
			(setq timestamp ts
			      latest (point))))))
    (if latest
	    (goto-char latest)
      (error "Cannot find latest message"))))

;(add-hook 'c++mode-hook
          
;;(require 'dap-cpptools)
(require 'dap-gdb-lldb)

;; requre helm
(require 'helm)
(require 'helm-autoloads)

(helm-mode 1)

(defvar spacemacs-helm-display-help-buffer-regexp '("\\*.*Helm.*Help.*\\*"))
(defvar spacemacs-helm-display-buffer-regexp `("\\*.*helm.*\\*"
                                               (display-buffer-in-side-window)
                                               (inhibit-same-window . nil)
                                               (side . bottom)
                                               (window-width . 0.6)
                                               (window-height . 0.4)))

(defun display-helm-at-bottom (buffer &optional _resume)
  (let ((display-buffer-alist (list spacemacs-helm-display-help-buffer-regexp
                                    spacemacs-helm-display-buffer-regexp)))
    (display-buffer buffer)))
(setq helm-display-function 'display-helm-at-bottom)

;(setq helm-display-function 'helm-default-display-buffer)
;(setq helm-display-function 'helm-display-buffer-in-own-frame)
(setq completion-styles '(flex))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(load "~/clients/fermioniq/emacs-scripts/ferm-api.el")

;;(load "~/.emacs.d/sysmon/systemctl.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1" "34af44a659b79c9f92db13ac7776b875a8d7e1773448a8301f97c18437a822b6" "330d5278ead8dd474f8e79d0cadae973aae3e56f86e6e6d1667d723992b34a59" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0" "c517e98fa036a0c21af481aadd2bdd6f44495be3d4ac2ce9d69201fcb2578533" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd" "badd1a5e20bd0c29f4fe863f3b480992c65ef1fa63951f59aa5d6b129a3f9c4c" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "e87fd8e24e82eb94d63b1a9c79abc8161d25de9f2f13b64014d3bf4b8db05e9a" default))
 '(package-selected-packages
   '(gptel chatgpt rust-mode flycheck-mypy company-lsp leuven-theme notmuch vterm helm org-ref helm-lsp which-key treemacs-all-the-icons terraform-doc terraform-mode dap-mode pyenv-mode pyenv plz doom-themes exotica-theme alect-themes exec-path-from-shell flycheck lsp-ui lsp-mode dired-sidebar elpy slime)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; .emacs ends here


;; Package management - ORDER IS IMPORTANT
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;;----------------------------------------------------------
;;----------------------------------------------------------

;; Global editor options

;; Enable syntax highlighting
(global-font-lock-mode 1)

;; System clipboard integration (macOS terminal)
(setq select-enable-clipboard t)
(unless (display-graphic-p)
  (defun copy-to-osc52 (text)
    (send-string-to-terminal
     (concat "\e]52;c;" (base64-encode-string text t) "\a")))
  (setq interprogram-cut-function #'copy-to-osc52)
  ;; Paste from system clipboard
  (setq interprogram-paste-function
        (lambda () (shell-command-to-string "pbpaste"))))

;; Enable line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Set up mouse integration
(xterm-mouse-mode 1)
;; Smooth scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
;; NO mouse accel
(setq mouse-wheel-progressive-scroll nil) 

(add-hook 'after-init-hook
	  (lambda ()
	    (xterm-mouse-mode 1)
	    (global-set-key (kbd "<mouse-4>")
			    'scroll-down-line)
	    (global-set-key [wheel-up]
			    'scroll-down-line)
	    (global-set-key (kbd "<mouse-5>")
			    'scroll-up-line)
	    (global-set-key [wheel-down]
			    'scroll-up-line)))

	  

;;----------------------------------------------------------
;;----------------------------------------------------------

;; OCaml / Tuareg
(use-package tuareg
  :mode (("\\.ml\\'" . tuareg-mode)
         ("\\.mli\\'" . tuareg-mode))
  :hook ((tuareg-mode . font-lock-ensure)
         (tuareg-mode . (lambda ()
                          (setq-local compile-command "dune build")))))

;; Language Configuration
;; OCAML
(use-package dune
  :mode ("dune\\'" "dune-project\\'"))

(use-package merlin
  :hook ((tuareg-mode . merlin-mode)
         (caml-mode . merlin-mode))
  :config
  (setq merlin-command 'opam))

(use-package merlin-eldoc
  :hook ((tuareg-mode . merlin-eldoc-setup)))

(use-package utop
  :hook (tuareg-mode . utop-minor-mode)
  :config
  (setq utop-command "opam exec -- utop -emacs"))

;; ocamlformat - load from opam instead of broken MELPA package
(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (when (require 'ocamlformat nil t)
      (add-hook 'tuareg-mode-hook
                (lambda ()
                  (define-key tuareg-mode-map (kbd "C-c C-f") #'ocamlformat)
                  (add-hook 'before-save-hook #'ocamlformat-before-save nil t))))))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))

;;----------------------------------------------------------
;;----------------------------------------------------------

;; File browsing
(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

;;----------------------------------------------------------
;;----------------------------------------------------------

;; LSP Mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((rust-mode . lsp-deferred))
  :config
  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable t))

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . (lambda ()
                       (setq-local compile-command "cargo build")))
  :config
  (setq rust-format-on-save t))

;;----------------------------------------------------------
;;----------------------------------------------------------

;; MAGIT

(use-package magit :bind ("C-x g" . magit-status))

;;----------------------------------------------------------
;;----------------------------------------------------------

;; ORG Mode

(setq org-directory "~/org")
(setq org-agenda-files
      (directory-files-recursively
       "~/org" "\\.org$"))
       

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      '(("t" "Task" entry (file "~/org/inbox.org")
	 "* TODO %?\n  %U\n")
	("n" "Note" entry (file "~/org/notes.org")
	 "* %?\n %U\n")))

(setq org-startup-indented t)
(global-set-key (kbd "C-c a") 'org-agenda)

(defun insert-today ()
  (interactive)
  (insert (format-time-string "%y-%m-%d")))


;;


;; Set by emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

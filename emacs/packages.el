;;; packages.el --- Package configuration -*- lexical-binding: t -*-
(setq use-package-always-ensure t)
;;; evil mode (vim bindings)
(setq evil-want-keybinding nil)  ; required before loading evil-collection
(setq evil-search-module 'evil-search)  ; required for cgn
(setq evil-undo-system 'undo-redo)  ; use emacs 28+ native undo-redo
(use-package evil :config (evil-mode 1))
(use-package evil-leader :config
             (global-evil-leader-mode)
             (evil-leader/set-leader "<SPC>"))
(use-package evil-collection :config (evil-collection-init))
(use-package evil-commentary :config (evil-commentary-mode 1))

;; Improve garbage collector
(use-package gcmh :demand t :config (gcmh-mode 1))

;;; Vertico + Consult + Orderless (telescope-like fuzzy finding)
(use-package vertico :config (vertico-mode 1))
(use-package consult)
(use-package orderless)
(use-package marginalia :config (marginalia-mode 1))
(use-package posframe :demand t)
(use-package vertico-posframe :demand t :config (vertico-posframe-mode 1))
(use-package fzf)
(use-package affe)
(recentf-mode 1)

(setq vertico-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))
(setq vertico-posframe-poshandler #'posframe-poshandler-frame-center)

(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))

;; Flex matching (fzf-style: characters in sequence)
(setq orderless-matching-styles '(orderless-literal orderless-flex))

;; Affe (async fuzzy finder using orderless)
(setq affe-find-command "fd --color=never -t f")

(setq consult-fd-args '("fd" "--color=never" "--type" "f" "--hidden" "--follow" "--exclude" ".git"))

;; Live preview as you navigate
(setq consult-preview-key 'any)

;;; magit
(use-package magit)
(setq magit-auto-revert-mode nil)

;;; Move Text
;(rc/require 'move-text)

;;; Corfu (CAPF-native autocompletion, works seamlessly with Eglot)
(use-package corfu
  :demand t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 1)   ; trigger after 1 char (so `.f` pops up)
  (corfu-cycle t)
  :init
  (global-corfu-mode))

;;; Language modes
(use-package rust-mode)
(use-package web-mode)
(use-package typescript-mode)
(use-package tuareg)
(use-package ocamlformat)
(use-package dune)
(use-package utop)
(use-package racket-mode
  :ensure t
  :hook
  (racket-mode . racket-xp-mode)
  :bind (:map racket-mode-map
              ("C-c C-d" . racket-xp-describe)
              ("M-."     . racket-xp-visit-declaration)
              ("M-,"     . xref-pop-marker-stack)))

(use-package sly :ensure t
  :config (setq inferior-lisp-program "sbcl"))

(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)))

(use-package metal-mode
  :vc (:url "https://github.com/masfj/metal-mode")
  :mode "\\.metal\\'")


(setq utop-command "opam exec -- utop -emacs")

;;; Tree-sitter text objects (vif, vaf, vic, vac, etc.)
(use-package tree-sitter
             :config
                (global-tree-sitter-mode)
                (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
                (define-key evil-outer-text-objects-map "f"
                            (evil-textobj-tree-sitter-get-textobj "function.outer"))
                (define-key evil-inner-text-objects-map "f"
                            (evil-textobj-tree-sitter-get-textobj "function.inner"))
                (define-key evil-outer-text-objects-map "c"
                            (evil-textobj-tree-sitter-get-textobj "class.outer"))
                (define-key evil-inner-text-objects-map "c"
                            (evil-textobj-tree-sitter-get-textobj "class.inner")))

(use-package tree-sitter-langs)
(use-package evil-textobj-tree-sitter)

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     ; Gotta clone this one manually! https://github.com/tree-sitter/tree-sitter-ocaml
     (ocaml "/tmp/ts-ocaml" nil "grammars/ocaml/src")
     (ocaml-interface "/tmp/ts-ocaml" nil "grammars/interface/src")
     (janet-simple "https://github.com/sogaiu/tree-sitter-janet-simple")))


(let ((failed '()))
  (dolist (pair treesit-language-source-alist)
    (let ((language (car pair)))
      (when (not (treesit-language-available-p language))
        (treesit-install-language-grammar language)
        (when (not (treesit-language-available-p language))
          (push language failed)))))
  (if failed
      (progn
        (message "Failed grammars:")
        (dolist (f failed)
          (message " %s: Did not install!" f)))
    (message "All grammars installed!")))

;;; CSS color preview
  (use-package rainbow-mode
    :config
    (add-hook 'css-mode-hook 'rainbow-mode)
    (add-hook 'html-mode-hook 'rainbow-mode)
    (add-hook 'js-mode-hook 'rainbow-mode)
    (add-hook 'web-mode-hook 'rainbow-mode)
    (add-hook 'scss-mode-hook 'rainbow-mode)
    (add-hook 'conf-mode-hook 'rainbow-mode)
    (add-hook 'toml-mode-hook 'rainbow-mode)
    (add-hook 'yaml-mode-hook 'rainbow-mode)
    (add-hook 'conf-toml-mode-hook 'rainbow-mode))

;;; Treesitter context (sticky function header)
(use-package topsy
             :config (add-hook 'prog-mode-hook 'topsy-mode))

;;; Org mode
(use-package org-superstar)
(use-package org-fancy-priorities)

(setq org-directory "~/org/")
(setq org-agenda-files '("~/repos/agendas/private.org"))

;; Pretty bullets
(add-hook 'org-mode-hook #'org-superstar-mode)
(setq org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆"))

;; Priority icons
(add-hook 'org-mode-hook #'org-fancy-priorities-mode)
(setq org-fancy-priorities-list '("⚑" "▲" "»"))

;; Syntax highlighting in code blocks
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-hide-block-startup nil
      org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)

;; Hide emphasis markers (*bold*, /italic/, etc.)
(setq org-hide-emphasis-markers t)

;; Visual tweaks
(setq org-ellipsis " ▾")           ; nicer fold indicator
(setq org-startup-folded 'content) ; show headings on open
(add-hook 'org-mode-hook #'org-indent-mode) ; clean indentation

;; Make RET follow links and toggle checkboxes
(setq org-return-follows-link t)

;;; Org Present (presentation mode)
(use-package org-present)
(use-package visual-fill-column)

(defun my/org-present-start ()
  ;; Smaller, more readable font scaling
  (setq-local face-remapping-alist
              '((default (:height 1.3) default)
                (header-line (:height 2.0) variable-pitch)
                (org-document-title (:height 1.5) org-document-title)
                (org-level-1 (:height 1.3) org-level-1)
                (org-level-2 (:height 1.2) org-level-2)
                (org-level-3 (:height 1.1) org-level-3)
                (org-code (:height 1.0) org-code)
                (org-block (:height 1.0) org-block)))
  ;; Center content
  (setq visual-fill-column-width 80)
  (setq visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  ;; Word wrap
  (visual-line-mode 1)
  ;; Hide UI
  (setq header-line-format " ")
  (display-line-numbers-mode 0)
  (org-display-inline-images))

(defun my/org-present-end ()
  (setq-local face-remapping-alist nil)
  (setq header-line-format nil)
  (visual-fill-column-mode 0)
  (visual-line-mode 0)
  (display-line-numbers-mode 1)
  (org-remove-inline-images))

(add-hook 'org-present-mode-hook #'my/org-present-start)
(add-hook 'org-present-mode-quit-hook #'my/org-present-end)

;;; LSP (eglot is built-in to Emacs 29+)
(require 'eglot)
(use-package eldoc-box)

;; Auto-start LSP for these modes
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)

(add-hook 'tuareg-mode-hook #'eglot-ensure)
(add-hook 'tuareg-mode 'utop-minor-mode)


;; (when (not (treeset-language-available-p 'janet-simple))
;;   (treeset-install-language-grammar 'janet-simple))

(use-package janet-ts-mode
  :vc (:url "https://github.com/sogaiu/janet-ts-mode"
            :rev :newest))

;; LSP server configurations
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(web-mode . ("intelephense" "--stdio")))
  ;; TypeScript/TSX (typescript-language-server)
  (add-to-list 'eglot-server-programs
               '(typescript-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(tsx-ts-mode . ("typescript-language-server" "--stdio"))))

;; File associations for TypeScript React
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(add-hook 'web-mode-hook 'eglot-ensure)


;;; Direnv integration (loads devshell environment)
;(use-package envrc :config (envrc-global-mode))

;;; vterm (terminal emulator)
(use-package vterm)
(defun rc/find-shell ()
  "Find a suitable shell, checking common locations."
  (or (getenv "SHELL")
      (seq-find #'file-executable-p
                '("/bin/bash"                      ; FHS standard
                  "/usr/bin/bash"                  ; Some distros
                  "/run/current-system/sw/bin/bash" ; NixOS
                  "/bin/sh"))                      ; Ultimate fallback
      "/bin/sh"))
(setq vterm-shell (rc/find-shell))
(setq vterm-kill-buffer-on-exit t)

;;; Theme
(use-package doom-themes
  :config (load-theme 'doom-gruvbox t))

;;; Clean up modeline (hide minor modes)
(setq eldoc-minor-mode-string nil)
(setq-default abbrev-mode nil)
(with-eval-after-load 'flymake (setq flymake-mode-line-format nil))
(with-eval-after-load 'envrc (setq envrc-lighter nil))
(with-eval-after-load 'evil-commentary (setq evil-commentary-mode-lighter nil))

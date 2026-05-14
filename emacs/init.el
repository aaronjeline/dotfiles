;;; init.el --- Main entry point -*- lexical-binding: t -*-
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

; Full path
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))


; Load config files
(load (expand-file-name "packages.el" user-emacs-directory))
(load (expand-file-name "config.el" user-emacs-directory))
(load (expand-file-name "keybinds.el" user-emacs-directory))


(when (file-exists-p custom-file)
  (load-file custom-file))

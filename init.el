;;; init.el --- Loads afer the early-init file -*- lexical-binding: t; -*-

;;; Commentary:
;;  - Adding all essential folders and files to the loading path.
;;  - Loading the core file of Semacs.

;;; Code:

;;
;;; Directory variables

(defun setup-load-path ()
  (defconst semacs-emacs-dir user-emacs-directory
    "The path to the currently loaded .emacs.d directory. Must end with a slash.")

  (defconst semacs-core-dir (concat semacs-emacs-dir "core/")
    "The root directory of Semacs's core files. Must end with a slash.")

  (defconst semacs-modules-dir (concat semacs-emacs-dir "modules/")
    "The root directory for Semacs's modules. Must end with a slash.")

  (defconst semacs-private-dir (concat semacs-emacs-dir "config/")
    "Where your private configuration is placed.")

	(defconst semacs-local-dir (concat semacs-emacs-dir ".local/")
	  "Root directory for local storage.")

  (defconst semacs-cache-dir (concat semacs-emacs-dir ".local/cache/")
    "Directory for volatile local storage.
     Use this for files that change often, like cache files. Must end with a slash.")

  (defconst semacs-etc-dir (concat semacs-emacs-dir ".local/etc/")
	 "Directory for non-volatile local storage.
    Use this for files that don't change much, like server binaries, external
    dependencies or long-term shared data. Must end with a slash.")

  (defconst semacs-docs-dir (concat semacs-emacs-dir "docs/")
    "Where Semacs's documentation files are stored. Must end with a slash.")

  ;; Add folders to load path
  (add-to-list 'load-path semacs-core-dir)
  (add-to-list 'load-path semacs-modules-dir)
  (add-to-list 'load-path semacs-cache-dir)
  (add-to-list 'load-path semacs-local-dir)
  (add-to-list 'load-path semacs-etc-dir)
  (add-to-list 'load-path semacs-private-dir)
  (add-to-list 'load-path semacs-docs-dir)

  ;; add all its subfolders too
  (let ((default-directory semacs-modules-dir))
    (normal-top-level-add-subdirs-to-load-path))
)

;; Setup load path
(setup-load-path)

;; Load the Core-file
(load "core.el")

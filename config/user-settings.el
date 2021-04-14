;;; Package --- user-settings.el
;;; Commentary:
;;; Da kommt der Kommentar

;;; Code:

;;; User
(setq user-full-name "Sean Avery")
(setq user-mail-address "seanavery@gmx.net")

;; Program-Hooks
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;(add-hook 'prog-mode-hook 'smartparens-mode)
(add-hook 'prog-mode-hook 'undo-tree-mode)
(add-hook 'prog-mode-hook 'yas-global-mode)
(add-hook 'prog-mode-hook 'tex-smart-umlauts-mode)

;; text-mode-hook
;(add-hook 'text-mode-hook 'smartparens-mode)
(add-hook 'text-mode-hook 'undo-tree-mode)
(add-hook 'text-mode-hook 'tex-smart-umlauts-mode)

;; Editor
(show-paren-mode t)                                        ;Zusammengeh�rende Klammern hervorhebne
(setq-default smartparense-mode t)

;; Ui
(set-frame-parameter (selected-frame) 'alpha '(99 99))
(add-to-list 'default-frame-alist '(alpha 99 99))
(setq initial-frame-alist '((left . .0)                    ;Transparency [from emacswiki]
                            (width . 105)
                            (fullscreen . fullheight)))
(window-divider-mode)

(setq-default
   display-line-numbers-type 'relative
   window-divider-default-right-width 3
   window-combination-resize t                             ;Take new window space from all other windows (not just current)
   window-divider-default-places 'right-only)

;; Cli
(display-time-mode 1)                                      ;Enable time in the mode-line
(display-battery-mode 1)                                   ;It's nice to know how much power you have
(setq display-time-24hr-format t)                          ;24h format
(setq display-time-day-and-date t)                         ;Display time and date
(setq column-number-mode t)                                ;Toggle automatic display of the current line number or column number
(minibuffer-depth-indicate-mode)

;(setq column-number-mode t)                                ;Show (line #, column #) in mode line
(setq default-fill-column 80)                              ;Toggle wrapping text at the 80th character
(setq line-move-visual t)                                  ;Move around lines based on how they displayed
(setq fill-column 80)                                      ;Maximum line width
(global-hl-line-mode t)                                    ;Highlight current line

;; Dont know
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;; JavaScript
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 4))

(add-hook 'web-mode-hook  'web-mode-init-hook)


;;; Abrevivations
;(setq abbrev-file-name             ;; tell emacs where to read abbrev
;        "~/Demacs/abbrev_defs")
(setq save-abbrevs 'silent)        ;; save abbrevs when files are saved
(setq-default abbrev-mode t)

(load "~/Demacs/my-abbrev.el")

graphical user interface
;; Ace-Jump-Mode
(use-package ace-jump-mode
:config
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
  )
;; Umalute
(use-package tex-smart-umlauts)

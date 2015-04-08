(add-to-list 'load-path "~/.emacs.d/lisp")

;; Use modern keyboard shortcuts.
(require 'modern)
(modern-mode)

;; Match parentheses.
(require 'paren)
(show-paren-mode)
(setq show-paren-delay 0)

;; Automatically pair parentheses, quotation marks, etc.
;; (electric-pair-mode)

;; Quickly switch between open files and navigate through directories.
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)

;;; package --- Summary key-bindings of mpc-mode. -*- lexical-binding: t -*-
;;; Commentary:

;; The prefix key for mpc commands is "m".#<buffer mpc-binds.el>

;;; Code:
(require 'mpc)

(defun mpc-bindings--bind-mpc-commands ()
  "Bind mpc commands to a keymap, returning the keymap."
  (let ((keymap (make-sparse-keymap)))
    (dolist (pair '(("p" . mpc-play)
		    ("u" . mpc-update)))
      (define-key keymap (car pair) (cdr pair)))
    keymap))


(defun mpc-bindings--select-window-by-buffer-name
    (windows name)
  "Select the widows with a NAME in WINDOWS."
  (seq-filter
   (lambda (w) (equal name (-> w window-buffer buffer-name)))
   (window-list)))

(defun mpc-bindings--bind-move-window
    ()
  "Bind moving to a mpc window to a keymap, returning the keymap."
  (let ((keymap (make-sparse-keymap)))
    1))

(define-key mpc-mode-map "m" (mpc-bindings--bind-mpc-commands))

(provide 'mpc-binds)
;;; mpc-binds.el ends here.

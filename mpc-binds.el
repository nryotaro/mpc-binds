;;; package --- Summary key-bindings of mpc-mode. -*- lexical-binding: t -*-
;;; Commentary:

;; The prefix key for mpc commands is "m".#<buffer mpc-binds.el>

;;; Code:
(require 'mpc)
(require 'dash)
(defun mpc-bindings--bind-mpc-commands ()
  "Bind mpc commands to a keymap, returning the keymap."
  (let ((keymap (make-sparse-keymap)))
    (dolist (pair '(("p" . mpc-play)
		    ("u" . mpc-update)))
      (define-key keymap (car pair) (cdr pair)))
    keymap))

(defun mpc-bindings--select-window-by-buffer-name
    (windows name)
  "Select the widojws with a NAME in WINDOWS."
  (seq-filter
   (lambda (w) (equal name (-> w window-buffer buffer-name)))
   windows))

(defun mpc-bindings--bind-move-window
    ()
  "Bind moving to a mpc window to a keymap, returning the keymap."
  (let ((keymap (make-sparse-keymap)))
    keymap))

(define-key mpc-mode-map "m" (mpc-bindings--bind-mpc-commands))
(define-key mpc-mode-map "b" (mpc-bindings--bind-move-window))

(provide 'mpc-binds)
;;; mpc-binds.el ends here.

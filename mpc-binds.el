;;; package --- Summary key-bindings of mpc-mode. -*- lexical-binding: t -*-
;;; Commentary:

;; The prefix key for mpc commands is "m".#<buffer mpc-binds.el>

;;; Code:
(require 'mpc)
(require 'dash)

(defun mpc-bind-select-genre
    ()
  "Select *MPC Genre* window."
  (interactive)
  (mpc-bind--select-window-by-buffer-name "*MPC Genres*"))

(defun mpc-bind--bind-mpc-commands ()
  "Bind mpc commands to a keymap, returning the keymap."
  (let ((keymap (make-sparse-keymap)))
    (dolist (pair '(("p" . mpc-play)
		    ("u" . mpc-update)))
      (define-key keymap (car pair) (cdr pair)))
    keymap))

(defun mpc-bind--find-window-by-buffer-name
    (windows name)
  "Select the widojws with a NAME in WINDOWS."
  (seq-filter
   (lambda (w) (equal name (-> w window-buffer buffer-name)))
   windows))

(defun mpc-bind--select-window-by-buffer-name
    (name)
  "Select the window whose buffer is NAME."
  (when-let ((windows (mpc-bind--find-window-by-buffer-name (window-list) name)))
    (select-window (car windows))))

(defun mpc-bind--bind-move-window
    ()
  "Bind moving to a mpc window to a keymap, returning the keymap."
  (let ((keymap (make-sparse-keymap)))
    (dolist (pair '(("g" . mpc-bind-select-genre)
		    ))
      (define-key keymap (car pair) (cdr pair)))
    keymap))

(define-key mpc-mode-map "m" (mpc-bind--bind-mpc-commands))
(define-key mpc-mode-map "b" (mpc-bind--bind-move-window))

(provide 'mpc-binds)
;;; mpc-binds.el ends here.

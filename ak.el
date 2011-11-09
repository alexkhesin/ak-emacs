(defun ak-load-local-library-if-present (file)
  (let ((fname (expand-file-name file user-emacs-directory)))
    (when (file-exists-p fname)
      (load-file fname))))

(defconst ak-mac-os-x (string-match "apple-darwin" system-configuration))
(defconst ak-linux (string-match "linux-gnu" system-configuration))

(defun ak-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun ak-single ()
  (interactive)
  (delete-other-windows)
  (set-frame-width (selected-frame) 80))

(defun ak-double ()
  (interactive)
  (delete-other-windows)
  (set-frame-width (selected-frame) 163)
  ; (sleep-for 0 500)  ; it looks like set-frame-window does not take effect
                     ; immediately in CVS emacs as of 2/18/09
  (split-window-horizontally -80))

(defun ak-triple ()  (interactive)
  (delete-other-windows)
  (set-frame-width (selected-frame) 246)
  ; (sleep-for 0 500)  ; it looks like set-frame-window does not take effect
                     ; immediately in CVS emacs as of 2/18/09
  (split-window-horizontally -80)
  (split-window-horizontally -80))

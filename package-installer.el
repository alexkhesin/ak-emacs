(push "/opt/local/bin" exec-path)  ; used on mac by MacPorts
(push "/usr/local/bin" exec-path)

;;; copied from http://bytes.inso.cc/2011/08/13/auto-installing-packages-in-emacs-with-elpa-and-el-get/
; derived from ELPA installation
; http://tromey.com/elpa/install.html
(defun eval-url (url)
  (let ((buffer (url-retrieve-synchronously url)))
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (eval-region (point) (point-max))
    (kill-buffer (current-buffer)))))

(defun install-elpa ()
  (eval-url "http://tromey.com/elpa/package-install.el"))

;; Load ELPA
(if (require 'package nil t)
    (progn 
      ;; Emacs 24+ includes ELPA, but requires some extra setup
      ;; to use the (better) tromey repo
      (setq package-archives
	    (cons '("tromey" . "http://tromey.com/elpa/")
		  package-archives))
      (package-initialize))
  (install-elpa))

;; Load el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(defun install-el-get ()
  (eval-url
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"))

(defconst ak-el-get-new-install (not (require 'el-get nil t)))
(if ak-el-get-new-install
  (install-el-get))

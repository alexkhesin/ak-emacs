;; Put autosave files (ie #foo#) in one place, *not* scattered all over the
;; file system! (The make-autosave-file-name function is invoked to determine
;; the filename of an autosave file.)
;
;; /tmp gets wiped on restarts; avoid NFS (~/) on corp workstations
(defconst durable-tmp-dir
  (let ((dir (concat "/usr/local/google/home/" (user-login-name) "/tmp/")))
    (if (file-exists-p dir) dir
      (concat user-emacs-directory "tmp/"))))

(defvar autosave-dir (concat durable-tmp-dir "emacs_autosaves/"))
(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat durable-tmp-dir "emacs_backups"))
(setq backup-directory-alist (list (cons "." backup-dir)))

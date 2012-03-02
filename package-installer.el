(push "/opt/local/bin" exec-path)  ; used on mac by MacPorts
(push "/usr/local/bin" exec-path)

; copied from http://bytes.inso.cc/2011/08/13/auto-installing-packages-in-emacs-with-elpa-and-el-get/
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

; Load ELPA
(if (require 'package nil t)
    (progn
      ;; Emacs 24+ includes ELPA, but requires some extra setup
      ;; to use the (better) tromey repo
      (setq package-archives
            (cons '("marmalade" . "http://marmalade-repo.org/packages/")
                  (cons '("tromey" . "http://tromey.com/elpa/")
                        package-archives)))
      (package-initialize))
  (install-elpa))

; Load el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(defun install-el-get ()
  (eval-url
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"))

(unless (require 'el-get nil t)
  (install-el-get))
; extra recipes unknown to el-get (yet)
(setq el-get-sources
  '((:name zenburn-theme :type elpa)
    (:name solarized-theme :type elpa)
    (:name minibuf-isearch
	   :description "incremental search on minibuffer history"
	   :type http
	   :url "http://www.sodan.org/~knagano/emacs/minibuf-isearch/minibuf-isearch.el"
           :features minibuf-isearch)))

(setq my-el-get-packages
      (append
       '(el-get                    ; el-get is self-hosting
         dot-mode                  ; like ctrl+. in vi (repeat last cmd)
         switch-window             ; takes over C-x o
         ; themes
         zenburn-theme
         solarized-theme
         ; need to understand how auto-complete interacts with dabbrev
         ; auto-complete           ; complete as you type with overlays
         graphviz-dot-mode         ; graphviz Dot language
         ; http://www.emacswiki.org/emacs/UndoTree might be better than Redo
         redo+                     ; Redo/undo system for Emacs
         magit                     ; git support
         minibuf-isearch           ; incremental search on minibuffer history
         goto-last-change          ; move pointer back to last change
         inf-ruby                  ; inferior ruby mode
         ruby-compilation          ; run ruby process in compilation buffer
         ;; Rinari, textmate, ECB have all been mentioned as being useful
         ;; for Ruby/Rails. They all are heavyweight; textmate is the thinnest
         ;; of them and provides all of the functionality I need at the moment.
         textmate
         ;;; if rinari is ever turned on, nxhtml should be reinstalled as it
         ;;; seems to do things differently based on rinari's presense
         ; rinari                  ; ruby IDE
         ; ecb                     ; emacs code browser

         ; http://rinari.rubyforge.org/Rhtml-Setup.html#Rhtml-Setup
         ; says that nxhtml is better than rhtml
         nxhtml                    ; provides Mumamo among other HTML utils
         ; rhtml-mode              ; major mode for RHTML (.html.erb) files
         yaml-mode
         ; color-theme ;; not using anymore?
         emacs-goodies-el          ; misc emacs add-ons
         ; psvn                    ; svn-status
         ; yasnippet               ; textmate-like snippet mode
         ;; think about how to integrate it with google-compile?
         mode-compile                 ; mode-specific compile support
         rvm)
       (mapcar 'el-get-source-name el-get-sources)))

; do I need the three lines below, or make them run on demand somehow?
(el-get-emacswiki-refresh "~/.emacs.d/el-get/el-get/recipes/emacswiki/")
(package-refresh-contents)  ; elpa refresh
; (el-get-update-all) ; <- does not appear to be smart, don't run it every time

(defun ak-el-get-sync()
  (el-get 'sync my-el-get-packages)
  (el-get 'wait))
(ak-el-get-sync)

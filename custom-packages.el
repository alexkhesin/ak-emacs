; extra recipes unknown to el-get (yet)
(setq el-get-sources
  '((:name minibuf-isearch
	   :description "incremental search on minibuffer history"
	   :type http
	   :url "http://www.sodan.org/~knagano/emacs/minibuf-isearch/minibuf-isearch.el"
           :features minibuf-isearch)))

(setq my-el-get-packages
      (append
       '(el-get                    ; el-get is self-hosting
         escreen                   ; screen for emacs, C-\ C-h
         dot-mode                  ; like ctrl+. in vi (repeat last cmd)
         switch-window             ; takes over C-x o
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
         ; rinari                  ; ruby IDE
         ; cedet                   ; emacs IDE
         ; ecb                     ; emacs code browser, depends on CEDET
         ; http://rinari.rubyforge.org/Rhtml-Setup.html#Rhtml-Setup
         ; says that nxhtml is better than rhtml
         nxhtml                    ; provides Mumamo among other HTML utils
         ; rhtml-mode              ; major mode for RHTML (.html.erb) files
         yaml-mode
         color-theme
         emacs-goodies-el          ; misc emacs add-ons
         ; psvn                    ; svn-status
         ; yasnippet               ; textmate-like snippet mode
         ;; think about how to integrate it with google-compile?
         mode-compile                 ; mode-specific compile support
         textmate
         rvm)
       (mapcar 'el-get-source-name el-get-sources)))

; do I need the three lines below, or make them run on demand somehow?
(el-get-emacswiki-refresh "~/.emacs.d/el-get/el-get/recipes/emacswiki/")
(package-refresh-contents)  ; elpa refresh
; (el-get-update-all) ; <- does not appear to be smart, don't run it every time

(el-get 'sync my-el-get-packages)
(el-get 'wait)

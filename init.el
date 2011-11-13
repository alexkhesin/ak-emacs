; No effort is made to preserve compatibility with older versions
; (all testing is done with emacs 24)
(if (or (< emacs-major-version 24) (string-match "XEmacs" emacs-version))
    (error "Requires GNU emacs 24 or above"))

(defun ak-load-source-controlled-library (file)
  (load-file (expand-file-name file (concat user-emacs-directory "ak-emacs"))))

(ak-load-source-controlled-library "package-installer.el")
(ak-load-source-controlled-library "ecb-cedit.el")

(defconst ak-mac-os-x (string-match "apple-darwin" system-configuration))
(defconst ak-linux (string-match "linux-gnu" system-configuration))

; --------------- turn silly things off

(fset 'yes-or-no-p 'y-or-n-p)        ; no "yes" / "no" prompts
(setq inhibit-startup-message t)
(blink-cursor-mode t)

; --------------- autosave

; Put autosave files (ie #foo#) in one place, instead scattered of all over the
; file system.  Don't use /tmp as it gets wiped on restarts on many systems.
; Don't use NFS (~/) on corp workstations to keep saves fast.
(defconst durable-tmp-dir
  (let ((dir (concat "/usr/local/google/home/" (user-login-name) "/tmp/")))
    (if (file-exists-p dir) (concat dir "emacs/")
      (concat user-emacs-directory "tmp/"))))
(make-directory durable-tmp-dir t)

; from www.emacswiki.org/emacs/AutoSave
(setq backup-directory-alist         `((".*" . ,durable-tmp-dir)))
(setq auto-save-file-name-transforms `((".*"   ,durable-tmp-dir t)))

; --------------- customize visual settings

(setq truncate-partial-width-windows nil) ; wrap horizontally-split windows
(setq line-number-mode t)                 ; have line numbers and
(setq column-number-mode t)               ; column numbers in the mode line
(scroll-bar-mode -1)                      ; no scroll bars
(tool-bar-mode -1)                        ; no tool bar with icons
(show-paren-mode 1)                       ; highlight matching parens
(unless ak-mac-os-x
  ; on mac, there's always a menu bar drown, don't have it empty
  (menu-bar-mode -1))                     ; C-mouse-3 to access menu

(when ak-linux
  ; TODO(alexk) why a different method for setting font?
  ; there is also
  ;   (set-face-font 'default "Menlo-12"))
  ; and
  ;   (set-frame-font "Menlo-12")
  ; don't know what method of setting fonts is right
  (setq default-frame-alist (append default-frame-alist '((font . "7x13")))))

(color-theme-xemacs)

; TODO(alexk) on Mac full cua-mode maps copy/paste to C-v/p, whereas by default
; (on emacs 24) copy/paste is M-v/p, which is great.  Need to figure
; out if the full cua-mode is still needed on Linux.
(if ak-mac-os-x
    ; this is mostly needed for rectangle support?
    ; and perhaps delete-selection-mode, but that can be turned on separately
    ; with (delete-selection-mode 1)
    (cua-selection-mode t)
  (cua-mode t))

; --------------- Ruby config

(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))

; run a single test buffer
; (from http://www.viget.com/extend/emacs-24-rails-development-environment-from-scratch-to-productive-in-5-minu/)
(defun is-rails-project ()
  (when (textmate-project-root)
    (file-exists-p (expand-file-name "config/environment.rb"
                                     (textmate-project-root)))))
(defun run-rails-test-or-ruby-buffer ()
  (interactive)
  (if (is-rails-project)
      (let* ((path (buffer-file-name))
             (filename (file-name-nondirectory path))
             (test-path (expand-file-name "test" (textmate-project-root)))
             (command (list ruby-compilation-executable "-I" test-path path)))
        (pop-to-buffer (ruby-compilation-do filename command)))
    (ruby-compilation-this-buffer)))

; --------------- customize various modes

(autoload 'redo "redo+" nil t)
(global-set-key [(control shift z)] 'redo)
; (global-set-key "\C-?" 'redo)  ; control shift _, but cannot make it work

(setq
 ; nxhtml-global-minor-mode t  ; show nxhtml menu (is it on by default?)
 mumamo-chunk-coloring 1     ; color submode chunks of depth > 1
 nxhtml-skip-welcome t)

; (autoload 'dot-mode "dot-mode" nil t)
(eval-after-load 'dot-mode
  '(progn
     ; rebind dot-mode-execute from C-. to M-.
     (define-key dot-mode-map [(control \.)] nil)
     (define-key dot-mode-map [(meta \.)] 'dot-mode-execute)))
;; dot-mode is broken in emacs 24 because it uses obsolete make-local-hook
;; (global-set-key "\M-." (lambda () (interactive) (dot-mode 1)
;;                       (message "Dot mode activated.")))

; Install mode-compile to give friendlier compiling support (for Ruby in
; particular). (think about how to integrate it with google-compile?)
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

; whenever an external process changes a file underneath emacs, and there
; was no unsaved changes in the corresponding buffer, just revert its
; content to reflect what's on-disk.
(global-auto-revert-mode 1)

; M-x shell is a nice shell interface to use, let's make it colorful.  If
; you need a terminal emulator rather than just a shell, consider M-x term
; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

; If you do use M-x term, you will notice there's line mode that acts like
; emacs buffers, and there's the default char mode that will send your
; input char-by-char, so that curses application see each of your key
; strokes.
;
; The default way to toggle between them is C-c C-j and C-c C-k, let's
; better use just one key to do the same.
(require 'term)
(define-key term-raw-map  (kbd "C-'") 'term-line-mode)
(define-key term-mode-map (kbd "C-'") 'term-char-mode)

; Have C-y act as usual in term-mode, to avoid C-' C-y C-'
; Well the real default would be C-c C-j C-y C-c C-k.
(define-key term-raw-map  (kbd "C-y") 'term-paste)

(require 'ido)
(ido-mode 'buffers)
; (ido-mode 'both)  ;; this enables ide-find-file which I do not like
(setq ido-enable-flex-matching t)
; alexk: does not seem to have any effect
; -- but may be it does - I think I was missing the ido-default-buffer-method bit
(setq ido-default-file-method 'samewindow)
(setq ido-default-buffer-method 'samewindow)
; from https://github.com/dimitri/emacs-kicker/blob/master/init.el
; --> figure out if makes sense (says "ido for minibuffer completion")
;; (setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
;; (setq ido-use-filename-at-point 'guess)
;; (setq ido-show-dot-for-dired t)

; ibuffer config
; (global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
; (global-set-key (kbd "C-x B") 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-default-sorting-mode 'filename/process)

(require 'savehist)
(savehist-load)                         ; save minubuffers history
(require 'minibuf-isearch)              ; minibuffer incremental search (C-r,C-s)
                                        ; not sure if needed in emacs 24
(eval-after-load 'rinari
  ; run something like
  ;   ctags-exuberant -a -e -f TAGS --tag-relative -R app lib vendor
  ; to create a tag file
  '(progn (setq rinari-tags-file-name "TAGS")))

(add-hook 'write-file-hooks #'delete-trailing-whitespace)

(require 'ffap)   ; find file at point
; rebind C-x C-f and others to the ffap bindings
(ffap-bindings)
; browse urls at point via w3m
; (setq ffap-url-fetcher 'w3m-browse-url)

(load "dired-x")   ; provide some dired goodies and dired-jump at C-x C-j

; Customize buffer names when several buffers visit identically-named files
(require 'uniquify)
;; (setq uniquify-min-dir-content 1)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)

;  turn on which-func, but do not show it in the mode-line
(which-func-mode t)
(delete (assoc 'which-func-mode mode-line-format) mode-line-format)

; Spell checking
;; to install on a Mac:
;;   sudo port install aspell aspell-dict-en
(setq-default ispell-program-name "aspell")
; make ipsell faster, according to http://www.emacswiki.org/emacs/InteractiveSpell
(setq-default ispell-extra-args '("--sug-mode=ultra"))
(setq-default flyspell-persistent-highlight nil)  ; only highlight the last
                                                  ; error found
(setq-default flyspell-issue-welcome-flag nil)
(setq-default flyspell-issue-message-flag nil)
(setq-default flyspell-duplicate-distance 0)

; rebind flyspell-auto-correct-word from C-. to C-'
(defun ak-fix-flyspell-keymap ()
  (define-key flyspell-mode-map [(control \.)] nil)
  (define-key flyspell-mode-map [(control \')] 'flyspell-auto-correct-word))
(defun ak-flyspell-mode ()
  (flyspell-mode 1)
  (ak-fix-flyspell-keymap))

; Enable tab-completion
(defun ak-indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding
point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
; hippie-emacs is broken at least in the multi-tty branch I am using at
; the moment in that it does not cycle through completion candidates
; on multiple applications (TAB key presses)
;      (hippie-expand arg)
; see also http://www.emacswiki.org/emacs/TabCompletion and auto-complete mode
    (indent-according-to-mode)))

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
;       try-expand-list
        try-expand-line
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(add-hook 'text-mode-hook       'ak-flyspell-mode)

(setq ak-prog-mode-hooks
      (list 'c-mode-common-hook
            'sh-mode-hook
            'python-mode-hook
            'ruby-mode-hook
            'emacs-lisp-mode-hook
            'cperl-mode-hook
            'autoconf-mode-hook
            'autotest-mode-hook
            'makefile-mode-hook
            'css-mode-hook))

(add-hook 'c-mode-common-hook
          '(lambda ()
             ; even smarter RET for c-modes
             (local-set-key (kbd "RET") 'c-context-line-break)
             ; When I yank a piece of code ( known as paste in Windows lingo )
             ; into an existing function, I like to have it indent itself to the
             ; proper level automatically. This simple macro runs yank ( C-y )
             ; followed by an indent current function. ( C-c C-q )
             (local-set-key "\C-v" 'do-smart-yank)
             (fset 'do-smart-yank "\C-y\C-c\C-q")))

(add-hook 'css-mode-hook
          '(lambda () (setq css-indent-offset 2)))

;  common hook function for all programming language modes
(dolist (mode-hook ak-prog-mode-hooks)
  (add-hook mode-hook
       '(lambda ()
          ; (setq-default tab-width 2)  ; do I need this?
          (setq-default indent-tabs-mode nil)  ; don't replace spaces with tabs
          (setq fill-column 80)
          (turn-on-auto-fill)                       ; automatic line breaking
          (setq compilation-scroll-output t)
          (local-set-key [tab] 'ak-indent-or-expand)
          (flyspell-prog-mode)
          (ak-fix-flyspell-keymap)
          (local-set-key (kbd "RET") 'newline-and-indent)
          (local-set-key [M-down]    'next-error)
          (local-set-key [M-up]      '(lambda () (interactive) (next-error -1))))))

; allow bringing up a search-and-replace using the history mechanism in
; MiniBuffer)
(setq enable-recursive-minibuffers t)

; attempt to reuse windows instead of splitting
; --> does not work very well in emacs 24.x anymore

(setq split-height-threshold nil)
; (setq split-width-threshold nil)
; and such appear to be the right ways to tune this, see
; http://lists.gnu.org/archive/html/help-gnu-emacs/2011-02/msg00350.html
;
;; (setq display-buffer-function
;;       '(lambda (buffer-or-name not-this-window)
;;       (message "%s" buffer-or-name)
;;       (if (null (get-buffer-window buffer-or-name))
;;           (progn (if (one-window-p t)
;;                      (split-window (get-largest-window) nil t))
;;                  (set-window-buffer (get-lru-window) buffer-or-name)))
;;       (get-buffer-window buffer-or-name)))

; --------------- random utilities

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
  (split-window-horizontally)
  (balance-windows))

(defun ak-triple ()
  (interactive)
  (delete-other-windows)
  (set-frame-width (selected-frame) 246)
  ; (sleep-for 0 500)  ; it looks like set-frame-window does not take effect
                       ; immediately in CVS emacs as of 2/18/09
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))

; Go to the first character on the line
(defun chrisk-beginning-of-line ()
  (interactive)
  (let ((start (point)))
    (back-to-indentation)
    (if (= start (point)) (beginning-of-line))))

(global-set-key (kbd "C-x C-z") 'magit-status)
(global-set-key (kbd "C-x C-/") 'goto-last-change)
(global-set-key [(control shift iso-lefttab)]
  '(lambda () (interactive (other-window -1))))
(global-set-key [(control tab)] 'other-window)
(global-set-key "\M-g" 'goto-line)
;; Smart paste
(global-set-key [(meta f12)] 'recentf-open-files)
(global-set-key "\C-a" 'chrisk-beginning-of-line)
(global-set-key [home] 'chrisk-beginning-of-line)
(global-set-key "%" 'ak-match-paren)             ; parenthesis matching

(winner-mode 1)   ; C-c + <left/right> to get back to previous window layout
(windmove-default-keybindings 'meta) ; navigate windows with M-<arrows>
(setq windmove-wrap-around t)

; load local customizations if any
(defun ak-load-local-library-if-present (file)
  (let ((fname (expand-file-name file user-emacs-directory)))
    (when (file-exists-p fname) (load-file fname))))
(ak-load-local-library-if-present "local.el")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
)

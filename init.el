; No effort is made to preserve compatibility with older versions
; (all testing is done with emacs 24)
(if (or (< emacs-major-version 24) (string-match "XEmacs" emacs-version))
    (error "Requires GNU emacs 24 or above"))
(defun ak-load-source-controlled-library (file)
  (load-file (expand-file-name file (concat user-emacs-directory "ak-emacs"))))
(ak-load-source-controlled-library "package-installer.el")
; too heavy-weight, do not use for now
; (ak-load-source-controlled-library "ecb-cedit.el")

(defconst ak-mac-os-x (string-match "apple-darwin" system-configuration))
(defconst ak-linux (string-match "linux-gnu" system-configuration))
; --------------- turn silly things off

(fset 'yes-or-no-p 'y-or-n-p)          ; no "yes" / "no" prompts
(setq inhibit-startup-message t)
(blink-cursor-mode t)
(setq compilation-ask-about-save nil)  ; save all buffers before compiling

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
(cua-mode t)
(unless ak-mac-os-x
  ; on mac, there's always a menu bar drown, don't have it empty
  (menu-bar-mode -1))                     ; C-mouse-3 to access menu

(when ak-linux
  ; emacs 25 changes AVERAGE_WEIGHT part of 7x13 font spec from 70 to 80 making
  ; bold characters take more space. Probably a bug in emacs. Remove bold from
  ; all faces for now.
  (add-hook 'after-init-hook
            '(lambda () (mapc (lambda (face)
                                (set-face-attribute face nil :weight 'normal)
                                )
                              (face-list))))

  (setq default-frame-alist (append default-frame-alist '((font . "7x13"))))
  ; set-face-font should have the same effect but it renders slightly
  ; differently for some reason
  ; (set-face-font 'default "7x13")
                                        ;
  ;  ---> look into http://www.handcoding.com/archives/2006/03/30/bitstream-vera-sans-mono-is-a-sweet-programming-font/
; also https://github.com/adobe-fonts/source-code-pro
)

(when ak-mac-os-x
  ; If you want a mac-native behavior (Command-c/v/x for copy-paste, uncomment
  ; (cua-selection-mode t) below, and kill the progn that follows.  Emacs 24
  ; already defaults to M-c/v for copy/paste on a Mac, and cua-selection-mode
  ; is only needed for rectangle support (and delete-selection-mode, but that
  ; can be turned on separately with (delete-selection-mode 1).
  ;; (cua-selection-mode t)
  ;
  ; This gives more familiar bindings to those who cannot get Linux bindings
  ; out of their muscle memory, even when using emacs on a Mac.
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
)

; see https://github.com/bbatsov/zenburn-emacs/issues/89
; and http://www.emacswiki.org/emacs/ELPA#toc4
(add-hook 'after-init-hook '(lambda () (load-theme 'zenburn t)))
; (load-theme 'solarized-dark t)

;; Not planning to use Ruby at this point
;;
;; ; --------------- Ruby config
;;
;; (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))

;; ; run a single test buffer
;; ; (from http://www.viget.com/extend/emacs-24-rails-development-environment-from-scratch-to-productive-in-5-minu/)
;; (defun is-rails-project ()
;;   (when (textmate-project-root)
;;     (file-exists-p (expand-file-name "config/environment.rb"
;;                                      (textmate-project-root)))))
;; (defun run-rails-test-or-ruby-buffer ()
;;   (interactive)
;;   (if (is-rails-project)
;;       (let* ((path (buffer-file-name))
;;              (filename (file-name-nondirectory path))
;;              (test-path (expand-file-name "test" (textmate-project-root)))
;;              (command (list ruby-compilation-executable "-I" test-path path)))
;;         (pop-to-buffer (ruby-compilation-do filename command)))
;;     (ruby-compilation-this-buffer)))
;; ; textmate wants to be a global minor mode, but I only use a few commands
;; ; out of it, and it's keymap is evil - it is different on Mac vs Linux.  So
;; ; disable the minor mode, and only pull in the functions that I actually use -
;; ; textmate-project-root is used by code above, textmate-goto-file is given a key
;; ; binding below.  textmate-goto-symbol also looks interesting, maybe I should
;; ; bind a key for it too.
;; (textmate-mode 0)
; --------------- customize various modes
(global-undo-tree-mode)
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
(require 'ido)
(ido-mode 'buffers)
; (ido-mode 'both)  ;; this enables ido-find-file which I do not like
(setq ido-enable-flex-matching t)
; allow for viewing a file in two different buffers
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; from https://github.com/dimitri/emacs-kicker/blob/master/init.el
; --> figure out if makes sense (says "ido for minibuffer completion")
;; (setq ido-use-filename-at-point 'guess)
;; (setq ido-show-dot-for-dired t)
; ibuffer config
; (global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-default-sorting-mode 'filename/process)
(require 'savehist)
(savehist-mode)                         ; save minubuffers history
(setq history-delete-duplicates t)
; (eval-after-load 'rinari
;  ; run something like
;  ;   ctags-exuberant -a -e -f TAGS --tag-relative -R app lib vendor
;  ; to create a tag file
;  '(progn (setq rinari-tags-file-name "TAGS")))
(require 'ffap)   ; find file at point
; rebind C-x C-f and others to the ffap bindings
(ffap-bindings)
(setq ffap-url-fetcher 'browse-url-at-point)
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
; turn on which-func, but do not show it in the mode-line (only in title bar)
(which-function-mode t)
(delete (assoc 'which-function-mode mode-line-format) mode-line-format)
; Spell checking
; -----> hunspell does not work well yet; emacs 24.4 is supposed to make better
;;   sudo apt-get install hunspell hunspell-en-us
;;   sudo port install hunspell hunspell-dict-en_US
(setq-default ispell-program-name "hunspell")
(setq ispell-really-hunspell t)
(setq ispell-dictionary "american")
; aspell
;; to install on a Mac:
;;   sudo port install aspell aspell-dict-en
;(setq-default ispell-program-name "aspell")
; make aspell faster, according to http://www.emacswiki.org/emacs/InteractiveSpell
;(setq-default ispell-extra-args '("--sug-mode=ultra"))
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

;; google-maybe-delete-trailing-whitespace does this better
;; (add-hook 'before-save-hook
;;           '(lambda () (when (derived-mode-p 'prog-mode)
;;                         (message "foo")
;;                         (delete-trailing-whitespace))))
;  common hook function for all programming language modes
(add-hook 'prog-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (setq indent-tabs-mode nil)  ; don't replace spaces with tabs
             (setq fill-column 80)
             (setq compilation-scroll-output t)
             (turn-on-auto-fill)                       ; automatic line breaking
             (flyspell-prog-mode)
             (subword-mode 1)
             (ak-fix-flyspell-keymap)
             (local-set-key [C-f10]      'compile)
             (local-set-key [tab]       'ak-indent-or-expand)
             (local-set-key "\M-p"      'textmate-goto-file)
             (local-set-key (kbd "RET") 'newline-and-indent)
             (local-set-key [M-down]    'next-error)
             (local-set-key [M-up]      '(lambda () (interactive)
                                           (next-error -1)))))
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
; --> display-buffer in emacs 24 is much more complex and overriding
; display-buffer-function does not seem to work cleanly anymore (in particular,
; in emacs 23, display-buffer-function was not called at all on opening new
; files, whereas in emacs 24 it is called, and with override below, new files
; are always open in lru window, which is not the desired effect (should be the
; current window).  But emacs 24 seems to behave sensibly even without this
; override; the biggest ill effect of not having it is that gtags searches do
; not seem to be picking LRU windows, which needs to be investigated.
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
  (split-window-horizontally)
  (balance-windows))
(defun ak-triple ()
  (interactive)
  (delete-other-windows)
  (set-frame-width (selected-frame) 246)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))
(defun ak-mac-height ()
  (interactive)
  (set-frame-height (selected-frame) 65))
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

;;; CEDET/ECB set up is from http://appsintheopen.com/articles/1-setting-up-emacs-for-rails-development/part/6-setting-up-the-emacs-code-browser
;; CEDET
; enable EDE (Project Management) features
(global-ede-mode 1)
; this enables the database and idle reparse engines
;; (semantic-load-enable-minimum-features)
; this enables some tools useful for coding, such as summary mode
; imenu support, and the semantic navigator
;; (semantic-load-enable-code-helpers)

;; ECB
; This variable is gone in emacs 24 but ECB still references it
(setq stack-trace-on-error t)
; wtf does this not work here but works in custom-set-variables??
; (setq ecb-options-version "2.40")
(setq ecb-layout-name "left8")
; (setq ecb-layout-window-sizes
;  (quote (("left14" (0.2564102564102564 . 0.6949152542372882)
;  (0.2564102564102564 . 0.23728813559322035)))))
; add the location of your code files so they are quickly
; accessible in ECB:
(setq ecb-source-path (quote ("~/ror")))
; By default, ecb opens files using the middle mouse button.
; Use the left mouse button instead:
(setq ecb-primary-secondary-mouse-buttons
      (quote mouse-1--C-mouse-1))
; get rid of the tip-of-the-day and use ascii directory listings:
(setq ecb-tip-of-the-day nil)
(setq ecb-tree-buffer-style (quote ascii-guides))

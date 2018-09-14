;;; dark-poet-theme.el --- A dark prose friendly theme.

;; Copyright 2018-now Kunal Bhalla

;; Author: Kunal Bhalla <bhalla.kunal@gmail.com>
;; URL: https://github.com/kunalb/poet/
;; Version: 2.0

;;; Commentary:

;; Emacs has very good support for multiple fonts in a single
;; file.  Poet uses this support to make it much more convenient to
;; write prose within Emacs, with particular attention paid to
;; org-mode and markdown-mode.  Code blocks, tables, etc are
;; formatted in monospace text with the appropriate backgrounds.

;; Recommended customizations for using this theme
;;
;; - Set up the base fonts you'd like to use in Emacs before loading Poet
;;     (set-face-attribute 'default nil :family "Iosevka" :height 130)
;;     (set-face-attribute 'fixed-pitch nil :family "Iosevka")
;;     (set-face-attribute 'variable-pitch nil :family "Baskerville")
;;   On loading this theme captures the default and treats that for fixed-pitch
;;   rendering.
;;
;; - Enable variable pitch mode for editing text
;; (add-hook 'text-mode-hook
;;            (lambda ()
;;             (variable-pitch-mode 1))
;;
;; - Some other modes I like to enable/disable
;;     (olivetti-mode 1)        ;; Centers text in the buffer
;;     (flyspell-mode 1))       ;; Catch Spelling mistakes
;;     (typo-mode 1))           ;; Good for symbols like em-dash
;;     (blink-cursor-mode 0)    ;; Reduce visual noise
;;     (linum-mode 0)           ;; No line numbers for prose
;;
;; - And prettier org mode bullets:
;;     (setq org-bullets-bullet-list
;;         '("◉" "○"))
;;     (org-bullets 1)

;;; Code:

(deftheme dark-poet
  "A dark prose friendly theme.")

;;; Utility functions

(custom-theme-set-faces
 'dark-poet
 '(default ((t (:foreground "#ffffff" :background "#4B4036"))))
 '(fringe ((t (:background nil))))
 '(mode-line ((t (:foreground "#eeeeee" :overline "#8c8771"))))
 '(vertical-border ((t (:foreground "#8c8771" :background "#8c8771"))))
 '(window-divider ((t (:foreground "#8c8771" :background "#8c8771")))))

(custom-theme-set-variables
 'dark-poet
 '(line-spacing .2)
 '(fci-rule-color "#111111"))

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))

(provide-theme 'dark-poet)
;;; dark-poet-theme.el ends here

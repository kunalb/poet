;;; poet2-theme.el -- A theme for prose.

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
;;     (flyspell-mode 1)        ;; Catch Spelling mistakes
;;     (typo-mode 1)            ;; Good for symbols like em-dash
;;     (blink-cursor-mode 0)    ;; Reduce visual noise
;;     (linum-mode 0)           ;; No line numbers for prose
;;
;; - And prettier org mode bullets:
;;     (setq org-bullets-bullet-list
;;         '("◉" "○"))
;;     (org-bullets 1)

;;; Code

(deftheme poet2
  "A prose friendly theme.")

(let ((fg "#444444")
      (bg "#e1d9c2")
      (emph "#222222")
      (sep "#eeeeee")
      (hlt "#efefef")
      (bg-hlt "#fff8e1")
      (muted "#795548")
      (meta "#4e342e")
      (link "#303f9f")
      (link-underline "#304ffe")
      (vlink-underline "#1a237e")
      (header "#770b0b")
      (button "#616161")
      (glyph "#673AB7")
      (cursor "#333333")
      (paren-match-bg "#ff1744")
      (paren-match-fg "#ffffff")
      (search-fg "#c2185b")
      (search-bg "#ffffff")
      (search-fail-bg "#f8bbd0")
      (tooltip-fg "#111111")
      (tooltip-bg "#fff176")
      (shadow "#999999")
      (secondary-bg "#fff59d")
      (trailing-bg "#ff8a65")
      (header-line-bg "#e0e0e0"))
 (custom-theme-set-faces 'poet2
  `(default ((t (:foreground ,fg :background ,bg))))
  `(italic ((t (:foreground ,emph))))
  `(fringe ((t (:background ,bg))))
  `(button ((t (:inherit default :foreground ,button))))
  `(link ((t (:foreground ,link :underline (:color ,link-underline :style line)))))
  `(link-visited ((t (:inherit link :underline (:color ,vlink-underline :style line)))))
  `(escape-glyph ((t (:foreground ,glyph))))
  `(highlight ((t (:background ,hlt :overline nil))))
  `(lazy-highlight ((t (:background ,bg-hlt))))
  `(region ((t (:background ,bg-hlt))))
  `(cursor ((t (:background ,cursor))))
  `(show-paren-match ((t (:foreground ,paren-match-fg :background ,paren-match-bg))))
  `(isearch ((t (:foreground ,search-fg :background ,search-bg))))
  `(isearch-fail ((t (:background ,search-fail-bg))))
  `(next-error ((t (:background ,bg-hlt))))
  `(query-replace ((t (:inherit isearch))))
  `(tooltip ((t (:foreground ,tooltip-fg :background ,tooltip-bg))))
  `(shadow ((t (:foreground ,shadow))))
  `(secondary-selection ((t (:background ,secondary-bg))))
  `(trailing-whitespace ((t (:background ,trailing-bg))))
  `(window-divider ((t (:background ,sep :foreground ,sep))))
  `(vertical-border ((t (:background ,sep :foreground ,sep))))
  `(mode-line ((t (:inherit fixed-pitch :foreground "#111111" :background ,bg :overline ,sep :box (:line-width 3 :color ,bg)))))
  `(header-line ((t (:overline nil :background ,header-line-bg :box (:line-width 3 :color ,header-line-bg) :underline ,sep :inherit mode-line))))
  `(mode-line-buffer-id ((t (:underline t))))
  `(mode-line-emphasis ((t (:weight bold))))
  `(mode-line-highlight ((t (:background "#ffffff"))))
  `(mode-line-inactive ((t (:inherit mode-line :background ,bg :foreground "#888888" :box (:color ,bg :line-width 3))))))
 (custom-theme-set-variables 'poet2
  '(line-spacing .2)
  `(fci-rule-color ,'fci)))

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))

(provide-theme 'poet2)

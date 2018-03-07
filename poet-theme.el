(deftheme poet
  "A variable-pitch-mode friendly theme for editing text in org.")

;;; Utility functions

;; Allow setting faces as (face :property value ...) directly
;; This theme is defined for the GUI only.
(defun poet--flatten (face-details)
  `(,(car face-details) ((t ,(cdr face-details)))))
(poet--flatten '(default :background "#ffffff"))

(defun poet--create-faces (faces)
    (apply
     'custom-theme-set-faces
     (cons 'poet
           (mapcar 'poet--flatten faces))))

;; TODO: Consider making this a macro instead
(defun poet--numbered-faces (prefix
                             range-start
                             range-end
                             callback)
  (mapcar
   (lambda (iter)
     `(,(intern (concat prefix (number-to-string iter)))
       ,@(if (functionp callback)
              (funcall callback iter)
            callback)))
   (number-sequence range-start range-end)))

(let*
    (;; Theme design

     ;; Typography
     (serif-family "Georgia")
     (sans-serif-family "Helvetica Neue")
     (monospace-family "Fira Code")

     (base-height 160)
     (monospace-height 130)

     ;; Colors
     (bg "#e1d9c2")
     (fg "#444444")

     (bg-highlight "#fff8e1")

     (muted "#795548")
     (highlight "#efefef")
     (meta "#4e342e")
     (link "#303F9F")

     (header-color "#970b0b")

     (basic
      `((fixed-pitch
         :family ,monospace-family
         :height ,monospace-height)
        (variable-pitch
         :family ,serif-family
         :height ,base-height)
        (default
          :inherit fixed-pitch
          :background ,bg
          :foreground ,fg
          :height ,monospace-height)
        (italic
         :foreground "#222222"
         :slant italic)
        (highlight
         :background ,highlight)
        (region
         :background ,bg-highlight)
        (fringe
         :background ,bg)
        (button
         :inherit default
         :foreground "#616161")
        (escape-glyph
         :foreground "#673AB7")
        (link
         :underline (:color "#304FFE" :style line)
         :foreground ,link)
        (link-visited
         :inherit link
         :underline (:color "#1A237E" :style line))
        (cursor
         :background "#333333")
        (show-paren-match
         :background "#FF1744"
         :foreground "#ffffff")

        (isearch
         :foreground "#C2185B"
         :background "#ffffff")
        (isearch-fail
         :background "#f8bbd0")
        (query-replace
         :inherit isearch)

        (tooltip
         :inherit default
         :foreground "#111111"
         :background "#fff176")

        (shadow
         :foreground "#999999")

        (secondary-selection
         :background "#FFF59D")

        (trailing-whitespace
         :background "#FF8A65")

        (lazy-highlight
         :background "white")

        (next-error
         :inherit region)

        (window-divider
         :background "#ffffff"
         :foreground "#ffffff")))

     (evil
      `((evil-ex-substitute-replacement
         :foreground "#ffffff"
         :background "#4E342E"
         :underline nil)))

     (minibuffer
      `((minibuffer-prompt
         :inherit fixed-pitch
         :weight bold
         :foreground ,meta)))

     (mode-line
      `((mode-line
         :inherit fixed-pitch
         :foreground "#111111"
         :background "#dedede"
         :box (:line-width 3
               :color "#dedede"))
        (header-line
         :inherit mode-line)
        (mode-line-buffer-id
         :underline t)
        (mode-line-emphasis
         :weight bold)
        (mode-line-highlight
         :background "#ffffff")
        (mode-line-inactive
         :inherit mode-line
         :background "#cccccc"
         :foreground "#777777"
         :box (:color "#cccccc"
               :line-width 3))))

     (syntax
      `((font-lock-comment-face
         :foreground ,muted
         :inherit fixed-pitch)
        (font-lock-builtin-face
         :foreground "#795548"
         :inherit fixed-pitch)
        (font-lock-string-face
         :inherit fixed-pitch
         :foreground "#7B1FA2")
        (font-lock-function-name-face
         :inherit fixed-pitch
         :foreground "#388E3C")
        (font-lock-keyword-face
         :inherit fixed-pitch
         :foreground "#bf360c")
        (font-lock-comment-delimiter-face
         :inherit font-lock-comment-face)
        (font-lock-constant-face
         :foreground "#0288D1")
        (font-lock-doc-face
         :inherit font-lock-string-face)
        (font-lock-preprocessor-face
         :inherit font-lock-builtin-face)
        (font-lock-regexp-grouping-backslash
         :inherit bold)
        (font-lock-regexp-grouping-construct
         :inherit bold)
        (font-lock-type-face
         :foreground "#3f51b5"
         :inherit fixed-pitch)
        (font-lock-variable-name-face
         :foreground "#455A64")
        (font-lock-warning-face
         :inherit error)))

     (org
      `(,@(poet--numbered-faces
           "org-level-" 1 8
           (lambda (index)
             (list
              ':inherit 'default
              ':foreground header-color
              ':underline (list ':color header-color)
              ':height (max
                        (+ 20 base-height)
                        (- 260 (* 20 index))))))

        (org-meta-line
         :inherit fixed-pitch
         :foreground "#8D6E63")

        (org-document-info-keyword
         :inherit fixed-pitch
         :foreground "#795548")

        (org-verbatim ; inline code
         :inherit fixed-pitch)

        (org-table
         :inherit fixed-pitch
         :background "#e0e0e0")
        (org-formula
         :inherit org-table
         :height ,monospace-height)

        (org-quote
         :inherit default
         :foreground "#4A148C")

        (org-hide
         :foreground ,bg)

        (org-document-title
         :inherit default
         :foreground "#B71C1C"
         :height 300
         :underline (:color "#aaaaaa"))

        (org-checkbox
         :inherit fixed-pitch
         :weight bold
         :foreground "#aaaaaa")

        (org-done
         :inherit fixed-pitch
         :foreground "#388E3C")
        (org-todo
         :inherit fixed-pitch
         :foreground "#BF360C")

        (org-tag
         :inherit fixed-pitch
         :height 140
         :foreground "#777777")

        (org-block-begin-line
         :inherit fixed-pitch
         :background "#d0d0d0")

        (org-block-end-line
         :inherit fixed-pitch
         :background "#d0d0d0")

        (org-block
         :background "#e0e0e0"
         :inherit fixed-pitch)))

     (hl-line
      `((hl-line
         :background "#efefef")))


     (linum
      `((linum-highlight-face
         :inherit default
         :foreground "#555555")
        (linum
         :inherit default
         :foreground "#aaaaaa")))

     (markdown
      `((markdown-markup-face
         :inherit fixed-pitch
         :foreground "#8D6E63")

        (markdown-inline-code-face
         :inherit fixed-pitch)

        ,@(poet--numbered-faces
           "markdown-header-face-" 1 8
           (lambda (index)
             (list
              ':foreground header-color
              ':inherit 'default
              ':underline '(:color "#aaaaaa")
              ':height (max
                        (+ 20 base-height)
                        (- 260 (* 20 index))))))))

     (imenu-list
      `(,@(poet--numbered-faces
           "imenu-list-entry-face-" 0 5
           '(:foreground "#4e342e")))))
  (poet--create-faces
   `(,@basic
     ,@minibuffer
     ,@syntax
     ,@mode-line
     ,@evil
     ,@org
     ,@linum
     ,@hl-line
     ,@imenu-list
     ,@markdown)))

(custom-theme-set-variables
 'poet
 '(fci-rule-color "#dedede"))

;;;###autoload
(provide-theme 'poet)

(defun poet--enable ()
  (linum-mode 0)
  (setq left-margin-width 5)
  (setq right-margin-width 5)
  (set-window-buffer (get-buffer-window) (window-buffer))
  (setq line-spacing .2))

(defun poet--disable ()
  (setq left-margin-width 0)
  (setq right-margin-width 0)
  (set-window-buffer (get-buffer-window) (window-buffer))
  (variable-pitch-mode 0)
  (setq line-spacing 0))

(add-hook 'poet-mode-hook
          (lambda ()
            (variable-pitch-mode 1)
            (flyspell-mode 1)
            (typo-mode 1)
            (blink-cursor-mode 0)))

;;;###autoload
(define-minor-mode poet-mode
  "Another opinionated minor mode to help you write."
  :lighter " prolific"
  (progn
    (if poet-mode
        (poet--enable)
      (poet--disable))))

;;;###autoload
(provide 'poet-mode)

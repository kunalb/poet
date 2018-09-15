;;; metapoet.el --- Generates variations of Poet, a prose friendly theme.

;; Copyright 2018- Kunal Bhalla

;; Author: Kunal Bhalla <bhalla.kunal@gmail.com>
;; URL: https//github.com/kunalb/poet
;; Version: 2.0

;;; Commentary:

;; This file isn't an Emacs theme directly: instead, it generates
;; multiple themes that are.  This allows me to be as sloppy as I want
;; while designing the themes and using hashmaps or whatever else I
;; want, and then generate the colours precisely *once*.

;; Generate themes with: Emacs -batch -l metapoet.el

;;; Template:


;;; Code:

(defun metapoet--enable-if-current (theme-name)
  "Update the current theme for faster iteration."
  (if (member theme-name custom-enabled-themes)
      (load-file (concat (symbol-name theme) "-theme.el"))))

(defun metapoet--create (theme params)
  "Create the actual themes.

  PARAMS defines all the custom values used in the theme."
  (let ((theme-name (alist-get 'theme params)))
    (with-temp-buffer
      (insert-file-contents "poem.el.txt")
      (dolist (param params)
        (goto-char (point-min))
        (message (symbol-name (car param)))
        (message (cdr param))
        (while (search-forward (concat "%" (symbol-name (car param)) "%") nil t)
          (replace-match (cdr param))))
      (write-region
       nil
       nil
       (concat theme-name "-theme.el")))

    (metapoet--enable-if-current theme)))


(defun metapoet--sort-face-list (ls)
  "Sort face spec LS by face name"
  (sort ls (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))


(defun metapoet--inner-merge (as bs)
  "Helper function to merge AS and BS for metapoet--merge-faces."
  (cond
   ((and as bs (equal (caar as) (caar bs)))
    (cons (append (car as) (cdar bs)) (metapoet--inner-merge (cdr as) (cdr bs))))
   ((and as bs (string< (symbol-name (caar as)) (symbol-name (caar bs))))
    (cons (car as) (metapoet--inner-merge (cdr as) bs)))
   ((and as bs)
    (cons (car bs) (metapoet--inner-merge as (cdr bs))))
   (as as)
   (bs bs)
   (t '())))


(defun metapoet--merge-faces (as bs)
  "Merge lists of face specs AS and BS."
  (let ((as (metapoet--sort-face-list as))
        (bs (metapoet--sort-face-list bs)))
    (metapoet--inner-merge as bs)))


(defun metapoet--make-faces (fs)
  "Create a list of face specs."
  (mapconcat
   (lambda (x)
     (format "'(%s ((t %S)))" (car x) (cdr x)))
   fs
   "\n "))

(defvar poet--monospace-height
  (face-attribute 'fixed-pitch :height nil 'default)
  "The base size to use: specified as a defvar to stay consistent.")

(defun metapoet--create-face-specs (al)
  (let ((pt (lambda (x) (alist-get x al))))
  `((default
      :foreground ,(funcall pt 'fg)
      :background ,(funcall pt 'bg))
    (vertical-border :foreground "#8c8771"
                     :background "#8c8771")
    (window-divider :foreground "#8c8771"
                    :background "#8c8771")
    (mode-line :foreground "#eeeeee"
               :overline "#8c8771")
    (fringe :background nil))))

(let* (
       (structural-faces
        '((mode-line :inherit fixed-pitch
                     :box (:line-width 3))))

       (dark-poet-faces
        '((default :foreground "#ffffff"
                   :background "#4B4036")
          (vertical-border :foreground "#8c8771"
                           :background "#8c8771")
          (window-divider :foreground "#8c8771"
                          :background "#8c8771")
          (mode-line :foreground "#eeeeee"
                     :overline "#8c8771")
          (fringe :background nil)))

       (dark-poet-params
        `((theme . "dark-poet")
          (tagline . "A dark prose friendly theme.")
          (faces . ,(metapoet--make-faces
                     (metapoet--create-face-specs
                      '((fg . "#ffffff")
                        (bg . "#8c8771")))))
          (fci-rule-color . "#111111"))))
  (metapoet--create 'dark-poet dark-poet-params))

;;; metapoet.el ends here

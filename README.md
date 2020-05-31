# poet [![MELPA](https://melpa.org/packages/poet-theme-badge.svg)](https://melpa.org/#/poet-theme)
An emacs theme that's well suited for prose: particularly org-mode and markdown-mode; or any mode that works well with `variable-pitch` mode.

## Motivation
The main idea behind this was to make Emacs more friendly for writing prose instead of code. Highlights include mixing monospace and variable pitch text in markdown and org-modes, making it convenient to write prose and code together. I wrote about building poet at https://explog.in/notes/poet.html. For example,

<p align="center">
  <img title="Screenshots" alt="Screenshots" src="https://github.com/kunalb/poet/raw/master/images/poet.gif" />
</p>

## Set up
- **Installation**:
  - **Melpa**: Make sure [melpa is in your package archives list](https://melpa.org/#/getting-started), and <kbd>M-x</kbd> `package-install poet-theme`
  - **Manual**: Download the theme to your `custom-theme-directory` which defaults to `~/emacs.d`.
- **Variable-pitch-mode**: Adding variable pitch mode to text modes will help in rendering mixed fonts every time you edit  markdown, org-mode, etc.
```
    (add-hook 'text-mode-hook
               (lambda ()
                (variable-pitch-mode 1)))
```
- **Custom fonts**: Choose your fonts/font sizes before loading the theme with
```
    (set-face-attribute 'default nil :family "Iosevka" :height 130)
    (set-face-attribute 'fixed-pitch nil :family "Iosevka")
    (set-face-attribute 'variable-pitch nil :family "Baskerville")
```
- **Mac OSX Title Bar**: Only for **Emacs 26.1**
```
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . light))
```
- **Additional useful modes**: Some modes I like to enable/disable
```
    (olivetti-mode 1)        ;; Centers text in the buffer
    (flyspell-mode 1)        ;; Catch Spelling mistakes
    (typo-mode 1)            ;; Good for symbols like em-dash

    (blink-cursor-mode 0)    ;; Reduce visual noise
    (linum-mode 0)           ;; No line numbers for prose

    (setq org-bullets-bullet-list
        '("◉" "○"))
    (org-bullets 1)
```

## [Reddit](https://www.reddit.com/r/emacs/comments/9e01wf/share_your_modern_emacs/e5lpfmy/) reviews poet
![Reddit](https://github.com/kunalb/poet/raw/master/images/reddit.png)

## Warning
- Exclusively aimed at graphical emacs

## Next Steps
(No fixed timelines for these, depends on my time and motivation)
- [x] Publish v1 to Melpa.
- [x] Add more screenshots showing off different modes (particularly markdown)
- [x] Evaluate & test github flavoured markdown in markdown-mode.
- [x] Publish v2 to Melpa
- [x] Create a poet-dark mode with a yellow/white/black scheme.
- [x] Add a dark brown theme
- [x] Add support for disabling variable font sizes.
- [ ] Explore a new bolder version of poet.
- [ ] Document support for additional modes.
- [ ] Figure out
- [ ] Publish v3 to Melpa.

## More Screenshots
![Org](https://github.com/kunalb/poet/raw/master/images/org.png)
![Markdown 1](https://github.com/kunalb/poet/raw/master/images/md1.png)
![Markdown 2](https://github.com/kunalb/poet/raw/master/images/md2.png)

# poet [![MELPA](https://melpa.org/packages/poet-theme-badge.svg)](https://melpa.org/#/poet-theme)
A light emacs theme that's well suited for prose: particularly org-mode and markdown-mode; or any mode that works well with `variable-pitch` mode.

## Motivation
The main idea behind this was to make Emacs more friendly for writing prose instead of code. Highlights include mixing monospace and variable pitch text in markdown and org-modes, making it convenient to write prose and code together. For example,

![Screenshot](https://github.com/kunalb/poet/raw/master/screenshot.png)

## Set up
- **Installation**: 
  - **Melpa**: Make sure [melpa is in your package archives list](https://melpa.org/#/getting-started), and <kbd>M-x</kbd> `package-install melpa`
  - **Manual**: Download the theme to your `custom-theme-directory` which defaults to `~/emacs.d`.
- **Variable-pitch-mode**: Adding variable pitch mode to text modes will help in rendering mixed fonts every time you edit  markdown, org-mode, etc.
```
    (add-hook 'text-mode-hook
               (lambda ()
                (variable-pitch-mode 1))
```
- **Custom fonts**: Choose your fonts/font sizes before loading the theme with
```
    (set-face-attribute 'default nil :height 130)
    (set-face-attribute 'fixed-pitch nil :family "Fira Code")
    (set-face-attribute 'variable-pitch nil :family "Georgia")
```
- **Additional useful modes**: Some modes I like to enable/disable
```
    (olivetti-mode 1)        ;; Centers text in the buffer
    (flyspell-mode 1))       ;; Catch Spelling mistakes
    (typo-mode 1))           ;; Good for symbols like em-dash

    (blink-cursor-mode 0)    ;; Reduce visual noise
    (linum-mode 0)           ;; No line numbers for prose

    (setq org-bullets-bullet-list
        '("◉" "○"))
    (org-bullets 1)
```

## Warning
This is still a very early version of the theme, and there are several caveats:
- Exclusively aimed at graphical emacs
- Not very widely tested

## Next Steps
(No fixed timelines for these, depends on my time and motivation)
- [x] Publish v1 to Melpa.
---
- [x] Add more screenshots showing off different modes (particularly markdown)
- [ ] Add support for customizing font faces
- [ ] Document support for additional modes.
- [ ] Evaluate & test github flavoured markdown in markdown-mode.
- [ ] Publish v2 to Melpa
---
- [ ] Create a poet-dark mode with a yellow/white/dark gray scheme.
- [ ] Publish v3 to Melpa.

## More Screenshots
![Org](https://github.com/kunalb/poet/raw/master/org.png)
![Markdown 1](https://github.com/kunalb/poet/raw/master/md1.png)
![Markdown 2](https://github.com/kunalb/poet/raw/master/md2.png)


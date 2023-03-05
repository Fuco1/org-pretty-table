# org-pretty-table

Draw pretty unicode tables in `org-mode` and `orgtbl-mode`.

Very experimental.

# Installation

This package is not yet distributed through package archives and you have to install it manually.

1. Download the `org-pretty-table.el` file into some folder in your hard drive (for example `~/.emacs.d/site-lisp`).

2. Put the following code into your `~/.emacs.d/init.el` or `~/.emacs` file:

``` emacs-lisp
(progn
  (add-to-list 'load-path "~/.emacs.d/site-lisp")
  (require 'org-pretty-table)
  (add-hook 'org-mode-hook (lambda () (org-pretty-table-mode))))
```

3. Evaluate the `progn` block by putting the point on the `progn` and calling `C-M-x` (`M-x eval-defun`).  If unsure, you can also simply restart Emacs.

4. Open an org-mode file.  Calling `M-: org-pretty-table-mode RET` should print `1` in the minibuffer.  You're done!

# Development

Use Eask to install dependencies:

``` shell
eask install-deps --dev
```

Run tests with

``` shell
eask test buttercup
```

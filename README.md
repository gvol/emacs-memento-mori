# emacs-memento-mori
Reminder of our mortality.

Shows your age in the global mode line as a reminder to squander less of
your brief time on this earth.

Your age is shown with two decimal places so you can witness it
increasing every 3-4 days. People commonly regard themselves as N years
old until the day they turn N+1 years old. The decimals remind you that
this is a false comfort: many N-year-olds are already closer to being
N+1 years old.

# Installation

Install from [MELPA](https://melpa.org/#/memento-mori).

# Usage
* Set `memento-mori-birth-date` in `YYYY-MM-DD` format.
* enable `memento-mori-mode`
* reflect

e.g. If you have something like the following in your emacs config

```emacs-lisp
;; Add something like the following adjusting the date accordingly to
;; your init.el.

(require 'memento-mori)
(setq memento-mori-birth-date "1970-01-01")
(memento-mori-mode)

;; Or using use-package:
 (use-package memento-mori
   :ensure t
   :init (setq memento-mori-birth-date "1970-01-01")
   :config (memento-mori-mode))
```

You'll see something like `49.54 years old` in your modeline.

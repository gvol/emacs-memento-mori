# emacs-memento-mori

Reminder of our mortality.

"Memento Mori," or consciously remembering one's own death, has a long
history in Western philosophical thought.  It is a way of reminding
oneself that death is inevitable, thereby increasing gratitude for the
time left, and motivation to live to the fullest.

Death, of course, is not the only ending, and so this package allows you to
choose what to be reminded of (including time _since_ some event such as your
birth).  Every day it will choose a new memento randomly and put it into
`global-mode-string' which is usually displayed in the mode line.

# Installation

Install from [MELPA](https://melpa.org/#/memento-mori).

# Usage
* Customize `memento-mori-mementos`
* enable `memento-mori-mode`
* reflect

e.g. If you have something like the following in your emacs config

```emacs-lisp
(require 'memento-mori)
(setq memento-mori-mementos
      '(("%.2f years old" :since "1970-01-01")))
(memento-mori-mode)
```

Or using `use-package`:

```emacs-lisp
(use-package memento-mori
  :ensure t
  :custom (memento-mori-mementos
           '(("%.2f years old" :since "1970-01-01")))
  :config (memento-mori-mode))
```

You'll see something like `49.54 years old` in your modeline.

## Customization

This package will likely not be useful to anyone out of the box.  To
have a meaningful experience you should customize `memento-mementos`.
You can do this by running `M-x customize-group RET memento RET`.
While it's unlikely that these exact questions will have the desired
psychological impact for you, here are a few example items which you
can refer to when creating your own.

Some questions to consider:
* How long until you will "likely" die?  I used https://population.io/
  to get an estimate of when I will die based on just birth date,
  country, and gender.  Undoubtedly, a better estimate could be had
  with more information (e.g. about general health), but it was simple
  and it's only an estimate.
* What percentage of your life have you "likely" lived?
* How long until your child(ren) will leave home?
* How many more vacations you will take (with your children)?
* How long until your (grand)parents die?
* How long until you are no longer a 20-something?
* How long until you reach "peak unhappiness" ([47.2 years old](https://link.springer.com/article/10.1007/s00148-020-00797-z))?
* How long has it been since some traumatic experience?

Consider using weeks or fractional years.  That way you can see the
number change every so often and be reminded how short life actually
is.  This package is not meant to be a count-down timer, so it only
updates once a day, but noticing a change helps to encourage the
desired reflection.

If the mode-line is too much, you could add it to your
`org-agenda-custom-commands` or similar.

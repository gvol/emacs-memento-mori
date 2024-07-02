;;; memento-mori.el --- Reminder of our mortality  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023  Lassi Kortela

;; Author: Lassi Kortela <lassi@lassi.io>
;; Maintainer: Ivan Andrus <darthandrus@gmail.com>
;; URL: https://github.com/gvol/emacs-memento-mori
;; Version: 0.3.2
;; Package-Requires: ((emacs "24.4"))
;; Keywords: help
;; SPDX-License-Identifier: ISC

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Shows your age in the global mode line as a reminder to squander less
;; of your brief time on this earth.
;;
;; Your age is shown with two decimal places so you can witness it
;; increasing every 3-4 days.  People commonly regard themselves as N
;; years old until the day they turn N+1 years old.  The decimals remind
;; you that this is a false comfort: many N-year-olds are already closer
;; to being N+1 years old.

;;; Usage:

;; Add something like the following (adjusting the date accordingly) to your
;; init.el.  If you have already set `memento-mori-birth-date', but not set
;; `memento-mori-mementos', then it will continue to work just providing your
;; age.  However, it's strongly encouraged to customize `memento-mori-mementos'
;; since it is much more flexible and can provide better reminders that you will
;; die soon enough.

;; (require 'memento-mori)
;; (setq memento-mori-mementos
;;       '(("%.5Y years old" :since "1970-01-01")))
;; (memento-mori-mode)

;; Or using use-package:
;; (use-package memento-mori
;;   :ensure t
;;   :custom (memento-mori-mementos
;;            '(("%.5Y years old" :since "1970-01-01")))
;;   :config (memento-mori-mode))

;;; Code:

(require 'format-spec)

(defgroup memento-mori nil
  "Reminder of our mortality."
  :group 'help)

(defcustom memento-mori-display-in-modeline t
  "If non-nil, `memento-mori-mode' will add mementos to the mode line.
Really, it adds it to the `global-mode-string', which usually
appears in the mode line, however, if you have customized your
mode line it may not appear.

If your mode line is already customized or the output provided by
`memento-mori-display-in-modeline' isn't to your liking, you can add
`memento-mori--modeline-info' to `mode-line-format' manually instead of
setting this variable."
  :group 'memento-mori
  :type 'boolean
  :set #'memento-mori--set-value-and-update-display
  :initialize #'custom-initialize-default)

(defcustom memento-mori-display-in-frame-title nil
  "If non-nil, `memento-mori-mode' will add mementos to the frame title.

If your frame title is already customized or the output provided by
`memento-mori-display-in-frame-title' isn't to your liking, you can add
`memento-mori--frame-title-info' to `frame-title-format' manually
instead of setting this variable."
  :group 'memento-mori
  :type 'boolean
  :set #'memento-mori--set-value-and-update-display
  :initialize #'custom-initialize-default)

(defcustom memento-mori-initial-scratch-message nil
  "If non-nil, overwrite the value of `initial-scratch-message' with this.
If this contains a %q it will be replaced with a random quote from
 `memento-mori-quotes'.  Similarly, a %m will be replaced with the
 current memento.

Ultimately, it will be displayed as the initial documentation in
*scratch* buffer at startup."
  :group 'memento-mori
  :type '(choice (text :tag "Scratch message format")
		         (const :tag "none" nil)))

(defcustom memento-mori-birth-date ""
  "Your birth date in YYYY-MM-DD format.
This is deprecated in favor of the more flexible `memento-mori-mementos'.
To use `memento-mori-mementos' customize it, or erase customization of
`memento-mori-birth-date'."
  :group 'memento-mori
  :type 'string
  :set #'memento-mori--set-value-and-update
  :initialize #'custom-initialize-default)
(make-obsolete-variable 'memento-mori-birth-date
                        'memento-mori-mementos "0.2.1")

(defcustom memento-mori-mementos
  '(("%y years since Harrison Bergeron was published"
     :since "1969-10-01")
    ("%y years until Harrison Bergeron becomes fully equal"
     :until "2081-03-14")
    ("%p%% of the way between Harrison Bergeron's writing and equality"
     :since "1969-10-01"
     :until "2081-03-14")
    ("%.5W weeks until Y2038"
     :until "2038-01-19 03:14:07")
    ("%.5Y years since Y2K"
     :since "2000-01-01")
    ("%.2P%% between Y2K and Y2038"
     :until "2038-01-19 03:14:07"
     :since "2000-01-01")
    ("%f lunar cycles since man set foot on the moon"
     :since "1969-07-20 20:17:40"
     :formula (lambda (days) (/ days 29.5))))
  "List of mementos, namely things that will remind you of your mortality.

Each memento is a list, the first element of which is a format
string.  The rest is a plist, namely a list of alternating
key/value pairs.  The supported keys are

:until  A time specification indicating the memento will happen in
        the future.
:since  A time specification indicating the memento happened in
        the past.

:formula A lambda taking either one or two arguments, both of which are
        the number of days as a float.  The two argument case takes
        since and until for cases when you need both (such as
        calculating a percent).  The one argument case will pass the
        appropriate one based on whether `:since' or `:until' was set.

        This is useful when the reminder depends on something other than
        simple time intervals, such as the number of books you might
        read before you die.  The lambda can return an already formatted
        string, or a float which will then be formatted below.

One of `:until' or `:since' is required to determine the time
between now and the memento.

The format string understands the `format'-like substitution
sequences below.  They also support all the flags supported by
`format-spec', such as padding and truncating to a given width.

%y  The number of years truncated to an integer.
%Y  The number of years as a float.
%m  The number of months truncated to an integer.
%M  The number of months as a float.
%w  The number of weeks truncated to an integer.
%W  The number of weeks as a float.
%d  The number of days truncated to an integer.
%D  The number of days as a float.
%p  The percent passed between `:since' and `:until' truncated to an integer.
%P  The percent passed between `:since' and `:until' as a float.
%f  The result of the `:formula' truncated to an integer.
%F  The result of the `:formula' as a float.
%%  A literal percent sign.

As shown, most sequences have a upper case and a lower case version.
The lower case versions are truncated to integers since this is a common
use case.  The upper case versions return a float for full control, but
will likely require a width or precision specifier in practice.
Unfortunately, the width and precision specifiers behave like the
corresponding ones in format when applied to %s, so they define the
width in characters, not the number of decimal places.  Therefore,
\"%.5Y\" will show the number of years with two or three decimal points
of precision depending on if it's longer than 10 years or not."
  :group 'memento-mori
  :type '(repeat (cons
                  (string :tag "Format string")
                  (plist :key-type symbol
                         :options ((:until string :tag "Date")
                                   (:since string) (:formula function))
                         :value-type string)))
  :set #'memento-mori--set-value-and-update
  :initialize #'custom-initialize-default)

(define-obsolete-variable-alias 'memento-mori-age-string
  'memento-mori-string "0.2.0")

(defcustom memento-mori-quotes
  '("A culture that denies death inevitably becomes shallow and superficial,\nconcerned only with the external form of things. When death is denied, life loses its depth.\n\t–Eckhart Tolle"
    "About death: Whether it is a dispersion, or a resolution into atoms,\nor annihilation, it is either extinction or change.\n\t—Marcus Aurelius"
    "Accept death in a cheerful spirit, as nothing but the dissolution of the\nelements from which each living thing is composed. If it doesn’t hurt the individual elements to change continually into one another, why are people afraid of all of them changing and separating? It’s a natural thing. And nothing natural is evil.\n\t—Marcus Aurelius"
    "At the end of my life, with just one breath left, if you come, I’ll sit up and sing.\n\t–Rumi"
    "Before I became old I tried to live well; now that I am old, I shall try to die well;\nbut dying well means dying gladly.\n\t—Lucius Annaeus Seneca"
    "Before death takes away what you are given, give away what there is to give.\n\t–Rumi"
    "Choose to die well while you can;\nwait too long, and it might become impossible to do so.\n\t—Gaius Musonius Rufus"
    "Death is beautiful when seen to be a law, and not an accident.\nIt is as common as life.\n\t–Henry David Thoreau"
    "Death is not an evil. What is it then?\nThe one law mankind has that is free of all discrimination.\n\t—Lucius Annaeus Seneca"
    "Death is nothing, but to live defeated and inglorious is to die daily.\n\t–Napoleon"
    "Do not fear death so much but rather the inadequate life.\n\t–Bertolt Brecht"
    "Don’t behave as if you are destined to live forever. Death hangs over you.\nWhile you live, while it is in your power, be good. Now.\n\t—Marcus Aurelius"
    "Dying is a wild night and a new road.\n\t–Emily Dickinson"
    "Even death is not to be feared by one who has lived wisely.\n\t–Buddha"
    "Everyone is so afraid of death, but the real sufis just laugh:\nnothing tyrannizes their hearts.\nWhat strikes the oyster shell does not damage the pearl.\n\t–Rumi"
    "Excess of grief for the dead is madness; for it is an injury to the living,\nand the dead know it not.\n\t–Xenophon"
    "Given that all must die, it is better to die with distinction than to live long.\n\t—Gaius Musonius Rufus"
    "He who doesn’t fear death dies only once.\n\t–Giovanni Falcone"
    "Healthy children will not fear life if their elders have integrity enough\nnot to fear death.\n\t–Erik Erikson"
    "How much more suffering is caused by the thought of death than by death itself.\n\t―Will Durant"
    "I am not afraid of death, I just don’t want to be there when it happens.\n\t–Woody Allen"
    "I cannot escape death, but at least I can escape the fear of it.\n\t—Epictetus"
    "I have no choice of living or dying, you see, sir–\nbut I do have a choice of how I do it.\n\t–John Steinbeck"
    "I learned that every mortal will taste death. But only some will taste life.\n\t–Rumi"
    "I shall not die of a cold. I shall die of having lived.\n\t–Willa Cather"
    "I went to the woods because I wished to live deliberately,\nto front only the essential facts of life, and see if I could not learn\nwhat it had to teach, and not, when I came to die, discover that I had not lived.\n\t–Henry David Thoreau"
    "I've told my children that when I die, to release balloons in the sky\nto celebrate that I graduated. For me, death is a graduation.\n\t–Elizabeth Kubler Ross"
    "If a man has not discovered something that he will die for, he isn’t fit to live.\n\t–Martin Luther King, Jr."
    "If you don't know how to die, don't worry; Nature will tell you what to do\non the spot, fully and adequately. She will do this job perfectly for you;\ndon't bother your head about it.\n\t–Michel de Montaigne"
    "If you realize that all things change, there is nothing you will try to hold on to.\nIf you are not afraid of dying, there is nothing you cannot achieve.\n\t–Lao Tzu"
    "Intellectual growth should commence at birth and cease only at death.\n\t–Albert Einstein"
    "It is necessary to be strong in the face of death, because death is intrinsic to life.\nIt is for this reason that I tell my students: aim to be the person at your\nfather’s funeral that everyone, in their grief and misery, can rely on.\nThere’s a worthy and noble ambition: strength in the face of adversity.\n\t–Jordan Peterson"
    "It is not death that a man should fear, but rather he should fear never beginning to live.\n\t—Marcus Aurelius"
    "It is not length of life, but depth of life.\n\t–Ralph Waldo Emerson"
    "It is not the end of the physical body that should worry us.\nRather, our concern must be to live while we're alive - to release our\ninner selves from the spiritual death that comes with living behind a facade\ndesigned to conform to external definitions of who and what we are.\n\t–Elisabeth Kubler-Ross"
    "It seems to me that if you or I must choose between two courses of thought\nor action, we should remember our dying and try so to live that our death brings\nno pleasure on the world.\n\t–John Steinbeck"
    "It’s better to conquer grief than to deceive it.\n\t—Lucius Annaeus Seneca"
    "Let death and exile, and all other things which appear terrible be daily before\nyour eyes, but chiefly death, and you will never entertain any abject thought,\nnor too eagerly covet anything.\n\t—Epictetus"
    "Let each thing you would do, say, or intend, be like that of a dying person.\n\t—Marcus Aurelius"
    "No evil is great which is the last evil of all. Death arrives; it would be a\nthing to dread, if it could remain with you. But death must either not come at all,\nor else must come and pass away.\n\t—Lucius Annaeus Seneca"
    "No evil is honorable: but death is honorable; therefore death is not evil.\n\t—Zeno of Citium"
    "Nothing real can die.\nWhen you see a dead body, you realize that this is no longer who you knew.\nThis is only a shell. So nothing real can be threatened.\nThere is no such thing as death.\n\t–Eckhart Tolle"
    "Only those are fit to live who do not fear to die; and none are fit to die\nwho have shrunk from the joy of life and the duty of life. Both life and death\nare parts of the same Great Adventure.\n\t—Theodore Roosevelt"
    "Ordinary people seem not to realize that those who really apply themselves in\nthe right way to philosophy are directly and of their own accord preparing\nthemselves for dying and death.\n\t–Socrates"
    "Our death is our wedding with eternity.\n\t–Rumi"
    "Our fear of death is like our fear that summer will be short, but when we have\nhad our swing of pleasure, our fill of fruit, and our swelter of heat,\nwe say we have had our day.\n\t–Ralph Waldo Emerson"
    "Remembering that I'll be dead soon is the most important tool I've ever\nencountered to help me make the big choices in life. Because almost everything -\nall external expectations, all pride, all fear of embarrassment or failure -\nthese things just fall away in the face of death, leaving only what is truly important.\n\t–Steve Jobs"
    "Some people die at 25 and aren’t buried until 75.\n\t–Benjamin Franklin"
    "Some things are rushing into existence, others out of it.\nSome of what now exists is already gone.\nChange and flux constantly remake the world, just as the incessant progression\nof time remakes eternity. We find ourselves in a river.\nWhich of the things around us should we value when none of them can offer a firm foothold?\n\t—Marcus Aurelius"
    "That man lives badly who does not know how to die well.\n\t—Lucius Annaeus Seneca"
    "The act of dying is one of the acts of life.\n\t–Marcus Aurelius"
    "The art of living well and the art of dying well are one.\n\t–Epicurius"
    "The day which we fear as our last is but the birthday of eternity.\n\t–Lucius Annaeus Seneca"
    "The fear of death follows from the fear of life. A man who lives fully is\nprepared to die at any time.\n\t–Mark Twain"
    "The fear of death is the most unjustified of all fears, for there’s no risk\nof accident for someone who’s dead.\n\t–Albert Einstein"
    "The world is a playground, and death is the night.\n\t–Rumi"
    "There’s something about death that is comforting. The thought that you could\ndie tomorrow frees you to appreciate your life now.\n\t–Angelina Jolie"
    "Think of yourself as dead. You have lived your life. Now, take what’s left and\nlive it properly. What doesn’t transmit light creates its own darkness.\n\t—Marcus Aurelius"
    "To be idle is a short road to death and to be diligent is a way of life; foolish\npeople are idle, wise people are diligent.\n\t–Buddha"
    "Too busy with the crowded hour to fear to live or die.\n\t–Ralph Waldo Emerson"
    "What is death? A scary mask. Take it off – see, it doesn’t bite.\nEventually, body and soul will have to separate, just as they existed separately\nbefore we were born. So why be upset if it happens now? If it isn’t now, it’s later.\n\t—Epictetus"
    "What upsets people is not things themselves but their judgments about the things.\nFor example, death is nothing dreadful (or else it would have appeared dreadful\nto Socrates), but instead the judgment about death that it is dreadful—that is what\nis dreadful. So, when we are thwarted or upset or distressed, let us never blame\nsomeone else but rather ourselves, that is, our own judgments.\nAn uneducated person accuses others when he is doing badly; a partly educated\nperson accuses himself, an educated person accuses neither someone else nor himself.\n\t—Epictetus"
    "When a man comes to die,\nNo matter what his talents and influences and genius,\nIf he dies unloved his life must be a failure to him\nAnd his dying a cold horror.\n\t–John Steinbeck"
    "When the longest- and shortest-lived of us dies, their loss is precisely equal.\nFor the sole thing of which any of us can be deprived is the present,\nsince this is all we own, and nobody can lose what is not theirs.\n\t–Marcus Aurelius"
    "When the only thing left to do is die, then die well.\n\t—Phil Van Treuren"
    "When your time comes to die, be not like those whose hearts are filled with\nfear of death, so that when their time comes they weep and pray for a little\nmore time to live their lives over again in a different way.\nSing your death song, and die like a hero going home.\n\t–Tecumseh"
    "You act like mortals in all that you fear, and like immortals in all that you desire.\n\t—Lucius Annaeus Seneca"
    "You may leave this life at any moment: have this possibility in your mind in\nall that you do or say or think.\n\t—Marcus Aurelius"
    "Your entire life only happens in this moment. The present moment is life itself.\nYet, people live as if the opposite were true and treat the present moment as a\nstepping stone to the next moment – a means to an end.\n\t–Eckhart Tolle")
  "A list of quotes to make you think about mortality.
If `memento-mori-initial-scratch-message' contains %q, a random quote
will replace it and the result will be saved in the variable
`initial-scratch-message' for display in *scratch* buffer."
  :group 'memento-mori
  :type '(repeat string))

(defvar memento-mori-string ""
  "The memento mori memento.
The string shown in the mode line and/or frame title when
`memento-mori-mode' is enabled.  This is not meant to be changed
by the user, but can be used in other places such as `org-mode'
agendas to display the current memento.")

(defun memento-mori--set-value-and-update-display (symbol value)
  "Function to set display values, so the display will then be recalculated.
Sets SYMBOL to VALUE, then calls updates `memento-mori--add-mementos'."
  (set-default-toplevel-value symbol value)
  (memento-mori--add-mementos))

(defun memento-mori--set-value-and-update (symbol value)
  "Function to set memento config, so that a new memento will be chosen.
Sets SYMBOL to VALUE, then calls updates `memento-mori--update'."
  (set-default-toplevel-value symbol value)
  (memento-mori--update))

(defvar memento-mori--modeline-info
  `(memento-mori-mode
    ((:propertize
      ("" memento-mori-string)
      mouse-face mode-line-highlight
      help-echo "mouse-1: Refresh memento\nmouse-2/3: Turn off memento-mori-mode"
      local-map
      ,(make-mode-line-mouse-map
        'mouse-1 (lambda () (interactive) (memento-mori--update))))
     " "))
  "A mode line construct to be added to `global-mode-string'.
See `mode-line-format' for information about the format.  It should
append a space to the `memento-mori-string' which is considered best
practice for inclusion in `global-mode-string'.")

(defvar memento-mori--frame-title-info
  `(memento-mori-mode
    (" -- " memento-mori-string))
  "A frame title construct to be added to `frame-title-format'.
See `frame-title-format' and `mode-line-format' for information
about the format.")

(defun memento-mori--assert-birth-date ()
  "Ensure that `memento-mori-birth-date' has been set."
  (when (or (null memento-mori-birth-date)
            (equal "" memento-mori-birth-date))
    (error "Birth date not set.  Try M-x customize-group memento-mori")))

(defun memento-mori--parse-time (value)
  "Return time in `encode-time' format.
The time is parsed from VALUE using `parse-time-string'.
An error is signaled if it is not valid."
  (when (symbolp value)
    (setq value (symbol-value value)))
  (let* ((decoded (parse-time-string
                   (if (stringp value)
                       value
                     "")))
         (day (elt decoded 3))
         (month (elt decoded 4))
         (year (elt decoded 5)))
    (unless (and day month year)
      (error "Cannot parse date %S" value))
    (encode-time 0 0 0 day month year)))

(defun memento-mori--age ()
  "Return your age in years.
This is a floating point number based on `memento-mori-birth-date'."
  (memento-mori--assert-birth-date)
  (/ (truncate (float-time
                (time-subtract (current-time)
                               (memento-mori--parse-time
                                memento-mori-birth-date))))
     (* 60 60 24 365.2425)))

(defun memento-mori--random-memento ()
  "Randomly choose a memento from `memento-mori-mementos'."
  (nth (random (length memento-mori-mementos)) memento-mori-mementos))

(defun memento-mori--random-quote ()
  "Randomly choose a quote from `memento-mori-quotes'."
  (nth (random (length memento-mori-quotes)) memento-mori-quotes))

(defun memento-mori--format-memento (memento)
  "Format MEMENTO based on the current time."
  (let* ((format-string (car memento))
         (args-plist (cdr memento))
         (until-value (plist-get args-plist :until))
         (since-value (plist-get args-plist :since))
         (_ (when (not (or until-value since-value))
                 (error ":since or :until required.  Parsing %s" memento)))
         (until-seconds (when until-value
                          (float-time
                           (time-subtract
                            (memento-mori--parse-time until-value)
                            (current-time)))))
         (since-seconds (when since-value
                          (float-time
                           (time-subtract
                            (current-time)
                            (memento-mori--parse-time since-value)))))
         (seconds (if until-value until-seconds since-seconds))
         (percent (if (and until-value since-value)
                      (* 100 (/ since-seconds (+ since-seconds until-seconds)))
                    0))
         (days (/ seconds (* 60 60 24)))
         (weeks (/ days 7))
         (months (/ days 30))
         (years (/ days 365.2425))
         (formula (plist-get args-plist :formula))
         (formula-arity (when (functionp formula)
                          (length (help-function-arglist formula))))
         (formula-value (when (functionp formula)
                          (cond
                           ((eq formula-arity 1)
                            (funcall formula days))
                           ((eq formula-arity 2)
                            (funcall formula
                                     (/ since-seconds (* 60 60 24))
                                     days))
                           (t 0)))))
    (format-spec format-string
                 `((?Y . ,years)
                   (?y . ,(truncate years))
                   (?M . ,months)
                   (?m . ,(truncate months))
                   (?W . ,weeks)
                   (?w . ,(truncate weeks))
                   (?D . ,days)
                   (?d . ,(truncate days))
                   (?F . ,formula-value)
                   (?f . ,(if (numberp formula-value)
                              (truncate formula-value)
                            formula-value))
                   (?P . ,percent)
                   (?p . ,(truncate percent)))
                 'ignore)))

;; Forward declare initial-scratch-message which should be defined at run time
(defvar initial-scratch-message)

(defun memento-mori--update ()
  "Update `memento-mori-string' based on the current time."
  (let ((has-set-new (not (eq memento-mori-mementos
                              (eval (car (get 'memento-mori-mementos
                                              'standard-value))))))
        (has-set-old (not (eq memento-mori-birth-date
                              (eval (car (get 'memento-mori-birth-date
                                              'standard-value)))))))
    (when (not has-set-new)
      (message "You'll get more out of memento-mori mode if you customize it.
Try M-x customize-group memento-mori RET"))
    (setq memento-mori-string
          (if (and has-set-old (not has-set-new))
              ;; Fall back to the old style for backwards compatibility
              (format " %.2f years old" (memento-mori--age))
            (memento-mori--format-memento (memento-mori--random-memento)))))
  (when (and memento-mori-initial-scratch-message
             (boundp 'initial-scratch-message))
    (setq initial-scratch-message
          (format-spec memento-mori-initial-scratch-message
                       `((?m . ,memento-mori-string)
                         (?q . ,(memento-mori--random-quote)))
                       'ignore))))

(defun memento-mori--add-mementos ()
  "Add constructs to mode line and/or frame title to display mementos.
Where it adds it is controlled by `memento-mori-display-in-modeline' and
`memento-mori-display-in-frame-title' respectively.

Alternatively, you might consider adding it (manually) to your
org mode agenda, splash screen, etc."
  (if memento-mori-display-in-modeline
      ;; This assumes that global-mode-string is a list, even though technically
      ;; it could be a string
      (add-to-list 'global-mode-string memento-mori--modeline-info)
    (setq global-mode-string
          (and global-mode-string
               (delete memento-mori--modeline-info global-mode-string))))
  (if memento-mori-display-in-frame-title
      (setq frame-title-format
            (list frame-title-format 'memento-mori--frame-title-info))
    (when (listp frame-title-format)
      (setq frame-title-format
            (delete 'memento-mori--frame-title-info frame-title-format)))))

;;;###autoload
(define-minor-mode memento-mori-mode
  "Toggle display of a memento mori memento in the mode line and/or frame title.
With a prefix argument ARG, enable if ARG is positive, and
disable it otherwise.  If called from Lisp, enable it if ARG is
omitted or nil."
  :global t
  :group 'memento-mori
  (cancel-function-timers #'memento-mori--update)
  (when memento-mori-mode
    (memento-mori--update)
    (run-at-time "00:00" (* 60 60 24) #'memento-mori--update)
    (memento-mori--add-mementos)))

(provide 'memento-mori)

;;; memento-mori.el ends here

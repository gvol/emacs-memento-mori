;;; memento-mori.el --- Reminder of our mortality  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023  Lassi Kortela

;; Author: Lassi Kortela <lassi@lassi.io>
;; Maintainer: Ivan Andrus <darthandrus@gmail.com>
;; URL: https://github.com/gvol/emacs-memento-mori
;; Version: 0.2.1
;; Package-Requires: ((emacs "24.3"))
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
;;       '(("%.2f years old" :since "1970-01-01")))
;; (memento-mori-mode)

;; Or using use-package:
;; (use-package memento-mori
;;   :ensure t
;;   :custom (memento-mori-mementos
;;            '(("%.2f years old" :since "1970-01-01")))
;;   :config (memento-mori-mode))

;;; Code:

(defgroup memento-mori nil
  "Reminder of our mortality."
  :group 'help)

(defcustom memento-mori-display-in-modeline t
  "If non-nil, `memento-mode' will add mementos to the modeline.
Really, it adds it to the `global-mode-string', which usually
appears in the modeline, however, if you have customized your
modeline it may not appear."
  :group 'memento-mori
  :type 'boolean)

(defcustom memento-mori-display-in-frame-title nil
  "If non-nil, `memento-mode' will add mementos to the frame title."
  :group 'memento-mori
  :type 'boolean)

(defcustom memento-mori-birth-date ""
  "Your birth date in YYYY-MM-DD format.
This is deprecated in favor of the more flexible `memento-mori-mementos'.
To use `memento-mori-mementos' customize it, or erase customization of
`memento-mori-birth-date'."
  :type 'string
  :group 'memento-mori)
(make-obsolete-variable 'memento-mori-birth-date
                        "memento-mori-mementos" "0.2.1")

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
  "List of mementos, namely things that will remind you of your short time.

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

As shown, most sequences have a upper case and a lower case
version.  The lower case versions are truncated to integers since
this is a common use case.  The upper case versions return a
float for full control, but will likely require a width or
precision specifier in practice, such as \"%.2Y\" to show the
number of years with two decimal points of precision."
  :group 'memento-mori
  :type '(repeat (cons
                  (string :tag "Format string")
                  (plist :key-type symbol
                         :options ((:until string :tag "Date")
                                   (:since string) (:formula function))
                         :value-type string))))

(define-obsolete-variable-alias 'memento-mori-age-string
  'memento-mori-string "0.2.0")

(defvar memento-mori-string ""
  "The string shown in the mode line when `memento-mori-mode' is enabled.")

(defvar memento-mori--modeline-construct
  `(memento-mori-mode
    ((:propertize
      ("" memento-mori-string)
      mouse-face mode-line-highlight
      help-echo "mouse-1: Refresh memento\nmouse-2/3: Turn off memento-mori"
      local-map
      ,(make-mode-line-mouse-map
        'mouse-1 #'memento-mori--update))
     " "))
  "A mode-line construct to be added to `global-mode-string'.
See `mode-line-format' for information about the format.  It should
append a space to the `memento-mori-string' which is considered best
practice for inclusion in `global-mode-string'.")

(defvar memento-mori--frame-title-construct
  `(memento-mori-mode
    (" -- " memento-mori-string))
  "A mode-line construct to be added to `global-mode-string'.
See `mode-line-format' for information about the format.  It should
append a space to the `memento-mori-string'.  This is considered best
practice for inclusion in `global-mode-string'.")

(defun memento-mori--assert-birth-date ()
  "Ensure that `memento-mori-birth-date' has been set."
  (when (or (null memento-mori-birth-date)
            (equal "" memento-mori-birth-date))
    (error "Birth date not set.  Try M-x customize-group memento-mori")))

(defun memento-mori--parse-time (value)
  "Return your birth time in `encode-time' format.
The birth time is parsed from `memento-mori-birth-date' using
`parse-time-string'.  An error is signaled if it is not valid."
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

(defun memento--random-memento ()
  "Randomly choose a memento from `memento-mori-mementos'."
  (nth (random (length memento-mori-mementos)) memento-mori-mementos))

(defun memento-mori--format-memento (memento)
  "Format MEMENTO based on the current time."
  (let* ((format-string (car memento))
         (args-plist (cdr memento))
         (until-value (plist-get args-plist :until))
         (since-value (plist-get args-plist :since))
         (_ (when (not (or until-value since-value))
                 (error ":since or :until required.  Parsing %s" memento)))
         (current-time-list nil)
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

(defun memento-mori--update ()
  "Update `memento-mori-string' based on the current time."
  (let ((has-set-new (not (eq memento-mori-mementos
                              (eval (car (get 'memento-mori-mementos
                                              'standard-value))))))
        (has-set-old (not (eq memento-mori-birth-date
                              (eval (car (get 'memento-mori-birth-date
                                              'standard-value)))))))
    (when (not has-set-new)
      (message "You'll get more out of memento-mori-mode if you customize it.
Try M-x customize-group memento-mori RET"))
    (setq memento-mori-string
          (if (and has-set-old (not has-set-new))
              ;; Fall back to the old style so that we don't break anyone
              (format " %.2f years old" (memento-mori--age))
            (memento-mori--format-memento (memento--random-memento))))))

(defun memento-mori--add-to-modeline ()
  "Adds constructs to modeline and frame-title to display mementos.
Where it adds it is controlled by `memento-mori-display-in-modeline' and
`memento-mori-display-in-frame-title'.

You might consider adding it (manually) to your org mode agenda, splash
screen, etc."
  (if memento-mori-display-in-modeline
      ;; This assumes that global-mode-string is a list, even though technically
      ;; it could be a string
      (add-to-list 'global-mode-string memento-mori--modeline-construct)
    (setq global-mode-string
          (and global-mode-string
               (delete memento-mori--modeline-construct global-mode-string))))
  (if memento-mori-display-in-frame-title
      (if (stringp frame-title-format)
          (setq frame-title-format
                (list frame-title-format memento-mori--frame-title-construct))
        (add-to-list 'frame-title-format
                     'memento-mori--frame-title-construct))
    (when (listp frame-title-format)
      (setq frame-title-format
            (delete memento-mori--frame-title-construct frame-title-format)))))

;;;###autoload
(define-minor-mode memento-mori-mode
  "Toggle display of your age in the mode line.
With a prefix argument ARG, enable if ARG is positive, and
disable it otherwise.  If called from Lisp, enable it if ARG is
omitted or nil."
  :global t
  :group 'memento-mori
  (cancel-function-timers #'memento-mori--update)
  (when memento-mori-mode
    (memento-mori--update)
    (run-at-time "00:00" (* 60 60 24) #'memento-mori--update)
    (memento-mori--add-to-modeline)))

(provide 'memento-mori)

;;; memento-mori.el ends here

;;; memento-mori.el --- Reminder of our mortality  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023  Lassi Kortela

;; Author: Lassi Kortela <lassi@lassi.io>
;; Maintainer: Ivan Andrus <darthandrus@gmail.com>
;; URL: https://github.com/gvol/emacs-memento-mori
;; Version: 0.2.0
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

;; Add something like the following adjusting the date accordingly to
;; your init.el.

;; (require 'memento-mori)
;; (setq memento-mori-birth-date "1970-01-01")
;; (memento-mori-mode)

;; Or using use-package:
;; (use-package memento-mori
;;   :ensure t
;;   :init (setq memento-mori-birth-date "1970-01-01")
;;   :config (memento-mori-mode))

;;; Code:

(defgroup memento-mori nil
  "Reminder of our mortality."
  :group 'help)

(defcustom memento-mori-birth-date ""
  "Your birth date in YYYY-MM-DD format."
  :type 'string
  :group 'memento-mori)


(define-obsolete-variable-alias 'memento-mori-age-string
  'memento-mori-string "0.2.0")

(defvar memento-mori-string ""
  "The string shown in the mode line when `memento-mori-mode' is enabled.")

(defun memento-mori--assert-birth-date ()
  "Ensure that `memento-mori-birth-date' has been set."
  (when (or (null memento-mori-birth-date)
            (equal "" memento-mori-birth-date))
    (error "Birth date not set.  Try M-x customize-group memento-mori")))

(defun memento-mori--parse-time (value)
  "Return your birth time in `encode-time' format.
The birth time is parsed from `memento-mori-birth-date' using
`parse-time-string'.  An error is signaled if it is not valid."
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

(defun memento-mori--update ()
  "Update `memento-mori-string' based on the current time."
  (setq memento-mori-string
        (format " %.2f years old" (memento-mori--age))))

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
    (run-at-time "00:00" (* 60 60 24) #'memento-mori--update))
  (setq global-mode-string
        (append (delete 'memento-mori-string
                        (or global-mode-string '("")))
                (when memento-mori-mode '(memento-mori-string)))))

(provide 'memento-mori)

;;; memento-mori.el ends here

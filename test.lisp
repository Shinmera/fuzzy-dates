(defpackage #:org.shirakumo.fuzzy-dates.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:dates #:org.shirakumo.fuzzy-dates))
  (:export
   #:fuzzy-dates))

(in-package #:org.shirakumo.fuzzy-dates.test)

(define-test fuzzy-dates)

(define-test decoders
  :parent fuzzy-dates)

(define-test weekday
  :parent decoders
  (is = 0 (dates:decode-weekday "mo"))
  (is = 1 (dates:decode-weekday "tue"))
  (is = 2 (dates:decode-weekday "wed"))
  (is = 3 (dates:decode-weekday "thursday"))
  (is = 4 (dates:decode-weekday "fr"))
  (is = 5 (dates:decode-weekday "sat"))
  (is = 6 (dates:decode-weekday "sunday")))

(define-test month
  :parent decoders
  (is = 1 (dates:decode-month "ja"))
  (is = 2 (dates:decode-month "feb"))
  (is = 3 (dates:decode-month "march"))
  (is = 4 (dates:decode-month "ap"))
  (is = 5 (dates:decode-month "may"))
  (is = 6 (dates:decode-month "june"))
  (is = 7 (dates:decode-month "jy"))
  (is = 8 (dates:decode-month "aug"))
  (is = 9 (dates:decode-month "september"))
  (is = 10 (dates:decode-month "oct"))
  (is = 11 (dates:decode-month "no"))
  (is = 12 (dates:decode-month "december")))

(define-test unit
  :parent decoders
  (is = 1/1000 (dates:decode-unit "ms"))
  (is = 1 (dates:decode-unit "s"))
  (is = 1/10 (dates:decode-unit "decisecond"))
  (is = 60 (dates:decode-unit "minute"))
  (is = (* 60 60) (dates:decode-unit "h"))
  (is = 31536000000000000 (dates:decode-unit "aeons")))

(define-test timezone
  :parent decoders
  (is = 0 (dates:decode-timezone "z"))
  (is = 0 (dates:decode-timezone "gmt"))
  (is = 0 (dates:decode-timezone "utc"))
  (is = 0 (dates:decode-timezone ""))
  (is = 9 (dates:decode-timezone "jst")))

(define-test integer
  :parent decoders
  (is = 0 (dates:decode-integer "0"))
  (is = 100 (dates:decode-integer "100"))
  (is = 0 (dates:decode-integer "zero"))
  (is = 40 (dates:decode-integer "fourty"))
  (is = 40 (dates:decode-integer "forty"))
  (is = 1000 (dates:decode-integer "thousand"))
  (is = 11 (dates:decode-integer "eleven"))
  (is = 24 (dates:decode-integer "twenty-four"))
  (is = 20001 (dates:decode-integer "twenty thousand one"))
  (is = 21345 (dates:decode-integer "twenty one thousand three hundred and forty-five")))

(define-test parsers
  :parent fuzzy-dates
  :depends-on (decoders))

(define-test forward-time
  :parent parsers)

(define-test backward-time
  :parent parsers)

(define-test rfc3339
  :parent parsers)

(define-test iso8661
  :parent parsers)

(define-test reverse
  :parent parsers)

(define-test rfc1123
  :parent parsers)

(define-test single
  :parent parsers)

(define-test parse
  :parent fuzzy-dates
  :depends-on (parsers))
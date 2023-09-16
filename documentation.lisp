(in-package #:org.shirakumo.fuzzy-dates)

(docs:define-docs
  (function decode-weekday
    "Turn a weekday name into an integer between 0 and 6.

If ERRORP is true, failing to parse the month name will signal an
error. Otherwise NIL is returned.

See http://www.lispworks.com/documentation/HyperSpec/Body/25_ada.htm

Examples:

  mo
  tue
  thursday")
  
  (function decode-month
    "Turn a month name into an integer between 1 and 12.

If ERRORP is true, failing to parse the month name will signal an
error. Otherwise NIL is returned.

See http://www.lispworks.com/documentation/HyperSpec/Body/25_ada.htm

Examples:

  ja
  feb
  march")
  
  (function decode-unit
    "Turn a unit name into a scaling factor relative to seconds.

If ERRORP is true, failing to parse the month name will signal an
error. Otherwise NIL is returned.

The supported time units go from nanoseconds to aeons and includes all
sorts of abbreviations for them.

See http://www.lispworks.com/documentation/HyperSpec/Body/25_ada.htm

Examples:

  ms
  sec
  y
  aeons")
  
  (function decode-timezone
    "Turn a timezone name into a number of hours of UTC offset.

If ERRORP is true, failing to parse the month name will signal an
error. Otherwise NIL is returned.

See http://www.lispworks.com/documentation/HyperSpec/Body/25_ada.htm

Examples:

  Z
  GMT
  JST")

  (function decode-integer
    "Parse an integer in digit or word format.

If ERRORP is true, failing to parse the month name will signal an
error. Otherwise NIL is returned.

The understood words go from one all the way up to trillion, though no
abbreviations are supported.

Examples:

  100
  one hundred
  twenty-two million three hundred two")
  
  (function parse-forward-time
    "Parse a relative time for the future.

The basic syntax is:

  STAMP ::= in (C U)(,? C U)*
  C     --- a positive integer
  U     --- a unit name

Examples:

  in 10 seconds
  in 5 minutes, 10 years
  in five hours

See DECODE-UNIT
See DECODE-INTEGER
See PARSE")
  
  (function parse-backward-time
    "Parse a relative time for the past.

The basic syntax is:

  STAMP ::= (C U)(,? C U)* ago
  C     --- a positive integer
  U     --- a unit name

Examples:
  10 seconds ago
  6 years, 5 minutes ago
  thirty hours ago

See DECODE-UNIT
See DECODE-INTEGER
See PARSE")
  
  (function parse-rfc3339-like
    "Parse a timestamp that looks vaguely like an RFC3339 date.

A typical RFC3339 string has the following format:

  2023-09-16T10:06:15.00-05:00

This function is more lenient, and makes the following fuzzy matches:
It permits the following separators between date parts:

  space comma period slash dash

It permits the following separators between date and time parts:

  space dash t

It permits the following separators between time parts:

  space period dash colon

It permits date and time parts to not be padded.

It also permits omitting the date and timezone parts of the
timestamp.

See PARSE")
  
  (function parse-iso8661-like
    "Parse a timestamp that looks vaguely like an ISO8661 date.

This is very similar to the RFC3339 format, but instead follows the
compact format without separators between date and time formats.

A typical compact ISO8661 string has the following format:

  20230916T100615Z

This function is slightly more lenient and permits the following
separators between date and time parts:

  space dash t

It permits date and time parts to not be padded.

It also permits omitting the date and timezone parts of the
timestamp.

See PARSE")
  
  (function parse-reverse-like
    "Parse a timestamp that is \"reverse\" from the others

A typical reverse time string has the following format:

  10:18:03 16.9.2023

This function is more lenient, and makes the following fuzzy matches:
It permits the following separators between date parts:

  space comma period slash dash

It permits the following separators between date and time parts:

  space dash t

It permits the following separators between time parts:

  space period dash colon

It permits date and time parts to not be padded.

It also permits omitting the time and timezone parts of the
timestamp.

See PARSE")
  
  (function parse-rfc1123-like
    "Parse a timestamp that looks vaguely like an RFC1123 date.

A typical RFC1123 string has the following format:

  Thu, 23 Jul 2013 19:42:23 GMT

This function is more lenient, and makes the following fuzzy matches:
It permits the following separators between date parts:

  space comma period slash dash

It permits the following separators between date and time parts:

  space comma period slash dash t

It permits the following separators between time parts:

  space period dash colon

It permits date and time parts to not be padded.

It also permits omitting the day of week, time, and timezone parts of
the timestamp.

See PARSE")
  
  (function parse-single
    "Parse a single token.

If it's an integer, it can denote either:

  seconds in the future, if below 1000
  a year, if below 5000
  a UNIX timestamp

If it's a word, it can denote either:

  a day of the week, pushed to the next week if the current day is
  already past.
  a month, pushed to the next year if the current month is already
  past.

Examples:

  10
  1900
  mon
  march

See PARSE")
  
  (function parse
    "Fuzzily parse a time string into a universal-time timestamp.


If ERRORP is true, failing to parse the string name will signal an
error. Otherwise NIL is returned. When an error is signalled, two
restarts will be active:

  USE-VALUE -- interactive, allows supplying a universal-time to use
  CONTINUE  -- simply returns the current universal-time timestamp

This also applies to all the sub-functions used.

See PARSE-FORWARD-TIME
See PARSE-BACKWARD-TIME
See PARSE-RFC3339-LIKE
See PARSE-ISO8661-LIKE
See PARSE-REVERSE-LIKE
See PARSE-RFC1123-LIKE
See PARSE-SINGLE"))

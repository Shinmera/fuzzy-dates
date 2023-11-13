(in-package #:org.shirakumo.fuzzy-dates)

(defun parse-integer* (thing)
  (when (and thing (string/= "" thing))
    (parse-integer thing)))

(defmacro with-integers-bound (vars (regex string) &body body)
  `(cl-ppcre:register-groups-bind ,vars (,(format NIL regex) ,string)
     (let ,(loop for var in vars
                 unless (char= #\_ (char (string var) 0))
                 collect `(,var (parse-integer* ,var)))
       ,@body)))

(defmacro with-scans (var &body cases)
  (let ((varg (gensym "VAR")))
    `(let ((,varg ,var))
       (cond ,@(loop for (regex . body) in cases
                     collect (if (member regex '(T :otherwise otherwise))
                                 `(T ,@body)
                                 `((cl-ppcre:scan ,(format NIL "^~a$" regex) ,varg) ,@body)))))))

(defun check-error (errorp string &optional (default (get-universal-time)))
  (when errorp
    (restart-case
        (error "Unknown date string: ~a" string)
      (use-value (v)
        :report "Supply a universal time"
        :interactive (lambda () 
                       (format *query-io* "~&Enter a universal time: ")
                       (parse-integer (read-line *query-io*)))
        :test integerp
        v)
      (continue ()
        :report "Return the current time"
        default))))

(defun backfill-timestamp (y o d h m s tz &optional (now (get-universal-time)))
  (multiple-value-bind (ls lm lh ld lo ly) (apply #'decode-universal-time now (if tz (list tz)))
    (let ((time (apply #'encode-universal-time (or s ls) (or m lm) (or h lh) (or d ld) (or o lo) (or y ly) (if tz (list tz)))))
      (when (and y (< 100 y))
        (incf y 1900))
      time)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-tzdb (db)
    (with-open-file (stream db)
      (let ((db (make-hash-table :test 'equalp)))
        (loop for line = (read-line stream NIL)
              while line
              unless (and (<= (length line) 1) (char= (char line 0) #\#))
              collect (with-integers-bound (_z _+ h m) ("(\\w+) *([+-])?(\\d+)(?::(\\d+))?" line)
                        (setf (gethash (string-downcase _z) db)
                              (- (* (if (string= _+ "-") -1 +1) (+ h (/ (or m 0) 60)))))))
        db))))

(defparameter *tzdb* (parse-tzdb #.(make-pathname :name "tz" :type "txt" :defaults (or *compile-file-pathname* *load-pathname*))))

(defun decode-weekday (w &optional errorp)
  (with-scans w
    ("(mo|mon|monday)" 0)
    ("(tu|tue|tuesday)" 1)
    ("(we|wed|wednesday)" 2)
    ("(th|thu|thursday)" 3)
    ("(fr|fri|friday)" 4)
    ("(sa|sat|saturday)" 5)
    ("(su|sun|sunday)" 6)
    (T (when errorp (error "Unknown weekday: ~a" w)))))

(defun encode-weekday (w &key length)
  (let ((name (ecase w
                (0 "monday")
                (1 "tuesday")
                (2 "wednesday")
                (3 "thursday")
                (4 "friday")
                (5 "saturday")
                (6 "sunday"))))
    (if length
        (subseq name 0 length)
        name)))

(defun decode-month (m &optional errorp)
  (with-scans m
    ("(ja|jan|january)" 1)
    ("(fe|feb|february)" 2)
    ("(ma|mar|march)" 3)
    ("(ap|apr|april)" 4)
    ("(my|may)" 5)
    ("(ju|jun|june)" 6) 
    ("(jy|jul|july)" 7)
    ("(au|aug|august)" 8)
    ("(se|sep|september)" 9)
    ("(oc|oct|october)" 10)
    ("(no|nov|november)" 11)
    ("(de|dec|december)" 12)
    (T (when errorp (error "Unknown month: ~a" m)))))

(defun encode-month (m &key length)
  (let ((name (ecase m
                (1 "january")
                (2 "february")
                (3 "march")
                (4 "april")
                (5 "may")
                (6 "june")
                (7 "july")
                (8 "august")
                (9 "september")
                (10 "october")
                (11 "november")
                (12 "december"))))
    (if length
        (subseq name 0 length)
        name)))

(defun decode-unit (u &optional errorp)
  (with-scans u
    ("(ns|nano|nanoseconds?)" 1/1000000000)
    ("(us|micro|microseconds?)" 1/1000000)
    ("(ms|milli|milliseconds?)" 1/1000)
    ("(cs|centi|centiseconds?)" 1/100)
    ("(ds|deci|deciseconds?)" 1/10)
    ("(s|sec|secs|seconds?)?" 1)
    ("(m|min|minutes?)" 60)
    ("(h|hours?)" 3600)
    ("(d|days?)" 86400)
    ("(w|weeks?)" 604800)
    ("(o|mo|months?)" 2592000)
    ("(y|years?)" 31536000)
    ("(dc|dec|decades?)" 315532800)
    ("(c|cen|century|centuries)" 3155695200)
    ("(a|aeons?)" 31536000000000000)
    (T (when errorp (error "Unknown time unit: ~a" u)))))

(defun decode-integer (i &optional errorp)
  (with-scans i
    (".*[ -].*"
     (let ((total 0) (accum 0) (prev most-positive-fixnum))
       (loop for part in (cl-ppcre:split "[ -]+(and[ -]+)?" i)
             for int = (or (decode-integer part errorp)
                           (return-from decode-integer NIL))
             do (if (< prev int)
                    (progn (incf total (* int accum))
                           (setf accum 0))
                    (incf accum int))
                (setf prev int))
       (+ total accum)))
    ("\\d+" (parse-integer i))
    ("zero|nil|none" 0)
    ("one" 1)
    ("two" 2)
    ("three" 3)
    ("four" 4)
    ("five" 5)
    ("six" 6)
    ("seven" 7)
    ("eight" 8)
    ("nine" 9)
    ("ten" 10)
    ("eleven" 11)
    ("twelve" 12)
    ("thirteen" 13)
    ("fourteen" 14)
    ("fifteen" 15)
    ("sixteen" 16)
    ("seventeen" 17)
    ("eighteen" 18)
    ("nineteen" 19)
    ("twenty" 20)
    ("thirty" 30)
    ("fou?rty" 40)
    ("fifty" 50)
    ("sixty" 60)
    ("seventy" 70)
    ("eighty" 80)
    ("ninety" 90)
    ("hundred" 100)
    ("thousand" 1000)
    ("million" 1000000)
    ("billion" 1000000000)
    ("trillion" 1000000000000)
    (T (when errorp (error "Unknown integer: ~a" i)))))

(defun decode-timezone (tz &optional errorp)
  (or (when (or (null tz) (string= "z" tz) (string= "" tz)) 0)
      (gethash tz *tzdb*)
      (when errorp (error "Unknown time zone: ~a" tz))))

(defun encode-timezone (tz &optional stream)
  (cond ((stringp tz) (write-string tz stream))
        ((= 0 tz) (format stream "Z"))
        (T (format stream "+~2,'0d~[~:;~:*:~2,'0d~]" (truncate tz) (mod (* tz 60) 60)))))

(defmacro define-parser (name (strvar &optional nowvar (errorp (gensym "ERRORP"))) &body body)
  `(defun ,name (string &key ,@(when nowvar '(now)) errorp)
     (let ((,strvar (string-downcase string))
           (,errorp errorp)
           ,@(when nowvar `((,nowvar (or now (get-universal-time))))))
       (or ,@body
           (check-error ,errorp ,strvar ,(or nowvar '(get-universal-time)))))))

(define-parser parse-timezone (string)
  ;; JST+5:00
  (with-integers-bound (_z _+ oh om) ("([A-Za-z]+)? *([+\\-])(\\d{1,2})(?::*(\\d+))?$" string)
    (+ (decode-timezone _z)
       (* (if (string= "-" _+) -1 +1)
          (+ (* (or oh 0) (/ (or om 0) 60)))))))

(defun parse-relative-time (string errorp)
  (let ((sum 0)
        (accum ()))
    ;; We loop until we find a unit and then use all preceding tokens as an integer
    (dolist (part (cl-ppcre:split "[ ,]+(and[ ,]+)?" string))
      (let ((u (decode-unit part)))
        (cond (u
               (incf sum (* u
                            (or (decode-integer (format NIL "~{~a~^ ~}" (nreverse accum)) errorp)
                                (return NIL))))
               (setf accum ()))
              (T
               (push part accum)))))
    ;; If we have non-unit tokens leftover, we fail to parse.
    (if accum
        (check-error errorp string)
        sum)))

(define-parser parse-forward-time (string now errorp)
  ;; in 5m, 9s
  (cl-ppcre:register-groups-bind (parts) ("^in *(.*)$" string)
    (let ((offset (parse-relative-time parts errorp)))
      (when offset (+ now offset)))))

(define-parser parse-backward-time (string now errorp)
  ;; 10 seconds ago
  (cl-ppcre:register-groups-bind (parts) ("^(.*) *ago$" string)
    (let ((offset (parse-relative-time parts errorp)))
      (when offset (- now offset)))))

(define-parser parse-rfc3339-like (string now)
  ;; 2023.09.15T20:35:42Z
  (with-integers-bound (y o d h m s) ("^(?:(\\d+)[ ,./\\-](\\d+)(?:[ ,./\\-](\\d+))?[t\\- ]+)?~
                                            (\\d+)[ .:\\-]+(\\d+)(?:[ .:\\-]+(\\d+))?(?:\\.+\\d*)?" string)
    (backfill-timestamp y o d h m s (parse-timezone string) now)))

(define-parser parse-iso8661-like (string now)
  ;; 20230915T203542Z
  (with-integers-bound (y o d h m s) ("^(?:(\\d{4})?(\\d{2})(\\d{2})[t\\- ]+)?~
                                        (\\d{2})(\\d{2})(\\d{2})?" string)
    (backfill-timestamp y o d h m s (parse-timezone string) now)))

(define-parser parse-reverse-like (string now)
  ;; 22:48:34 15.9.2023 GMT
  (with-integers-bound (h m s d o y) ("^(?:(\\d+)[ .:\\-](\\d+)(?:[ .:\\-](\\d+))?[t\\- ]+)?~
                                        (\\d+)[ ,./\\-](\\d+)(?:[ ,./\\-](\\d+))?" string)
    (backfill-timestamp y o d h m s (parse-timezone string) now)))

(define-parser parse-rfc1123-like (string now)
  ;; Thu, 23 Jul 2013 19:42:23 GMT
  (with-integers-bound (_dow d _o y h m s) ("^([A-Za-z]+)[ ,./\\-]*(\\d+)[ ,./\\-]*([A-Za-z]+)[ ,./\\-]*(\\d+)~
                                              (?:[t ,./\\-]*(\\d+)[ .:\\-]+(\\d+)(?:[ .:\\-]+(\\d+))?(?:\\.+\\d*)?)?" string)
    (backfill-timestamp y (decode-month _o T) d h m s (parse-timezone string) now))
  (with-integers-bound (d _o y h m s) ("^(\\d+)[ ,./\\-]*([A-Za-z]+)[ ,./\\-]*(\\d+)~
                                         (?:[t ,./\\-]*(\\d+)[ .:\\-]+(\\d+)(?:[ .:\\-]+(\\d+))?(?:\\.+\\d*)?)?" string)
    (backfill-timestamp y (decode-month _o T) d h m s (parse-timezone string) now)))

(define-parser parse-single (string now)
  (with-scans string
    ("\\d+"
     (let ((stamp (parse-integer string)))
       (cond ((< stamp 1000) ;; Seconds in the future
              (+ stamp now))
             ((< stamp 5000) ;; A year relative to current time
              (multiple-value-bind (ls lm lh ld lo) (decode-universal-time now)
                (encode-universal-time ls lm lh ld lo stamp)))
             (T ;; A UNIX timestamp
              (+ stamp (encode-universal-time 0 0 0 1 1 1970 NIL))))))
    ("(just +)?now"
     now)
    ("[A-Za-z]+"
     (let ((w (decode-weekday string))
           (o (decode-month string)))
       (multiple-value-bind (ls lm lh ld lo ly lw) (decode-universal-time now)
         (cond (w ;; A day of the week relative to current time, always in the future
                (+ now (* (decode-unit "d") (if (< lw w) (- w lw) (- 7 (- lw w))))))
               (o ;; A month relative to current time, always in the future
                (encode-universal-time ls lm lh ld o (if (< lo o) ly (1+ ly))))))))))

(defun parse (string &key now errorp)
  (let ((now (or now (get-universal-time))))
    (or (parse-forward-time string :now now)
        (parse-backward-time string :now now)
        (parse-rfc3339-like string :now now)
        (parse-iso8661-like string :now now)
        (parse-reverse-like string :now now)
        (parse-rfc1123-like string :now now)
        (parse-single string :now now)
        (check-error errorp string now))))

(defmacro define-printer (name (stream &optional ss mm hh d m y dow dsp tz) &body body)
  (let ((binds (loop for var in (list ss mm hh d m y dow dsp tz)
                     collect (or var (gensym)))))
    `(defun ,name (stamp &optional stream time-zone)
       (flet ((thunk (,stream)
                (multiple-value-bind ,binds (decode-universal-time stamp (etypecase time-zone
                                                                           (null NIL)
                                                                           (string (gethash time-zone *tzdb*))
                                                                           (real time-zone)))
                  (declare (ignore ,@(loop for symb in binds
                                           unless (symbol-package symb)
                                           collect symb)))
                  ,@body)))
         (etypecase stream
           (null
            (with-output-to-string (stream)
              (thunk stream)))
           ((eql T)
            (thunk *standard-output*))
           (stream
            (thunk stream)))))))

(define-printer print-rfc3339 (stream ss mm hh d m y)
  (format stream "~4,'0d.~2,'0d.~2,'0dT~2,'0d:~2,'0d:~2,'0d"
          y m d hh mm ss)
  (encode-timezone time-zone stream))

(define-printer print-iso8661 (stream ss mm hh d m y)
  (format stream "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d"
          y m d hh mm ss)
  (encode-timezone time-zone stream))

(define-printer print-reverse (stream ss mm hh d m y)
  (format stream "~2,'0:d~2,'0:d~2,'0d ~2,'0d.~2,'0d.~4,'0d "
          hh mm ss d m y)
  (encode-timezone time-zone stream))

(define-printer print-rfc1123 (stream ss mm hh d m y dow)
  (format stream "~a, ~d ~a ~d ~d:~2,'0d:~2,'0d "
          (encode-weekday dow :length 3) d
          (encode-month m :length 3) y hh mm ss)
  (encode-timezone time-zone stream))

(define-printer print-date (stream NIL NIL NIL d m y)
  (format stream "~4,'0d.~2,'0d.~2,'0d"
          y m d))

(define-printer print-clock (stream ss mm hh)
  (format stream "~d:~2,'0d:~2,'0d"
          hh mm ss))

(defun print-relative (stamp &optional stream now)
  (let ((diff (- stamp (or now (get-universal-time)))))
    (labels ((format-relative (stream)
               (let ((seconds   (mod (floor (/ diff 1)) 60))
                     (minutes   (mod (floor (/ diff 60)) 60))
                     (hours     (mod (floor (/ diff 60 60)) 24))
                     (days      (mod (floor (/ diff 60 60 24)) 7))
                     ;; We approximate by saying each month has four weeks
                     (weeks     (mod (floor (/ diff 60 60 24 7)) 4))
                     (months    (mod (floor (/ diff 60 60 24 7 4)) 12))
                     ;; More accurate through diff in a year
                     (years     (mod (floor (/ diff 31557600)) 10))
                     (decades   (mod (floor (/ diff 31557600 10)) 10))
                     (centuries (mod (floor (/ diff 31557600 10 10)) (expt 10 (- 9 2))))
                     (aeons          (floor (/ diff 31557600 10 10 (expt 10 (- 9 2)))))
                     (non-NIL ()))
                 (flet ((p (i format) (when (< 0 i) (push (format NIL format i) non-NIL))))
                   (p seconds "~a second~:p")
                   (p minutes "~a minute~:p")
                   (p hours "~a hour~:p")
                   (p days "~a day~:p")
                   (p weeks "~a week~:p")
                   (p months "~a month~:p")
                   (p years "~a year~:p")
                   (p decades "~a decade~:p")
                   (p centuries "~a centur~:@p")
                   (p aeons "~a aeon~:p")
                   (format stream "~{~a~^, ~}" non-NIL))))
             (thunk (stream)
               (cond ((= 0 diff)
                      (format stream "now"))
                     ((< 0 diff)
                      (format stream "in ")
                      (format-relative stream))
                     (T
                      (format-relative stream)
                      (format stream " ago")))))
      (etypecase stream
        (null
         (with-output-to-string (stream)
           (thunk stream)))
        ((eql T)
         (thunk *standard-output*))
        (stream
         (thunk stream))))))

(defun print (stamp &key (format :rfc3339) stream time-zone now)
  (ecase format
    (:rfc3339 (print-rfc3339 stamp stream time-zone))
    (:iso8661 (print-iso8661 stamp stream time-zone))
    (:reverse (print-reverse stamp stream time-zone))
    (:rfc1123 (print-rfc1123 stamp stream time-zone))
    (:relative (print-relative stamp stream now))
    (:date (print-date stamp stream time-zone))
    (:clock (print-clock stamp stream time-zone))))

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

(defun backfill-timestamp (y o d h m s tz)
  (multiple-value-bind (ls lm lh ld lo ly) (apply #'decode-universal-time (get-universal-time) (if tz (list tz)))
    (let ((time (apply #'encode-universal-time (or s ls) (or m lm) (or h lh) (or d ld) (or o lo) (or y ly) (if tz (list tz)))))
      (when (and y (< 100 y))
        (incf y 1900))
      time)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-tzdb (db)
    (with-open-file (stream db)
      (let ((db (make-hash-table :test 'equal)))
        (loop for line = (read-line stream NIL)
              while line
              unless (and (<= (length line) 1) (char= (char line 0) #\#))
              collect (with-integers-bound (_z _+ h m) ("(\\w+) *([+-])?(\\d+)(?::(\\d+))?" line)
                        (setf (gethash (string-downcase _z) db)
                              (* (if (string= _+ "-") -1 +1) (+ h (/ (or m 0) 60))))))
        db))))

(defvar *tzdb* (parse-tzdb #.(make-pathname :name "tz" :type "txt" :defaults (or *compile-file-pathname* *load-pathname*))))

(defun decode-weekday (w &optional errorp)
  (with-scans w
    ("(mo|mon|monday)" 0)
    ("(tu|tue|tuesday)" 1)
    ("(th|thu|thursday)" 2)
    ("(we|wed|wednesday)" 3)
    ("(fr|fri|friday)" 4)
    ("(sa|sat|saturday)" 5)
    ("(su|sun|sunday)" 6)
    (T (when errorp (error "Unknown weekday: ~a" w)))))

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
    ("(mo|months?)" 2592000)
    ("(y|years?)" 31536000)
    ("(de|decades?)" 315532800)
    ("(c|century|centuries)" 3155695200)
    ("(a|aeons?)" 31536000000000000)
    (T (when errorp (error "Unknown time unit: ~a" u)))))

(defun decode-timezone (tz &optional errorp)
  (or (when (or (null tz) (string= "z" tz) (string= "" tz)) 0)
      (gethash tz *tzdb*)
      (when errorp (error "Unknown time zone: ~a" tz))))

(defun parse-forward-time (string)
  ;; in 5m, 9s
  (cl-ppcre:register-groups-bind (parts) ("^in *(.*)$" string)
    (let ((sum (get-universal-time)))
      (dolist (part (cl-ppcre:split "[, ]+" parts) sum)
        (cl-ppcre:register-groups-bind (c u) ("(\\d*) *(.*)" part)
          (incf sum (* (parse-integer* c) (decode-unit u T))))))))

(defun parse-backward-time (string)
  ;; 10 seconds ago
  (cl-ppcre:register-groups-bind (parts) ("^(.*) *ago$" string)
    (let ((sum (get-universal-time)))
      (dolist (part (cl-ppcre:split "[, ]+" parts) sum)
        (cl-ppcre:register-groups-bind (c u) ("(\\d*) *(.*)" part)
          (decf sum (* (parse-integer* c) (decode-unit u T))))))))

(defun parse-timezone (string)
  ;; JST+5:00
  (with-integers-bound (_z _+ oh om) ("(\\w+)? *([+\\-])(\\d{1,2})(?::*(\\d+))?$" string)
    (+ (decode-timezone _z)
       (* (if (string= "-" _+) -1 +1)
          (+ (* (or oh 0) (/ (or om 0) 60)))))))

(defun parse-rfc3339-like (string)
  ;; 2023.09.15T20:35:42Z
  (with-integers-bound (y o d h m s) ("^(?:(\\d+)[ ,./\\-](\\d+)(?:[ ,./\\-](\\d+))?[t\\- ]+)?~
                                        (\\d+):+(\\d+)(?::+(\\d+))?(?:\\.+\\d*)?" string)
    (backfill-timestamp y o d h m s (parse-timezone string))))

(defun parse-iso8661-like (string)
  ;; 20230915T203542Z
  (with-integers-bound (y o d h m s) ("^(?:(\\d{4})?(\\d{2})(\\d{2})[t\\- ]+)?~
                                        (\\d{2})(\\d{2})(\\d{2})?" string)
    (backfill-timestamp y o d h m s (parse-timezone string))))

(defun parse-human-like (string)
  ;; 22:48:34 15.9.2023 GMT
  (with-integers-bound (h m s d o y) ("^(?:(\\d+):(\\d+)(?::(\\d+))?[t\\- ]+)?~
                                        (\\d+)[./-](\\d+)(?:[./-](\\d+))?" string)
    (backfill-timestamp y o d h m s (parse-timezone string))))

(defun parse-rfc1123-like (string)
  ;; Thu, 23 Jul 2013 19:42:23 GMT
  (or (with-integers-bound (_dow d _o y h m s) ("^(\\w+)[ ,./\\-]*(\\d+)[ ,./\\-]*(\\w+)[ ,./\\-]*(\\d+)~
                                           (?:[ ,./\\-]*(\\d+):+(\\d+)(?::+(\\d+))?(?:\\.+\\d*)?)?" string)
        (backfill-timestamp y (decode-month _o T) d h m s (parse-timezone string)))
      (with-integers-bound (d _o y h m s) ("^(\\d+)[ ,./\\-]*(\\w+)[ ,./\\-]*(\\d+)~
                                           (?:[ ,./\\-]*(\\d+):+(\\d+)(?::+(\\d+))?(?:\\.+\\d*)?)?" string)
        (backfill-timestamp y (decode-month _o T) d h m s (parse-timezone string)))))

(defun parse-single (string)
  (with-scans string
    ("\\d+"
     (let ((stamp (parse-integer string)))
       (cond ((< stamp 1000) ;; Seconds in the future
              (+ stamp (get-universal-time)))
             ((< stamp 5000) ;; A year relative to current time
              (multiple-value-bind (ls lm lh ld lo) (decode-universal-time (get-universal-time))
                (encode-universal-time ls lm lh ld lo stamp)))
             (T ;; A UNIX timestamp
              (+ stamp (encode-universal-time 0 0 0 1 1 1970 NIL))))))
    ("\\w+"
     (let ((w (decode-weekday string))
           (o (decode-month string)))
       (multiple-value-bind (ls lm lh ld lo ly lw) (decode-universal-time (get-universal-time))
         (cond (w ;; A day of the week relative to current time, always in the future
                (+ (get-universal-time) (* (decode-unit "d") (if (< lw w) (- w lw) (- 7 (- lw w))))))
               (o ;; A month relative to current time, always in the future
                (encode-universal-time ls lm lh ld o (if (< lo o) ly (1+ ly))))))))))

(defun parse (string &optional errorp)
  (let ((string (string-downcase string)))
    (or (parse-forward-time string)
        (parse-backward-time string)
        (parse-rfc3339-like string)
        (parse-iso8661-like string)
        (parse-human-like string)
        (parse-rfc1123-like string)
        (parse-single string)
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
              (get-universal-time)))))))
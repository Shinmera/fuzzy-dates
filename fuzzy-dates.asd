(asdf:defsystem fuzzy-dates
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A library to fuzzily parse date strings"
  :homepage "https://shinmera.com/docs/fuzzy-dates/"
  :bug-tracker "https://shinmera.com/project/fuzzy-dates/issues"
  :source-control (:git "https://shinmera.com/project/fuzzy-dates.git")
  :serial T
  :components ((:file "package")
               (:file "fuzzy-dates")
               (:file "documentation"))
  :depends-on (:cl-ppcre
               :documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :fuzzy-dates/test))))

(asdf:defsystem fuzzy-dates/test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for fuzzy-dates"
  :homepage "https://shinmera.com/docs/fuzzy-dates/"
  :bug-tracker "https://shinmera.com/project/fuzzy-dates/issues"
  :source-control (:git "https://shinmera.com/project/fuzzy-dates.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:fuzzy-dates :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.fuzzy-dates.test)))

(asdf:defsystem fuzzy-dates
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A library to fuzzily parse date strings"
  :homepage "https://shinmera.github.io/fuzzy-dates/"
  :bug-tracker "https://github.com/shinmera/fuzzy-dates/issues"
  :source-control (:git "https://github.com/shinmera/fuzzy-dates.git")
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
  :homepage "https://shinmera.github.io/fuzzy-dates/"
  :bug-tracker "https://github.com/shinmera/fuzzy-dates/issues"
  :source-control (:git "https://github.com/shinmera/fuzzy-dates.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:fuzzy-dates :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.fuzzy-dates.test)))

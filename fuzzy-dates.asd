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
               :documentation-utils))

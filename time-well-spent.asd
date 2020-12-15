;;;; time-well-spent.asd

(asdf:defsystem #:time-well-spent
  :description "Describe time-well-spent here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:bknr.datastore
               #:lazybones
               #:local-time
               #:lass
               #:spinneret
               #:jonathan
               #:trivia
               #:parenscript)
  :components ((:file "package")
               (:file "time-well-spent")))

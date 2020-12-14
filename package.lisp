;;;; package.lisp

(defpackage #:time-well-spent
  (:use #:cl)
  (:local-nicknames (#:db #:bknr.datastore))
  (:import-from #:spinneret
               #:with-html
               #:with-html-string)
  (:import-from #:alexandria
                #:if-let
                #:when-let)
  (:import-from #:lazybones
                #:defroute
                #:*req*
                #:*body*
                #:http-ok
                #:http-err
                #:http-redirect))

(ql:quickload :time-well-spent)
(clack.util:find-handler :hunchentoot)
(sb-ext:save-lisp-and-die #P"tws"
                          :toplevel #'tws::start-loop
                          :executable t)

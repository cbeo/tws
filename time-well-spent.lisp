;;;; time-well-spent.lisp

(in-package #:time-well-spent)

;;; SYSTEM INITIALIZATION

(defparameter +data-store-directory-name+
  "tws-store")

(defun data-store-path ()
  (make-pathname
   :directory (append (pathname-directory (user-homedir-pathname))
                      (list +data-store-directory-name+))))

(defun initialize-datastore ()
  (ensure-directories-exist (data-store-path))
  (make-instance 'db:mp-store
                 :directory (data-store-path)
                 :subsystems (list (make-instance 'db:store-object-subsystem))))

(defvar *current-activity* nil
  "For single-user prototype only. Holds the current activity. Will be
  set at initialization:")

(defun initialize-current-activity ()
  (dolist (act (db:store-objects-with-class 'activity))
    (when (currently-working-p act)
      (setf *current-activity* act)
      (return-from initialize-current-activity))))

(defun start ()
  (unless (boundp 'db:*store*)
    (initialize-datastore))
  (unless (get-config)
    (db:with-transaction ()
      (make-instance 'config)))
  (initialize-current-activity)

  (bt:make-thread (lambda () (swank:create-server :port 4010 :dont-close t)))

  (lzb:start))

(defun help-menu ()
  (format t "Enter one of the following commands:~%~{~s~%~}~%"
          '(:quit :snapshot))
  (force-output))

(defun start-loop () 
  (start)
  (help-menu)
  (loop :for command = (read)
        :do (case command
              (:quit
               (format t "quitting~%")
               (lzb:stop)
               (uiop:quit))
              (:snapshot
               (format t "snapshotting~%")
               (db:snapshot))
              (t
               (help-menu)))))

;;; Utilities

(defun local-datestring->utc (datestring &key end-of-day)
  (lt:timestamp-to-universal
   (lt:parse-timestring
    (format nil "~aT~a-06:00"
            datestring
            (if end-of-day
                "23:59:59.000000"
                "00:00:00.000000")))))

(defun utc->timestring (utc)
  (datestring (lt:universal-to-timestamp utc) t))

(defun datestring (timestamp &optional include-time-p)
  (let* ((month-portion
           (if (< (lt:timestamp-month timestamp) 10)
               '( "0" :month)
               '(:month)))

         (day-portion
           (if (< (lt:timestamp-day timestamp) 10)
               '("0" :day)
               '(:day)))

         (format
           (append '(:year)
                   (cons "-" month-portion)
                   (cons "-" day-portion)
                   (when include-time-p
                     (list " " :hour ":" :min)))))
    (lt:format-timestring nil timestamp
                          :format format )))

(defun utc->datestring (utc)
  (datestring (lt:universal-to-timestamp utc)))


(defun datestring-today ()
  (datestring (lt:now)))

(defun datestring-start-of-week  ()
  (let ((now (lt:now)))
    (datestring (lt:timestamp- now (lt:timestamp-day-of-week now) :day))))

(defun datestring-end-of-week ()
  (let ((now (lt:now)))
    (datestring (lt:timestamp+ now (- 6 (lt:timestamp-day-of-week now)) :day ))))

(defun datestring-start-of-month ()
  (let ((now (lt:now)))
    (datestring (lt:timestamp- now (1- (lt:timestamp-day now)) :day))))

(defun datestring-end-of-month ()
  (let ((this-time-next-month (lt:timestamp+ (lt:now) 1 :month)))
    (datestring
     (lt:timestamp- this-time-next-month (lt:timestamp-day this-time-next-month) :day))))

(defun remove-nth (n ls)
  (cond ((= n 0) (cdr ls))
        ((null ls) ls)
        (t 
         (cons (car ls)
               (remove-nth (1- n) (cdr ls))))))

(defun seconds->minutes (sec)
  (/ sec 60.0))

(defun seconds->hours (sec)
  (/ sec 60.0 60.0))

(defun hours-minutes (sec)
  (multiple-value-bind (hours frac) (floor (seconds->hours sec))
    (list hours (floor  (* frac 60)))))

(defun hours-minutes-string (sec)
  (apply #'format nil "~2,'0d:~2,'0d"  (hours-minutes sec)))


(defun make-keyword (string)
  (and string
       (plusp (length string))
       (intern (format nil "~a" (string-upcase string))
               (find-package 'keyword))))

;;; CLASSES and PROTOCOL FUNCTIONS

(defclass config (db:store-object)
  ((categories
    :accessor categories
    :initform nil)
   (statuses
    :accessor statuses
    :initform (list :backlog :todo :done)))
  (:metaclass db:persistent-class))

(defun get-config ()
  (car  (db:store-objects-with-class  'config)))

(defun add-status (config status)
  (pushnew status (statuses config)))

(defun add-category (config category)
  (pushnew category (categories config)))

(defun remove-category (config index)
  (setf (categories config)
        (remove-nth index (categories config))))

(defun remove-status (config index)
  (setf (statuses config)
        (remove-nth  index (statuses config))))

(defclass project (db:store-object)
  ((name
    :accessor project-name
    :initarg :name
    :initform (error "Projects Require a Name"))
   (description
    :accessor project-description
    :initarg :description
    :initform "")
   (archived-p
    :accessor project-archived-p
    :initform nil))
  (:metaclass db:persistent-class))

(defun project-time (project &key start (stop (get-universal-time)))
  (reduce #'+ (activities-by-project project)
          :initial-value 0
          :key (lambda (activity)
                 (seconds-worked activity :start start :stop stop))))

(defun get-projects (ids)
  "IDS is a list of ids"
  (mapcar 'db:store-object-with-id ids))

(defun all-projects (&key just-archived-p include-archived-p)
  (sort
   (copy-seq
    (remove-if-not
     (lambda (project) (or include-archived-p
                           (eql just-archived-p (project-archived-p project))))     
     (db:store-objects-with-class 'project)))
   #'>
   :key (lambda (o) (db:store-object-last-change o 1))))

(defclass time-span (db:store-object)
  ((start-time
    :accessor start-time
    :initarg :start
    :initform (error "A Time-Span Requires A Start Time"))
   (stop-time
    :accessor stop-time
    :initarg :stop
    :initform nil))
  (:metaclass db:persistent-class))


(defun span-seconds (span)
  (with-slots (start-time stop-time) span
    (if stop-time
        (- stop-time start-time)
        0)))

(defun span-between-p (span start stop)
  (and (stop-time span)
       (< start (start-time span) (stop-time span) stop)))

(defclass activity (db:store-object)
  ((name
    :accessor activity-name
    :initarg :name
    :initform (error "Activity Name Required"))
   (description
    :accessor activity-description
    :initarg :description
    :initform "")
   (estimate
    :accessor activity-estimate
    :initarg :estimate
    :initform (error "Estimated Time To Complete Required")
    :documentation "Estimated number of minutes to complete.")
   (log
    :accessor activity-log
    :initform nil
    :documentation "A list of TIME-SPAN instances.")
   (currently-working-p
    :accessor currently-working-p
    :initform nil
    :documentation "Either NIL or time returned from (GET-UNIVERSAL-TIME)")
   (project
    :accessor activity-project
    :initarg :project
    :initform (error "activities must belong to a project")
    :index-type bknr.indices:hash-index
    :index-reader activities-by-project
    :documentation "A way to group activities.")
   (status 
    :accessor activity-status
    :initarg :status
    :initform :todo)
   (category
    :accessor activity-category
    :initarg :category
    :initform nil
    :index-type bknr.indices:hash-index
    :index-initargs (:test 'equal)
    :index-reader activities-by-category))
  (:metaclass db:persistent-class))

(defun estimate-ratio (activity)
  (/ (seconds-worked activity) (* (activity-estimate activity) 60 60)))

(defun done-p (activity)
  (eql :done (activity-status activity)))

(defun monte-carlo-estimate-for-activity (activity &key (samples 100))
  (let* ((space
           (remove-if-not 'done-p
                          (activities-by-category (activity-category activity))))
         (space-size (length space))
         (sum 0)
         (worked-sofar (/  (seconds-worked activity) 3600))
         (estimate (activity-estimate activity)))
    (- (if (plusp space-size)
           (dotimes (i samples (/ sum samples))
             (incf sum
                   (* estimate 
                      (estimate-ratio
                       (elt space (random space-size))))))
           estimate)
       worked-sofar)))

(defun todo-p (activity)
  (eql :todo (activity-status activity)))

(defun monte-carlo-estimate-for-project (project &key (samples 100))
  (loop
    :for a :in (activities-by-project project)
    :when (todo-p a)
      :summing (monte-carlo-estimate-for-activity a :samples samples)))

(defun project-estimate-range (project &key (samples 100) (simulations 10))
  (loop
    :with sum = 0
    :with min-time = nil
    :with max-time = nil
    :for i :upto simulations
    :for estimate = (monte-carlo-estimate-for-project project :samples samples)
    :do
       (incf sum estimate)
       (setf max-time (if (null max-time) estimate (max estimate max-time))
             min-time (if (null min-time) estimate (min estimate min-time)))
    :finally
       (return (list :min min-time :average (/ sum simulations) :max max-time))))

(defun start-working (activity)
  (when *current-activity*
    (stop-working *current-activity*))

  (setf (currently-working-p activity) (get-universal-time)
        (activity-status activity) :todo)
  (setf *current-activity* activity)
  (db:store-object-touch (activity-project activity)))

(defun stop-working (activity)
  (when (eql activity *current-activity*)
    (push (make-instance 'time-span
                         :start (currently-working-p activity)
                         :stop (get-universal-time))
          (activity-log activity))
    (setf (currently-working-p activity) nil)
    (db:store-object-touch (activity-project activity))
    (setf *current-activity* nil)))

(defun seconds-worked (activity &key start (stop (get-universal-time)))
  (let ((activities
          (if start
              (remove-if-not (lambda (span) (span-between-p span start stop))
                             (activity-log activity))
              (activity-log activity))))
    (reduce #'+ activities
            :key 'span-seconds
            :initial-value 0)))

(defun activity-before (a b)
  (or 
   (match (list (activity-status a) (activity-status b))
     ((list :todo :todo)
      (> (seconds-worked a)
         (seconds-worked b)))
     ((list :todo _) t)
     ((list :backlog :done) t)
     ((list s1 s2)
      (and (eql s1 s2)
           (> (seconds-worked a)
              (seconds-worked b)))))

   (< (db:store-object-last-change a 2)
      (db:store-object-last-change b 2))))

(defun sort-activities (activities)
  (sort (copy-seq activities) 'activity-before))

;;; Pages

(defmacro defpage  (name lambda-list (&key stylesheets scripts (title "")) &body body)
  (let ((page-name (intern (format nil "PAGE/~a" name))))
    `(defun ,page-name ,lambda-list
       (with-html-string
         (:doctype)
         (:html
          (:head
           (:title ,title)
           (dolist (css (list ,@stylesheets)) 
             (:link :rel "stylesheet" :href css)))
          (:body
           ,@body
           (dolist (js (list ,@scripts))
             (:script :src js))))))))

(defmacro defview (name lambda-list &body body)
  (let ((view-name (intern (format nil "VIEW/~a" name))))
    `(defun ,view-name ,lambda-list
       (with-html
         ,@body))))

(defun main-css ()
  (lass:compile-and-write
   '(:let ((tertiary-color "#BADA55")
           (secondary-color "#55BCDA")
           (primary-color "#DA55BB")
           (dark "#202020")
           (medium-dark "#363636")
           (medium "#444444")
           (light "#EEF0EE")
           (radius-low 3px)
           (radius-mid 6px)
           (padding-low 4px)
           (padding-mid 8px))
     (body
      :padding 10px
      :background-color #(dark)
      :color #(secondary-color))

     (a
      :text-decoration none
      )
     
     (.primary-color
      :color #(primary-color ))

     (.tertiary-color
      :color #(tertiary-color))

     (.secondary-color
      :color #(secondary-color))

     (h1
      :color #(primary-color)
      :font-size 2.5em)


     (.form-input
      :margin #(padding-mid )
      :border none
      :font-size 1.2em
      :color #(secondary-color)
      :background-color #(medium-dark))

     (label
      :width 50%
      :margin-top 1em
      :font-size 1.2em
      :color #(secondary-color))
     
     (.button
      :background-color #(dark)
      :border 1px solid #(primary-color)
      :color #(primary-color)
      :border-radius #(radius-low)
      :text-decoration none
      :text-align center

      :margin 4px
      :padding #(padding-low))

     ((:and .button :hover)
      :border 1px solid #(secondary-color)
      :color #(secondary-color))

     (.inline
      :display inline)

     (.nav
      :position fixed
      :height 100%
      :width 16%
      :left #(padding-low)
      :top 20%
      :z-index 1

      (a
       :width 100%
       :height 100px
       :display inline-block
       :margin-top 10px
       :padding-top 2px
       :margin-right 20px
       :color #(tertiary-color)
       :font-size 1.3em
       :border-top 2px solid #(secondary-color))

      ((:and a :hover)
       :border-right 10px solid #(secondary-color)))

     (.right
      :float right)
     
     (.card
      :padding 0.8em
      :border-radius #(radius-low)
      :background-color #(medium-dark)
      :text-decoration none
      :color #(secondary-color))
     
     (.primary-highlight
      :border 1px solid #(primary-color))

     (.bg-highlight
      :background-color #(medium))

     (.unstyled
      :list-style-type none
      (li :margin 6px))

     (.hidden
      :visibility hidden
      :height 0
      :widht 0)

     (a
      ((:and .card :hover)
       :transform "rotate(-0.003turn)"))

     (.activity
      :margin-top 5px
      :border-top 1px dotted #(primary-color)
      :display grid
      :width 100%
      :grid-template-columns "3fr 1fr 1fr 1fr 3fr"
      )

     (.activity-title
      :color #(tertiary-color)
      :margin-top 5px
      :border-top 1px dotted #(primary-color)
      :display grid
      :width 100%
      :grid-template-columns "3fr 1fr 1fr 1fr 3fr"
      )


     (.timesheet-entry
      :margin-top 5px
      :border-bottom 1px dotted #(primary-color)
      :display grid
      :width 100%
      :grid-template-columns "2fr 1fr 1fr"
      )
     
     ((:and .activity :hover)
      :background-color #(medium) )

     (.project
      (h2
       :padding-top 0
       :color #(tertiary-color)
       :border-bottom 1px dotted #(secondary-color)))

     (.canceller
      :color #(primary-color))
     
     (.main-content
      :margin-left 20%)

     (.checklist-grid
      :list-style-type none

      (li
       :display block
       :float left
       :width 33%)

      ) 
          
     (.main-grid
      :margin-top 1em
      :display grid
      :grid-column-gap 1.5em
      :grid-row-gap 1.5em
      :grid-template-columns "repeat(auto-fit, minmax(250px, auto))"))))

(defview project-dashboard (project)
  (with-slots (db::id name description) project

    (:a :href (format nil  "/project/view/~a?status=TODO" db::id)
        (:div :class (if (and *current-activity*
                              (eql project (activity-project *current-activity*)))
                         "project card primary-highlight"
                         "project card")
              (:h2 name)
              (when (and *current-activity*
                         (eql project (activity-project *current-activity*)))
                (:p "Currently working on "
                    (:span :class "primary-color" (activity-name *current-activity*))))
              (:p "Total Time:"
                  (:span :class "tertiary-color" (hours-minutes-string (project-time project))))
              (:h4 "Estimated Work Left")
              (view/project-estimate project)
              (:p description)))))

(defview project-estimate (project)
  (let ((prediction (project-estimate-range project :simulations 100)))
    (:ul :class "unstyled"
         (:li :class "tertiary-color"
              (format nil "Between ~a and ~a"
                      (hours-minutes-string  (* 60 60  (getf prediction :min)))
                      (hours-minutes-string (* 60 60 (getf prediction :max)))))
         (:li :class "tertiary-color"
              (format nil "Average ~a"
                      (hours-minutes-string (* 60 60 (getf prediction :average))))))))


(defview nav ()
  (:div :class "nav"
        (:a :href "/" "Dashboard")
        " "
        (:a 
         :href "/activities/view?status=TODO"
         "Overview")
        " "
        (:a :href (format nil "/stats/?start-date=~a&end-date=~a"
                          (datestring-today) (datestring-today))
            "Stats")
        " "
        (:a :href "/config" "Config")))


(defpage dashboard () (:title "TWS - DASHBOARD"
                       :stylesheets ("/css/main.css"))
  (view/nav)
  (:div
   :class "main-content"
   (:h1 "PROJECTS")
   (:div
    (:a :class "button"
        :href "/project/add"
        "Add Project")
    (:a :class "button"
        :href "/archived"
        "Archived Projects")
    (:div :class "main-grid"
          (dolist (project (all-projects))
            (view/project-dashboard project))))))

(defpage archived () (:title "Archived Projects"
                      :stylesheets ("/css/main.css"))
  (view/nav)
  (:div
   :class "main-content"
   (:h1 "ARCHIVED PROJECTS")
   (:div :class "main-grid"
         (dolist (project (all-projects :just-archived-p t))
           (view/project-dashboard project)))))

(defpage new-project () (:title "New Project"
                         :stylesheets ("/css/main.css"))
  (view/nav)
  (:div
   :class "main-content" 
   (:h1 "New Project")
   (:form
    :method "POST" :action "/project/add"
    (:input :name "name" :placeholder "Project Name" :size "40"
            :class "form-input")
    (:br)
    (:textarea :name "description" :rows "12" :cols "80"
               :class "form-input" :placeholder "Description ...")
    (:br)
    (:button :type "submit" :class "button"
             "Create Project"))))

(defpage activities () (:title "TWS - Activities"
                        :stylesheets ("/css/main.css"))
  (view/nav)
  (let* ((query (query-plist))
         (status (when-let (stat-string (getf query :status))
                   (make-keyword stat-string))))
    (:div
     :class "main-content"
     (:h2 :class "tertiary-color"
          "OVERVIEW" (when status (:span :class "primary-color" " - " status)))

     (:div
      (:strong "View By")
      (:a :class "button" :href "/activities/view?status=TODO"
          "TODO")
      (:a :class "button" :href "/activities/view?status=BACKLOG"
          "BACKLOG")
      (:a :class "button" :href "/activities/view?status=DONE"
          "DONE")
      (:a :class "button" :href "/activities/view"
          "View All")
      )
     (:br)
     (dolist (project (all-projects)) 
       (when-let (activities
                  (remove-if-not (lambda (act) (or (not status)
                                                   (eql status (activity-status act))))
                                 (activities-by-project project))) 
         (:div
          (:a :class "primary-color"
              :href (format nil  "/project/view/~a" (db:store-object-id project))
              (:h3  (project-name project)))
          (:div :class "activity-title"
                (:span "NAME")
                (:span "CATEGORY")
                (:span "ESTIMATE")
                (:span "WORKED")
                (:span ""))
          (dolist (activity activities)
            (view/activity activity)))
         (:br))))))

(defview activity (activity)
  (with-slots (db::id name estimate currently-working-p category log) activity 
    (:div 
     :class (if currently-working-p "activity bg-highlight" "activity")
     (:span  name)
     (:span  category)
     (:span  estimate)
     (:span  (if log
              (hours-minutes-string (seconds-worked activity))
              "00:00"))
     (:div :class "inline" (view/activity-controls activity)))))

(defpage activity (activity) (:stylesheets ("/css/main.css"))
    (with-slots (db::id log name description) activity
      (view/nav)
      (:div :class "main-content"
            (:p 
             (:a
              :class "tertiary-color"
              :href (format nil "/project/view/~a"
                            (db:store-object-id
                             (activity-project activity)))
              " ⮪ back to " (project-name (activity-project activity))))

            (:form
             :action (format nil "/activity/update/~a" db::id)
             :method "POST"
             (:input :value name :name "name" :class "form-input")
             (:label :for "name" "Name")
             (:br)
             (:textarea :name "description" :rows 5 :cols 60 :class "form-input"
                        description)
             (:label :for "description" "Description")
             (:br)
             (:button :type "submit" :class "button" "Update"))
            
            (:br) (:br)
            (:a :class "button"
                :href (format nil "/activity/delete/~a" db::id)
                "Delete This Activity")
            (:h4 "Log ")
            (:ul :class "unstyled"
                 (dolist (span log)
                   (:li
                    (:a :class "button"
                        :href (format nil "/span/delete/~a/~a"
                                      db::id
                                      (db:store-object-id span))
                            " ❌ ")
                    (:span  (utc->timestring (start-time span)) " -- "
                            (utc->timestring (stop-time span)))
                    " "))))))


(defview activity-controls (activity)
  (with-slots (db::id name currently-working-p status log) activity
    (let ((status-id (format nil "status-~a" db::id))
          (form-id (format nil "status-form-~a" db::id)))
      (if currently-working-p 
          (:a :class "button"
              :href (format nil  "/activity/clock-out/~a" db::id)
              "Clock Out")
          (:a :class "button"
              :href (format nil  "/activity/clock-in/~a" db::id)
              "Clock In"))
      (:form :method "POST" :action (format nil "/activity/status/~a" db::id)
             :style "display:inline;"
             :id form-id
             (:select :name "status" :class "button" :id status-id
               (dolist (s (statuses (get-config)))
                 (if (eql s status)
                     (:option  :value s :selected "true" s)
                     (:option :value s s)))))
      
      (:a :class "tertiary-color"
          :href (format nil "/activity/view/~a" db::id)
          "view")
      (:script
       :type "text/javascript"
       (ps:ps
         (ps:chain document
                   (get-element-by-id (ps:lisp status-id))
                   (add-event-listener
                    "change"
                    (lambda (event)
                      (ps:chain document
                                (get-element-by-id (ps:lisp form-id))
                                (submit))))))))))

(defview new-activity-form (project)
  (:div :class "right" :style "margin-right: 10%;margin-bottom:50px;"
   (:h3 "Add Another Activity")
   (:form
    :method "POST"
    :action (format nil "/activity/add-to-project/~a" (db:store-object-id project))

    (:input :name "name" :placeholder "Name" :class "form-input" :style "width:100%")
    (:br)

    (:input :name "estimate" :type "number" :min "0" :step "0.1"
            :class "form-input" :placeholder "Estimated Hours"
            :style "width:100%")
    (:br)
    (:select :name "category" :style "width:50%" :class "form-input"
      (dolist (category (categories (get-config)))
        (:option :value category category)))
    (:label :for "category"  "Category")
    (:br)
    (:button :class "button" :type "submit" "Create Activity")
    )))

(defpage project (project)
    (:title "Project View"
     :stylesheets ("/css/main.css"))
  (view/nav)
  (:div
   :class "main-content"
    (view/new-activity-form project)
   (:h1 (project-name project))
   (:p "Total Time: "
       (:span :class "tertiary-color" (hours-minutes-string (project-time project))))
   (:p "Estimated Time To Completion: ")
   (view/project-estimate project)
   (:p (project-description project))

   (:button :class "button" :id (format nil "delete-project-button")
            "Delete Project")

   (if (project-archived-p project)
       (:a :class "button"
           :href (format nil "/project/unarchive/~a" (db:store-object-id project))
           "Unarchive Project")
       (:a :class "button"
           :href (format nil "/project/archive/~a" (db:store-object-id project))
           "Archive Project"))


   (:form
    :id "delete-project-form"
    :class "hidden"
    :method "POST"
    :action (format nil "/project/delete/~a" (db:store-object-id project))
    (:p "Are you sure you want to delete this project?")
    (:input :name "ignore" :value "Ignore Me" :class "hidden")
    (:br)
    (:button :class "button" :type "submit" "Comfirm & Delete"))

   (:div
    (let ((id (db:store-object-id project)))
      (:div
       (:strong "View By")
       (:a :class "button"
           :href (format nil "/project/view/~a?status=TODO" id)
           "TODO")
       (:a :class "button"
           :href (format nil "/project/view/~a?status=BACKLOG" id)
           "BACKLOG")
       (:a :class "button"
           :href (format nil "/project/view/~a?status=DONE" id)
           "DONE")
       (:a :class "button"
           :href (format nil  "/project/view/~a" id)
           "View All")))
    (:div :class "activity-title"
          (:span "NAME")
          (:span "CATEGORY")
          (:span "ESTIMATE")
          (:span "WORKED")
          (:span ""))
    (let* ((status
             (when-let (stat-string (getf (query-plist) :status))
               (make-keyword stat-string))))
      (dolist (activity (sort-activities  (activities-by-project project)))
        (if status 
            (when (eql status (activity-status activity))
              (view/activity activity))
            (view/activity activity))))))
  (:script
   (ps:ps
     (ps:chain
      document
      (get-element-by-id  "delete-project-button")
      (add-event-listener 
       "click"
       (lambda (event)
         (ps:chain document (get-element-by-id "delete-project-form")
                   class-list
                   (remove "hidden"))
         (ps:chain event target class-list (add "hidden"))))))))


(defview categories-list (categories)
  (:h3 "Categories")
  (:ul
   (loop :for cat :in categories
         :for index :from 0
         :do 
            (:li
             (:a :class "canceller"
                 :href (format nil "/config/drop-category/~a" index)
                 "❌")
             " "
             cat))))

(defview add-category-form ()
  (:form :method "POST" :action "/config/add-category"
         (:input :class "form-input" :placeholder "Category" :name "category")
         (:button :type "submit" :class "button" "Add Category")))

(defview statuses-list (statuses)
  (:h3 "Statuses")
  (:ul :class "unstyled"
   (loop :for status :in statuses
         :for index :from 0
         :do
            (:li
             (:a
              :class "canceller"
              :href (format nil  "/config/drop-status/~a" index)
              "❌")
             " "
             status
             ))))

(defview add-status-form ()
  (:form :method "POST" :action "/config/add-status"
         (:input :class "form-input" :placeholder "Status"
                 :id "new-status-input"
                 :name "status"
                 :pattern "[a-zA-Z0-9\-_]+")
         (:button :type "submit" :class "button"   "Add Status")))

(defpage config () (:title "TWS - Config"
                    :stylesheets ("/css/main.css"))
  (with-slots (categories statuses) (get-config) 
    (view/nav)
    (:div
     :class "main-content"
     (:h1 "Config")
     (view/categories-list categories)
     (view/add-category-form))))



(defpage stats () (:title "TWS - Stats"
                   :stylesheets ("/css/main.css"))
  (let* ((query (query-plist))
         (project-ids (loop :for (k v . more) :on query :by #'cddr
                         :when (eql :projects k)
                           :collect (parse-integer v))))
    (view/nav)
    (:div
     :class "main-content"
     (:h1 "Stats")
     (:div 
      (:a :href (format nil "/stats/?start-date=~a&end-date=~a"
                        (datestring-today) (datestring-today))
          :class "button"
          "Today")
      (:a :href (format nil "/stats/?start-date=~a&end-date=~a"
                        (datestring-start-of-week)
                        (datestring-end-of-week))
          :class "button"
          "This Week")
      (:a :href (format nil "/stats/?start-date=~a&end-date=~a"
                        (datestring-start-of-month)
                        (datestring-end-of-month))
          :class "button"
          "This Month")
      (:a :href "/stats/"
          :class "button"
          "All Time"))
     
     (:br)
     (:div 
      (:form
       :method "GET" :action "/stats"
       (:label :for "start-date" "Days between ")
       (:input :class "form-input" :name "start-date" :type "date"
               :value (when query (getf query :start-date)))
       (:label :for "end-date" " and " )
       (:input :class "form-input" :name "end-date"  :type "date"
               :value (when query (getf query :end-date)))
       (:br)
       (:ul :class "checklist-grid"
        (dolist (proj (all-projects))
          (:li 
           (:input :type "checkbox" :value (format nil "~a" (db:store-object-id proj))
                   :checked (member (db:store-object-id proj) project-ids)
                   :name "projects"
                   :class "project-checkbox"
                   (project-name proj)))))
       (:br)
       (:button :type "submit" :class "button" "View")))

     (:div :class "timesheet-entry tertiary-color"
           (:span "PROJECT") (:span "HOURS") (:span))
     (let ((start (and query  (local-datestring->utc (getf query :start-date))))
           (stop  (and query (local-datestring->utc (getf query :end-date) :end-of-day t)))
           (total-seconds 0))
       (dolist (proj (if project-ids (get-projects project-ids) (all-projects :include-archived-p t)))
         (let ((seconds (project-time proj :start start :stop stop)))
           (incf total-seconds seconds)
           (when (plusp seconds)
             (:div
              :class "timesheet-entry"
              (:span (project-name proj))
              (:span  (hours-minutes-string seconds))
              (:span)))))
       (:div :class "timesheet-entry tertiary-color"
             (:span "TOTAL")
             (:span (hours-minutes-string total-seconds))
             (:span))))))


;;; ROUTES

(defun redirect-to-referrer ()
  (http-redirect (gethash "referer" (getf *req* :headers))))

(defroute :get "/css/main.css"
  (http-ok "text/css"
           (main-css)))

(defroute :get "/"
  (http-ok "text/html"
           (page/dashboard)))

(defroute :get "/project/add"
  (http-ok "text/html"
           (page/new-project)))

(defroute :post "/project/add"
  (db:with-transaction ()
    (apply #'make-instance 'project *body*))
  (http-redirect "/"))

(defroute :get "/project/view/:id"
  (http-ok "text/html"
           (page/project (db:store-object-with-id (parse-integer id)))))

(defroute :get "/config"
  (http-ok "text/html" (page/config)))


(defroute :post "/config/add-status"
  (when-let (new-status (make-keyword (getf *body* :status)))  
    (db:with-transaction ()
      (add-status (get-config) new-status))
    (http-redirect "/config")))

(defroute :post "/config/add-category"
  (when-let (new-category (getf *body* :category))
    (db:with-transaction ()
      (add-category (get-config) new-category)))
  (http-redirect "/config"))

(defroute :get "/config/drop-category/:index"
  (db:with-transaction ()
    (remove-category (get-config) (parse-integer  index)))
  (http-redirect "/config"))

(defroute :get "/config/drop-status/:index"
  (db:with-transaction ()
    (remove-status (get-config) (parse-integer  index)))
  (http-redirect "/config"))

(defroute :post "/activity/add-to-project/:projectid"
  (when-let (project (db:store-object-with-id (parse-integer projectid)))
    (db:with-transaction ()
      (make-instance 'activity
                     :project project
                     :name (getf *body* :name)
                     :estimate (parse-float:parse-float (getf *body* :estimate))
                     :category (getf *body* :category))
      (db:store-object-touch project)))
  (redirect-to-referrer))

(defroute :get "/activity/clock-in/:id"
  (let ((activity (db:store-object-with-id (parse-integer id))))
    (db:with-transaction ()  (start-working activity))    
    (redirect-to-referrer)))

(defroute :get "/activity/clock-out/:id"
  (let ((activity (db:store-object-with-id (parse-integer id))))
    (db:with-transaction ()  (stop-working activity))
    (redirect-to-referrer)))


(defroute :post "/activity/status/:id"
  (let ((activity (db:store-object-with-id (parse-integer id))))
    (db:with-transaction ()
      (setf (activity-status activity)
            (make-keyword (getf *body* :status)))
      (if (eql :DONE (activity-status activity))
          (stop-working activity))
      (db:store-object-touch (activity-project activity)))
    (redirect-to-referrer)))

(defroute :get "/activity/delete/:id"
  (let* ((activity (db:store-object-with-id (parse-integer id)))
         (project  (activity-project activity)))
    (db:with-transaction ()
      (db:store-object-touch project)
      (dolist (span (activity-log activity))
        (db:delete-object span))
      (db:delete-object activity))
    (http-redirect (format nil "/project/view/~a" (db:store-object-id project)))))


(defun query->plist (qstring)
  (when qstring
    (loop :for kv :in (split-sequence:split-sequence #\& (urlencode:urldecode qstring))
          :appending (destructuring-bind (k v) (split-sequence:split-sequence #\= kv)
                       (list (make-keyword k) v)))))

(defun query-plist ()
  (query->plist (getf *req* :query-string)))

(defroute :get "/stats"
  (http-ok "text/html" (page/stats)))


(defroute :get "/activities/view"
  (http-ok "text/html" (page/activities)))

(defroute :post "/project/delete/:project-id"
  (when-let (project (db:store-object-with-id (parse-integer project-id)))
    (db:with-transaction ()
      (dolist (activity (activities-by-project project))
        (dolist (span (activity-log activity))
          (db:delete-object span))
        (db:delete-object activity))
      (db:delete-object project)))
  (http-redirect "/"))

(defroute :get "/activity/view/:act-id"
  (http-ok "text/html"
           (page/activity
            (db:store-object-with-id
             (parse-integer act-id)))))

(defroute :get "/span/delete/:act-id/:sp-id"
  (when-let (span (db:store-object-with-id (parse-integer sp-id)))
    (when-let (activity (db:store-object-with-id (parse-integer act-id)))
      (db:with-transaction ()
        (setf (activity-log activity)
              (remove span (activity-log activity)))
        (db:delete-object span))))
  (redirect-to-referrer))

(defroute :post "/activity/update/:id"
  (if-let (activity (db:store-object-with-id (parse-integer id)))
    (progn (db:with-transaction ()
             (setf (activity-name activity) (getf *body* :name)
                   (activity-description  activity) (getf *body* :description)))
           (http-redirect (format nil "/activity/view/~a" id)))
    (http-err 404 "Activity not found")))

(defroute :get "/project/archive/:id"
  (if-let (project (db:store-object-with-id (parse-integer id)))
    (progn (db:with-transaction ()
             (setf (project-archived-p project) t))
           (http-redirect "/"))
    (http-err 404 "Project not found")))

(defroute :get "/project/unarchive/:id"
  (if-let (project (db:store-object-with-id (parse-integer id)))
    (progn (db:with-transaction ()
             (setf (project-archived-p project) nil))
           (http-redirect "/archived"))
    (http-err 404 "Project not found"))  )

(defroute :get "/archived"
  (http-ok "text/html" (page/archived)))

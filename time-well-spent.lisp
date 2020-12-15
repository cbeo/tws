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

(defun remove-nth (n ls)
  (cond ((= n 0) (cdr ls))
        ((null ls) ls)
        (t 
         (cons (car ls)
               (remove-nth (1- n) (cdr ls))))))

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
    :initform ""))
  (:metaclass db:persistent-class))

(defun project-time (project)
  (reduce #'+ (activities-by-project project)
          :initial-value 0
          :key 'seconds-worked))

(defun all-projects ()
  (sort (copy-seq (db:store-objects-with-class 'project))
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

(defun seconds->minutes (sec)
  (/ sec 60.0))

(defun seconds->hours (sec)
  (/ sec 60.0 60.0))

(defun hours-minutes (sec)
  (multiple-value-bind (hours frac) (floor (seconds->hours sec))
    (list hours (floor  (* frac 60)))))

(defun hours-minutes-string (sec)
  (apply #'format nil "~2,'0d:~2,'0d"  (hours-minutes sec)))

(defclass activity (db:store-object)
  ((name
    :accessor activity-name
    :initarg :name
    :initform (error "Activity Name Required"))
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
    :initform :backlog)
   (category
    :accessor activity-category
    :initarg :category
    :initform nil))
  (:metaclass db:persistent-class))

(defun start-working (activity)
  (when *current-activity*
    (stop-working *current-activity*))

  (setf (currently-working-p activity) (get-universal-time)
        (activity-status activity) :todo)
  (setf *current-activity* activity)
  (db:store-object-touch (activity-project activity)))

(defun stop-working (activity)
  (assert (eql  activity *current-activity*))
  (push (make-instance 'time-span
                       :start (currently-working-p activity)
                       :stop (get-universal-time))
        (activity-log activity))
  (setf (currently-working-p activity) nil)
  (db:store-object-touch (activity-project activity))
  (setf *current-activity* nil))

(defun seconds-worked (activity)
  (reduce #'+ (activity-log activity)
          :key 'span-seconds
          :initial-value 0))

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
      :margin-top 4px
      :margin-bottom 4px
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
      :list-style-type none)

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
          
     (.main-grid
      :margin-top 1em
      :display grid
      :grid-column-gap 1.5em
      :grid-row-gap 1.5em
      :grid-template-columns "repeat(auto-fit, minmax(250px, auto))"))))

(defview project-dashboard (project)
  (with-slots (db::id name description) project
    (:a :href (format nil  "/project/view/~a" db::id)
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
              (:p description)
              
              ))))

(defview nav ()
  (:div :class "nav"
        (:a :href "/" "Dashboard")
        " "
        (:a :href "/config" "Config")))


(defpage dashboard () (:title "TWS - DASHBOARD"
                       :stylesheets ("/css/main.css"))
  (view/nav)
  (:div
   :class "main-content"
   (:h1 "time well spent")
   (:div
    (:a :class "button"
        :href "/project/add"
        "Add Project")
    (:div :class "main-grid"
          (dolist (project (all-projects))
            (view/project-dashboard project))))))

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
                     (:option :value s :selected "true" s)
                     (:option :value s s)))))
      (:a :class "button"
          :href (format nil "/activity/delete/~a" db::id)
          "❌")
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
   (:p (project-description project))

   (:div
    (:div :class "activity-title"
          (:span "NAME")
          (:span "CATEGORY")
          (:span "ESTIMATE")
          (:span "WORKED")
          (:span ""))
    (dolist (activity (sort-activities  (activities-by-project project)))
      (view/activity activity)))))


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

(defpage config () (:title "Config"
                    :stylesheets ("/css/main.css"))
  (with-slots (categories statuses) (get-config) 
    (view/nav)
    (:div
     :class "main-content"
     (:h1 "Config")
     (:div
      :class "main-content"
      (view/categories-list categories)
      (view/add-category-form)
      (view/statuses-list statuses)
      (view/add-status-form)))))

;;; ROUTES

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

(defun make-keyword (string)
  (and string
       (plusp (length string))
       (intern (format nil "~a" (string-upcase string))
               (find-package 'keyword))))

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
  (http-redirect (format nil "/project/view/~a" projectid)))

(defroute :get "/activity/clock-in/:id"
  (let ((activity (db:store-object-with-id (parse-integer id))))
    (db:with-transaction ()  (start-working activity))    
    (http-redirect (format nil "/project/view/~a"
                           (db:store-object-id
                            (activity-project activity))))))

(defroute :get "/activity/clock-out/:id"
  (let ((activity (db:store-object-with-id (parse-integer id))))
    (db:with-transaction ()  (stop-working activity))
    (http-redirect (format nil "/project/view/~a"
                           (db:store-object-id
                            (activity-project activity))))))


(defroute :post "/activity/status/:id"
  (let ((activity (db:store-object-with-id (parse-integer id))))
    (db:with-transaction ()
      (setf (activity-status activity)
            (make-keyword (getf *body* :status)))
      (db:store-object-touch (activity-project activity)))
    (http-redirect (format nil "/project/view/~a"
                           (db:store-object-id
                            (activity-project activity))))))

(defroute :get "/activity/delete/:id"
  (let* ((activity (db:store-object-with-id (parse-integer id)))
         (project  (activity-project activity)))
    (db:with-transaction ()
      (db:store-object-touch project)
      (db:delete-object activity))
    (http-redirect (format nil "/project/view/~a"
                           (db:store-object-id project)))))

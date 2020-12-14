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

(defun all-projects ()
  (db:store-objects-with-class 'project))

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
  (setf *current-activity* activity))

(defun stop-working (activity)
  (assert (eql  activity *current-activity*))
  (push (make-instance 'time-span
                       :start (currently-working-p activity)
                       :stop (get-universal-time))
        (activity-log activity))
  (setf (currently-working-p activity) nil)
  (setf *current-activity* nil))

(defun seconds-worked (activity)
  (reduce #'+ (activity-log activity)
          :key 'span-seconds
          :initial-value 0))

;;; Transactions


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
           (medium "#949494")
           (light "#EEF0EE")
           (radius-low 3px)
           (radius-mid 6px)
           (padding-low 4px)
           (padding-mid 8px))
     (body
      :padding 10px
      :background-color #(dark)
      :color #(secondary-color))

     (.primary-color
      :color #(primary-color ))

     (h1
      :color #(primary-color)
      :font-size 2.5em)

     (.form-input
      :border none
      :font-size 1.2em
      :display block
      :color #(secondary-color)
      :background-color #(medium-dark))

     (label
      :margin-top 1em
      :font-size 1.2em
      :display block
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
       :text-decoration none
       :border-top 2px solid #(secondary-color))

      ((:and a :hover)
       :border-right 10px solid #(secondary-color)))

     (.card
      :padding 1em
      :border-radius #(radius-low)
      :background-color #(medium-dark)
      :text-decoration none
      :color #(secondary-color)
      :height 250px)

     (.lighter
      :background-color #(medium))

     (.unstyled
      :list-style-type none)

     (a
      ((:and .card :hover)
       :transform "rotate(-0.003turn)"))

     (.project
      (h2
       :padding-top 1.2em
       :color #(secondary-color)
       :border-bottom 1px dotted #(tertiary-color)))

     (.canceller
      :color #(primary-color))
     
     (.main-content
      :margin-left 20%)
          
     (.main-grid
      :margin-top 1em
      :display grid
      :grid-column-gap 1.5em
      :grid-row-gap 1.5em
      :grid-template-columns "repeat(auto-fit, minmax(340px, 500px))"))))

(defview project-dashboard (project)
  (with-slots (db::id name description) project
    (:a :href (format nil  "/project/view/~a" db::id)
        (:div :class "project card"
              (:h2 name)
              (when (and *current-activity*
                         (eql project (activity-project *current-activity*)))
                (:p "Currently working on "
                    (:span :class "primary-color" (activity-name *current-activity*))))
              (:p description)
              ))))

(defview nav ()
  (:div :class "nav"
        (:a :href "/" "Dashboard")
        " "
        (:a :href "/config" "Config")))


(defpage dashboard () (:title "Dashboard"
                       :stylesheets ("/css/main.css"))
  (view/nav)
  (:div
   :class "main-content"
   (:h1 "Dashboard")
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
  (with-slots (db::id name estimate currently-working-p status category log) activity 
    (:div
     :class (if currently-working-p "card lighter" "card")
     (if currently-working-p 
         (:a :class "button"
             :href (format nil  "/activity/clock-out/~a" db::id)
             "Clock Out")
         (:a :class "button"
             :href (format nil  "/activity/clock-in/~a" db::id)
             "Clock In"))
     (:ul
      :class "unstyled"
      (:li (:h3  name))
      (:li (:strong "Category: ") category)
      (:li (:strong "Status: ") status)
      (:li (:strong "Estimated: ") estimate " Hours")
      (:li (:strong "Hours Logged: ")
           (if log
               (apply #'format nil "~2,'0d:~2,'0d"
                      (hours-minutes (seconds-worked activity)))
               "00:00"))))))

(defview new-activity-form (project)
  (:div
   (:form
    :method "POST"
    :action (format nil "/activity/add-to-project/~a" (db:store-object-id project))
    (:input :name "name" :placeholder "Name" :class "form-input")
    (:label :for "estimate" "Hours")
    (:input :name "estimate" :type "number" :min "0" :step "0.1"
            :class "form-input")
    (:label :for "status" "Status")
    (:select :name "status" :style "min-width:50%" :class "form-input"      
      (dolist (status (statuses (get-config)))
        (:option :value status status)))
    (:label :for "category" "Category")
    (:select :name "category" :style "min-width:50%" :class "form-input"
      (dolist (category (categories (get-config)))
        (:option :value category category)))
    (:button :class "button" :type "submit" "Create Activity"))))

(defpage project (project)
    (:title "Project View"
     :stylesheets ("/css/main.css"))
  (view/nav)
  (:div
   :class "main-content"
   (:h1 (project-name project))
   (:p (project-description project))

   (:div
    :class "main-grid"
    (view/new-activity-form project)
    (dolist (activity (activities-by-project project))
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
         (:button :type "submit" :class "button"   "Add Status"))
  (:script
   (ps:ps
     (let ((input (ps:chain document (get-element-by-id "new-status-input"))))
       (setf (ps:chain input oninvalid)
             (lambda (event)
               (setf (ps:chain  disabled) true)))))))

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
                     :status (make-keyword  (getf *body* :status))
                     :category (getf *body* :category))))
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

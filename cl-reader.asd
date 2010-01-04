;;;; 2010-01-03 13:52:11
;;;;
;;;; Think of this as your project file.
;;;; Keep it up to date, and you can reload your project easily
;;;;  by right-clicking on it and selecting "Load Project"

(defsystem cl-reader
  :name "cl-reader"
  :version "0.1"
  :serial t
  :components ((:file "package")
               (:file "utils") 
               (:file "conditions")
               (:file "readtable")
               (:file "reader")
               (:file "backq")
               (:file "sharpm")
               )
  :depends-on ())

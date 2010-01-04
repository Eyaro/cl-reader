;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package :cl-reader)

(declaim (special *read-suppress* *cl-standard-readtable* *bq-vector-flag*))

;;; FIXME: Is it standard to ignore numeric args instead of raising errors?
(defun ignore-numarg (sub-char numarg)
  (when numarg
    (warn "A numeric argument was ignored in #~W~A." numarg sub-char)))

;;;; reading arrays and vectors: the #(, #*, and #A readmacros

(defun sharp-left-paren (stream ignore length)
  (declare (ignore ignore) (special *backquote-count*))
  (let* ((list (read-list stream nil))
         (listlength (handler-case (length list)
                                   (type-error
                                     (error)
                                     (declare (ignore error))
                                     (simple-reader-error stream
                                                          "improper list in #(): ~S"
                                                          list)))))
    (declare (list list)
             (fixnum listlength))
    (cond (*read-suppress* nil)
      ((zerop *backquote-count*)
       (if length
           (cond ((> listlength (the fixnum length))
                  (simple-reader-error
                   stream
                   "vector longer than specified length: #~S~S"
                   length list))
             (t
              (fill (the simple-vector
                         (replace (the simple-vector
                                       (make-array length))
                                  list))
                    (car (last list))
                    :start listlength)))
           (coerce list 'vector)))
      (t (cons *bq-vector-flag* list)))))

(defun sharp-star (stream ignore numarg)
  (declare (ignore ignore))
  (multiple-value-bind (bstring escape-appearedp) (read-extended-token stream)
    (declare (simple-string bstring))
    (cond (*read-suppress* nil)
      (escape-appearedp
       (simple-reader-error stream
                            "An escape character appeared after #*."))
      ((and numarg (zerop (length bstring)) (not (zerop numarg)))
       (simple-reader-error
        stream
        "You have to give a little bit for non-zero #* bit-vectors."))
      ((or (null numarg) (>= (the fixnum numarg) (length bstring)))
       (let* ((len1 (length bstring))
              (last1 (1- len1))
              (len2 (or numarg len1))
              (bvec (make-array len2 :element-type 'bit
                                :initial-element 0)))
         (declare (fixnum len1 last1 len2))
         (do ((i 0 (1+ i))
              (char ()))
             ((= i len2))
           (declare (fixnum i))
           (setq char (elt bstring (if (< i len1) i last1)))
           (setf (elt bvec i)
                 (cond ((char= char #\0) 0)
                   ((char= char #\1) 1)
                   (t
                    (simple-reader-error
                     stream
                     "illegal element given for bit-vector: ~S"
                     char)))))
         bvec))
      (t
       (simple-reader-error
        stream
        "Bit vector is longer than specified length #~A*~A"
        numarg
        bstring)))))

(defun sharp-A (stream ignore dimensions)
  (declare (ignore ignore))
  (when *read-suppress*
    (cl-read stream t nil t)
    (return-from sharp-A nil))
  (unless dimensions (simple-reader-error 
                      stream
                      "no dimensions argument to #A"))
  (collect ((dims))
    (let* ((contents (cl-read stream t nil t))
           (seq contents))
      (dotimes (axis dimensions
                     (make-array (dims) :initial-contents contents))
        (unless (typep seq 'sequence)
          (simple-reader-error stream
                               "#~WA axis ~W is not a sequence:~%  ~S"
                               dimensions axis seq))
        (let ((len (length seq)))
          (dims len)
          (unless (or (= axis (1- dimensions))
                      ;; ANSI: "If some dimension of the array whose
                      ;; representation is being parsed is found to be
                      ;; 0, all dimensions to the right (i.e., the
                      ;; higher numbered dimensions) are also
                      ;; considered to be 0."
                      (= len 0))
            (setq seq (elt seq 0))))))))

;;TODO: sharp-S
;;;; reading structure instances: the #S readmacro
#|
(defun sharp-S (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (when *read-suppress*
    (cl-read stream t nil t)
    (return-from sharp-S nil))
  (let ((body (if (char= (read-char stream t) #\( )
                  (read-list stream nil)
                  (simple-reader-error stream "non-list following #S"))))
    (unless (listp body)
      (simple-reader-error stream "non-list following #S: ~S" body))
    (unless (symbolp (car body))
      (simple-reader-error stream
                           "Structure type is not a symbol: ~S"
                           (car body)))
    (let ((classoid (find-classoid (car body) nil)))
      (unless (typep classoid 'structure-classoid)
        (simple-reader-error stream
                             "~S is not a defined structure type."
                             (car body)))
      (let ((default-constructor (dd-default-constructor
                                  (layout-info (classoid-layout classoid)))))
        (unless default-constructor
          (simple-reader-error
           stream
           "The ~S structure does not have a default constructor."
           (car body)))
        (when (and (atom (rest body))
                   (not (null (rest body))))
          (simple-reader-error stream "improper list for #S: ~S." body))
        (apply (fdefinition default-constructor)
               (loop for tail on (rest body) by #'cddr
                     with slot-name = (and (consp tail) (car tail))
                     do (progn
                          (when (null (cdr tail))
                            (simple-reader-error
                             stream
                             "the arglist for the ~S constructor in #S ~
                             has an odd length: ~S."
                             (car body) (rest body)))
                          (when (or (atom (cdr tail))
                                    (and (atom (cddr tail))
                                         (not (null (cddr tail)))))
                            (simple-reader-error
                             stream
                             "the arglist for the ~S constructor in #S ~
                             is improper: ~S."
                             (car body) (rest body)))
                          (when (not (typep (car tail) 'string-designator))
                            (simple-reader-error
                             stream
                             "a slot name in #S is not a string ~
                             designator: ~S."
                             slot-name))
                          (when (not (keywordp slot-name))
                            (warn 'structure-initarg-not-keyword
                                  :format-control
                                  "in #S ~S, the use of non-keywords ~
                                  as slot specifiers is deprecated: ~S."
                                  :format-arguments
                                  (list (car body) slot-name))))
                     collect (intern (string (car tail)) *keyword-package*)
                     collect (cadr tail)))))))

|#
;;;; reading numbers: the #B, #C, #O, #R, and #X readmacros
(defun sharp-R (stream sub-char radix)
  (cond (*read-suppress*
         (read-extended-token stream)
         nil)
    ((not radix)
     (simple-reader-error stream "radix missing in #R"))
    ((not (<= 2 radix 36))
     (simple-reader-error stream "illegal radix for #R: ~D." radix))
    (t
     (let ((res (let ((*read-base* radix))
                  (cl-read stream t nil t))))
       (unless (typep res 'rational)
         (simple-reader-error 
          stream
          "#~A (base ~D.) value is not a rational: ~S."
          sub-char
          radix
          res))
       res))))

(defun sharp-B (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (sharp-R stream sub-char 2))

(defun sharp-C (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  ;; The next thing had better be a list of two numbers.
  (let ((cnum (cl-read stream t nil t)))
    (when *read-suppress* (return-from sharp-C nil))
    (if (and (listp cnum) (= (length cnum) 2))
        (complex (car cnum) (cadr cnum))
        (simple-reader-error stream
                             "illegal complex number format: #C~S"
                             cnum))))

(defun sharp-O (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (sharp-R stream sub-char 8))


(defun sharp-X (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (sharp-R stream sub-char 16))

;;;; reading circular data: the #= and ## readmacros

;;; objects already seen by CIRCLE-SUBST
(defvar *sharp-equal-circle-table*)
(declaim (type hash-table *sharp-equal-circle-table*))

;; This function is kind of like NSUBLIS, but checks for circularities and
;; substitutes in arrays and structures as well as lists. The first arg is an
;; alist of the things to be replaced assoc'd with the things to replace them.
(defun circle-subst (old-new-alist tree)
  (cond ((not (typep tree '(or cons (array t) ;instance funcallable-instance
                               )))
         (let ((entry (find tree old-new-alist :key #'second)))
           (if entry (third entry) tree)))
    ((null (gethash tree *sharp-equal-circle-table*))
     (setf (gethash tree *sharp-equal-circle-table*) t)
     (cond ((consp tree)
            (let ((a (circle-subst old-new-alist (car tree)))
                  (d (circle-subst old-new-alist (cdr tree))))
              (unless (eq a (car tree))
                (rplaca tree a))
              (unless (eq d (cdr tree))
                (rplacd tree d))))
       ((arrayp tree)
        (with-array-data ((data tree) (start) (end))
          (declare (fixnum start end))
          (do ((i start (1+ i)))
              ((>= i end))
            (let* ((old (aref data i))
                   (new (circle-subst old-new-alist old)))
              (unless (eq old new)
                (setf (aref data i) new))))))
       ;;TODO: what are these for???
       #|((typep tree 'instance)
       (let* ((n-untagged (layout-n-untagged-slots (%instance-layout tree)))
       (n-tagged (- (%instance-length tree) n-untagged)))
       ;; N-TAGGED includes the layout as well (at index 0), which
       ;; we don't grovel.
       (do ((i 1 (1+ i)))
       ((= i n-tagged))
       (let* ((old (%instance-ref tree i))
       (new (circle-subst old-new-alist old)))
       (unless (eq old new)
       (setf (%instance-ref tree i) new))))
       (do ((i 0 (1+ i)))
       ((= i n-untagged))
       (let* ((old (%raw-instance-ref/word tree i))
       (new (circle-subst old-new-alist old)))
       (unless (= old new)
       (setf (%raw-instance-ref/word tree i) new))))))
       ((typep tree 'funcallable-instance)
       (do ((i 1 (1+ i))
       (end (- (1+ (get-closure-length tree)) sb!vm:funcallable-instance-info-offset)))
       ((= i end))
       (let* ((old (%funcallable-instance-info tree i))
       (new (circle-subst old-new-alist old)))
       (unless (eq old new)
       (setf (%funcallable-instance-info tree i) new)))))|#
       
       
       )
     tree)
    (t tree)))

;;; Sharp-equal works as follows. When a label is assigned (i.e. when
;;; #= is called) we GENSYM a symbol is which is used as an
;;; unforgeable tag. *SHARP-SHARP-ALIST* maps the integer tag to this
;;; gensym.
;;;
;;; When SHARP-SHARP encounters a reference to a label, it returns the
;;; symbol assoc'd with the label. Resolution of the reference is
;;; deferred until the cl-read done by #= finishes. Any already resolved
;;; tags (in *SHARP-EQUAL-ALIST*) are simply returned.
;;;
;;; After reading of the #= form is completed, we add an entry to
;;; *SHARP-EQUAL-ALIST* that maps the gensym tag to the resolved
;;; object. Then for each entry in the *SHARP-SHARP-ALIST, the current
;;; object is searched and any uses of the gensysm token are replaced
;;; with the actual value.
(defvar *sharp-sharp-alist* ())

(defun sharp-equal (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-equal (values)))
  (unless label
    (simple-reader-error stream "missing label for #=" label))
  (when (or (assoc label *sharp-sharp-alist*)
            (assoc label *sharp-equal-alist*))
    (simple-reader-error stream "multiply defined label: #~D=" label))
  (let* ((tag (gensym))
         (*sharp-sharp-alist* (acons label tag *sharp-sharp-alist*))
         (obj (cl-read stream t nil t)))
    (when (eq obj tag)
      (simple-reader-error stream
                           "must tag something more than just #~D#"
                           label))
    (push (list label tag obj) *sharp-equal-alist*)
    (let ((*sharp-equal-circle-table* (make-hash-table :test 'eq :size 20)))
      (circle-subst *sharp-equal-alist* obj))))

(defun sharp-sharp (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-sharp nil))
  (unless label
    (simple-reader-error stream "missing label for ##" label))
  
  (let ((entry (assoc label *sharp-equal-alist*)))
    (if entry
        (third entry)
        (let (;; Has this label been defined previously? (Don't read
              ;; ANSI "2.4.8.15 Sharpsign Equal-Sign" and worry that
              ;; it requires you to implement forward references,
              ;; because forward references are disallowed in
              ;; "2.4.8.16 Sharpsign Sharpsign".)
              (pair (assoc label *sharp-sharp-alist*)))
          (unless pair
            (simple-reader-error stream
                                 "reference to undefined label #~D#"
                                 label))
          (cdr pair)))))

;;;; conditional compilation: the #+ and #- readmacros

(flet ((guts (stream not-p)
             (unless (if (let ((*package* *keyword-package*)
                               (*read-suppress* nil))
                           (featurep (cl-read stream t nil t)))
                         (not not-p)
                         not-p)
               (let ((*read-suppress* t))
                 (cl-read stream t nil t)))
             (values)))
  
  (defun sharp-plus (stream sub-char numarg)
    (ignore-numarg sub-char numarg)
    (guts stream nil))
  
  (defun sharp-minus (stream sub-char numarg)
    (ignore-numarg sub-char numarg)
    (guts stream t)))

;;;; reading miscellaneous objects: the #P, #\, and #| readmacros

(defun sharp-P (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (let ((namestring (cl-read stream t nil t)))
    (unless *read-suppress*
      (parse-namestring namestring))))

(defun sharp-backslash (stream backslash numarg)
  (ignore-numarg backslash numarg)
  (let ((charstring (read-extended-token-escaped stream)))
    (declare (simple-string charstring))
    (cond (*read-suppress* nil)
      ((= (the fixnum (length charstring)) 1)
       (char charstring 0))
      ((name-char charstring))
      (t
       (simple-reader-error stream
                            "unrecognized character name: ~S"
                            charstring)))))

(defun sharp-vertical-bar (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (handler-bind nil ;;TODO: make this work!!!
                #|  ((character-decoding-error
   #'(lambda (decoding-error)
   (declare (ignorable decoding-error))
   (style-warn
   'sb!kernel::character-decoding-error-in-dispatch-macro-char-comment
   :sub-char sub-char :position (file-position stream) :stream stream)
   (invoke-restart 'attempt-resync))))|#
                (let ((stream (in-synonym-of stream))) 
                  ;; fundamental-stream
                  (do ((level 1)
                       (prev (read-char stream t) char)
                       (char (read-char stream t) (read-char stream t)))
                      (())
                    (cond ((and (char= prev #\|) (char= char #\#))
                           (setq level (1- level))
                           (when (zerop level)
                             (return (values)))
                           (setq char (read-char stream t)))
                      ((and (char= prev #\#) (char= char #\|))
                       (setq char (read-char stream t))
                       (setq level (1+ level))))))))

;;;; a grab bag of other sharp readmacros: #', #:, and #.

(defun sharp-quote (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  ;; The fourth arg tells READ that this is a recursive call.
  `(function ,(cl-read stream t nil t)))

(defun sharp-colon (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (multiple-value-bind (token escapep colon) (read-extended-token stream)
    (declare (simple-string token) (ignore escapep))
    (cond
      (*read-suppress* nil)
      (colon
       (simple-reader-error
        stream "The symbol following #: contains a package marker: ~S" token))
      (t
       (make-symbol token)))))

(defvar *read-eval* t
  "If false, then the #. read macro is disabled.")

(defun sharp-dot (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (let ((token (cl-read stream t nil t)))
    (unless *read-suppress*
      (unless *read-eval*
        (simple-reader-error stream "can't read #. while *READ-EVAL* is NIL"))
      (eval token))))

(defun sharp-illegal (stream sub-char ignore)
  (declare (ignore ignore))
  (simple-reader-error stream "illegal sharp macro character: ~S" sub-char))

;;; for cold init: Install SHARPM stuff in the current *READTABLE*.
(defun !sharpm-cold-init ()
  (cl-make-dispatch-macro-character #\# t)
  (cl-set-dispatch-macro-character #\# #\\ #'sharp-backslash)
  (cl-set-dispatch-macro-character #\# #\' #'sharp-quote)
  (cl-set-dispatch-macro-character #\# #\( #'sharp-left-paren)
  (cl-set-dispatch-macro-character #\# #\* #'sharp-star)
  (cl-set-dispatch-macro-character #\# #\: #'sharp-colon)
  (cl-set-dispatch-macro-character #\# #\. #'sharp-dot)
  (cl-set-dispatch-macro-character #\# #\R #'sharp-R)
  (cl-set-dispatch-macro-character #\# #\r #'sharp-R)
  (cl-set-dispatch-macro-character #\# #\B #'sharp-B)
  (cl-set-dispatch-macro-character #\# #\b #'sharp-B)
  (cl-set-dispatch-macro-character #\# #\O #'sharp-O)
  (cl-set-dispatch-macro-character #\# #\o #'sharp-O)
  (cl-set-dispatch-macro-character #\# #\X #'sharp-X)
  (cl-set-dispatch-macro-character #\# #\x #'sharp-X)
  (cl-set-dispatch-macro-character #\# #\A #'sharp-A)
  (cl-set-dispatch-macro-character #\# #\a #'sharp-A)
  ;;TODO: Sharp-s
  ;(cl-set-dispatch-macro-character #\# #\S #'sharp-S)
  ;(cl-set-dispatch-macro-character #\# #\s #'sharp-S)
  (cl-set-dispatch-macro-character #\# #\= #'sharp-equal)
  (cl-set-dispatch-macro-character #\# #\# #'sharp-sharp)
  (cl-set-dispatch-macro-character #\# #\+ #'sharp-plus)
  (cl-set-dispatch-macro-character #\# #\- #'sharp-minus)
  (cl-set-dispatch-macro-character #\# #\C #'sharp-C)
  (cl-set-dispatch-macro-character #\# #\c #'sharp-C)
  (cl-set-dispatch-macro-character #\# #\| #'sharp-vertical-bar)
  (cl-set-dispatch-macro-character #\# #\p #'sharp-P)
  (cl-set-dispatch-macro-character #\# #\P #'sharp-P)
  (cl-set-dispatch-macro-character #\# #\) #'sharp-illegal)
  (cl-set-dispatch-macro-character #\# #\< #'sharp-illegal)
  (cl-set-dispatch-macro-character #\# #\Space #'sharp-illegal)
  (dolist (cc '#.(list (char-code #\Tab) 
                       (char-code #\Page)  ;;page==form feed
                       (char-code #\Return)
                       (char-code #\Newline) ;;line-feed-char-code == Newline 
                       (char-code #\Backspace)))
    (cl-set-dispatch-macro-character #\# (code-char cc) #'sharp-illegal)))

(!sharpm-cold-init)


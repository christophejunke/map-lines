(defpackage :map-lines
  (:use :cl :alexandria)
  #+sbcl
  (:import-from :sb-ext
                sb-ext:muffle-conditions
                sb-ext:compiler-note)
  (:import-from :asdf
                asdf:system-relative-pathname)
  (:export #:map-lines-on-shared-buffer
           #:truncate
           #:split
           #:*max-line-size*
           #:*default-buffer-size*
           #:internal-buffer-overflow))

(in-package :map-lines)

(declaim (type array-total-size
               *default-buffer-size*
               *max-line-size*))

(defparameter *max-line-size* 2048
  "Default longest line allowed when extending the input buffer")

(defparameter *default-buffer-size* 256
  "Default buffer size")

(define-condition internal-buffer-overflow (error)
  ((size
    :initarg :size
    :accessor size))
  (:report (lambda (condition stream)
             (format stream
                     "Parsing buffer overflow (current size is ~d)."
                     (size condition)))))

(defun report-truncate (stream separator)
  (format stream
          "Callback with :TRUNCATED status and continue after the next ~@c."
          separator))

(defun report-split (stream)
  (format stream "Callback with :PART status and continue processing."))

(declaim (ftype (function (stream
                           function
                           &key
                           (:separator (or character unsigned-byte))
                           (:buffer (or null
                                        (vector (or character (unsigned-byte 8)))))
                           (:buffer-size  array-total-size)
                           (:max-line-size array-total-size)
                           (:extendp t)
                           (:on-overflow (member :error :truncate :split))
                           (:partialp t))
                          (values &optional))
                map-lines-on-shared-buffer))

(defun map-lines-on-shared-buffer
    (stream function
     &key
       (separator #\newline)
       (buffer)
       (buffer-size *default-buffer-size*)
       (extendp t extendp-p)
       (max-line-size *max-line-size*)
       (on-overflow :error)
       (partialp nil))
  #+profile
  (declare (OPTIMIZE SB-C::INSTRUMENT-CONSING))
  (assert (plusp max-line-size))
  (assert (plusp (if buffer (length buffer) buffer-size)))
  (when (and partialp extendp)
    (unless extendp-p
      (warn "Ignoring non-nil :EXTENDP when :PARTIALP is T"))
    (setf extendp nil))
  (let* ((buffer-size (if buffer (length buffer) buffer-size))
         (growth-function (etypecase extendp
                            (function extendp)
                            ((eql t)
                             (locally (declare
                                       #+sbcl
                                       (muffle-conditions compiler-note))
                               (lambda (x)
                                 (declare (type array-total-size x))
                                 (* x 2))))
                            ((eql nil)
                             (lambda (x)
                               (declare (ignore x))
                               (error "Unexpected call to grown")))))
         (element-type (stream-element-type stream))
         (peek-function (if (subtypep element-type 'character)
                            (lambda () (peek-char separator stream nil nil))
                            (lambda ()
                              (loop
                                 for byte = (read-byte stream nil nil)
                                 thereis (eql byte separator)))))
         (reader-function (if (subtypep element-type 'character)
                              (lambda () (read-char stream))
                              ;; no need to read one more byte here
                              (lambda ())))
         (buffer (or buffer (make-array buffer-size
                                        :element-type element-type
                                        :adjustable extendp)))
         (view (make-array 0
                           :displaced-to buffer
                           :displaced-index-offset 0
                           :element-type element-type
                           :adjustable t))
         (buffer-start 0) ; where to start filling the buffer from stream
         (buffer-end 0)   ; buffer position after reading new elements
         (line-start 0) ; start of current line within buffer (w.r.t. #\newline)
         (line-end 0))  ; end of current line within buffer
    (declare (type function function)
             (type (or character (unsigned-byte 8)) separator)
             (type array-total-size
                   buffer-size
                   buffer-end
                   buffer-start
                   line-start
                   line-end
                   max-line-size)
             (type vector buffer view)
             (dynamic-extent buffer-end buffer-start line-start line-end)
             (optimize (space 3) (speed 3)))
    (flet ((peek () (funcall peek-function))
           (read-element () (funcall reader-function))
           (grow (size)
             (declare #+sbcl (muffle-conditions compiler-note))
             (let ((result (funcall growth-function size)))
               (assert (> result size))
               (min max-line-size result)))
           (callback (line-start line-end &optional status)
             (funcall function
                      (adjust-array view
                                    (- line-end line-start)
                                    :element-type element-type
                                    :displaced-to buffer
                                    :displaced-index-offset line-start)
                      :status status)))
      (declare (inline callback grow))
      (block nil
        (tagbody

         :buffer
           ;; Fill buffer with characters from the stream. When
           ;; nothing is read, we can exit the state machine.
           (setf buffer-end (read-sequence buffer stream :start buffer-start))
           
           (when (= buffer-end buffer-start)
             ;; It is however still possible that there are are characters left
             ;; between position zero and buffer-start: if so, process that
             ;; region with the callback function.
             (when (plusp buffer-start)
               (assert (not (position separator buffer :end buffer-start)))
               (callback 0 buffer-start))
             (return))

         :next-line

           ;; Try to find the line separator in current buffer. In case it is
           ;; not found, there are different possibilities:
           ;;
           ;; 1. The line is too long to fit the buffer and we may need to read
           ;;    more data or eventually handle an overflow. The first step in
           ;;    that case consists in shifting all data prior to line-start to
           ;;    offset zero, so as to have more space towards the end of the
           ;;    buffer; if that fails, the buffer is extended or the line sent
           ;;    back truncated or split.
           ;;
           ;; 2. The stream ended without a line separator, which means the
           ;;    buffer contains the entirety of what was available from the
           ;;    stream; either buffer-end is below the end of the buffer, in
           ;;    which case we can directly process the line, or buffer-end is
           ;;    exactly at the end of the buffer. This case is
           ;;    indistinguishable from case 1. above; we could proceed
           ;;    directly as explained in part 1 (shifting left, etc.) but
           ;;    before doing that we can try to see if the stream still
           ;;    contain inputs to read.

           (when-let (separator-position (position separator
                                                   buffer
                                                   :test #'eql
                                                   :start line-start
                                                   :end buffer-end))
             ;; separator found
             (setf line-end separator-position)
             (callback line-start line-end)
             (setf line-start (1+ line-end))
             (go :next-line))

           (when (or (< buffer-end (length buffer))
                     (not (listen stream)))
             ;; no separator, end of file: see above comment
             (callback line-start buffer-end)
             (return))

           (when partialp
             ;; just emit this line and continue
             (callback line-start buffer-end :part)
             (setf line-start 0)
             (setf buffer-start 0)
             (go :buffer))

           ;; fallthrough
           
         :copy
           ;; not enough room: copy the latest line fragment (the one being
           ;; currently read) at the beginning of the buffer. If we already
           ;; are at the beginning (ZEROP LINE-START), then we may try to
           ;; extend to buffer instead or cope with an overflow.
           (when (zerop line-start)
             (if extendp
                 (go :extend)
                 (go :handle-overflow)))

           (locally (declare #+sbcl (muffle-conditions compiler-note))
             (replace buffer buffer :start2 line-start :end2 buffer-end))
           (setf buffer-start (- buffer-end line-start))
           (setf line-start 0)

           ;; We shifted the current line on the left, because we did
           ;; not yet find a newline. Read more characters.
           (go :buffer)

         :extend
           ;; Try to extend the array so that the entire line fits.
           (let ((size (array-total-size buffer)))
             (when (>= size max-line-size)
               (go :handle-overflow))

             (setf buffer-start size)
             (setf buffer (adjust-array buffer (grow size)))
             ;; buffer is extended, go read more input
             (go :buffer))

         :handle-overflow
           (ecase on-overflow
             (:error
              (restart-case (error 'internal-buffer-overflow
                                   :buffer buffer
                                   :size (array-total-size buffer))
                (extend (amount)
                  :report "Extend buffer size by AMOUNT."
                  :interactive
                  (lambda (&aux number)
                    (loop
                       (clear-input *query-io*)
                       (format *query-io*
                               "~&Enter positive amount ~
                                 (how much buffer should be extended): ")
                       (force-output *query-io*)
                       (setq number (read *query-io*))
                       (when (typep number '(and fixnum
                                             (integer 1)))
                         (return (list number)))))
                  (declare (type (and fixnum (integer 1)) amount))
                  (let* ((size (array-total-size buffer))
                         (new (if (> size (- array-total-size-limit 1 amount))
                                  (1- array-total-size-limit)
                                  (+ size amount))))
                    (declare (type array-total-size size new))
                    (assert (not (= new size)))
                    (setf buffer (adjust-array buffer new))
                    (setf buffer-start size))
                  (go :buffer))
                (split ()
                  :report report-split
                  (go :split))
                (truncate ()
                  :report (lambda (stream) (report-truncate stream separator))
                  (go :truncate))))
             (:split (go :split))
             (:truncate (go :truncate)))

         :split
           (callback line-start buffer-end :part)
           (setf buffer-start 0)
           (go :buffer)

         :truncate
           (callback line-start buffer-end :truncated)
           (setf line-start 0)
           (setf buffer-start 0)
           (and (peek) (read-element))
           (go :buffer)))
      (values))))

;; (map-lines-on-shared-buffer (make-string-input-stream "abcdefgh")
;;                             (compose #'print #'list)
;;                             :buffer-size 4
;;                             :extendp nil)

(with-output-to-file (out "/tmp/test" :if-exists :supersede :element-type 'character)
  (dotimes (j 6000)
    (dotimes (i (+ 1500 (random 200)))
      (write-char (code-char (+ #.(char-code #\A)
                                (random 50)))
                  out))
    (terpri out)))

(with-input-from-file (in "/tmp/test" :element-type '(unsigned-byte 8))
  (let ((max 0) ;; (buffer (make-array 2048 :element-type 'character))
        )
    (time
     (map-lines-on-shared-buffer in
                                 (lambda (line &key &allow-other-keys)
                                   (setf max (max max (length line))))
                                 :separator 10
                                 :buffer-size 2048
                                 :extendp nil))
    max))

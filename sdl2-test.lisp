;;;; sdl2-test.lisp

(in-package #:sdl2-test)

(defparameter *window* nil)



;;; shader utilities

(defun load-shader (path)
  "Load a shader given a path. Shader type is guessed from filename."
  (let* ((ext (subseq path (- (length path) 4)))
         (type (cond ((equal ext "vert") :vertex-shader)
                     ((equal ext "frag") :fragment-shader)
                     ((equal ext "geom") :geometry-shader))))
    (with-open-file (stream path)
      (let ((string (make-string (file-length stream)))
            (shader (gl:create-shader type)))
        (read-sequence string stream)
        (gl:shader-source shader string)
        (gl:compile-shader shader)
        (unless (gl:get-shader shader :compile-status)
          (error (format nil "Failed to compile shader '~a'." path)))
        shader))))

(defun load-program (&rest args)
  "Link a program given shader paths. Shader types are guessed from filenames."
  (let ((shaders (mapcar #'load-shader args)))
    (unwind-protect
         (let ((program (gl:create-program)))
           (dolist (shader shaders)
             (gl:attach-shader program shader))
           (gl:link-program program)
           program)
      (dolist (shader shaders)
        (gl:delete-shader shader)))))

(defun bind-vertex-attribs (program struct slot-vars)
  "Helper for binding VAO vertex attributes. `struct' is a cffi struct, usually
defined with cffi:defstruct and named as `(:struct struct-name). `slot-vars' is
a list of `(slot var)' cons cells where `slot' is the symbol naming the slot in
the struct and `var' is the string name of the variable in the program."
  (dolist (slot-var slot-vars)
    (destructuring-bind (slot var) slot-var
      (let ((location (gl:get-attrib-location program var)))
        (when (< -1 location)
          (gl:vertex-attrib-pointer location
                                    (cffi:foreign-slot-count struct slot)
                                    (cffi:foreign-slot-type struct slot)
                                    nil
                                    (cffi:foreign-type-size struct)
                                    (cffi:foreign-slot-offset struct slot))
          (gl:enable-vertex-attrib-array location))))))

(defmacro with-cstruct-slots ((vars ptr type) &body body)
  "Create local symbol macros for each var in VARS to reference
foreign slots in PTR of TYPE. Similar to WITH-SLOTS.
Each var can be of the form: slot-name - in which case slot-name will
be bound to the value of the slot or: (:pointer slot-name) - in which
case slot-name will be bound to the pointer to that slot. This is like
cffi:with-foreign-slots except it calculates slot offsets at compile time."
  (let ((ptr-var (gensym "PTR")))
    `(let ((,ptr-var ,ptr))
       (symbol-macrolet
           ,(loop :for var :in vars
               :collect
               (if (listp var)
                   (if (eq (first var) :pointer)
                       `(,(second var) (cffi:inc-pointer
                                        ,ptr-var
                                        ,(cffi:foreign-slot-offset
                                          type (second var))))
                       (error
                        "Malformed slot specification ~a"
                        var))
                   `(,var (cffi:mem-ref
                           (cffi:inc-pointer ,ptr-var
                                             ,(cffi:foreign-slot-offset
                                               type var))
                           ,(cffi:foreign-slot-type type var)))))
         ,@body))))



;;; sprite layout

(cffi:defcstruct sprite
  (pos :float :count 2)
  (cell :float :count 2)
  (size :float :count 2))



;;; init/deinit

(defparameter *program* nil)
(defparameter *vao* nil)
(defparameter *vbo* nil)
(defparameter *vbo-map* nil)
(defparameter *num-sprites* 2000)

(defun init ()
  ;; load shaders, program
  (setf *program* (load-program "sprite.vert" "sprite.geom" "sprite.frag"))
  (gl:use-program *program*)

  ;; make vao, vbo, bind attributes
  (setf *vao* (gl:gen-vertex-array))
  (gl:bind-vertex-array *vao*)
  (setf *vbo* (car (gl:gen-buffers 1)))
  (gl:bind-buffer :array-buffer *vbo*)
  (bind-vertex-attribs *program*
                       '(:struct sprite)
                       '((pos "pos") (cell "cell") (size "size")))

  ;; initialize empty buffer data store, map buffer
  (%gl:buffer-data :array-buffer
                   (* *num-sprites* (cffi:foreign-type-size '(:struct sprite)))
                   (cffi:null-pointer)
                   :stream-draw)
  (setf *vbo-map* (gl:map-buffer :array-buffer :write-only)))

(defun deinit ()
  ;; cleanup GL stuff
  (when *vbo* (gl:delete-buffers (list *vbo*)))
  (when *vao* (gl:delete-vertex-arrays (list *vao*)))
  (when *program* (gl:delete-program *program*)))



;;; draw

(defun draw ()
  (gl:clear :color-buffer)

  (gl:use-program *program*)
  (gl:bind-vertex-array *vao*)
  (gl:bind-buffer :array-buffer *vbo*)
  (dotimes (i *num-sprites*)
    (with-cstruct-slots (((:pointer pos) (:pointer cell) (:pointer size))
                         (cffi:mem-aptr *vbo-map* '(:struct sprite) i)
                         (:struct sprite))
      (setf (cffi:mem-aref pos :float 0) (random 5.0))
      (setf (cffi:mem-aref pos :float 1) (random 5.0))
      (setf (cffi:mem-aref cell :float 0) 0.0)
      (setf (cffi:mem-aref cell :float 1) 0.0)
      (setf (cffi:mem-aref size :float 0) 10.0)
      (setf (cffi:mem-aref size :float 1) 10.0)))
  (gl:draw-arrays :points 0 *num-sprites*)

  (gl:flush)
  (sdl2:gl-swap-window *window*))



;;; main

(let ((nframes 0) (last-fps 0))
  (defun update-fps (period dt)
    (incf nframes)
    (incf last-fps dt)
    (when (<= period last-fps)
      (format t "~&fps: ~a~%" (/ nframes last-fps))
      (finish-output nil)
      (setf nframes 0 last-fps 0))))

(let ((last-time))
  (defun update ()
    (let ((curr-time (get-internal-real-time)))
      (when last-time
        (let ((dt (coerce (/ (- curr-time last-time)
                             internal-time-units-per-second) 'float)))
          (update-fps 5 dt)))
      (setf last-time curr-time))))

(defmacro continuable (&body body)
  "Allow continuing execution from errors."
  `(restart-case (progn ,@body)
     (continue () :report "Continue")))

(defun update-swank ()
  "Handle REPL requests."
  (continuable
    (let ((connection (or swank::*emacs-connection*
                          (swank::default-connection))))
      (when connection
        (swank::handle-requests connection t)))))

(defmacro with-main (&body body)
  `(sdl2:make-this-thread-main
    (lambda ()
      (sb-int:with-float-traps-masked (:invalid)
        ,@body))))

(defun main (&optional *num-sprites*)
  (sdl2:with-init (:everything)
    ;; use core profile
    (sdl2:gl-set-attr :context-major-version 3)
    (sdl2:gl-set-attr :context-minor-version 2)
    (sdl2:gl-set-attr :context-profile-mask
                      sdl2-ffi::+SDL-GL-CONTEXT-PROFILE-CORE+)

    ;; translate gl function pointers
    (setf cl-opengl-bindings::*gl-get-proc-address* #'sdl2::gl-get-proc-address)

    ;; create window, opengl context
    (sdl2:with-window (*window* :flags '(:shown :opengl))
      (sdl2:set-window-position *window* 634 53)
      (sdl2:with-gl-context (gl-context *window*)
        (init)
        (sdl2:with-event-loop (:method :poll)
          (:idle ()
                 (update-swank)
                 (continuable
                   (update)
                   (draw)))
          (:quit ()
                 (deinit)
                 t))))))



(in-package #:stumpwm)

(defstruct key
  keysym shift control meta alt hyper super)

(defstruct binding
  (key nil :type key)
  (command nil :type stumpwm-cmd-specifier))

(defstruct kmap
  (bindings nil :type list))

(defstruct key-state
  (sequence nil :type list)
  (kmap nil :type (or null kmap)))

(declaim (type key-state *key-state*))
(defparameter *key-state* (make-key-state))

(defun find-binding (kmaps key)
  (declare (type list kmaps) (type key key)
           (optimize (speed 3) (safety 1)))
  (dolist (map kmaps)
    (if-let (value (lookup-key (dereference-kmap map) key))
      (return value))))

(defun reset-key-state (state grab)
  (declare (type key-state state))
  (setf (key-state-kmap state) nil
        (key-state-sequence state) nil)
  (when grab
    (ungrab-pointer)))

(defun handle-key (code state key-state grab update-fn)
  (declare (type key-state key-state)
           (type function update-fn)
           (optimize (speed 3) (safety 0)))
  (let* ((key (code-state->key code state))
         (key-seq (cons key (key-state-sequence key-state))))
    (if (key-state-sequence key-state)
        ;; prior state:
        (let ((match (lookup-key (key-state-kmap state) key)))
          (run-hook-with-args *key-press-hook* key key-seq match)
          (when update-fn
            (funcall update-fn key-seq))
          (cond
            (match
                (typecase match
                  (string ;; we have a command, do something and reset:
                   (message "running command ~s" match)
                   (reset-key-state key-state grab))
                  (otherwise ;; we have a keymap, advance the state:
                   (setf (key-state-kmap key-state) (dereference-kmap match)
                         (key-state-sequence key-state) key-seq))))
            ;; We failed to find a command, show an error message:
            (t
             (reset-key-state key-state grab)
             (message "~{~a ~}not bound." (mapcar 'print-key (nreverse key-seq))))))
        ;; no prior state:
        (let ((match (find-binding (top-maps) key)))
          (run-hook-with-args *key-press-hook* key key-seq match)
          (when update-fn
            (funcall update-fn key-seq))
          (when match
            (typecase match
              (string ;; we have a command, do something:
               (message "running command ~s" match))
              (otherwise ;; we have a keymap, advance the state:
               (when grab (grab-pointer (current-screen)))
               (setf (key-state-kmap key-state) (dereference-kmap match)
                     (key-state-sequence key-state) key-seq))))))))

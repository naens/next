(in-package :next)

(defun history-as-list (node active-node)
  (let ((data (node-data node))
        (active (if (eq node active-node) 'active 'non-active))
        (children (mapcar (lambda (child)
                            (history-as-list child active-node))
                          (node-children node))))
    (list data active children)))

(defun get-root-node (buffer)
  (let ((node (active-history-node (mode buffer))))
    (loop
       for n = node then p
       for p = (node-parent node) then (node-parent p)
       until (null p)
       finally (return n))))

(define-command print-history ()
  "Print the history"
  (format *standard-output* "~&~%Print history:~%")
  (let* ((active-node (active-history-node (mode (active-buffer *interface*))))
         (root-node (get-root-node (active-buffer *interface*))))
    (format *standard-output*
            "~&active node: ~a~%~s~%"
            (node-data active-node)
            (history-as-list root-node active-node))
    (finish-output)))

(define-key *global-map* (key "M-p") #'print-history)

(define-command session-save ()
  "Save the session."
  (with-open-file (stream "/tmp/session.txt"
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (loop for key being the hash-keys
       in (buffers *interface*)
       using (hash-value buffer)
       do (let ((root-node (get-root-node buffer))
                (active-node (active-history-node (mode buffer))))
            (format stream "~&~s~%" (history-as-list root-node active-node))))
    (format *standard-output* "~&Session saved.")
    (finish-output)))

(define-key *global-map* (key "M-s") #'session-save)


;; returns (values <node> <active>)
(defun node-from-s-exp (s-exp parent)
  (let ((node (make-instance 'node :parent parent :data (first s-exp))))
    (multiple-value-bind (children active)
        (node-from-s-exp-list (third s-exp) node nil nil)
      (setf (node-children node) children)
      (values node
              (or active
                  (and (equal (second s-exp) 'active) node))))))

;; returns (values <children> <active>)
(defun node-from-s-exp-list (s-exps parent children active)
  (cond ((null s-exps) (values children active))
        (t (multiple-value-bind (child act)
               (node-from-s-exp (first s-exps) parent)
             (node-from-s-exp-list (rest s-exps)
                                   parent
                                   (cons child children)
                                   (or active act))))))


(define-command session-restore ()
  "Restore the session."
  (let ((buffers (alexandria:hash-table-values (buffers *interface*))))
    (with-open-file (stream "/tmp/session.txt")
      (loop for s-exp = (read stream t)
         while s-exp
         do (progn
              (multiple-value-bind (root-node active-node)
                  (node-from-s-exp s-exp nil)
                  (declare (ignore root-node))
                (let ((buffer (make-buffer))
                      (mode (make-instance 'document-mode
                                           :name "Document-Mode"
                                           :keymap *document-mode-map*
                                           :active-node active-node)))
                  (setf (mode buffer) mode)
                  (setf (active-history-node mode) active-node)
                  (set-url-buffer (car s-exp) buffer t)
                  (set-active-buffer *interface* buffer))))))
    (loop for buffer in buffers
       do (buffer-delete *interface* buffer)))
  (format *standard-output* "~&Session restored.")
  (finish-output))

(define-key *global-map* (key "M-r") #'session-restore)

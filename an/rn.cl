#!/usr/bin/sbcl --script

(load "~/.sbclrc")

(defvar *use-port* nil)
(defvar *next-inst-dir* "/home/andrei/projects/next")
(asdf:load-asd (format nil "~a/next.asd" *next-inst-dir*))
(sb-posix:chdir *next-inst-dir*)

(require :asdf)
(ql:quickload :bt-semaphore)
(ql:quickload :next)

(next::set-default 'next::port 'next::path (format nil "~a/ports/gtk-webkit/next-gtk-webkit" *next-inst-dir*))

(defun bt-start ()
  (let ((name (format nil
                      "~a/ports/gtk-webkit/next-gtk-webkit"
                      *next-inst-dir*)))
    (bt:make-thread
     (lambda ()
       (uiop:run-program name))))
  (next:start))

(if *use-port*
    (next:start-with-port)
    (bt-start))

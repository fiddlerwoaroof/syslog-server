;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:syslog_helper)

(cffi:defcstruct winsize
  (ws_row :unsigned-short)
  (ws_col :unsigned-short)
  (ws_xpixel :unsigned-short)
  (ws_ypixel :unsigned-short))

(defun get-term-size ()
  (flet ((ioctl-gwinsz (fd)
	   (cffi:with-foreign-object (ptr '(:pointer (:struct winsize)))
	     (let* ((res (osicat-posix:ioctl fd osicat-posix:tiocgwinsz ptr)))
	       (if (= res 0)
		   (cffi:with-foreign-slots ((ws_row ws_col) ptr (:struct winsize))
		     (list ws_row ws_col))
		   (format t "~&error~%"))))))
    (loop with err = nil
       for x from 0 to 2
       for res = (handler-case (ioctl-gwinsz x)
		   (osicat-posix:enotty (c) (setf err c)))
       finally (if err
		   (error err)
		   (return res)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbol-concat (start end)
    (serapeum:concat
     (symbol-name start)
     (symbol-name end))))

(defmacro define-codecs (name () &body mappings)
  `(progn
     (defun ,(intern (symbol-concat '#:decode- name)) (,name)
       (ecase ,name
	 ,@mappings))
     (defun ,(intern (symbol-concat '#:encode- name)) (,name)
       (string-case:string-case ((string-downcase ,name))
	 ,@(mapcar #'reverse mappings)))))

(cl-ansi-term:update-style-sheet
 '((:emergency :black :b-red :bold)
   (:alert :black :b-red)
   (:critical :red :bold)
   (:error :red)
   (:warning :yellow :bold)
   (:notice :yellow)
   (:info :green)))


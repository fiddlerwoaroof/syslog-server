;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:syslog-server)

(defsynopsis ()
  (flag :short-name "h" :long-name "help" :description "Show the help")
  (stropt :short-name "s" :long-name "severity" :description "Show this syslog severity (or higher)"
	  :default-value (format nil "~a" (encode-severity "debug")))
  (stropt :short-name "f" :long-name "facility" :description "Show messages from comma-separated list of facilities"))

(defun main ()
  (declare (optimize (debug 3)))
  (ql:quickload :swank)
  (swank:create-server :port 5676 :dont-close t)
  (make-context)
  (cl-ansi-term:register-hook :before-printing
			      (lambda ()
				(handler-case (setf cl-ansi-term:*terminal-width*
						    (cadr (get-term-size)))
				  (osicat-posix:enotty (c) c nil))))
  (if (getopt :long-name "help")
      (help)
      (let ((match-severity (parse-integer (or (getopt :long-name "severity")
					       "7")))
	    (match-facilities (split-sequence:split-sequence #\, (getopt :long-name "facility")
							     :remove-empty-subseqs t)))
	(when match-facilities
	  (setf match-facilities
		(loop for facility in match-facilities
		   if (digit-char-p (elt facility 0)) collect (parse-integer facility)
		   else collect (encode-facility (string-downcase facility)))))
	(handler-case
	    (with-simple-restart (abort "Exit")
	      (loop
		 (with-simple-restart (continue "Restart server")
		   (start-syslog-server (main-loop match-severity match-facilities)))))
	  #+sbcl (sb-sys:interactive-interrupt (c) c)))))


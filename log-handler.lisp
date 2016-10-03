;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:syslog_helper)

(define-codecs facility ()
  (0 "kernel")
  (1 "user")
  (2 "mail")
  (3 "system")
  (4 "security")
  (5 "syslogd")
  (6 "lpr")
  (7 "nntp")
  (8 "uucp")
  (9 "clock")
  (10 "auth")
  (11 "ftp")
  (12 "ntp")
  (13 "audit")
  (14 "alert")
  (15 "clock1")
  (16 "local0")
  (17 "local1")
  (18 "local2")
  (19 "local3")
  (20 "local4")
  (21 "local5")
  (22 "local6")
  (23 "local7"))

(define-codecs severity ()
  (0 "emergency") ; system is unusable
  (1 "alert")	; action must be taken immediately
  (2 "critical") ; critical conditions
  (3 "error")	   ; error conditions
  (4 "warning")  ; warning conditions
  (5 "notice") ; normal but significant condition
  (6 "info")	; informational messages
  (7 "debug"))	; debug-level messages

(defun format-tag (tag-data)
  (if tag-data
      (destructuring-bind (tag pid) tag-data
	(format nil "[~a~:[~; ~:*~a~]]" tag pid))
      "[untagged]"))

(defgeneric handle-log-message (tag metadata message orig-line)
  (:method (tag metadata message orig-line)
    (declare (ignore tag metadata message))))

(:method (tag metadata message orig-line)
    (declare (ignore tag metadata message)))

(defmethod handle-log-message :before ((tag (eql :untagged)) metadata message orig-line)
  (declare (ignore metadata message))
  (dbi:with-connection (db :sqlite3 :database-name "/tmp/logs.db")
      (log-untagged-to-sqlite db :line orig-line)))

(defmethod handle-log-message ((tag (eql :dnsmasq)) metadata message orig-line)
  (let* ((parsed-query (smug:parse (.dnsmasq-query) message))
	 (parsed-reply (unless parsed-query
			 (smug:parse (.dnsmasq-reply) message))))
    (dbi:with-connection (db :sqlite3 :database-name "/tmp/logs.db")
      (when parsed-query
	(apply #'log-dnsquery-to-sqlite db parsed-query))
      (when parsed-reply
	(apply #'log-dnsreply-to-sqlite db parsed-reply)))))

(defun main-loop (match-severity match-facilities)
  (declare (optimize (debug 3)))
  (lambda (line)
    (let ((host-info (format nil "~{~a~^.~}:~a "
			     (map 'list #'identity usocket:*remote-host*)
			     usocket:*remote-port*)))
      (with-simple-restart (skip "skip this line")
	(multiple-value-bind (facility severity tag-els timestamp message) (parse-syslog line)
	  (destructuring-bind (tag pid) tag-els
	    (dbi:with-connection (db :sqlite3 :database-name "/tmp/logs.db")
	      (when (< severity 7)
		(log-to-sqlite db severity facility tag pid message host-info)))
	    (when (and (<= severity match-severity)
		       (or (null match-facilities)
			   (member facility match-facilities)))
	      (cl-ansi-term:cat-print
	       `((,(format nil "~11<<~a~>:~11@<~a>~>"
			   (decode-facility facility)
			   (decode-severity severity))
		   ,(alexandria:make-keyword (string-upcase (decode-severity severity))))
		 ,host-info
		 (,(format-tag tag-els) :info)
		 ,message)))))))))


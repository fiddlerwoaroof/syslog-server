;;;; syslog_helper.lisp

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:syslog_helper)

(defvar *zxcv* *debug-io*
  "Where to send debug output: only used during development")

(defun start-syslog-server (handler &key (port 514))
  (usocket:socket-server "0.0.0.0" port
			 (lambda (buffer)
			   (declare (type (simple-array (unsigned-byte 8) *) buffer))
			   (funcall handler (babel:octets-to-string buffer))
			   (vector))
			 nil
			 :protocol :datagram))


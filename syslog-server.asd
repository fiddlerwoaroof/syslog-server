;;;; syslog_helper.asd

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(asdf:defsystem #:syslog_helper
  :description "A syslog server that prints incoming logs prettily and writes them to a db"
  :author "Edward Langeley"
  :license "MPLv2"
  :depends-on (#:alexandria
	       #:cffi
	       #:cl-ansi-term
	       #:cl-dbi
	       #:dbd-sqlite3
	       #:fwoar.lisputils
	       #:net.didierverna.clon
	       #:osicat
	       #:positional-lambda
	       #:serapeum
	       #:smug
	       #:swank
	       #:usocket)
  :serial t
  :components ((:file "package")
	       (:file "utils")
	       (:file "parser")
	       (:file "db-write")
	       (:file "syslog-server")
	       (:file "log-handler")
	       (:file "client")))


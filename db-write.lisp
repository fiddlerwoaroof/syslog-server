;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:syslog_helper)

(defun log-to-sqlite (db severity facility tag pid message host)
  (let* ((query (dbi:prepare db "insert into messages (severity, facility, tag, pid, message, host) values (?,?,?,?,?,?)"))
	 (result (dbi:execute query severity facility tag pid message host)))
    result))

(defun log-untagged-to-sqlite (db &key line)
  (let* ((db-query (dbi:prepare db "insert into untagged (data) values (?)"))
	 (result (dbi:execute db-query line)))
    result))

(defun log-dnsquery-to-sqlite (db &key query-type query from)
  (let* ((db-query (dbi:prepare db "insert into dns_query (query_type, request, requester) values (?,?,?)"))
	 (result (dbi:execute db-query query-type query from)))
    result))

(defun log-dnsreply-to-sqlite (db &key query reply)
  (let* ((db-query (dbi:prepare db "insert into dns_reply (query, reply) values (?,?)"))
	 (result (dbi:execute db-query query reply)))
    result))


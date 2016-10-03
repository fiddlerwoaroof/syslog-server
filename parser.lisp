;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:syslog_helper)

(defun .one-of (choices)
  (apply #'smug:.or
	 (mapcar #'smug:.string=
		 choices)))

(defun .numeric-in-range (&key (min 0) max (min-inclusive t) max-inclusive)
  (let ((min-op (if min-inclusive #'<= #'<))
	(max-op (if max-inclusive #'>= #'>)))
    (smug:.let* ((num (smug:.first (smug:.map 'string (smug:.is #'digit-char-p)))))
      (let ((num (parse-integer num)))
	(if (and (funcall min-op min num)
		 (if max
		     (funcall max-op max num)
		     t))
	    (smug:.identity num)
	    (smug:.fail))))))

(defun .time ()
  (flet ((.min-sec ()
	     (.numeric-in-range :max 60))
	   (.hour ()
	     (.numeric-in-range :max 24)))
    (smug:.let* ((hour (smug:.prog1 (.hour)
				    (smug:.char= #\:)))
		 (minute (smug:.prog1 (.min-sec)
				      (smug:.char= #\:)))
		 (second (.min-sec)))
      (smug:.identity (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))))

(defun .timestamp ()
  (flet ((.month ()
	   (.one-of '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
		      "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
	 (.day ()
	   (.numeric-in-range :max 32)))
    (smug:.let* ((month (smug:.prog1 (.month)
				     (smug:.map 'list
						(smug:.char= #\space))))
		 (day (smug:.prog1 (.day)
				   (smug:.map 'list
					      (smug:.char= #\space))))
		 (time (.time)))
      (smug:.identity (format nil "~a ~a ~a" month day time)))))

(defun .hostname ()
  (smug:.prog1
   (smug:.first
    (smug:.map 'string
	       (smug:.or (smug:.char= #\.)
			 (smug:.is #'alphanumericp))))
   (smug:.char= #\space)))

(defun .tag ()
  (smug:.first
   (smug:.prog1
    (smug:.let* ((tag (smug:.optional (smug:.map 'string
						 (smug:.is #'alphanumericp))))
		 (process (smug:.optional
			   (smug:.prog2 (smug:.char= #\[)
					(smug:.map 'string (smug:.is #'digit-char-p))
					(smug:.char= #\])))))
      (if (> (length tag) 32)
	  (smug:.fail)
	  (smug:.identity (list (or tag "untagged")
				(when process
				  (parse-integer process))))))
    (smug:.optional (smug:.char= #\:)))))

(defun .dnsmasq-reply ()
  (smug:.progn (smug:.optional (smug:.char= #\space))
	       (.one-of '("cached " "reply " "/tmp/hosts/dhcp " "DHCP "))
	       (smug:.let* ((query (smug:.prog1 (smug:.map 'string
							   (smug:.or (smug:.char= #\-)
								     (smug:.char= #\_)
								     (smug:.char= #\.)
								     (smug:.is #'alphanumericp)))
						(smug:.string= " is ")))
			    (reply (smug:.prog1 (smug:.map 'string (smug:.item))
						(smug:.not (smug:.item)))))
		 (smug:.identity (list :query query
				       :reply reply)))))

(defun .dnsmasq-query ()
  (smug:.progn (smug:.optional (smug:.char= #\space))
	       (smug:.string= "query")
	       (smug:.let* ((query-type (smug:.prog2 (smug:.char= #\[)
						     (smug:.map 'string (smug:.is #'upper-case-p))
						     (smug:.char= #\])
						     (smug:.char= #\space)))
			    (query (smug:.prog1 (smug:.map 'string
							       (smug:.or (smug:.char= #\-)
									 (smug:.char= #\_)
									 (smug:.char= #\.)
									 (smug:.is #'alphanumericp)))
						    (smug:.string= " from ")))
			    (from (smug:.prog1 (smug:.map 'string (smug:.item))
					       (smug:.not (smug:.item)))))
		 (smug:.identity (list :query-type query-type
				       :query query
				       :from from)))))

(defstruct (priority (:type vector))
  facility severity)

(defun extract-priority (line)
  "Extract a priority from a syslog line and parse it into a severity/priority list"
  (when (char= (elt line 0)
	       #\<)
    (multiple-value-bind (result end) (parse-integer line :start 1 :junk-allowed t)
      (when (and (< end
		    (length line))
		 (char= (elt line end)
			#\>))
	(values (multiple-value-call #'vector
		  (floor result 8))
		(1+ end))))))

(defun parse-syslog (line)
  (multiple-value-bind (priority end-priority) (extract-priority line)
    (let ((line (subseq line end-priority)))
      (multiple-value-bind (timestamp ts-leftover) (smug:parse (smug:.optional (smug:.prog1 (.timestamp)
											    (smug:.map 'list
												       (smug:.char= #\space))))
							       line)
	(multiple-value-bind (hostname hn-leftover) (smug:parse (smug:.optional (.hostname)) ts-leftover)
	  (multiple-value-bind (tag-els leftover) (smug:parse (.tag) hn-leftover)
	    (handle-log-message (alexandria:make-keyword
				 (string-upcase (car tag-els)))
				(list :priority priority
				      :timestamp timestamp
				      :hostname hostname
				      :tag-value (car tag-els)
				      :tag-pid (cadr tag-els))
				leftover
				line)
	    (values (priority-facility priority)
		    (priority-severity priority)
		    tag-els
		    timestamp
		    leftover)))))))


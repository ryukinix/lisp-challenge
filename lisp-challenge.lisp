;; Common Lisp Script
;; Manoel Vilela
;; level 0 -- evaluation
(defun level-0 ()
  (expt 2 38))

;; level 1 -- cipher
(defparameter *text* "g fmnc wms bgblr rpylqjyrc gr zw fylb. rfyrq ufyr amknsrcpq ypc dmp. bmgle gr gl zw fylb gq glcddgagclr ylb rfyr'q ufw rfgq rcvr gq qm jmle. sqgle qrpgle.kyicrpylq() gq pcamkkclbcb. lmu ynnjw ml rfc spj.")

(defun decode-char (c key)
  (let ((code (char-code c))
        (start (char-code #\a))
        (final (char-code #\z)))
    (if (<= start code final)
        (code-char (+ start (mod (- (+ code key) start)
                                 (1+ (- final start)))))
        c)))

(defun decode-text (text &optional (key 2))
  (map 'string (lambda (c) (decode-char c key)) text))

(defun level-1 ()
  (princ (decode-text *text*))
  (format t "~&ANWER: map => ~a ~%" (decode-text "map")))

;; level 2 -- counting
(defun rare-chars (text)
  (let* ((unique (remove-duplicates text))
         (map-char (loop for c across unique collect (list c (count c text))))
         (rares (mapcar #'first (remove-if (lambda (x) (> (second x) 1))
                                           map-char))))
    (map 'string #'identity
         (sort rares (lambda (x y) (< (position x text)
                                      (position y text)))))))

(defun get-content (stream)
  (labels ((concat (xs)
             (reduce (lambda (x y)
                       (concatenate 'string x y))
                     xs
                     :from-end t
                     :initial-value nil))
           (read-stream (s)
             (loop for line = (read-line s nil 'eof)
                   until (eq line 'eof)
                   collect line)))
    (concat (read-stream stream))))


(defun read-file (path)
  (with-open-file (stream path)
    (get-content stream)))


(defun level-2 (&optional (path "level-2.txt"))
  (rare-chars (read-file path)))


;; level 3 -- pattern matching
(defun case-match (pattern target)
  "(case-match 'XXXxxx' 'ABCdef') => t"
  (loop for x across pattern
        for y across target
        always (eq (upper-case-p x)
                   (upper-case-p y))))

(defun search-case-pattern (text pattern)
  (loop with length-pattern = (length pattern)
        with length-text = (length text)
        for x from 0 to (- length-text
                           length-pattern)
        for target = (subseq text x (+ x length-pattern))
        when (case-match pattern target)
          collect target))

(defun level-3 (&optional (path "level-3.txt"))
  (map 'string (lambda (x) (aref x 4))
       (search-case-pattern (read-file path) "xXXXxXXXx")))

(if (not (find-package :drakma))
    (ql:quickload :drakma))

;; level 4 -- linkedlist
(defun acess-page (url)
  (nth-value 0 (drakma:http-request url)))

(defun split-string (string &optional (separator #\space))
  "Sorry for this messy. I don't wanna use external lib for this"
  (loop with next = 0
        for start from 0
        for substring = (subseq string next)
        while (position separator substring)
        when (equalp (aref string start) separator)
          collect (subseq string next start) into parts
           and do (setq next (1+ start))
        finally (return (append parts (list substring)))))

(defun parse-result (content)
  (car (last (split-string content))))

(defun follow-nothing-chain ()
  (let ((base-url "http://www.pythonchallenge.com/pc/def/linkedlist.php?nothing=~a")
        (head "12345")
        (limit 400))
    (labels ((rules (x)
               (let ((int (parse-integer x)))
                 (case int
                   (16044 (/ int 2))
                   (otherwise int)))))
      (loop for chain = head
              then (parse-result (acess-page (format nil base-url (rules chain))))
            for counter from 0
            while (< counter limit)
            when (not (parse-integer chain :junk-allowed t))
              return chain
            do (format t "~a " chain)))))

(defun level-4 ()
  (follow-nothing-chain))

;; level 5

(defun char-repeat (char n)
  (map 'string #'identity
       (loop repeat n
         collect char)))

(defun level-5 (&optional (path "level-5.txt"))
  (let ((lines (read-from-string (read-file path))))
    (loop for line in lines
          for translate = (loop for (c n) in line collect (char-repeat c n))
          do (format t "~{~a~}~%" translate))))

;; level 6
(if (not (find-package :zip))
    (ql:quickload :zip))

(defun chain-filename (number)
  (format nil "~a.txt" number))

(defun valid-number (input)
  (parse-integer input :junk-allowed t))

(defun entry-contents-as-string (entry)
  (map 'string #'code-char (zip:zipfile-entry-contents entry)))

;; Mon 27 Sep 2021 12:45:40 AM -03: NOTE
;; buggy, when the condition #<SB-INT:CLOSED-STREAM-ERROR {1001840013}> raises,
;; just invoke RETRY and hopes to the best to finish with the proper answer
(defun comment-chain-zipfile ()
  (let* ((z (zip:open-zipfile "level-6.zip"))
         (entries (zip:zipfile-entries z))
         (seed "90052"))
    (loop for entry = (gethash (chain-filename seed) entries)
          for next = (parse-result (entry-contents-as-string entry))
          while (valid-number next)
            collect (zip:zipfile-entry-comment entry) into comments
          do (setq seed next)
          finally (return comments))))


(defun level-6 ()
  (format nil "~{~a~}" (comment-chain-zipfile)))

#lang racket/base

(require csv-reading)
(require csv-writing)

(require "enum.rkt")

(provide read-TOCFL write-TOCFL write-TOCFL-csv
 INDEX-FIELD-NUM
 EXPRESSION-FIELD-NUM
 PINYIN-FIELD-NUM
 VARIANTS-FIELD-NUM
 PoS-FIELD-NUM
 HINT-FIELD-NUM
 KEYWORD-FIELD-NUM
 COMMENTS-FIELD-NUM
 STROKES-FIELD-NUM
 CHARACTERS-RTH-FIELD-NUM
 MEANING-FIELD-NUM
 CATEGORY-FIELD-NUM
 VARIANTS-PINYIN-FIELD-NUM
 SIMPLIFIED-FIELD-NUM
 RANK-FIELD-NUM
 PRODUCE-FIELD-NUM
 TAGS-FIELD-NUM
)


(define IN-FILENAME "TOCFL.txt")
;; (define OUT-FILENAME "RTH-el-pleco.txt")


(enum
 INDEX-FIELD-NUM
 EXPRESSION-FIELD-NUM
 PINYIN-FIELD-NUM
 VARIANTS-FIELD-NUM
 PoS-FIELD-NUM
 HINT-FIELD-NUM
 KEYWORD-FIELD-NUM
 COMMENTS-FIELD-NUM
 STROKES-FIELD-NUM
 CHARACTERS-RTH-FIELD-NUM
 MEANING-FIELD-NUM
 CATEGORY-FIELD-NUM
 VARIANTS-PINYIN-FIELD-NUM
 SIMPLIFIED-FIELD-NUM
 RANK-FIELD-NUM
 PRODUCE-FIELD-NUM
 TAGS-FIELD-NUM
)

;;; --- Reading the file
(define (make-reader [delimiter #\tab])
  (make-csv-reader-maker
   `((separator-chars              ,delimiter)
     (strip-leading-whitespace?  . #f)
     (strip-trailing-whitespace? . #f))))

(define (all-rows port)
  (define read-row ((make-reader) port))
  (for/list ([row (in-producer read-row '())])
      row))

(define (read-file infilename)
  (with-input-from-file infilename
    (lambda () (all-rows (current-input-port)))))


(define (read-TOCFL infilename)
  (read-file infilename))


;;; --- Writing

(define (write-TOCFL-csv outfilename data)
  (let ((printing-params (make-csv-printing-params
                          #:quotes-only-when-needed? #f)))
    (with-output-to-file outfilename
      (lambda () (display-table data (current-output-port) #:printing-params printing-params ))
      #:exists 'replace)))


(define (write-TOCFL outfilename data)
  (let ((printing-params default-tsv-printing-params))
    (with-output-to-file outfilename
      (lambda () (display-table data (current-output-port) #:printing-params printing-params ))
      #:exists 'replace)))

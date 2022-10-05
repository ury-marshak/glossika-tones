#lang racket/base

(require csv-reading)
(require csv-writing)

(require "enum.rkt")

(provide read-Tones write-Tones write-Tones-csv
 INDEX-FIELD-NUM
 KEYWORD-FIELD-NUM
 TRAD-FIELD-NUM
 AUDIO-FIELD-NUM
 PINYIN-FIELD-NUM
 TONE-A-FIELD-NUM
 TONE-B-FIELD-NUM
 TONE-C-FIELD-NUM
 MC-TONES-FIELD-NUM
 PoS-FIELD-NUM
 COMMENTS-FIELD-NUM
 STROKES-FIELD-NUM
 CHARACTERS-RTH-FIELD-NUM
 MEANING-FIELD-NUM
 VARIANTS-FIELD-NUM
 VARIANTS-PINYIN-FIELD-NUM
 SIMPLIFIED-FIELD-NUM
 SORT_ORDER-FIELD-NUM
 TAGS-FIELD-NUM
)


(define IN-FILENAME "Tones.txt")
;; (define OUT-FILENAME "RTH-el-pleco.txt")


(enum
 INDEX-FIELD-NUM
 KEYWORD-FIELD-NUM
 TRAD-FIELD-NUM
 AUDIO-FIELD-NUM
 PINYIN-FIELD-NUM
 TONE-A-FIELD-NUM
 TONE-B-FIELD-NUM
 TONE-C-FIELD-NUM
 MC-TONES-FIELD-NUM
 PoS-FIELD-NUM
 COMMENTS-FIELD-NUM
 STROKES-FIELD-NUM
 CHARACTERS-RTH-FIELD-NUM
 MEANING-FIELD-NUM
 VARIANTS-FIELD-NUM
 VARIANTS-PINYIN-FIELD-NUM
 SIMPLIFIED-FIELD-NUM
 SORT_ORDER-FIELD-NUM
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


(define (read-Tones infilename)
  (read-file infilename))


;;; --- Writing

(define (write-Tones-csv outfilename data)
  (let ((printing-params (make-csv-printing-params
                          #:quotes-only-when-needed? #f)))
    (with-output-to-file outfilename
      (lambda () (display-table data (current-output-port) #:printing-params printing-params ))
      #:exists 'replace)))


(define (write-Tones outfilename data)
  (let ((printing-params default-tsv-printing-params))
    (with-output-to-file outfilename
      (lambda () (display-table data (current-output-port) #:printing-params printing-params ))
      #:exists 'replace)))

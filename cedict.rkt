#lang racket/base

(require  racket/file)
(require  racket/string)
(require  racket/match)
(require  racket/list)

(provide CEDICT_FILENAME
         (struct-out cedict-entry)
         cedict-all-meanings
         lookup-in-cedict
         read-CEDICT)

(define CEDICT_FILENAME "cedict_1_0_ts_utf-8_mdbg.txt")


(struct cedict-entry
  (trad
   simp
   pinyin
   translations-string

   [translations #:auto])
  #:transparent
  #:mutable)


(define (parse-translations-string tr-string)
  (string-split (string-trim tr-string "/") "/"))

(define (parse-cedict-line line)
  "^(\\w+)\\s+(\\w+)\\s+\\[([^]]*)\\]\\s+([^\\r]*)\\r?$"
  (let ([rmatch (regexp-match #px"(\\S+)\\s+(\\S+)\\s+\\[([^]]*)\\]\\s+([^\r]*)\r?$" (string-trim line))])
    (match rmatch
        [(list _ trad simp pinyin-with-spaces translations-s)

         (let ([entry (cedict-entry trad simp pinyin-with-spaces translations-s)])
           (set-cedict-entry-translations! entry (parse-translations-string translations-s))
           entry)         ])))


(define (add-cedict-line cedict-hash line)
  (unless (or (string=? line "")
              (string-prefix? line "#"))
    (let* ([entry (parse-cedict-line line)]
           [trad (cedict-entry-trad entry)])
      (hash-update! cedict-hash trad
                    (lambda (lst)
                      (append lst (list entry)))
                    '()))))

(define (read-CEDICT path)
  (let ([lines (file->lines path)]
        [cedict (make-hash)])
    (for ([line lines])
      (add-cedict-line cedict line))
    cedict))

(define (lookup-in-cedict cedict-hash trad [failure-result #f])
  ;; returns a list of entries
  (hash-ref cedict-hash trad failure-result))

(define (cedict-all-meanings cedict-hash trad [failure-result #f])
  (let ([entries (lookup-in-cedict cedict-hash trad #f)])
    (if entries
        (append-map cedict-entry-translations entries)
        failure-result)))

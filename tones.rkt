#lang racket/base

(require  racket/string)
(require  racket/path)
(require  racket/generator)
(require  racket/file)
(require  racket/match)
(require  racket/sequence)
(require  racket/list)
(require  racket/fixnum)

(require "unaccent.rkt")
(require (prefix-in TOCFL: "tocfl-tsv.rkt") )
(require (prefix-in RTH: "rth-tsv.rkt") )
(require "cedict.rkt")
(require "tones-tsv.rkt")


(define (not-hidden-path? filepath)
  (let-values ([(base name must-be-dir?) (split-path filepath)])
    (not (string-prefix? (path->string name) "."))))


(define (load-sound-files dirname)
  (filter
   not-hidden-path?
   (directory-list dirname #:build? #t)))

(define (filename-has-trimmed? filename)
  (let-values ([(base name must-be-dir?) (split-path filename)])
    (string-suffix? (path-replace-extension name #"") "_trimmed")))


(define (has-trimmed-file dirname)
  (let ([filenames (load-sound-files dirname)])
    (ormap filename-has-trimmed?
           filenames)))



(define (kill-untrimmed filepath)
  (define trimmed_str "_trimmed")
  (let-values ([(base name must-be-dir?) (split-path filepath)])
    (let ([ext (path-get-extension name)]
          [base-fn (path->string (path-replace-extension name #""))])
      (when (string-suffix? base-fn  trimmed_str)
        (let* ([untrimmed-base-fn (string-trim base-fn trimmed_str)]
               [untrimmed-path (build-path base (path-replace-extension untrimmed-base-fn ext))])
          ;; (displayln untrimmed-path)
          ;; (displayln filepath)
          (delete-file untrimmed-path)
          (rename-file-or-directory filepath untrimmed-path)
          )))))



(define (remove-untrimmed-files dirname)
  (let ([filenames (load-sound-files dirname)])
    (for-each kill-untrimmed
              filenames)))




;; (define get4lines
;;   (generator (lines)
;;              (let loop ([curr lines])
;;                (if (null? curr)
;;                    eof
;;                    (let ([four-lines (for/list ([i (in-range 4)])
;;                                        (when (null? curr)
;;                                          (error (format "Missing line ~a of 4" (+ 1 i))))
;;                                        (begin0
;;                                            (car curr)
;;                                          ;(println (car curr))
;;                                          (set! curr (cdr curr))))])
;;                      (println four-lines)
;;                      (yield four-lines)
;;                      (loop curr))))))

;;     (for/list ([mc-simp-trad-pinyin (in-producer get4lines eof  name-file-lines)])
;;       (println mc-simp-trad-pinyin)  )


(define (get-props-from-lines name-file-lines)
  (define (rem-ascii-letters word)
    (string-replace word #px"\\w+|\\s+" "" ))

  (for/list ([mc-simp-trad-pinyin (in-slice 4 name-file-lines)])
      (match mc-simp-trad-pinyin
        [(list mc trad simp pinyin)

         (list mc trad (rem-ascii-letters trad) simp (rem-ascii-letters simp) pinyin)])))

(struct tonefile
  (index
   index-in-tone
   resulting-filename
   trad
   simp
   source-path
   tonenumberA
   tonenumberB
   mc-tones
   mc-trad
   mc-simp
   pinyin

   [keyword #:auto]
   [meaning #:auto]
   [RTH #:auto]
   [PoS #:auto]
   [strokes #:auto]
   [variants #:auto]
   [variants-pinyin #:auto]
   )
  #:auto-value ""
  #:transparent
  #:mutable)


;; (define (glossika-file-lines->names name-file-lines)
;;   (for/list ([mc-simp-trad-pinyin (get-props-from-lines name-file-lines)])
;;       (match mc-simp-trad-pinyin
;;         [(list mc simp-mc simp trad-mc trad pinyin)

;;          (string-append trad "_"  pinyin)])))


(define (glossika-file-lines->tonefiles name-file-lines)
  (for/list ([mc-simp-trad-pinyin (get-props-from-lines name-file-lines)])
      (match mc-simp-trad-pinyin
        [(list mc trad-mc trad simp-mc simp pinyin)

         (tonefile 0 0
                   "" trad simp ""
                   0 0
                   mc
                   trad-mc
                   simp-mc
                   pinyin)])))


(define (glossika-file->tonefiles glossika-file-name)
  ;; return a list of (parts of) new file names
  (let ([tonefiles (glossika-file-lines->tonefiles (file->lines glossika-file-name))])
    tonefiles))

(define (tone-file-name-from-dirname dirname)
  (define namefilebasename (string-downcase (string-trim dirname "/")))
  (define namefilepath (path-replace-extension namefilebasename ".txt"))
  namefilepath)

;; (define (process-names-file dirname)
;;   (define namefilebasename (string-downcase (string-trim dirname "/")))
;;   (define namefilepath (path-replace-extension namefilebasename ".txt"))
;;   (define outputfilepath (path-replace-extension namefilebasename "-n.txt"))
;;   (displayln outputfilepath)
;;   ;(unless (file-exists? outputfilepath) )

;;   (let ([partial-names (glossika-file->names namefilepath)])
;;     (println (length partial-names)))

;;   )

(define (verify-count dirname sound-file-paths tonefiles)
  (unless (= (length sound-file-paths)
             (length tonefiles))
      (println sound-file-paths)
      (error "Mismatch " dirname "~n"
             "files: " (length sound-file-paths)
             "tones: " (length tonefiles))))


;; (define (update-filenames-in-tonefiles tonefiles)
;;   )


(define (make-resulting-filename tf)
  (format "TONES-~a~a-~a.mp3" (tonefile-tonenumberA tf) (tonefile-tonenumberB tf)
          (unaccent (tonefile-pinyin tf))))

;; (define (process-dir dirname toneA toneB)
;;   ;(process-names-file dirname)
;;   (let ([sound-file-paths (load-sound-files dirname)]
;;         [tonefiles (glossika-file->tonefiles (tone-file-name-from-dirname dirname))])
;;     (verify-count dirname sound-file-paths tonefiles)
;;     (for ([audio-path sound-file-paths]
;;           [tf tonefiles])
;;       (set-tonefile-source-path! tf audio-path)
;;       (set-tonefile-tonenumberA! tf toneA)
;;       (set-tonefile-tonenumberB! tf toneB)
;;       (set-tonefile-resulting-filename! tf (make-resulting-filename tf)))

;;      ;; (println tonefiles)

;;      (for ([tf tonefiles])
;;        (let ([out-path (build-path "output" (tonefile-resulting-filename tf))])
;;          (println out-path)
;;          (copy-file (tonefile-source-path tf) out-path #t))
;;       )
;;     ))

(define (load-tonefiles-for-dir dirname toneA toneB)
  (let ([sound-file-paths (load-sound-files dirname)]
        [tonefiles (glossika-file->tonefiles (tone-file-name-from-dirname dirname))])
    (verify-count dirname sound-file-paths tonefiles)
    (for ([audio-path sound-file-paths]
          [tf tonefiles])
      (set-tonefile-source-path! tf audio-path)
      (set-tonefile-tonenumberA! tf toneA)
      (set-tonefile-tonenumberB! tf toneB)
      (set-tonefile-resulting-filename! tf (make-resulting-filename tf)))

     tonefiles    ))



(define (load-all-dirs)
  (let ([tones (for*/list ([i (in-inclusive-range 1 4)]
                           [j (in-inclusive-range 1 4)])
                 (let* ([dirname (format "tones/T~a~a" i j)]
                        [dirtones (load-tonefiles-for-dir dirname i j)])
                   ;; add index in tone directory
                   (for ([tf dirtones]
                         [i (in-range 1 (most-positive-fixnum))])
                     (set-tonefile-index-in-tone! tf i))
                   dirtones
                   ))])
    (set! tones (append* tones))
    (for ([tf tones]
          [i (in-range 1 (most-positive-fixnum))])
      (set-tonefile-index! tf i))
    tones))


(define TOCFL-IN-FILENAME "TOCFL.txt")
(define tocfl (make-hash))
(define (load-tocfl [use-variants? #f])
  (let ([data (TOCFL:read-TOCFL TOCFL-IN-FILENAME)])
    (define (add-to-hash row)
      (define (get-field fld-num)
        (list-ref row fld-num))

      (let ([word (get-field TOCFL:EXPRESSION-FIELD-NUM) ]
            [variant (get-field TOCFL:VARIANTS-FIELD-NUM)])
        (hash-set! tocfl word row)
        (when (and use-variants? (not (string=? variant "")))
          (when (> (length (string-split variant)) 1)
            (error "More than one variant " variant))
          (hash-set! tocfl variant row))))
    (for-each add-to-hash data) ))
(load-tocfl)


(define RTH-IN-FILENAME "Remembering Traditional Hanzi 1+2.txt")
(define rth (make-hash))
(define (load-rth)
  (let ([data (RTH:read-RTH RTH-IN-FILENAME)])
    (define (add-to-hash row)
      (define (get-field fld-num)
        (list-ref row fld-num))

      (let ([word (get-field RTH:CHARACTER-FIELD-NUM) ])
        (hash-set! rth word row)
        ))
    (for-each add-to-hash data) ))
(load-rth)


(define cedict (read-CEDICT "/Users/ury/Downloads/cedict_1_0_ts_utf-8_mdbg.txt"))


;; (define (count-present2)
;;   (let ([tones (load-all-dirs)])
;;     (count (lambda (tf)
;;                 (when (hash-ref tocfl (tonefile-trad tf) #f)
;;                   ;;(println (tonefile-trad tf))
;;                   #t))
;;            tones )))

(define (count-present-in-tocfl)
  (let ([tones (load-all-dirs)])
    (count (lambda (tf)
             (hash-ref tocfl (tonefile-trad tf) #f))
           tones )))


;; (define (load-and-update-from-tocfl)
;;   (let ([tones (load-all-dirs)]
;;         [missing 0])

;;     (for ([tf tones])
;;       (let ([row (hash-ref tocfl (tonefile-trad tf) #f)])
;;         (define (get-field fld-num)
;;           (list-ref row fld-num))

;;         (if row
;;             (begin (set-tonefile-PoS! tf (get-field PoS-FIELD-NUM))
;;                    (set-tonefile-RTH! tf (get-field CHARACTERS-RTH-FIELD-NUM))
;;                    (set-tonefile-keyword! tf (get-field KEYWORD-FIELD-NUM))
;;                    (set-tonefile-meaning! tf (get-field MEANING-FIELD-NUM))
;;                    (set-tonefile-strokes! tf (get-field STROKES-FIELD-NUM))
;;                    (set-tonefile-variants! tf (get-field VARIANTS-FIELD-NUM))
;;                    (set-tonefile-variants! tf (get-field VARIANTS-PINYIN-FIELD-NUM)))
;;             (begin
;;               ;(println (tonefile-trad tf))
;;               (set! missing (+ 1 missing))
;;               ))
;;         ))
;;     (println missing)
;;     ))



(define (lookup-rths word)
  (let ([rth-lst (for/list ([ch (in-string word)])

                   (let ([rth-row (hash-ref rth (string ch) #f)])

                     (define (get-field fld-num)
                       (list-ref rth-row fld-num))

                     (if rth-row
                         (get-field RTH:KEYWORD-FIELD-NUM)
                         "?")  ))])
    (string-join rth-lst " + ")  ))




(define (load-and-update-all)
  (let ([tones (load-all-dirs)]
        [missing-meaning 0])

    (for ([tf tones])
      (let* ([trad (tonefile-trad tf)]
             [meanings (cedict-all-meanings cedict trad)]
             [rths (lookup-rths trad)])

        (if meanings
            (set-tonefile-meaning! tf (string-join meanings "<br>"))
            (set! missing-meaning (add1 missing-meaning)))
        (set-tonefile-RTH! tf rths)
        ))
    ;; (printf "missing ~a ~n" missing-meaning)
    tones))

(define (show-missing-meaning tones)
  (for ([tf tones])
    (when (string=? (tonefile-meaning tf) "")
      (println (tonefile-trad tf)))
    ))



(define (tonefile->tags-string tf)
  (string-join
   (list
    "TONES"
    "TONES-2SYL"
    (format "TONE~a~a" (tonefile-tonenumberA tf) (tonefile-tonenumberB tf)))))

(define (tonefile->audio-string tf)
  (format "[sound:~a]" (tonefile-resulting-filename tf) ))


(define (remove-tabs s)
  (string-replace s "\t" ""))


(define (tonefile->row tf)
  (let ([row (make-list (add1 TAGS-FIELD-NUM) "")])
    (define (set-field field-num value)
      (set! row (list-set row field-num (remove-tabs (format "~a" value)))))
    (set-field INDEX-FIELD-NUM (tonefile-index tf))
    (set-field INDEX-IN-TONE-FIELD-NUM (tonefile-index-in-tone tf))
    (set-field KEYWORD-FIELD-NUM "")
    (set-field TRAD-FIELD-NUM (tonefile-trad tf))
    (set-field AUDIO-FIELD-NUM (tonefile->audio-string tf))
    (set-field PINYIN-FIELD-NUM (tonefile-pinyin tf))
    (set-field TONE-A-FIELD-NUM (tonefile-tonenumberA tf))
    (set-field TONE-B-FIELD-NUM (tonefile-tonenumberB tf))
    (set-field TONE-C-FIELD-NUM "")
    (set-field MC-TONES-FIELD-NUM (tonefile-mc-tones tf))
    (set-field PoS-FIELD-NUM (tonefile-PoS tf))
    (set-field COMMENTS-FIELD-NUM "")
    (set-field STROKES-FIELD-NUM "")
    (set-field CHARACTERS-RTH-FIELD-NUM (tonefile-RTH tf))
    (set-field MEANING-FIELD-NUM (tonefile-meaning tf))
    (set-field VARIANTS-FIELD-NUM "")
    (set-field VARIANTS-PINYIN-FIELD-NUM "")
    (set-field SIMPLIFIED-FIELD-NUM (tonefile-simp tf))
    (set-field SORT-ORDER-FIELD-NUM "")
    (set-field TAGS-FIELD-NUM (tonefile->tags-string tf))
    row))

(define (tones->data tones)
  (for/list ([tf tones])
    (tonefile->row tf)))

(define (write-tones-to-tsv tones)
  (let ([data (tones->data tones)])
    (write-Tones "Tones.tsv" data)))

(define (write-all)
  (let ([tones (load-and-update-all)])
    (write-tones-to-tsv tones)
    #t))

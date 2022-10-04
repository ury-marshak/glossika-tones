#lang racket/base

(require  racket/string)
(require  racket/path)
(require  racket/generator)
(require  racket/file)
(require  racket/match)
(require  racket/sequence)

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
        [(list mc simp trad pinyin)

         (list mc simp (rem-ascii-letters simp) trad (rem-ascii-letters trad) pinyin)])))

(struct tonefile
  (resulting-filename
   trad
   simp
   source-path
   tonenumberA
   tonenumberB
   mc-tones
   mc-trad
   mc-simp
   pinyin
   )
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
        [(list mc simp-mc simp trad-mc trad pinyin)

         (tonefile "" trad simp ""
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

(define (process-dir dirname toneA toneB)
  ;(process-names-file dirname)
  (let ([sound-file-paths (load-sound-files dirname)]
        [tonefiles (glossika-file->tonefiles (tone-file-name-from-dirname dirname))])
    (verify-count dirname sound-file-paths tonefiles)
    (for ([audio-path sound-file-paths]
          [tf tonefiles])
      (set-tonefile-source-path! tf audio-path)
      (set-tonefile-tonenumberA! tf toneA)
      (set-tonefile-tonenumberB! tf toneB)
      (set-tonefile-resulting-filename! tf (make-resulting-filename tf)))

     ;; (println tonefiles)

     (for ([tf tonefiles])
       (let ([out-path (build-path "output" (tonefile-resulting-filename tf))])
         (println out-path)
         (copy-file (tonefile-source-path tf) out-path #t))
      )
    ))



(define (process-all-dirs)
  (for* ([i (in-inclusive-range 1 4)]
         [j (in-inclusive-range 1 4)])
    (let ([dirname (format "tones/T~a~a" i j)])
      (process-dir dirname i j)
      )))


(define (unaccent str)
  (let (($charMap
         '[
           [#px"ß" "ss"]
           [#px"á|à|â|ä|ā|ǎ|ã|å|ą|ă|ạ|ả|ả|ấ|ầ|ẩ|ẫ|ậ|ắ|ằ|ẳ|ặ" "a"]
           [#px"æ" "ae"]
           [#px"ç|č|ć" "c"]
           [#px"é|è|ê|ë|ē|ě|ę|ẹ|ẻ|ẽ|ế|ề|ể|ễ|ệ" "e"]
           [#px"í|ì|î|ï|ī|ǐ|ỉ|ị" "i"]
           [#px"ñ|ň|ń" "n"]
           [#px"ó|ò|ô|ö|õ|ǒ|ø|ō|ồ|ơ|ọ|ỏ|ố|ổ|ỗ|ộ|ớ|ờ|ở|ợ" "o"]
           [#px"ú|ù|û|ü|ū|ũ|ư|ụ|ủ|ǔ|ứ|ừ|ử|ữ|ự"     "u"] ;; no ü|, we need to keep it
           [#px"ǜ|ǖ|ǘ|ǚ|ṻ"     "ü"]
           [#px"ý|ÿ|ỳ|ỷ|ỹ"     "y"]
           [#px"þ" "th"]
           [#px"ď|ð|đ" "d"]
           [#px"ĩ" "i"]
           [#px"ľ|ĺ|ł" "l"]
           [#px"ř|ŕ" "r"]
           [#px"š|ś" "s"]
           [#px"ť" "t"]
           [#px"ž|ź|ż" "z"]
           [#px" " " "]       ; thin space etc
           [#px"–" "-"]       ; dash
           [#px"—|一" "--"] ; em dash etc
           [#px"'" "_"] ; single quote to underscore
           ]) )
    (for/fold ([word str])
              ([p $charMap])
      ;; (println word)
      ;; (println p)
      (string-replace word (car p) (car (cdr p))))
    ))

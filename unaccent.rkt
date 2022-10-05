#lang racket/base

(require  racket/string)

(provide unaccent)

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

(define (list-processes)
  (sort (filter-map string->number (directory "/proc")) >))

(define (process-running? pid)
  (directory-exists? (sprintf "/proc/~a" pid)))

(define (process-parent-pid pid)
  (let ((lines (handle-exceptions exn
                 #f
                 (with-input-from-file
                     (sprintf "/proc/~a/status" pid)
                   read-lines))))
    (and lines
         (let loop ((lines lines))
           (if (null? lines)
               #f
               (let ((line (car lines)))
                 (if (substring-index "PPid:" line)
                     (string->number (string-trim-both (substring line 5)))
                     (loop (cdr lines)))))))))

(define (process-run-time pid)
  (- (current-seconds)
     (file-modification-time (sprintf "/proc/~a" pid))))

(define (report-hanging-process hanging-pid)
  (read-symbolic-link (sprintf "/proc/~a/cwd" hanging-pid)))

(define (list-processes)
  (sort (filter-map string->number (directory "/proc")) >))

(define (process-running? pid)
  (directory-exists? (sprintf "/proc/~a" pid)))

(define (%read-status-file pid)
  ;; Return the status line from /proc/<pid>/status
  (handle-exceptions exn
      #f
    (with-input-from-file (sprintf "/proc/~a/status" pid) read-line)))

(define (process-parent-pid pid)
  (and-let* ((status-line (%read-status-file pid))
             (tokens (string-split status-line))
             (parent (string->number (list-ref tokens 2))))
    (if (zero? parent)
        #f
        parent)))

(define (process-run-time pid)
  (and-let* ((status-line (%read-status-file pid))
             (start-time/usecs (list-ref (string-split status-line) 7))
             (start-time (string->number
                          (car (string-split start-time/usecs ",")))))
    (- (current-seconds) start-time)))

(define (report-hanging-process pid)
  (with-input-from-file (sprintf "/proc/~a/cmdline" pid) read-line))

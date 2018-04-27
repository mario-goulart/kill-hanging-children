(module kill-hanging-children ()

(import chicken scheme)
(use data-structures extras files irregex posix srfi-1 srfi-13)

(include "version.scm")

(cond-expand
  (linux (include "linux-specific.scm"))
  (else  (error "platform not supported")))

(define *max-run-time-before-kill* (* 60 60))
(define *check-interval* 60)
(define *log-file* #f)

(define (log! fmt . args)
  (when *log-file*
    (with-output-to-file *log-file*
      (lambda ()
        (let* ((now (time->string (seconds->local-time)))
               (msg (sprintf "[~a] ~a\n" now fmt)))
          (print* (apply sprintf (cons msg args)))))
      append:)))

(define (child? parent-pid pid)
  (let ((parent (process-parent-pid pid)))
    (and parent
         (or (= parent-pid parent)
             (child? parent-pid parent)))))

(define (process-hanging? pid)
  (> (process-run-time pid) *max-run-time-before-kill*))

(define (child-hanging? parent-pid pid)
  (and (child? parent-pid pid)
       (process-hanging? pid)))

(define (find-hanging-process parent-pid)
  (let ((pids (list-processes)))
    (let loop ((pids pids))
      (if (null? pids)
          #f
          (let ((pid (car pids)))
            (if (child-hanging? parent-pid pid)
                pid
                (loop (cdr pids))))))))

(define (kill-hanging-process! hanging-pid #!key (max-retries 10))
  (call/cc
   (lambda (return)
     (let retry ((retry-count 1))
       (cond ((<= retry-count max-retries)
              (log! "Attempting to kill ~a with signal/term (attempt ~a of ~a)"
                    hanging-pid retry-count max-retries)
              (process-signal hanging-pid)
              (sleep 1)
              (cond ((memq hanging-pid (list-processes))
                     (log! "Attempting to kill ~a with signal/kill (attempt ~a of ~a)"
                           hanging-pid retry-count max-retries)
                     (process-signal hanging-pid signal/kill))
                    (else
                     (log! "Killed ~a" hanging-pid)
                     (return #f)))
              (sleep 1)
              (cond ((memq hanging-pid (list-processes))
                     (retry (fx+ 1 retry-count)))
                    (else
                     (log! "Killed ~a" hanging-pid)
                     (return #f))))
             (else
              (log! "Could not kill ~a after ~a attempts"
                    hanging-pid max-retries)))))))

(define (cmd-line-arg option args)
  ;; Returns the argument associated to the command line option OPTION
  ;; in ARGS or #f if OPTION is not found in ARGS or doesn't have any
  ;; argument.
  (let ((val (any (lambda (arg)
                    (irregex-match
                     `(seq ,(->string option) "=" (submatch (* any)))
                     arg))
                  args)))
    (and val (irregex-match-substring val 1))))

(define (die! fmt . args)
  (apply fprintf (append (list (current-error-port)
                               (string-append fmt "\n"))
                         args))
  (exit 1))

(define (check-positive-arg arg loc)
  (let ((n (string->number arg)))
    (if (and n (positive? n))
        n
        (die! "~a: expected a positive integer.  Aborting." loc))))

(define (usage #!optional exit-code)
  (let ((port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port)))
        (this (pathname-strip-directory (program-name))))
    (display #<#EOF
Usage: #this [<options>] <parent pid> [<log file>]

<options>
--help
  Show this message and exit.

--max-run-time-before-kill=<seconds>
  Maximum run time (in seconds) before proclaiming a child as hung.
  Default = 3600 (1 hour).

--check-interval=<seconds>
  Time to wait (in seconds) before checking for hanging processes again.
  Default = 60 (1 minute)

--version
  Show version and exit.

EOF
             port)
    (when exit-code
      (exit exit-code))))

(let* ((args (command-line-arguments))
       (non-options (remove (lambda (opt)
                              (string-prefix? "--" opt))
                            args)))
  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))
  (when (member "--version" args)
    (print kill-hanging-children-version)
    (exit 0))
  (when (null? non-options)
    (usage 1))
  (let ((parent-pid (check-positive-arg (car non-options) "<parent pid>")))
    (unless (null? (cdr non-options))
      (set! *log-file* (cadr non-options)))
    (and-let* ((max-time-str (cmd-line-arg "--max-run-time-before-kill" args)))
      (set! *max-run-time-before-kill*
        (check-positive-arg max-time-str
                            "--max-run-time-before-kill")))
    (and-let* ((check-interval (cmd-line-arg "--check-interval" args)))
      (set! *check-interval*
        (check-positive-arg check-interval "--check-interval")))
    (let loop ()
      (when (process-running? parent-pid)
        (let ((hanging-pid (find-hanging-process parent-pid)))
          (when hanging-pid
            (log! "Hanging process: ~a (~a)"
                  hanging-pid
                  (report-hanging-process hanging-pid))
            (kill-hanging-process! hanging-pid))
          (sleep *check-interval*)
          (loop))))))

) ;; end module

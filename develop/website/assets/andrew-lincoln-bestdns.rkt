#lang racket

;; Andy Lincoln
;; BestDNS
;; April 28, 2014
(require net/dns)
(require racket/date)

;; Minimum of list
(define (min lst)
    (cond ((null? (cdr lst)) (car lst))
          ((< (car lst) (min (cdr lst))) (car lst))
          (else (min (cdr lst)))))

;;;
;;; DNS Analyzer
;;;

;;;;                       ;;;;
;; DNS Table for DNS Lookups ;;
;;;;                       ;;;;

(define (make-dns-table)
  (let ((local-table (make-hash)))
    (define (lookup key)
      (hash-ref local-table (list key) #f))
    (define (insert! key value)
      (hash-set! local-table (list key) value)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-provider) lookup)
            ((eq? m 'insert-provider!) insert!)
            (else (error "Unknown operation -- DNS TABLE" m))))
    dispatch))

(define dns-table (make-dns-table))
(define get-dns (dns-table 'lookup-provider))
(define put-dns (dns-table 'insert-provider!))

;;;;                ;;;;
;; DNS Server Package ;;
;;;;                ;;;;

;; Server is a cons pair
;; address of type string
;; status of type symbol

(define (active? x) (boolean? x))

(define (make-dns-server ip)
  (let ([address ip]
        [status #t])
    (define (get-server-address) ip)
    (define (get-server-status) status)
    (define (server-dispatch m)
      (cond ((eq? m 'get-server-address) (get-server-address))
            ((eq? m 'get-server-status) (get-server-status))
            (else (error "Unknown operation --DNS TABLE" m))))
    server-dispatch))

;;;;                  ;;;;
;; DNS Provider Package ;;
;;;;                  ;;;;
(define providers '())

(define (make-dns-provider n lst)
  (let ([name n]
        [servers (map make-dns-server lst)])
    (define (get-active-servers) (map (lambda (x) (if (x 'get-server-status) (x 'get-server-address) '())) servers))
    (define (get-inactive-servers) (map (lambda (x) (if (not(x 'get-server-status)) (x 'get-server-address) '())) servers))
    (define (get-provider-name) name)
    (define (get-primary) (car (get-servers)))
    (define (get-nth-server n) (if (> (- n 1) (length (get-servers))) (error "Server list index too large -- DNS PROVIDER") (list-ref (get-servers)  n)))
    (define (get-servers) (map (lambda (x)  (x 'get-server-address)) servers))
    (define (add-server s) (set! servers (append servers (list (make-dns-server s)))))
    (define (dns-provider-dispatch m)
      (cond ((eq? m 'get-active-servers) (get-active-servers))
            ((eq? m 'get-inactive-servers) (get-inactive-servers))
            ((eq? m 'get-primary) (get-primary))
            ((eq? m 'get-provider-name) (get-provider-name))
            ((eq? m 'get-nth-server) get-nth-server)
            ((eq? m 'get-servers) (get-servers))
            ((eq? m 'add-server) add-server)
            (else (error "Unknown operation -- DNS PROVIDER" m))))
    dns-provider-dispatch))

;;Creates a provider using the symbol name and the list of servers
;; Put-dns is for adding into the hashmap
;; Providers is the list of all providers added to the system for evaluating the fastest
(define (add-provider name servers)
  (define p (make-dns-provider name servers))
  (put-dns name p)
  (set! providers (append providers (list p)))
  'ok)

;;
(define (get-name p) (p 'get-provider-name))
(define (get-all-providers) (map get-name providers))

;; Takes a provider as an argument, returns a list of all the times to complete
(define (get-times p) (map time-dns-request (p 'get-active-servers)))


;;;;       ;;;;
;; Analytics ;;
;;;;       ;;;;


;;Takes a string with the server's IP address and returns the
;;amount of milliseconds that it took to resolve using the server
(define (time-dns-request server)
  (define start 0)
  (define end 0)
  (set! start (current-milliseconds))
  (dns-get-address server "www.cs.uml.edu")
  (set! end (current-milliseconds))
  (- end start))

;; Given a provider, finds the fastest server's index and time
(define (fastest-server p)
  (display "Getting fastest server for ")
  (display (p 'get-provider-name))
  (display "...")
  
  (let ([times (get-times p)])
    
    (define (iter lst count) ;; Finds the fastest in the list, returns it's list index
      (cond ((fastest? (car lst)) (list (list-ref times count) (p 'get-provider-name) count))
          (else (iter (cdr lst) (+ 1 count)))))
    
    (define (fastest? s) (= s (min times)))
    (display "Done\n")
    (iter times 0)))


;; Takes a list of (time provider server-index) lists, returns the best
(define (fastest-provider lst)
  (let ([times (map car lst)])
    (define best (min times))
    (filter (lambda(x) (= (car x) best)) lst)))

;;This is the main driver of the appplication
(define (compare-providers)
  (let ([providers-best (map fastest-server providers)])
    (define indexes providers-best)
    (define best (car (fastest-provider indexes)))
    ;; Reference the symbol from the database to get the provider
    (define best-provider (get-dns (cadr best)))
    ;; Get the server at index given
    (define best-server ((best-provider 'get-nth-server) (caddr best)))
    (display "\n\n\t\tTime in milliseconds, Provider, Server Address\n\n")
    (display "\t\t")
    (display providers-best)
    (display "\n\t\t")
    (display "You should use ")
    (display (get-name best-provider))
    (display " ")
    (display best-server)
    (display "\n")))

;;(add-provider 'Comcast (list "68.87.76.178" "68.87.78.130"))
(add-provider 'Google  (list "8.8.8.8" "8.8.4.4"))
(add-provider 'OpenDNS (list "208.67.222.222" "208.67.220.220"))
(add-provider 'Verizon (list "4.2.2.1" "4.2.2.2"))
(add-provider 'Norton  (list "198.153.192.40" "198.153.194.40"))


(display "\n\n\n\n\n\n\n\n\n\n --BestDNS: Find the best DNS provider and server for you!--\n\n\n\n\n\n\n\n\n\n\n")
(display "Available providers are")
(get-all-providers)
(display "To find out who's the best for you run: (compare-providers)\n")

;;;;          ;;;;
;; Testing code ;;
;;;;          ;;;;

;; Testing 
(define google (get-dns 'Google))
(define verizon (get-dns 'Verizon))
;;(define comcast (get-dns 'Comcast))

;; Testing the add ability
;;((comcast 'add-server) "68.87.64.146")
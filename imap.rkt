#lang racket/base

(define imap-server "mail.---.com")
(define imap-port-no 993)
(define username "---")
(define pw "---")
(define mailbox-name "INBOX.Notes")
(define apple-tag "X-Uniform-Type-Identifier: com.apple.mail-note\r\n")

(require racket/list openssl/mzssl net/imap net/head)

(define (fastmail-connect)
  (let ([c (ssl-make-client-context)])
    (let-values ([(in out) (ssl-connect imap-server imap-port-no c)])
      (imap-connect* in out username pw mailbox-name))))

(define-values [imap messages recent] (fastmail-connect))

(define (send imap mailbox msg)
  (void (imap-get-expunges imap))
  (imap-append imap mailbox msg '()))

(define message-list
  (imap-get-messages
   imap
   (for/list ([i messages]) (add1 i))
   '(header body uid)))

(define count-substring
  (compose length regexp-match*))

(define uids-to-delete
  (for/list ([message message-list]
             #:when (eqv? 0 (count-substring "com.apple.mail-note" (car message))))
    (define header (bytes->string/utf-8 (car message)))
    (define body (bytes->string/utf-8 (cadr message)))
    (define final-message (string-append apple-tag header body))
    (send imap mailbox-name final-message)
    (caddr message)))

(cond
  [(empty? uids-to-delete) (println "Nothing to do actually")]
  [else
   (println "Finished tagging. Now marking for deletion:")
   (println uids-to-delete)
   (imap-store imap '+ uids-to-delete (list (symbol->imap-flag 'deleted)))
   (imap-expunge imap)])

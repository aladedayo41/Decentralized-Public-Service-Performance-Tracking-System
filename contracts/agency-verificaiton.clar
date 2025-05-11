;; Agency Verification Contract
;; This contract validates government entities

(define-data-var admin principal tx-sender)

;; Map to store verified agencies
(define-map verified-agencies principal
  {
    name: (string-utf8 100),
    verification-date: uint,
    status: (string-ascii 20)
  }
)

;; Check if caller is admin
(define-private (is-admin)
  (is-eq tx-sender (var-get admin))
)

;; Verify a new agency
(define-public (verify-agency (agency-principal principal) (agency-name (string-utf8 100)))
  (begin
    (asserts! (is-admin) (err u1))
    (asserts! (not (is-some (map-get? verified-agencies agency-principal))) (err u2))

    (map-set verified-agencies agency-principal
      {
        name: agency-name,
        verification-date: block-height,
        status: "active"
      }
    )
    (ok true)
  )
)

;; Revoke agency verification
(define-public (revoke-agency (agency-principal principal))
  (begin
    (asserts! (is-admin) (err u1))
    (asserts! (is-some (map-get? verified-agencies agency-principal)) (err u3))

    (map-set verified-agencies agency-principal
      (merge (unwrap-panic (map-get? verified-agencies agency-principal))
        { status: "revoked" }
      )
    )
    (ok true)
  )
)

;; Check if an agency is verified
(define-read-only (is-verified (agency-principal principal))
  (match (map-get? verified-agencies agency-principal)
    agency (is-eq (get status agency) "active")
    false
  )
)

;; Get agency details
(define-read-only (get-agency-details (agency-principal principal))
  (map-get? verified-agencies agency-principal)
)

;; Transfer admin rights
(define-public (transfer-admin (new-admin principal))
  (begin
    (asserts! (is-admin) (err u1))
    (var-set admin new-admin)
    (ok true)
  )
)

;; Service Registration Contract
;; Records details of public service offerings

(define-constant err-not-verified u1)
(define-constant err-unauthorized u2)
(define-constant err-already-exists u3)
(define-constant err-not-found u4)

;; Define trait for agency verification
(define-trait agency-verification-trait
  (
    (is-verified (principal) (response bool uint))
  )
)

;; Counter for service IDs
(define-data-var next-service-id uint u1)

;; Map to store services
(define-map services uint
  {
    name: (string-utf8 100),
    description: (string-utf8 500),
    agency: principal,
    category: (string-utf8 50),
    creation-date: uint,
    status: (string-ascii 20)
  }
)

;; Map to track services by agency
(define-map agency-services { agency: principal, service-id: uint } bool)

;; Register a new service
(define-public (register-service
    (agency-verification <agency-verification-trait>)
    (name (string-utf8 100))
    (description (string-utf8 500))
    (category (string-utf8 50)))
  (let
    (
      (agency tx-sender)
      (service-id (var-get next-service-id))
    )
    ;; Check if agency is verified
    (asserts! (unwrap! (contract-call? agency-verification is-verified agency) (err err-not-verified)) (err err-not-verified))

    ;; Create the service
    (map-set services service-id
      {
        name: name,
        description: description,
        agency: agency,
        category: category,
        creation-date: block-height,
        status: "active"
      }
    )

    ;; Track this service for the agency
    (map-set agency-services { agency: agency, service-id: service-id } true)

    ;; Increment the service ID counter
    (var-set next-service-id (+ service-id u1))

    (ok service-id)
  )
)

;; Update service details
(define-public (update-service
    (service-id uint)
    (name (string-utf8 100))
    (description (string-utf8 500))
    (category (string-utf8 50)))
  (let
    (
      (service (unwrap! (map-get? services service-id) (err err-not-found)))
    )
    ;; Check if caller is the service owner
    (asserts! (is-eq (get agency service) tx-sender) (err err-unauthorized))

    ;; Update the service
    (map-set services service-id
      (merge service
        {
          name: name,
          description: description,
          category: category
        }
      )
    )

    (ok true)
  )
)

;; Deactivate a service
(define-public (deactivate-service (service-id uint))
  (let
    (
      (service (unwrap! (map-get? services service-id) (err err-not-found)))
    )
    ;; Check if caller is the service owner
    (asserts! (is-eq (get agency service) tx-sender) (err err-unauthorized))

    ;; Update the service status
    (map-set services service-id
      (merge service { status: "inactive" })
    )

    (ok true)
  )
)

;; Get service details
(define-read-only (get-service (service-id uint))
  (map-get? services service-id)
)

;; Check if a service belongs to an agency
(define-read-only (is-agency-service (agency principal) (service-id uint))
  (default-to false (map-get? agency-services { agency: agency, service-id: service-id }))
)

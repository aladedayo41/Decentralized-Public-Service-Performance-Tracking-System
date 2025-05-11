;; Performance Metric Contract
;; Establishes measurement standards for services

(define-constant err-not-verified u1)
(define-constant err-unauthorized u2)
(define-constant err-already-exists u3)
(define-constant err-not-found u4)

;; Define traits for other contracts
(define-trait agency-verification-trait
  (
    (is-verified (principal) (response bool uint))
  )
)

(define-trait service-registration-trait
  (
    (is-agency-service (principal uint) (response bool uint))
  )
)

;; Counter for metric IDs
(define-data-var next-metric-id uint u1)

;; Map to store metrics
(define-map metrics uint
  {
    name: (string-utf8 100),
    description: (string-utf8 500),
    service-id: uint,
    unit: (string-utf8 20),
    target-value: uint,
    creation-date: uint,
    status: (string-ascii 20)
  }
)

;; Map to track metrics by service
(define-map service-metrics { service-id: uint, metric-id: uint } bool)

;; Create a new performance metric
(define-public (create-metric
    (agency-verification <agency-verification-trait>)
    (service-registration <service-registration-trait>)
    (service-id uint)
    (name (string-utf8 100))
    (description (string-utf8 500))
    (unit (string-utf8 20))
    (target-value uint))
  (let
    (
      (agency tx-sender)
      (metric-id (var-get next-metric-id))
    )
    ;; Check if agency is verified
    (asserts! (unwrap! (contract-call? agency-verification is-verified agency) (err err-not-verified)) (err err-not-verified))
    ;; Check if service belongs to the agency
    (asserts! (unwrap! (contract-call? service-registration is-agency-service agency service-id) (err err-unauthorized)) (err err-unauthorized))

    ;; Create the metric
    (map-set metrics metric-id
      {
        name: name,
        description: description,
        service-id: service-id,
        unit: unit,
        target-value: target-value,
        creation-date: block-height,
        status: "active"
      }
    )

    ;; Track this metric for the service
    (map-set service-metrics { service-id: service-id, metric-id: metric-id } true)

    ;; Increment the metric ID counter
    (var-set next-metric-id (+ metric-id u1))

    (ok metric-id)
  )
)

;; Update a metric
(define-public (update-metric
    (service-registration <service-registration-trait>)
    (metric-id uint)
    (name (string-utf8 100))
    (description (string-utf8 500))
    (unit (string-utf8 20))
    (target-value uint))
  (let
    (
      (metric (unwrap! (map-get? metrics metric-id) (err err-not-found)))
      (service-id (get service-id metric))
    )
    ;; Check if service belongs to the agency
    (asserts! (unwrap! (contract-call? service-registration is-agency-service tx-sender service-id) (err err-unauthorized)) (err err-unauthorized))

    ;; Update the metric
    (map-set metrics metric-id
      (merge metric
        {
          name: name,
          description: description,
          unit: unit,
          target-value: target-value
        }
      )
    )

    (ok true)
  )
)

;; Deactivate a metric
(define-public (deactivate-metric
    (service-registration <service-registration-trait>)
    (metric-id uint))
  (let
    (
      (metric (unwrap! (map-get? metrics metric-id) (err err-not-found)))
      (service-id (get service-id metric))
    )
    ;; Check if service belongs to the agency
    (asserts! (unwrap! (contract-call? service-registration is-agency-service tx-sender service-id) (err err-unauthorized)) (err err-unauthorized))

    ;; Update the metric status
    (map-set metrics metric-id
      (merge metric { status: "inactive" })
    )

    (ok true)
  )
)

;; Get metric details
(define-read-only (get-metric (metric-id uint))
  (map-get? metrics metric-id)
)

;; Check if a metric belongs to a service
(define-read-only (is-service-metric (service-id uint) (metric-id uint))
  (default-to false (map-get? service-metrics { service-id: service-id, metric-id: metric-id }))
)

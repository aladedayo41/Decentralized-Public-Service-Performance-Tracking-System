;; Reporting Contract
;; Generates authenticated performance disclosures

(define-constant err-not-verified u1)
(define-constant err-unauthorized u2)
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

(define-trait data-collection-trait
  (
    (get-data-entry (uint) (response (tuple (metric-id uint) (service-id uint) (agency principal) (value uint) (timestamp uint) (notes (string-utf8 200))) uint))
  )
)

;; Counter for report IDs
(define-data-var next-report-id uint u1)

;; Map to store reports
(define-map reports uint
  {
    title: (string-utf8 100),
    description: (string-utf8 500),
    service-id: uint,
    agency: principal,
    start-period: uint,
    end-period: uint,
    creation-date: uint,
    data-entries: (list 20 uint),
    status: (string-ascii 20)
  }
)

;; Map to track reports by service
(define-map service-reports { service-id: uint, report-id: uint } bool)

;; Generate a new performance report
(define-public (generate-report
    (agency-verification <agency-verification-trait>)
    (service-registration <service-registration-trait>)
    (data-collection <data-collection-trait>)
    (service-id uint)
    (title (string-utf8 100))
    (description (string-utf8 500))
    (start-period uint)
    (end-period uint)
    (data-entries (list 20 uint)))
  (let
    (
      (agency tx-sender)
      (report-id (var-get next-report-id))
    )
    ;; Check if agency is verified
    (asserts! (unwrap! (contract-call? agency-verification is-verified agency) (err err-not-verified)) (err err-not-verified))
    ;; Check if service belongs to the agency
    (asserts! (unwrap! (contract-call? service-registration is-agency-service agency service-id) (err err-unauthorized)) (err err-unauthorized))

    ;; Create the report
    (map-set reports report-id
      {
        title: title,
        description: description,
        service-id: service-id,
        agency: agency,
        start-period: start-period,
        end-period: end-period,
        creation-date: block-height,
        data-entries: data-entries,
        status: "published"
      }
    )

    ;; Track this report for the service
    (map-set service-reports { service-id: service-id, report-id: report-id } true)

    ;; Increment the report ID counter
    (var-set next-report-id (+ report-id u1))

    (ok report-id)
  )
)

;; Update report status
(define-public (update-report-status
    (service-registration <service-registration-trait>)
    (report-id uint)
    (new-status (string-ascii 20)))
  (let
    (
      (report (unwrap! (map-get? reports report-id) (err err-not-found)))
      (service-id (get service-id report))
    )
    ;; Check if service belongs to the agency
    (asserts! (unwrap! (contract-call? service-registration is-agency-service tx-sender service-id) (err err-unauthorized)) (err err-unauthorized))

    ;; Update the report status
    (map-set reports report-id
      (merge report { status: new-status })
    )

    (ok true)
  )
)

;; Get report details
(define-read-only (get-report (report-id uint))
  (map-get? reports report-id)
)

;; Check if a report belongs to a service
(define-read-only (is-service-report (service-id uint) (report-id uint))
  (default-to false (map-get? service-reports { service-id: service-id, report-id: report-id }))
)

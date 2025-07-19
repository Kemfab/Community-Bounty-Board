;; Digital Art Marketplace: Decentralized platform for artists to showcase and sell digital artwork
;; Enables artists to mint artwork, collectors to purchase, and curators to authenticate pieces

(define-data-var master-curator principal tx-sender)

(define-map artwork-registry
  { artwork-id: uint }
  {
    artist: principal,
    price-micro-stx: uint,
    title: (string-ascii 50),
    description: (string-ascii 500),
    creation-timestamp: uint,
    authenticated: bool
  })

(define-map transaction-history
  { artwork-id: uint, transaction-id: uint }
  {
    buyer: principal,
    purchase-timestamp: uint,
    status: (string-ascii 20)
  })

(define-data-var next-artwork-id uint u1)

(define-map transaction-counter
  { artwork-id: uint }
  { count: uint })

;; Mint new digital artwork
(define-public (mint-artwork (title-input (string-ascii 50)) (description-input (string-ascii 500)) (creation-time uint) (price-input uint))
  (let
    (
      (artwork-id (var-get next-artwork-id))
      (transaction-id u0)
      (title title-input)
      (description description-input)
      (timestamp creation-time)
      (price price-input)
    )
    ;; Input validation
    (asserts! (> price u0) (err u1))
    (asserts! (> (len title) u0) (err u5))
    (asserts! (> (len description) u0) (err u6))
    (asserts! (> timestamp u0) (err u7))
    
    (map-set artwork-registry
      { artwork-id: artwork-id }
      {
        artist: tx-sender,
        price-micro-stx: price,
        title: title,
        description: description,
        creation-timestamp: timestamp,
        authenticated: false
      }
    )
    (map-set transaction-history
      { artwork-id: artwork-id, transaction-id: transaction-id }
      {
        buyer: tx-sender,
        purchase-timestamp: artwork-id,
        status: "minted"
      }
    )
    (map-set transaction-counter
      { artwork-id: artwork-id }
      { count: u1 }
    )
    (var-set next-artwork-id (+ artwork-id u1))
    (ok artwork-id)
  ))

;; Purchase digital artwork
(define-public (purchase-artwork (artwork-id-input uint))
  (let
    (
      (artwork-id artwork-id-input)
      (artwork-info (unwrap! (map-get? artwork-registry { artwork-id: artwork-id }) (err u2)))
      (price (get price-micro-stx artwork-info))
      (artist (get artist artwork-info))
      (transaction-data (default-to { count: u0 } (map-get? transaction-counter { artwork-id: artwork-id })))
      (transaction-id (get count transaction-data))
      (new-transaction-id (+ transaction-id u1))
    )
    ;; Input validation
    (asserts! (> artwork-id u0) (err u8))
    (asserts! (not (is-eq tx-sender artist)) (err u3))
    
    (try! (stx-transfer? price tx-sender artist))
    (map-set transaction-history
      { artwork-id: artwork-id, transaction-id: transaction-id }
      {
        buyer: tx-sender,
        purchase-timestamp: (var-get next-artwork-id),
        status: "purchased"
      }
    )
    (map-set transaction-counter
      { artwork-id: artwork-id }
      { count: new-transaction-id }
    )
    (ok true)
  ))

;; Authenticate artwork (master curator only)
(define-public (authenticate-artwork (artwork-id-input uint))
  (let
    (
      (artwork-id artwork-id-input)
      (artwork-info (unwrap! (map-get? artwork-registry { artwork-id: artwork-id }) (err u2)))
      (transaction-data (default-to { count: u0 } (map-get? transaction-counter { artwork-id: artwork-id })))
      (transaction-id (get count transaction-data))
      (new-transaction-id (+ transaction-id u1))
    )
    ;; Input validation
    (asserts! (> artwork-id u0) (err u8))
    (asserts! (is-eq tx-sender (var-get master-curator)) (err u4))
    
    (map-set artwork-registry
      { artwork-id: artwork-id }
      (merge artwork-info { authenticated: true })
    )
    (map-set transaction-history
      { artwork-id: artwork-id, transaction-id: transaction-id }
      {
        buyer: (get artist artwork-info),
        purchase-timestamp: (var-get next-artwork-id),
        status: "authenticated"
      }
    )
    (map-set transaction-counter
      { artwork-id: artwork-id }
      { count: new-transaction-id }
    )
    (ok true)
  ))

;; Get artwork details
(define-read-only (get-artwork (artwork-id uint))
  (map-get? artwork-registry { artwork-id: artwork-id }))

;; Get transaction history entry
(define-read-only (get-transaction-history (artwork-id uint) (transaction-id uint))
  (map-get? transaction-history { artwork-id: artwork-id, transaction-id: transaction-id }))

;; Get total transactions for artwork
(define-read-only (get-transaction-count (artwork-id uint))
  (let
    (
      (transaction-data (default-to { count: u0 } (map-get? transaction-counter { artwork-id: artwork-id })))
    )
    (get count transaction-data)
  ))

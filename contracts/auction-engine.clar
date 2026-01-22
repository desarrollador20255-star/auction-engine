;; ============================================================
;; Contract: auction-engine
;; Purpose : Time-based auctions with bid refunds
;; ============================================================

;; ------------------------------------------------------------
;; Error Codes
;; ------------------------------------------------------------
(define-constant ERR-NOT-SELLER          (err u1500))
(define-constant ERR-AUCTION-NOT-FOUND   (err u1501))
(define-constant ERR-AUCTION-ENDED       (err u1502))
(define-constant ERR-BID-TOO-LOW         (err u1503))
(define-constant ERR-NOT-HIGHEST-BIDDER  (err u1504))
(define-constant ERR-AUCTION-ACTIVE      (err u1505))
(define-constant ERR-ALREADY-SETTLED     (err u1506))
(define-constant ERR-INVALID-PARAMS      (err u1507))

;; ------------------------------------------------------------
;; Data Storage
;; ------------------------------------------------------------

(define-data-var auction-counter uint u0)

(define-map auctions
  { id: uint }
  {
    seller: principal,
    reserve: uint,
    start-height: uint,
    end-height: uint,
    highest-bid: uint,
    highest-bidder: (optional principal),
    settled: bool
  }
)

(define-map refunds
  { auction-id: uint, bidder: principal }
  { amount: uint }
)

;; ------------------------------------------------------------
;; Read-Only Functions
;; ------------------------------------------------------------

(define-read-only (get-auction (id uint))
  (map-get? auctions { id: id })
)

(define-read-only (is-active (id uint))
  (match (map-get? auctions { id: id })
    a
    (and
      (>= stacks-block-height (get start-height a))
      (< stacks-block-height (get end-height a))
      (not (get settled a))
    )
    false
  )
)

;; ------------------------------------------------------------
;; Public Functions - Auction Creation
;; ------------------------------------------------------------

(define-public (create-auction
  (reserve uint)
  (duration uint)
)
  (begin
    (asserts! (> duration u0) ERR-INVALID-PARAMS)

    (let ((id (+ (var-get auction-counter) u1)))
      (var-set auction-counter id)

      (map-set auctions
        { id: id }
        {
          seller: tx-sender,
          reserve: reserve,
          start-height: stacks-block-height,
          end-height: (+ stacks-block-height duration),
          highest-bid: u0,
          highest-bidder: none,
          settled: false
        }
      )

      (ok id)
    )
  )
)

;; ------------------------------------------------------------
;; Public Functions - Bidding
;; ------------------------------------------------------------

;; Added bid-amount parameter since (amount-ustx) doesn't exist in Clarity
(define-public (bid (id uint) (bid-amount uint))
  (match (map-get? auctions { id: id })
    a
    (begin
      (asserts! (is-active id) ERR-AUCTION-ENDED)
      (asserts! (> bid-amount (get highest-bid a)) ERR-BID-TOO-LOW)

      ;; Refund previous highest bidder
      (match (get highest-bidder a)
        prev
        (map-set refunds
          { auction-id: id, bidder: prev }
          { amount: (get highest-bid a) }
        )
        true
      )

      ;; Accept new bid
      (try!
        (stx-transfer?
          bid-amount
          tx-sender
          (as-contract tx-sender)
        )
      )

      (map-set auctions
        { id: id }
        {
          seller: (get seller a),
          reserve: (get reserve a),
          start-height: (get start-height a),
          end-height: (get end-height a),
          highest-bid: bid-amount,
          highest-bidder: (some tx-sender),
          settled: false
        }
      )

      (ok true)
    )
    ERR-AUCTION-NOT-FOUND
  )
)

;; ------------------------------------------------------------
;; Public Functions - Refunds
;; ------------------------------------------------------------

(define-public (withdraw-refund (id uint))
  (match (map-get? refunds { auction-id: id, bidder: tx-sender })
    r
    (begin
      (try!
        (stx-transfer?
          (get amount r)
          (as-contract tx-sender)
          tx-sender
        )
      )

      (map-delete refunds { auction-id: id, bidder: tx-sender })

      (ok (get amount r))
    )
    ERR-NOT-HIGHEST-BIDDER
  )
)

;; ------------------------------------------------------------
;; Public Functions - Settlement
;; ------------------------------------------------------------

(define-public (settle (id uint))
  (match (map-get? auctions { id: id })
    a
    (begin
      (asserts! (>= stacks-block-height (get end-height a)) ERR-AUCTION-ACTIVE)
      (asserts! (not (get settled a)) ERR-ALREADY-SETTLED)

      ;; Transfer funds to seller if reserve met
      (if (>= (get highest-bid a) (get reserve a))
        (try!
          (stx-transfer?
            (get highest-bid a)
            (as-contract tx-sender)
            (get seller a)
          )
        )
        true
      )

      (map-set auctions
        { id: id }
        {
          seller: (get seller a),
          reserve: (get reserve a),
          start-height: (get start-height a),
          end-height: (get end-height a),
          highest-bid: (get highest-bid a),
          highest-bidder: (get highest-bidder a),
          settled: true
        }
      )

      (ok true)
    )
    ERR-AUCTION-NOT-FOUND
  )
)

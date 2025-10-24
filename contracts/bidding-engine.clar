;; Bidding Engine - Simple STX-only auctions
;; STX-only auctions with reserve price support
;; Participants bid with STX and can only win if their bid meets reserve price

;; Bidding Engine - Simple STX-only auctions
;; STX-only auctions with reserve price support
;; Participants bid with STX and can only win if their bid meets reserve price

;; Error codes
(define-constant ERR-NOT-FOUND (err u100))
(define-constant ERR-INVALID-BID (err u101))
(define-constant ERR-AUCTION-ENDED (err u102))
(define-constant ERR-AUCTION-NOT-ENDED (err u103))
(define-constant ERR-ONLY-SELLER (err u104))

;; Data vars
(define-data-var next-auction-id uint u1)

;; Map to track all auctions
(define-map auctions uint {
  seller: principal,
  start-block: uint,
  end-block: uint,
  reserve-price: uint,
  highest-bid: uint,
  highest-bidder: (optional principal),
  settled: bool
})

;; Public functions

;; Get current block 
(define-read-only (get-current-block)
  u100) ;; Placeholder for block height - in real contract this would be provided by the blockchain

;; Create a new auction 
(define-public (create-auction (duration uint) (reserve-price uint))
  (let ((auction-id (var-get next-auction-id))
        (current-block (get-current-block))) 
    (begin
      (asserts! (> duration u0) ERR-INVALID-BID)
      (asserts! (>= reserve-price u0) ERR-INVALID-BID)
      (map-set auctions auction-id {
        seller: tx-sender,
        start-block: current-block,
        end-block: (+ current-block duration),
        reserve-price: reserve-price,
        highest-bid: u0,
        highest-bidder: none,
        settled: false
      })
      (var-set next-auction-id (+ auction-id u1))
      (ok auction-id))))

;; Place bid on an auction
(define-public (place-bid (auction-id uint))
  (let ((auction (unwrap! (map-get? auctions auction-id) ERR-NOT-FOUND))
        (bid-amount (stx-get-balance tx-sender))
        (current-block (get-current-block)))
    (begin
      (asserts! (< current-block (get end-block auction)) ERR-AUCTION-ENDED)
      (asserts! (> bid-amount (get highest-bid auction)) ERR-INVALID-BID)
      
      ;; Refund previous bidder if exists
      (match (get highest-bidder auction) prev-bidder 
        (try! (as-contract (stx-transfer? (get highest-bid auction) tx-sender prev-bidder)))
        true)
      
      ;; Record new bid
      (map-set auctions auction-id (merge auction {
        highest-bid: bid-amount,
        highest-bidder: (some tx-sender)
      }))
      (ok true))))

;; End auction and settle bids
(define-public (end-auction (auction-id uint))
  (let ((auction (unwrap! (map-get? auctions auction-id) ERR-NOT-FOUND))
        (current-block (get-current-block)))
    (begin
      (asserts! (>= current-block (get end-block auction)) ERR-AUCTION-NOT-ENDED)
      (asserts! (not (get settled auction)) ERR-AUCTION-ENDED)
      (asserts! (is-eq tx-sender (get seller auction)) ERR-ONLY-SELLER)
      
      (match (get highest-bidder auction) winner
        (if (>= (get highest-bid auction) (get reserve-price auction))
          (begin
            (try! (as-contract (stx-transfer? (get highest-bid auction) tx-sender (get seller auction))))
            (map-set auctions auction-id (merge auction { settled: true }))
            (ok true))
          (begin
            (try! (as-contract (stx-transfer? (get highest-bid auction) tx-sender winner)))
            (map-set auctions auction-id (merge auction { settled: true }))
            (ok true)))
        (ok true)))))

;; Read-only functions

;; Get auction details
(define-read-only (get-auction (auction-id uint))
  (ok (map-get? auctions auction-id)))

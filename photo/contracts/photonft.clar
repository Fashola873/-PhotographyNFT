;; Photography NFT Gallery
;; Curated marketplace for photography NFT collections

(define-constant GALLERY_CURATOR tx-sender)
(define-constant ERR_UNAUTHORIZED_ACCESS (err u900))
(define-constant ERR_PHOTO_NOT_AVAILABLE (err u901))
(define-constant ERR_EXHIBITION_CONFLICT (err u902))
(define-constant ERR_TRANSACTION_FAILED (err u903))
(define-constant ERR_PRICING_ERROR (err u904))
(define-constant ERR_SELF_BIDDING_BLOCKED (err u905))
(define-constant ERR_PHOTOGRAPHER_MISMATCH (err u906))
(define-constant ERR_VALIDATION_ERROR (err u907))
(define-constant ERR_BID_DEADLINE_PASSED (err u908))

;; Photography exhibition listings
(define-map photo-gallery uint {
    photo-contract: principal,
    photo-id: uint,
    photographer: principal,
    exhibition-price: uint,
    on-display: bool,
    exhibition-date: uint
})

;; Collector bids on photographs
(define-map collector-bids { gallery-id: uint, collector: principal } {
    bid-value: uint,
    expiration: uint,
    active: bool
})

;; Photographer and collector analytics
(define-map gallery-analytics principal {
    photos-exhibited: uint,
    sales-revenue: uint,
    current-exhibitions: uint
})

;; Gallery configuration
(define-data-var curation-fee uint u350) ;; 3.5% in basis points
(define-data-var next-gallery-id uint u1)
(define-data-var gallery-volume uint u0)

;; Exhibit photography NFT
(define-public (exhibit-photography (photo-contract <photography-nft-trait>) (photo-id uint) (exhibition-price uint))
    (let ((gallery-id (var-get next-gallery-id))
          (photographer tx-sender)
          (contract-principal (contract-of photo-contract)))
        
        ;; Validate exhibition parameters
        (asserts! (> exhibition-price u0) ERR_PRICING_ERROR)
        (asserts! (<= exhibition-price u2200000000) ERR_PRICING_ERROR)
        (asserts! (<= photo-id u4294967295) ERR_VALIDATION_ERROR)
        
        ;; Verify photo ownership
        (asserts! (is-some (unwrap-panic (contract-call? photo-contract get-owner photo-id))) ERR_PHOTOGRAPHER_MISMATCH)
        
        ;; Create gallery exhibition
        (map-set photo-gallery gallery-id {
            photo-contract: contract-principal,
            photo-id: photo-id,
            photographer: photographer,
            exhibition-price: exhibition-price,
            on-display: true,
            exhibition-date: stacks-block-height
        })
        
        ;; Update photographer analytics
        (let ((photographer-data (default-to { photos-exhibited: u0, sales-revenue: u0, current-exhibitions: u0 }
                                             (map-get? gallery-analytics photographer))))
            (map-set gallery-analytics photographer
                (merge photographer-data { current-exhibitions: (+ (get current-exhibitions photographer-data) u1) }))
        )
        
        (var-set next-gallery-id (+ gallery-id u1))
        (ok gallery-id)
    ))

;; Purchase exhibited photography
(define-public (purchase-photography (gallery-id uint))
    (let ((exhibition (unwrap! (map-get? photo-gallery gallery-id) ERR_PHOTO_NOT_AVAILABLE))
          (collector tx-sender)
          (price (get exhibition-price exhibition))
          (artist (get photographer exhibition))
          (curation-cost (/ (* price (var-get curation-fee)) u10000))
          (artist-earnings (- price curation-cost)))
        
        (asserts! (get on-display exhibition) ERR_PHOTO_NOT_AVAILABLE)
        (asserts! (not (is-eq collector artist)) ERR_SELF_BIDDING_BLOCKED)
        
        ;; Execute purchase
        (try! (stx-transfer? artist-earnings collector artist))
        (try! (stx-transfer? curation-cost collector GALLERY_CURATOR))
        
        ;; Update exhibition status
        (map-set photo-gallery gallery-id (merge exhibition { on-display: false }))
        
        ;; Update analytics
        (let ((artist-data (default-to { photos-exhibited: u0, sales-revenue: u0, current-exhibitions: u0 }
                                       (map-get? gallery-analytics artist)))
              (collector-data (default-to { photos-exhibited: u0, sales-revenue: u0, current-exhibitions: u0 }
                                          (map-get? gallery-analytics collector))))
            (map-set gallery-analytics artist
                (merge artist-data {
                    photos-exhibited: (+ (get photos-exhibited artist-data) u1),
                    sales-revenue: (+ (get sales-revenue artist-data) price),
                    current-exhibitions: (- (get current-exhibitions artist-data) u1)
                }))
            (map-set gallery-analytics collector
                (merge collector-data {
                    sales-revenue: (+ (get sales-revenue collector-data) price)
                }))
        )
        
        (var-set gallery-volume (+ (var-get gallery-volume) price))
        (ok true)
    ))

;; Place bid on photography
(define-public (place_photography_bid (gallery-id uint) (bid-value uint) (bid-duration uint))
    (let ((exhibition (unwrap! (map-get? photo-gallery gallery-id) ERR_PHOTO_NOT_AVAILABLE))
          (collector tx-sender))
        
        (asserts! (> bid-value u0) ERR_PRICING_ERROR)
        (asserts! (<= bid-value u2200000000) ERR_PRICING_ERROR)
        (asserts! (> bid-duration u0) ERR_VALIDATION_ERROR)
        (asserts! (<= bid-duration u144000) ERR_VALIDATION_ERROR)
        
        (let ((expiration (+ stacks-block-height bid-duration)))
            (asserts! (get on-display exhibition) ERR_PHOTO_NOT_AVAILABLE)
            (asserts! (not (is-eq collector (get photographer exhibition))) ERR_SELF_BIDDING_BLOCKED)
            
            ;; Secure bid amount
            (try! (stx-transfer? bid-value collector (as-contract tx-sender)))
            
            ;; Register bid
            (map-set collector-bids { gallery-id: gallery-id, collector: collector } {
                bid-value: bid-value,
                expiration: expiration,
                active: true
            })
            
            (ok true)
        )
    ))

;; Accept collector bid
(define-public (accept-collector-bid (gallery-id uint) (collector principal))
    (let ((exhibition (unwrap! (map-get? photo-gallery gallery-id) ERR_PHOTO_NOT_AVAILABLE))
          (photographer tx-sender)
          (bid-key { gallery-id: gallery-id, collector: collector }))
        
        (asserts! (> gallery-id u0) ERR_VALIDATION_ERROR)
        (asserts! (<= gallery-id (var-get next-gallery-id)) ERR_VALIDATION_ERROR)
        
        (let ((bid (unwrap! (map-get? collector-bids bid-key) ERR_PHOTO_NOT_AVAILABLE))
              (amount (get bid-value bid))
              (curation-cost (/ (* amount (var-get curation-fee)) u10000))
              (artist-earnings (- amount curation-cost)))
            
            (asserts! (is-eq photographer (get photographer exhibition)) ERR_UNAUTHORIZED_ACCESS)
            (asserts! (get on-display exhibition) ERR_PHOTO_NOT_AVAILABLE)
            (asserts! (get active bid) ERR_PHOTO_NOT_AVAILABLE)
            (asserts! (<= stacks-block-height (get expiration bid)) ERR_BID_DEADLINE_PASSED)
            
            ;; Release from escrow
            (try! (as-contract (stx-transfer? artist-earnings tx-sender photographer)))
            (try! (as-contract (stx-transfer? curation-cost tx-sender GALLERY_CURATOR)))
            
            ;; Update exhibition and bid
            (map-set photo-gallery gallery-id (merge exhibition { on-display: false }))
            (map-set collector-bids bid-key (merge bid { active: false }))
            
            (var-set gallery-volume (+ (var-get gallery-volume) amount))
            (ok true)
        )
    ))

;; Remove photography exhibition
(define-public (remove-exhibition (gallery-id uint))
    (let ((exhibition (unwrap! (map-get? photo-gallery gallery-id) ERR_PHOTO_NOT_AVAILABLE)))
        (asserts! (is-eq tx-sender (get photographer exhibition)) ERR_UNAUTHORIZED_ACCESS)
        (asserts! (get on-display exhibition) ERR_PHOTO_NOT_AVAILABLE)
        
        (map-set photo-gallery gallery-id (merge exhibition { on-display: false }))
        (ok true)
    ))

;; Withdraw expired bid
(define-public (withdraw-expired-bid (gallery-id uint))
    (let ((bid-key { gallery-id: gallery-id, collector: tx-sender }))
        (asserts! (> gallery-id u0) ERR_VALIDATION_ERROR)
        (asserts! (<= gallery-id (var-get next-gallery-id)) ERR_VALIDATION_ERROR)
        
        (let ((bid (unwrap! (map-get? collector-bids bid-key) ERR_PHOTO_NOT_AVAILABLE)))
            (asserts! (get active bid) ERR_PHOTO_NOT_AVAILABLE)
            (asserts! (> stacks-block-height (get expiration bid)) ERR_UNAUTHORIZED_ACCESS)
            
            (try! (as-contract (stx-transfer? (get bid-value bid) tx-sender tx-sender)))
            (map-set collector-bids bid-key (merge bid { active: false }))
            (ok (get bid-value bid))
        )
    ))

;; Update curation fee (curator only)
(define-public (set-curation-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender GALLERY_CURATOR) ERR_UNAUTHORIZED_ACCESS)
        (asserts! (<= new-fee u1000) ERR_PRICING_ERROR)
        (var-set curation-fee new-fee)
        (ok new-fee)
    ))

;; Read-only functions
(define-read-only (get-photo-exhibition (gallery-id uint))
    (map-get? photo-gallery gallery-id))

(define-read-only (get-collector-bid (gallery-id uint) (collector principal))
    (map-get? collector-bids { gallery-id: gallery-id, collector: collector }))

(define-read-only (get-gallery-stats (user principal))
    (map-get? gallery-analytics user))

(define-read-only (get-curation-fee)
    (var-get curation-fee))

(define-read-only (get-gallery-volume)
    (var-get gallery-volume))

(define-read-only (calculate-curation-fees (price uint))
    (let ((curation-cost (/ (* price (var-get curation-fee)) u10000)))
        {
            curation-fee: curation-cost,
            artist-earnings: (- price curation-cost)
        }
    ))

;; Photography NFT trait definition
(define-trait photography-nft-trait
    (
        (get-owner (uint) (response (optional principal) uint))
        (transfer (uint principal principal) (response bool uint))
    ))

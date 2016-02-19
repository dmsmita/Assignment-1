"Assignment 1 Grades
1 bill-total	10/10
2 add-to-bill	10/10
3 make-poly	10/10
4 differentiate	10/10
5 find-root	10/10
6	10/10

Total	60/60 -3 late = 57
"

;; Question 1
(defn calc_item_price
  [item]
  (*(:price item) (:quantity item))            )


(defn bill-total
  [list]
   (if (empty? list)
    ( bigint 0N)
    (+ (calc_item_price (first list)) (bill-total (rest list)))))


;; Question 2
(defn add-to-bill
  [bill items]

  (if (empty? items)
   (bill)

    (if (empty? bill)
      (items)

      (let
        [concat_items (into [] (concat bill items))]
        (
           ->> concat_items (group-by :name)
           (map
            (fn [[item-name item]]
              {
                 :name item-name
                 :quantity (apply + (map :quantity item))
                 :price (get (first item) :price)
              }
            )
           )
        )
      )
    )
  )
)

;; Question 3

(defn eval-expr
  [expr valueOfX]

    (double (* (first expr ) (java.lang.Math/pow valueOfX (second expr))))
)

(defn make-poly
  [v]

  (defn polynomial [y]

    (loop [v v accumulator 0]

      (if (= (seq v) nil)
        (double accumulator)

        (recur (rest v) (+ accumulator (eval-expr (first v) y)))
      )
    )
  )
)


;; Question 4

(defn eval-derivative
  [x]

  (def coeff (first x))
  (def exponent (second x))

  (if (> exponent 0)
    ( vector (* coeff exponent) (- exponent 1))

  )
)


(defn differentiate
  [v]

  (loop [v v accumulator (vector) ]

    (if (= (seq v) nil)
      (into [] (filter identity accumulator))

      (recur (rest v)  (conj accumulator (eval-derivative (first v))))

    )
  )
)

;; Question 5

(defn calc-xn [xn_1 v derivate]

  (double (- xn_1 (double (/((make-poly v) xn_1) ((make-poly derivate) xn_1)))))
)


(defn find-root
  [delta poly x0]

  (def derivate (differentiate poly))

  (def x1 (calc-xn x0 poly derivate) )

  (loop [xn_1 x0 xn x1]


    (def abs_val (java.lang.Math/abs (- xn xn_1 )))

    (if (<=  abs_val delta)
      (double xn)

      (recur xn (calc-xn xn poly derivate))
    )

  )
)

;;Question 6

(def account-balance (atom 0))

(defn deposit
  [amount]

  (if (> amount 0)
    (reset! account-balance (+ @account-balance amount))

    (throw (Exception. "Deposit amount should greater than zero"))
  )
)

(defn withdraw
  [amount]

  (if (>= @account-balance amount)

    (reset! account-balance (- @account-balance amount))

    (throw (Exception. "Insufficient funds"))
  )
)

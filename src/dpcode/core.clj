(ns dpcode.core
  (:require [gorilla-plot.core :refer :all]
            [couchbase-clj.client :as cc]))

(cc/defclient cb {:bucket "znetroyalty"
                   :uris ["http://127.0.0.1:8091/pools"]})

(defn cbquery
  ([design-name view-name]
   (cbquery design-name view-name {}))
  ([design-name view-name opt-map]
   (->> (assoc opt-map :include-docs true)
        (cc/query cb design-name view-name)
        (map cc/view-doc-json))))

(defn get-content
  [cc]
  (if-let [data (cc/get-json cb (keyword (str "content-" cc)))]
    (reduce + (vals (:hits data)))
    nil))

(defn all-content
  [lim]
  (keep get-content (range 50 lim)))

(defn what-ever
  [design views]
  (sequence
    (comp (map :hits)
          (map vals)
          (map #(apply + %)))
    (cbquery design views)))


(defn square
  [x]
  (* x x))

(defn cube
  [x]
  (* x x x))

(defn poly1
  [x]
  (- (* 3 (cube x))
     (* 9 (square x))
     (* 34 x)
     109))

(defn start-all
  [start end]
  (map square (range start end)))

(defn prime?
  [^long n]
  (cond (< n 2) false
        (== n 2) true
        (even? n) false
        :else (loop [i 3]
                (if (> (* i i) n)
                  true
                  (if (== 0 (rem n i))
                    false
                    (recur (+ i 2)))))))

;; totients :: Int -> [Int]
(defn ^longs totients
  "Returns the totient value of all positive numbers less than lim"
  [^long lim]
  (let [refs (boolean-array (+ lim 1) true)
        tots (into-array (range (+ lim 1)))]
    (do (doseq [i (range 2 (+ lim 1))
                :when (aget refs i)]
          (do (aset tots i (- i 1))
              (doseq [j (range (* 2 i) (+ lim 1) i)]
                (do (aset refs j false)
                    (aset tots j (quot (* (aget tots j) (- i 1)) i))))))
        (map-indexed #(if (zero? %1) %2 (/ %2 %1)) (into [] tots)))))

(defn prime-distributions
  [start end step]
  (map #(count (filter prime? %))
       (partition step (range start (+ end 1)))))



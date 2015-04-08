(defn bsearch
  ([coll t]
   (bsearch coll 0 (dec (count coll)) t))
  ([coll l u t]
   (if (> l u) -1
       (let [m (quot (+ l u) 2) mth (nth coll m)]
         (cond
           (> mth t) (recur coll l (dec m) t)
           (< mth t) (recur coll (inc m) u t)
           (= mth t) m)))))

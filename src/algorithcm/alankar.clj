(ns algorithcm.alankar
  (:use overtone.core)
  (:use overtone.music.pitch)
  (:use overtone.inst.synth))

;(connect 21998)

;(status)

(defn- incr [inseq]
  (map inc inseq))

(defn- decr [inseq]
  (map dec inseq))

(def oct3 (concat (scale :c3 :major) (rest (scale :c4 :major)) (rest (scale :c5 :major))))

(defn extrapolate [inseq]
  (let [aaroh  (mapcat #(into [] %) (take-while #(>= 7 (last %))  (iterate incr inseq)))
        avroh (mapcat #(into [] %) (take-while #(<= -7 (last %))  (iterate decr (map #(- 0 %)  inseq))))]
  (concat (map #(nth oct3 (+ 7 %)) aaroh) (map #(nth oct3 (+ 14 %)) avroh  ))))


(defn playseq [iseq]
  (map #(do
          (Thread/sleep 250)
          (tb303 %)) iseq)) 


;(playseq (extrapolate '(0 1 0 2 2 2 1 0)))



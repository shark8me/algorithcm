(ns algorithcm.alankar
  (:use overtone.core)
  (:use overtone.music.pitch)
  (:use overtone.inst.synth))

(use 'overtone.inst.synth)
(connect 21998)

(status)

(def SEVEN 7)
(def oct3 (concat (scale :c3 :major) 
                  (rest (scale :c4 :major)) 
                  (rest (scale :c5 :major))))

;some standard alankars
;
(def standard_alankars ["s r g" "s r g m" "s r g m p" 
                        "r g s -" "s g r g s" "s g r s"
                        "r _n s" "s r g g" "s r s g r s"])
(defn- dofn [f n ]
  (if (string? n) n (f n)))
  
(defn- incr [inseq]
  (map #(dofn inc %) inseq))

(defn- decr [inseq]
  (map #(dofn dec %) inseq))

(defn- lastShruti [iseq]
  (first (filter number? (reverse iseq))))


(defn extrapolate [inseq]
  (let [aaroh  (mapcat #(into [] %) 
                       (take-while #(>= SEVEN (lastShruti %))  (iterate incr inseq)))
        avroh (mapcat #(into [] %) 
                      (take-while #(<= -7 (lastShruti %))  
                                  (iterate decr (map #(dofn (fn [x] (- 0 x)) %)  inseq))))]
  (concat (map #(dofn (fn [x] (nth oct3 (+ SEVEN x))) %) aaroh) 
          (map #(dofn (fn [x] (nth oct3 (+ 14 x))) %) avroh  ))))


(defn playseq [iseq]
  (map #(do
          (Thread/sleep 250)
          (dofn  tb303 %)) iseq)) 

(def swaramap {"_s" -7 "_r" -6 "_g" -5 "_m" -4 "_p" -3 "_d" -2 "_n" -1
               "s" 0 "r" 1 "g" 2 "m" 3 "p" 4 "d" 5 "n" 6 
               "S" 7 "R" 8 "G" 9 "M" 10 "P" 11 "D" 12 "N" 13
               "-" "-"})

(defn getSwaranum [ swarastr]
  (map #(swaramap %) (seq (. swarastr split " "))))

(map #(playseq (extrapolate (getSwaranum %))) standard_alankars)






(ns algorithcm.leipzig-trial
  (:require [overtone.live :as overtone]
            [overtone.inst.piano :refer [piano]]
            [clojure.string :as str]
            [leipzig.melody :refer [bpm is phrase then times where with]]
            [leipzig.scale :as scale]
            [leipzig.canon :as canon]
            [leipzig.chord :as chord]
            [leipzig.live :as live]))

(def melody
         ; Row,  row,  row   your  boat
  (phrase [3/3   3/3   2/3   1/3   3/3]
          [  0     0     0     1     2]))

melody
(def s1 "p n s n - - - - p n s n - - - -")
(def smap {"p" 0 "d" 2 "n" 3 "s" 4 "-" 4})
(def m1
(let [inp (str/split s1 #" ")
      timing (vec (for [i inp] 1/4))]
  (phrase timing (mapv smap inp))
  ))

(chord/root 0)
(def s2
(let [ t (vec (for [i (range 22)] 2/4))]
  (phrase t (vec (range 22)))))

(overtone/definst beep [freq 440 dur 1.0]
  (-> freq
      overtone/saw
      (* (overtone/env-gen (overtone/perc 0.05 dur) :action overtone/FREE))))

(defmethod live/play-note :default [{midi :pitch seconds :duration}]
  (-> midi overtone/midi->hz (beep seconds)))

(map overtone/midi->hz (range 60 73))
(overtone/scale :c4 :major)
(->>
  ;(phrase [ 2/4 2/4 2/4 2/4 2/4 2/4 2/4] [ 0 1 2 3 4 5 6])
 s2
  (where :time (bpm 130))
  (where :duration (bpm 90))
  (where :pitch (comp scale/C scale/chromatic))
  live/play)

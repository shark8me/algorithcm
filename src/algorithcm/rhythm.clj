(ns algorithcm.rhythm
  (:use [overtone.live]
        [overtone.inst.drum :only [quick-kick haziti-clap soft-hat open-hat]]
        [overtone.synth.stringed :only [guitar-strum]]
        [overtone.examples.instruments.guitar-synth :only [strum-pattern]]
        ))

(def m (metronome 160))

(def clap1 (sample (freesound-path 15389)))
(defn player
  [beat]
  (let [next-beat (inc beat)
        wanted #{0 1}]
    (at (m beat)
        ;clap on first and second beat
        (if (wanted (mod beat 3))
          (clap1 :amp 0.8 )))
          ;(haziti-clap :amp 0.6)))
    (at (m (+ (rand 0.15) beat))
        (if (wanted (mod beat 3))
          (clap1 :amp 0.3)))

    (apply-by (m next-beat) #'player [next-beat])))

;;(player (m))
;;(stop)


(def g (guitar))
;; strum it on your own
(guitar-strum g :Dbm :down 0.5)
(guitar-strum g :E :up 0.75)
(guitar-strum g :B :down 0.25)
(guitar-strum g :Ab7 :up 0.5)
(do ;; knocking on heaven's door
  (let [metro (metronome 120)]
    (doall
     (doseq [[i c] (map-indexed vector [:Dbm :Dbm :Dbm :Dbm :Ab7 :Ab7 :Ab7 :Ab7])]
       (strum-pattern g metro i c "dd-dd-")))))

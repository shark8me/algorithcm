(ns algorithcm.core
 ; (:require [overtone.inst.drum :refer [kick]])
  )
(use 'overtone.live)
(comment
(definst foo [] (saw 220))
(foo)
(kill foo)

(definst saw-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4]
  (* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(saw-wave 440)

;play notes
(saw-wave (midi->hz 69))
(saw-wave (midi->hz 72))
(saw-wave (midi->hz 60)) ;

(saw-wave (midi->hz (note :A4)))
(saw-wave (midi->hz (note :C5)))
(saw-wave (midi->hz (note :C4))) ;

(defn saw2 [music-note]
    (saw-wave (midi->hz (note music-note))))

;play A4
(saw2 :A4)

(defn play-chord [a-chord]
  (doseq [note a-chord] (saw2 note)))

(play-chord (chord :C4 :major))

(defn chord-progression-time []
  (let [time (now)]
    (at time (play-chord (chord :C4 :major)))
    (at (+ 2000 time) (play-chord (chord :G3 :major)))
    (at (+ 3000 time) (play-chord (chord :F3 :sus4)))
    (at (+ 4300 time) (play-chord (chord :F3 :major)))
    (at (+ 5000 time) (play-chord (chord :G3 :major)))))

(chord-progression-time)

(defonce metro (metronome 120))
(metro)
(defn chord-progression-beat [m beat-num]
  (at (m (+ 0 beat-num)) (play-chord (chord :C4 :major)))
  (at (m (+ 4 beat-num)) (play-chord (chord :G3 :major)))
  (at (m (+ 8 beat-num)) (play-chord (chord :A3 :minor)))
  (at (m (+ 14 beat-num)) (play-chord (chord :F3 :major)))
)

(chord-progression-beat metro (metro))

(defn chord-progression-beat [m beat-num]
  (at (m (+ 0 beat-num)) (play-chord (chord :C4 :major)))
  (at (m (+ 4 beat-num)) (play-chord (chord :G3 :major)))
  (at (m (+ 8 beat-num)) (play-chord (chord :A3 :minor)))
  (at (m (+ 12 beat-num)) (play-chord (chord :F3 :major)))
  (apply-at (m (+ 16 beat-num)) chord-progression-beat m (+ 16 beat-num) [])
)
(chord-progression-beat metro (metro))
(map kill (range 10))
(kill 1)

;Metronome and sequencing

(def kick overtone.inst.drum.kick)
)
(definst kick [freq 120 dur 0.3 width 0.5]
  (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
        env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
        sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
        src (sin-osc freq-env)
        drum (+ sqr (* env src))]
    (compander drum drum 0.2 1 0.1 0.01 0.01)))

;(kick)

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(c-hat)

(def metro (metronome 128))

(metro)
(metro 0)

(defn player [beat]
  (at (metro beat) (kick))
  (at (metro (+ 0.5 beat)) (c-hat))
  (at (metro (+ 0.75 beat)) (c-hat))
  (apply-by (metro (inc beat)) #'player (inc beat) []))

(player (metro))
(kill 1)
(def f (freesound-path 2086))
(def kick (sample (freesound-path 2086)))

(scale :C3 :major)

(def scale-degrees [:i :ii :iii :iv :v :vi :vii])

(degrees->pitches scale-degrees :dorian :E3)

(def scale-degrees [:vi :vii :i+ :_ :vii :_ :i+ :vii :vi :_ :vii :_])
(def pitches (degrees->pitches scale-degrees :dorian :C4))

(defn play [time notes sep]
  (let [note (first notes)]
    (when note
      (at time (saw (midi->hz note))))
    (let [next-time (+ time sep)]
      (apply-at next-time play [next-time (rest notes) sep]))))

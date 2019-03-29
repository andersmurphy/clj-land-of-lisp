(ns clj-land-of-lisp.chapter2)

(def small (atom 1))
(def big (atom 100))

(defn guess-my-number []
  (bit-shift-right (+ @small @big) 1))

(defn smaller []
  (reset! big (dec (guess-my-number)))
  (guess-my-number))

(defn bigger []
  (reset! small (inc (guess-my-number)))
  (guess-my-number))

(defn start-over []
  (reset! small 1)
  (reset! big 100)
  (guess-my-number))

(ns clj-land-of-lisp.chapter06
  (:require [clojure.string :refer [capitalize]]))

(def nodes
  {:living-room
   '(:you :are :in :a :living :room :.
          :a :wizard :is :snoring :loudly :on :the :couch :.)
   :garden
   '(:you :are :in :a :beautiful :garden :.
          :there :is :a :well :in :front :of :you :.)
   :attic
   '(:you :are :in :the :attic :.
          :there :is :a :giant :welding :tourch :in :the :corner :.)})

(defn describe-location [location nodes]
  (nodes location))

(def edges
  {:living-room
   '({:destination :garden
      :direction   :west
      :portal      :door}
     {:destination :attic
      :direction   :upstairs
      :portal      :ladder})
   :garden
   '({:destination :living-room
      :direction   :east
      :portal      :door})
   :attic
   '({:destination :living-room
      :direction   :down-stairs
      :portal      :ladder})})

(defn describe-path [{:keys [direction portal]}]
  [:there :is :a portal :going direction :from :here :.])

(defn describe-paths [location edges]
  (->> (edges location)
       (map describe-path)
       (apply concat)))

(def objects
  [:whiskey :bucket :frog :chain])

(def object-locations
  (atom
   {:whiskey [:living-room]
    :bucket  [:living-room]
    :chain   [:garden]
    :frog    [:garden]}))

(defn objects-at [loc objs obj-loc]
  (letfn [(at-loc? [obj]
            (= (last (obj-loc obj)) loc))]
    (filter at-loc? objs)))

(defn describe-objects [loc objs obj-loc]
  (letfn [(describe-obj [obj]
            [:you :see :a obj :on :the :floor :.])]
    (->> (objects-at loc objs obj-loc)
         (map describe-obj)
         (apply concat))))

(def location (atom :living-room))

(defn look []
  (let [loc @location]
    (concat (describe-location loc nodes)
            (describe-paths loc edges)
            (describe-objects loc objects @object-locations))))

(defn walk [direction]
  (if-let [{:keys [destination]}
           (->> (edges @location)
                (filter #(-> % :direction (= direction)))
                first)]
    (do (reset! location destination)
        (look))
    [:you :cannot :go :that :way :.]))

(defn pickup [object]
  (if (some #{object} (objects-at @location objects @object-locations))
    (do (swap! object-locations update object conj :body)
        [:you :are :now :carrying :the object :.])
    [:you :cannot :get :that :.]))

(defn inventory []
  {:items (objects-at :body objects @object-locations)})

(defn game-read []
  (let [cmd (read-string (str "(" (read-line) ")"))]
    (cons (first cmd) (map keyword (rest cmd)))))

(def allowed-commands
  ;; We use a set here as it's ideal for member checks.
  ;; We also need to quote the functions to get the symbols
  ;; as opposed to the functions themselves.
  #{'look 'walk 'pickup 'inventory})

(defn game-eval [sexp]
  (if (allowed-commands (first sexp))
    (eval sexp)
    [:i :do :not :know :that :command :.]))

(def punctuation? #{"." "?" "!"})

(defn game-print [lst]
  ;; Clojure doesn't have the same symbol case/punctuation restrictions as CL:
  ;; keywords are case sensitive and can contain commas (keyword ",").
  ;; So unlike the book which process the descriptions per character we
  ;; will process the descriptions per word.
  ;;
  ;; As strings are valid keys in Clojure an argument could be made for using
  ;; strings instead of keywords in this game.
  (->> (map name lst)
       (reduce (fn [previous-words word]
                 (let [previous-word (last previous-words)]
                   (conj previous-words
                         (if (or (nil? previous-word)
                                 (punctuation? previous-word))
                           (capitalize word)
                           word)))) [])
       (map (fn [word]
              (if-not (punctuation? word)
                (str " " word)
                word)))
       (apply str)
       println))

(defn game-repl []
  (let [cmd (game-read)]
    (when-not (= cmd '(quit))
      (game-print (game-eval cmd))
      (recur))))

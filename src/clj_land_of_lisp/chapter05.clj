(ns clj-land-of-lisp.chapter05)

(def nodes
  ;; The example in the book uses an association list (list of lists).
  ;;
  ;;'((key1 (dataB))
  ;; (key2 (dataA)))
  ;;
  ;; In Clojure maps are the idiomatic choice when you need to associate
  ;; data with a lookup key.
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
  ;; Maps are functions in Clojure.
  (nodes location))

(def edges
  ;; The example in the book uses symbols for the words.
  ;; In Clojure symbols need to be defined before they are used,
  ;; unless you quote them. We could just use quoted symbols here.
  ;;
  ;; ['garden 'west 'door]
  ;;
  ;; However, for now, if we just want a symbol that evaluates to
  ;; itself, keys provide this functionality.
  ;;
  ;; Later if we want to map these keys to functions we can just
  ;; use a map as a look up. The advantage of this is that it is easier
  ;; to define different mappings.
  ;;
  ;; Eg: In context A :door -> Green Door.
  ;;     In context B :door -> Red Door.
  ;;
  ;; It's harder to do this if 'door maps to (defn door ...) without
  ;; redefining the door function.
  {:living-room
   ;; Using maps here instead of lists, so that we don't associate
   ;; position of something in a list with semantic meaning.
   ;; Rather than the first item in the list being the destination
   ;; we use an explicit and descriptive key.
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

(defn describe-path
  ;; Destructuring has been used to get the direction and portal.
  [{:keys [direction portal]}]
  ;; Can be written using a quasiquoted list:
  ;;
  ;; `(:there :is :a ~portal :going ~direction :from :here :.)
  ;;
  ;; But in Clojure a vector is more idiomatic.
  [:there :is :a portal :going direction :from :here :.])

(defn describe-paths [location edges]
  ;; Can be written as:
  ;;
  ;; (apply concat (map describe-path (edges location)))
  ;;
  ;; But the ->> macro makes it read more naturally.
  (->> (edges location)
       (map describe-path)
       (apply concat))) ;; (apply concat) can be replaced with flatten

(def objects
  [:whiskey :bucket :frog :chain])

(def object-locations
  (atom
   {:whiskey [:living-room] ;; locations are vectors so we have history
    :bucket  [:living-room]
    :chain   [:garden]
    :frog    [:garden]}))

(defn objects-at [loc objs obj-loc]
  ;; In Clojure ? denote predicated unlike -p in CL.
  (letfn [(at-loc? [obj]
            (= (last (obj-loc obj)) loc))]
    (filter at-loc? objs))) ;; filter is Clojure's remove-if-not

(defn describe-objects [loc objs obj-loc]
  (letfn [(describe-obj [obj]
            [:you :see :a obj :on :the :floor :.])]
    (->> (objects-at loc objs obj-loc)
         (map describe-obj)
         (apply concat))))

(def location (atom :living-room))

(defn look []
  ;; We used let here to ensure that all functions are passed the same
  ;; de-referenced location.
  (let [loc @location]
    (concat (describe-location loc nodes)
            (describe-paths loc edges)
            (describe-objects loc objects @object-locations))))

(defn walk [direction]
  (if-let [{:keys [destination]}
           (->> (edges @location)
                ;; filter first gives us the same behaviour as find in CL.
                (filter #(-> % :direction (= direction)))
                first)]
    (do (reset! location destination)
        (look))
    [:you :cannot :go :that :way :.]))

(defn pickup [object]
  ;; In Clojure (some #{item} coll) is used to check if a list contains
  ;; and item (i.e member in CL).
  (if (some #{object} (objects-at @location objects @object-locations))
    (do (swap! object-locations update object conj :body)
        [:you :are :now :carrying :the object :.])
    [:you :cannot :get :that :.]))

(defn inventory []
  {:items (objects-at :body objects @object-locations)})

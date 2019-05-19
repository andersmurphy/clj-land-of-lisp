(ns clj-land-of-lisp.chapter08
  (:require [clojure.set :as set]))

(def congestion-city-nodes nil)
(def congestion-city-edges nil)
(def node-num 30)
(def edge-num 45)
(def worm-num 3)
(def cop-odds 15)

(defn random-node []
  (inc (rand-int node-num)))

(defn edge-pair [a b]
  ;; We use set here to represent pairs, that way we only need one pair
  ;; per edge (rather than two).
  (when-not (= a b)
    #{a b}))

(defn make-edge-list []
  ;; We use repeatedly to generate an infinite lazy sequence
  ;; and take the first edge-num of items.
  ;;
  ;; Lazy sequences in Clojure use the thunk concept covered
  ;; earlier in the book heavily. This shouldn't be too surprising
  ;; as a thunk is an uninvoked closure that takes no arguments.
  ;; Uninvoked closures are a fundamental way of creating deferred
  ;; a.k.a lazy computation in a functional language.
  ;;
  ;; We use set here to remove any duplicates
  (-> (repeatedly edge-num
                  #(edge-pair (random-node) (random-node)))
      set
      (disj nil)))

(defn get-neigbouring-nodes [node edge-list]
  (->> (filter #(contains? % node) edge-list)
       (apply concat)
       set))

(defn get-connected [node edge-list]
  (loop [visited #{} nodes #{node}]
    (if (empty? nodes)
      visited
      (recur
       (into visited nodes)
       (set (mapcat #(get-neigbouring-nodes % edge-list)
                    (set/difference nodes visited)))))))

(defn find-islands [nodes edge-list]
  (loop [islands [] unconnected nodes]
    (if (empty? unconnected)
      islands
      (let [connected (get-connected (first unconnected) edge-list)]
        (recur (conj islands connected)
               (set/difference unconnected connected))))))

(defn connect-with-bridges [islands]
  (loop [bridges [] [first-island second-island & remaining-islands] islands]
    (if-not second-island
      bridges
      (recur (conj bridges #{(first first-island) (first second-island)})
             (conj remaining-islands (concat first-island second-island))))))

(defn connect-all-islands [nodes edge-list]
  (->> (find-islands nodes edge-list)
       connect-with-bridges
       (into edge-list)))

(defn edges->nodes [edge-list]
  (-> (apply concat edge-list)
      set))

(defn edges->alist [nodes edge-list]
  ;; The structure used here is the same as an alist the only difference
  ;; is that keys are used to avoid place oriented programming.
  ;;
  ;; So rather than:
  ;;
  ;; ((1 (2) (3 cop))
  ;;  (2 (1))
  ;;  (3 (1 cop)))
  ;;
  ;; We have:
  ;;
  ;; [{:node 1 :conns [{:node 2} {:node 3 :clues #{:cops}}]}
  ;;  {:node 2 :conns [{:node 1}]}
  ;;  {:node 3 :conns [{:node 1 :clues #{cops}}]}]
  (->> (merge-with concat (group-by first edge-list) (group-by second edge-list))
       (map (fn [[k v]] {:node  k
                         :conns
                         (map #(hash-map :node %)
                              (disj (apply clojure.set/union v) k))
                         :clues #{}}))))

(defn add-cops [edges-with-cops edge-alist]
  (map (fn [{:keys [node] :as m}]
         (if (some #(% node) edges-with-cops)
           (update m :conns (partial map (fn [{:keys [node] :as m}]
                                           (if (some #(% node) edges-with-cops)
                                             (assoc m :clues #{:cops})
                                             m))))
           m))
       edge-alist))

(defn make-city-edges []
  (let [nodes           (set (range 1 (inc node-num)))
        edge-list       (connect-all-islands nodes (make-edge-list))
        edges-with-cops (filter (fn [_] (zero? (rand-int 15))) edge-list)]
    (->> edge-list
         (edges->alist nodes)
         (add-cops edges-with-cops)
         (group-by :node))))

(defn neighbours [node edge-alist]
  (->> (edge-alist node)
       first
       :conns
       (map :node)))

(defn neighbours-2 [node edge-alist]
  (let [initial-neighbours (neighbours node edge-alist)]
    (reduce (fn [acc neighbour-node]
              (concat acc (neighbours neighbour-node edge-alist)))
            initial-neighbours initial-neighbours)))

(defn add-glow-worms [edge-alist glow-worms]
  (reduce (fn [acc worm]
            (update-in acc [worm 0 :clues] conj :glow-worms))
          edge-alist
          glow-worms))

(defn add-wumpus-blood [edge-alist wumpus]
  (->> (neighbours-2 wumpus edge-alist)
       (reduce (fn [acc light]
                 (update-in acc [light 0 :clues] conj :blood)) edge-alist)))

(defn add-glow-worm-light [edge-alist glow-worms]
  (->> (mapcat #(neighbours % edge-alist) glow-worms)
       (reduce (fn [acc light]
                 (update-in acc [light 0 :clues] conj :light)) edge-alist)))

(defn node-has-cop? [node]
  (->> (node :conns)
       (some #(-> % :clues :cops))))

(defn add-sirens [edge-alist]
  (->> (map (fn [[k v :as tuple]]
              (if (node-has-cop? (first v))
                [k (update-in v [0 :clues] conj :sirens)]
                tuple)) edge-alist)
       (into {})))

(defn make-city-nodes [edge-alist]
  (let [wumpus     (random-node)
        glow-worms (take worm-num (repeatedly random-node))]
    (-> edge-alist
        (update-in [wumpus 0 :clues] conj :wumpus)
        (add-glow-worms glow-worms)
        (add-glow-worm-light glow-worms)
        (add-wumpus-blood wumpus)
        add-sirens)))

(defn find-empty-node [edge-alist]
  (let [x (random-node)]
    (if (-> (edge-alist x) first :clues empty?)
      x
      (recur edge-alist))))

(defn draw-city [m]
  m)

(defn new-game []
  (let [congestion-city-edges (make-city-edges)
        congestion-city-nodes (make-city-nodes congestion-city-edges)
        player-pos            (find-empty-node congestion-city-nodes)
        visited-nodes         [player-pos]]
    (draw-city {:city-nodes    congestion-city-nodes
                :player-pos    player-pos
                :visited-nodes visited-nodes})))

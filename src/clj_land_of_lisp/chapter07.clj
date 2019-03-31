(ns clj-land-of-lisp.chapter07
  (:require [clojure.java.shell :refer [sh]]))

(def wizard-nodes
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

(def wizard-edges
  ;; Refactored edges to a flat structure closer to the one in the book.
  [{:node        :living-room
    :destination :garden
    :direction   :west
    :portal      :door}
   {:node        :living-room
    :destination :attic
    :direction   :upstairs
    :portal      :ladder}
   {:node        :garden
    :destination :living-room
    :direction   :east
    :portal      :door}
   {:node        :attic
    :destination :living-room
    :direction   :down-stairs
    :portal      :ladder}])

(defn dot-name [exp]
  (clojure.string/replace (name exp) #"-" "_"))

(defn dot-label [exp]
  ;; We don't convert the keywords to anything or remove the colons.
  ;;
  ;; So where the book prints:  (EAST DOOR)
  ;; We print:                  (:east :door)
  ;;
  ;; Using keywords is less fiddly than using symbols in Clojure as
  ;; there is no syntax literal for symbols that prints nicely.
  ;;
  ;; - 'foo prints as (quote foo)
  ;;
  ;; - foo without the quote needs to resolve to something
  ;;
  ;; Lisp 1 vs Lisp 2?
  (let [max-label-length 30
        string-exp       (str exp)]
    (if (> (count string-exp) max-label-length)
      (str (subs string-exp 0 (- max-label-length 3)) "...")
      string-exp)))

(defn label-string [exp]
  (str "[label=\"" (dot-label exp) "\"];"))

(defn nodes->dot [nodes]
  (->> (map (fn [[k _ :as node]]
              (str (dot-name k) (label-string node)))
            nodes)
       (apply str)))

(defn edges->dot [edges]
  (->> (map (fn [{:keys [destination portal direction node]}]
              (str (dot-name node) "->"
                   (dot-name destination)
                   (label-string (list direction portal))))
            edges)
       (apply str)))

(defn graph->dot [nodes edges]
  (str "digraph{"
       (nodes->dot nodes)
       (edges->dot edges)
       "}"))

(defn dot->png [fname thunk]
  ;; Thunk is somewhat unnecessary here as our graph->dot and edges->dot
  ;; functions return strings (and don't print).
  ;;
  ;; Interesting to learn that thunks are a lisp fp concept that
  ;; redux-thunk.js seems to have adopted.
  (spit fname (thunk))
  (sh "dot" "-Tpng" "-O" fname))

(defn graph-png [fname nodes edges]
  (dot->png fname
            #(graph->dot nodes edges)))

(comment
  (graph-png "output/wizard.dot" wizard-nodes wizard-edges))

(defn uedges->dot [edges]
  ;; Group the nodes by sets of destination and node (sets ignores order).
  (->> (group-by (fn [{:keys [destination node]}]
                   #{destination node}) edges)
       ;; Select the first of each grouping (to remove the duplicates).
       (map (fn [[_ [first-edge]]] first-edge))
       ;; Same code as before
       (map (fn [{:keys [destination portal direction node]}]
              (str (dot-name node) "->" ;; Couldn't use -- encoding problem?
                   (dot-name destination)
                   (label-string (list direction portal)))))
       (apply str)))

(defn ugraph->dot [nodes edges]
  (str "digraph{"
       (nodes->dot nodes)
       (uedges->dot edges)
       "}"))

(defn ugraph->png [fname nodes edges]
  (dot->png fname
            #(ugraph->dot nodes edges)))

(comment
  (ugraph->png "output/uwizard.dot" wizard-nodes wizard-edges))

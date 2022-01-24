(defrecord Graph [vertices edges])
(defn make-graph []
  (Graph. (ref {}) (ref {})))

;todo: new record/structure
(defrecord Vertex [label lat lon visited neighbors distance])
(defn make-vertex [label lat lon]
  (Vertex. label lat lon (ref 0) (ref '()) (ref nil)))

(defn graph-add-vertex! [graph label lat lon]
  (let [vertices (:vertices graph)
        new-vertex (make-vertex label lat lon)]
    (dosync
      (ref-set vertices (assoc @vertices label new-vertex))))
  nil)

(defrecord Edge [from to label weight])
(defn make-edge [from to label weight]
  (Edge. from to label weight))

(defn graph-edge-key [from to]
  (sort (list from to)))


(defn graph-add-edge! [graph from to label weight]
  (let [edges (:edges graph)
        vertices @(:vertices graph)
        from-vertex (get vertices from)
        to-vertex (get vertices to)
        from-vertex-neighbors (:neighbors from-vertex)
        to-vertex-neighbors (:neighbors to-vertex)
        new-edge (make-edge from to label weight)
        new-edge-key (graph-edge-key from to)]
    (dosync
      (ref-set edges (assoc @edges new-edge-key new-edge))
      (ref-set from-vertex-neighbors (conj @from-vertex-neighbors to))
      (ref-set to-vertex-neighbors (conj @to-vertex-neighbors from))
      ))
  nil)

(defn graph-get-neighbors [graph label]
  @(:neighbors (get @(:vertices graph) label)))

(defn graph-has-vertex? [graph label]
  (contains? @(:vertices graph) label))

(defn graph-has-edge? [graph from to]
  (contains? @(:edges graph) (graph-edge-key from to)))

(defn graph-reset! [graph]
  (doseq [vertex (vals @(:vertices graph))]
    (dosync (ref-set (:visited vertex) 0)
            (ref-set (:distance vertex) nil))))

(defn get-edge-weight [graph from to]
  (:weight (get @(:edges graph) (graph-edge-key from to))))

(defn rest-queue! [queue label]
  (filter
    (fn [vertex] (not (= vertex label))) queue))

;; state 0 - not encountered at all
;; state 1 - in the open queue
;; state 2 - current vertex
;; state 3 - visited
(defn graph-bfs!
  ([graph]
   (graph-bfs! graph (first (keys @(:vertices graph)))))
  ([graph start]
   (graph-bfs! graph start (fn [vertex] nil)))
  ([graph start func]
   (graph-bfs! graph start func first))
  ([graph start func func-m]
   (let [vertices @(:vertices graph)]
     (loop [queue (list start)]
       (when (not (empty? queue))
         (let [current-label (if (= func-m manager-func)
                               (func-m queue graph)
                               (func-m queue))
               rest-queue (rest-queue! queue current-label)
               current-vertex (get vertices current-label)
               visited-status (:visited current-vertex)
               current-neighbors @(:neighbors current-vertex)
               unseen-neighbors (filter
                                  (fn [label]
                                    (= @(:visited (get vertices label)) 0))
                                  current-neighbors)
               ]
           (dosync (ref-set visited-status 2))
           (func current-vertex)
           (dosync (ref-set visited-status 3))
           (doseq [label unseen-neighbors]
             (dosync
               (ref-set (:visited (get vertices label)) 1)))
           (recur (concat rest-queue unseen-neighbors))))))))

(defn manager-func [queue graph]
  (loop [queue queue
         best-distance nil
         best-vertex nil]
    (if (empty? queue)
      best-vertex
      (let [queue-label (first queue)
            queue-vertex (get @(:vertices graph) queue-label)]
        (if (or (nil? best-vertex)
                (< @(:distance queue-vertex) best-distance))
          (recur (rest queue)
                 @(:distance queue-vertex)
                 queue-vertex)
          (recur (rest queue)
                 best-distance
                 best-vertex))))))

(defn graph-dijkstra-mark! [graph finish use-weights]
  (let [vertices @(:vertices graph)
        start-vertex (get vertices finish)]
    (graph-reset! graph)
    (dosync
      (ref-set (:distance start-vertex) 0))
    (if (not use-weights)
      (graph-bfs! graph
                  finish
                  (fn [vertex]
                    (let [next-distance (inc @(:distance vertex))]
                      (doseq [neighbor-label @(:neighbors vertex)]
                        (let [neighbor (get vertices neighbor-label)]
                          (if (= @(:visited neighbor) 0)
                            (dosync
                              (ref-set (:distance neighbor) next-distance))))))))
      (graph-bfs! graph
                  finish
                  (fn [vertex]
                    (doseq [neighbor-label @(:neighbors vertex)]
                      (let [neighbor (get vertices neighbor-label)
                            next-distance (+ @(:distance vertex) (get-edge-weight graph (:label vertex) neighbor-label))]
                        (when (or (= @(:visited neighbor) 0) (> @(:distance neighbor) next-distance))
                          (dosync
                            (ref-set (:distance neighbor) next-distance))))))
                  manager-func
                  ))))

(defn find-best-neighbor [graph vertices vertex use-weights]
  (loop [neighbors-labels @(:neighbors vertex)
         best-distance nil
         best-vertex nil]
    (if (empty? neighbors-labels)
      best-vertex
      (let [vertex-label (:label vertex)
            distance-vertex @(:distance vertex)
            neighbor-label (first neighbors-labels)
            neighbor-vertex (get vertices neighbor-label)
            neighbor-distance @(:distance neighbor-vertex)
            edge-key (graph-edge-key vertex-label neighbor-label)
            edges (:edges graph)
            edge-weight (:weight (get edges edge-key))]
        (if (not use-weights)
          (if (or (nil? best-vertex)
                  (< neighbor-distance best-distance))
            (recur (rest neighbors-labels)
                   neighbor-distance
                   neighbor-vertex)
            (recur (rest neighbors-labels)
                   best-distance
                   best-vertex))

          (if (= (- distance-vertex neighbor-distance)
                 edge-weight)
            (if (or (nil? best-vertex)
                    (< neighbor-distance best-distance))

              (recur (rest neighbors-labels)
                     neighbor-distance
                     neighbor-vertex)

              (recur (rest neighbors-labels)
                     best-distance
                     best-vertex))

            (recur (rest neighbors-labels)
                   best-distance
                   best-vertex))
          )))))

(defn graph-dijkstra-trace [graph start use-weights]
  (let [vertices @(:vertices graph)
        start-vertex (get vertices start)]
    (if (= @(:visited start-vertex) 0)
      (println "There is no path!")
      (loop [current-vertex start-vertex]
        (println (:label current-vertex))
        (if (> @(:distance current-vertex) 0)
          (recur (find-best-neighbor graph vertices current-vertex use-weights)))))))

(defn graph-dijkstra! [graph start finish weighted]
  (graph-dijkstra-mark! graph finish weighted)
  (graph-dijkstra-trace graph start weighted))


(load-file "e-roads-2020-full.clj")
;Wei graph
(println "Problem 1")
(graph-dijkstra! g "Alat, Uzbekistan" "Prague" false)
(graph-dijkstra! g "Newport, Wales" "Prague" false)

;Non-weig graph
(graph-dijkstra! g "Alat, Uzbekistan" "Prague" true)
(graph-dijkstra! g "Newport, Wales" "Prague" true)


(ns ezbitmap.ezbitmap
  (:require [ezbitmap.colors :as co]
            [ezbitmap.font-small :as fs]
            [ezbitmap.ascii-art :as aa]
            [ezbitmap.view-image :as vi]
            [clojure.set :as se]
            [clojure.string :as st]
            [fbc-utils.core :as ut :refer [let->]]
            [fbc-utils.debug :as db :refer [let-dbg defn-dbg for-dbg dbg-switch fn-dbg dbg-switch-on-error dbg-switch-index]]
            [snek.core :as sn]
            [clojure.java.io :as io]
            [good-guesser.good-guesser :as gg]))

(defn stringize [bmp]
  (map (partial apply str) bmp))

(defn print-bmp [bmp]
  (println "+")
  (doall (map println (stringize bmp)))
  nil)

(defn digitize [art]
  (vec (map vec
            (mapcat (fn [row]
                      (apply map
                             str
                             (for [c row]
                               (let [font (fs/font (ut/ord c))]
                                 (when-not font
                                   (ut/throw (str "missing font for #" (ut/ord c) " character " c)))
                                 font))))
                    art))))

(defn regions [bmp]
  (map second
       (:regions (reduce (fn [{:keys [index
                                      regions]
                               :as   acc}
                              item]
                           {:index   (inc index)
                            :regions (:regions (reduce (fn [{:keys [index2
                                                                    regions]
                                                             :as   acc2}                                                            item2]
                                                         (let [has-index (fn [index2 [k v]]
                                                                           (k index2))
                                                               above     (ut/find-index (partial has-index index2) regions)
                                                               left      (ut/find-index (partial has-index (dec index2)) regions)
                                                               empty     (= item2 \space)
                                                               pos       [index2 index]]
                                                           {:index2  (inc index2)
                                                            :regions (if empty
                                                                       (if (and above left)
                                                                         (if (= above left)
                                                                           (update-in regions [above 1] conj pos)
                                                                           (let [regions (sn/modify {left [(partial se/union (first (regions above)))
                                                                                                           (partial se/union (second (regions above)) #{pos})]}
                                                                                                    regions)]
                                                                             (vec (concat (subvec regions 0 above) (subvec regions (inc above))))))
                                                                         (cond above (update-in regions [above 1] conj pos)
                                                                               left  (sn/modify {left [(fn [k]
                                                                                                         (conj k index2))
                                                                                                       (fn [k]
                                                                                                         (conj k pos))]}
                                                                                                regions)
                                                                               :else (conj regions [#{index2} #{pos}])))
                                                                       (if above
                                                                         (update-in regions [above 0] disj index2)
                                                                         regions))}))
                                                       {:index2  0
                                                        :regions regions}
                                                       item))})
                         {:index   0
                          :regions []}
                         bmp))))

(defn width [bmp]
  (count (first bmp)))

(defn height [bmp]
  (count bmp))

(defn internal-regions [bmp]
  (let [regions (regions bmp)
        wid     (width bmp)
        ht      (height bmp)]
    (remove (partial some
                     (fn [[x y]]
                       (or (zero? x) (zero? y) (= x (dec wid)) (= y (dec ht)))))
            regions)))

(defn regions->bmp [off regions]
  (let [wid (inc (apply max (cons 0 (map first (apply concat regions)))))
        ht  (inc (apply max (cons 0 (map second (apply concat regions)))))]
    (reduce (fn [acc [index pos :as item]]
              (let [[x y] pos]
                (assoc-in acc [y x] (char (+ (ut/ord \0) (+ off index))))))
            (vec (repeat ht (vec (repeat wid \space))))
            (apply concat
                   (map-indexed (fn [index positions]
                                  (map (partial vector index) positions))
                                regions)))))

(defn brush [size] ;size is the width of the circular brush in pixels
  (let [actual-center (int (/ size 2))
        circle-center (/ size 2)
        radius        (/ size 2)]
    (sort-by (fn [[x y :as pos]]
               (+ (ut/abs x) (ut/abs y)))
             (for [[x y :as pixel-pos] (filter (fn [pixel-pos]
                                                 (<= (ut/sqr-dist (map (partial + 0.5) pixel-pos) [circle-center circle-center]) (ut/square radius)))
                                               (ut/cartesians size size))]
               [(- actual-center x) (- actual-center y)]))))

;;(brush 5)

(defn convolution [fun brush bmp]
  (let [bmp (mapv vec bmp)
        wid (width bmp)
        ht  (height bmp)]
    (reduce (fn [acc [offx offy :as item]]
              (reduce (fn [acc2 [x y :as item2]]
                        (let [xx (+ x offx)
                              yy (+ y offy)]
                          (if (and (ut/between 0 xx wid) (ut/between 0 yy ht))
                            (let [offc (get-in bmp [yy xx])]
                              (update-in acc2
                                         [y x]
                                         (fn [c]
                                           (fun c offc))))
                            acc2)))
                      acc
                      (ut/cartesians (width bmp) (height bmp))))
            bmp
            brush)))

(defn fatten-colors [brush bmp]
  (convolution (fn [c offc]
                 (if (and (not (#{\space \A} offc)) (#{\space \A} c))
                   offc
                   c))
               brush
               bmp))


(defn fatten-outlines [brush bmp]
  (convolution (fn [c offc]
                 (if (not= offc \space)
                   offc
                   c))
               brush
               bmp))

(defn fatten-regions [brush regions]
  (reduce (fn [acc item]
            (let [visited (apply se/union acc)]
              (map (fn [region old-region]
                     (reduce (fn [acc2 item2]
                               (let [pos (mapv + item2 item)]
                                 (if (visited pos)
                                   acc2
                                   (conj acc2 pos))))
                             region
                             old-region))
                   acc
                   regions)))
          regions
          brush))

(defn fatten-region [brush region]
  (set (mapcat (fn [pos]
                 (for [off brush]
                   (mapv + pos off)))
               region)))

(defn tree-add [tree region]
  (if-let [index (ut/find-index (fn [[positions :as branch]]
                                  (seq (se/intersection positions region)))
                                tree)]
    (update tree
            index
            (fn [[positions & chis :as branch]]
              #_(assert (not (seq (se/difference region positions))))
              (let [k (se/difference region positions)]
                (if (seq k)
                  (cons (se/union positions region) (tree-add (vec chis) k))
                  branch))))
    (conj tree [region])))

(defn fill-tree [depth bmp]
  (reduce (fn [acc item]
            (let [fat     (if (> item 1)
                            (fatten-outlines (brush item) bmp)
                            bmp)    
                  regions (internal-regions fat)
                  regions (if (> item 1)
                            (fatten-regions (brush item) regions)
                            regions)]
              (reduce tree-add acc regions)))
          []
          (range 1 depth)))

(defn merge-bmps [bmp1 bmp2]
  (mapv (fn [row1 row2]
          (mapv (fn [c1 c2]
                  (if (= c2 \space)
                    c1
                    c2))
                (concat row1 (repeat \space))
                row2))
        (concat bmp1 (repeat (repeat \space)))
        bmp2))

(defn detangle-tree [])

(defn flatten-tree [tree]
  (mapcat (fn [[positions & chis :as branch]]
            (if (seq chis)
              (cons positions (flatten-tree chis))
              [positions]))
          tree))

(defn strip-region-dupes [regions]
  (reverse (filter seq
                   (:regions (reduce (fn [{:keys [visited
                                                  regions]
                                           :as   acc}
                                          item]
                                       {:visited (se/union visited item)
                                        :regions (conj regions (se/difference item visited))})
                                     {:visited #{}
                                      :regions []}
                                     (reverse regions))))))

(defn group-touching-regions [regions]
  (reduce (fn [acc item]
            (let [fat-region (fatten-region [[-1 0] [1 0] [0 -1] [0 1]] item)
                  index      (ut/find-index (fn [region]
                                              (seq (se/intersection region fat-region)))
                                            acc)]
              (if index
                (update acc index se/union item)
                (conj acc item))))
          []
          regions))

(defn spit-bmp [fname bmp]
  (with-open [out (io/output-stream (io/file fname))]
    (let [wid (width bmp)
          ht  (height bmp)
          ba  (byte-array (* 4 wid ht))]
      (dotimes [x wid]
        (dotimes [y ht]
          (let [off         (* (+ (* y wid) x) 4)
                pix         (get-in bmp [y x])
                rgb (cond (= pix \space)  (:rgb (co/colors :white))
                          (= pix \#)      (:rgb (co/colors :black))
                          :else           (co/char->rgb pix))
                alpha       255 #_(if (= pix \space)
                                    0
                                    255)]
            (dotimes [n 3]
              (aset-byte ba (+ off n) (unchecked-byte (rgb n))))
            (aset-byte ba (+ off 3) (unchecked-byte alpha)))))
      (.write out ba))))

(defn valid? [bmp]
  (every? (fn [row]
            (= (count row) (width bmp))) bmp))

(defn blacken-bmp [bmp]
  (ut/forv [row bmp]
           (ut/forv [c row]
                    (if (= c \space)
                      \space
                      \A))))

(defn decolor-bmp [bmp]
  (ut/forv [row bmp]
           (ut/forv [c row]
                    (if (= c \#)
                      \#
                      \space))))

(def diamond-brush [[-1 0] [1 0] [0 -1] [0 1]])
(def square-brush [[-1 1] [1 1] [1 -1] [-1 -1] [-1 0] [1 0] [0 -1] [0 1]])

(defn region-outline [region]
  (se/difference (set (for [pos region
                            off diamond-brush]
                        (mapv + pos off)))
                 region))

(defn prelabel-regions [regions color-labels]
  (let [centers      (map :center regions)
        midpoint     [(apply ut/avg (map first centers)) (apply ut/avg (map second centers))]
        color-labels (sort-by (fn [{:keys [pos
                                           color]
                                    :as   label}]
                                (- (ut/dist midpoint pos)))
                              color-labels)]
    (reduce (fn [acc
                 {:keys [pos
                         color]
                  :as   item}]
              (if-let [{:keys [id]
                        :as   best} (apply min-key
                                           (fn [{:keys [center] :as region}]
                                             (ut/dist center pos))
                                           (remove (fn [{:keys [id]
                                                         :as   region}]
                                                     (acc id))
                                                   regions))]
                (assoc acc id color)))
            {}
            color-labels)))

(let [size-sum   (fn [{:keys [bmp]
                       :as   input}]
                   (+ (width bmp) (height bmp)))
      size       (fn [{:keys [bmp]
                       :as   input}]
                   (* (width bmp) (height bmp)))
      pixels     (fn [{:keys [summary]
                       :as   input}]
                   (apply + (map :num (vals summary))))
      region-num (fn [{:keys [summary]
                       :as   input}]
                   (count summary))
      visualizer (fn [{:keys [summary
                              bmp]
                       :as   input}
                      output]
                   (let [bmp         (mapv vec bmp)
                         big-regions (set (map :center
                                               (filter (fn [{:keys [num]
                                                             :as   region}]
                                                         (>= num output))
                                                       (vals summary))))]
                     (cons (str output)
                           (stringize (reduce (fn [acc [x y :as item]]
                                                (let [bmp-pixel (get-in acc [y x])]
                                                  (assoc-in acc
                                                            [y x]
                                                            (cond (big-regions item) \*
                                                                  (= bmp-pixel \#)   \.
                                                                  :else              \space))))
                                              bmp
                                              (ut/cartesians (width bmp) (height bmp)))))))]
  (defn best-large-region-cutoff [input]
    (gg/good-guesser :large-region-cutoff input size-sum size pixels region-num :visualizer visualizer :verbose true)))

;;gg/cache

(defn cluster-colors [large-region-cutoff summary color-labels]
  (let [[big small]         (split-with (fn [{:keys [num]
                                              :as   region}]
                                          (>= num large-region-cutoff))
                                        (reverse (sort-by :num (vals summary))))
        big-prelabeled      (prelabel-regions big color-labels)
        remaining-colors    (take 4 (sort (se/difference (set (range 16)) (set (vals big-prelabeled)))))
        big-colors          (into {}
                                  (map (fn [{:keys [id]
                                             :as   region}
                                            color-index]
                                         [id color-index])
                                       (remove (fn [{:keys [id]
                                                     :as   region}]
                                                 (big-prelabeled id))
                                               big)
                                       (drop (+ 3 (count big)) (cycle remaining-colors))))
        big-colors          (merge big-colors big-prelabeled)
        connected-to-big    (loop [z (zipmap (keys big-colors) (keys big-colors))]
                              (let [z-new (merge z
                                                 (into {}
                                                       (filter identity
                                                               (for [{:keys [id
                                                                             direct-neighbors]
                                                                      :as   region} small]
                                                                 (when-let [neigh (seq (sort-by (fn [[k v]]
                                                                                                  (- (:num (summary v))))
                                                                                                (select-keys z direct-neighbors)))]
                                                                   [id (second (first neigh))])))))]
                                (if (= z z-new)
                                  z
                                  (recur z-new))))
        small-colors        (into {}
                                  (for [{:keys [id
                                                num
                                                center]
                                         :as   region}  small]
                                    [id
                                     (cond (connected-to-big id) (let [big-id            (connected-to-big id)
                                                                       big-color         (big-colors big-id)
                                                                       {big-center :center
                                                                        :as big-summary} (summary big-id)]
                                                                   (if (< (second center) (second big-center))
                                                                     (co/lighten-color big-color)
                                                                     (co/darken-color big-color)))
                                           (ut/between 2 num 10) 10
                                           :else                 6)]))]
    (merge big-colors small-colors)))

(defn region-summary [regions]
  (let [outlines (mapv region-outline regions)
        regions  (vec regions)
        num      (count regions)]
    (into {}
          (for [index (range num)]
            (let [region           (regions index)
                  outline          (outlines index)
                  direct-neighbors (set (filter (fn [index-other]
                                                  (seq (se/intersection outline (regions index-other))))
                                                (range num)))]
              [index
               {:id                 index
                :center             [(ut/round (apply ut/avg (map first region))) (ut/round (apply ut/avg (map second region)))]
                :num                (count region)
                :direct-neighbors   direct-neighbors
                :indirect-neighbors (se/difference (set (filter (fn [index-other]
                                                                  (seq (se/intersection outline (outlines index-other))))
                                                                (range num)))
                                                   direct-neighbors)}])))))

(defn strip-color-labels-row [{:keys [s]
                               :as   acc}]
  (let [result (reduce (fn [{:keys [s
                                    labels]
                             :as   acc}
                            [nam color-index :as item]]
                         (if-let [index (st/index-of s nam)]
                           (let [len (count nam)]
                             {:s      (str (subs s 0 index) (apply str (repeat len \space)) (subs s (+ index len)))
                              :labels (conj labels
                                            {:x     (+ index (/ len 2))
                                             :color color-index})})
                           acc))
                       acc
                       co/color-names)]
    (if (= (:s result) s)
      result
      (recur result))))

(defn transpose [art]
  (apply mapv
         (fn [& chars]
           (apply str chars))
         art))

(defn strip-color-labels-one-axis [art]
  (reduce (fn [{:keys [art
                       color-labels]
                :as   acc}
               item]
            (let [{:keys [s
                          labels]} (strip-color-labels-row {:s      item
                                                            :labels []})]
              {:art          (conj art s)
               :color-labels (vec (concat color-labels
                                          (for [{:keys [x
                                                        color]
                                                 :as   label} labels]
                                            {:pos   [(int (+ 2.5 (* x 5))) (int (+ 2.5 (* (count art) 5)))]
                                             :color color})))}))
          {:art          []
           :color-labels []}
          art))

(defn strip-color-labels [art]
  (let [{:keys          [art]
         color-labels-1 :color-labels} (strip-color-labels-one-axis art)
        art                            (transpose art)
        {:keys          [art]
         color-labels-2 :color-labels} (strip-color-labels-one-axis art)
        art                            (transpose art)]
    {:art          art
     :color-labels (vec (concat color-labels-1 (sn/modify [{:pos (comp vec reverse)}] color-labels-2)))}))

(defn giant-bitmap [giant-wid giant-bmp bmp]
  (let [wid       (+ (width bmp) 2)
        ht        (+ (height bmp) 2)
        giant-ht  (height giant-bmp)
        columns   (reduce (fn [acc [index row :as item]]
                            (map (fn [col c]
                                   (if (not= c \space)
                                     (inc index)
                                     col))
                                 acc
                                 row))
                          (repeat 0)
                          (map-indexed vector giant-bmp))
        [x y]     (first (conj (vec (filter identity
                                            (map (fn [y]
                                                   (:result (reduce (fn [{:keys [x
                                                                                 result]
                                                                          :as   acc}
                                                                         [yes no :as item]]
                                                                      (if (>= yes wid)
                                                                        (reduced (assoc acc :result [x y]))
                                                                        {:x (+ x yes no)}))
                                                                    {:x      0
                                                                     :result nil}
                                                                    (for [[yes no] (ut/group-while (fn [col]
                                                                                                     (<= col y))
                                                                                                   columns)]
                                                                      [(count yes) (count no)]))))
                                                 (range giant-ht))))
                               [0 giant-ht]))
        giant-bmp (vec (take (max giant-ht (+ y ht)) (concat giant-bmp (repeat (vec (repeat giant-wid \space))))))]
    (reduce (fn [acc [xx yy :as item]]
              (assoc-in acc [(+ y yy 1) (+ x xx 1)] (get-in bmp [yy xx])))
            giant-bmp
            (ut/cartesians (- wid 2) (- ht 2)))))

(defn fill-high-neighbor-pixels [amnt bmp]
  (let [wid (width bmp)
        ht  (height bmp)]
    (reduce (fn [acc [x y :as item]]
              (let [neigh (for [[x y :as pos] (filter (fn [[x y :as pos]]
                                                        (and (ut/between 0 x wid) (ut/between 0 y ht)))
                                                      (map (partial mapv + item) square-brush))]
                            (get-in bmp [y x]))]
                (cond-> acc
                  (and (= (get-in bmp [y x]) \space) (>= (count (remove #{\space} neigh)) amnt)) (assoc-in [y x] (first (conj (vec (remove #{\# \space} neigh)) \6))))))
            bmp
            (ut/cartesians wid ht))))

(defn antialias-high-contrast [bmp]
  (let [wid (width bmp)
        ht  (height bmp)]
    (reduce (fn [acc [x y :as item]]
              (let [square [(get-in bmp [y x]) (get-in bmp [y (inc x)]) (get-in bmp [(inc y) x]) (get-in bmp [(inc y) (inc x)])]]
                (cond-> acc
                  (= square [\space \# \# \space]) (-> (assoc-in [y x] \:)
                                                       (assoc-in [(inc y) (inc x)] \:))
                  (= square [\# \space \space \#]) (-> (assoc-in [(inc y) x] \:)
                                                       (assoc-in [y (inc x)] \:)))))
            bmp
            (ut/cartesians (dec wid) (dec ht)))))

(defn ezbitmap [ascii]
  (let [{:keys [art
                color-labels]} (strip-color-labels ascii)
        {:keys [bmp
                regions]}      (reduce (fn [{:keys [bmp
                                                    regions]
                                             :as   acc} item]
                                         (let [tree        (fill-tree item bmp)
                                               regions-new (flatten-tree tree)
                                               col-bmp     (regions->bmp 0 regions-new)
                                               full        (merge-bmps col-bmp bmp)]
                                           {:bmp     full
                                            :regions (concat regions regions-new)}))
                                       {:bmp     (digitize art)
                                        :regions []}
                                       [6 4 2])
        regions                (strip-region-dupes regions)
        overlaps               (:result (reduce (fn [{:keys [visited
                                                             result]
                                                      :as   acc}
                                                     item]
                                                  {:visited (se/union visited item)
                                                   :result  (if-let [k (seq (se/intersection visited item))]
                                                              (se/union result k)
                                                              result)})
                                                {:visited #{}
                                                 :result  #{}}
                                                regions))
        _                      (assert (not (seq overlaps)))
        regions                (vec regions)
        summary                (region-summary regions)
        large-region-cutoff    (best-large-region-cutoff {:bmp     (stringize bmp)
                                                          :summary summary})
        colors                 (cluster-colors large-region-cutoff summary color-labels)
        bmp                    (decolor-bmp bmp)
        bmp                    (reduce (fn [acc [pos color-index :as item]]
                                         (let [[x y] pos]
                                           (assoc-in acc [y x] (co/color->char color-index))))
                                       bmp
                                       (mapcat (fn [[id color-index]]
                                                 (let [region (regions id)]
                                                   (for [pos region]
                                                     [pos color-index])))
                                               colors))
        bmp (fill-high-neighbor-pixels 4 bmp)
        bmp (fill-high-neighbor-pixels 4 bmp)
        bmp (antialias-high-contrast bmp)]
    bmp))

(defn giant-bitmap-test []
  (let [giant-bmp (reduce (partial giant-bitmap 300)
                          []
                          (for [art (vals aa/art)]
                            (ezbitmap art)))]
    (spit-bmp "foo.rgb" giant-bmp)
    (vi/view "foo.rgb" (width giant-bmp) (height giant-bmp))))


;;(giant-bitmap-test)

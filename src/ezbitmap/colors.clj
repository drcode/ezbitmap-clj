(ns ezbitmap.colors
  (:require [fbc-utils.core :as ut]))

;;adapted from http://alumni.media.mit.edu/~wad/color/pal_comp.html
(def colors (into {}
                  (map (fn [{:keys [name]
                             :as   color}]
                         [name color])
                       [{:name  :black
                         :short :blk
                         :rgb   [0 0 0]}
                        {:name  :gray
                         :short :gry
                         :rgb   [100 100 100]}
                        {:name  :red
                         :short :red
                         :rgb   [173 35 35]}
                        {:name  :blue
                         :short :blu
                         :rgb   [42 75 215]}
                        {:name  :green
                         :short :grn
                         :rgb   [29 105 20]}
                        {:name  :brown
                         :short :brn
                         :rgb   [129 74 25]}
                        {:name  :purple
                         :short :prp
                         :rgb   [129 38 192]}
                        {:name  :silver
                         :short :slv
                         :rgb   [160 160 160]}
                        {:name  :lime
                         :short :lim
                         :rgb   [129 197 122]}
                        {:name  :teal
                         :short :tel
                         :rgb   [157 175 255]}
                        {:name  :sky
                         :short :sky
                         :rgb   [41 208 208]}
                        {:name  :orange
                         :short :org
                         :rgb   [255 146 51]}
                        {:name  :yellow
                         :short :yel
                         :rgb   [255 238 51]}
                        {:name  :tan
                         :short :tan
                         :rgb   [243 202 167]}
                        {:name  :pink
                         :short :pnk
                         :rgb   [255 180 180]}
                        {:name  :white
                         :short :wht
                         :rgb   [255 255 255]}])))

(def ascii-offsets [48 65 97 78])


(defn darken-channel [n]
  (int (* n 0.7)))

(defn lighten-rgb [rgb]
  (if (> (apply + rgb) 550)
    (mapv (fn [n]
            (int (+ 128 (/ n 2))))
          rgb)
    (mapv (fn [n]
            (min 255 (+ n 60)))
          rgb)))

(defn color->char [color]
  (char (+ (mod color 16) (ascii-offsets (int (/ color 16))))))

#_(color->char 16)

(defn lighten-color [color]
  (+ 16 color))

(defn darken-color [color]
  (+ 32 color))

(defn darken-color-a-lot [color]
  (+ 48 color))

(def color-priority [:blue :green :red :yellow :purple :orange :gray :pink :tan :brown :silver :lime :teal :sky :white :black])

(defn char->rgb [c]
  (first (filter identity
                 (map-indexed (fn [index off]
                                (let [n (- (int c) off)]
                                  (when (ut/between 0 n 16)
                                    (let [{:keys [rgb]
                                           :as   col} (colors (color-priority n))]
                                      (case index
                                        0 rgb
                                        1 (lighten-rgb rgb)
                                        2 (mapv darken-channel rgb)
                                        3 (mapv (comp darken-channel darken-channel) rgb))))))
                              ascii-offsets))))

(def color-names (reduce (fn [acc [index nam :as item]]
                           (assoc acc (name nam) index (name (:short (colors nam))) index))
                         {}
                         (map-indexed vector color-priority)))

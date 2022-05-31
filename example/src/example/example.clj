(ns example.example
  (:require [ezbitmap.ezbitmap :as ez]
            [ezbitmap.view-image :as vi]))

(def duck (ez/ezbitmap ["  __    "
                        "<(o )___"
                        " ( ._> /"
                        "  `---' "]))

(ez/spit-bmp "duck.rgb" duck)
(vi/view "duck.rgb" (ez/width duck) (ez/height duck))


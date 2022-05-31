(ns ezbitmap.ezbitmap-test
  (:require [ezbitmap.ezbitmap :as qa]
            [ezbitmap.font :as fo]
            [fbc-utils.debug :refer [loop-dbg let-dbg for-dbg map-dbg defn-dbg fn-dbg dbg-switch dbg-switch-force]]
            [fbc-utils.test :as te]))

(def smilie [":-)"
             "uWu"])

(def small-duck ["    AAAA        "
                 "AAAAAA  AA      "
                 "AAAAAA  AAAAAAAA"
                 "  AA      AA  AA"
                 "  AA  AAAAAA  A "
                 "    AAAAAAAAA   "])

(def small-duck-colors ["0000    11111111"            
                        "      33  111111"
                        "      33        "
                        "22  333333  44  "
                        "22  33      44 4"
                        "2222         444"])

(def box ["     "
          " AAA "
          " A A "
          " AAA "
          "     "])

(def leaky-box ["          "            
                "          "
                "  AAAAAA  "
                "  AAAAAA  "
                "  AA      "
                "  AA  AA  "
                "  AAAAAA  "
                "  AAAAAA  "
                "          "
                "          "])

(def smil (atom nil))

(te/test-file (reset! smil (qa/stringize (qa/digitize smilie)))
              ["           ##  "            
               "  ##         # "
               "     #####   # "
               "  ##         # "
               "           ##  "
               "     #   #     "
               "     # # #     "
               "#   ## # ##   #"
               "#   ## # ##   #"
               " #### ### ####"]
              (qa/regions box)
              [#{[4 3]          
                 [0 0]
                 [1 0]
                 [3 4]
                 [4 2]
                 [3 0]
                 [4 1]
                 [1 4]
                 [0 3]
                 [2 4]
                 [0 2]
                 [2 0]
                 [0 4]
                 [4 4]
                 [0 1]
                 [4 0]}
               #{[2 2]}]
              (qa/stringize (qa/regions->bmp (qa/regions box)))
              ["00000"
               "0   0"
               "0 1 0"
               "0   0"
               "00000"]
              (qa/stringize (qa/regions->bmp (qa/regions small-duck)))
              ["0000    11111111"            
               "      33  111111"
               "      33        "
               "22  333333  44  "
               "22  33      44 4"
               "2222         444"]
              (qa/stringize (qa/fatten-colors [[-1 0] [1 0] [0 -1] [0 1]] small-duck-colors))
              ["00000 3111111111"          
               "0000 33331111111"
               "22  333333111111"
               "2223333333344444"
               "2223333333 44444"
               "222223      4444"]
              (qa/strip-color-labels-row {:s "a tan foo red b" :labels []})
              {:s "a     foo     b", :labels [{:x 23/2, :color 3} {:x 7/2, :color 8}]}
              (qa/transpose ["lo" "ve"])
              ["lv" "oe"]
              (qa/strip-color-labels ["a tan foo red b" "#blue##########"])
              {:art          ["a     foo     b" "#    ##########"],          
               :color-labels [{:pos [23/2 0], :color 3}
                              {:pos [7/2 0], :color 8}
                              {:pos [3 1], :color 0}]}
              (qa/stringize (qa/giant-bitmap 10 (mapv vec ["####      "]) (mapv vec ["***" "* *" "***"])))
              ["####***   "
               "    * *   "
               "    ***   "]          
              start
              (qa/doit)
              nil
              )

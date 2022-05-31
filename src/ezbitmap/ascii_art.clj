(ns ezbitmap.ascii-art
  (:require [fbc-utils.core :as ut :refer [let->]]
            [fbc-utils.debug :as db :refer [let-dbg defn-dbg for-dbg dbg-switch fn-dbg dbg-switch-on-error dbg-switch-index]]))

(defn normalize-bmp [bmp]
  (let [wid (apply max (map count bmp))]
    (ut/forv [row bmp]
             (apply str (take wid (concat row (repeat \space)))))))

(def duck (normalize-bmp ["  __    "
                          "<(o )___"
                          " ( ._> /"
                          "  `---' "]))

;;plane credit jgs

(def plane (normalize-bmp ["            __/\\__"
                           "            ==/\\=="
                           " ____________/__\\____________"
                           "/____________________________\\"
                           "  __||__||__/.--.\\__||__||__"
                           " /__|___|___( >< )___|___|__\\"
                           "           _/`--`\\_"
                           "          (/------\\)"]))


(def mario (normalize-bmp [" red_____"
                           "    L____\\__"
                           "   (A_/ O`-.tan"
                           "   `Ytan k-'"
                           "   ,+-m--("
                           "t (_  H  H`n-.t"
                           "a/  )_H--H_/ |a"
                           "n`-y  o  o +-'n"
                           "  /---+-+--\\"
                           "  ----` `---`"
                           "   red    red"]))

(def truck (normalize-bmp [" ____|~\\_"
                           "[ _ _|_|-]"
                           " (_)   (_)"]))

(def ship (normalize-bmp ["  __|__ |___| |\\"
                          "  |o__| |___| | \\"
                          "  |___| |___| |o \\"
                          " _|___| |___| |__o\\"
                          "/...\\_____|___|____\\_/"
                          "\\   o * o * * o o  /"
                          " ^^^^^^^^^^^^^^^^^^^"]))

(def bike (normalize-bmp [" ------- __@"
                          " ----- _`\\<,_"
                          " ---- (_)/ (_)"]))

(def parrot (normalize-bmp ["    __,---."
                            "   /__|o\\  )"
                            "    \"-\\ / /"
                            "      ,) (,"
                            "     //   \\\\"
                            "    {(     )}"
                            "#=====M===M=====#"
                            "      T|||T"
                            "       |||"
                            "        |"]))

(def disk (normalize-bmp [" ______________"
                          "|[]            |"
                          "|  __________  |"
                          "|  |Quickie |  |"
                          "|  |  Art   |  |"
                          "|  |________|  |"
                          "|   ________   |"
                          "|   [ [ ]  ]   |"
                          "|   [ [ ]  ]   |"
                          "\\___[_[_]__]___|"]))

(def donkey (normalize-bmp ["            //"
                            "  .--._.-^-(.}"
                            "y{        / \\d"
                            "  )  ._.- >"
                            "  |`T   /'"]))

(def mermaid (normalize-bmp ["     @@@"
                             "    @(\"}_"
                             "   @(.(.)\\_"
                             "  @@@) ( "
                             "    (-.-\\"
                             "\\    )  /"
                             "\\\\.-\"  /"
                             "//^~-~\""
                             "/"]))

(def shark (normalize-bmp ["        ,"
                           "      __)\\_"
                           "(\\_.-'    a`-."
                           "(/~~````(/~^^`"]))

(def semi (normalize-bmp ["           _________________"
                          "   .--H--.|                 |"
                          " _//_||  ||                 |"
                          "[    -|  |'--;--------------'"
                          "'-()-()----()\"()^^^^^^^()\"()'"]))

(def turtle (normalize-bmp ["     _.._    _ "
                            "   .\"\\__/\"./`_\\"
                            " _/__<__>__\\/"
                            " \"/_/\"\"\"\"\\_\\\\"]))

(def mouse (normalize-bmp ["            c._"
                           "   .\"```\"-\"C  o'-."
                           " _/   \\       _..'"
                           "~-(  _/--.<<-'"
                           "  `\\)      \\)"]))

(def spider (normalize-bmp ["  //  \\\\"
                            " _\\\\()//_"
                            "/ //  \\\\ \\"
                            " | \\__/ |"]))

(def boar (normalize-bmp ["   .-~~~~-. |\\\\_"
                          "@_/        /  oo\\_"
                          "  [ pnk\\   \\pnk_(\")"
                          "  \\   /-| ||'--'"
                          "   \\_\\  \\_\\\\"]))

(def dog (normalize-bmp ["     __"
                         "(___()'~"
                         "/,    /`"
                         "\\\\\"--\\\\"]))

(def art {:duck    duck
          :plane   plane
          :mario   mario
          :truck   truck
          :ship    ship
          :bike    bike
          :parrot  parrot
          :disk    disk
          :donkey  donkey
          :mermaid mermaid
          :shark   shark
          :semi    semi
          :turtle  turtle
          :mouse   mouse
          :spider  spider
          :boar    boar
          :dog     dog})

(defn print-art []
  (doseq [[k v] art]
    (doall (map println v))))

;;(print-art)

(ns ezbitmap.view-image
  (:require [fbc-utils.core :as ut :refer [let->]]
            [fbc-utils.debug :as db :refer [let-dbg defn-dbg for-dbg dbg-switch fn-dbg dbg-switch-on-error dbg-switch-index]]
            [clojure.java.shell :as sh]
            [clojure.core.async :as as]))

(def shell-definitions {})

(defn apply-shell-defs [s]
  (reduce (fn [acc [k v :as item]]
            (clojure.string/replace acc (re-pattern (str "\\{" (name k) "\\}")) v))
          s
          shell-definitions))

(defn ! [s]
  (let [s                                 (apply-shell-defs s)
        {:keys [out exit err] :as result} (sh/sh "bash" "-c" s)]
    (when-not (zero? exit)
      (ut/throw (str "exit failed: " err)))
    out))

(defn view [fname wid ht]
  (let [dir (apply str (butlast (:out (sh/sh "pwd"))))]
    (! (str "convert -size " wid "x" ht " -depth 8 rgba:" dir "/" fname " " dir "/out.png"))
    (as/thread (! "viewnior --fullscreen out.png &"))))


# Introduction

See [ezbitmap.com](https://ezbitmap.com) to learn more about this library.

This library will generate a full-color bitmap from ASCII art in Clojure. Note that it is not identical to the javascript version, and is somewhat less advanced: It has no upscaling support, and will generate slightly different colors than the javascript version.

To use it, include `ezbitmap-clj` in your `deps.edn` file:

```clojure
{:deps {io.github.drcode/ezbitmap-clj {:git/tag "1.0.0"
                                       :git/sha "9465053"}}}
```

The main library will generate a RAW bitmap file in a byte array.

If you want to view the bmp, you will need to install the `imagemagick` and `viewnior` packages. In linux:

```
sudo apt-get install imagemagick viewnior
```

Now you can create and view a bitmap as follows:

```clojure
(ns example.example
  (:require [ezbitmap.ezbitmap :as ez]
            [ezbitmap.view-image :as vi]))

(def duck (ez/ezbitmap ["  __    "
                        "<(o )___"
                        " ( ._> /"
                        "  `---' "]))

(ez/spit-bmp "duck.rgb" duck)
(vi/view "duck.rgb" (ez/width duck) (ez/height duck))
```

See the example directory for a functioning example.

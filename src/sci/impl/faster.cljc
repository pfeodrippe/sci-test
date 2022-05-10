(ns sci.impl.faster
  {:no-doc true}
  #?(:cljs (:require-macros [sci.impl.faster :refer [nth-2 assoc-3 get-2]])))

(defmacro nth-2
  [c i]
  `(~'-nth ~c ~i))

(defmacro assoc-3
  [m k v]
  `(~'-assoc ~m ~k ~v))

(defmacro get-2
  [m k]
  `(get ~m ~k)
  #_`(.get ~m ~k))

(defmacro deref-1
  [ref]
  `(~'-deref ~ref))

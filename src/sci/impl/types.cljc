(ns sci.impl.types
  {:no-doc true}
  (:refer-clojure :exclude [eval])
  #?(:clj (:require [sci.impl.macros :as macros]
                    [missing.stuff :refer [instance?]]))
  #?(:cljs (:require-macros [sci.impl.macros :as macros]
                            [sci.impl.types :refer [->Node]]))
  #?(:clj (:import [sci.impl.types IReified])))

#?(:cljd ()
   :clj (set! *warn-on-reflection* true))

(defprotocol IBox
  (setVal [_this _v])
  (getVal [_this]))

(defprotocol IReified
  (getInterfaces [_])
  (getMethods [_])
  (getProtocols [_]))

#?(:cljd ()
   :clj
   (do (defn getMethods [obj]
         (.getMethods ^IReified obj))
       (defn getInterfaces [obj]
         (.getInterfaces ^IReified obj))
       (defn getProtocols [obj]
         (.getProtocols ^IReified obj))))

(deftype Reified [interfaces meths protocols]
  IReified
  (getInterfaces [_] interfaces)
  (getMethods [_] meths)
  (getProtocols [_] protocols))

(defn type-impl [x & _xs]
  (or (when (instance? #?(:clj sci.impl.types/IReified :cljs sci.impl.types/Reified) x)
        :sci.impl.protocols/reified)
      (some-> x meta :type)
      #?(:clj (identity x) ;; no need to check for metadata anymore
         :cljs (type x))))



;; returned from analyzer when macroexpansion needs interleaved eval
(deftype EvalForm [form]
  IBox
  (getVal [_this] form))

(defprotocol Stack
  (stack [this]))

#?(:cljd (extend-type Object
           Stack
           (stack [_this] nil))
   :clj (extend-protocol Stack
          Object (stack [_this] nil))
   :cljs (extend-protocol Stack
           default (stack [_this] nil)))

#?(:cljd (defprotocol Eval
           (eval [expr ctx bindings]))
   :clj (defprotocol Eval
          (eval [expr ctx ^objects bindings])))

#?(:cljs
   (defrecord NodeR [f stack]
     Stack (stack [_] stack)))

#?(:cljs
   ;; For performance reasons on CLJS we do not use eval as a protcol method but
   ;; as a separate function which does an instance check on a concrete type.
   (defn eval [expr ctx bindings]
     (if (instance? NodeR expr)
       ((.-f expr) expr ctx bindings)
       expr)))

(defmacro ->Node
  [body stack]
  `(reify
     sci.impl.types/Eval
     (~'eval [~'this ~'ctx ~'bindings]
      ~body)
     sci.impl.types/Stack
     (~'stack [_#] ~stack)))

#_( #?(:cljd
    (defmacro ->Node
      [body stack]
      10
      #_`(reify
           sci.impl.types/Eval
           (~'eval [~'this ~'ctx ~'bindings]
            ~body)
           sci.impl.types/Stack
           (~'stack [_#] ~stack)))

    #_ #_:clj
    (macros/deftime
      (defmacro ->Node
        [body stack]
        (macros/?
         :clj `(reify
                 sci.impl.types/Eval
                 (~'eval [~'this ~'ctx ~'bindings]
                  ~body)
                 sci.impl.types/Stack
                 (~'stack [_#] ~stack))
         :cljs `(->NodeR
                 (fn [~'this ~'ctx ~'bindings]
                   ~body)
                 ~stack))))

    :cljs
    (macros/deftime
      (defmacro ->Node
        [body stack]
        (macros/?
         :clj `(reify
                 sci.impl.types/Eval
                 (~'eval [~'this ~'ctx ~'bindings]
                  ~body)
                 sci.impl.types/Stack
                 (~'stack [_#] ~stack))
         :cljs `(->NodeR
                 (fn [~'this ~'ctx ~'bindings]
                   ~body)
                 ~stack))))))

#?(:clj
   (deftype ConstantNode [x]
     Eval (eval [_expr _bindings _ctx]
            x)
     Stack (stack [_] nil)))

(defn ->constant [x]
  #?(:clj (->ConstantNode x)
     :cljs x))

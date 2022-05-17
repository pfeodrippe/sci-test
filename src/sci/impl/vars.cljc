(ns sci.impl.vars
  {:no-doc true}
  (:refer-clojure :exclude [var? binding
                            push-thread-bindings
                            get-thread-bindings
                            pop-thread-bindings
                            with-bindings*
                            with-bindings
                            thread-bound?
                            alter-var-root
                            var-get
                            var-set
                            bound-fn*])
  (:require [sci.impl.macros :as macros]
            [sci.impl.types :as t]
            [sci.impl.unrestrict :refer [*unrestricted*]]
            [sci.lang]
            [missing.stuff :refer [instance?]])
  #?(:cljs (:require-macros [sci.impl.vars :refer [with-bindings
                                                   with-writeable-namespace
                                                   with-writeable-var]])))

#?(:cljd ()
   :clj (set! *warn-on-reflection* true))

(defprotocol HasName ;; INamed was already taken by CLJS
  (getName [_]))

(macros/deftime
  (defmacro with-writeable-namespace
    [the-ns-object ns-meta & body]
    `(let [m# ~ns-meta]
       (if (or *unrestricted* (not (:sci/built-in m#)))
         (do ~@body)
         (let [ns-obj# ~the-ns-object
               name# (getName ns-obj#)]
           (throw (ex-info (str "Built-in namespace " name# " is read-only.")
                           {:ns ns-obj#})))))))

(deftype SciNamespace [name #?(:clj ^:volatile-mutable meta
                               :cljs ^:mutable meta)]
  Object
  (toString [_]
    (str name))
  HasName
  (getName [_] name)
  #?(:cljd cljd.core/IMeta
     #_ #_:clj clojure.lang.IMeta
     :cljs IMeta)
  #?(:cljd (-meta [_] meta)
     :clj (clojure.core/meta [_] meta)
     :cljs (-meta [_] meta))
  ;; #?(:clj clojure.lang.IReference)
  ;; #?(:clj (alterMeta [this f args]
  ;;                    (with-writeable-namespace this meta
  ;;                      (locking (set! meta (apply f meta args))))))
  ;; #?(:clj (resetMeta [this m]
  ;;                    (with-writeable-namespace this meta
  ;;                      (locking (set! meta m)))))
  )

(defn namespace? [x]
  (instance? #?(:cljd sci.impl.vars/SciNamespace
                :clj sci.impl.vars.SciNamespace
                :cljs sci.impl.vars/SciNamespace) x))

(deftype Frame [bindings prev])

(def top-frame (Frame. {} nil))

#?(:cljd
   (def dvals (volatile! top-frame))

   #_ #_:clj
   (def ^ThreadLocal dvals (proxy [ThreadLocal] []
                             (initialValue [] top-frame)))
   :cljs
   (def dvals (volatile! top-frame)))

(defn get-thread-binding-frame ^Frame []
  #?(:cljd @dvals
     :clj (.get dvals)
     :cljs @dvals))

(deftype TBox #?(:clj [thread ^:mutable val]
                 :cljs [thread ^:mutable val])
  t/IBox
  (setVal [this v]
    (set! val v))
  (getVal [this] val))

(defn clone-thread-binding-frame ^Frame []
  (let [^Frame f #?(:cljd @dvals
                    :clj (.get dvals)
                    :cljs @dvals)]
    (Frame. (.-bindings f) nil)))

(defn reset-thread-binding-frame [frame]
  #?(:cljd (vreset! dvals frame)
     :clj (.set dvals frame)
     :cljs (vreset! dvals frame)))

(declare var?)

(defn dynamic-var? [v]
  ;; TODO: make separate field
  (and (var? v)
       (:dynamic (meta v))))

(defprotocol IVar
  (bindRoot [this v])
  (getRawRoot [this])
  (toSymbol [this])
  (isMacro [this])
  (hasRoot [this])
  (setThreadBound [this v])
  (unbind [this]))

(defn push-thread-bindings [bindings]
  (let [^Frame frame (get-thread-binding-frame)
        bmap (.-bindings frame)
        bmap (reduce (fn [acc [var* val*]]
                       (when-not (dynamic-var? var*)
                         (throw (new #?(:cljd Exception
                                        :clj IllegalStateException
                                        :cljs js/Error)
                                     (str "Can't dynamically bind non-dynamic var " var*))))
                       (setThreadBound var* true)
                       (assoc acc var* (TBox. #?(:cljd nil
                                                 :clj (Thread/currentThread)
                                                 :cljs nil) val*)))
                     bmap
                     bindings)]
    (reset-thread-binding-frame (Frame. bmap frame))))

(defn pop-thread-bindings []
  ;; type hint needed to satisfy CLJS compiler / shadow
  (if-let [f (.-prev ^Frame (get-thread-binding-frame))]
    (if (identical? top-frame f)
      #?(:cljd (vreset! dvals top-frame)
         :clj (.remove dvals)
         :cljs (vreset! dvals top-frame))
      (reset-thread-binding-frame f))
    (throw (new #?(:clj Exception :cljs js/Error) "No frame to pop."))))

(defn get-thread-bindings []
  (let [;; type hint added to prevent shadow-cljs warning, although fn has return tag
        ^Frame f (get-thread-binding-frame)]
    (loop [ret {}
           kvs (seq (.-bindings f))]
      (if kvs
        (let [[var* ^TBox tbox] (first kvs)
              tbox-val (t/getVal tbox)]
          (recur (assoc ret var* tbox-val)
                 (next kvs)))
        ret))))

(defn get-thread-binding ^TBox [sci-var]
  (when-let [;; type hint added to prevent shadow-cljs warning, although fn has return tag
             ^Frame f #?(:cljd @dvals
                         :clj (.get dvals)
                         :cljs @dvals)]
    #?(:cljd (get ^cljd.core/PersistentHashMap (.-bindings f) sci-var)
       :clj (.get ^java.util.Map (.-bindings f) sci-var)
       :cljs (.get (.-bindings f) sci-var))))

(defn binding-conveyor-fn
  [f]
  (let [frame (clone-thread-binding-frame)]
    (fn
      ([]
       (reset-thread-binding-frame frame)
       (f))
      ([x]
       (reset-thread-binding-frame frame)
       (f x))
      ([x y]
       (reset-thread-binding-frame frame)
       (f x y))
      ([x y z]
       (reset-thread-binding-frame frame)
       (f x y z))
      ([x y z & args]
       (reset-thread-binding-frame frame)
       (apply f x y z args)))))

(defn throw-unbound-call-exception [the-var]
  (throw (new #?(:cljd Exception
                 :clj IllegalStateException
                 :cljs js/Error) (str "Attempting to call unbound fn: " the-var))))

(deftype SciUnbound [the-var]
  Object
  (toString [_]
    (str "Unbound: " the-var))
  #?@(:cljd [cljd.core/IFn] :clj [cljd.core/IFn] :cljs [IFn])
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [_]
    (throw-unbound-call-exception the-var))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a]
    (throw-unbound-call-exception the-var))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b]
    (throw-unbound-call-exception the-var))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c]
    (throw-unbound-call-exception the-var))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c d]
    (throw-unbound-call-exception the-var))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c d e]
    (throw-unbound-call-exception the-var))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c d e f]
    (throw-unbound-call-exception the-var))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c d e f g]
    (throw-unbound-call-exception the-var))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c d e f g h]
    (throw-unbound-call-exception the-var))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c d e f g h i]
    (throw-unbound-call-exception the-var))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c d e f g h i j]
    (throw-unbound-call-exception the-var))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c d e f g h i j k]
    (throw-unbound-call-exception the-var))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c d e f g h i j k l]
    (throw-unbound-call-exception the-var))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m]
    (throw-unbound-call-exception the-var))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n]
    (throw-unbound-call-exception the-var))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o]
    (throw-unbound-call-exception the-var))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p]
    (throw-unbound-call-exception the-var))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q]
    (throw-unbound-call-exception the-var))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q r]
    (throw-unbound-call-exception the-var))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q r s]
    (throw-unbound-call-exception the-var))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q r s t]
    (throw-unbound-call-exception the-var))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q r s t rest]
    (throw-unbound-call-exception the-var))
  #?(:clj
     (applyTo [_ args]
              (throw-unbound-call-exception the-var))))

;; adapted from https://github.com/clojure/clojurescript/blob/df1837048d01b157a04bb3dc7fedc58ee349a24a/src/main/cljs/cljs/core.cljs#L1118

(defn built-in-var? [var-meta]
  (:sci/built-in var-meta))

(macros/deftime
  (defmacro with-writeable-var
    [the-var var-meta & body]
    `(let [vm# ~var-meta]
       (if (or *unrestricted* (not (:sci/built-in vm#)))
         (do ~@body)
         (let [the-var# ~the-var
               ns# (:ns vm#)
               ns-name# (getName ns#)
               name# (getName the-var#)]
           (throw (ex-info (str "Built-in var #'" ns-name# "/" name# " is read-only.")
                           {:var ~the-var})))))))

(deftype SciVar [#?(:cljd ^:mutable root
                    :clj ^:volatile-mutable root
                    :cljs ^:mutable root)
                 sym
                 #?(:cljd ^:mutable meta
                    :clj ^:volatile-mutable meta
                    :cljs ^:mutable meta)
                 #?(:cljd ^:mutable thread-bound
                    :clj ^:volatile-mutable thread-bound
                    :cljs ^:mutable thread-bound)]
  #?(:cljd
     (^:setter root2 [this v]
      (set! root v)))
  #?(:cljd
     (^:setter thread_bound2 [this v]
      (set! thread-bound v)))
  #?(:clj
     ;; marker interface, clj only for now
     sci.lang/IVar)
  HasName
  (getName [this]
    (or (:name meta) sym))
  IVar
  (bindRoot [this v]
    (with-writeable-var this meta
      (set! (.-root2 this) v)))
  (getRawRoot [this]
    root)
  (toSymbol [this]
                                        ; if we have at least a name from metadata, then build the symbol from that
    (if-let [sym-name (some-> (:name meta) name)]
      (symbol (some-> (:ns meta) getName name) sym-name)
                                        ; otherwise, fall back to the symbol
      sym))
  (isMacro [_]
    (or (:macro meta)
        (when-some [m (clojure.core/meta root)]
          (:sci/macro m))))
  (setThreadBound [this v]
    (set! (.-thread_bound2 this) v))
  (unbind [this]
    (with-writeable-var this meta
      (set! (.-root2 this) (SciUnbound. this))))
  (hasRoot [this]
    (not (instance? SciUnbound root)))
  t/IBox
  (setVal [this v]
    (if-let [b (get-thread-binding this)]
      #?(:cljd (t/setVal b v)
         :clj
         (let [t (.-thread b)]
           (if (not (identical? t (Thread/currentThread)))
             (throw (new IllegalStateException
                         (format "Can't set!: %s from non-binding thread" (toSymbol this))))
             (t/setVal b v)))
         :cljs (t/setVal b v))
      (throw (new #?(:cljd Exception :clj IllegalStateException :cljs js/Error)
                  (str "Can't change/establish root binding of " this " with set")))))
  (getVal [this] root)
  #?(:cljd cljd.core/IDeref :clj cljd.core/IDeref :cljs IDeref)
  (#?(:cljd -deref
      :clj deref
      :cljs -deref) [this]
    (if thread-bound
      (if-let [tbox (get-thread-binding this)]
        (t/getVal tbox)
        root)
      root))
  Object
  (toString [this]
    (str "#'" (toSymbol this)))
  #?(:cljs IPrintWithWriter)
  #?(:cljs (-pr-writer [a writer opts]
                       (-write writer "#'")
                       (pr-writer (toSymbol a) writer opts)))
  #?(:clj cljd.core/IMeta :cljs IMeta)
  #?(:cljd (-meta [_] meta)
     :clj (clojure.core/meta [_] meta)
     :cljs (-meta [_] meta))
  ;; #?(:clj Comparable :cljs IEquiv)
  ;; (-equiv [this other]
  ;;   (if (instance? Var other)
  ;;     (= (.-sym this) (.-sym other))
  ;;     false))
  ;; #?(:clj clojure.lang.IHashEq :cljs IHash)
  ;; (-hash [_]
  ;;   (hash-symbol sym))
  ;; #?(:clj clojure.lang.IReference)
  ;; #?(:clj (alterMeta [this f args]
  ;;                    (with-writeable-var this meta
  ;;                      (locking (set! meta (apply f meta args))))))
  ;; #?(:clj (resetMeta [this m]
  ;;                    (with-writeable-var this meta
  ;;                      (locking (set! meta m)))))
  ;; #?(:clj clojure.lang.IRef)
  ;; added for multi-methods

  ;; #?(:cljs Fn) ;; In the real CLJS this is there... why?
  #?(:cljd cljd.core/IFn #_ #_:clj clojure.lang.IFn :cljs IFn)
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this]
    (@this))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a]
    (@this a))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b]
    (@this a b))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c]
    (@this a b c))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d]
    (@this a b c d))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e]
    (@this a b c d e))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f]
    (@this a b c d e f))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g]
    (@this a b c d e f g))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g h]
    (@this a b c d e f g h))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g h i]
    (@this a b c d e f g h i))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g h i j]
      (@this a b c d e f g h i j))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g h i j k]
      (@this a b c d e f g h i j k))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g h i j k l]
      (@this a b c d e f g h i j k l))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g h i j k l m]
      (@this a b c d e f g h i j k l m))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g h i j k l m n]
      (@this a b c d e f g h i j k l m n))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g h i j k l m n o]
      (@this a b c d e f g h i j k l m n o))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g h i j k l m n o p]
      (@this a b c d e f g h i j k l m n o p))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g h i j k l m n o p q]
      (@this a b c d e f g h i j k l m n o p q))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g h i j k l m n o p q r]
      (@this a b c d e f g h i j k l m n o p q r))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g h i j k l m n o p q r s]
      (@this a b c d e f g h i j k l m n o p q r s))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g h i j k l m n o p q r s t]
      (@this a b c d e f g h i j k l m n o p q r s t))
  #_(#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g h i j k l m n o p q r s t rest]
      (apply @this a b c d e f g h i j k l m n o p q r s t rest))
  #?(:cljd
     (-apply [this args]
             (apply @this args))
     :clj
     (applyTo [this args]
              (apply @this args))))

#?(:cljd ()
   :clj
   ;; Use public interface for print-method so it can be overriden in bb itself
   (do (defmethod print-method sci.lang/IVar [o ^java.io.Writer w]
         (.write w (str "#'" (toSymbol ^sci.impl.vars/IVar o))))
       (prefer-method print-method sci.lang/IVar clojure.lang.IDeref)))

(defn var-get [v]
  (deref v))

(defn var-set [v val]
  (t/setVal v val))

(defn var? [x]
  (instance? #?(:cljd sci.impl.vars/SciVar
                :clj sci.impl.vars.SciVar
                :cljs sci.impl.vars/SciVar) x))

(defn unqualify-symbol
  "If sym is namespace-qualified, remove the namespace, else return sym"
  [sym]
  (if (qualified-symbol? sym)
    (symbol (name sym))
    sym))

(defn dynamic-var
  ([name]
   (dynamic-var name nil (meta name)))
  ([name init-val]
   (dynamic-var name init-val (meta name)))
  ([name init-val meta]
   (let [meta (assoc meta :dynamic true :name (unqualify-symbol name))]
     (SciVar. init-val name meta false))))

;; foundational namespaces
(def user-ns (->SciNamespace 'user nil))

(def clojure-core-ns (->SciNamespace 'clojure.core nil))


(def current-file (dynamic-var '*file* nil {:ns clojure-core-ns}))

(def current-ns (dynamic-var '*ns* user-ns {:ns clojure-core-ns}))

(defn current-ns-name []
  (getName @current-ns))

(macros/deftime
  (defmacro with-bindings
    "Macro for binding sci vars for internal use."
    [bindings & body]
    `(do
       ;; important: outside try
       (vars/push-thread-bindings ~bindings)
       (try
         (do ~@body)
         (finally
           (vars/pop-thread-bindings))))))

(defn alter-var-root
  ([v f]
   #?(:cljd (bindRoot v (f (getRawRoot v)))
      :clj (locking v (bindRoot v (f (getRawRoot v))))
      :cljs (bindRoot v (f (getRawRoot v)))))
  ([v f & args]
   #?(:cljd (bindRoot v (f (getRawRoot v)))
      :clj (locking v (bindRoot v (apply f (getRawRoot v) args)))
      :cljs (bindRoot v (apply f (getRawRoot v) args)))))

(defn new-var
  "Returns a new sci var."
  ([name] (doto (new-var name nil nil)
            (unbind)))
  ([name init-val] (new-var name init-val (meta name)))
  ([name init-val meta] (->SciVar init-val name (assoc meta :name (unqualify-symbol name)) false)))

(comment
  (def v1 (SciVar. (fn [] 0) 'foo nil))
  @v1 ;; 0
  (push-thread-bindings {v1 2})
  (get-thread-binding v1) ;; 2
  (push-thread-bindings {v1 3})
  (get-thread-binding v1) ;; 3
  (pop-thread-bindings)
  (get-thread-binding v1) ;; 2
  (pop-thread-bindings)
  (get-thread-binding v1) ;; nil
  @v1 ;; 0
  (pop-thread-bindings) ;; exception
  )

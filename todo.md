- [x] Add clojure tools reader
- [ ] Add edamame

## Issues
### Can't implement a type using namespaced methods
Only complains at runtime.

``` clojure
(deftype Eita [meta2]
  cljd.core/IMeta
  (cljd.core/meta [_] meta2))

```
;; =>
``` shell
Failed to build dartapp:dartapp:
lib/cljd-out/quickstart/helloworld.dart:18:12: Error: A method declaration needs an explicit list of parameters.
Try adding a parameter list to the method declaration.
dc.dynamic cljd.core/meta(){
           ^^^^
lib/cljd-out/quickstart/helloworld.dart:18:21: Error: Expected '{' before this.
dc.dynamic cljd.core/meta(){
                    ^
lib/cljd-out/quickstart/helloworld.dart:18:12: Error: The name of a constructor must match the name of the enclosing class.
dc.dynamic cljd.core/meta(){
           ^^^^
lib/cljd-out/quickstart/helloworld.dart:18:1: Error: Constructors can't have a return type.
Try removing the return type.
dc.dynamic cljd.core/meta(){
```

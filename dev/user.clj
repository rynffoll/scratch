(ns user
  (:require [clojure.tools.deps.alpha.repl :refer [add-lib]]))

(comment
  
  (add-lib 'expound {:mvn/version "0.7.2"})

  (add-lib 'metosin/spec-tools {:mvn/version "0.10.0"})

  (add-lib 'cheshire {:mvn/version "5.9.0"})

  )

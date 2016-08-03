(ns ring.swagger.json-schema.spec
  (:require [clojure.spec :as spec]
            [clojure.core.match :refer [match]]
            [ring.swagger.common :as c]))

(defn keys->swagger
  [{:keys [req req-un opt opt-un]}]
  (let [properties (into {} (map (juxt name spec-object)
                                 (concat req req-un opt opt-un)))]
    {:type "object"
     :properties properties
     :required (map name (concat req req-un))
     :additionalProperties false}))

(defn spec-object
  "Returns a JSON schema matching a spec."
  [spec]
  (match (if (symbol? spec) spec (spec/describe spec))
    (['keys & args]    :seq) (keys->swagger args)
    (['tuple & args]   :seq) {:type "array" :items (map spec-object args) :minItems (count args)}
    (['every type & _] :seq) {:type "array" :items (spec-object type)}
    (['cat & args]     :seq) {:type "array"}  ; the regex support makes this special!
    (['and & args]     :seq) {:allOf (map spec-object args)}
    (['or & args]      :seq) {:anyOf (map spec-object args)}
    (['* & args]       :seq) {:type "array" :items (spec-object type)}
    (['+ & args]       :seq) {:type "array" :items (spec-object type) :minItems 1}
    (a-set :guard set?)      {:enum (vec a-set)}
    'int? {:type "integer"}
    'float? {:type "number"}
    'string? {:type "string"}
    :else {}))

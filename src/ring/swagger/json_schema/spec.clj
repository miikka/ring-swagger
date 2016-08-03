(ns ring.swagger.json-schema.spec
  (:require [clojure.spec :as spec]
            [clojure.core.match :refer [match]]
            [ring.swagger.common :as c]))

(defn map-spec?
  [spec]
  (let [desc (spec/describe spec)]
    (when (seq? desc)
      (= 'keys (first desc)))))

(defn properties
  [spec]
  {:pre [(map-spec? spec)]}
  (let [{:keys [req req-un opt opt-un]} (seq (rest (spec/describe spec)))
        unk #(-> % name)]
    (into {} (map (juxt name spec-object) (concat req req-un opt opt-un)))))

(defn spec-object
  "Returns a JSON schema matching a spec."
  [spec]
  (match (if (symbol? spec) spec (spec/describe spec))
    (['keys & _]       :seq) {:type "object" :properties (properties spec)}
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

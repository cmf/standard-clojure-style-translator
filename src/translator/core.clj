(ns translator.core
  (:require [clojure.data.json :as json]
            [clojure.java.io :as jio]
            [clojure.string :as str])
  (:import (java.net URI)
           (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
           (java.time Duration)))

(defonce whole-text (slurp "/Users/colin/dev/standard-clojure-style-js/lib/standard-clojure-style.js"))
(defonce lines (vec (line-seq (jio/reader "/Users/colin/dev/standard-clojure-style-js/lib/standard-clojure-style.js"))))

(defn find-index
  ([pattern]
   (find-index pattern 0))
  ([pattern index]
   (loop [i index]
     (if (>= i (count lines))
       nil
       (if (re-find pattern (nth lines i))
         i
         (recur (inc i)))))))


(defn find-chunks []
  (let [; Start after the module stuff, use + 2 here to skip this line
        first-index (+ 2 (find-index #"use strict"))
        ; End at the public API section
        last-index (dec (find-index #"^\s+// Public API"))
        splits (loop [index first-index
                      start-index first-index
                      ret [first-index]]
                 (if-let [next-index (find-index #"^\s+function" (inc index))]
                   ; Is there room in this chunk?
                   (if (< next-index (+ start-index 500))
                     (recur next-index start-index ret)
                     ; No room, make a new chunk
                     ; We check here whether index == start-index, because some functions are larger than 500 lines
                     (if (= index start-index)
                       (recur next-index next-index (conj ret next-index))
                       (recur index index (conj ret index))))
                   ; No more functions, end at last-index
                   (conj ret last-index)))
        ranges (partition 2 1 splits)]
    (into []
          (map (fn [[from to]]
                 (str/join "\n" (subvec lines from to))))
          ranges)))

(comment
  (find-index #"^\s+// Parser Definitions")
  (re-find #"use strict" "  'use strict'")
  (count (find-chunks))
  (println (nth (find-chunks) 4)))

(def anthropic-endpoint "https://api.anthropic.com/v1/messages")
(def anthropic-key (-> (str (System/getProperty "user.home") "/.api-keys/anthropic")
                       (slurp)
                       (str/trim)))
(def anthropic-version "2023-06-01")

(defonce http-client (delay
                       (-> (HttpClient/newBuilder)
                           (.connectTimeout (Duration/ofSeconds 10))
                           (.build))))

(def system
  [{:type "text"
    :text "
You are an expert software developer specialising in translating JavaScript code to Python.
You have been tasked with translating a large JavaScript file to Python. I will provide the
full file as a reference, and then I will provide a chunk of the file.

1. You should translate the passed chunk, referring to the whole file if necessary.
2. Return *only* the translated code, nothing else, no commentary, no markdown fences.
3. You must retain all comments from the original code in the translation.
4. You *MUST* provide a complete translation, even if the chunk is large!
"}
   {:type          "text"
    :text          (str "Here is the original file:\n```javascript\n" whole-text "\n```\n")
    :cache-control {:type "ephemeral"}}])

(defn call-claude-api
  [messages]
  (let [request {:model      "claude-3-5-sonnet-20240620"
                 :system     system
                 :max-tokens 8192
                 :messages   messages}
        body (json/write-str request :key-fn #(str/replace (name %) "-" "_"))
        request (-> (HttpRequest/newBuilder)
                    (.uri (URI/create anthropic-endpoint))
                    (.header "Content-Type" "application/json")
                    (.header "X-API-Key" anthropic-key)
                    (.header "anthropic-version" anthropic-version)
                    (.header "anthropic-beta" "prompt-caching-2024-07-31,max-tokens-3-5-sonnet-2024-07-15")
                    (.POST (HttpRequest$BodyPublishers/ofString body))
                    (.build))
        response (.send @http-client request (HttpResponse$BodyHandlers/ofInputStream))
        status (.statusCode response)]
    (if (>= status 400)
      (let [error-body (slurp (.body response))]
        (throw (ex-info "API request failed" {:status status
                                              :body   error-body})))
      (json/read-str (str/trim (slurp (.body response))) :key-fn #(keyword (str/replace % "_" "-"))))))

(defn translate-file []
  (reduce (fn [ret chunk]
            (let [response (call-claude-api [{:role    "user"
                                              :content (str "Here is the chunk to translate:\n```javascript\n"
                                                            chunk
                                                            "\n```\n")}])]
              (spit "translation.py" (get-in response [:content 0 :text]) :append true)
              (spit "translation.py" "\n" :append true)
              (merge-with + ret (:usage response))))
          {}
          (find-chunks)))

(comment
  (translate-file)

  (println (str/join "\n" (subvec lines 2117 2396))))

(ns wikipedia-game.wiki-parser
  (:require [net.cgrand.enlive-html :as h]
            [clojure.string :as s]
            [clojure.java.io :as jio])
  (:import [java.net URL]
           (java.io FileNotFoundException)))

(def base-page-link "https://en.wikipedia.org/wiki/")

(defn parse-page [url]
  (let [html (URL. url)]
    (h/html-resource html)))

(defn extract-link-text [partial-link]
  ; TODO: Regex seems to split on both \ and /. No clue how it works though.
  (last (s/split partial-link #"/+\\*")))

(defn linkify-text [text]
  (-> text
      (s/replace " " "_")))

(defn construct-page-link [link-text]
  (str base-page-link link-text))

(defn simple-construct-page-link [text]
  (construct-page-link
    (linkify-text text)))

(defn main-body-links [parsed-html]
  (h/select parsed-html [:#mw-content-text :p :> :a]))

(defn remove-internal-links [links]
  (->> links
    (remove #(-> % :attrs :href (first) (= \#)))))

(defn filtered-links [parsed-html]
  (-> (main-body-links parsed-html)
    (remove-internal-links)))

(defn get-first-link-partial
  "Takes a page link and returns the first partial link in the main body.
  Returns nil if the body doesn't contain any links."
  [link]
  (let [page (parse-page link)
        first-link? (first (filtered-links page))]
    (if first-link?
      (first (h/attr-values first-link? :href))
      nil)))

(defn simple-get-first-link [text]
  (-> (simple-construct-page-link text)
    (get-first-link-partial)
    (first)
    (extract-link-text)
    (construct-page-link)))

(defn try-get-next-page-name [link]
  (try
    (let [next-link-partial (get-first-link-partial link)]
      (extract-link-text next-link-partial))

    (catch FileNotFoundException e
      nil)))

(defn link-jump [seed-term max-jumps]
  (loop [i 0
         current-link (simple-construct-page-link seed-term)
         visited #{(linkify-text seed-term)}
         acc []]
    (let [next-page-name? (try-get-next-page-name current-link)
          acc' (conj acc next-page-name?)]

      (cond
        (not next-page-name?)
        (conj acc :bad-page)

        (or (> i max-jumps) (visited next-page-name?))
        acc'

        :else
        (recur (inc i)
               (construct-page-link next-page-name?)
               (conj visited next-page-name?)
               acc')))))

(def test-page
  (delay
    (parse-page (simple-construct-page-link "Soft Drink"))))


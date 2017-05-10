(ns wikipedia-game.wiki-parser
  (:require [net.cgrand.enlive-html :as h]
            [clojure.string :as s])
  (:import [java.net URL]))

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

(defn get-first-link-partial
  "Takes a page link and returns the first partial link in the main body.
  Returns nil if the body doesn't contain any links."
  [link]
  (let [page (parse-page link)
        first-link? (first (h/select page [:#mw-content-text :p :> :a]))]
    (if first-link?
      (first (h/attr-values first-link? :href))
      nil)))

(defn simple-get-first-link [text]
  (-> (simple-construct-page-link text)
    (get-first-link-partial)
    (first)
    (extract-link-text)
    (construct-page-link)))

(defn link-jump [seed-term max-jumps]
  (loop [i 0
         current-link (simple-construct-page-link seed-term)
         visited #{(linkify-text seed-term)}
         acc []]

    (let [next-link-partial (get-first-link-partial current-link)
          next-page-name (extract-link-text next-link-partial)
          next-link (construct-page-link next-page-name)
          acc' (conj acc next-page-name)]

      (if (or (> i max-jumps) (visited next-page-name))
        acc'

        (recur (inc i)
               next-link
               (conj visited next-page-name)
               acc')))))

#_
(def test-page
  (parse-page (simple-construct-page-link "Tiger Woods")))


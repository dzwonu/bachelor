(ns bachelor.zip
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clj-http.client :as client])
  (:import java.io.File)
  (:import java.io.FileNotFoundException)
  )

(import 'java.util.zip.ZipInputStream)

(defn downloadZip
  "Downloads zip file and saves it to drive"
  [url]
  (with-open [w (io/output-stream "resources/mstcgl.zip")]
             (.write w (:body (client/get url {:as :byte-array}))))
  )

(defn zipEntries
  "Gets seq of zip entries"
  [zip]
  (enumeration-seq (.entries zip))
  )

(defn getZipEntryContent
  "Reads zip entry content"
  [zip entry]
  (slurp (.getInputStream zip entry))
  )

(defn writeZipEntry
  "Writes zip entry to file"
  [zip entry]
  (with-open [w (io/writer (str "resources/notowania/" (.getName entry)))]
             (.write w (getZipEntryContent zip entry)))
  )
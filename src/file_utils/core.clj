(ns file-utils.core
  (:import java.io.File
           (java.security NoSuchAlgorithmException MessageDigest))
  (:use [clojure.java.io])
  (:require [clj-time.coerce :as dt]))
  
(def file-sep (System/getProperty "file.separator"))

(defn md5sum 
  "Compute the MD5 sum of the file."
  [file]
  (let [dig (doto (MessageDigest/getInstance "MD5")
              (.reset)
              (.update (.getBytes (slurp file))))]
    (try
      (.toString (new BigInteger 1 (.digest dig)) 16)
      (catch NoSuchAlgorithmException e
        (throw (new RuntimeException e))))))

(defn exists?
  "Determines if a file at the given path exists.
   Path can be a File, String, URL or URI."
  [path]
  (let [f (as-file path)]
    (.exists f)))

(defn dir?
  "Returns true if the path is a directory."
  [path]
  (.isDirectory (as-file path)))

(defn file?
  "Returns true if the path is a file."
  [path]
  (.isFile (as-file path)))

(defn same-file?
  "Determines if f1 and f2 are the same file by
   comparing their MD5 hashes."
  [f1 f2]
  (= (md5sum f1) (md5sum f2)))

(defn file-size
  "Returns the size of the file in bytes."
  [file]
  (.length (as-file file)))

(defn date-modified
  "Returns a DateTime object representing the date and time the file was
   last modified."
  [path]
  (dt/from-long (.lastModified (as-file path))))

(defn executable?
  "Tests whether the path is executable."
  [path]
  (.canExecute (as-file path)))

(defn readable?
  "Tests whether the path is readable."
  [path]
  (.canRead (as-file path)))

(defn writable?
  "Tests whether the path is writable."
  [path]
  (.canWrite (as-file path)))

(defn mkdir
  "Creates the directories specified by path. This will
   create any parent directories as required."
  [path]
  (.mkdirs (as-file path)))

(defn rm
  "Deletes the file or directory."
  [path]
  (.delete (as-file path)))

(defn create-temp
  "Creates a temporary file."
  []
  (java.io.File/createTempFile "clj", "tmp"))

(defn path
  [file]
  (.getPath file))

(defn name
  "Returns the name of the file or directory of the path."
  [path]
  (.getName (as-file path)))

(defn extension
  "Returns the file extension."
  [path]
  (let [filename (name path)
        idx (.lastIndexOf (.getName (as-file path)) ".")]
    (if (> idx 0)
      (.substring filename (+ idx 1))  
      nil)))


(defn- make-filter-fn [pattern]
  (fn [f]
    (re-matches pattern (name f))))

(defn find-file
  "Returns a lazy sequence of all files that match the 
  file-pattern, if given. If recursizve is true, this 
  will search the subdirectories as well."
  [start-path & {:keys [file-pattern recursive]}]
  (let [filter-fn (make-filter-fn file-pattern)]
    (if recursive
      (filter filter-fn (file-seq (as-file start-path)))  
      (filter filter-fn (.listFiles (as-file start-path))))))


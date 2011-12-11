(ns file-utils.core
  (:import java.io.File
           (java.security NoSuchAlgorithmException MessageDigest))
  (:use [clojure.java.io])
  (:require [clj-time.coerce :as dt]
            [clojure.walk :as walk]))
  
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

(defn list-files
  [dir]
  (seq (.listFiles (as-file dir))))

(defn children?
  "Returns true if dir has children."
  [dir]
  (or (> (count (list-files dir)) 0)))

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

(defn create-temp
  "Creates a temporary file."
  []
  (java.io.File/createTempFile "clj", "tmp"))

(defn path
  [file]
  (.getPath file))

(defn filename
  "Returns the name of the file or directory of the path."
  [path]
  (.getName (as-file path)))

(defn extension
  "Returns the file extension."
  [path]
  (let [filename (filename path)
        idx (.lastIndexOf (.getName (as-file path)) ".")]
    (if (> idx 0)
      (.substring filename (+ idx 1))  
      nil)))

(defn make-filter-fn [pattern]
  (if (= nil pattern)
    (fn [_] true)
    (condp instance? pattern
      String (fn [f] (.equalsIgnoreCase pattern (filename f)))
      java.util.regex.Pattern  (fn [f] (re-matches pattern (filename f))))))

(defn find-file
  "Returns a lazy sequence of all files that match the 
  file-pattern, if given. If recursive is true, this 
  will search the subdirectories as well.
  
  file-pattern may be a regular expression or a string.
  When it is a string, it will only match exact file
  names ignoring case.  "
  [start-path & {:keys [file-pattern recursive]}]
  (let [filter-fn (make-filter-fn file-pattern)]
    (if recursive
      (filter filter-fn (file-seq (as-file start-path)))  
      (filter filter-fn (.listFiles (as-file start-path))))))

(defn rm
  "Deletes the file or directory."
  [path & recursively]
  (if (or recursively false)
    (walk/prewalk #(println (str "rm " %)) (file-seq (as-file path)))
    (.delete (as-file path))))

(defn rm-r
  "Recursively removes a directory and all of its contents."
  [path]
  (let [f (as-file path)]
    (if (dir? f)
      (doseq [child (.listFiles f)]
        (rm-r child))
      (rm f))))


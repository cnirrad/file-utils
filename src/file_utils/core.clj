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
  "Returns the path of a file."
  [^File file]
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

(defn base-filename
  "Returns the name of the file without the extension."
  [path]
  (let [fname (filename path)
        ext (extension path)]
    (.substring fname 0 (- (count fname) (count ext) (if ext 1 0)))))

(defn- make-filter-fn [pattern]
  (if (= nil pattern)
    (fn [_] true)
    (condp instance? pattern
      String (fn [f] (.equalsIgnoreCase pattern (filename f)))
      java.util.regex.Pattern  (fn [f] (re-matches pattern (filename f)))
      clojure.lang.IFn pattern)))

(defn find-file
  "Returns a lazy sequence of all files that match the 
  file-pattern, if given. If recursive is true, this 
  will search the subdirectories as well.
  
  file-filter may be a regular expression, a string or
  a funciton that takes a file as a parameter and returns 
  true if it is a match or a string.  When it is a string, 
  it will only match exact file names ignoring case.  "
  [start-path & {:keys [file-filter recursive]}]
  (let [filter-fn (make-filter-fn file-filter)]
    (if recursive
      (filter filter-fn (file-seq (as-file start-path)))  
      (filter filter-fn (.listFiles (as-file start-path))))))

(defn rm
  "Deletes the file or directory."
  [path]
  (.delete (as-file path)))

(defn rm-r
  "Recursively removes a directory and all of its contents."
  [path]
  (let [f (as-file path)]
    (when (dir? f)
      (doseq [child (list-files f)]
                (rm-r child))
      (rm f))))

(defn copy-file 
  "Copies input to output. Both args must be a String that
  represents the paths. 

  For other arg types, see clojure.java.io/copy"
  [#^String input #^String output]
  (copy (as-file input) (as-file output)))

(defn move-file
  "Copies the source to the destination and then removes 
  the source."
  [source destination]
  (do
    (copy (as-file source) (as-file destination))  
    (rm source)))

(defn #^File file-str
  "Concatenates args as strings and returns a java.io.File.  Replaces
  all / and \\ with File/separatorChar.  Replaces ~ at the start of
  the path with the user.home system property.
  
  This is modified from clojure.contrib.duck-streams."
  [& args]
  (let [#^String s (apply str (interpose File/separator args))
        s (.replaceAll (re-matcher #"(//)|(\\)" s) File/separator)
        s (if (.startsWith s "~")
            (str (System/getProperty "user.home")
                 File/separator (subs s 1))
            s)]
    (File. s)))

(defn copy-dir
  "Copies the source directory and its contents to the destination"
  [source destination] 
  (let [f (as-file source)]
    (if (dir? f)
      (do
        (mkdir destination)
        (doseq [child (list-files f)]
           (copy-dir child (file-str destination (filename child)))))
      (copy-file f destination))))

(defn compile-glob 
  "Compiles a Pattern from a glob string."
	[pattern is-dos]
		(loop [ptrn (seq pattern)
			   regex ""
			   grp false]
		    (let [c (first ptrn)]
				(cond
					(= c \\) (recur (next ptrn) (str regex "\\") grp)
					(= c \/) (recur (next ptrn) (if is-dos (str regex "\\\\") (str regex "/")) grp)
					(= c \?) (recur (next ptrn) (if is-dos (str regex "[^\\\\]") (str regex "[^/]")) grp)
					(= c \*) (if (= (first (next ptrn)) \*) 
								(recur (nnext ptrn) (str regex ".*") grp) ; double astrick spans mutliple directories
								(recur (next ptrn) (if is-dos (str regex "[^\\\\]*") (str regex "[^/]*")) grp)) ; single astrick confined to single directory
					(nil? c) (re-pattern (str regex "$"))
					:else (recur (next ptrn) (str regex c) grp)))))

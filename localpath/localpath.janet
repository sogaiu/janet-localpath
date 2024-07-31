# based on code from spork/path by bakpakin

# a subset of spork/path functionality provided for local file paths
# only

# typically the functions here have a signature such that if the
# (optional) last argument is true, a function's behavior is meant to
# be appropriate for windows.  if the last argument is false, the
# behavior is supposed to be more posix-like.

# https://en.wikipedia.org/wiki/Path_(computing)

# https://en.wikipedia.org/wiki/Canonicalization

(defn abspath?
  [path &opt doze?]
  (default doze? (= :windows (os/which)))
  (if doze?
    # https://stackoverflow.com/a/23968430
    # https://learn.microsoft.com/en-us/dotnet/standard/io/file-path-formats
    (truthy? (peg/match ~(sequence :a `:\`) path))
    (string/has-prefix? "/" path)))

(comment

  (abspath? "/" false)
  # =>
  true

  (abspath? "." false)
  # =>
  false

  (abspath? ".." false)
  # =>
  false

  (abspath? `C:\` true)
  # =>
  true

  (abspath? `C:` true)
  # =>
  false

  (abspath? "config.sys" true)
  # =>
  false

  )

########################################################################

(def w32-grammar
  ~{:main (sequence (opt (sequence (replace (capture :lead)
                                            ,(fn [& xs]
                                               [:lead (get xs 0)]))
                                   (any (set `\/`))))
                    (opt (capture :span))
                    (any (sequence :sep (capture :span)))
                    (opt (sequence :sep (constant ""))))
    :lead (sequence (opt (sequence :a `:`)) `\`)
    :span (some (if-not (set `\/`) 1))
    :sep (some (set `\/`))})

(comment

  (peg/match w32-grammar `C:\WINDOWS\config.sys`)
  # =>
  @[[:lead `C:\`] "WINDOWS" "config.sys"]

  # absolute file path from root of drive C:
  (peg/match w32-grammar `C:\Documents\Newsletters\Summer2018.pdf`)
  # =>
  @[[:lead `C:\`] "Documents" "Newsletters" "Summer2018.pdf"]

  # relative path from root of current drive
  (peg/match w32-grammar `\Program Files\Custom Utilities\StringFinder.exe`)
  # =>
  @[[:lead `\`] "Program Files" "Custom Utilities" "StringFinder.exe"]

  # relative path to a file in a subdirectory of current directory
  (peg/match w32-grammar `2018\January.xlsx`)
  # =>
  @["2018" "January.xlsx"]

  # relative path to a file in a directory starting from current directory
  (peg/match w32-grammar `..\Publications\TravelBrochure.pdf`)
  # =>
  @[".." "Publications" "TravelBrochure.pdf"]

  # absolute path to a file from root of drive C:
  (peg/match w32-grammar `C:\Projects\apilibrary\apilibrary.sln`)
  # =>
  @[[:lead `C:\`] "Projects" "apilibrary" "apilibrary.sln"]

  # XXX
  # relative path from current directory of drive C:
  (peg/match w32-grammar `C:Projects\apilibrary\apilibrary.sln`)
  # =>
  @["C:Projects" "apilibrary" "apilibrary.sln"]

  (peg/match w32-grammar "autoexec.bat")
  # =>
  @["autoexec.bat"]

  (peg/match w32-grammar `C:\`)
  # =>
  @[[:lead `C:\`]]

  # XXX
  (peg/match w32-grammar `C:`)
  # =>
  @["C:"]

  )

(def posix-grammar
  ~{:main (sequence (opt (sequence (replace (capture :lead)
                                            ,(fn [& xs]
                                               [:lead (get xs 0)]))
                                   (any "/")))
                    (opt (capture :span))
                    (any (sequence :sep (capture :span)))
                    (opt (sequence :sep (constant ""))))
    :lead "/"
    :span (some (if-not "/" 1))
    :sep (some "/")})

(comment

  (peg/match posix-grammar "/home/alice/.bashrc")
  # =>
  @[[:lead "/"] "home" "alice" ".bashrc"]

  (peg/match posix-grammar ".profile")
  # =>
  @[".profile"]

  (peg/match posix-grammar "/tmp/../usr/local/../bin")
  # =>
  @[[:lead "/"] "tmp" ".." "usr" "local" ".." "bin"]

  (peg/match posix-grammar "/")
  # =>
  @[[:lead "/"]]

  )

(defn normalize
  [path &opt doze?]
  (default doze? (= :windows (os/which)))
  (def accum @[])
  (def parts
    (peg/match (if doze?
                 w32-grammar
                 posix-grammar)
               path))
  (var seen 0)
  (var lead nil)
  (each x parts
    (match x
      [:lead what] (set lead what)
      #
      "." nil
      #
      ".."
      (if (zero? seen)
        (array/push accum x)
        (do
          (-- seen)
          (array/pop accum)))
      #
      (do
        (++ seen)
        (array/push accum x))))
  (def ret
    (string (or lead "")
            (string/join accum (if doze? `\` "/"))))
  #
  (if (empty? ret)
    "."
    ret))

(comment

  (normalize `C:\WINDOWS\config.sys` true)
  # =>
  `C:\WINDOWS\config.sys`

  (normalize `C:\Documents\Newsletters\Summer2018.pdf` true)
  # =>
  `C:\Documents\Newsletters\Summer2018.pdf`

  (normalize `\Program Files\Custom Utilities\StringFinder.exe` true)
  # =>
  `\Program Files\Custom Utilities\StringFinder.exe`

  (normalize `2018\January.xlsx` true)
  # =>
  `2018\January.xlsx`

  # XXX: not enough info to eliminate ..
  (normalize `..\Publications\TravelBrochure.pdf` true)
  # =>
  `..\Publications\TravelBrochure.pdf`

  (normalize `C:\Projects\apilibrary\apilibrary.sln` true)
  # =>
  `C:\Projects\apilibrary\apilibrary.sln`

  (normalize `C:Projects\apilibrary\apilibrary.sln` true)
  # =>
  `C:Projects\apilibrary\apilibrary.sln`

  (normalize "autoexec.bat" true)
  # =>
  "autoexec.bat"

  (normalize `C:\` true)
  # =>
  `C:\`

  (normalize `C:` true)
  # =>
  "C:"

  (normalize `C:\WINDOWS\SYSTEM32\..` true)
  # =>
  `C:\WINDOWS`

  (normalize `C:\WINDOWS\SYSTEM32\..\SYSTEM32` true)
  # =>
  `C:\WINDOWS\SYSTEM32`

  )

########################################################################

# for some windows paths (e.g. C:here - no backslash; \hello - leading
# backslash), some "techniques" can help when calling.  see examples
# in comment form below
(defn join
  [& els]
  (def end (last els))
  (when (and (one? (length els))
             (not (string? end)))
    (error "when els only has a single element, it must be a string"))
  #
  (def [items sep]
    (cond
      (true? end)
      [(slice els 0 -2) `\`]
      #
      (false? end)
      [(slice els 0 -2) "/"]
      #
      [els (if (= :windows (os/which)) `\` "/")]))
  #
  (normalize (string/join items sep)))

(comment

  (join `C:` "WINDOWS" "config.sys" true)
  # =>
  `C:\WINDOWS\config.sys`

  (join `C:` "Documents" "Newsletters" "Summer2018.pdf" true)
  # =>
  `C:\Documents\Newsletters\Summer2018.pdf`

  (join "" "Program Files" "Custom Utilities" "StringFinder.exe" true)
  # =>
  `\Program Files\Custom Utilities\StringFinder.exe`

  (join "2018" "January.xlsx" true)
  # =>
  `2018\January.xlsx`

  (join ".." "Publications" "TravelBrochure.pdf" true)
  # =>
  `..\Publications\TravelBrochure.pdf`

  (join `C:` "Projects" "apilibrary" "apilibrary.sln" true)
  # =>
  `C:\Projects\apilibrary\apilibrary.sln`

  (join "autoexec.bat" true)
  # =>
  "autoexec.bat"

  (join `C:` true)
  # =>
  "C:"

  # below here are some possibly non-obvious "techniques"

  (join `C:Projects` `apilibrary` `apilibrary.sln` true)
  # =>
  `C:Projects\apilibrary\apilibrary.sln`

  (join `C:` "" true)
  # =>
  `C:\`

  (join "" "tmp" false)
  # =>
  "/tmp"

  )

########################################################################

(defn abspath
  [path &opt doze?]
  (default doze? (= :windows (os/which)))
  (if (abspath? path doze?)
    (normalize path doze?)
    # dynamic variable useful for testing
    (join (or (dyn :localpath-cwd) (os/cwd))
          path
          doze?)))

(comment

  (with-dyns [:localpath-cwd "/root"]
    (abspath "." false))
  # =>
  "/root"

  (with-dyns [:localpath-cwd `C:\WINDOWS`]
    (abspath "config.sys" true))
  # =>
  `C:\WINDOWS\config.sys`

  )

########################################################################

(defn parts
  [path &opt doze?]
  (default doze? (= :windows (os/which)))
  (string/split (if doze? `\` "/")
                path))

(comment

  (parts `C:\WINDOWS\config.sys` true)
  # =>
  @["C:" "WINDOWS" "config.sys"]

  (parts `/tmp` false)
  # =>
  @["" "tmp"]

  (parts `/home/jiro/.profile` false)
  # =>
  @["" "home" "jiro" ".profile"]

  (parts `local/share/man` false)
  # =>
  @["local" "share" "man"]

  # might find this counter-intuitive, but seems to make things work...
  (parts `C:Projects\apilibrary\apilibrary.sln` true)
  # =>
  @["C:Projects" "apilibrary" "apilibrary.sln"]

  )

########################################################################

(defn relpath
  [source target &opt doze?]
  (default doze? (= :windows (os/which)))
  (def source-parts (parts (abspath source doze?) doze?))
  (def target-parts (parts (abspath target doze?) doze?))
  (def same-parts
    (length (take-until identity
                        (map not= source-parts target-parts))))
  (def up-walk
    (array/new-filled (- (length source-parts) same-parts)
                      ".."))
  (def down-walk (tuple/slice target-parts same-parts))
  #
  (join ;up-walk ;down-walk doze?))

(comment

  (relpath "/home/bob/.local/lib/janet"
           "/home/bob/.local/include"
           false)
  # =>
  "../../include"

  (relpath `C:\WINDOWS\SYSTEM32`
           `C:\`
           true)
  # =>
  `..\..\`

  # XXX: why three sets of dots?
  (relpath "/usr/local/lib/janet/"
           "/usr/local/include"
           false)
  # =>
  "../../../include"

  )


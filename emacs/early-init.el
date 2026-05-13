;; Fix native compilation on macOS with Homebrew GCC/libgccjit.
;; When Emacs.app launches, it doesn't inherit shell PATH/LIBRARY_PATH,
;; so libgccjit can't find gcc-15 driver or system libs when linking .eln files.
(let ((homebrew-gcc-lib "/opt/homebrew/opt/gcc/lib/gcc/current")
      (homebrew-libgccjit-lib "/opt/homebrew/opt/libgccjit/lib/gcc/current")
      (homebrew-bin "/opt/homebrew/bin"))
  ;; Ensure gcc-15 is findable by libgccjit
  (setenv "PATH" (concat homebrew-bin ":" (or (getenv "PATH") "")))
  (push homebrew-bin exec-path)
  ;; Ensure GCC can find its own runtime libs and system libs when linking
  (setenv "LIBRARY_PATH"
          (mapconcat #'identity
                     (list homebrew-gcc-lib
                           homebrew-libgccjit-lib
                           "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"
                           (or (getenv "LIBRARY_PATH") ""))
                     ":")))

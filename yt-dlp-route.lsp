
#!/usr/bin/env sbcl --script

;;; yt-dlp-route in Common Lisp
;;; Dependency: yt-dlp

(defun program-exists-p (program)
  "Check if a program exists in PATH"
  (handler-case
      (progn
        #+sbcl (sb-ext:run-program "which" (list program) :output nil :error nil)
        #+ccl (ccl:run-program "which" (list program) :output nil :error nil)
        #+clisp (ext:run-program "which" :arguments (list program) :output nil :error nil)
        t)
    (error () nil)))

(defun check-yt-dlp ()
  "Check if yt-dlp is installed"
  (unless (program-exists-p "yt-dlp")
    (format t "Error: yt-dlp is not installed.~%")
    (format t "Please install it using one of the following methods:~%")
    (format t "- pip install yt-dlp~%")
    (format t "- brew install yt-dlp (on macOS)~%")
    (format t "- Download from: https://github.com/yt-dlp/yt-dlp~%")
    (quit)))

(defun read-input (prompt)
  "Read user input with a prompt"
  (format t "~A" prompt)
  (force-output)
  (string-trim '(#\Space #\Tab #\Newline) (read-line)))

(defun validate-youtube-url (url)
  "Basic validation for YouTube URLs"
  (or (search "youtube.com" url)
      (search "youtu.be" url)))

(defun ensure-directory-exists (path)
  "Create directory if it doesn't exist"
  (ensure-directories-exist 
   (if (char= (char path (1- (length path))) #\/)
       path
       (concatenate 'string path "/"))))

(defun run-command (program args &key (output t) (error t))
  "Run external command with arguments"
  #+sbcl (sb-ext:run-program program args :output output :error error :wait t)
  #+ccl (ccl:run-program program args :output output :error error :wait t)
  #+clisp (ext:run-program program :arguments args :output output :error error :wait t)
  #-(or sbcl ccl clisp) (error "Unsupported Lisp implementation"))

(defun build-yt-dlp-args (url download-dir quality-choice)
  "Build arguments for yt-dlp command"
  (let ((base-args (list "-o" 
                         (concatenate 'string download-dir "/%(title)s.%(ext)s"))))
    (case quality-choice
      (2 (append base-args (list "-f" "best[height<=720]") (list url)))
      (3 (append base-args (list "-f" "best[height<=480]") (list url)))
      (4 (append base-args (list "-x" "--audio-format" "mp3") (list url)))
      (t (append base-args (list "-f" "best") (list url))))))

(defun get-quality-choice ()
  "Get user's quality preference"
  (format t "~%Select quality:~%")
  (format t "1. Best quality (default)~%")
  (format t "2. 720p~%")
  (format t "3. 480p~%")
  (format t "4. Audio only (mp3)~%")
  (let ((choice-str (read-input "Choice (1-4): ")))
    (if (string= choice-str "")
        1
        (parse-integer choice-str :junk-allowed t))))

(defun download-video (url download-dir quality-choice)
  "Download video using yt-dlp"
  (let ((args (build-yt-dlp-args url download-dir quality-choice)))
    (format t "~%Downloading...~%")
    (format t "Command: yt-dlp ~{~A ~}~%" args)
    (format t "~%")
    (handler-case
        (let ((result (run-command "yt-dlp" args)))
          (if (and result (zerop (process-exit-code result)))
              (format t "~%Download completed!~%")
              (progn
                (format t "~%Error downloading video~%")
                (print-troubleshooting-tips))))
      (error (e)
        (format t "Error: ~A~%" e)
        (print-troubleshooting-tips)))))

(defun process-exit-code (process)
  "Get exit code from process (implementation specific)"
  #+sbcl (sb-ext:process-exit-code process)
  #+ccl (nth-value 1 (ccl:external-process-status process))
  #+clisp 0  ; clisp doesn't easily provide exit codes
  #-(or sbcl ccl clisp) 0)

(defun print-troubleshooting-tips ()
  "Print troubleshooting information"
  (format t "~%Troubleshooting tips:~%")
  (format t "- Make sure the URL is correct and accessible~%")
  (format t "- Check your internet connection~%")
  (format t "- Try updating yt-dlp: pip install -U yt-dlp~%"))

(defun get-video-info (url)
  "Get video information without downloading"
  (format t "Getting video info...~%")
  (run-command "yt-dlp" (list "--get-title" "--get-duration" url)))

(defun list-formats (url)
  "List available video formats"
  (format t "Available formats:~%")
  (run-command "yt-dlp" (list "-F" url)))

(defun show-menu ()
  "Show main menu options"
  (format t "~%YouTube Downloader Menu:~%")
  (format t "1. Download video~%")
  (format t "2. Show video info~%")
  (format t "3. List available formats~%")
  (format t "4. Exit~%"))

(defun main-menu ()
  "Main interactive menu"
  (loop
    (show-menu)
    (let ((choice (read-input "Select option (1-4): ")))
      (case (parse-integer choice :junk-allowed t)
        (1 (download-video-interactive))
        (2 (get-video-info-interactive))
        (3 (list-formats-interactive))
        (4 (progn (format t "Goodbye!~%") (return)))
        (t (format t "Invalid choice. Please try again.~%"))))))

(defun download-video-interactive ()
  "Interactive video download"
  (let ((url (read-input "Enter YouTube URL: ")))
    (when (string= url "")
      (format t "Error: No URL provided~%")
      (return-from download-video-interactive))
    
    (unless (validate-youtube-url url)
      (format t "Error: Please provide a valid YouTube URL~%")
      (return-from download-video-interactive))
    
    (let ((download-dir (read-input "Enter download directory (press Enter for current): ")))
      (when (string= download-dir "")
        (setf download-dir "."))
      
      (ensure-directory-exists download-dir)
      (let ((quality-choice (get-quality-choice)))
        (download-video url download-dir quality-choice)))))

(defun get-video-info-interactive ()
  "Interactive video info retrieval"
  (let ((url (read-input "Enter YouTube URL: ")))
    (if (validate-youtube-url url)
        (get-video-info url)
        (format t "Error: Please provide a valid YouTube URL~%"))))

(defun list-formats-interactive ()
  "Interactive format listing"
  (let ((url (read-input "Enter YouTube URL: ")))
    (if (validate-youtube-url url)
        (list-formats url)
        (format t "Error: Please provide a valid YouTube URL~%"))))

(defun main ()
  "Main entry point"
  (format t "YouTube Video Downloader (Common Lisp)~%")
  (format t "=====================================~%")
  (check-yt-dlp)
  (main-menu))

;; Run main function if script is executed directly
(main)

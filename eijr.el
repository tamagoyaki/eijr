; eijiro on sdic-mode.
(require 'sdic)


(defun eijr-delete-unnecessary-part ()
  (goto-char (point-min))
  (if (search-forward "に該当する項目は見つかりませんでした。" nil t)
      (delete-region (point-min) (re-search-backward "^"))
    (progn
      (delete-region (point-min)
		     (search-forward
		      "・データ提供 : EDP ※データの転載は禁じられています。"))
      (delete-blank-lines)
      (goto-char (point-max))
      (let ((pos (search-backward "＊データの転載は禁じられています。" nil t)))
	(if (not pos)
	    (setq pos (re-search-backward "^● *スポンサーサイト" nil t)))
	(if (not pos)
	    (setq pos (re-search-backward "^.*データ提供:EDP" nil t)))
	(if pos
	    (delete-region pos (point-max))))))
  (delete-blank-lines)
  (goto-char (point-min)))

(defun eijr-search (word)
  (call-process w3m-command nil t nil
		(concat "https://eow.alc.co.jp/search?q="
			(w3m-url-encode-string word 'utf-8))))

(defun eijr-word (word)
  (let ((old-buffer (current-buffer)))
    (set-buffer (get-buffer-create sdic-buffer-name))
    (or (string= mode-name sdic-mode-name) (sdic-mode))
    (setq buffer-read-only nil)
    (erase-buffer)
    (eijr-search word)
    (eijr-delete-unnecessary-part)
    (let* ((swin (selected-window))
	   (nwin (if (one-window-p)
		     (split-window swin
				   (- (window-height swin) sdic-window-height))
		   (get-buffer-window sdic-buffer-name))))
      (set-window-buffer swin old-buffer)
      (set-window-buffer nwin sdic-buffer-name)
      (select-window nwin))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)))







(defvar eijr-instead-of-sdic nil "")
(defvar eijr-current-word "" "");


(defun eijr-forward-item ()
  (interactive)
  (end-of-line)
  (let ((pos (re-search-forward "^ *• " nil t)))
    (if pos (goto-char pos)))
  (beginning-of-line))

(defun eijr-backward-item ()
  (interactive)
  (let ((pos (re-search-backward "^ *• " nil t)))
    (if pos (goto-char pos))))

(defun eijr-toggle ()
  (interactive)
  (if eijr-instead-of-sdic
      (progn
	(setq eijr-instead-of-sdic nil)
	(define-key sdic-mode-map "n" 'sdic-forward-item)
	(define-key sdic-mode-map "p" 'sdic-backward-item)
	(sdic-describe-word eijr-current-word))
    (progn
      (setq eijr-instead-of-sdic t)
      (define-key sdic-mode-map "n" 'eijr-forward-item)
      (define-key sdic-mode-map "p" 'eijr-backward-item)
      (eijr-word eijr-current-word))))

(defun eijr-sdic-describe-word-at-point ()
  (interactive)
  (setq eijr-current-word (sdic-word-at-point))
  (if eijr-instead-of-sdic
      (eijr-word (sdic-word-at-point))
    (sdic-describe-word-at-point)))


(defun eijr-sdic-describe-word ()
  (interactive)
  (let ((w (sdic-read-from-minibuffer)))
    (setq eijr-current-word w)
    (if eijr-instead-of-sdic
	(eijr-word w)
      (sdic-describe-word w))))


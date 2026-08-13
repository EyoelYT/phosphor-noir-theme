;;; phosphor-noir-theme.el -*- lexical-binding: t; -*-

;;; Commentary:
;; A high-contrast, near-black theme with:
;; - muted olive keywords
;; - cool cyan identifiers
;; - violet comments
;; - warm coral constants
;; - subdued olive line numbers

;;; Code:

(require 'color)

(defvar phosphor-noir--reloading nil
  "Non-nil while reloading the Phosphor Noir theme.")

(defun phosphor-noir--set-brightness (symbol value)
  "Set SYMBOL to brightness VALUE and refresh the theme.

VALUE must be between 0.0 and 1.0."
  (unless (and (numberp value)
               (<= 0.0 value)
               (<= value 1.0))
    (user-error
     "Phosphor Noir brightness must be between 0.0 and 1.0"))

  (set-default symbol (float value))

  ;; Rebuild the theme immediately when changed through `setopt' or
  ;; the Customize interface.
  (when (and (memq 'phosphor-noir custom-enabled-themes)
             (not phosphor-noir--reloading))
    (let ((phosphor-noir--reloading t))
      (disable-theme 'phosphor-noir)
      (load-theme 'phosphor-noir t))))

(defcustom phosphor-noir-brightness 0.25
  "Brightness and vibrancy of the Phosphor Noir color palette.

The accepted range is 0.0 through 1.0.

  0.00  Slightly duller than the original palette.
  0.25  The original palette.
  0.50  Moderately brighter.
  0.75  Bright and vivid.
  1.00  Maximum brightness and vibrancy.

The adjustment preserves each color's hue."
  :type 'number
  :group 'faces
  :set #'phosphor-noir--set-brightness)

(defun phosphor-noir--clamp (value)
  "Clamp VALUE to the range 0.0 through 1.0."
  (max 0.0 (min 1.0 value)))

(defun phosphor-noir--rgb-to-hsv (red green blue)
  "Convert RED, GREEN, and BLUE from RGB to HSV.

All components use the range 0.0 through 1.0.  Return a list in
the form (HUE SATURATION VALUE)."
  (let* ((maximum (max red green blue))
         (minimum (min red green blue))
         (delta (- maximum minimum))
         (value maximum)
         (saturation
          (if (zerop maximum)
              0.0
            (/ delta maximum)))
         hue)

    (setq hue
          (cond
           ((zerop delta)
            0.0)

           ((= maximum red)
            (/ (mod (/ (- green blue) delta) 6.0)
               6.0))

           ((= maximum green)
            (/ (+ (/ (- blue red) delta) 2.0)
               6.0))

           (t
            (/ (+ (/ (- red green) delta) 4.0)
               6.0))))

    (list hue saturation value)))

(defun phosphor-noir--hsv-to-rgb (hue saturation value)
  "Convert HUE, SATURATION, and VALUE from HSV to RGB.

All components use the range 0.0 through 1.0.  Return a list in
the form (RED GREEN BLUE)."
  (let* ((hue (mod hue 1.0))
         (chroma (* value saturation))
         (hue-sector (* hue 6.0))
         (secondary
          (* chroma
             (- 1.0
                (abs (- (mod hue-sector 2.0)
                        1.0)))))
         (offset (- value chroma))
         (sector (mod (floor hue-sector) 6))
         rgb)

    (setq rgb
          (pcase sector
            (0 (list chroma secondary 0.0))
            (1 (list secondary chroma 0.0))
            (2 (list 0.0 chroma secondary))
            (3 (list 0.0 secondary chroma))
            (4 (list secondary 0.0 chroma))
            (_ (list chroma 0.0 secondary))))

    (mapcar (lambda (component)
              (+ component offset))
            rgb)))

(defun phosphor-noir--adjust-color (hex &optional neutral-max-value)
  "Adjust HEX according to `phosphor-noir-brightness'.

The original color is reproduced exactly at brightness 0.25.

Below 0.25, the color becomes subtly duller.

Above 0.25, chromatic colors become more saturated and increase
their HSV value.  This makes them more vivid without washing them
out toward white.

When NEUTRAL-MAX-VALUE is a number, treat HEX as a neutral color:
reduce its saturation toward zero and move its HSV value toward
NEUTRAL-MAX-VALUE.  A value of 1.0 therefore produces pure white."
  (let* ((amount (phosphor-noir--clamp
                  (float phosphor-noir-brightness)))
         (baseline 0.25)
         (rgb (color-name-to-rgb hex))
         adjusted-rgb)

    (if (<= amount baseline)
        ;; Dull side: make only a small HSL adjustment.
        (pcase-let* ((`(,red ,green ,blue) rgb)
                     (`(,hue ,saturation ,lightness)
                      (color-rgb-to-hsl red green blue))
                     (progress (/ amount baseline))
                     (new-saturation
                      (* saturation
                         (+ 0.88 (* 0.12 progress))))
                     (new-lightness
                      (* lightness
                         (+ 0.93 (* 0.07 progress)))))
          (setq adjusted-rgb
                (color-hsl-to-rgb
                 hue
                 (phosphor-noir--clamp new-saturation)
                 (phosphor-noir--clamp new-lightness))))

      ;; Bright side: use HSV so colors become vivid rather than pale.
      (pcase-let* ((`(,red ,green ,blue) rgb)
                   (`(,hue ,saturation ,value)
                    (phosphor-noir--rgb-to-hsv red green blue))
                   (progress
                    (/ (- amount baseline)
                       (- 1.0 baseline))))

        (if neutral-max-value
            ;; Neutral colors move toward gray or white.
            (let ((new-saturation
                   (* saturation (- 1.0 progress)))
                  (new-value
                   (+ value
                      (* (- neutral-max-value value)
                         progress))))
              (setq adjusted-rgb
                    (phosphor-noir--hsv-to-rgb
                     hue
                     (phosphor-noir--clamp new-saturation)
                     (phosphor-noir--clamp new-value))))

          ;; Chromatic colors move toward maximum saturation and
          ;; maximum HSV value while retaining their original hue.
          (let ((new-saturation
                 (+ saturation
                    (* (- 1.0 saturation)
                       progress)))
                (new-value
                 (+ value
                    (* (- 1.0 value)
                       progress))))
            (setq adjusted-rgb
                  (phosphor-noir--hsv-to-rgb
                   hue
                   (phosphor-noir--clamp new-saturation)
                   (phosphor-noir--clamp new-value)))))))

    (pcase-let ((`(,red ,green ,blue) adjusted-rgb))
      (color-rgb-to-hex red green blue 2))))

(deftheme phosphor-noir
  "A near-black programming theme inspired by a terminal editor palette.")

(let ((bg          "#050505")
      (bg-alt      "#101010")
      (bg-active   "#181818")

      ;; Main text reaches exactly #ffffff.
      (fg          (phosphor-noir--adjust-color "#d8d6d0" 1.0))
      (variable    (phosphor-noir--adjust-color "#d8d6d0" 1.0))

      ;; Muted foreground becomes a brighter neutral gray, but does
      ;; not become as bright as the main foreground.
      (fg-muted    (phosphor-noir--adjust-color "#96958d" 0.78))

      ;; Chromatic colors become saturated and vivid.
      (line-number (phosphor-noir--adjust-color "#747342"))
      (comment     (phosphor-noir--adjust-color "#8864ad"))
      (comment-doc (phosphor-noir--adjust-color "#9a78bc"))
      (keyword     (phosphor-noir--adjust-color "#a7aa62"))
      (function    (phosphor-noir--adjust-color "#70abc1"))
      (type        (phosphor-noir--adjust-color "#79aaa5"))
      (builtin     (phosphor-noir--adjust-color "#b5b773"))
      (string      (phosphor-noir--adjust-color "#b8b87b"))
      (number      (phosphor-noir--adjust-color "#cf765f"))
      (warning     (phosphor-noir--adjust-color "#d6a55f"))
      (error       (phosphor-noir--adjust-color "#d66565"))
      (success     (phosphor-noir--adjust-color "#7fa66a"))

      ;; Structural backgrounds remain fixed.
      (region      "#29323a")
      (selection   "#343d45")
      (border      "#303030"))

  (custom-theme-set-faces
   'phosphor-noir

   ;; Core UI
   `(default
     ((t (:background ,bg
          :foreground ,fg))))

   `(cursor
     ((t (:background ,fg))))

   `(fringe
     ((t (:background ,bg
          :foreground ,fg-muted))))

   `(vertical-border
     ((t (:foreground ,border))))

   `(window-divider
     ((t (:foreground ,border))))

   `(window-divider-first-pixel
     ((t (:foreground ,border))))

   `(window-divider-last-pixel
     ((t (:foreground ,border))))

   `(region
     ((t (:background ,region
          :extend t))))

   `(secondary-selection
     ((t (:background ,selection
          :extend t))))

   `(highlight
     ((t (:background ,bg-active))))

   `(hl-line
     ((t (:background ,bg-alt
          :extend t))))

   `(shadow
     ((t (:foreground ,fg-muted))))

   `(link
     ((t (:foreground ,function
          :underline t))))

   `(link-visited
     ((t (:foreground ,comment-doc
          :underline t))))

   `(escape-glyph
     ((t (:foreground ,number))))

   `(homoglyph
     ((t (:foreground ,warning))))

   ;; Line numbers
   `(line-number
     ((t (:background ,bg
          :foreground ,line-number))))

   `(line-number-current-line
     ((t (:background ,bg
          :foreground ,keyword
          :weight bold))))

   ;; Font Lock
   `(font-lock-comment-face
     ((t (:foreground ,comment
          :slant normal))))

   `(font-lock-comment-delimiter-face
     ((t (:foreground ,comment))))

   `(font-lock-doc-face
     ((t (:foreground ,comment-doc
          :slant normal))))

   `(font-lock-doc-markup-face
     ((t (:foreground ,comment-doc
          :weight bold))))

   `(font-lock-keyword-face
     ((t (:foreground ,keyword
          :weight semi-bold))))

   `(font-lock-builtin-face
     ((t (:foreground ,builtin))))

   `(font-lock-function-name-face
     ((t (:foreground ,function
          :weight semi-bold))))

   `(font-lock-function-call-face
     ((t (:foreground ,fg))))

   `(font-lock-variable-name-face
     ((t (:foreground ,variable))))

   `(font-lock-variable-use-face
     ((t (:foreground ,variable))))

   `(font-lock-type-face
     ((t (:foreground ,type))))

   `(font-lock-constant-face
     ((t (:foreground ,number))))

   `(font-lock-number-face
     ((t (:foreground ,number))))

   `(font-lock-string-face
     ((t (:foreground ,string))))

   `(font-lock-regexp-grouping-backslash
     ((t (:foreground ,number
          :weight bold))))

   `(font-lock-regexp-grouping-construct
     ((t (:foreground ,keyword
          :weight bold))))

   `(font-lock-preprocessor-face
     ((t (:foreground ,builtin))))

   `(font-lock-negation-char-face
     ((t (:foreground ,number
          :weight bold))))

   `(font-lock-warning-face
     ((t (:foreground ,warning
          :weight bold))))

   ;; Parentheses and delimiters
   `(show-paren-match
     ((t (:background ,selection
          :foreground ,function
          :weight bold))))

   `(show-paren-mismatch
     ((t (:background ,error
          :foreground ,bg
          :weight bold))))

   `(match
     ((t (:background ,selection
          :foreground ,function
          :weight bold))))

   ;; Search
   `(isearch
     ((t (:background ,keyword
          :foreground ,bg
          :weight bold))))

   `(lazy-highlight
     ((t (:background ,selection
          :foreground ,string))))

   `(isearch-fail
     ((t (:background ,error
          :foreground ,bg
          :weight bold))))

   `(query-replace
     ((t (:inherit isearch))))

   ;; Mode line
   `(mode-line
     ((t (:background ,bg-active
          :foreground ,fg
          :box (:line-width 1 :color ,border)))))

   `(mode-line-inactive
     ((t (:background ,bg-alt
          :foreground ,fg-muted
          :box (:line-width 1 :color ,border)))))

   `(mode-line-buffer-id
     ((t (:foreground ,function
          :weight bold))))

   `(mode-line-emphasis
     ((t (:foreground ,keyword
          :weight bold))))

   `(mode-line-highlight
     ((t (:background ,selection
          :box nil))))

   ;; Minibuffer and completion
   `(minibuffer-prompt
     ((t (:foreground ,function
          :weight bold))))

   `(completions-common-part
     ((t (:foreground ,keyword
          :weight bold))))

   `(completions-first-difference
     ((t (:foreground ,function))))

   `(completions-annotations
     ((t (:foreground ,fg-muted
          :slant italic))))

   ;; Errors and diagnostics
   `(error
     ((t (:foreground ,error
          :weight bold))))

   `(warning
     ((t (:foreground ,warning
          :weight bold))))

   `(success
     ((t (:foreground ,success
          :weight bold))))

   `(flymake-error
     ((t (:underline (:style wave :color ,error)))))

   `(flymake-warning
     ((t (:underline (:style wave :color ,warning)))))

   `(flymake-note
     ((t (:underline (:style wave :color ,function)))))

   `(flycheck-error
     ((t (:underline (:style wave :color ,error)))))

   `(flycheck-warning
     ((t (:underline (:style wave :color ,warning)))))

   `(flycheck-info
     ((t (:underline (:style wave :color ,function)))))

   ;; Diff and version control
   `(diff-added
     ((t (:background "#101a10"
          :foreground ,success))))

   `(diff-removed
     ((t (:background "#1d1010"
          :foreground ,error))))

   `(diff-changed
     ((t (:background "#1b1810"
          :foreground ,warning))))

   `(diff-header
     ((t (:background ,bg-active
          :foreground ,function))))

   `(diff-file-header
     ((t (:background ,bg-active
          :foreground ,function
          :weight bold))))

   `(diff-hunk-header
     ((t (:background ,bg-alt
          :foreground ,comment-doc))))

   ;; Dired
   `(dired-directory
     ((t (:foreground ,function
          :weight bold))))

   `(dired-symlink
     ((t (:foreground ,type))))

   `(dired-marked
     ((t (:foreground ,warning
          :weight bold))))

   `(dired-flagged
     ((t (:foreground ,error
          :weight bold))))

   ;; Org
   `(org-level-1
     ((t (:foreground ,function
          :weight bold))))

   `(org-level-2
     ((t (:foreground ,keyword
          :weight bold))))

   `(org-level-3
     ((t (:foreground ,comment-doc
          :weight bold))))

   `(org-level-4
     ((t (:foreground ,type
          :weight bold))))

   `(org-code
     ((t (:foreground ,string))))

   `(org-verbatim
     ((t (:foreground ,number))))

   `(org-block
     ((t (:background ,bg-alt
          :foreground ,fg
          :extend t))))

   `(org-block-begin-line
     ((t (:background ,bg-alt
          :foreground ,comment
          :extend t))))

   `(org-block-end-line
     ((t (:inherit org-block-begin-line))))

   `(org-todo
     ((t (:foreground ,error
          :weight bold))))

   `(org-done
     ((t (:foreground ,success
          :weight bold))))

   `(org-date
     ((t (:foreground ,function
          :underline t))))

   ;; Markdown
   `(markdown-header-face-1
     ((t (:foreground ,function
          :weight bold
          :height 1.15))))

   `(markdown-header-face-2
     ((t (:foreground ,keyword
          :weight bold
          :height 1.10))))

   `(markdown-header-face-3
     ((t (:foreground ,comment-doc
          :weight bold))))

   `(markdown-code-face
     ((t (:background ,bg-alt
          :foreground ,string))))

   `(markdown-inline-code-face
     ((t (:foreground ,string))))

   `(markdown-link-face
     ((t (:foreground ,function))))

   `(markdown-url-face
     ((t (:foreground ,fg-muted
          :underline t))))

   ;; Company
   `(company-tooltip
     ((t (:background ,bg-active
          :foreground ,fg))))

   `(company-tooltip-selection
     ((t (:background ,selection
          :foreground ,fg))))

   `(company-tooltip-common
     ((t (:foreground ,keyword
          :weight bold))))

   `(company-tooltip-annotation
     ((t (:foreground ,comment-doc))))

   `(company-scrollbar-bg
     ((t (:background ,bg-alt))))

   `(company-scrollbar-fg
     ((t (:background ,fg-muted))))

   ;; Corfu
   `(corfu-default
     ((t (:background ,bg-active
          :foreground ,fg))))

   `(corfu-current
     ((t (:background ,selection
          :foreground ,fg))))

   `(corfu-border
     ((t (:background ,border))))

   ;; ANSI terminal colors
   `(ansi-color-black
     ((t (:foreground ,bg
          :background ,bg))))

   `(ansi-color-red
     ((t (:foreground ,error
          :background ,error))))

   `(ansi-color-green
     ((t (:foreground ,success
          :background ,success))))

   `(ansi-color-yellow
     ((t (:foreground ,keyword
          :background ,keyword))))

   `(ansi-color-blue
     ((t (:foreground ,function
          :background ,function))))

   `(ansi-color-magenta
     ((t (:foreground ,comment-doc
          :background ,comment-doc))))

   `(ansi-color-cyan
     ((t (:foreground ,type
          :background ,type))))

   `(ansi-color-white
     ((t (:foreground ,fg
          :background ,fg))))

   ;; Tree-sitter faces used by newer Emacs versions
   `(font-lock-bracket-face
     ((t (:foreground ,fg-muted))))

   `(font-lock-delimiter-face
     ((t (:foreground ,fg-muted))))

   `(font-lock-operator-face
     ((t (:foreground ,fg))))

   `(font-lock-property-name-face
     ((t (:foreground ,type))))

   `(font-lock-property-use-face
     ((t (:foreground ,type))))

   `(font-lock-punctuation-face
     ((t (:foreground ,fg-muted))))

   `(font-lock-misc-punctuation-face
     ((t (:foreground ,fg-muted))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))

(provide-theme 'phosphor-noir)
;;; phosphor-noir-theme.el ends here

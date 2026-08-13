;;; phosphor-noir-theme.el -*- lexical-binding: t; -*-

;;; Commentary:
;; A high-contrast, near-black theme with:
;; - muted olive keywords
;; - cool cyan identifiers
;; - violet comments
;; - warm coral constants
;; - subdued olive line numbers
;;
;; Customization:
;;
;; `phosphor-noir-brightness'
;;     Controls how far colors move from the original palette.
;;
;; `phosphor-noir-maximum-saturation'
;;     Controls the maximum saturation target for chromatic colors.
;;
;; `phosphor-noir-maximum-lightness'
;;     Controls the maximum HSV value target for chromatic colors.
;;
;; Hue is always preserved so that colors retain their identities.

;;; Code:

(require 'color)

(defgroup phosphor-noir nil
  "Customization options for the Phosphor Noir theme."
  :group 'faces)

(defvar phosphor-noir--reloading nil
  "Non-nil while reloading the Phosphor Noir theme.")

(defun phosphor-noir--set-unit-option (symbol value)
  "Set SYMBOL to VALUE and refresh the theme.

VALUE must be a number between 0.0 and 1.0."
  (unless (and (numberp value)
               (<= 0.0 value)
               (<= value 1.0))
    (user-error "%s must be between 0.0 and 1.0" symbol))

  (set-default symbol (float value))

  ;; Rebuild the theme immediately when an option is changed with
  ;; `setopt' or through the Customize interface.
  (when (and (memq 'phosphor-noir custom-enabled-themes)
             (not phosphor-noir--reloading))
    (let ((phosphor-noir--reloading t))
      (disable-theme 'phosphor-noir)
      (load-theme 'phosphor-noir t))))

(defcustom phosphor-noir-brightness 0.8
  "Overall intensity of the Phosphor Noir palette.

The accepted range is 0.0 through 1.0.

  0.00  Slightly duller than the original palette.
  0.25  The exact original palette.
  0.50  Halfway toward the configured maximums.
  0.75  Bright and vivid.
  1.00  The configured maximum saturation and lightness.

This option controls interpolation strength.  The final targets
are controlled by `phosphor-noir-maximum-saturation' and
`phosphor-noir-maximum-lightness'."
  :type 'number
  :group 'phosphor-noir
  :set #'phosphor-noir--set-unit-option)

(defcustom phosphor-noir-maximum-saturation 0.72
  "Maximum saturation target for chromatic colors.

The accepted range is 0.0 through 1.0.

This target is reached when `phosphor-noir-brightness' is 1.0.
The original color is not reduced in saturation if it is already
more saturated than this value.

Reasonable values include:

  0.55  Muted.
  0.65  Colorful but restrained.
  0.72  Vivid without being excessively neon.
  0.85  Highly saturated.
  1.00  Maximum saturation."
  :type 'number
  :group 'phosphor-noir
  :set #'phosphor-noir--set-unit-option)

(defcustom phosphor-noir-maximum-lightness 0.95
  "Maximum brightness target for chromatic colors.

The accepted range is 0.0 through 1.0.

Internally this is an HSV value rather than HSL lightness.  HSV
value allows colors to become brighter while avoiding the pastel
or white appearance caused by high HSL lightness.

This target is reached when `phosphor-noir-brightness' is 1.0.
The original color is not darkened if it already exceeds this
value.

Reasonable values include:

  0.75  Dull.
  0.85  Restrained.
  0.92  Bright but comfortable.
  0.96  Very bright.
  1.00  Maximum RGB intensity."
  :type 'number
  :group 'phosphor-noir
  :set #'phosphor-noir--set-unit-option)

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

(defun phosphor-noir--blend (foreground background alpha)
  "Blend FOREGROUND into BACKGROUND by ALPHA.

ALPHA must be between 0.0 and 1.0.  A value of 0.0 returns
BACKGROUND, while 1.0 returns FOREGROUND."
  (let ((alpha (phosphor-noir--clamp (float alpha))))
    (pcase-let ((`(,foreground-red
                   ,foreground-green
                   ,foreground-blue)
                 (color-name-to-rgb foreground))
                (`(,background-red
                   ,background-green
                   ,background-blue)
                 (color-name-to-rgb background)))
      (color-rgb-to-hex
       (+ (* foreground-red alpha)
          (* background-red (- 1.0 alpha)))
       (+ (* foreground-green alpha)
          (* background-green (- 1.0 alpha)))
       (+ (* foreground-blue alpha)
          (* background-blue (- 1.0 alpha)))
       2))))

(defun phosphor-noir--adjust-color (hex &optional neutral-max-value)
  "Adjust HEX according to the Phosphor Noir options.

The original color is reproduced exactly when
`phosphor-noir-brightness' is 0.25.

Below 0.25, the color becomes subtly duller.

Above 0.25, chromatic colors move toward
`phosphor-noir-maximum-saturation' and
`phosphor-noir-maximum-lightness'.  Hue is preserved.

When NEUTRAL-MAX-VALUE is a number, treat HEX as a neutral color:
reduce its saturation toward zero and move its HSV value toward
NEUTRAL-MAX-VALUE.  A value of 1.0 produces pure white."
  (let* ((amount
          (phosphor-noir--clamp
           (float phosphor-noir-brightness)))
         (baseline 0.25)
         (rgb (color-name-to-rgb hex))
         adjusted-rgb)

    (if (<= amount baseline)
        ;; Dull side: only a subtle reduction from the original.
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

      ;; Bright side: use HSV so chromatic colors become vivid rather
      ;; than pale.
      (pcase-let* ((`(,red ,green ,blue) rgb)
                   (`(,hue ,saturation ,value)
                    (phosphor-noir--rgb-to-hsv
                     red green blue))
                   (progress
                    (/ (- amount baseline)
                       (- 1.0 baseline))))

        (if neutral-max-value
            ;; Neutral colors move toward a configured gray or white.
            (let ((new-saturation
                   (* saturation
                      (- 1.0 progress)))
                  (new-value
                   (+ value
                      (* (- neutral-max-value value)
                         progress))))
              (setq adjusted-rgb
                    (phosphor-noir--hsv-to-rgb
                     hue
                     (phosphor-noir--clamp new-saturation)
                     (phosphor-noir--clamp new-value))))

          ;; Do not reduce an original color when the configured
          ;; maximum is below the color's starting component.
          (let* ((target-saturation
                  (max saturation
                       phosphor-noir-maximum-saturation))
                 (target-value
                  (max value
                       phosphor-noir-maximum-lightness))
                 (new-saturation
                  (+ saturation
                     (* (- target-saturation saturation)
                        progress)))
                 (new-value
                  (+ value
                     (* (- target-value value)
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

(let* ((bg          "#050505")
       (bg-alt      "#101010")
       (bg-active   "#181818")

       ;; Main foreground reaches exactly #ffffff when brightness is 1.0.
       (fg          (phosphor-noir--adjust-color "#d8d6d0" 1.0))
       (variable    (phosphor-noir--adjust-color "#d8d6d0" 1.0))

       ;; Muted foreground approaches a neutral gray rather than pure white.
       (fg-muted    (phosphor-noir--adjust-color "#96958d" 0.78))

       ;; Chromatic colors.
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

       ;; Structural colors.
       (region      "#29323a")
       (selection   "#343d45")
       (border      "#303030")

       ;; Derived neutral colors.
       (fg-dim (phosphor-noir--blend fg bg 0.58))
       (fg-faint (phosphor-noir--blend fg bg 0.36))

       ;; Subtle diff backgrounds.
       (diff-added-bg (phosphor-noir--blend success bg 0.12))
       (diff-added-bg-highlight (phosphor-noir--blend success bg 0.21))

       (diff-removed-bg (phosphor-noir--blend error bg 0.12))
       (diff-removed-bg-highlight (phosphor-noir--blend error bg 0.21))

       (diff-changed-bg (phosphor-noir--blend function bg 0.12))
       (diff-changed-bg-highlight (phosphor-noir--blend function bg 0.21))

       (diff-base-bg (phosphor-noir--blend warning bg 0.12))
       (diff-base-bg-highlight (phosphor-noir--blend warning bg 0.21))

       (diff-conflict-bg (phosphor-noir--blend warning bg 0.18))

       ;; Magit headings and selection colors.
       (magit-hunk-bg (phosphor-noir--blend comment-doc bg 0.20))
       (magit-hunk-bg-highlight (phosphor-noir--blend comment-doc bg 0.31))
       (magit-selection-bg (phosphor-noir--blend function bg 0.24)))

  (custom-theme-set-faces
   'phosphor-noir

   ;; Core UI
   `(default ((t (:background ,bg :foreground ,fg))))
   `(cursor ((t (:background ,fg))))
   `(fringe ((t (:background ,bg :foreground ,fg-muted))))
   `(vertical-border ((t (:foreground ,border))))
   `(window-divider ((t (:foreground ,border))))
   `(window-divider-first-pixel ((t (:foreground ,border))))
   `(window-divider-last-pixel ((t (:foreground ,border))))
   `(region ((t (:background ,region :extend t))))
   `(secondary-selection ((t (:background ,selection :extend t))))
   `(highlight ((t (:background ,bg-active))))
   `(hl-line ((t (:background ,bg-alt :extend t))))
   `(shadow ((t (:foreground ,fg-muted))))
   `(link ((t (:foreground ,function :underline t))))
   `(link-visited ((t (:foreground ,comment-doc :underline t))))
   `(escape-glyph ((t (:foreground ,number))))
   `(homoglyph ((t (:foreground ,warning))))

   ;; Line numbers
   `(line-number ((t (:background ,bg :foreground ,line-number))))
   `(line-number-current-line ((t (:background ,bg :foreground ,keyword :weight bold))))

   ;; Font Lock
   `(font-lock-comment-face ((t (:foreground ,comment :slant normal))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comment))))
   `(font-lock-doc-face ((t (:foreground ,comment-doc :slant normal))))
   `(font-lock-doc-markup-face ((t (:foreground ,comment-doc :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,keyword :weight semi-bold))))
   `(font-lock-builtin-face ((t (:foreground ,builtin))))
   `(font-lock-function-name-face ((t (:foreground ,function :weight semi-bold))))
   `(font-lock-function-call-face ((t (:foreground ,fg))))
   `(font-lock-variable-name-face ((t (:foreground ,variable))))
   `(font-lock-variable-use-face ((t (:foreground ,variable))))
   `(font-lock-type-face ((t (:foreground ,type))))
   `(font-lock-constant-face ((t (:foreground ,number))))
   `(font-lock-number-face ((t (:foreground ,number))))
   `(font-lock-string-face ((t (:foreground ,string))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,number :weight bold))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,keyword :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,builtin))))
   `(font-lock-negation-char-face ((t (:foreground ,number :weight bold))))
   `(font-lock-warning-face ((t (:foreground ,warning :weight bold))))

   ;; Parentheses and delimiters
   `(show-paren-match ((t (:background ,selection :foreground ,function :weight bold))))
   `(show-paren-mismatch ((t (:background ,error :foreground ,bg :weight bold))))
   `(match ((t (:background ,selection :foreground ,function :weight bold))))

   ;; Search
   `(isearch ((t (:background ,keyword :foreground ,bg :weight bold))))
   `(lazy-highlight ((t (:background ,selection :foreground ,string))))
   `(isearch-fail ((t (:background ,error :foreground ,bg :weight bold))))
   `(query-replace ((t (:inherit isearch))))

   ;; Mode line
   `(mode-line ((t (:background ,bg-active :foreground ,fg :box (:line-width 1 :color ,border)))))
   `(mode-line-inactive ((t (:background ,bg-alt :foreground ,fg-muted :box (:line-width 1 :color ,border)))))
   `(mode-line-buffer-id ((t (:foreground ,function :weight bold))))
   `(mode-line-emphasis ((t (:foreground ,keyword :weight bold))))
   `(mode-line-highlight ((t (:background ,selection :box nil))))

   ;; Minibuffer and completion
   `(minibuffer-prompt ((t (:foreground ,function :weight bold))))
   `(completions-common-part ((t (:foreground ,keyword :weight bold))))
   `(completions-first-difference ((t (:foreground ,function))))
   `(completions-annotations ((t (:foreground ,fg-muted :slant italic))))

   ;; Errors and diagnostics
   `(error ((t (:foreground ,error :weight bold))))
   `(warning ((t (:foreground ,warning :weight bold))))
   `(success ((t (:foreground ,success :weight bold))))
   `(flymake-error ((t (:underline (:style wave :color ,error)))))
   `(flymake-warning ((t (:underline (:style wave :color ,warning)))))
   `(flymake-note ((t (:underline (:style wave :color ,function)))))
   `(flycheck-error ((t (:underline (:style wave :color ,error)))))
   `(flycheck-warning ((t (:underline (:style wave :color ,warning)))))
   `(flycheck-info ((t (:underline (:style wave :color ,function)))))

   ;; Diff mode
   `(diff-context ((t (:foreground ,fg-muted :background ,bg :extend t))))
   `(diff-added ((t (:foreground ,success :background ,diff-added-bg :extend t))))
   `(diff-removed ((t (:foreground ,error :background ,diff-removed-bg :extend t))))
   `(diff-changed ((t (:foreground ,function :background ,diff-changed-bg :extend t))))
   `(diff-header ((t (:foreground ,function :background ,bg-active :extend t))))
   `(diff-file-header ((t (:foreground ,function :background ,bg-active :weight bold :extend t))))
   `(diff-hunk-header ((t (:foreground ,comment-doc :background ,magit-hunk-bg :weight bold :extend t))))
   `(diff-index ((t (:foreground ,warning :background ,bg-active :weight bold :extend t))))
   `(diff-function ((t (:foreground ,keyword :background ,bg-alt :extend t))))
   `(diff-nonexistent ((t (:foreground ,fg-muted :background ,bg :strike-through t :extend t))))
   `(diff-error ((t (:foreground ,error :weight bold))))
   `(diff-warning ((t (:foreground ,warning :weight bold))))
   `(diff-indicator-added ((t (:foreground ,success :background ,diff-added-bg :weight bold))))
   `(diff-indicator-removed ((t (:foreground ,error :background ,diff-removed-bg :weight bold))))
   `(diff-indicator-changed ((t (:foreground ,function :background ,diff-changed-bg :weight bold))))
   `(diff-refine-added ((t (:foreground ,success :background ,diff-added-bg-highlight :weight bold :extend t))))
   `(diff-refine-removed ((t (:foreground ,error :background ,diff-removed-bg-highlight :weight bold :extend t))))
   `(diff-refine-changed ((t (:foreground ,function :background ,diff-changed-bg-highlight :weight bold :extend t))))

   ;; diff-hl
   ;; Foreground and background are intentionally identical because
   ;; these faces are commonly rendered as fringe or margin markers.
   `(diff-hl-insert ((t (:foreground ,success :background ,success))))
   `(diff-hl-delete ((t (:foreground ,error :background ,error))))
   `(diff-hl-change ((t (:foreground ,function :background ,function))))
   `(diff-hl-unknown ((t (:foreground ,fg-muted :background ,fg-muted))))
   `(diff-hl-margin-insert ((t (:foreground ,success :background ,success))))
   `(diff-hl-margin-delete ((t (:foreground ,error :background ,error))))
   `(diff-hl-margin-change ((t (:foreground ,function :background ,function))))
   `(diff-hl-margin-unknown ((t (:foreground ,fg-muted :background ,fg-muted))))

   ;; Newer diff-hl versions use these for inline hunk previews.
   `(diff-hl-show-hunk-inline-insert ((t (:foreground ,success :background ,diff-added-bg-highlight :extend t))))
   `(diff-hl-show-hunk-inline-delete ((t (:foreground ,error :background ,diff-removed-bg-highlight :extend t))))
   `(diff-hl-show-hunk-inline-change ((t (:foreground ,function :background ,diff-changed-bg-highlight :extend t))))
   `(diff-hl-show-hunk-posframe ((t (:foreground ,fg :background ,bg-alt))))

   ;; Ediff
   `(ediff-current-diff-A ((t (:foreground ,error :background ,diff-removed-bg :extend t))))
   `(ediff-current-diff-B ((t (:foreground ,success :background ,diff-added-bg :extend t))))
   `(ediff-current-diff-C ((t (:foreground ,function :background ,diff-changed-bg :extend t))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,warning :background ,diff-conflict-bg :extend t))))
   `(ediff-fine-diff-A ((t (:foreground ,error :background ,diff-removed-bg-highlight :weight bold :extend t))))
   `(ediff-fine-diff-B ((t (:foreground ,success :background ,diff-added-bg-highlight :weight bold :extend t))))
   `(ediff-fine-diff-C ((t (:foreground ,function :background ,diff-changed-bg-highlight :weight bold :extend t))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,warning :background ,diff-base-bg-highlight :weight bold :extend t))))
   `(ediff-even-diff-A ((t (:foreground ,fg-muted :background ,bg-alt :extend t))))
   `(ediff-even-diff-B ((t (:foreground ,fg-muted :background ,bg-alt :extend t))))
   `(ediff-even-diff-C ((t (:foreground ,fg-muted :background ,bg-alt :extend t))))
   `(ediff-even-diff-Ancestor ((t (:foreground ,fg-muted :background ,bg-alt :extend t))))
   `(ediff-odd-diff-A ((t (:foreground ,fg-muted :background ,bg :extend t))))
   `(ediff-odd-diff-B ((t (:foreground ,fg-muted :background ,bg :extend t))))
   `(ediff-odd-diff-C ((t (:foreground ,fg-muted :background ,bg :extend t))))
   `(ediff-odd-diff-Ancestor ((t (:foreground ,fg-muted :background ,bg :extend t))))

   ;; Smerge
   `(smerge-upper ((t (:foreground ,error :background ,diff-removed-bg :extend t))))
   `(smerge-lower ((t (:foreground ,success :background ,diff-added-bg :extend t))))
   `(smerge-base ((t (:foreground ,function :background ,diff-changed-bg :extend t))))
   `(smerge-markers ((t (:foreground ,warning :background ,bg-active :weight bold :extend t))))
   `(smerge-refined-added ((t (:foreground ,success :background ,diff-added-bg-highlight :weight bold))))
   `(smerge-refined-removed ((t (:foreground ,error :background ,diff-removed-bg-highlight :weight bold))))

   ;; Magit: sections and headings
   `(magit-section-highlight ((t (:background ,bg-alt :extend t))))
   `(magit-section-heading ((t (:foreground ,function :weight bold :extend t))))
   `(magit-section-heading-selection ((t (:foreground ,warning :background ,magit-selection-bg :weight bold :extend t))))
   `(magit-section-secondary-heading ((t (:foreground ,comment-doc :weight bold :extend t))))
   `(magit-header-line ((t (:foreground ,keyword :background ,bg-alt :weight bold :extend t))))
   `(magit-header-line-key ((t (:foreground ,function :background ,bg-alt :weight bold))))
   `(magit-dimmed ((t (:foreground ,fg-faint))))
   `(magit-filename ((t (:foreground ,comment-doc))))
   `(magit-hash ((t (:foreground ,fg-muted))))
   `(magit-keyword ((t (:foreground ,keyword))))
   `(magit-keyword-squash ((t (:foreground ,comment-doc))))

   ;; Magit: branches, refs, and tags
   `(magit-branch-current ((t (:foreground ,function :weight bold :box (:line-width -1 :color ,function)))))
   `(magit-branch-local ((t (:foreground ,type))))
   `(magit-branch-remote ((t (:foreground ,success))))
   `(magit-branch-upstream ((t (:foreground ,comment-doc))))
   `(magit-branch-warning ((t (:foreground ,warning :weight bold))))
   `(magit-head ((t (:foreground ,function :weight bold))))
   `(magit-refname ((t (:foreground ,fg-muted))))
   `(magit-tag ((t (:foreground ,keyword :weight bold))))
   `(magit-cherry-equivalent ((t (:foreground ,comment-doc))))
   `(magit-cherry-unmatched ((t (:foreground ,function))))

   ;; Magit: diff
   `(magit-diff-context ((t (:foreground ,fg-dim :background ,bg :extend t))))
   `(magit-diff-context-highlight ((t (:foreground ,fg :background ,bg-alt :extend t))))
   `(magit-diff-added ((t (:foreground ,success :background ,diff-added-bg :extend t))))
   `(magit-diff-added-highlight ((t (:foreground ,success :background ,diff-added-bg-highlight :weight bold :extend t))))
   `(magit-diff-removed ((t (:foreground ,error :background ,diff-removed-bg :extend t))))
   `(magit-diff-removed-highlight ((t (:foreground ,error :background ,diff-removed-bg-highlight :weight bold :extend t))))
   `(magit-diff-base ((t (:foreground ,warning :background ,diff-base-bg :extend t))))
   `(magit-diff-base-highlight ((t (:foreground ,warning :background ,diff-base-bg-highlight :weight bold :extend t))))
   `(magit-diff-our ((t (:foreground ,error :background ,diff-removed-bg :extend t))))
   `(magit-diff-our-highlight ((t (:foreground ,error :background ,diff-removed-bg-highlight :weight bold :extend t))))
   `(magit-diff-their ((t (:foreground ,success :background ,diff-added-bg :extend t))))
   `(magit-diff-their-highlight ((t (:foreground ,success :background ,diff-added-bg-highlight :weight bold :extend t))))
   `(magit-diff-conflict-heading ((t (:foreground ,warning :background ,diff-conflict-bg :weight bold :extend t))))
   `(magit-diff-file-heading ((t (:foreground ,fg :weight bold :extend t))))
   `(magit-diff-file-heading-highlight ((t (:foreground ,function :background ,bg-alt :weight bold :extend t))))
   `(magit-diff-file-heading-selection ((t (:foreground ,comment-doc :background ,magit-selection-bg :weight bold :extend t))))
   `(magit-diff-hunk-heading ((t (:foreground ,comment-doc :background ,magit-hunk-bg :extend t))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,fg :background ,magit-hunk-bg-highlight :weight bold :extend t))))
   `(magit-diff-hunk-heading-selection ((t (:foreground ,warning :background ,magit-selection-bg :weight bold :extend t))))
   `(magit-diff-revision-summary ((t (:foreground ,function :background ,bg-alt :weight bold :extend t))))
   `(magit-diff-revision-summary-highlight ((t (:foreground ,function :background ,magit-selection-bg :weight bold :extend t))))
   `(magit-diff-lines-boundary ((t (:foreground ,bg :background ,warning :weight bold :extend t))))
   `(magit-diff-lines-heading ((t (:foreground ,bg :background ,warning :weight bold :extend t))))
   `(magit-diff-whitespace-warning ((t (:foreground ,bg :background ,error :weight bold))))
   `(magit-diffstat-added ((t (:foreground ,success))))
   `(magit-diffstat-removed ((t (:foreground ,error))))

   ;; Magit: logs and blame
   `(magit-log-author ((t (:foreground ,number))))
   `(magit-log-date ((t (:foreground ,function))))
   `(magit-log-graph ((t (:foreground ,fg-muted))))
   `(magit-log-select ((t (:background ,bg-alt :weight bold))))
   `(magit-log-signature ((t (:foreground ,comment-doc))))
   `(magit-blame-heading ((t (:foreground ,warning :background ,bg-alt :weight bold :extend t))))
   `(magit-blame-highlight ((t (:foreground ,fg :background ,bg-alt :extend t))))
   `(magit-blame-margin ((t (:foreground ,fg-muted :background ,bg-alt))))
   `(magit-blame-hash ((t (:foreground ,function))))
   `(magit-blame-name ((t (:foreground ,number))))
   `(magit-blame-date ((t (:foreground ,comment-doc))))
   `(magit-blame-summary ((t (:foreground ,fg))))
   `(magit-blame-dimmed ((t (:foreground ,fg-faint))))

   ;; Magit: bisect, sequence, process, signatures
   `(magit-bisect-bad ((t (:foreground ,error :weight bold))))
   `(magit-bisect-good ((t (:foreground ,success :weight bold))))
   `(magit-bisect-skip ((t (:foreground ,warning :weight bold))))
   `(magit-process-ng ((t (:foreground ,error :weight bold))))
   `(magit-process-ok ((t (:foreground ,success :weight bold))))
   `(magit-sequence-drop ((t (:foreground ,error))))
   `(magit-sequence-head ((t (:foreground ,function))))
   `(magit-sequence-part ((t (:foreground ,warning))))
   `(magit-sequence-stop ((t (:foreground ,success))))
   `(magit-signature-bad ((t (:foreground ,error :weight bold))))
   `(magit-signature-error ((t (:foreground ,error :weight bold))))
   `(magit-signature-expired ((t (:foreground ,warning))))
   `(magit-signature-good ((t (:foreground ,success :weight bold))))
   `(magit-signature-revoked ((t (:foreground ,comment-doc))))
   `(magit-signature-untrusted ((t (:foreground ,keyword))))

   ;; Magit: reflog
   `(magit-reflog-amend ((t (:foreground ,comment-doc))))
   `(magit-reflog-checkout ((t (:foreground ,function))))
   `(magit-reflog-cherry-pick ((t (:foreground ,success))))
   `(magit-reflog-commit ((t (:foreground ,success))))
   `(magit-reflog-merge ((t (:foreground ,success))))
   `(magit-reflog-other ((t (:foreground ,type))))
   `(magit-reflog-rebase ((t (:foreground ,comment-doc))))
   `(magit-reflog-remote ((t (:foreground ,type))))
   `(magit-reflog-reset ((t (:foreground ,error :weight bold))))

   ;; Git Commit
   `(git-commit-summary ((t (:foreground ,fg :weight bold))))
   `(git-commit-overlong-summary ((t (:foreground ,error :weight bold))))
   `(git-commit-nonempty-second-line ((t (:foreground ,error :weight bold))))
   `(git-commit-comment-heading ((t (:foreground ,keyword :weight bold))))
   `(git-commit-comment-action ((t (:foreground ,warning :weight bold))))
   `(git-commit-comment-branch-local ((t (:foreground ,type))))
   `(git-commit-comment-branch-remote ((t (:foreground ,success))))
   `(git-commit-comment-file ((t (:foreground ,function))))
   `(git-commit-known-pseudo-header ((t (:foreground ,comment-doc :weight bold))))
   `(git-commit-pseudo-header ((t (:foreground ,fg-muted))))

   ;; Git Rebase
   `(git-rebase-action ((t (:foreground ,keyword :weight bold))))
   `(git-rebase-commit ((t (:foreground ,fg))))
   `(git-rebase-description ((t (:foreground ,fg-muted))))
   `(git-rebase-hash ((t (:foreground ,function))))
   `(git-rebase-kill-label ((t (:foreground ,error :weight bold))))
   `(git-rebase-label ((t (:foreground ,success :weight bold))))
   `(git-rebase-squash-label ((t (:foreground ,warning :weight bold))))

   ;; Transient
   `(transient-heading ((t (:foreground ,function :weight bold))))
   `(transient-key ((t (:foreground ,keyword :weight bold))))
   `(transient-argument ((t (:foreground ,warning))))
   `(transient-value ((t (:foreground ,string))))
   `(transient-inactive-argument ((t (:foreground ,fg-muted))))
   `(transient-inactive-value ((t (:foreground ,fg-faint))))
   `(transient-enabled-suffix ((t (:foreground ,success :background ,diff-added-bg :weight bold))))
   `(transient-disabled-suffix ((t (:foreground ,error :background ,diff-removed-bg :weight bold))))
   `(transient-unreachable ((t (:foreground ,fg-faint))))
   `(transient-unreachable-key ((t (:foreground ,fg-muted))))
   `(transient-inapt-suffix ((t (:foreground ,fg-faint))))
   `(transient-inapt-suffix-key ((t (:foreground ,fg-muted))))
   `(transient-mismatched-key ((t (:foreground ,error :weight bold))))
   `(transient-nonstandard-key ((t (:foreground ,warning :weight bold))))

   ;; Built-in VC
   `(vc-state-base ((t (:foreground ,fg-muted))))
   `(vc-up-to-date-state ((t (:foreground ,success))))
   `(vc-edited-state ((t (:foreground ,function))))
   `(vc-locally-added-state ((t (:foreground ,success))))
   `(vc-locked-state ((t (:foreground ,warning))))
   `(vc-needs-update-state ((t (:foreground ,warning))))
   `(vc-removed-state ((t (:foreground ,error))))
   `(vc-missing-state ((t (:foreground ,error :weight bold))))
   `(vc-conflict-state ((t (:foreground ,error :weight bold))))
   `(vc-dir-directory ((t (:foreground ,function :weight bold))))
   `(vc-dir-file ((t (:foreground ,fg))))
   `(vc-dir-header ((t (:foreground ,keyword :weight bold))))
   `(vc-dir-header-value ((t (:foreground ,function))))
   `(vc-dir-mark-indicator ((t (:foreground ,warning :weight bold))))
   `(vc-dir-status-edited ((t (:foreground ,function))))
   `(vc-dir-status-up-to-date ((t (:foreground ,success))))
   `(vc-dir-status-warning ((t (:foreground ,warning))))
   `(vc-dir-status-ignored ((t (:foreground ,fg-faint))))

   ;; Log View
   `(log-view-message ((t (:foreground ,fg :weight bold))))
   `(log-view-file ((t (:foreground ,function))))
   `(log-view-commit-body ((t (:foreground ,fg-muted))))
   `(log-view-label ((t (:foreground ,keyword :weight bold))))
   `(log-view-acknowledgement ((t (:foreground ,success))))
   `(log-view-alternative-mail ((t (:foreground ,comment-doc))))

   ;; git-gutter
   `(git-gutter:added ((t (:foreground ,success :background ,success :weight bold))))
   `(git-gutter:deleted ((t (:foreground ,error :background ,error :weight bold))))
   `(git-gutter:modified ((t (:foreground ,function :background ,function :weight bold))))
   `(git-gutter:unchanged ((t (:foreground ,fg-muted :background ,fg-muted))))
   `(git-gutter:separator ((t (:foreground ,border :background ,bg))))

   ;; git-gutter-fringe
   `(git-gutter-fr:added ((t (:foreground ,success))))
   `(git-gutter-fr:deleted ((t (:foreground ,error))))
   `(git-gutter-fr:modified ((t (:foreground ,function))))

   ;; Dired
   `(dired-directory ((t (:foreground ,function :weight bold))))
   `(dired-symlink ((t (:foreground ,type))))
   `(dired-marked ((t (:foreground ,warning :weight bold))))
   `(dired-flagged ((t (:foreground ,error :weight bold))))

   ;; Org
   `(org-level-1 ((t (:foreground ,function :weight bold))))
   `(org-level-2 ((t (:foreground ,keyword :weight bold))))
   `(org-level-3 ((t (:foreground ,comment-doc :weight bold))))
   `(org-level-4 ((t (:foreground ,type :weight bold))))
   `(org-code ((t (:foreground ,string))))
   `(org-verbatim ((t (:foreground ,number))))
   `(org-block ((t (:background ,bg-alt :foreground ,fg :extend t))))
   `(org-block-begin-line ((t (:background ,bg-alt :foreground ,comment :extend t))))
   `(org-block-end-line ((t (:inherit org-block-begin-line))))
   `(org-todo ((t (:foreground ,error :weight bold))))
   `(org-done ((t (:foreground ,success :weight bold))))
   `(org-date ((t (:foreground ,function :underline t))))

   ;; Markdown
   `(markdown-header-face-1 ((t (:foreground ,function :weight bold :height 1.15))))
   `(markdown-header-face-2 ((t (:foreground ,keyword :weight bold :height 1.10))))
   `(markdown-header-face-3 ((t (:foreground ,comment-doc :weight bold))))
   `(markdown-code-face ((t (:background ,bg-alt :foreground ,string))))
   `(markdown-inline-code-face ((t (:foreground ,string))))
   `(markdown-link-face ((t (:foreground ,function))))
   `(markdown-url-face ((t (:foreground ,fg-muted :underline t))))

   ;; Company
   `(company-tooltip ((t (:background ,bg-active :foreground ,fg))))
   `(company-tooltip-selection ((t (:background ,selection :foreground ,fg))))
   `(company-tooltip-common ((t (:foreground ,keyword :weight bold))))
   `(company-tooltip-annotation ((t (:foreground ,comment-doc))))
   `(company-scrollbar-bg ((t (:background ,bg-alt))))
   `(company-scrollbar-fg ((t (:background ,fg-muted))))

   ;; Corfu
   `(corfu-default ((t (:background ,bg-active :foreground ,fg))))
   `(corfu-current ((t (:background ,selection :foreground ,fg))))
   `(corfu-border ((t (:background ,border))))

   ;; ANSI terminal colors
   `(ansi-color-black ((t (:foreground ,bg :background ,bg))))
   `(ansi-color-red ((t (:foreground ,error :background ,error))))
   `(ansi-color-green ((t (:foreground ,success :background ,success))))
   `(ansi-color-yellow ((t (:foreground ,keyword :background ,keyword))))
   `(ansi-color-blue ((t (:foreground ,function :background ,function))))
   `(ansi-color-magenta ((t (:foreground ,comment-doc :background ,comment-doc))))
   `(ansi-color-cyan ((t (:foreground ,type :background ,type))))
   `(ansi-color-white ((t (:foreground ,fg :background ,fg))))

   ;; Tree-sitter faces used by newer Emacs versions
   `(font-lock-bracket-face ((t (:foreground ,fg-muted))))
   `(font-lock-delimiter-face ((t (:foreground ,fg-muted))))
   `(font-lock-operator-face ((t (:foreground ,fg))))
   `(font-lock-property-name-face ((t (:foreground ,type))))
   `(font-lock-property-use-face ((t (:foreground ,type))))
   `(font-lock-punctuation-face ((t (:foreground ,fg-muted))))
   `(font-lock-misc-punctuation-face ((t (:foreground ,fg-muted))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))

(provide-theme 'phosphor-noir)
;;; phosphor-noir-theme.el ends here

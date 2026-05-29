#' Copy Web Assets for Slide Themes
#'
#' Copies the CSS and HTML files required for theme switching (light, dark,
#' and high-contrast modes) into a \code{www/} directory in the current
#' working directory. These files can then be referenced by xaringan or
#' other HTML-based slide frameworks.
#'
#' This function is intended to support slide templates that provide
#' user-selectable display modes while defaulting to system preferences.
#'
#' @param ... Additional arguments (currently unused).
#'
#' @details
#' The function retrieves packaged web assets using \code{system.file()}
#' and copies them into a local \code{www/} directory. If the directory
#' does not exist, it will be created. Existing files with the same names
#' will be overwritten.
#'
#' The following files are copied:
#' \itemize{
#'   \item \code{theme-modes.css}: Defines CSS variables and overrides for
#'   light, dark, and high-contrast themes.
#'   \item \code{theme-selector.html}: Provides the JavaScript and UI controls
#'   for selecting display modes and persisting user preferences.
#' }
#'
#' These files can be included in a xaringan slide deck via the YAML header,
#' for example:
#'
#' \preformatted{
#' output:
#'   xaringan::moon_reader:
#'     css:
#'       - default
#'       - xaringan-themer.css
#'       - www/theme-modes.css
#'     includes:
#'       in_header: www/theme-selector.html
#' }
#'
#' @return Invisibly returns \code{NULL}. Called for its side effects.
#'
#' @examples
#' \dontrun{
#' get_web_files()
#' }
#'
#' @export
get_web_files <- function(...){
  cssf <- system.file("css", "theme-modes.css", package="DAslides")
  layf <- system.file("css", "layout.css", package="DAslides")
  self <- system.file("html", "theme-selector.html", package="DAslides")
  mjxf <- system.file("html", "mathjax.html", package="DAslides")
  dir.create("www", showWarnings = FALSE)
  file.copy(cssf, "www/theme-modes.css", overwrite = TRUE)
  file.copy(layf, "www/layout.css", overwrite = TRUE)
  file.copy(self, "www/theme-selector.html", overwrite = TRUE)
  file.copy(mjxf, "www/mathjax.html", overwrite = TRUE)
  message("Files copied to www/ directory.\n")
}

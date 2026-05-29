#' Accessible ggplot2 Theme for Slide Presentations
#'
#' An accessibility-focused ggplot2 theme designed for slide presentations.
#' This theme emphasizes high contrast, legibility, and minimal visual clutter,
#' making it suitable for projection and classroom use.
#'
#' The theme uses a light background with dark text, removes unnecessary grid
#' elements, and increases default text and line sizes to improve readability.
#'
#' @param base_size Base font size. Defaults to 20, which is larger than typical
#'   ggplot defaults and appropriate for slides.
#' @param base_family Base font family for plot text.
#' @param header_family Font family used for titles and headings.
#' @param accent Accent color (currently unused but included for future
#'   extensibility and consistency with other theme systems).
#' @param fg Foreground (text) color.
#' @param bg Background color.
#' @param grid Color for major grid lines.
#' @param grid_minor Color for minor grid lines (currently not used, as minor
#'   grid lines are removed for clarity).
#'
#' @details
#' This theme is built on top of \code{ggplot2::theme_minimal()} with the following
#' modifications:
#' \itemize{
#'   \item Uses a white background and dark text for high contrast.
#'   \item Removes minor grid lines and vertical grid lines to reduce clutter.
#'   \item Retains horizontal major grid lines to aid value estimation.
#'   \item Increases text sizes and line widths for better readability on screens
#'   and projectors.
#'   \item Restores axis lines and ticks, which are often removed in minimal themes
#'   but can improve interpretability.
#'   \item Styles facet strips and legends for clarity.
#' }
#'
#' This theme is intended to be used alongside accessible color scales, such as
#' \code{scale_colour_okabe_ito()} for discrete data and viridis-based scales
#' for continuous data.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_smooth(se = FALSE) +
#'   scale_colour_okabe_ito() +
#'   theme_DAslides()
#' }
#'
#' @seealso \code{\link[ggplot2]{theme_minimal}}
#'
#' @export
theme_DAslides <- function(
    base_size = 20,
    base_family = "Inter",
    header_family = "Inter",
    accent = "#0072B2",
    fg = "#111111",
    bg = "#FFFFFF",
    grid = "#D9D9D9",
    grid_minor = "#EFEFEF"
) {
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = bg, colour = NA),

      plot.title = ggplot2::element_text(
        family = header_family,
        face = "bold",
        colour = fg,
        size = ggplot2::rel(1.2),
        margin = ggplot2::margin(b = 10)
      ),
      plot.subtitle = ggplot2::element_text(
        colour = fg,
        size = ggplot2::rel(1.0),
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        colour = fg,
        size = ggplot2::rel(0.8),
        hjust = 1
      ),

      axis.title = ggplot2::element_text(
        colour = fg,
        face = "bold"
      ),
      axis.text = ggplot2::element_text(
        colour = fg
      ),
      axis.ticks = ggplot2::element_line(
        colour = fg,
        linewidth = 0.5
      ),
      axis.line = ggplot2::element_line(
        colour = fg,
        linewidth = 0.6
      ),

      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        colour = grid,
        linewidth = 0.4
      ),

      legend.background = ggplot2::element_rect(fill = bg, colour = NA),
      legend.key = ggplot2::element_rect(fill = bg, colour = NA),
      legend.title = ggplot2::element_text(
        face = "bold",
        colour = fg
      ),
      legend.text = ggplot2::element_text(
        colour = fg
      ),

      strip.background = ggplot2::element_rect(
        fill = "#F2F2F2",
        colour = "#BDBDBD",
        linewidth = 0.6
      ),
      strip.text = ggplot2::element_text(
        face = "bold",
        colour = fg
      ),

      complete = TRUE
    )
}

#' Accessible Classic ggplot2 Theme for Slide Presentations
#'
#' A minimal, accessibility-focused ggplot2 theme based on
#' \code{ggplot2::theme_classic()}, designed for slide presentations.
#'
#' This theme removes grid lines entirely and relies on strong axis lines,
#' clear text, and high contrast to support readability and interpretation.
#'
#' @param base_size Base font size. Defaults to 20, appropriate for slides.
#' @param base_family Base font family for plot text.
#' @param header_family Font family used for titles and headings.
#' @param fg Foreground (text) color.
#' @param bg Background color.
#'
#' @details
#' This theme is built on top of \code{ggplot2::theme_classic()} with the following
#' modifications:
#' \itemize{
#'   \item Uses a white background and dark text for maximum contrast.
#'   \item Removes all grid lines for a clean, uncluttered appearance.
#'   \item Emphasizes axis lines and ticks to support value interpretation.
#'   \item Increases text sizes and uses bold styling for titles and axis labels.
#'   \item Styles facet strips and legends for clarity.
#' }
#'
#' This variant may be preferable for plots where grid lines are unnecessary or
#' distracting, or where a more traditional “ink-on-paper” appearance is desired.
#'
#' @return A ggplot2 theme object.
#'
#' @import ggplot2
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   scale_colour_okabe_ito() +
#'   theme_DAslides_classic()
#' }
#'
#' @seealso \code{\link[ggplot2]{theme_classic}}
#' @export
theme_DAslides_classic <- function(
    base_size = 20,
    base_family = "Inter",
    header_family = "Inter",
    fg = "#111111",
    bg = "#FFFFFF"
) {
  ggplot2::theme_classic(
    base_size = base_size,
    base_family = base_family
  ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = bg, colour = NA),
      plot.title = ggplot2::element_text(face = "bold", colour = fg),
      plot.subtitle = ggplot2::element_text(colour = fg),
      axis.title = ggplot2::element_text(face = "bold", colour = fg),
      axis.text = ggplot2::element_text(colour = fg),
      legend.title = ggplot2::element_text(face = "bold", colour = fg),
      legend.text = ggplot2::element_text(colour = fg),
      strip.background = ggplot2::element_rect(
        fill = "#F2F2F2", colour = "#BDBDBD", linewidth = 0.6
      ),
      strip.text = ggplot2::element_text(face = "bold", colour = fg),
      complete = TRUE
    )
}

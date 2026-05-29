#' Okabe-Ito palette
#'
#' Returns colors from the Okabe-Ito palette.
#'
#' @param palette Integer or character. Currently only one palette is supported.
#' @param direction Sets the order of the colors. If -1, the order of colors
#'   is reversed.
#'
#' @return A function that takes \code{n} and returns \code{n} colors.
#' @noRd
okabe_ito_pal <- function(palette = 1, direction = 1) {
  okabe_ito <- c(
    "orange" = "#E69F00",
    "sky_blue" = "#56B4E9",
    "bluish_green" = "#009E73",
    "yellow" = "#F0E442",
    "blue" = "#0072B2",
    "vermillion" = "#D55E00",
    "reddish_purple" = "#CC79A7",
    "black" = "#000000"
  )

  direction <- as.integer(direction)
  if (!identical(direction, 1L) && !identical(direction, -1L)) {
    stop("direction must be 1 or -1", call. = FALSE)
  }

  if (direction == -1) {
    okabe_ito <- rev(okabe_ito)
  }

  force(okabe_ito)

  function(n) {
    if (n < 0) {
      stop("n must be non-negative", call. = FALSE)
    }

    if (n > length(okabe_ito)) {
      stop(
        sprintf(
          "Okabe-Ito palette supports at most %d discrete values; you requested %d.",
          length(okabe_ito), n
        ),
        call. = FALSE
      )
    }

    unname(okabe_ito[seq_len(n)])
  }
}

#' Discrete Okabe-Ito scale for colour aesthetics
#'
#' A discrete colour scale using the Okabe-Ito colorblind-friendly palette.
#' This is intended to mirror the interface of \code{ggplot2::scale_colour_brewer()}.
#'
#' @param ... Other arguments passed on to \code{ggplot2::discrete_scale()}.
#' @param type Included for interface compatibility with
#'   \code{ggplot2::scale_colour_brewer()}. Ignored.
#' @param palette Included for interface compatibility with
#'   \code{ggplot2::scale_colour_brewer()}. Currently only a single Okabe-Ito
#'   palette is supported.
#' @param direction Sets the order of colors in the scale. If -1, the order
#'   of colors is reversed.
#' @param aesthetics Character vector of aesthetics to scale, default
#'   \code{"colour"}.
#'
#' @importFrom ggplot2 discrete_scale
#'
#' @return A ggplot2 scale object.
#' @export
scale_colour_okabe_ito <- function(...,
                                   type = "qual",
                                   palette = 1,
                                   direction = 1,
                                   aesthetics = "colour") {
  direction <- as.integer(direction)
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    scale_name = "okabe_ito",
    palette = okabe_ito_pal(palette = palette, direction = direction),
    ...
  )
}

#' @rdname scale_colour_okabe_ito
#' @export
scale_color_okabe_ito <- scale_colour_okabe_ito

#' Discrete Okabe-Ito scale for fill aesthetics
#'
#' A discrete fill scale using the Okabe-Ito colorblind-friendly palette.
#' This is intended to mirror the interface of \code{ggplot2::scale_fill_brewer()}.
#'
#' @param ... Other arguments passed on to \code{ggplot2::discrete_scale()}.
#' @param type Included for interface compatibility with
#'   \code{ggplot2::scale_fill_brewer()}. Ignored.
#' @param palette Included for interface compatibility with
#'   \code{ggplot2::scale_fill_brewer()}. Currently only a single Okabe-Ito
#'   palette is supported.
#' @param direction Sets the order of colors in the scale. If -1, the order
#'   of colors is reversed.
#' @param aesthetics Character vector of aesthetics to scale, default
#'   \code{"fill"}.
#'
#' @importFrom ggplot2 discrete_scale
#' @return A ggplot2 scale object.
#' @export
scale_fill_okabe_ito <- function(...,
                                 type = "qual",
                                 palette = 1,
                                 direction = 1,
                                 aesthetics = "fill") {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    scale_name = "okabe_ito",
    palette = okabe_ito_pal(palette = palette, direction = direction),
    ...
  )
}

#' Okabe-Ito colors
#'
#' @return Named character vector of Okabe-Ito colors.
#' @export
okabe_ito_cols <- function() {
  c(
    "blue" = "#0072B2",
    "vermillion" = "#D55E00",
    "bluish_green" = "#009E73",
    "yellow" = "#F0E442",
    "reddish_purple" = "#CC79A7",
    "sky_blue" = "#56B4E9",
    "orange" = "#E69F00",
    "black" = "#000000"
  )
}

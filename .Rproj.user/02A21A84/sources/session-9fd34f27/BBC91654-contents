#' Add significance annotations across two facets
#'
#' Add horizontal significance lines and labels across two facet panels in a
#' faceted `ggplot2` figure. This function is designed for plots with exactly
#' two facet levels arranged horizontally, and is especially useful when
#' comparisons are made between the same x-group across two facets.
#'
#' In addition to drawing cross-facet significance annotations, this function
#' can:
#' \itemize{
#'   \item automatically remove the gap between facet panels on the x-axis,
#'   \item hide the default x-axis line,
#'   \item estimate a safe upper y-limit to avoid overlap between annotations
#'   and the plotting region,
#'   \item place `"ns"` labels slightly higher than significance symbols.
#' }
#'
#' @param plot_data A data frame used for plotting. Objects coercible to a
#'   `data.frame` are also accepted.
#' @param cross_data A data frame containing cross-facet comparison results.
#'   This is typically the `@stat` slot extracted from the S4 result returned
#'   by [tinystatr::stat3()]. It must contain at least `group_col` and `label`.
#' @param x A single character string giving the x variable name in `plot_data`.
#' @param y A single character string giving the y variable name in `plot_data`.
#' @param facet_var A single character string giving the facet variable name in
#'   `plot_data`.
#' @param facet_levels Optional character vector of length 2 specifying which
#'   two facet levels to connect. If `NULL`, the function uses the detected facet
#'   order in `plot_data`.
#' @param group_col A single character string giving the column name in
#'   `cross_data` that identifies the x-group for cross-facet comparison.
#' @param label A single character string giving the column name in
#'   `cross_data` that stores significance labels, such as `"*"`, `"**"`,
#'   or `"ns"`.
#' @param hide.ns Logical; if `TRUE`, rows with label `"ns"` are removed before
#'   plotting.
#' @param x_levels Optional character vector specifying the order of x groups.
#'   If `NULL`, the order is inferred from `plot_data`.
#' @param y_base Optional numeric scalar giving the baseline y position for the
#'   first cross-facet annotation. If `NULL`, the function estimates it from the
#'   maximum of the plotted data and optional facet annotation positions.
#' @param facet_stat_data Optional statistical results for within-facet
#'   comparisons, typically the `@stat` slot from [tinystatr::stat3()]. When
#'   provided and `y_base = NULL`, this is used to prevent overlap between
#'   within-facet and cross-facet annotations.
#' @param expand Numeric scalar controlling how far above the base height the
#'   first cross-facet annotation is placed.
#' @param step.increase Numeric scalar controlling the vertical spacing between
#'   successive cross-facet annotations.
#' @param size Numeric scalar giving the label text size.
#' @param fontface Character string specifying label font face, such as
#'   `"plain"` or `"bold"`.
#' @param line_width Numeric scalar giving the width of cross-facet horizontal
#'   lines.
#' @param sig_offset Optional numeric scalar controlling the vertical offset of
#'   significant labels (e.g. `"*"`, `"**"`). If `NULL`, an automatic value is
#'   used.
#' @param ns_offset Optional numeric scalar controlling the vertical offset of
#'   `"ns"` labels. If `NULL`, an automatic value is used and is set slightly
#'   higher than `sig_offset`.
#' @param axis_extend Optional numeric scalar controlling how far the custom
#'   broken x-axis extends into adjacent facet boundaries. If `NULL`, this is
#'   automatically chosen based on the number of x groups.
#' @param axis_linewidth Numeric scalar giving the line width of the custom
#'   broken x-axis.
#' @param auto_panel_fix Logical; if `TRUE`, automatically add
#'   `panel.spacing.x = unit(0, "cm")` and `axis.line.x = element_blank()` to
#'   reduce visible gaps between facets.
#' @param auto_ylim Logical; if `TRUE`, automatically expand the y-axis upper
#'   limit based on the maximum of raw data, within-facet annotations, and
#'   cross-facet annotations.
#' @param ylim_top_expand Numeric scalar giving extra expansion above the highest
#'   detected annotation when `auto_ylim = TRUE`.
#'
#' @return A list of `ggplot2` layers that can be added to a ggplot object.
#'
#' @details
#' This function currently supports exactly two facet panels. It is intended for
#' horizontally arranged facets, such as those produced by
#' `facet_wrap(..., nrow = 1)` or equivalent layouts.
#'
#' The `cross_data` input should contain one row per cross-facet annotation. The
#' group specified by `group_col` must match one of the x-axis groups in
#' `plot_data`.
#'
#' If `facet_stat_data` is supplied, the function internally computes the
#' maximum within-facet annotation height and uses it together with the raw data
#' maximum to determine a safe starting height for cross-facet annotations.
#'
#' Because this function may internally modify panel spacing, x-axis appearance,
#' and y-axis limits, it should usually be added at the end of the ggplot
#' construction, preferably after `theme()` and other scale or coordinate
#' settings.
#'
#' @examples
#' \dontrun{
#' p +
#'   theme_prism() +
#'   theme(...) +
#'   add_cross_facet_annotation(
#'     plot_data = testdata,
#'     cross_data = cross_cpg,
#'     x = "group",
#'     y = "value",
#'     facet_var = "diet",
#'     group_col = "smoke",
#'     label = "p.adj.signif",
#'     hide.ns = TRUE,
#'     facet_stat_data = stat_cpg,
#'     expand = 0.12,
#'     step.increase = 0.10,
#'     size = 6
#'   )
#' }
#'
#' @export
add_cross_facet_annotation <- function(plot_data,
                                       cross_data,
                                       x,
                                       y,
                                       facet_var,
                                       facet_levels   = NULL,
                                       group_col      = "group",
                                       label          = "p.adj.signif",
                                       hide.ns        = TRUE,
                                       x_levels       = NULL,
                                       y_base         = NULL,
                                       facet_stat_data = NULL,
                                       expand         = 0.12,
                                       step.increase  = 0.10,
                                       size           = 8,
                                       fontface       = "bold",
                                       line_width     = 3,
                                       sig_offset     = NULL,
                                       ns_offset      = NULL,
                                       axis_extend    = NULL,
                                       axis_linewidth = 1,
                                       auto_panel_fix = TRUE,
                                       auto_ylim      = TRUE,
                                       ylim_top_expand = 0.002) {

  plot_data <- .as_df(plot_data)
  cross_data <- .as_df(cross_data)
  if (!is.null(facet_stat_data)) {
    facet_stat_data <- .as_df(facet_stat_data)
  }

  .validate_single_string(x, "x")
  .validate_single_string(y, "y")
  .validate_single_string(facet_var, "facet_var")
  .validate_single_string(group_col, "group_col")
  .validate_single_string(label, "label")
  .validate_logical_scalar(hide.ns, "hide.ns")
  .validate_logical_scalar(auto_panel_fix, "auto_panel_fix")
  .validate_logical_scalar(auto_ylim, "auto_ylim")

  if (!is.null(y_base)) {
    if (!is.numeric(y_base) || length(y_base) != 1 || is.na(y_base) || !is.finite(y_base)) {
      stop("`y_base` must be NULL or one finite numeric value.", call. = FALSE)
    }
  }

  if (!is.null(sig_offset)) {
    if (!is.numeric(sig_offset) || length(sig_offset) != 1 || is.na(sig_offset) ||
        !is.finite(sig_offset) || sig_offset < 0) {
      stop("`sig_offset` must be NULL or one non-negative finite numeric value.", call. = FALSE)
    }
  }

  if (!is.null(ns_offset)) {
    if (!is.numeric(ns_offset) || length(ns_offset) != 1 || is.na(ns_offset) ||
        !is.finite(ns_offset) || ns_offset < 0) {
      stop("`ns_offset` must be NULL or one non-negative finite numeric value.", call. = FALSE)
    }
  }

  if (!is.null(axis_extend)) {
    if (!is.numeric(axis_extend) || length(axis_extend) != 1 || is.na(axis_extend) ||
        !is.finite(axis_extend) || axis_extend < 0) {
      stop("`axis_extend` must be NULL or one non-negative finite numeric value.", call. = FALSE)
    }
  }

  if (!is.numeric(ylim_top_expand) || length(ylim_top_expand) != 1 || is.na(ylim_top_expand) ||
      !is.finite(ylim_top_expand) || ylim_top_expand < 0) {
    stop("`ylim_top_expand` must be one non-negative finite numeric value.", call. = FALSE)
  }

  for (v in c(x, y, facet_var)) {
    if (!v %in% colnames(plot_data)) {
      stop(paste0("`", v, "` is not a column in `plot_data`."), call. = FALSE)
    }
  }

  if (!group_col %in% colnames(cross_data)) {
    stop(
      paste0("`group_col` ('", group_col, "') is not a column in `cross_data`."),
      call. = FALSE
    )
  }

  if (!label %in% colnames(cross_data)) {
    stop(
      paste0("`label` ('", label, "') is not a column in `cross_data`."),
      call. = FALSE
    )
  }

  x_raw <- plot_data[[x]]
  if (!(is.factor(x_raw) || is.character(x_raw) || is.numeric(x_raw))) {
    stop(
      "`x` must be a discrete variable (factor, character, or discrete numeric).",
      call. = FALSE
    )
  }

  if (is.null(facet_levels)) {
    fv <- plot_data[[facet_var]]
    facet_levels <- if (is.factor(fv)) {
      levels(droplevels(fv))
    } else {
      sort(unique(as.character(fv)))
    }
  } else {
    if (!is.character(facet_levels) || anyNA(facet_levels)) {
      stop("`facet_levels` must be a character vector without NA.", call. = FALSE)
    }
    facet_levels <- unique(facet_levels)
  }

  if (length(facet_levels) != 2) {
    stop(
      "`add_cross_facet_annotation()` currently supports exactly 2 facets.",
      call. = FALSE
    )
  }

  present_facets <- unique(as.character(plot_data[[facet_var]]))
  if (!all(facet_levels %in% present_facets)) {
    stop(
      paste0(
        "The following `facet_levels` are not present in `plot_data`: ",
        paste(setdiff(facet_levels, present_facets), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  x_levels <- .get_discrete_levels(x_raw, x_levels = x_levels)
  n_groups <- length(x_levels)

  if (is.null(axis_extend)) {
    axis_extend <- if (n_groups <= 2) {
      0.65
    } else if (n_groups == 3) {
      0.50
    } else {
      0.40
    }
  }

  .warn_if_numeric_label(cross_data, label)
  cross_data <- .filter_ns_rows(cross_data, label, hide.ns)

  y_raw <- suppressWarnings(as.numeric(plot_data[[y]]))
  if (all(is.na(y_raw))) {
    stop(
      "`y` in `plot_data` must contain at least one non-missing numeric value.",
      call. = FALSE
    )
  }

  y_min <- min(y_raw, na.rm = TRUE)
  y_max <- max(y_raw, na.rm = TRUE)
  y_range <- diff(range(y_raw, na.rm = TRUE))
  if (!is.finite(y_range) || y_range == 0) {
    y_range <- max(abs(y_raw), na.rm = TRUE)
  }
  if (!is.finite(y_range) || y_range == 0) {
    y_range <- 1
  }

  facet_ymax <- -Inf
  if (!is.null(facet_stat_data) && nrow(facet_stat_data) > 0) {
    facet_ready <- .prepare_pvalue_facet_annotation_data(
      plot_data = plot_data,
      stat_data = facet_stat_data,
      x = x,
      y = y,
      facet_var = facet_var,
      label = label,
      hide.ns = hide.ns,
      expand = expand,
      step.increase = step.increase,
      x_levels = x_levels
    )

    if (nrow(facet_ready) > 0 && "y.position" %in% colnames(facet_ready)) {
      facet_ymax <- max(facet_ready$y.position, na.rm = TRUE)
    }
  }

  if (is.null(y_base)) {
    y_base <- max(y_max, facet_ymax, na.rm = TRUE)
  }

  left_data  <- plot_data[as.character(plot_data[[facet_var]]) == facet_levels[1], , drop = FALSE]
  right_data <- plot_data[as.character(plot_data[[facet_var]]) == facet_levels[2], , drop = FALSE]

  if (nrow(left_data) == 0 || nrow(right_data) == 0) {
    stop("Both facets must contain data in `plot_data`.", call. = FALSE)
  }

  if (is.null(sig_offset)) {
    sig_offset <- max(0.002, size * 0.002)
  }
  if (is.null(ns_offset)) {
    ns_offset <- sig_offset + max(0.009, size * 0.009)
  }

  text_fontsize <- size * getFromNamespace(".pt", "ggplot2") * 1.0

  x_index <- setNames(seq_along(x_levels), x_levels)

  valid_group <- as.character(cross_data[[group_col]]) %in% x_levels
  if (!all(valid_group)) {
    bad_groups <- unique(as.character(cross_data[[group_col]][!valid_group]))
    warning(
      paste0(
        "Dropping groups in `cross_data` not found in `x_levels`: ",
        paste(bad_groups, collapse = ", ")
      ),
      call. = FALSE
    )
    cross_data <- cross_data[valid_group, , drop = FALSE]
  }

  line_grob <- grid::linesGrob(
    gp = grid::gpar(col = "black", lwd = line_width)
  )

  layers <- list()

  # broken x-axis
  for (i in seq_along(facet_levels)) {
    x_start <- if (i == 1) -Inf else 1 - axis_extend
    x_end   <- if (i == length(facet_levels)) Inf else n_groups + axis_extend

    df_ax <- data.frame(
      tmp_facet = facet_levels[i],
      x_s = x_start,
      x_e = x_end,
      stringsAsFactors = FALSE
    )
    colnames(df_ax)[1] <- facet_var

    layers[[length(layers) + 1]] <- ggplot2::geom_segment(
      data = df_ax,
      mapping = ggplot2::aes(x = x_s, xend = x_e, y = -Inf, yend = -Inf),
      color = "black",
      linewidth = axis_linewidth,
      inherit.aes = FALSE
    )
  }

  if (auto_panel_fix) {
    layers[[length(layers) + 1]] <- ggplot2::theme(
      panel.spacing.x = grid::unit(0, "cm"),
      axis.line.x = ggplot2::element_blank()
    )
  }

  max_cross_label_y <- -Inf

  if (nrow(cross_data) > 0) {
    cross_data <- cross_data[
      order(match(as.character(cross_data[[group_col]]), x_levels)),
      ,
      drop = FALSE
    ]

    for (i in seq_len(nrow(cross_data))) {
      grp  <- as.character(cross_data[[group_col]][i])
      sig  <- as.character(cross_data[[label]][i])
      gidx <- unname(x_index[grp])

      if (is.na(gidx)) next

      y_line <- y_base + y_range * (expand + (i - 1) * step.increase)

      sig_trim <- trimws(sig)
      is_ns <- tolower(sig_trim) == "ns"
      this_offset <- if (is_ns) ns_offset else sig_offset
      y_text <- y_line + y_range * this_offset

      max_cross_label_y <- max(max_cross_label_y, y_text, na.rm = TRUE)

      star_grob <- grid::grobTree(
        grid::textGrob(
          sig,
          x = 0.5,
          y = 0.5,
          hjust = 0.5,
          vjust = 0.5,
          gp = grid::gpar(
            col = "black",
            fontsize = text_fontsize,
            fontface = fontface
          )
        )
      )

      layers[[length(layers) + 1]] <- annotation_custom2(
        grob = line_grob,
        data = left_data,
        ymin = y_line, ymax = y_line,
        xmin = gidx, xmax = Inf
      )

      layers[[length(layers) + 1]] <- annotation_custom2(
        grob = line_grob,
        data = right_data,
        ymin = y_line, ymax = y_line,
        xmin = -Inf, xmax = gidx
      )

      if (gidx <= ceiling(length(x_levels) / 2)) {
        layers[[length(layers) + 1]] <- annotation_custom2(
          grob = star_grob,
          data = left_data,
          ymin = y_text, ymax = y_text,
          xmin = gidx,
          xmax = length(x_levels) + 1
        )
      } else {
        layers[[length(layers) + 1]] <- annotation_custom2(
          grob = star_grob,
          data = right_data,
          ymin = y_text, ymax = y_text,
          xmin = 0,
          xmax = gidx
        )
      }
    }
  }

  if (auto_ylim) {
    y_upper <- max(y_max, facet_ymax, max_cross_label_y, na.rm = TRUE)
    if (!is.finite(y_upper)) y_upper <- y_max
    y_upper <- y_upper + y_range * ylim_top_expand

    layers[[length(layers) + 1]] <- ggplot2::coord_cartesian(
      ylim = c(y_min, y_upper),
      clip = "off"
    )
  } else {
    layers[[length(layers) + 1]] <- ggplot2::coord_cartesian(clip = "off")
  }

  layers
}

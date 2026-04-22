#' Add significance annotations across two facet panels
#'
#' Draw horizontal significance lines and labels that span across two
#' horizontally adjacent facet panels in a `ggplot2` figure.
#'
#' In addition to cross-facet annotations, this function:
#' \itemize{
#'   \item draws a broken x-axis between the two facet panels,
#'   \item can automatically remove the default continuous x-axis line and set
#'     panel spacing to zero (`auto_panel_fix = TRUE`),
#'   \item can automatically expand the upper y-limit to prevent clipping of
#'     labels and lines (`auto_ylim = TRUE`),
#'   \item coordinates with within-facet bracket heights via `facet_stat_data`
#'     to reduce overlap,
#'   \item inherits axis line colour, axis line width, and text family from the
#'     active ggplot2 theme.
#' }
#'
#' This function is intended to be used together with
#' [add_pvalue_facet_annotation()] when both within-facet and cross-facet
#' annotations are needed.
#'
#' @param plot_data A data frame (or object coercible to one) used for plotting.
#'   Must include the columns named by `x`, `y`, and `facet_var`.
#' @param cross_data A data frame (or object coercible to one) containing
#'   cross-facet comparison results. Must contain at least the columns named by
#'   `group_col` and `label`. Typically constructed from the `@stat` slot of
#'   [tinystatr::stat2()] or [tinystatr::stat3()], with one row per x-group.
#' @param x A single character string giving the x-axis variable name in
#'   `plot_data`.
#' @param y A single character string giving the y-axis variable name in
#'   `plot_data`.
#' @param facet_var A single character string giving the facet variable name in
#'   `plot_data`. Must match the variable used in `facet_wrap()`.
#' @param facet_levels An optional character vector of length 2 specifying the
#'   two facet levels to connect, in left-to-right order. When `NULL` (default),
#'   the order is auto-detected from `plot_data[[facet_var]]`.
#' @param group_col A single character string giving the column name in
#'   `cross_data` that identifies the x-group for each cross-facet comparison.
#'   Defaults to `"group"`.
#' @param label A single character string giving the column name in `cross_data`
#'   that stores significance labels. Defaults to `"p.adj.signif"`.
#' @param hide.ns Controls removal of non-significant rows before plotting.
#'   Defaults to `TRUE`. See [add_pvalue_annotation()] for accepted values.
#' @param signif.cutoff A numeric threshold used when `hide.ns` targets a
#'   numeric column. Defaults to `0.05`.
#' @param x_levels An optional character vector specifying the order of x
#'   groups. When `NULL` (default), inferred from `plot_data[[x]]`.
#' @param y_base An optional single finite numeric value giving the baseline
#'   y-position from which cross-facet annotation heights are computed. When
#'   `NULL` (default), `y_base` is estimated automatically from the maximum of
#'   the raw data and, if provided, the highest within-facet bracket.
#' @param facet_stat_data An optional data frame containing within-facet
#'   comparison results, typically the same `stat_data` passed to
#'   [add_pvalue_facet_annotation()]. When provided and `y_base = NULL`, this is
#'   used to place cross-facet annotations above within-facet brackets.
#' @param expand A numeric scalar controlling how far above `y_base` the first
#'   cross-facet line is placed, expressed as a fraction of the y-range.
#'   Defaults to `0.12`.
#' @param step.increase A numeric scalar controlling the vertical spacing
#'   between successive cross-facet lines, expressed as a fraction of the
#'   y-range. Defaults to `0.10`.
#' @param size A numeric scalar giving the text size of cross-facet labels.
#'   Defaults to `8`.
#' @param fontface A character string specifying the font face for cross-facet
#'   labels, e.g. `"bold"` or `"plain"`. Defaults to `"plain"`.
#' @param line_width A numeric scalar giving the visual width of cross-facet
#'   horizontal lines, on a ggplot-like scale. Defaults to `1`.
#' @param sig_offset Reserved for backward compatibility. Currently validated
#'   but not used in the current label placement system.
#' @param ns_offset Reserved for backward compatibility. Currently validated
#'   but not used in the current label placement system.
#' @param sig_nudge_pt A numeric scalar giving the vertical offset, in points,
#'   between a significant label (`"*"`, `"**"`, `"***"`, etc.) and its
#'   horizontal line. Can be negative. Defaults to `-6`.
#' @param ns_nudge_pt A numeric scalar giving the vertical offset, in points,
#'   between an `"ns"` label and its horizontal line. Defaults to `3.5`.
#' @param label_band_mult A positive numeric scalar giving the vertical height
#'   reserved above each cross-facet line for the label, expressed as a
#'   fraction of the y-range. Defaults to `0.06`.
#' @param axis_extend An optional non-negative numeric scalar controlling how
#'   far the broken x-axis extends toward the panel gap. A smaller value
#'   produces a wider visible gap. When `NULL` (default), the value is chosen
#'   automatically based on the number of x groups.
#' @param auto_panel_fix Logical. When `TRUE` (default), automatically injects
#'   `theme(panel.spacing.x = unit(0, "cm"), axis.line.x = element_blank())`
#'   to close the gap between panels and remove the default continuous x-axis
#'   line.
#' @param auto_ylim Logical. When `TRUE` (default), automatically injects
#'   `coord_cartesian(ylim = c(y_min, y_upper), clip = "off")`, where `y_upper`
#'   is computed from the highest annotation position. When `FALSE`, only
#'   `coord_cartesian(clip = "off")` is added.
#' @param ylim_top_expand A single non-negative numeric value giving additional
#'   expansion above the highest detected annotation when `auto_ylim = TRUE`,
#'   expressed as a fraction of the y-range. Defaults to `0.002`.
#'
#' @return A list of ggplot2 layers that can be added to a `ggplot` object
#'   with `+`.
#'
#' @details
#' This function currently supports exactly two horizontally arranged facet
#' panels.
#'
#' The broken x-axis is drawn using the current ggplot2 theme axis line
#' settings. Cross-facet text labels inherit the current theme text family, and
#' line and text colour inherit the current theme axis line colour.
#'
#' Label positions are controlled using fixed point-based offsets
#' (`sig_nudge_pt`, `ns_nudge_pt`) rather than pure data-scale offsets, so the
#' apparent distance between the label and the line remains more stable when the
#' figure size changes.
#'
#' This function should generally be added as the final layer in the ggplot call
#' chain, because it injects `coord_cartesian()` and may also inject a `theme()`
#' adjustment.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#' library(tinystatr)
#' library(tinysignifr)
#'
#' data("ToothGrowth")
#' ToothGrowth$dose <- factor(ToothGrowth$dose, levels = c("0.5", "1", "2"))
#' ToothGrowth$supp <- factor(ToothGrowth$supp, levels = c("OJ", "VC"))
#'
#' stat_within <- rbind(
#'   stat3(
#'     dplyr::filter(ToothGrowth, supp == "OJ"),
#'     "dose", "len", formula = len ~ dose
#'   )@stat %>% mutate(supp = "OJ"),
#'   stat3(
#'     dplyr::filter(ToothGrowth, supp == "VC"),
#'     "dose", "len", formula = len ~ dose
#'   )@stat %>% mutate(supp = "VC")
#' )
#'
#' stat_cross <- do.call(rbind, lapply(levels(ToothGrowth$dose), function(d) {
#'   stat3(
#'     dplyr::filter(ToothGrowth, dose == d),
#'     "supp", "len", formula = len ~ supp
#'   )@stat %>% mutate(dose_group = d)
#' }))
#'
#' ggplot(ToothGrowth, aes(x = dose, y = len)) +
#'   geom_violin(aes(color = dose), fill = NA) +
#'   facet_wrap(~ supp, strip.position = "bottom") +
#'   scale_x_discrete(expand = expansion(add = 0.7)) +
#'   theme_classic() +
#'   theme(
#'     strip.placement  = "outside",
#'     strip.background = element_rect(color = NA),
#'     legend.position  = "none"
#'   ) +
#'   add_pvalue_facet_annotation(
#'     plot_data = ToothGrowth,
#'     stat_data = stat_within,
#'     x = "dose",
#'     y = "len",
#'     facet_var = "supp",
#'     hide.ns = FALSE
#'   ) +
#'   add_cross_facet_annotation(
#'     plot_data = ToothGrowth,
#'     cross_data = stat_cross,
#'     x = "dose",
#'     y = "len",
#'     facet_var = "supp",
#'     group_col = "dose_group",
#'     label = "p.adj.signif",
#'     hide.ns = FALSE,
#'     facet_stat_data = stat_within
#'   )
#' }
#'
#' @seealso [add_pvalue_annotation()], [add_pvalue_facet_annotation()],
#'   [add_pvalue_mixed()]
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
                                       signif.cutoff  = 0.05,
                                       x_levels       = NULL,
                                       y_base         = NULL,
                                       facet_stat_data = NULL,
                                       expand         = 0.12,
                                       step.increase  = 0.10,
                                       size           = 8,
                                       fontface       = "plain",
                                       line_width     = 1,
                                       sig_offset     = NULL,
                                       ns_offset      = NULL,
                                       sig_nudge_pt   = -6,
                                       ns_nudge_pt    = 3.5,
                                       label_band_mult = 0.06,
                                       axis_extend    = NULL,
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
  .validate_logical_scalar(auto_panel_fix, "auto_panel_fix")
  .validate_logical_scalar(auto_ylim, "auto_ylim")

  if (!is.logical(hide.ns) && !is.character(hide.ns)) {
    stop("`hide.ns` must be TRUE/FALSE or a character column specifier.", call. = FALSE)
  }

  if (!is.numeric(signif.cutoff) || length(signif.cutoff) != 1 ||
      is.na(signif.cutoff) || !is.finite(signif.cutoff) || signif.cutoff < 0) {
    stop("`signif.cutoff` must be one non-negative finite numeric value.", call. = FALSE)
  }

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

  if (!is.numeric(sig_nudge_pt) || length(sig_nudge_pt) != 1 ||
      is.na(sig_nudge_pt) || !is.finite(sig_nudge_pt)) {
    stop("`sig_nudge_pt` must be one finite numeric value.", call. = FALSE)
  }

  if (!is.numeric(ns_nudge_pt) || length(ns_nudge_pt) != 1 ||
      is.na(ns_nudge_pt) || !is.finite(ns_nudge_pt)) {
    stop("`ns_nudge_pt` must be one finite numeric value.", call. = FALSE)
  }

  if (!is.numeric(label_band_mult) || length(label_band_mult) != 1 ||
      is.na(label_band_mult) || !is.finite(label_band_mult) || label_band_mult <= 0) {
    stop("`label_band_mult` must be one positive finite numeric value.", call. = FALSE)
  }

  if (!is.null(axis_extend)) {
    if (!is.numeric(axis_extend) || length(axis_extend) != 1 || is.na(axis_extend) ||
        !is.finite(axis_extend) || axis_extend < 0) {
      stop("`axis_extend` must be NULL or one non-negative finite numeric value.", call. = FALSE)
    }
  }

  if (!is.numeric(line_width) || length(line_width) != 1 ||
      is.na(line_width) || !is.finite(line_width) || line_width < 0) {
    stop("`line_width` must be one non-negative finite numeric value.", call. = FALSE)
  }

  if (!is.numeric(ylim_top_expand) || length(ylim_top_expand) != 1 ||
      is.na(ylim_top_expand) || !is.finite(ylim_top_expand) || ylim_top_expand < 0) {
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
  cross_data <- .filter_ns_rows(
    stat_data = cross_data,
    label = label,
    hide.ns = hide.ns,
    signif.cutoff = signif.cutoff
  )

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
      plot_data     = plot_data,
      stat_data     = facet_stat_data,
      x             = x,
      y             = y,
      facet_var     = facet_var,
      label         = label,
      hide.ns       = hide.ns,
      signif.cutoff = signif.cutoff,
      expand        = expand,
      step.increase = step.increase,
      x_levels      = x_levels
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

  theme_def <- .get_theme_defaults()
  axis_linewidth <- .get_theme_axis_linewidth()
  axis_colour <- .get_theme_axis_colour()

  text_fontsize <- size * getFromNamespace(".pt", "ggplot2")
  text_family <- theme_def$font_family
  text_face <- if (!is.null(fontface)) fontface else theme_def$font_face

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
    gp = grid::gpar(
      col = axis_colour,
      lwd = .convert_linewidth_to_lwd(line_width)
    )
  )

  layers <- list()

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
      color = axis_colour,
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
  label_band_height <- y_range * label_band_mult

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

      sig_trim <- trimws(tolower(sig))
      is_ns <- sig_trim == "ns" | grepl("^p\\s*=\\s*ns$", sig_trim)
      this_nudge_pt <- if (is_ns) ns_nudge_pt else sig_nudge_pt

      y_top  <- y_line + label_band_height
      max_cross_label_y <- max(max_cross_label_y, y_top, na.rm = TRUE)

      star_grob <- grid::grobTree(
        grid::textGrob(
          sig,
          x = 0.5,
          y = grid::unit(this_nudge_pt, "pt"),
          hjust = 0.5,
          vjust = 0,
          gp = grid::gpar(
            col = axis_colour,
            fontsize = text_fontsize,
            fontface = text_face,
            fontfamily = text_family
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
          ymin = y_line,
          ymax = y_top,
          xmin = gidx,
          xmax = length(x_levels) + 1
        )
      } else {
        layers[[length(layers) + 1]] <- annotation_custom2(
          grob = star_grob,
          data = right_data,
          ymin = y_line,
          ymax = y_top,
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

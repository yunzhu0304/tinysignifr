#' Internal utilities for tinysignifr
#'
#' Internal helper functions used by exported annotation functions.
#'
#' @name tinysignifr-internal
#' @keywords internal
NULL

.as_df <- function(data) {
  tryCatch(
    as.data.frame(data),
    error = function(e) {
      stop("`data` must be coercible to a data.frame.", call. = FALSE)
    }
  )
}

.validate_single_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    stop(
      paste0("`", arg, "` must be a single non-missing character string."),
      call. = FALSE
    )
  }
}

.validate_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop(
      paste0("`", arg, "` must be TRUE or FALSE."),
      call. = FALSE
    )
  }
}

.get_discrete_levels <- function(x, x_levels = NULL) {
  if (!is.null(x_levels)) {
    if (!is.character(x_levels) || length(x_levels) == 0 || anyNA(x_levels)) {
      stop("`x_levels` must be a non-empty character vector without NA.", call. = FALSE)
    }
    return(unique(x_levels))
  }

  if (is.factor(x)) {
    return(levels(droplevels(x)))
  }

  if (is.numeric(x)) {
    return(as.character(sort(unique(x[!is.na(x)]))))
  }

  sort(unique(as.character(x[!is.na(x)])))
}

.safe_group_max <- function(y, group) {
  tapply(
    y, group,
    function(z) {
      if (all(is.na(z))) {
        NA_real_
      } else {
        max(z, na.rm = TRUE)
      }
    }
  )
}

.filter_ns_rows <- function(stat_data,
                            label = NULL,
                            hide.ns = FALSE,
                            signif.cutoff = 0.05) {
  stat_data <- .as_df(stat_data)

  if (nrow(stat_data) == 0) {
    return(stat_data)
  }

  if (!is.numeric(signif.cutoff) || length(signif.cutoff) != 1 ||
      is.na(signif.cutoff) || !is.finite(signif.cutoff) || signif.cutoff < 0) {
    stop("`signif.cutoff` must be one non-negative finite numeric value.", call. = FALSE)
  }

  if (is.logical(hide.ns)) {
    if (length(hide.ns) != 1 || is.na(hide.ns)) {
      stop("`hide.ns` must be TRUE/FALSE or a character column specifier.", call. = FALSE)
    }
    if (!hide.ns) {
      return(stat_data)
    }
    col <- "any"
  } else if (is.character(hide.ns)) {
    if (length(hide.ns) != 1 || is.na(hide.ns)) {
      stop("`hide.ns` must be TRUE/FALSE or a character column specifier.", call. = FALSE)
    }
    col <- hide.ns
  } else {
    stop("`hide.ns` must be TRUE/FALSE or a character column specifier.", call. = FALSE)
  }

  if (!is.null(label)) {
    .validate_single_string(label, "label")
  }

  p_adj_names  <- c("p.adj", "p_adj", "adj.p", "adj_p")
  p_names      <- c("p", "p.value", "p_value", "pval", "p_val")
  p_adj_signif <- paste0(p_adj_names, ".signif")
  p_signif     <- paste0(p_names, ".signif")

  if (identical(col, "any")) {
    possible_cols <- unique(c(
      p_adj_signif,
      p_adj_names,
      p_signif,
      p_names,
      label
    ))
    possible_cols <- possible_cols[!is.na(possible_cols) & nzchar(possible_cols)]

    matched <- intersect(possible_cols, colnames(stat_data))

    if (length(matched) == 0) {
      warning(
        "No suitable column found for filtering non-significant rows; returning input unchanged.",
        call. = FALSE
      )
      return(stat_data)
    }

    col <- matched[1]
  } else {
    if (identical(col, "p")) {
      matched <- intersect(p_names, colnames(stat_data))
      if (length(matched) == 0) {
        stop("`hide.ns = 'p'` requires a p-value column in `stat_data`.", call. = FALSE)
      }
      col <- matched[1]
    } else if (identical(col, "p.adj")) {
      matched <- intersect(p_adj_names, colnames(stat_data))
      if (length(matched) == 0) {
        stop("`hide.ns = 'p.adj'` requires an adjusted p-value column in `stat_data`.", call. = FALSE)
      }
      col <- matched[1]
    } else {
      if (!col %in% colnames(stat_data)) {
        stop("Can't find the column `", col, "` in `stat_data`.", call. = FALSE)
      }
    }
  }

  values <- stat_data[[col]]

  if (is.numeric(values)) {
    keep <- !is.na(values) & values <= signif.cutoff
  } else {
    values_chr <- trimws(tolower(as.character(values)))
    is_ns <- values_chr == "ns" | grepl("^p\\s*=\\s*ns$", values_chr)
    keep <- !is.na(values) & !is_ns
  }

  stat_data[keep, , drop = FALSE]
}

.warn_if_numeric_label <- function(stat_data, label) {
  if (nrow(stat_data) == 0) {
    return(invisible(NULL))
  }

  label_vals <- stat_data[[label]]
  if (is.numeric(label_vals)) {
    warning(
      paste0(
        "`label` column (`", label, "`) appears to be numeric p-values. ",
        "Filtering will use `signif.cutoff`, and plotted labels will be numeric values unless another label column is provided."
      ),
      call. = FALSE
    )
  }

  invisible(NULL)
}

.compute_pvalue_positions <- function(plot_data,
                                      stat_data,
                                      x,
                                      y,
                                      expand = 0.12,
                                      step.increase = 0.08,
                                      x_levels = NULL,
                                      context = NULL) {
  plot_data <- .as_df(plot_data)
  stat_data <- .as_df(stat_data)

  if (nrow(stat_data) == 0) {
    return(stat_data)
  }

  y_raw <- suppressWarnings(as.numeric(plot_data[[y]]))
  if (all(is.na(y_raw))) {
    stop(
      "`y` in `plot_data` must contain at least one non-missing numeric value.",
      call. = FALSE
    )
  }

  x_raw <- plot_data[[x]]
  x_chr <- as.character(x_raw)
  group_levels <- .get_discrete_levels(x_raw, x_levels = x_levels)

  valid_x <- !is.na(x_chr)
  group_max <- .safe_group_max(y_raw[valid_x], x_chr[valid_x])

  g1 <- as.character(stat_data$group1)
  g2 <- as.character(stat_data$group2)

  valid_cmp <- g1 %in% names(group_max) &
    g2 %in% names(group_max) &
    g1 %in% group_levels &
    g2 %in% group_levels

  if (!all(valid_cmp)) {
    bad_pairs <- paste0(g1[!valid_cmp], " vs ", g2[!valid_cmp])
    bad_pairs <- unique(bad_pairs)

    warning(
      paste0(
        "Dropping comparison(s) not found in `plot_data`",
        if (!is.null(context)) paste0(" [", context, "]") else "",
        ": ",
        paste(bad_pairs, collapse = "; ")
      ),
      call. = FALSE
    )

    stat_data <- stat_data[valid_cmp, , drop = FALSE]
    g1 <- g1[valid_cmp]
    g2 <- g2[valid_cmp]
  }

  if (nrow(stat_data) == 0) {
    return(stat_data)
  }

  overall_ymax <- max(y_raw, na.rm = TRUE)
  overall_ymin <- min(y_raw, na.rm = TRUE)
  y_range <- overall_ymax - overall_ymin

  if (!is.finite(y_range) || y_range == 0) {
    y_range <- max(abs(y_raw), na.rm = TRUE)
  }
  if (!is.finite(y_range) || y_range == 0) {
    y_range <- 1
  }

  base_y <- pmax(
    unname(group_max[g1]),
    unname(group_max[g2]),
    na.rm = FALSE
  )
  base_y[is.na(base_y) | !is.finite(base_y)] <- overall_ymax

  span <- abs(match(g2, group_levels) - match(g1, group_levels))
  if (any(is.na(span))) {
    stop(
      "Failed to determine comparison span from `group1` and `group2`.",
      call. = FALSE
    )
  }

  ord <- order(span, base_y)

  first_floor <- overall_ymax + y_range * expand
  target_y <- pmax(base_y + y_range * expand, first_floor)

  y_pos <- rep(NA_real_, nrow(stat_data))
  current_y <- -Inf

  for (idx in ord) {
    current_y <- max(current_y + y_range * step.increase, target_y[idx])
    y_pos[idx] <- current_y
  }

  stat_data$y.position <- y_pos
  stat_data
}

.prepare_pvalue_facet_annotation_data <- function(plot_data,
                                                  stat_data,
                                                  x,
                                                  y,
                                                  facet_var,
                                                  label = "p.adj.signif",
                                                  hide.ns = TRUE,
                                                  signif.cutoff = 0.05,
                                                  expand = 0.12,
                                                  step.increase = 0.10,
                                                  x_levels = NULL) {
  plot_data <- .as_df(plot_data)
  stat_data <- .as_df(stat_data)

  .validate_single_string(x, "x")
  .validate_single_string(y, "y")
  .validate_single_string(facet_var, "facet_var")
  .validate_single_string(label, "label")

  if (!is.logical(hide.ns) && !is.character(hide.ns)) {
    stop("`hide.ns` must be TRUE/FALSE or a character column specifier.", call. = FALSE)
  }

  if (!is.numeric(signif.cutoff) || length(signif.cutoff) != 1 ||
      is.na(signif.cutoff) || !is.finite(signif.cutoff) || signif.cutoff < 0) {
    stop("`signif.cutoff` must be one non-negative finite numeric value.", call. = FALSE)
  }

  if (!x %in% colnames(plot_data)) {
    stop("`x` is not a column in `plot_data`.", call. = FALSE)
  }
  if (!y %in% colnames(plot_data)) {
    stop("`y` is not a column in `plot_data`.", call. = FALSE)
  }
  if (!facet_var %in% colnames(plot_data)) {
    stop("`facet_var` is not a column in `plot_data`.", call. = FALSE)
  }
  if (!facet_var %in% colnames(stat_data)) {
    stop("`facet_var` is not a column in `stat_data`.", call. = FALSE)
  }

  required_cols <- c("group1", "group2", label)
  missing_cols <- setdiff(required_cols, colnames(stat_data))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "`stat_data` is missing required column(s): ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (nrow(stat_data) == 0) {
    stat_data$y.position <- numeric(0)
    return(stat_data)
  }

  .warn_if_numeric_label(stat_data, label)
  stat_data <- .filter_ns_rows(
    stat_data = stat_data,
    label = label,
    hide.ns = hide.ns,
    signif.cutoff = signif.cutoff
  )

  if (nrow(stat_data) == 0) {
    stat_data$y.position <- numeric(0)
    return(stat_data)
  }

  fv <- plot_data[[facet_var]]
  facet_levels <- if (is.factor(fv)) {
    levels(droplevels(fv))
  } else {
    sort(unique(as.character(fv)))
  }
  facet_levels <- facet_levels[facet_levels %in% unique(as.character(stat_data[[facet_var]]))]

  out_list <- vector("list", length(facet_levels))
  names(out_list) <- facet_levels

  for (fc in facet_levels) {
    pd <- plot_data[as.character(plot_data[[facet_var]]) == fc, , drop = FALSE]
    sd <- stat_data[as.character(stat_data[[facet_var]]) == fc, , drop = FALSE]

    if (nrow(pd) == 0 || nrow(sd) == 0) {
      next
    }

    out_list[[fc]] <- .compute_pvalue_positions(
      plot_data = pd,
      stat_data = sd,
      x = x,
      y = y,
      expand = expand,
      step.increase = step.increase,
      x_levels = x_levels,
      context = paste0(facet_var, " = ", fc)
    )
  }

  out_list <- Filter(function(z) !is.null(z) && nrow(z) > 0, out_list)

  if (length(out_list) == 0) {
    stat_data <- stat_data[0, , drop = FALSE]
    stat_data$y.position <- numeric(0)
    return(stat_data)
  }

  do.call(rbind, out_list)
}

.convert_linewidth_to_lwd <- function(linewidth) {
  if (!is.numeric(linewidth) || length(linewidth) != 1 ||
      is.na(linewidth) || !is.finite(linewidth) || linewidth < 0) {
    stop("`line_width` must be one non-negative finite numeric value.", call. = FALSE)
  }
  linewidth * 2.845
}

.get_theme_defaults <- function() {
  th <- ggplot2::theme_get()

  axis_line <- th$axis.line
  line_colour <- if (!is.null(axis_line$colour)) axis_line$colour else "black"
  line_width <- if (!is.null(axis_line$linewidth)) {
    axis_line$linewidth
  } else if (!is.null(axis_line$size)) {
    axis_line$size
  } else {
    0.5
  }

  text <- th$text
  font_family <- if (!is.null(text$family)) text$family else ""
  font_face <- if (!is.null(text$face)) text$face else "plain"
  font_size <- if (!is.null(text$size)) text$size else 11

  list(
    line_colour = line_colour,
    line_width  = line_width,
    font_family = font_family,
    font_face   = font_face,
    font_size   = font_size
  )
}

.get_theme_axis_linewidth <- function() {
  .get_theme_defaults()$line_width
}

.get_theme_axis_colour <- function() {
  .get_theme_defaults()$line_colour
}


annotation_custom2 <- function(grob,
                               xmin = -Inf, xmax = Inf,
                               ymin = -Inf, ymax = Inf,
                               data) {
  ggplot2::layer(
    data = data,
    stat = getFromNamespace("StatIdentity", "ggplot2"),
    position = getFromNamespace("PositionIdentity", "ggplot2"),
    geom = getFromNamespace("GeomCustomAnn", "ggplot2"),
    inherit.aes = FALSE,
    params = list(
      grob = grob,
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    )
  )
}

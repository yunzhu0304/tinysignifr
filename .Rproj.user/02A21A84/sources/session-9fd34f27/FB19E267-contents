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

.filter_ns_rows <- function(stat_data, label, hide.ns) {
  if (!hide.ns || nrow(stat_data) == 0) {
    return(stat_data)
  }

  keep <- !is.na(stat_data[[label]]) &
    tolower(as.character(stat_data[[label]])) != "ns"

  stat_data[keep, , drop = FALSE]
}

.warn_if_numeric_label <- function(stat_data, label) {
  if (nrow(stat_data) == 0) return(invisible(NULL))

  label_vals <- stat_data[[label]]
  if (is.numeric(label_vals)) {
    warning(
      paste0(
        "`label` column (`", label, "`) appears to be numeric p-values. ",
        "`hide.ns` filtering expects significance labels such as 'ns', '*', '**'."
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
                                                  expand = 0.12,
                                                  step.increase = 0.10,
                                                  x_levels = NULL) {
  .validate_single_string(x, "x")
  .validate_single_string(y, "y")
  .validate_single_string(facet_var, "facet_var")
  .validate_single_string(label, "label")
  .validate_logical_scalar(hide.ns, "hide.ns")

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
  stat_data <- .filter_ns_rows(stat_data, label, hide.ns)

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
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax
    )
  )
}


#' Add p-value brackets with automatic size adjustment for mixed symbol types
#'
#' A wrapper around [ggpubr::stat_pvalue_manual()] that automatically computes
#' bracket y-positions and renders user-defined *special* significance symbols
#' (e.g. `"#"`, `"$"`) at a reduced font size so that they appear visually
#' consistent with standard star symbols (`"*"`, `"**"`, etc.).
#'
#' This is useful when `stat_data` contains labels from different symbol
#' systems, such as `"*"` from one test and `"#"` from another.
#'
#' @param plot_data A data frame (or object coercible to one) containing the
#'   raw data used for plotting. Must include the columns named by `x` and `y`.
#' @param stat_data A data frame (or object coercible to one) containing
#'   pairwise comparison results. Must contain columns `group1`, `group2`, and
#'   the column specified by `label`. Typically the `@stat` slot from
#'   [tinystatr::stat3()] or [tinystatr::stat2()].
#' @param x A single character string giving the name of the discrete grouping
#'   variable (x-axis) in `plot_data`.
#' @param y A single character string giving the name of the numeric response
#'   variable (y-axis) in `plot_data`.
#' @param label A single character string giving the name of the column in
#'   `stat_data` that contains significance labels. Defaults to
#'   `"p.adj.signif"`.
#' @param hide.ns Controls removal of non-significant rows before bracket
#'   positions are computed. See [add_pvalue_annotation()] for accepted values.
#' @param signif.cutoff A single non-negative finite numeric value used as the
#'   threshold when filtering numeric significance columns via `hide.ns`.
#'   Defaults to `0.05`.
#' @param expand A single non-negative numeric value controlling the gap between
#'   the top of the data and the first bracket (fraction of y range). Defaults
#'   to `0.12`.
#' @param step.increase A single non-negative numeric value controlling the
#'   vertical step between successive brackets (fraction of y range). Defaults
#'   to `0.08`.
#' @param size Font size for significance labels. Defaults to `6`.
#' @param bracket.size Line width of the significance brackets. Defaults to `1`.
#' @param tip.length Length of the bracket tips. Defaults to `0`.
#' @param special_symbols A character vector of label prefixes that should be
#'   rendered at a reduced font size (see `special_size_ratio`). A label is
#'   treated as special if it starts with any of the listed prefixes.
#'   Accepted values:
#'   \itemize{
#'     \item `NULL` (default): only `"#"` is treated as special.
#'     \item A character vector such as `c("#", "$", "%", "@", "&")`.
#'     \item `NA` or `character(0)`: disables special handling; all labels are
#'       rendered at the same size.
#'   }
#' @param special_size_ratio A single numeric value in `(0, 1]` giving the
#'   scaling factor applied to `size` for special-symbol labels. Defaults to
#'   `0.65`.
#' @param ... Additional arguments passed to [ggpubr::stat_pvalue_manual()],
#'   such as `fontface = "bold"`.
#' @param x_levels An optional character vector specifying the order of groups
#'   on the x-axis. When `NULL` (default), the order is inferred from
#'   `plot_data[[x]]`.
#'
#' @return A ggplot2 layer, or a list of two layers when both normal and special
#'   labels are present, that can be added to a `ggplot` object with `+`.
#'   Returns `NULL` when there are no brackets to draw after filtering.
#'
#' @details
#' When `special_symbols` is active, `stat_data` is split into two subsets and
#' [ggpubr::stat_pvalue_manual()] is called twice: once at full `size` for
#' standard labels and once at `size * special_size_ratio` for special-symbol
#' labels. Both subsets share the same y-positions computed from the filtered
#' `stat_data`.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(tinystatr)
#' library(tinysignifr)
#'
#' data("ToothGrowth")
#' ToothGrowth$dose <- factor(ToothGrowth$dose)
#'
#' stat_df <- stat3(
#'   data = ToothGrowth,
#'   group = "dose",
#'   value = "len",
#'   formula = len ~ dose
#' )@stat
#'
#' stat_df$p.adj.signif[3] <- "#"
#'
#' ggplot(ToothGrowth, aes(x = dose, y = len)) +
#'   geom_boxplot(aes(color = dose), fill = NA) +
#'   theme_classic() +
#'   theme(legend.position = "none") +
#'   add_pvalue_mixed(
#'     plot_data = ToothGrowth,
#'     stat_data = stat_df,
#'     x = "dose",
#'     y = "len",
#'     hide.ns = TRUE,
#'     special_symbols = "#",
#'     special_size_ratio = 0.65
#'   )
#' }
#'
#' @seealso [add_pvalue_annotation()], [add_pvalue_facet_annotation()],
#'   [ggpubr::stat_pvalue_manual()]
#'
#' @export

add_pvalue_mixed <- function(plot_data,
                             stat_data,
                             x,
                             y,
                             label              = "p.adj.signif",
                             hide.ns            = FALSE,
                             signif.cutoff      = 0.05,
                             expand             = 0.12,
                             step.increase      = 0.08,
                             size               = 6,
                             bracket.size       = 1,
                             tip.length         = 0,
                             special_symbols    = NULL,
                             special_size_ratio = 0.65,
                             ...,
                             x_levels           = NULL) {

  plot_data <- .as_df(plot_data)
  stat_data <- .as_df(stat_data)

  .validate_single_string(x, "x")
  .validate_single_string(y, "y")
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

  required_cols <- c("group1", "group2", label)
  missing_cols  <- setdiff(required_cols, colnames(stat_data))
  if (length(missing_cols) > 0) {
    stop(
      paste0("`stat_data` is missing required column(s): ",
             paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  if (nrow(stat_data) == 0) {
    return(NULL)
  }

  .warn_if_numeric_label(stat_data, label)

  stat_data <- .filter_ns_rows(
    stat_data     = stat_data,
    label         = label,
    hide.ns       = hide.ns,
    signif.cutoff = signif.cutoff
  )

  if (nrow(stat_data) == 0) {
    return(NULL)
  }

  stat_data <- .compute_pvalue_positions(
    plot_data     = plot_data,
    stat_data     = stat_data,
    x             = x,
    y             = y,
    expand        = expand,
    step.increase = step.increase,
    x_levels      = x_levels
  )

  if (nrow(stat_data) == 0) {
    return(NULL)
  }

  .escape_regex <- function(x) {
    special_chars <- c(".", "\\", "^", "$", "*", "+", "?", "(", ")", "[", "]")
    for (ch in special_chars) {
      x <- gsub(ch, paste0("\\", ch), x, fixed = TRUE)
    }
    x
  }

  if (is.null(special_symbols)) {
    special_symbols <- "#"
  } else if ((length(special_symbols) == 1 && is.na(special_symbols)) ||
             length(special_symbols) == 0) {
    special_symbols <- character(0)
  } else {
    if (!is.character(special_symbols)) {
      stop("`special_symbols` must be a character vector, NA, or NULL.", call. = FALSE)
    }
    special_symbols <- unique(special_symbols)
  }

  if (!is.numeric(special_size_ratio) || length(special_size_ratio) != 1 ||
      is.na(special_size_ratio) || special_size_ratio <= 0 || special_size_ratio > 1) {
    stop("`special_size_ratio` must be a single numeric value between 0 and 1.",
         call. = FALSE)
  }

  if (length(special_symbols) > 0) {
    escaped    <- vapply(special_symbols, .escape_regex, character(1), USE.NAMES = FALSE)
    pattern    <- paste0("^(", paste(escaped, collapse = "|"), ")")
    is_special <- grepl(pattern, as.character(stat_data[[label]]))
  } else {
    is_special <- rep(FALSE, nrow(stat_data))
  }

  stat_normal  <- stat_data[!is_special, , drop = FALSE]
  stat_special <- stat_data[ is_special, , drop = FALSE]

  out <- list()

  if (nrow(stat_normal) > 0) {
    out[[1]] <- ggpubr::stat_pvalue_manual(
      stat_normal,
      label        = label,
      y.position   = "y.position",
      size         = size,
      bracket.size = bracket.size,
      tip.length   = tip.length,
      hide.ns      = hide.ns,
      ...
    )
  }

  if (nrow(stat_special) > 0) {
    out[[2]] <- ggpubr::stat_pvalue_manual(
      stat_special,
      label        = label,
      y.position   = "y.position",
      size         = size * special_size_ratio,
      bracket.size = bracket.size,
      tip.length   = tip.length,
      hide.ns      = hide.ns,
      ...
    )
  }

  out <- Filter(Negate(is.null), out)

  if (length(out) == 0) {
    return(NULL)
  }
  if (length(out) == 1) {
    return(out[[1]])
  }

  out
}

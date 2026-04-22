# tinysignifr <img src="man/figures/logo.png" align="right" height="139" alt="" />

> Automatic significance bracket annotation for `ggplot2` — with full support
> for faceted plots, cross-facet comparisons, and mixed symbol styles.

---

## Overview

`tinysignifr` is designed to work directly with the `@stat` output of
[`tinystatr`](https://github.com/yunzhu0304/tinystatr), turning statistical
results into publication-ready significance annotations in a single function
call. All y-axis bracket positions are computed automatically — no manual
calculation needed.

| Function | Use case |
|---|---|
| `add_pvalue_annotation()` | Single-panel plots with standard `*` labels |
| `add_pvalue_mixed()` | Single-panel plots with **mixed** symbol types (e.g. `*` and `#`) |
| `add_pvalue_facet_annotation()` | **Within-facet** brackets in a faceted plot |
| `add_cross_facet_annotation()` | **Cross-facet** lines + broken x-axis |

---

## Installation

```r
# Install tinystatr first (required for statistical computation)
if (!require("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("yunzhu0304/tinystatr")

# Install tinysignifr
devtools::install_github("yunzhu0304/tinysignifr")
```

---

## Required packages

```r
library(tinystatr)
library(tinysignifr)
library(ggplot2)
library(dplyr)
```

---

## Workflow

```
Raw data
   │
   ▼
stat3() / stat2()          ← tinystatr: auto-selects statistical method
   │
   └── @stat slot          ← data frame of pairwise comparison results
          │
          ▼
   add_pvalue_annotation()
   add_pvalue_mixed()       ← tinysignifr: adds brackets to ggplot
   add_pvalue_facet_annotation()
   add_cross_facet_annotation()
```

> **Critical rule**: All `tinysignifr` functions must be added **after**
> `theme()` in the ggplot call chain. `add_cross_facet_annotation()` must
> always be the **very last** layer, as it injects `coord_cartesian()` and
> `theme()` modifications that must not be overridden.

---

## 1. `add_pvalue_annotation()` — Single-panel plot

### Statistical analysis with `tinystatr`

```r
data("ToothGrowth")
ToothGrowth$dose <- factor(ToothGrowth$dose)

# stat3() automatically selects the appropriate statistical method
stat_result <- stat3(
  data    = ToothGrowth,
  group   = "dose",
  value   = "len",
  formula = len ~ dose
)

# All groups have 3 or more samples.
# Normally distributed
# Variance equal
# Anova

# Extract the @stat slot — this is the input for tinysignifr
stat_df <- stat_result@stat
stat_df
#   group1 group2        p.adj posthoc variable           p1 P1method p.adj.signif
# 1    0.5      1 1.268342e-07     hsd       id 9.532777e-16    ANOVA         ****
# 2    0.5      2 4.398450e-14     hsd       id 9.532777e-16    ANOVA         ****
# 3      1      2 1.378340e-04     hsd       id 9.532777e-16    ANOVA         ***
```

### Plot with significance brackets

```r
ggplot(ToothGrowth, aes(x = dose, y = len)) +
  geom_boxplot(aes(color = dose), fill = NA, linewidth = 1) +
  geom_jitter(aes(color = dose), width = 0.15, size = 2,
              alpha = 0.6, show.legend = FALSE) +
  scale_color_manual(values = c("#56B4E9", "#E69F00", "#009E73")) +
  labs(x = "Dose (mg)", y = "Tooth length") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  # ── tinysignifr must come after theme() ────────────────────────
  add_pvalue_annotation(
    plot_data     = ToothGrowth,
    stat_data     = stat_df,        # @stat from stat3()
    x             = "dose",
    y             = "len",
    label         = "p.adj.signif",
    hide.ns       = TRUE,
    expand        = 0.12,
    step.increase = 0.10,
    size          = 5,
    bracket.size  = 1,
    tip.length    = 0,
    fontface      = "bold"
  )
```

### Key parameters

| Parameter | Default | Description |
|---|---|---|
| `hide.ns` | `FALSE` | `TRUE` removes `"ns"` rows; `"p.adj"` filters by adjusted p-value |
| `signif.cutoff` | `0.05` | Threshold used when `hide.ns` targets a numeric column |
| `expand` | `0.12` | Gap above data to first bracket (fraction of y range) |
| `step.increase` | `0.08` | Vertical step between brackets (fraction of y range) |
| `x_levels` | `NULL` | Explicit group order; inferred from factor levels when `NULL` |

### `hide.ns` — flexible non-significant filtering

All four functions share the same `hide.ns` interface:

```r
hide.ns = FALSE          # keep everything (default for add_pvalue_annotation)
hide.ns = TRUE           # remove rows where label column equals "ns"
hide.ns = "p"            # remove rows where p column > signif.cutoff
hide.ns = "p.adj"        # remove rows where p.adj column > signif.cutoff
hide.ns = "my_col"       # remove rows where custom column > signif.cutoff
signif.cutoff = 0.01     # change the threshold (default 0.05)
```

---

## 2. `add_pvalue_mixed()` — Mixed symbol types

Use this when `stat_data` contains both standard star labels (`*`, `**`) **and**
alternative symbols such as `#` from a one-sided test or a secondary comparison.
Alternative symbols are automatically rendered at a smaller font size to visually
match `*`.

### Why mixed symbols appear

A common pattern is to use `*` for two-sided adjusted p-values and `#` for
one-sided p-values:

```r
# After running stat3(), add one-sided annotation
stat_df$p.adj.signif <- ifelse(
  stat_df$p.adj.signif == "ns" & stat_df$p.oneside < 0.05,
  "#",
  stat_df$p.adj.signif
)

# Demo
stat_df[1,8] <- "#"
```

### Plot with mixed symbols

```r
ggplot(ToothGrowth, aes(x = dose, y = len)) +
  geom_boxplot(aes(color = dose), fill = NA, linewidth = 1) +
  geom_jitter(aes(color = dose), width = 0.15, size = 2,
              alpha = 0.6, show.legend = FALSE) +
  scale_color_manual(values = c("#56B4E9", "#E69F00", "#009E73")) +
  labs(x = "Dose (mg)", y = "Tooth length") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  # ── tinysignifr must come after theme() ────────────────────────
  add_pvalue_mixed(
    plot_data          = ToothGrowth,
    stat_data          = stat_df,
    x                  = "dose",
    y                  = "len",
    label              = "p.adj.signif",
    hide.ns            = TRUE,
    size               = 5,
    special_symbols    = "#",    # rendered at reduced font size
    special_size_ratio = 0.65    # 65% of `size`
  )
```

### `special_symbols` accepted values

```r
special_symbols = NULL               # default: only "#" is reduced
special_symbols = c("#", "$", "%")   # multiple custom symbols
special_symbols = NA                 # disable — all labels use the same size
```

---

## 3. Faceted plots — `add_pvalue_facet_annotation()` + `add_cross_facet_annotation()`

These two functions are **designed to work together**:

- `add_pvalue_facet_annotation()` draws brackets **within** each facet panel
- `add_cross_facet_annotation()` draws lines **across** facet panels, adds a
  broken x-axis gap, and uses `facet_stat_data` to prevent overlap

```
 facet: OJ                       facet: VC
 ┌─────────────────┐              ┌─────────────────┐
 │      **         │◄─cross-facet─►│      *          │
 │   ┌──────┐      │    line       │   ┌──────┐      │
 │   │violin│      │               │   │violin│      │
 └─────────────────┘              └─────────────────┘
   0.5   1   2                      0.5   1   2
    ↑ within-facet brackets ↑
```

### Statistical analysis with `tinystatr`

```r
data("ToothGrowth")
ToothGrowth$dose <- factor(ToothGrowth$dose, levels = c("0.5", "1", "2"))
ToothGrowth$supp <- factor(ToothGrowth$supp, levels = c("OJ", "VC"))

# ── Within-facet: compare dose levels within each supp group ──────
# Use variable/id to filter within stat3()
stat_oj <- stat3(
  data     = ToothGrowth,
  group    = "dose",
  value    = "len",
  variable = "supp",
  id       = "OJ",
  formula  = len ~ dose
)

stat_vc <- stat3(
  data     = ToothGrowth,
  group    = "dose",
  value    = "len",
  variable = "supp",
  id       = "VC",
  formula  = len ~ dose
)

# Combine: add the facet column so tinysignifr knows which panel each row belongs to
stat_within <- rbind(
  stat_oj@stat %>% mutate(supp = "OJ"),
  stat_vc@stat %>% mutate(supp = "VC")
)

# ── Cross-facet: compare OJ vs VC within each dose level ──────────
stat_cross_list <- lapply(levels(ToothGrowth$dose), function(d) {
  df_sub <- ToothGrowth %>% filter(dose == d)
  res <- stat3(
    data    = df_sub,
    group   = "supp",
    value   = "len",
    formula = len ~ supp
  )
  res@stat %>% mutate(dose_group = d)   # dose_group = group_col in the plot call
})

stat_cross <- do.call(rbind, stat_cross_list)
```

### Combined faceted plot

```r
my_palette <- c("0.5" = "#56B4E9", "1" = "#009E73", "2" = "#E69F00")

ggplot(ToothGrowth, aes(x = dose, y = len)) +
  geom_violin(aes(color = dose), fill = NA, linewidth = 1, width = 0.8) +
  geom_jitter(aes(color = dose), alpha = 0.5,
              width = 0.15, size = 2, height = 0,
              show.legend = FALSE) +
  stat_summary(geom = "errorbar", fun.data = "mean_se",
               color = "black", width = 0.2, linewidth = 1) +
  stat_summary(fun = "mean", geom = "point",
               color = "black", size = 2) +
  facet_wrap(~ supp, strip.position = "bottom") +
  scale_color_manual(values = my_palette) +
  scale_x_discrete(expand = expansion(add = 0.7)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  labs(x = "", y = "Tooth length", title = "OJ vs VC by dose") +
  theme_classic(base_size = 14) +
  theme(
    legend.position  = "none",
    strip.placement  = "outside",
    strip.background = element_rect(color = NA),
    strip.text       = element_text(size = 14, face = "bold")
  ) +
  # ── Step 1: within-facet brackets (after theme) ────────────────
  add_pvalue_facet_annotation(
    plot_data     = ToothGrowth,
    stat_data     = stat_within,    # combined @stat with supp column
    x             = "dose",
    y             = "len",
    facet_var     = "supp",         # must match facet_wrap variable
    label         = "p.adj.signif",
    hide.ns       = TRUE,
    expand        = 0.12,
    step.increase = 0.10,
    size          = 6,
    bracket.size  = 1,
    tip.length    = 0,
    fontface      = "bold"
  ) +
  # ── Step 2: cross-facet lines + broken x-axis (must be LAST) ───
  add_cross_facet_annotation(
    plot_data       = ToothGrowth,
    cross_data      = stat_cross,
    x               = "dose",
    y               = "len",
    facet_var       = "supp",
    group_col       = "dose_group",   # column in stat_cross for x-group identity
    label           = "p.adj.signif",
    hide.ns         = TRUE,
    facet_stat_data = stat_within,    # used to place cross-facet lines above within-facet brackets
    expand          = 0.12,
    step.increase   = 0.10,
    size            = 6,
    line_width      = 1,
    axis_extend     = 0.45            # gap width; should be < scale_x_discrete expand value
  )
```

### How `facet_stat_data` prevents overlap

```r
# Without facet_stat_data — cross-facet lines may overlap within-facet brackets
add_cross_facet_annotation(..., facet_stat_data = NULL)

# With facet_stat_data — cross-facet lines automatically placed above
add_cross_facet_annotation(..., facet_stat_data = stat_within)

# Manual override — bypass automatic calculation entirely
add_cross_facet_annotation(..., y_base = 35)
```

---

## 4. Facet panel order and `facet_levels`

`facet_levels` controls which panel is "left" and which is "right" for
cross-facet line drawing. Always set the facet variable as an **ordered factor**:

```r
# Set before plotting — all three functions will use this order
df$supp <- factor(df$supp, levels = c("OJ", "VC"))

# facet_levels defaults to NULL (auto-detected from factor levels)
# Manual override when needed:
add_cross_facet_annotation(
  ...,
  facet_levels = c("OJ", "VC")   # left panel = OJ, right panel = VC
)
```

---

## 5. Tuning the broken x-axis gap

`axis_extend` controls how far the x-axis line extends toward the panel gap.
A smaller value creates a wider visible gap:

```r
# axis_extend should be slightly smaller than the scale_x_discrete expand value
scale_x_discrete(expand = expansion(add = 0.7))   # ggplot layer
axis_extend = 0.45                                 # add_cross_facet_annotation

# Default values by number of x groups:
# ≤ 2 groups → axis_extend = 0.65
# 3 groups   → axis_extend = 0.50
# ≥ 4 groups → axis_extend = 0.40
```

---

## 6. Complete parameter reference

### `add_pvalue_annotation()`

```r
add_pvalue_annotation(
  plot_data,                   # raw plotting data
  stat_data,                   # @stat from stat3() / stat2()
  x,                           # x-axis column name
  y,                           # y-axis column name
  label         = "p.adj.signif",
  hide.ns       = FALSE,
  signif.cutoff = 0.05,
  expand        = 0.12,
  step.increase = 0.08,
  size          = 6,
  bracket.size  = 1,
  tip.length    = 0,
  ...,                         # passed to ggpubr::stat_pvalue_manual()
  x_levels      = NULL
)
```

### `add_pvalue_mixed()`

```r
add_pvalue_mixed(
  plot_data,
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
  special_symbols    = NULL,   # "#" by default; NA to disable
  special_size_ratio = 0.65,   # size multiplier for special symbols
  ...,
  x_levels           = NULL
)
```

### `add_pvalue_facet_annotation()`

```r
add_pvalue_facet_annotation(
  plot_data,
  stat_data,             # must contain a column named by facet_var
  x,
  y,
  facet_var,             # facet column name; must match facet_wrap variable
  label         = "p.adj.signif",
  hide.ns       = TRUE,
  signif.cutoff = 0.05,
  expand        = 0.12,
  step.increase = 0.10,
  size          = 8,
  bracket.size  = 1,
  tip.length    = 0,
  ...,
  x_levels      = NULL
)
```

### `add_cross_facet_annotation()`

```r
add_cross_facet_annotation(
  plot_data,
  cross_data,                  # cross-facet comparison results (@stat + group_col)
  x,
  y,
  facet_var,
  facet_levels    = NULL,      # auto from factor levels; or c("OJ", "VC")
  group_col       = "group",   # column in cross_data identifying the x-group
  label           = "p.adj.signif",
  hide.ns         = TRUE,
  signif.cutoff   = 0.05,
  x_levels        = NULL,
  y_base          = NULL,      # manual y baseline override
  facet_stat_data = NULL,      # within-facet @stat for overlap prevention
  expand          = 0.12,
  step.increase   = 0.10,
  size            = 6,
  fontface        = "bold",
  line_width      = 1,
  sig_offset      = NULL,
  ns_offset       = NULL,
  axis_extend     = NULL,      # auto by n_groups; or manual numeric
  auto_panel_fix  = TRUE,      # adds panel.spacing.x=0 and removes axis.line.x
  auto_ylim       = TRUE,      # expands y-axis to fit all annotations
  ylim_top_expand = 0.002
)
```

---

## 7. Common issues and solutions

**Cross-facet lines have a gap in the middle.**
`panel.spacing.x` is not zero. Set `auto_panel_fix = TRUE` (default) in
`add_cross_facet_annotation()`, or manually add
`theme(panel.spacing.x = unit(0, "cm"))` before calling the function.

**Brackets are hidden behind violins or bars.**
Increase the top expansion of `scale_y_continuous`, e.g.
`expansion(mult = c(0.05, 0.3))`, or rely on `auto_ylim = TRUE` (default).

**`Error: Both facets must contain data in 'plot_data'`.**
One of the `facet_levels` values does not exist in `plot_data`. Check that
`facet_levels` matches the actual values in the facet column exactly.

**`#` appears much larger than `*`.**
Use `add_pvalue_mixed()` instead of `add_pvalue_annotation()`. It renders
`#` at `size * special_size_ratio` to compensate for its larger typographic
footprint.

**`add_cross_facet_annotation()` overrides my `coord_cartesian()`.**
This function always adds `coord_cartesian(clip = "off")` as the last layer.
Do not add another `coord_cartesian()` in the same plot. Use
`auto_ylim = FALSE` if you need manual y-axis control, then the function will
add only `coord_cartesian(clip = "off")` without setting `ylim`.

**Within-facet brackets and cross-facet lines overlap.**
Pass the within-facet `@stat` data frame to `facet_stat_data` in
`add_cross_facet_annotation()`. The function will automatically place
cross-facet lines above the highest within-facet bracket.

---

## See also

- [`tinystatr`](https://github.com/yunzhu0304/tinystatr) — automated
  statistical method selection that produces the `@stat` input for `tinysignifr`
- [`ggpubr::stat_pvalue_manual()`](https://rpkgs.datanovia.com/ggpubr/reference/stat_pvalue_manual.html) — the underlying bracket rendering engine used by `add_pvalue_annotation()`, `add_pvalue_mixed()`, and `add_pvalue_facet_annotation()`

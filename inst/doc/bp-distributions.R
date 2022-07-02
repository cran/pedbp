## ----label = "setup", include = FALSE-----------------------------------------
knitr::opts_chunk$set(collapse = TRUE, fig.align = "center")
library(pedbp)

## ----echo = FALSE, results = "asis"-------------------------------------------
cat(paste0("<img src=\"", normalizePath(system.file("images", "flowchart.png", package = "pedbp")), "\">\n"))

## ----Distribution Function----------------------------------------------------
args(p_bp)

# Quantile Function
args(q_bp)

## -----------------------------------------------------------------------------
p_bp(q_sbp = 100, q_dbp = 60, age = 44, male = 1)

## -----------------------------------------------------------------------------
p_bp(q_sbp = 100, q_dbp = 60, age = 44, male = 1, height = 183)

## -----------------------------------------------------------------------------
ht <- q_stature_for_age(p = 0.90, age = 44, male = 1)
ht

p_bp(q_sbp = 100, q_dbp = 60, age = 44, male = 1, height = ht)

## ----fig.width = 5, fig.height = 5--------------------------------------------
bp_cdf(age = 44, male = 1, height = ht, sbp = 100, dbp = 60)

## -----------------------------------------------------------------------------
bps <-
  p_bp(
         q_sbp  = c(100, NA, 90)
       , q_dbp  = c(60, 82, 48)
       , age    = 44
       , male   = 1
       , height = ht
      )
bps

## -----------------------------------------------------------------------------
attr(bps, "bp_params")
str(bps)

## -----------------------------------------------------------------------------
q_bp(
       p_sbp = c(0.701, NA, 0.36)
     , p_dbp = c(0.85, 0.99, 0.50)
     , age = 44
     , male = 1
     , height = ht
    )

## ----label = "bp_batch_example"-----------------------------------------------
eg_data <- read.csv(system.file("example_data", "for_batch.csv", package = "pedbp"))
eg_data

bp_percentiles <-
  p_bp(
         q_sbp  = eg_data$sbp..mmHg.
       , q_dbp  = eg_data$dbp..mmHg.
       , age    = eg_data$age
       , male   = eg_data$male
       , height = eg_data$height
       )
bp_percentiles

str(bp_percentiles)

## -----------------------------------------------------------------------------
q_bp(
       p_sbp  = bp_percentiles$sbp_percentile
     , p_dbp  = bp_percentiles$dbp_percentile
     , age    = eg_data$age
     , male   = eg_data$male
     , height = eg_data$height
     )

## ----label = "chart1_setup", echo = FALSE-------------------------------------
percentile_factor <- function(p) {
  factor(p, levels = sort(unique(p)), labels = paste0(sort(unique(p)) * 100, "th"))
}

q_bp_with_source <- function(bpp, age, male, height = NA, height_percentile = 0.5) {
  d <- q_bp(bpp, bpp, age, male, height, height_percentile)
  data.table::data.table(source = attr(d, "bp_params")$source, sbp = d$sbp, dbp = d$dbp)
}

d <- data.table::CJ(male = 0:1
                    , age = seq(min(bp_parameters$age), max(bp_parameters$age), by = 0.5)
                    # , age = unique(bp_parameters$age)
                    , bp_percentile = c(5, 10, 25, 50, 75, 90, 95) / 100)

d <- d[, as.list(q_bp_with_source(bp_percentile, age, male))
       , by = .(male, age, bp_percentile)]

d <- data.table::melt(d
                      , id.vars = c("source", "male", "age", "bp_percentile")
                      , measure.vars = c("sbp", "dbp")
                      )
d[, variable := factor(variable, c("sbp", "dbp"), c("Systolic", "Diastolic"))]
d[, bp_percentile := percentile_factor(bp_percentile)]

# d[, .(range(age)), by = .(source)]
bkgrnd <-
  data.table::data.table(
      source = c("Gemelli", "NHLBI", "Lo")
    , xmin   = c(0, 12, 36)
    , xmax   = c(12, 36, 204)
    , ymin   = rep(-Inf, 3)
    , ymax   = rep(Inf, 3)
    )

g <- function(d) {
  ggplot2::ggplot(d) +
    ggplot2::theme_bw() +
    ggplot2::aes(x = age, y = value, linetype = variable, color = bp_percentile) +
    ggplot2::geom_rect(data = bkgrnd
                       , inherit.aes = FALSE
                       , alpha = 0.3
                       , mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = source)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid( ~ factor(male, 0:1, c("Female", "Male"))) +
    ggplot2::scale_x_continuous(name = "Age"
                                , breaks = seq(0, max(d$age) + 12, by = 12)
                                , labels = paste(
                                                   paste0(seq(0, max(d$age) + 12, by = 12), "m")
                                                 , paste0(seq(0, max(d$age) + 12, by = 12) / 12, "yr")
                                                 , sep = "\n")
                                ) +
    ggplot2::scale_y_continuous(name = "mmHg",breaks=c(0, 30, 60, 90, 120), limits = c(15, 140)) +
    ggplot2::scale_linetype(name = "") +
    ggplot2::scale_color_hue(name = "BP\nPercentile") +
    ggplot2::scale_fill_manual(name = "Data\nSource", values = c("Gemelli" = ggplot2::alpha("#236192", 0.5)
                                                                , "NHLBI"  = ggplot2::alpha("#6F263D", 0.5)
                                                                , "Lo"     = ggplot2::alpha("#A2AAAD", 0.5)
                                                                )) +
    ggplot2::guides(
                    linetype = ggplot2::guide_legend(order = 1, ncol = 1),
                    fill     = ggplot2::guide_legend(order = 2, ncol = 1),
                    color    = ggplot2::guide_legend(order = 3, ncol = 4)
                    ) +
    ggplot2::theme(
                   legend.position = "bottom"
                   )
}

## ----label = "chart1_female", echo = FALSE, fig.width = 8, fig.height = 6-----
g(d[male == 0])

## ----label = "chart1_male", echo = FALSE, fig.width = 8, fig.height = 6-------
g(d[male == 1])

## ----label = "chart2_setup", echo = FALSE, results = FALSE--------------------
d <- data.table::CJ(male = 0:1
                    # , age = unique(bp_parameters$age)
                    , age = seq(min(bp_parameters$age), max(bp_parameters$age), by = 0.5)
                    , bp_percentile = 50 / 100
                    , height_percentile = unique(na.omit(bp_parameters$height_percentile)) / 100)
d <- d[, as.list(q_bp_with_source(bp_percentile, age, male, height_percentile = height_percentile))
       , by = .(male, age, bp_percentile, height_percentile)]

d <- data.table::melt(d
                      , id.vars = c("source", "male", "age", "bp_percentile","height_percentile")
                      , measure.vars = c("sbp", "dbp")
                      )
d[, variable := factor(variable, c("sbp", "dbp"), c("Systolic", "Diastolic"))]
d[, height_percentile := percentile_factor(height_percentile)]
d <- subset(d, age <= 84)
d[, range(age), by = .(source)]
bkgrnd <-
  data.table::data.table(
      source = c("Gemelli", "NHLBI", "Lo")
    , xmin   = c(0, 12, 36)
    , xmax   = c(12, 36, max(d$age))
    , ymin   = rep(-Inf, 3)
    , ymax   = rep(Inf, 3)
    )

g <- function(d) {
  ggplot2::ggplot(d) +
    ggplot2::theme_bw() +
    ggplot2::aes(x = age, y = value, linetype = variable, color = height_percentile) +
    ggplot2::geom_rect(data = bkgrnd
                       , inherit.aes = FALSE
                       , alpha = 0.3
                       , mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = source)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid( ~ factor(male, 0:1, c("Female", "Male"))) +
    ggplot2::scale_x_continuous(name = "Age"
                                , breaks = seq(0, 84, by = 12)
                                , labels = paste(
                                                   paste0(seq(0, 84, by = 12), "m")
                                                 , paste0(seq(0, 84, by = 12) / 12, "yr")
                                                 , sep = "\n")
                                ) +
    ggplot2::scale_y_continuous(name = "Median BP (mmHg)",breaks=c(0, 30, 60, 90, 120), limits = c(15, 140)) +
    ggplot2::scale_linetype(name = "") +
    ggplot2::scale_color_hue(name = "Height\nPercentile") +
    ggplot2::scale_fill_manual(name = "Data\nSource", values = c("Gemelli" = ggplot2::alpha("#236192", 0.5)
                                                                , "NHLBI"  = ggplot2::alpha("#6F263D", 0.5)
                                                                , "Lo"     = ggplot2::alpha("#A2AAAD", 0.5)
                                                                )) +
    ggplot2::guides(
                    linetype = ggplot2::guide_legend(order = 1, ncol = 1),
                    fill     = ggplot2::guide_legend(order = 2, ncol = 1),
                    color    = ggplot2::guide_legend(order = 3, ncol = 4)
                    ) +
    ggplot2::theme(
                   legend.position = "bottom"
                   )
}

## ----label = "chart2_female", echo = FALSE, fig.width = 8, fig.height = 6-----
g(d[male == 0])

## ----label = "chart2_male", echo = FALSE, fig.width = 8, fig.height = 6-------
g(d[male == 1])

## ----label = "chart3", echo = FALSE, results = "hide"-------------------------
d <- data.table::CJ(male = 0:1
                    , age = seq(min(bp_parameters$age), max(bp_parameters$age), by = 0.5)
                    , bp_percentile = 50 / 100
                    , height_percentile = unique(na.omit(bp_parameters$height_percentile)) / 100)

d[, height := NA_real_]
d[age < 36, height := q_length_for_age_inf(height_percentile, age, male), by = .(male, age, height_percentile)]
d[age >= 36, height := q_stature_for_age(height_percentile, age, male), by = .(male, age, height_percentile)]


d <-
  d[, as.list(q_bp_with_source(bp_percentile, age, male, height)) , by = .(male, age, bp_percentile, height_percentile)]

d <- data.table::melt(d
                      , id.vars = c("source", "male", "age", "bp_percentile", "height_percentile")
                      , measure.vars = c("sbp", "dbp")
                      )

d[, variable := factor(variable, c("sbp", "dbp"), c("Systolic", "Diastolic"))]
d[, height_percentile := percentile_factor(height_percentile)]

d[, .(xmin = min(age), xmax = max(age)), by = .(source)]
bkgrnd <- data.table::data.table(source = c("Gemelli", "NHLBI"), xmin = c(0, 12), xmax = c(12, max(d$age)), ymin = c(-Inf, -Inf), ymax = c(Inf, Inf))

g <- function(d) {
  ggplot2::ggplot(d) +
    ggplot2::theme_bw() +
    ggplot2::geom_rect(data = bkgrnd
                       , inherit.aes = FALSE
                       , alpha = 0.3
                       , mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = source)) +
    ggplot2::aes(x = age, y = value, linetype = variable, color = height_percentile) +
    ggplot2::geom_line() +
    ggplot2::facet_grid( ~ factor(male, 0:1, c("Female", "Male"))) +
    ggplot2::scale_x_continuous(name = "Age"
                                , breaks = seq(0, max(d$age) + 12, by = 12)
                                , labels = paste(
                                                   paste0(seq(0, max(d$age) + 12, by = 12), "m")
                                                 , paste0(seq(0, max(d$age) + 12, by = 12) / 12, "yr")
                                                 , sep = "\n")
                                ) +
    ggplot2::scale_y_continuous(name = "Median BP (mmHg)",breaks=c(0, 30, 60, 90, 120), limits = c(15, 140)) +
    ggplot2::scale_linetype(name = "") +
    ggplot2::scale_color_hue(name = "Height\nPercentile") +
    ggplot2::scale_fill_manual(name = "Data\nSource", values = c("Gemelli" = ggplot2::alpha("#236192", 0.5)
                                                                , "NHLBI"  = ggplot2::alpha("#6F263D", 0.5)
                                                                , "Lo"     = ggplot2::alpha("#A2AAAD", 0.5)
                                                                )) +
    ggplot2::guides(
                    linetype = ggplot2::guide_legend(order = 1, ncol = 1),
                    fill     = ggplot2::guide_legend(order = 2, ncol = 1),
                    color    = ggplot2::guide_legend(order = 3, ncol = 4)
                    ) +
    ggplot2::theme(
                   legend.position = "bottom"
                   # , legend.direction = "vertical"
                   )
}

## ----label = "chart3_female", echo = FALSE, fig.width = 8, fig.height = 6-----
g(d[male == 0])

## ----label = "chart3_male", echo = FALSE, fig.width = 8, fig.height = 6-------
g(d[male == 1])

## ----label = "shiny", eval = FALSE--------------------------------------------
#  shiny::runApp(system.file("shinyapps", "pedbp", package = "pedbp"))

## ----label = "shiny_batch_example_file", eval = FALSE-------------------------
#  system.file("example_data", "for_batch.csv", package = "pedbp")

## ----label = 'lms_data_table', include = FALSE--------------------------------
lms <- data.table::setDT(data.table::copy(pedbp:::cdc_lms_data))

## -----------------------------------------------------------------------------
p_stature_for_age(q = 154, age = 13 * 12, male = 1L)

## -----------------------------------------------------------------------------
q_stature_for_age(p = c(0.50, 0.60, 0.75), age = 9.5 * 12, male = 0L)

## -----------------------------------------------------------------------------
qnorm(p_stature_for_age(q = 154, age = 13 * 12, male = 1L))
z_stature_for_age(q = 154, age = 13 * 12, male = 1L)

## ----echo = FALSE-------------------------------------------------------------
lfa <- data.table::CJ(p = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99),
                      age = seq(0, 18*12, by = 3),
                      male = 0:1)

lfa[age <  36, l := q_length_for_age_inf(p, age, male), by = .(p, age, male)]
lfa[age >= 36, l := q_stature_for_age(p, age, male), by = .(p, age, male)]
lfa[, lab := paste(p * 100, "%")]
lfa[, male := factor(male, 0:1, c("Female", "Male"))]
lfa[, p := percentile_factor(p)]

g <- function(lfa) {
  ggplot2::ggplot(lfa) +
    ggplot2::theme_bw() +
    ggplot2::aes(x = age, y = l, color = p) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap( ~ male) +
    ggplot2::scale_x_continuous(name = "Age"
                                , breaks = seq(0, max(lfa$age) + 12, by = 12)
                                , labels = paste(
                                                   paste0(seq(0, max(lfa$age) + 12, by = 12), "m")
                                                 , paste0(seq(0, max(lfa$age) + 12, by = 12) / 12, "yr")
                                                 , sep = "\n")
                                ) +
    ggplot2::scale_y_continuous(name = "Length or Height (cm)") +
    ggplot2::scale_color_hue(name = "Length for Age percentile") +
    ggplot2::theme(
                   legend.position = "bottom"
                   )
}

## ----label = "lfa_female", echo = FALSE, fig.width = 8, fig.height = 6--------
g(lfa[male == "Female"])

## ----label = "lfa_male", echo = FALSE, fig.width = 8, fig.height = 6----------
g(lfa[male == "Male"])

## -----------------------------------------------------------------------------
p_weight_for_age(33 * 0.453592, age = 4 * 12, male = 1)

## -----------------------------------------------------------------------------
round(q_weight_for_age_inf(p = 0.2, age = 18, male = 0), 5)

## -----------------------------------------------------------------------------
q_weight_for_length_inf(0.5, 95, 1)
q_weight_for_stature(0.5, 95, 1)

## -----------------------------------------------------------------------------
p_weight_for_length_inf(5.8, 61, 0)

## -----------------------------------------------------------------------------
p_bmi_for_age(q = 22.2, age = c(144, 144), male = c(0, 1))

## -----------------------------------------------------------------------------
q_bmi_for_age(p = 0.5, age = c(120, 120), male = c(1, 0))

## -----------------------------------------------------------------------------
q_head_circ_for_age(0.5, 10, 1)
p_head_circ_for_age(42, 8, 0)

## -----------------------------------------------------------------------------
data(list = "nhlbi_bp_norms", package = "pedbp")
str(nhlbi_bp_norms)

## -----------------------------------------------------------------------------
d <- nhlbi_bp_norms[nhlbi_bp_norms$age == 144 & nhlbi_bp_norms$height_percentile == 50, ]
d <- d[d$male == 0, ]
d

est_norm(q = d$sbp, p = d$bp_percentile / 100)
est_norm(q = d$dbp, p = d$bp_percentile / 100)

bp_parameters[bp_parameters$male == 0 & bp_parameters$age == 144 & bp_parameters$height_percentile == 50, ]

## ----fig.width = 5, fig.height = 5--------------------------------------------
plot( est_norm(q = d$dbp, p = d$bp_percentile / 100) )

## -----------------------------------------------------------------------------
qs <- c(-1.92, 0.05, 0.1, 1.89) * 1.8 + 3.14
ps <- c(0.025, 0.40, 0.50, 0.975)

# with equal weights
w0 <- est_norm(qs, ps)
# weight to ignore one of the middle value and make sure to hit the other
w1 <- est_norm(qs, ps, weights = c(1, 2, 0, 1))
# equal weight the middle, more than the tails
w2 <- est_norm(qs, ps, weights = c(1, 2, 2, 1))

## ----label = "est_norm_plots", fig.width = 9, fig.height = 5------------------
gridExtra::grid.arrange(
  plot(w0) + ggplot2::ggtitle(label = "w0", subtitle = paste0("Mean: ", round(w0$par[1], 2), " SD: ", round(w0$par[2], 3)))
  , plot(w1) + ggplot2::ggtitle(label = "w1", subtitle = paste0("Mean: ", round(w1$par[1], 2), " SD: ", round(w1$par[2], 3)))
  , plot(w2) + ggplot2::ggtitle(label = "w2", subtitle = paste0("Mean: ", round(w2$par[1], 2), " SD: ", round(w2$par[2], 3)))
  , nrow = 1
)

## ----label = "sessioninfo"----------------------------------------------------
sessionInfo()


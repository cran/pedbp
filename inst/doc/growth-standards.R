## ----label = "setup", include = FALSE-----------------------------------------
knitr::opts_chunk$set(collapse = TRUE, fig.align = "center")
library(qwraps2)

## -----------------------------------------------------------------------------
library(pedbp)

## ----echo = FALSE, fig.width = 7, fig.height = 5------------------------------
p_bmi_for_age(21, male = 1, age = 13 * 12) # default source is CDC
p_bmi_for_age(21, male = 1, age = 13 * 12, source = c("CDC", "WHO"))

## ----fig.width = 7, fig.height = 5--------------------------------------------
gs_chart(metric = "bmi_for_age", male = 1, source = "CDC") +
  ggplot2::geom_point(x = 13 * 12, y = 21, inherit.aes = FALSE)

## ----fig.width = 7, fig.height = 5--------------------------------------------
gs_cdf(metric = "bmi_for_age", male = 1, age = 13*12) +
   ggplot2::geom_point(x = 21, y = p_bmi_for_age(21, male = 1, age = 13*12))

## -----------------------------------------------------------------------------
z_bmi_for_age(q = 21, male = 1, age = 13*12)

## -----------------------------------------------------------------------------
q_bmi_for_age(p = 0.5, male = 0, age = 48) # default is CDC
q_bmi_for_age(p = 0.5, male = 0, age = 48, source = c("CDC", "WHO"))


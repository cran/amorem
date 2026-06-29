## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    fig.width = 6,
    fig.height = 4
)

## -----------------------------------------------------------------------------
library(amorem)

## -----------------------------------------------------------------------------
set.seed(2024)
actors <- as.character(1:10)
true_beta <- 0.6

cc <- simulate_relational_events(
  n_events = 1200,
  senders = actors,
  receivers = actors,
  baseline_rate = 1,
  allow_loops = FALSE,
  n_controls = 1,
  endogenous_stats = "reciprocity_count",
  endogenous_effects = true_beta
)
head(cc)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(mgcv)
cases    <- cc[cc$event == 1L, ]
controls <- cc[cc$event == 0L, ]
cases    <- cases[order(cases$stratum), ]
controls <- controls[order(controls$stratum), ]
fit_df <- data.frame(
  one     = 1,
  delta_r = cases$reciprocity_count - controls$reciprocity_count
)
fit <- gam(one ~ delta_r - 1, family = "binomial", data = fit_df)
unname(coef(fit)[1])

## -----------------------------------------------------------------------------
set.seed(2024)
gc <- data.frame(
  time_start = seq(0, 10, by = 1),
  weekday    = rep(c(0, 1), length.out = 11)
)
ev <- simulate_relational_events(
  n_events = 200,
  senders = letters[1:5],
  receivers = letters[1:5],
  baseline_rate = 0.3,
  horizon = 11,
  global_covariates = gc,
  global_effects = c(weekday = 3)
)

share_weekday <- mean(ev$weekday == 1)
share_weekday

## -----------------------------------------------------------------------------
set.seed(7)
actors <- letters[1:5]
gc <- data.frame(time_start = c(0, 2, 4, 6), weekday = c(1, 0, 1, 0))

ev <- simulate_relational_events(
  n_events = 60,
  senders = actors,
  receivers = actors,
  baseline_rate = 1,
  horizon = 7,
  endogenous_stats = "reciprocity_count",
  endogenous_effects = c(reciprocity_count = 0.4),
  global_covariates = gc,
  global_effects = c(weekday = 1.5)
)
head(ev)


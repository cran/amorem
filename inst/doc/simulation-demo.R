## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(amorem)

## -----------------------------------------------------------------------------
senders <- paste0("s", 1:3)
receivers <- paste0("r", 1:2)

covs <- simulate_actor_covariates(
  senders = senders,
  receivers = receivers,
  covariate_names = c("activity", "popularity"),
  time_points = 0:5,
  rho = 0.7,
  sd = 0.2,
  seed = 2024
)
head(covs$sender_covariates)

## -----------------------------------------------------------------------------
static_sender <- reshape(
  covs$sender_covariates,
  direction = "wide",
  idvar = "actor",
  timevar = "covariate"
)

events <- simulate_relational_events(
  n_events = 20,
  senders = senders,
  receivers = receivers,
  baseline_rate = 2,
  sender_covariates = static_sender[, c("value.activity", "value.popularity")],
  sender_effects = c(0.8, -0.2),
  allow_loops = FALSE
)
head(events)


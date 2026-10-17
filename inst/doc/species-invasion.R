## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)

## ----setup--------------------------------------------------------------------
library(amorem)

## ----data---------------------------------------------------------------------
data("dist_matrix", package = "amorem")
states <- rownames(dist_matrix)

# Convert metres to log-units, scaled to a usable range.
dist_log <- log(dist_matrix / 1e5 + 1)

## ----enso, fig.alt = "reconstructed Nino3.4 anomaly, 1500 to 2005"------------
data("enso_nino34", package = "amorem")
enso <- enso_nino34[enso_nino34$year >= 1500, ]
g <- data.frame(time_start = enso$year, nino34 = enso$nino34)

plot(enso$year, enso$nino34, type = "l", col = "grey30",
     xlab = "year", ylab = "Niño3.4 anomaly (°C)",
     main = "Reconstructed ENSO index (Li et al. 2013)")
abline(h = 0, lty = 2, col = "grey60")

## ----simulate-----------------------------------------------------------------
set.seed(4)
ev <- simulate_relational_events(
  n_events            = 600,
  senders             = states,
  receivers           = states,
  contribution_logits = sin(-dist_log / 1.5),
  baseline_rate       = 8e-4,
  start_time          = 1500,
  horizon             = 2005,
  allow_loops         = FALSE,
  endogenous_stats    = "reciprocity_exp_decay",
  endogenous_effects  = c(reciprocity_exp_decay = 0.4),
  half_life           = 50,
  global_covariates   = g,
  global_effects      = c(nino34 = 0.6),
  risk                = "remove"
)
ev <- ev[, c("sender", "receiver", "time")]
nrow(ev)
range(ev$time)

## ----unique-check-------------------------------------------------------------
any(duplicated(paste(ev$sender, ev$receiver)))

## ----no-shift-----------------------------------------------------------------
cc0 <- sample_non_events(ev, mode = "one", risk = "remove", seed = 1)
cc0$nino34 <- g$nino34[findInterval(cc0$time, g$time_start)]
w0 <- widen_case_control(cc0, covariates = "nino34")
all(w0$d_nino34 == 0)

## ----shift--------------------------------------------------------------------
cc <- sample_non_events(ev, mode = "one", risk = "remove", seed = 1,
                        shift = "exponential", window = c(1500, 2005))
attr(cc, "shift")$mean            # mean shift, in years
length(attr(cc, "shift")$uninformative)

## ----covariates---------------------------------------------------------------
cc <- endogenous_features(cc, stats = "reciprocity_exp_decay",
                          half_life = 50, history_log = ev)
cc$dist   <- dist_log[cbind(match(cc$sender, states),
                            match(cc$receiver, states))]
cc$nino34 <- g$nino34[findInterval(cc$time, g$time_start)]

w <- widen_case_control(cc, covariates = c("dist", "reciprocity_exp_decay",
                                           "nino34", "time"))
summary(w$d_nino34)

## ----recovery-----------------------------------------------------------------
fit <- rem(~ nl(dist) + reciprocity_exp_decay + nino34,
           data = w, method = "gam")
est <- summary(fit)$p.table[, c("Estimate", "Std. Error")]
round(est, 3)

## ----recovery-numbers, include = FALSE----------------------------------------
b  <- est[, "Estimate"]
se <- est[, "Std. Error"]
z  <- (b - c(0.4, 0.6)) / se

## ----distance-smooth, fig.alt = "recovered smooth distance effect against the true curve"----
plot(fit, select = 1, shade = TRUE, rug = FALSE,
     xlab = "log-distance", ylab = "f(log-distance)",
     main = "Distance effect: estimate and truth")
d_grid <- seq(min(dist_log[dist_log > 0]), max(dist_log), length.out = 200)
f_true <- sin(-d_grid / 1.5)
lines(d_grid, f_true - mean(f_true), col = "firebrick", lwd = 2, lty = 2)
legend("topright", c("estimate", "truth (centred)"),
       col = c("black", "firebrick"), lty = c(1, 2), lwd = c(1, 2), bty = "n")


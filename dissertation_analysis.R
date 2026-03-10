# ============================================================
# APPENDIX: Full R Script
# "The End of a Benchmark: Swap Rate Volatility Before and
#  After the LIBOR to SONIA Transition"
# ============================================================

library(readxl)
library(rugarch)
library(PerformanceAnalytics)

# ============================================================
# 0. FILE PATH AND SHEET DEFINITIONS
# ============================================================

file_path <- "C:/your/folder/swap_rates.xlsx"

sheets <- list(
  libor_2y        = list(sheet = "libor 2y",           col = "libor_2y"),
  libor_5y        = list(sheet = "libor 5y",           col = "libor_5y"),
  libor_10y       = list(sheet = "libor 10y",          col = "libor_10y"),
  sonia_2y_pre    = list(sheet = "sonia 2y pre 2022",  col = "sonia_2y"),
  sonia_5y_pre    = list(sheet = "sonia 5y pre 2022",  col = "sonia_5y"),
  sonia_10y_pre   = list(sheet = "sonia 10y pre 2022", col = "sonia_10y"),
  sonia_2y_post   = list(sheet = "sonia 2y",           col = "sonia_2y"),
  sonia_5y_post   = list(sheet = "sonia 5y",           col = "sonia_5y"),
  sonia_10y_post  = list(sheet = "sonia 10y",          col = "sonia_10y")
)

series_labels <- c(
  libor_2y       = "LIBOR 2Y",
  libor_5y       = "LIBOR 5Y",
  libor_10y      = "LIBOR 10Y",
  sonia_2y_pre   = "SONIA 2Y (Pre-2022)",
  sonia_5y_pre   = "SONIA 5Y (Pre-2022)",
  sonia_10y_pre  = "SONIA 10Y (Pre-2022)",
  sonia_2y_post  = "SONIA 2Y (Post-2022)",
  sonia_5y_post  = "SONIA 5Y (Post-2022)",
  sonia_10y_post = "SONIA 10Y (Post-2022)"
)

# ============================================================
# 1. HELPER FUNCTIONS
# ============================================================

build_df <- function(sheet, col_name) {
  tmp <- read_excel(file_path, sheet = sheet)
  tmp$Date <- as.Date(tmp$Date)
  tmp <- tmp[order(tmp$Date), ]
  df <- data.frame(
    date  = tmp$Date[-1],
    d_bps = 100 * diff(tmp[[col_name]])
  )
  na.omit(df)
}

candidates <- list(
  AR0    = c(0, 0),
  AR1    = c(1, 0),
  AR2    = c(2, 0),
  MA1    = c(0, 1),
  ARMA11 = c(1, 1),
  ARMA21 = c(2, 1),
  ARMA12 = c(1, 2)
)

fit_arma_candidates <- function(y) {
  lapply(candidates, function(pq) {
    tryCatch(
      arima(y, order = c(pq[1], 0, pq[2]), include.mean = TRUE),
      error = function(e) NULL
    )
  })
}

lb_test <- function(fit, lag = 20) {
  if (is.null(fit)) return(c(p_resid = NA, p_sqresid = NA))
  c(
    p_resid   = tryCatch(Box.test(residuals(fit),   lag = lag, type = "Ljung-Box")$p.value, error = function(e) NA),
    p_sqresid = tryCatch(Box.test(residuals(fit)^2, lag = lag, type = "Ljung-Box")$p.value, error = function(e) NA)
  )
}

fit_garch <- function(df, arma_order, external_regressors = NULL) {
  spec <- ugarchspec(
    variance.model = list(
      model      = "sGARCH",
      garchOrder = c(1, 1),
      external.regressors = external_regressors
    ),
    mean.model = list(
      armaOrder    = arma_order,
      include.mean = TRUE
    ),
    distribution.model = "std"
  )
  fit <- ugarchfit(spec, data = df$d_bps)
  df$vol <- as.numeric(sigma(fit))
  list(fit = fit, df = df)
}

# ============================================================
# 2. LOAD ALL 9 SERIES
# ============================================================

all_data <- lapply(names(sheets), function(nm) {
  build_df(sheets[[nm]]$sheet, sheets[[nm]]$col)
})
names(all_data) <- names(sheets)

# ============================================================
# SECTION A: DESCRIPTIVE STATISTICS (ALL 9 SERIES)
# ============================================================


cat("\n")
cat("================================================================\n")
cat("  SECTION A: DESCRIPTIVE STATISTICS — Swap Rate Levels (%)\n")
cat("================================================================\n\n")

build_levels <- function(sheet, col_name) {
  tmp <- read_excel(file_path, sheet = sheet)
  tmp$Date <- as.Date(tmp$Date)
  tmp <- tmp[order(tmp$Date), ]
  df <- data.frame(
    date  = tmp$Date,
    rate  = tmp[[col_name]]   # already in % as quoted on Bloomberg
  )
  na.omit(df)
}

levels_data <- lapply(names(sheets), function(nm) {
  build_levels(sheets[[nm]]$sheet, sheets[[nm]]$col)
})
names(levels_data) <- names(sheets)

desc_table <- do.call(rbind, lapply(names(levels_data), function(nm) {
  x <- levels_data[[nm]]$rate
  data.frame(
    Series   = series_labels[nm],
    N        = length(x),
    Mean     = round(mean(x),     4),
    Std_Dev  = round(sd(x),       4),
    Min      = round(min(x),      4),
    Max      = round(max(x),      4),
    Skewness = round(skewness(x), 4),
    Kurtosis = round(kurtosis(x), 4),
    stringsAsFactors = FALSE
  )
}))

print(desc_table, row.names = FALSE)

# ============================================================
# SECTION B: DAILY CHANGES TIME-SERIES PLOTS (ALL 9 SERIES)
# ============================================================

cat("\n")
cat("================================================================\n")
cat("  SECTION B: DAILY CHANGES — Time-Series Plots\n")
cat("================================================================\n\n")

op <- par(mfrow = c(3, 3), mar = c(3, 3, 2, 1))
for (nm in names(all_data)) {
  df <- all_data[[nm]]
  plot(df$date, df$d_bps, type = "l",
       main = series_labels[nm],
       xlab = "Date", ylab = "Daily change (bps)",
       col  = "black", lwd = 1)
  abline(h = 0, col = "darkgrey", lty = 3, lwd = 1.5)
}
par(op)

# ============================================================
# SECTION C: ACF AND PACF PLOTS (ALL 9 SERIES)
# ============================================================

cat("\n")
cat("================================================================\n")
cat("  SECTION C: ACF AND PACF PLOTS\n")
cat("================================================================\n\n")

for (nm in names(all_data)) {
  df  <- all_data[[nm]]
  y   <- df$d_bps
  n   <- length(y)
  lbl <- series_labels[nm]
  
  op <- par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
  
  # ACF
  ac <- acf(y, lag.max = 30, plot = FALSE)
  plot(ac$lag[-1], ac$acf[-1], type = "h",
       xlab = "Lag", ylab = "ACF",
       main = paste(lbl, ": ACF"),
       ylim = c(-0.15, 0.15))
  abline(h = 0, lty = 2)
  abline(h = c(-1.96 / sqrt(n), 1.96 / sqrt(n)), lty = 3, col = "blue")
  
  # PACF
  pa <- pacf(y, lag.max = 30, plot = FALSE)
  plot(pa$lag, pa$acf, type = "h",
       xlab = "Lag", ylab = "PACF",
       main = paste(lbl, ": PACF"),
       ylim = c(-0.15, 0.15))
  abline(h = 0, lty = 2)
  abline(h = c(-1.96 / sqrt(n), 1.96 / sqrt(n)), lty = 3, col = "blue")
  
  par(op)
}

# ============================================================
# SECTION D: MEAN EQUATION SELECTION — BIC/AIC + LJUNG-BOX
# ============================================================

cat("\n")
cat("================================================================\n")
cat("  SECTION D: MEAN EQUATION SELECTION (BIC / AIC / Ljung-Box)\n")
cat("================================================================\n\n")

for (nm in names(all_data)) {
  y   <- all_data[[nm]]$d_bps
  lbl <- series_labels[nm]
  fits <- fit_arma_candidates(y)
  
  ic <- do.call(rbind, lapply(names(fits), function(mod) {
    f <- fits[[mod]]
    if (is.null(f)) {
      return(data.frame(Model = mod, AIC = NA, BIC = NA,
                        LB_resid = NA, LB_sqresid = NA))
    }
    lb <- lb_test(f)
    data.frame(
      Model      = mod,
      AIC        = round(AIC(f), 2),
      BIC        = round(BIC(f), 2),
      LB_resid   = round(lb["p_resid"],   4),
      LB_sqresid = round(lb["p_sqresid"], 4)
    )
  }))
  
  ic <- ic[order(ic$BIC), ]
  
  cat("------------------------------------------------------------\n")
  cat("Series:", lbl, "\n")
  cat("Ranked by BIC (smallest = best)\n")
  cat("LB_resid / LB_sqresid = Ljung-Box p-values (lag 20)\n")
  cat("------------------------------------------------------------\n")
  print(ic, row.names = FALSE)
  cat("\n")
}

# ============================================================
# SECTION E: HISTOGRAMS (ALL 9 SERIES)
# ============================================================

cat("\n")
cat("================================================================\n")
cat("  SECTION E: HISTOGRAMS OF DAILY CHANGES\n")
cat("================================================================\n\n")

op <- par(mfrow = c(3, 3), mar = c(4, 3, 3, 1))
for (nm in names(all_data)) {
  x   <- all_data[[nm]]$d_bps
  lbl <- series_labels[nm]
  
  h    <- hist(x, breaks = 50, plot = FALSE)
  dens <- density(x, na.rm = TRUE)
  ymax <- max(h$density, dens$y) * 1.1
  
  hist(x, breaks = 50, freq = FALSE,
       main   = lbl,
       xlab   = "Daily change (bps)",
       col    = "lightgray", border = "white",
       ylim   = c(0, ymax))
  lines(dens, lwd = 2, col = "black")
}
par(op)

# ============================================================
# SECTION F: GARCH(1,1) ESTIMATION — ALL 9 SERIES
# ============================================================
# Mean equation choices (from methodology):
#   2Y maturities  -> AR(1)
#   5Y, 10Y        -> AR(0)
# ============================================================

cat("\n")
cat("================================================================\n")
cat("  SECTION F: GARCH(1,1) ESTIMATION RESULTS\n")
cat("================================================================\n\n")

mean_orders <- list(
  libor_2y       = c(1, 0),
  libor_5y       = c(0, 0),
  libor_10y      = c(0, 0),
  sonia_2y_pre   = c(1, 0),
  sonia_5y_pre   = c(0, 0),
  sonia_10y_pre  = c(0, 0),
  sonia_2y_post  = c(1, 0),
  sonia_5y_post  = c(0, 0),
  sonia_10y_post = c(0, 0)
)

garch_fits <- list()

for (nm in names(all_data)) {
  cat("------------------------------------------------------------\n")
  cat("GARCH(1,1) results:", series_labels[nm], "\n")
  cat("------------------------------------------------------------\n")
  g <- fit_garch(all_data[[nm]], arma_order = mean_orders[[nm]])
  garch_fits[[nm]] <- g
  show(g$fit)
  cat("\n")
}

# ============================================================
# SECTION G: EVENT DATES
# ============================================================

mpc_dates_pre <- as.Date(c(
  "2017-02-02","2017-03-16","2017-05-11","2017-06-15",
  "2017-08-03","2017-09-14","2017-11-02","2017-12-14",
  "2018-02-08","2018-03-22","2018-05-10","2018-06-21",
  "2018-08-02","2018-09-13","2018-11-01","2018-12-20",
  "2019-02-07","2019-03-21","2019-05-02","2019-06-20",
  "2019-08-01","2019-09-19","2019-11-07","2019-12-19",
  "2020-01-30","2020-03-11","2020-03-19","2020-05-07",
  "2020-06-18","2020-08-06","2020-09-17","2020-11-05",
  "2020-12-17","2021-02-04","2021-03-18","2021-05-06",
  "2021-06-24","2021-08-05","2021-09-23","2021-11-04",
  "2021-12-16"
))

mpc_dates_post <- as.Date(c(
  "2022-02-03","2022-03-17","2022-05-05","2022-06-16",
  "2022-08-04","2022-09-15","2022-11-03","2022-12-15",
  "2023-02-02","2023-03-23","2023-05-11","2023-06-22",
  "2023-08-03","2023-09-21","2023-11-02","2023-12-14",
  "2024-02-01","2024-03-21","2024-05-09","2024-06-20",
  "2024-08-01","2024-09-19","2024-11-07","2024-12-19",
  "2025-02-06","2025-03-20","2025-05-08","2025-06-19",
  "2025-08-07","2025-09-18","2025-11-06","2025-12-18"
))

make_mpc_window <- function(dates) {
  sort(unique(c(dates - 1, dates, dates + 1)))
}

mpc_window_pre  <- make_mpc_window(mpc_dates_pre)
mpc_window_post <- make_mpc_window(mpc_dates_post)

covid_start <- as.Date("2020-03-16")
covid_end   <- as.Date("2020-03-27")
ldi_start   <- as.Date("2022-09-23")
ldi_end     <- as.Date("2022-10-14")

# ============================================================
# SECTION H: DUMMY-AUGMENTED GARCH — PRE-2022
# ============================================================

cat("\n")
cat("================================================================\n")
cat("  SECTION H: DUMMY-AUGMENTED GARCH(1,1) — PRE-2022\n")
cat("================================================================\n\n")

make_pre_dummies <- function(df) {
  dates <- df$date
  covid_d <- as.integer(dates >= covid_start & dates <= covid_end)
  mpc_d   <- as.integer(dates %in% mpc_window_pre)
  matrix(c(covid_d, mpc_d), ncol = 2,
         dimnames = list(NULL, c("COVID", "MPC")))
}

pre_series <- c("libor_2y", "libor_5y", "libor_10y",
                "sonia_2y_pre", "sonia_5y_pre", "sonia_10y_pre")

garch_fits_pre_dummy <- list()

for (nm in pre_series) {
  df  <- all_data[[nm]]
  ext <- make_pre_dummies(df)
  cat("------------------------------------------------------------\n")
  cat("Dummy-augmented GARCH(1,1):", series_labels[nm], "\n")
  cat("External regressors: COVID window, MPC window (t-1,t,t+1)\n")
  cat("------------------------------------------------------------\n")
  g <- fit_garch(df, arma_order = mean_orders[[nm]],
                 external_regressors = ext)
  garch_fits_pre_dummy[[nm]] <- g
  show(g$fit)
  cat("\n")
}

# ============================================================
# SECTION I: DUMMY-AUGMENTED GARCH — POST-2022
# ============================================================

cat("\n")
cat("================================================================\n")
cat("  SECTION I: DUMMY-AUGMENTED GARCH(1,1) — POST-2022 SONIA\n")
cat("================================================================\n\n")

make_post_dummies <- function(df) {
  dates <- df$date
  ldi_d <- as.integer(dates >= ldi_start & dates <= ldi_end)
  mpc_d <- as.integer(dates %in% mpc_window_post)
  matrix(c(ldi_d, mpc_d), ncol = 2,
         dimnames = list(NULL, c("LDI", "MPC")))
}

post_series <- c("sonia_2y_post", "sonia_5y_post", "sonia_10y_post")

garch_fits_post_dummy <- list()

for (nm in post_series) {
  df  <- all_data[[nm]]
  ext <- make_post_dummies(df)
  cat("------------------------------------------------------------\n")
  cat("Dummy-augmented GARCH(1,1):", series_labels[nm], "\n")
  cat("External regressors: LDI crisis window, MPC window (t-1,t,t+1)\n")
  cat("------------------------------------------------------------\n")
  g <- fit_garch(df, arma_order = mean_orders[[nm]],
                 external_regressors = ext)
  garch_fits_post_dummy[[nm]] <- g
  show(g$fit)
  cat("\n")
}

# ============================================================
# SECTION J: VOLATILITY PLOTS
# ============================================================

cat("\n")
cat("================================================================\n")
cat("  SECTION J: GARCH(1,1) CONDITIONAL VOLATILITY PLOTS\n")
cat("================================================================\n\n")

# ---- Helper: shade a rectangle within a plot ----
shade_rect <- function(x0, x1, ylim, col = gray(0.85)) {
  rect(x0, ylim[1], x1, ylim[2], col = col, border = NA)
}

# ---- Helper: draw MPC vertical lines ----
draw_mpc <- function(dates_window, xlim) {
  in_range <- dates_window[dates_window >= xlim[1] & dates_window <= xlim[2]]
  abline(v = in_range, col = "lightblue", lty = 3, lwd = 0.8)
}

# ----------------------------------------------------------
# J1: PRE-2022 LIBOR vs SONIA — BASELINE (3 panels, no dummies)
# ----------------------------------------------------------

merge_vols <- function(nm_L, nm_S) {
  merge(
    setNames(garch_fits[[nm_L]]$df[, c("date", "vol")], c("date", "vol_libor")),
    setNames(garch_fits[[nm_S]]$df[, c("date", "vol")], c("date", "vol_sonia")),
    by = "date"
  )
}

vol2_base  <- merge_vols("libor_2y",  "sonia_2y_pre")
vol5_base  <- merge_vols("libor_5y",  "sonia_5y_pre")
vol10_base <- merge_vols("libor_10y", "sonia_10y_pre")

all_pre_vols <- c(vol2_base$vol_libor,  vol2_base$vol_sonia,
                  vol5_base$vol_libor,  vol5_base$vol_sonia,
                  vol10_base$vol_libor, vol10_base$vol_sonia)
ylim_pre <- c(min(all_pre_vols) * 0.9, max(all_pre_vols) * 1.1)

op <- par(mfrow = c(3, 1), mar = c(3, 4, 2, 2), oma = c(4, 0, 4, 0))

for (i in seq_along(list(vol2_base, vol5_base, vol10_base))) {
  vd    <- list(vol2_base, vol5_base, vol10_base)[[i]]
  labs  <- c("2Y swaps", "5Y swaps", "10Y swaps")[i]
  
  plot(vd$date, vd$vol_libor, type = "l",
       xlab = "", ylab = "Volatility (bps)",
       main = labs,
       ylim = ylim_pre,
       col  = "blue", lwd = 2)
  lines(vd$date, vd$vol_sonia, col = "red", lwd = 2)
  
  if (i == 1) {
    legend("topleft",
           legend = c("LIBOR", "SONIA (pre-2022)"),
           col = c("blue", "red"), lwd = 2, bty = "n", cex = 0.85)
  }
}

mtext("Pre-2022 LIBOR vs SONIA: GARCH(1,1) conditional volatility (baseline)",
      side = 3, outer = TRUE, line = 1.5, cex = 1.1, font = 2)
mtext("Date", side = 1, outer = TRUE, line = 2.2)
par(op)

# ----------------------------------------------------------
# J2: PRE-2022 LIBOR vs SONIA — WITH COVID & MPC WINDOWS
# ----------------------------------------------------------

op <- par(mfrow = c(3, 1), mar = c(3, 4, 2, 2), oma = c(4, 0, 4, 0))

for (i in seq_along(list(vol2_base, vol5_base, vol10_base))) {
  vd   <- list(vol2_base, vol5_base, vol10_base)[[i]]
  labs <- c("2Y swaps", "5Y swaps", "10Y swaps")[i]
  xlim <- range(vd$date)
  
  plot(vd$date, vd$vol_libor, type = "n",
       xlab = "", ylab = "Volatility (bps)",
       main = labs, ylim = ylim_pre)
  
  # COVID shading
  shade_rect(max(xlim[1], covid_start), min(xlim[2], covid_end), ylim_pre)
  
  # MPC lines
  draw_mpc(mpc_window_pre, xlim)
  
  lines(vd$date, vd$vol_libor, col = "blue", lwd = 2)
  lines(vd$date, vd$vol_sonia, col = "red",  lwd = 2)
  
  if (i == 1) {
    legend("topleft",
           legend = c("LIBOR", "SONIA (pre-2022)",
                      "COVID window", "MPC window (t-1,t,t+1)"),
           col    = c("blue", "red", gray(0.7), "lightblue"),
           lwd    = c(2, 2, NA, 1),
           lty    = c(1, 1, NA, 3),
           pch    = c(NA, NA, 15, NA),
           pt.cex = c(NA, NA, 2, NA),
           bty    = "n", cex = 0.78)
  }
}

mtext("Pre-2022 LIBOR vs SONIA: GARCH(1,1) conditional volatility (with COVID & MPC windows)",
      side = 3, outer = TRUE, line = 1.5, cex = 1.1, font = 2)
mtext("Date", side = 1, outer = TRUE, line = 2.2)
par(op)

# ----------------------------------------------------------
# J3: POST-2022 SONIA — BASELINE (no dummies, 2-panel)
# ----------------------------------------------------------

vol2_post  <- garch_fits[["sonia_2y_post"]]$df[, c("date", "vol")]
vol5_post  <- garch_fits[["sonia_5y_post"]]$df[, c("date", "vol")]
vol10_post <- garch_fits[["sonia_10y_post"]]$df[, c("date", "vol")]

post_merged <- Reduce(function(a, b) merge(a, b, by = "date"),
                      list(
                        setNames(vol2_post,  c("date", "vol_2y")),
                        setNames(vol5_post,  c("date", "vol_5y")),
                        setNames(vol10_post, c("date", "vol_10y"))
                      )
)

ylim_post <- c(min(post_merged[, -1]) * 0.9,
               max(post_merged[, -1]) * 1.1)

op <- par(mfrow = c(2, 1), mar = c(3, 4, 3, 2), oma = c(4, 0, 4, 0))

# Top panel: baseline
plot(post_merged$date, post_merged$vol_2y, type = "l",
     xlab = "", ylab = "Volatility (bps)",
     main = "Baseline GARCH(1,1) SONIA volatility (post-2022)",
     ylim = ylim_post,
     col  = "blue", lwd = 2)
lines(post_merged$date, post_merged$vol_5y,  col = "red",       lwd = 2)
lines(post_merged$date, post_merged$vol_10y, col = "darkgreen", lwd = 2)
legend("topright",
       legend = c("2Y SONIA", "5Y SONIA", "10Y SONIA"),
       col    = c("blue", "red", "darkgreen"),
       lwd    = 2, bty = "n", cex = 0.85)

# Bottom panel: LDI & MPC windows
xlim_post <- range(post_merged$date)
plot(post_merged$date, post_merged$vol_2y, type = "n",
     xlab = "", ylab = "Volatility (bps)",
     main = "SONIA volatility with LDI crisis and MPC windows",
     ylim = ylim_post)

shade_rect(max(xlim_post[1], ldi_start),
           min(xlim_post[2], ldi_end), ylim_post)
draw_mpc(mpc_window_post, xlim_post)

lines(post_merged$date, post_merged$vol_2y,  col = "blue",      lwd = 2)
lines(post_merged$date, post_merged$vol_5y,  col = "red",       lwd = 2)
lines(post_merged$date, post_merged$vol_10y, col = "darkgreen", lwd = 2)
legend("topright",
       legend = c("2Y SONIA", "5Y SONIA", "10Y SONIA",
                  "LDI crisis window", "MPC window (t-1,t,t+1)"),
       col    = c("blue", "red", "darkgreen", gray(0.7), "lightblue"),
       lwd    = c(2, 2, 2, NA, 1),
       lty    = c(1, 1, 1, NA, 3),
       pch    = c(NA, NA, NA, 15, NA),
       pt.cex = c(NA, NA, NA, 2, NA),
       bty    = "n", cex = 0.78)

mtext("Post-2022 SONIA swaps: GARCH(1,1) conditional volatility",
      side = 3, outer = TRUE, line = 1.5, cex = 1.1, font = 2)
mtext("Date", side = 1, outer = TRUE, line = 2.2)
par(op)




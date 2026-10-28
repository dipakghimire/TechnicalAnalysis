# =============================================================================
# BDA400 - Assignment 2: Technical Analysis using R (Preliminary Stage)
# Author : <Your Name>
# File   : technical_analysis_stage1.R
# Purpose: Import stock data listed in portfolio.txt, compute basic statistics,
#          and display the data and visualizations.
# =============================================================================

# ---- 1. Packages -------------------------------------------------------------
# Run once (screenshot this step for your report):
# install.packages(c("quantmod", "TTR"))

library(quantmod)   # getSymbols(), chartSeries()
library(TTR)        # SMA() and other technical indicators

# ---- 2. Import functions ----------------------------------------------------

# Read ticker symbols (one per line) from the portfolio file
read_portfolio <- function(file = "portfolio.txt") {
  if (!file.exists(file)) stop("File not found: ", file,
                               " (check your working directory with getwd())")
  symbols <- trimws(readLines(file, warn = FALSE))
  symbols <- symbols[symbols != "" & !grepl("^#", symbols)]  # skip blanks/comments
  unique(toupper(symbols))
}

# Load Yahoo Finance data for each symbol; returns a named list of data frames.
# Optionally also creates one data frame per symbol in the global environment.
load_stock_data <- function(file = "portfolio.txt",
                            from = Sys.Date() - 365 * 2,
                            to = Sys.Date(),
                            assign_global = TRUE) {
  symbols <- read_portfolio(file)
  stocks <- list()

  for (sym in symbols) {
    xt <- tryCatch(
      getSymbols(sym, src = "yahoo", from = from, to = to,
                 auto.assign = FALSE, warnings = FALSE),
      error = function(e) {
        message("Could not load ", sym, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(xt)) next

    df <- data.frame(Date = index(xt), coredata(xt), row.names = NULL)
    names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    stocks[[sym]] <- df

    if (assign_global) assign(make.names(sym), df, envir = .GlobalEnv)
    message("Loaded ", sym, ": ", nrow(df), " rows")
  }
  stocks
}

# ---- 3. Statistics functions ------------------------------------------------

# Mode for continuous prices: most frequent value after rounding
get_mode <- function(x, digits = 2) {
  x <- round(na.omit(x), digits)
  counts <- table(x)
  as.numeric(names(counts)[which.max(counts)])
}

# Safe simple moving average (returns NA if there are too few observations)
safe_sma <- function(x, n) {
  if (length(x) < n) rep(NA_real_, length(x)) else as.numeric(SMA(x, n = n))
}

# Basic statistics for one stock's data frame
calculate_statistics <- function(df, windows = c(20, 50)) {
  close <- df$Close

  ma <- data.frame(Date = df$Date, Close = close)
  for (w in windows) ma[[paste0("SMA_", w)]] <- safe_sma(close, w)

  summary_df <- data.frame(
    Mean   = mean(close, na.rm = TRUE),
    Median = median(close, na.rm = TRUE),
    Mode   = get_mode(close),
    StdDev = sd(close, na.rm = TRUE),
    Min    = min(close, na.rm = TRUE),
    Max    = max(close, na.rm = TRUE)
  )
  for (w in windows) {
    summary_df[[paste0("Latest_SMA_", w)]] <-
      tail(na.omit(ma[[paste0("SMA_", w)]]), 1)[1]
  }

  list(summary = summary_df, moving_averages = ma)
}

# Apply to every stock; returns one summary table + moving averages per stock
calculate_all_statistics <- function(stocks, windows = c(20, 50)) {
  results <- lapply(stocks, calculate_statistics, windows = windows)
  summary_table <- do.call(rbind, lapply(names(results), function(s)
    cbind(Symbol = s, results[[s]]$summary)))
  rownames(summary_table) <- NULL
  list(summary = summary_table,
       moving_averages = lapply(results, `[[`, "moving_averages"))
}

# ---- 4. Display utilities ---------------------------------------------------

# Yahoo-Finance-style "Historical Data" view for each stock
display_stock_data <- function(stocks, n = 5) {
  for (sym in names(stocks)) {
    df <- stocks[[sym]]
    cat("\n==================== ", sym, " ====================\n", sep = "")
    cat("Rows:", nrow(df), "| From", format(min(df$Date)),
        "to", format(max(df$Date)), "\n\n")
    cat("First", n, "rows:\n"); print(head(df, n), row.names = FALSE)
    cat("\nLast", n, "rows:\n");  print(tail(df, n), row.names = FALSE)
    cat("\nStructure:\n"); str(df)
    cat("\nSummary:\n");   print(summary(df[, -1]))
  }
}

# Snapshot like Yahoo's quote header (latest close, day change, 52-week range)
display_quote_summary <- function(stocks) {
  snap <- do.call(rbind, lapply(names(stocks), function(sym) {
    df <- stocks[[sym]]; n <- nrow(df)
    last <- df$Close[n]; prev <- df$Close[n - 1]
    yr <- tail(df, 252)
    data.frame(Symbol = sym,
               Last = round(last, 2),
               Change = round(last - prev, 2),
               PctChange = round((last / prev - 1) * 100, 2),
               Low52w = round(min(yr$Low), 2),
               High52w = round(max(yr$High), 2),
               Volume = df$Volume[n])
  }))
  print(snap, row.names = FALSE)
  invisible(snap)
}

display_statistics <- function(stats) {
  cat("\n=========== Basic Statistics (Close price) ===========\n")
  print(format(stats$summary, digits = 2, nsmall = 2), row.names = FALSE)
}

# ---- 5. Visualizations ------------------------------------------------------

# Close price with moving averages
plot_price_with_ma <- function(sym, ma_df) {
  sma_cols <- grep("^SMA_", names(ma_df), value = TRUE)
  cols <- c("red", "darkgreen", "purple")[seq_along(sma_cols)]
  plot(ma_df$Date, ma_df$Close, type = "l", col = "steelblue", lwd = 1.5,
       xlab = "Date", ylab = "Close price",
       main = paste(sym, "- Close Price with Moving Averages"))
  for (i in seq_along(sma_cols))
    lines(ma_df$Date, ma_df[[sma_cols[i]]], col = cols[i], lwd = 2)
  legend("topleft", legend = c("Close", sma_cols),
         col = c("steelblue", cols), lwd = 2, bty = "n")
}

# Histogram of closing prices with mean/median/mode markers
plot_distribution <- function(sym, df, stats_row) {
  hist(df$Close, breaks = 30, col = "lightblue", border = "white",
       main = paste(sym, "- Distribution of Close Price"), xlab = "Close price")
  abline(v = stats_row$Mean,   col = "red",       lwd = 2)
  abline(v = stats_row$Median, col = "darkgreen", lwd = 2, lty = 2)
  abline(v = stats_row$Mode,   col = "purple",    lwd = 2, lty = 3)
  legend("topright", legend = c("Mean", "Median", "Mode"),
         col = c("red", "darkgreen", "purple"), lwd = 2, lty = 1:3, bty = "n")
}

# Candlestick chart (quantmod) with SMAs and volume
plot_candlestick <- function(sym, df, months = 6) {
  xt <- xts::xts(df[, c("Open", "High", "Low", "Close", "Volume", "Adjusted")],
                 order.by = df$Date)
  chartSeries(xt, name = sym, theme = chartTheme("white"),
              subset = paste("last", months, "months"),
              TA = "addSMA(20);addSMA(50);addVolume()")
}

# All stocks rebased to 100 for comparison
plot_comparison <- function(stocks) {
  rebased <- lapply(stocks, function(df) df$Close / df$Close[1] * 100)
  dates <- stocks[[1]]$Date
  len <- min(sapply(rebased, length))
  cols <- rainbow(length(rebased))
  matplot(dates[seq_len(len)],
          sapply(rebased, function(x) x[seq_len(len)]),
          type = "l", lty = 1, lwd = 2, col = cols,
          xlab = "Date", ylab = "Rebased price (start = 100)",
          main = "Portfolio Comparison")
  legend("topleft", legend = names(rebased), col = cols, lwd = 2, bty = "n")
}

# ---- 6. Run the analysis ----------------------------------------------------
# In RStudio: Session > Set Working Directory > To Source File Location

stocks <- load_stock_data("portfolio.txt")

display_stock_data(stocks)
display_quote_summary(stocks)

stats <- calculate_all_statistics(stocks)
display_statistics(stats)

# Plots (use the Plots pane arrows to flip through; screenshot for your report)
for (sym in names(stocks)) {
  plot_price_with_ma(sym, stats$moving_averages[[sym]])
  plot_distribution(sym, stocks[[sym]], stats$summary[stats$summary$Symbol == sym, ])
  plot_candlestick(sym, stocks[[sym]])
}
plot_comparison(stocks)

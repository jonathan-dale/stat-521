plot_ci <- function(lo, hi, m,
                   col_in = "#2C7BE5",   # blue for intervals containing mu
                   col_out = "#E55353",  # red for intervals missing mu
                   lwd_in = 2, lwd_out = 2.5,
                   pch_in = 16, pch_out = 16,
                   cex_in = 0.7, cex_out = 0.7,
                   legend_pos = "topleft") {

  stopifnot(length(lo) == length(hi))
  k <- length(lo)

  # Set margins and restore afterward
  op <- par(mar = c(2, 1, 1, 1), mgp = c(2.7, 0.7, 0))
  on.exit(par(op), add = TRUE)

  # Plot setup
  ci.half <- pmax(abs(hi - m), abs(lo - m))
  xR <- m + max(ci.half) * c(-1, 1) * 1.05
  yR <- c(0, k + 1)

  plot(xR, yR, type = "n", xlab = "", ylab = "", axes = FALSE)
  abline(v = m, lty = 2, col = "#00000080")
  axis(1, at = m, labels = paste("mu =", round(m, 4)), cex.axis = 0.9)

  contains <- (m >= lo) & (m <= hi)

  for (i in seq_len(k)) {
    ci <- c(lo[i], hi[i])
    mid <- mean(ci)

    if (contains[i]) {
      lines(ci, rep(i, 2), col = col_in, lwd = lwd_in)
      points(mid, i, pch = pch_in, cex = cex_in, col = col_in)
    } else {
      lines(ci, rep(i, 2), col = col_out, lwd = lwd_out)
      points(mid, i, pch = pch_out, cex = cex_out, col = col_out)
    }
  }
 }

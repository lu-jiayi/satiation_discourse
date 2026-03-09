library(bootstrap)

theta <- function(x, xdata, na.rm = TRUE) {
  mean(xdata[x], na.rm = na.rm)
}

ci.low <- function(x, na.rm = TRUE) {
  mean(x, na.rm = na.rm) -
    quantile(
      bootstrap(1:length(x), 1000, theta, x, na.rm = na.rm)$thetastar,
      .025,
      na.rm = na.rm
    )
}

ci.high <- function(x, na.rm = TRUE) {
  quantile(
    bootstrap(1:length(x), 1000, theta, x, na.rm = na.rm)$thetastar,
    .975,
    na.rm = na.rm
  ) - mean(x, na.rm = na.rm)
}

`%notin%` <- Negate(`%in%`)

cbPalette <- c("#d55e00", "#009e74", "#e69d00", "#cc79a7", "#0071b2")
gridY <- c(rep(-4, 5), rep(-3, 6), rep(-2, 7), rep(-1, 8), rep(0, 9), rep(1, 8), rep(2, 7), rep(3, 6), rep(4, 5))
gridY <- -gridY
plot(c(-6, 6), c(-6, 6))
gridX <- c(seq(-2, 2, 1), seq(-2.5, 2.5, 1), seq(-3, 3, 1), seq(-3.5, 3.5, 1), seq(-4, 4, 1), seq(-3.5, 3.5, 1), seq(-3, 3, 1), seq(-2.5, 2.5, 1), seq(-2, 2, 1))
grid <- rep(NA, 61)

pvalues <- tapply(all_data$pvalue, all_data$seg, mean)
slopes <- tapply(all_data$slope, all_data$seg, mean)

plot(c(-6, 6), c(-6, 6))
for(i in 1:61) {
  if (pvalues[i] < 0.05) {
    result_char = "0"
    if (slopes[i] > 0) result_char = "+"
    if (slopes[i] < 0) result_char = "-"  
  } else {
    result_char = "."
  }
  text(gridX[i], gridY[i], result_char)
}
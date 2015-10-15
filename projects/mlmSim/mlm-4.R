pdf.options(onefile=F,family="Times",pointsize=10)

set.seed(234234)
library(rockchalk)
library(lme4)
workingdata <- "workingdata/"
if (!file.exists(workingdata)) dir.create(workingdata, recursive = FALSE)
## CAUTION: TO re run sim, set this TRUE
runsim <- FALSE


###################################################
### code chunk number 5: gen10
###################################################
gen1 <- function (beta = c(3, 0.5), xbari = 25, xsdi = 5, 
  xsd = 4, M = 10, n = 3, bsd = 2, esd = 4){
  xbars <- rnorm(M, m = xbari, sd = xsdi)
  dat <- data.frame(i = rep(1:M, each = n))
  dat$x <-  rep(xbars, each = n) + rnorm(M*n, m = 0, sd = xsd)
  b <- rnorm(M, m = 0, bsd)
  dat$b <- rep(b, each = n)
  error <- rnorm(M*n, m = 0, sd = esd)
  dat$ynob <- beta[1] + beta[2] * dat$x + error
  dat$ynoe <- beta[1] + beta[2] * dat$x + dat$b
  dat$y <- dat$ynoe + rnorm(M*n, m = 0, sd = esd)
  list(dat = dat, b = b)
}


###################################################
### code chunk number 6: case1a
###################################################
M <- 10; n <- 3; beta0 <- 3; beta1 <- 0.5
simdata <- gen1(M = M, n = 3, beta = c(beta0, beta1))
dat <- simdata[["dat"]]
b <- simdata[["b"]]


###################################################
### code chunk number 7: fig10
###################################################
plot(y ~ x, data = dat)


###################################################
### code chunk number 8: simulating-mlm-1.Rnw:452-458
###################################################
par(ask=TRUE)
for (mm in 1:10){
    plot(y ~ x, type = "n", data = dat)
    with(dat[dat$i <= mm, ], text(y ~ x, labels = i))
}


###################################################
### code chunk number 9: fig40a
###################################################
m1 <- lm(y ~ x, data = dat)
outreg(list("True Slope 0.5" = m1), tight = FALSE)


###################################################
### code chunk number 10: fig40b
###################################################
plotSlopes(m1, plotx = "x", interval = "confidence")


###################################################
### code chunk number 11: fig19a
###################################################
plot(y ~ x, type = "n", data = dat, ylim = magRange(dat$y, 1.3))
for(mm in 1:M){
   abline(a = beta0 + b[mm], b = beta1, lty = mm, col = "gray80")
}


###################################################
### code chunk number 12: fig19b
###################################################
plot(y ~ x, type = "n", data = dat, ylim = magRange(dat$y, 1.3))
for(mm in 1:M){
   abline(a = beta0 + b[mm], b = beta1, lty = mm, col = "gray80")
}
abline(a = beta0, b = beta1, col = "blue", lwd = 2)


###################################################
### code chunk number 13: fig20b
###################################################
plot(y ~ x, type = "n", data = dat, ylim = magRange(dat$y, 1.3))
for(mm in 1:M){
   abline(a = beta0 + b[mm], b = beta1, lty = mm, col = "gray80")
}
points(ynoe ~ x, data = dat)


###################################################
### code chunk number 14: fig20c
###################################################
plot(y ~ x, type = "n", data = dat, ylim = magRange(dat$y, 1.3))
for(mm in 1:M){
   abline(a = beta0 + b[mm], b = beta1, lty = mm, col = "gray80")
}
points(y ~ x, data = dat)


###################################################
### code chunk number 15: fig20d
###################################################
plot(y ~ x, type = "n", data = dat, ylim = magRange(dat$y, 1.3))
for(mm in 1:M){
   abline(a = beta0 + b[mm], b = beta1, lty = mm, col = "gray80")
}
points(y ~ x, data = dat)
m1 <- lm(y ~ x, data = dat)
abline(m1, lwd = 2, col = "blue")


###################################################
### code chunk number 16: fig20e
###################################################
plot(y ~ x, type = "n", data = dat, ylim = magRange(dat$y, 1.3))
for(mm in 1:M){
   abline(a = beta0 + b[mm], b = beta1, lty = mm, col = "gray80")
}
points(y ~ x, data = dat)
m1 <- lm(y ~ x, data = dat)
abline(m1, lwd = 3, col = "blue")


###################################################
### code chunk number 17: fig20f
###################################################
plot(y ~ x, type = "n", data = dat, ylim = magRange(dat$y, 1.3))
for(mm in 1:M){
   abline(a = beta0 + b[mm], b = beta1, lty = mm, col = "gray80")
}
points(y ~ x, data = dat)
m1 <- lm(y ~ x, data = dat)
abline(m1, lwd = 3, col = "blue")
mlm1 <- lmer(y ~ x + (1 | i), data = dat)
abline(fixef(mlm1), col = "red", lwd = 2)
legend("topleft", legend = c("OLS", "MLM"), lty = 1, col = c("blue", "red"))


###################################################
### code chunk number 18: rep10a1
###################################################
simdata <- gen1(M = M, n = n, beta = c(beta0, beta1))
b <- simdata[["b"]]
dat <- simdata[["dat"]]


###################################################
### code chunk number 19: rep10a2
###################################################
m1 <- lm(y ~ x, data = simdata[["dat"]])
outreg(list("Run 2, True Slope 0.5" = m1), tight = FALSE)


###################################################
### code chunk number 20: rep10a3
###################################################
plot(y ~ x, type = "n", data = simdata[["dat"]], ylim = magRange(dat$y, 1.3))
for(mm in 1:M){
   abline(a = beta0 + b[mm], b = beta1, lty = mm, col = "gray80")
}
points(y ~ x, data = dat)
m1 <- lm(y ~ x, data = dat)
abline(m1, lwd = 2, col = "blue")
mlm1 <- lmer(y ~ x + (1 | i), data = dat)
abline(fixef(mlm1), col = "red", lwd = 2)
legend("topleft", legend = c("OLS", "MLM"), lty = 1, col = c("blue", "red"))


###################################################
### code chunk number 21: rep10b1
###################################################
simdata <- gen1(M = M, n = n, beta = c(beta0, beta1))
b <- simdata[["b"]]
dat <- simdata[["dat"]]


###################################################
### code chunk number 22: rep10b2
###################################################
m1 <- lm(y ~ x, data = simdata[["dat"]])
outreg(list("Run 2, True Slope 0.5" = m1), tight = FALSE)


###################################################
### code chunk number 23: rep10b3
###################################################
plot(y ~ x, type = "n", data = dat, ylim = magRange(dat$y, 1.3))
for(mm in 1:M){
   abline(a = beta0 + b[mm], b = beta1, lty = mm, col = "gray80")
}
points(y ~ x, data = dat)
m1 <- lm(y ~ x, data = dat)
abline(m1, lwd = 3, col = "blue")
mlm1 <- lmer(y ~ x + (1 | i), data = dat)
abline(fixef(mlm1), col = "red", lwd = 2)
legend("topleft", legend = c("OLS", "MLM"), lty = 1, col = c("blue", "red"))


###################################################
### code chunk number 24: rep20a
###################################################
if (runsim) {
betahet <- matrix(NA, nrow = 1000, ncol = 2)
betanob <- matrix(NA, nrow = 1000, ncol = 2)
betamlm <- matrix(NA, nrow = 1000, ncol = 2)
set.seed(234234)
for (i in 1:1000){
	simdata <- gen1(M = M, beta = c(beta0, beta1))
	m1 <- lm(y ~ x, data = simdata[["dat"]])
	betahet[i, ] <- coef(m1)
	m2 <- lm(ynob ~ x, data = simdata[["dat"]])
	betanob[i, ] <- coef(m2)
	m3 <- lmer(y ~ x + (1 | i), data = simdata[["dat"]])
	betamlm[i, ] <- fixef(m3)
}
saveRDS(betahet, paste0(workingdata, "betahet.rds"))
saveRDS(betanob, paste0(workingdata, "betanob.rds"))
saveRDS(betamlm, paste0(workingdata, "betamlm.rds"))
} else {
  betahet <- readRDS(paste0(workingdata, "betahet.rds"))
  betanob <- readRDS(paste0(workingdata, "betanob.rds"))
  betamlm <- readRDS(paste0(workingdata, "betamlm.rds"))
}


###################################################
### code chunk number 25: rep20b
###################################################
plot(y ~ x, data = dat, type = "n", ylim = c(-5, 45), xlim = c(15, 40))
for (i in 1:1000){
	abline(betahet[i, 1], betahet[i, 2], lwd = 1, col = "blue")
}
abline(beta0, beta1, col = "black")


###################################################
### code chunk number 26: rep20c
###################################################
hist(betahet[ ,1], prob = TRUE, breaks = 50, main = "Intercept Estimates")
lines(density(betahet[ ,1]))
abline(v = beta1, lty = 4, lwd = 3,  col = "black")


###################################################
### code chunk number 27: rep20d
###################################################
  hist(betahet[ ,2], prob = TRUE, breaks = 50, main = "Slope Estimates")
  lines(density(betahet[ ,2]))
abline(v = beta1, lty = 4, lwd = 3, col = "black")


###################################################
### code chunk number 28: rep20e
###################################################
plot(betahet[ , 2] ~ betahet[ , 1], xlab = "Estimates of Intercept", ylab = "Estimates of Slope", col = "blue", ylim = c(-0.3, 1.3), xlim = c(-22, 22))
points(beta0, beta1, col = "black", cex = 1.5, lwd = 3)


###################################################
### code chunk number 29: genols2
###################################################
plot(betanob[ ,2] ~ betanob[ , 1], xlab = "Estimates of Intercept", ylab = "Estimates of Slope", col = "blue", ylim = c(-0.3, 1.3), xlim = c(-22, 22))
points(beta0, beta1, col = "black", cex = 1.5, lwd = 3)


###################################################
### code chunk number 30: mlmsim10
###################################################
plot(y ~ x, data = dat, type = "n", ylim = c(-5, 45), xlim = c(15, 40))
for (i in 1:1000){
	abline(betahet[i, 1], betahet[i, 2], lwd = 1, col = "blue")
}
abline(beta0, beta1, col = "black")
for (i in 1:1000){
	abline(betamlm[i, 1], betamlm[i, 2], lwd = 1, col = "red")
}
abline(beta0, beta1, col = "black")


###################################################
### code chunk number 31: mlmsim20a
###################################################
  hist(betahet[ ,2], prob = TRUE, breaks = 50, main = "OLS Slope Estimates")
  lines(density(betahet[ ,2]))
abline(v = beta1, lty = 4, lwd = 3, col = "black")


###################################################
### code chunk number 32: mlmsim20b
###################################################
hist(betamlm[ ,2], prob = TRUE, breaks = 50, main = "MLM Slope Estimates")
lines(density(betamlm[ ,2]))
abline(v = beta1, lty = 4, lwd = 3,  col = "black")


###################################################
### code chunk number 33: mlmsim30
###################################################
hist(betahet[ ,2], prob = TRUE, breaks = 50, main = "OLS Slope Estimates")
lines(density(betahet[ ,2]), col = "blue", lty = 4)
lines(density(betamlm[ ,2]), col = "red", lty = 1)
abline(v = beta1, lty = 4, lwd = 3, col = "black")
legend("topright", c("OLS", "MLM"), col = c("blue", "red"), lty = c(4,1))


###################################################
### code chunk number 34: mlmsim31
###################################################
hist(betahet[ ,1], prob = TRUE, breaks = 50, main = "OLS Intercept Estimates", border = "gray90")
lines(density(betahet[ ,1]), col = "blue", lty = 4)
lines(density(betamlm[ ,1]), col = "red", lty = 1)
abline(v = beta0, lty = 4, lwd = 3, col = "black")
legend("topright", c("OLS", "MLM"), col = c("blue", "red"), lty = c(4,1))


###################################################
### code chunk number 35: vrep10a1
###################################################
simdata <- gen1(M = M, n = n, beta = c(beta0, beta1), bsd = 20)
b <- simdata[["b"]]
dat <- simdata[["dat"]]


###################################################
### code chunk number 36: vrep10a2
###################################################
m1 <- lm(y ~ x, data = simdata[["dat"]])
outreg(list("Run 2, True Slope 0.5" = m1), tight = FALSE)


###################################################
### code chunk number 37: vrep10a3
###################################################
plot(y ~ x, type = "n", data = simdata[["dat"]], ylim = magRange(dat$y, 1.3))
for(mm in 1:M){
   abline(a = beta0 + b[mm], b = beta1, lty = mm, col = "gray80")
}
points(y ~ x, data = dat)
m1 <- lm(y ~ x, data = dat)
abline(m1, lwd = 2, col = "blue")
mlm1 <- lmer(y ~ x + (1 | i), data = dat)
abline(fixef(mlm1), col = "red", lwd = 2)
legend("topleft", legend = c("OLS", "MLM"), lty = 1, col = c("blue", "red"))


###################################################
### code chunk number 38: vrep10b1
###################################################
simdata <- gen1(M = M, n = n, beta = c(beta0, beta1), bsd = 20)
b <- simdata[["b"]]
dat <- simdata[["dat"]]


###################################################
### code chunk number 39: vrep10b2
###################################################
m1 <- lm(y ~ x, data = simdata[["dat"]])
outreg(list("Run 2, True Slope 0.5" = m1), tight = FALSE)


###################################################
### code chunk number 40: vrep10b3
###################################################
plot(y ~ x, type = "n", data = dat, ylim = magRange(dat$y, 1.3))
for(mm in 1:M){
   abline(a = beta0 + b[mm], b = beta1, lty = mm, col = "gray80")
}
points(y ~ x, data = dat)
m1 <- lm(y ~ x, data = dat)
abline(m1, lwd = 3, col = "blue")
mlm1 <- lmer(y ~ x + (1 | i), data = dat)
abline(fixef(mlm1), col = "red", lwd = 2)
legend("topleft", legend = c("OLS", "MLM"), lty = 1, col = c("blue", "red"))


###################################################
### code chunk number 41: vrep20a
###################################################
if (runsim) {
betahet <- matrix(NA, nrow = 1000, ncol = 2)
betanob <- matrix(NA, nrow = 1000, ncol = 2)
betamlm <- matrix(NA, nrow = 1000, ncol = 2)
set.seed(234234)
for (i in 1:1000){
	simdata <- gen1(M = M, beta = c(beta0, beta1), bsd = 20)
	m1 <- lm(y ~ x, data = simdata[["dat"]])
	betahet[i, ] <- coef(m1)
	m2 <- lm(ynob ~ x, data = simdata[["dat"]])
	betanob[i, ] <- coef(m2)
	m3 <- lmer(y ~ x + (1 | i), data = simdata[["dat"]])
	betamlm[i, ] <- fixef(m3)
}
saveRDS(betahet, paste0(workingdata, "vbetahet.rds"))
saveRDS(betanob, paste0(workingdata, "vbetanob.rds"))
saveRDS(betamlm, paste0(workingdata, "vbetamlm.rds"))
} else {
  betahet <- readRDS(paste0(workingdata, "vbetahet.rds"))
  betanob <- readRDS(paste0(workingdata, "vbetanob.rds"))
  betamlm <- readRDS(paste0(workingdata, "vbetamlm.rds"))
}


###################################################
### code chunk number 42: vmlmsim10
###################################################
plot(y ~ x, data = dat, type = "n", ylim = c(-5, 45), xlim = c(15, 40))
for (i in 1:1000){
	abline(betahet[i, 1], betahet[i, 2], lwd = 1, col = "blue")
}
abline(beta0, beta1, col = "black")
for (i in 1:1000){
	abline(betamlm[i, 1], betamlm[i, 2], lwd = 1, col = "red")
}
abline(beta0, beta1, col = "black")


###################################################
### code chunk number 43: vmlmsim20a
###################################################
  xrange <- range(betahet[ ,2])
  hist(betahet[ ,2], prob = TRUE, breaks = 50, main = "OLS Slope Estimates", xlim = xrange)
  lines(density(betahet[ ,2]))
  abline(v = beta1, lty = 4, lwd = 3, col = "black")


###################################################
### code chunk number 44: vmlmsim20b
###################################################
  hist(betamlm[ ,2], prob = TRUE, breaks = 50, main = "MLM Slope Estimates", xlim = xrange)
  lines(density(betamlm[ ,2]))
  abline(v = beta1, lty = 4, lwd = 3,  col = "black")


###################################################
### code chunk number 45: vmlmsim30
###################################################
xh <- hist(betahet[ ,2], prob = TRUE, breaks = 50, plot = FALSE)
plot(rep(0, length(xh$breaks)) ~ xh$breaks, type = "n",
	ylim = magRange(xh$density, c(1, 2)), ylab = "Density", xlab = "Slope Estimates")
lines(density(betahet[ ,2]), col = "blue", lty = 4)
lines(density(betamlm[ ,2]), col = "red", lty = 1)
abline(v = beta1, lty = 4, lwd = 1, col = "black")
legend("topright", c("OLS", "MLM"), col = c("blue", "red"), lty = c(4,1))


###################################################
### code chunk number 46: vmlmsim21a
###################################################
  xrange <- range(betahet[ , 1])
  hist(betahet[ ,1], prob = TRUE, breaks = 50, main = "OLS Intercept Estimates", xlim = xrange)
  lines(density(betahet[ , 1]))
  abline(v = beta0, lty = 4, lwd = 3, col = "black")


###################################################
### code chunk number 47: vmlmsim21b
###################################################
  hist(betamlm[ ,1], prob = TRUE, breaks = 50, main = "MLM Intercept Estimates", xlim = xrange)
  lines(density(betamlm[ , 1]))
  abline(v = beta0, lty = 4, lwd = 3,  col = "black")


###################################################
### code chunk number 48: simulating-mlm-1.Rnw:1183-1185
###################################################
n <- 20
simdata <- gen1(M = M, n = n, beta = c(beta0, beta1), bsd = 20, xbari = 50, xsd = 30,  xsdi = 0)


###################################################
### code chunk number 49: dec10a
###################################################
plot(y ~ x, type = "n", data = simdata[["dat"]], ylim = magRange(simdata[["dat"]][ ,"y"], 1.3))
for(mm in 1:M){
   abline(a = beta0 + simdata[["b"]][mm], b = beta1, lty = mm, col = "gray80")
}


###################################################
### code chunk number 50: dec10b
###################################################
plot(y ~ x, type = "n", data = simdata[["dat"]], ylim = magRange(simdata[["dat"]][ ,"y"], 1.3))
for(mm in 1:M){
   abline(a = beta0 + simdata[["b"]][mm], b = beta1, lty = mm, col = "gray80")
}
points(ynoe ~ x, data = simdata[["dat"]])


###################################################
### code chunk number 51: gen20
###################################################
gen2 <- function (beta = c(3, 0.5), xbari = 25, xsdi = 5, 
  xsd = 4, M = 10, n = 3, bsd = 2, esd = 4){
  b <- rnorm(M, m = 0, bsd)
  xbars <- rnorm(M, m = xbari, sd = xsdi)
  dat <- data.frame(i = rep(1:M, each = n))
  dat$x <-  rep(xbars, each = n) + rnorm(M*n, m = 0, sd = xsd)
  dat$b <- rep(b, each = n)
  dat$ynoe <- beta[1] + beta[2] * dat$x + dat$b
  dat$y <- dat$ynoe + rnorm(M*n, m=0, sd = esd)
  list(dat = dat, b = b)
}


###################################################
### code chunk number 52: bad10
###################################################
M <- 10; beta0 <- 3; beta1 <- 0.5
simdata <- gen2(M = M, beta = c(beta0, beta1), bsd = 20, xsdi = 10)
dat <- simdata[["dat"]]
b <- simdata[["b"]]


###################################################
### code chunk number 53: dec20a
###################################################
plot(y ~ x, type = "n", data = simdata[["dat"]], ylim = magRange(simdata[["dat"]][ ,"y"], 1.3))
for(mm in 1:M){
   abline(a = beta0 + simdata[["b"]][mm], b = beta1, lty = mm, col = "gray80")
}


###################################################
### code chunk number 54: dec20b
###################################################
plot(y ~ x, type = "n", data = simdata[["dat"]], ylim = magRange(simdata[["dat"]][ ,"y"], 1.3))
for(mm in 1:M){
   abline(a = beta0 + simdata[["b"]][mm], b = beta1, lty = mm, col = "gray80")
}
points(ynoe ~ x, data = simdata[["dat"]])


###################################################
### code chunk number 55: gen15
###################################################
M <- 10; beta0 <- 3; beta1 <- 0.5
simdata <- gen1(M = M, beta = c(beta0, beta1), xsdi = 0)
dat <- simdata[["dat"]]
b <- simdata[["b"]]


###################################################
### code chunk number 56: gen15ols
###################################################
m15 <- lm(y ~ x, data = dat)
summary(m15)


###################################################
### code chunk number 57: dec30
###################################################
library(rockchalk) 
plot(y ~ x, type = "n", ylim = magRange(dat$y, 1.5), data = dat)
for(mm in 1:M){
   abline(a = beta0 + b[mm], b = beta1, lty = mm, col = "gray80")
}
abline(m15, col = "blue", lwd = 2)


###################################################
### code chunk number 58: gen20
###################################################
beta0 <- 3; beta1 <- 0.5; xbari <- 5; xsdi <- 5;
M <- 4; n <- 3; bsd <- 2; esd <- 4
xbars <- 20 + rnorm(M, m = xbari, sd = xsdi)
b <- rnorm(M, m = 0, bsd)
dat <- data.frame(i = rep(1:M, each = n))
dat$b <- rep(b, each = n)
dat$x <- unlist(lapply(xbars, function(xbar) { x <- xbar + rnorm(n, 4) }))
dat$y <- beta0 + beta1 * dat$x + rnorm(M*n, esd) + dat$b


###################################################
### code chunk number 59: gen30
###################################################
beta0 <- 3; beta1 <- 0.5; xbari <- 25; xsdi <- 5;
M <- 4; n <- 3; bsd <- 2; esd <- 4
xbars <- rnorm(M, m = xbari, sd = xsdi)
b <- rnorm(M, m = (xbari - xbars), bsd)
dat <- data.frame(i = rep(1:M, each = n))
dat$b <- rep(b, each = n)
dat$x <- unlist(lapply(xbars, function(xbar) { x <- xbar + rnorm(n, 4) }))
dat$y <- beta0 + beta1 * dat$x + rnorm(M*n, esd) + dat$b


###################################################
### code chunk number 60: dot10
###################################################
library(lattice)
dotplot(i ~ y, data = dat, xlab = "Observed Outcome")



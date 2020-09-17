opar <- par(las = 1, ann = FALSE)

phivals <- seq(4.25, -2.25, by = -0.5)

SD <- 1.1

probs <- pnorm(phivals, mean = 1, sd = SD, lower.tail = FALSE)

diams <- 2 ^ (-phivals)

grains <- data.frame(diams, probs)

grains <- transform(out, diams = round(diams,3), probs = round(probs,4))

plot(probs ~ diams, data = grains, type = 'l')

abline(h = 0.5, v = 0.5, lty = 2)

## shows CDF of a very skew distn with long tail to the right. Median
## grain size is approx. 0.5mm. Mean grain size?

## What is the distn of grain size on log scale?

plot(probs ~ diams, data = grains, type = 'l', log = 'x')

abline(h = 0.5, v = 0.5, lty = 2)

## This shows CDF of a symmetric distn, very close to normal.  Median
## (and mean) is log(0.5). To estimate sd note that standard normal
## deviate corresponding to 0.9442 is approx 1.59, so log(1.6818) -
## log(0.5) = 1.59 * sd, sd = 1.213/1.59 = 0.763

## Find a range of sizes that includes 95% of grains.

On the log scale, where the distn is normal, this is

## log(.5) +/- 1.96 * 0.763,  so interval is (-2.189, 0.802), or
## (log(0.11),log 2.23)). Display on graph:

## abline(v = c(0.11, 2.23), lty = 3)

vals <- seq(-3.156, 1.156, len = 50)

rug(2 ^ vals, col = 'blue')

## symmetric about the mean/median.  Back to linear scale on x axis 

plot(probs ~ diams, data = grains, type = 'l')

abline(h = 0.5, v = 0.5, lty = 2)

## show the 95% interval:
vals <- seq(from = .11, to = 2.23, len = 50)

rug(vals, col = 'blue')

## not symmetric, reflecting skewness. And not equi-tailed.

par(opar)

Mean diameter? Use a known result for the log-normal distn. If log(Y)
is normally distd with mean m and variance sigma^2, mean value of Y is
exp(m + 0.5 * sigma^2). Here m = log(0.5), sigma = 0.763, and mean
diam is

exp(log(0.5)  + 0.5 * .763^2) = 0.669

For a positively skew distn, mean > median.

## Ignore the rest

## breaks <- diams
## nB <- length(breaks)
## h <- diff(breaks); dens <- diff(probs)/h
## r <- range(breaks)
## curve(dlnorm(x, m, SD),
##       from = r[1], to = r[2], las = 1, ann = FALSE)
## rect(breaks[-nB], 0, breaks[-1], dens)

## out <- transform(out, density = probs/widths)
## sf <- with(out, stepfun(diams, c(0, density)))
## plot(sf, do.points = FALSE, las = 1, ann = FALSE)
## points(density ~ diams, type = 'h', data = out)
## abline(h = 0)

# draw sample of size n from N(0,1), calculate sample mean.
# Repeat 2000 times.
# Compare histogram of 2000 sample means to normal distribution
# with mean zero and variance 1/n.
# E.g. with n = 100

n <- 100
SE <- sqrt(1/n)
hist(replicate(2000, mean(rnorm(n))), freq = FALSE, main = "", las = 1)
curve(dnorm(x,sd = SE), qnorm(0.005, sd = SE), qnorm(0.995, sd =SE), add=T)

# draw a sample of size n from N(0,1), calculate corrected sum of squares
# as (n-1)*(sample variance). Repeat 2000 times.
# Compare histogram of the 2000 sums of squares to
# chi-squared distribution with (n-1) degrees of freedom.
# E.g. with n = 10

n <- 10
hist(replicate(2000,(n-1)*var(rnorm(n))), freq = FALSE, main = "", las = 1)
curve(dchisq(x, df = n-1), qchisq(0.005, df = n-1), qchisq(.995, df = n-1),
add = T)

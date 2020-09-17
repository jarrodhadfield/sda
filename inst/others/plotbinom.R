plotbinom <- function (n) 
{
    x <- 0:n
    scaled <- (2 * x - n)/sqrt(n)
    plot(scaled, pbinom(x, n, 0.5), type = "s", xlim = c(-3, 
        3), ylim = c(0, 1), ann = FALSE, las = 1,
    panel.last = plot(pnorm, -3, 3, col = 'red', add = TRUE))
}

op <- options(device.ask.default = TRUE)
readline("\nHit <Return> to start: ")

plotbinom(n = 16)
plotbinom(n = 100)
plotbinom(n = 400)
plotbinom(n = 1600)
plotbinom(n = 2500)

options(op)

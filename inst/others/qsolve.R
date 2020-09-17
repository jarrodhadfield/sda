qsolve <- function (x) 
{
    if (length(x) != 3) 
        stop("Input must be single vector with 3 values")
    a <- x[1]
    b <- x[2]
    c <- x[3]
    test <- b^2 - 4 * a * c
    if (test < 0) 
        stop("No real roots")
    (sqrt(test) * c(-1, 1) - b)/(2 * a)
}

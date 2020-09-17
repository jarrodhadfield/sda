    iterations <- 1000
    genotypes <- c("AA", "Aa", "aa")
    ian.geno <- character(iterations)
    A.frq <- numeric(iterations)
    P <- array(c(1, 0, 0, 0.5, 0.5, 0, 0, 1, 0, 0.5, 0.5, 0, 
        0.25, 0.5, 0.25, 0, 0.5, 0.5, 0, 1, 0, 0, 0.5, 0.5, 0, 
        0, 1), dim = c(3, 3, 3), dimnames = list(genotypes,
        genotypes, genotypes))
    Ann <- Brian <- Clare <- Diane <- Eric <- Fred <- Ian <- "Aa"
    Gina <- Henry <- "Aa"
    Jane <- "aa"
    p <- runif(1)
    unaffected <- c(1, 1, 0)
    for (i in 1:iterations) {
        q <- 1 - p
        HW <- c(p^2, 2 * p * q, q^2)
        probs <- HW * P[Clare, Brian, ] * P[Diane, Brian, ] * 
            unaffected
        Ann <- sample(genotypes, 1, prob = probs)
        probs <- HW * P[Clare, Ann, ] * P[Diane, Ann, ] * unaffected
        Brian <- sample(genotypes, 1, prob = probs)
        probs <- P[, Ann, Brian] * P[Ian, Fred, ] * unaffected
        Clare <- sample(genotypes, 1, prob = probs)
        probs <- P[, Ann, Brian] * P[Fred, Eric, ] * P[Gina, 
            Eric, ] * unaffected
        Diane <- sample(genotypes, 1, prob = probs)
        probs <- HW * P[Fred, Diane, ] * P[Gina, Diane, ] * unaffected
        Eric <- sample(genotypes, 1, prob = probs)
        probs <- P[, Diane, Eric] * P[Ian, Clare, ] * unaffected
        Fred <- sample(genotypes, 1, prob = probs)
        probs <- P[, Clare, Fred]
        Ian <- sample(genotypes, 1, prob = probs)
        anybody <- c(Ann, Brian, Clare, Diane, Eric, Fred, Gina, 
            Henry, Ian, Jane)
        no.A <- sum(anybody == "Aa") + 2 * sum(anybody == "AA")
        no.a <- 20 - no.A
        p <- rbeta(1, no.A + 1, no.a + 1)
        ian.geno[i] <- Ian
        A.frq[i] <- p
    }
table(ian.geno)
summary(A.frq)    
plot(A.frq)

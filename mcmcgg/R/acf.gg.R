acf.gg <-
function(x, iter, nchain) {
    history <- matrix(NA, nrow = iter, ncol = nchain)
    start <- 1
    end <- iter
    for (index in seq(1, nchain)) {
        history[, index] <- x[start:end]
        start <- end + 1
        end <- end + iter
    }
    lagVector <- rep(0:100, nchain)
    acfVector <- rep(NA, nchain * 101)
    chainNumber <- rep(1:nchain, each = 101)
    start <- 1
    end <- 101
    for (index in seq(1, nchain)) {
        acfIndex <- acf(history[, index], lag.max = 100, plot = FALSE)
        print(index)
        acfVector[start:end] <- acfIndex$acf
        chainNumber[start:end] <- index
        start <- end + 1
        end <- end + 101
    }
    results <- data.frame(lag = lagVector,
                          acf = acfVector,
                          chain = paste("Chain", chainNumber))
    results$max <- ifelse(results$acf <= 0, 0, results$acf)
    results$min <- ifelse(results$acf > 0, 0, results$acf)
    ggplot(results, aes(ymax = max, ymin = min, x = lag)) + geom_linerange() + 
        facet_grid(. ~ chain) + theme_bw() + scale_x_continuous("Lag") + 
        scale_y_continuous("ACF")
}

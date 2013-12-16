bgr.gg <-
function(x, iter, nchain, T, qtype = 7) {
    history <- matrix(NA, nrow = iter, ncol = nchain)
    start <- 1
    end <- iter
    for (index in seq(1, nchain)) {
        history[, index] <- x[start:end]
        start <- end + 1
        end <- end + iter
    }
    width <- matrix(NA, ncol = nchain + 1, nrow = length(T))
    for (chain in 1:nchain) {
        for (index in 1:length(T)) {
            startIter <- T[index]
            start <- floor(startIter/2 + 1)
            end <- startIter
            width[index, chain] <- quantile(history[start:end, chain], 0.9, 
                                            type = qtype) - 
                quantile(history[start:end, chain], 0.1, type = qtype)
        }
    }
    for (index in 1:length(T)) {
        startIter <- T[index]
        start <- ceiling(startIter/2 + 1)
        end <- startIter
        nx <- rep(NA, nchain * (end - start + 1))
        start2 <- start
        end2 <- end
        nxStart <- 1
        nxEnd <- (end - start + 1)
        for (chain in 1:nchain) {
            nx[nxStart:nxEnd] <- x[start2:end2]
            start2 <- start + (chain * iter)
            end2 <- start2 + (end - start)
            nxStart <- nxEnd + 1
            nxEnd <- nxEnd + (end - start + 1)
        }
        width[index, (nchain + 1)] <- quantile(nx, 0.9, type = qtype) - 
            quantile(nx, 0.1, type = qtype)
    }
    W <- apply(width[, 1:nchain], 1, mean)
    B <- width[, (nchain + 1)]
    R <- B/W
    sW <- W/max(W, B)
    sB <- B/max(W, B)
    results <- data.frame(x = T + 1, W = sW, B = sB, R = R)
    ggplot(results, aes(x = x)) + geom_line(aes(y = R), linetype = 1) + 
        geom_line(aes(y = B), linetype = 2, color = "blue") + 
        geom_line(aes(y = W), linetype = 3, color = "red") + 
        theme_bw() + scale_x_continuous("Starting Iteration") + 
        scale_y_continuous("") + geom_hline(yintercept = 1.05, linetype = 5)
}

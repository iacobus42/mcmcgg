history.gg <-
function(x, iter, nchain, thin = 1) {
    results <- data.frame(history = x, 
                          iteration = rep(1:iter, nchain),
                          chain = paste("Chain", rep(1:nchain, each = iter)))
    if (thin > 1) {
        results <- results[results$iteration %% thin == 0, ]
    }
    ggplot(results, aes(y = history, x = iteration)) + geom_line() + 
        facet_grid(. ~ chain) + scale_x_continuous("Iteration Number") + 
        scale_y_continuous("Iteration Value") + theme_bw() 
}

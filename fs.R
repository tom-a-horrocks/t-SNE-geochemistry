#' Applies recursive feature elimination with Random Forests as described in:
#' 
#' Gregorutti, B., Michel, B., Saint-Pierre, P., 2017. 
#'   Correlation and variable importance in random forests. 
#'   Stat. Comput. 27, 659-678. 
#'   https://doi.org/10.1007/s11222-016-9646-1
#' 
#' The median feature importance over multiple runs ('n_runs') is used to reduce random effects,
#'
#' @param X Input data frame with features as columns and instances as rows.
#' @param Y A factor holding the classification labels for X
#' @param n_trees The number of trees used in the Random Forest (recommended: 1000)
#' @param n_runs The number of Random Forests used in each iteration (recommended: 20).
#' @param subsample If true, X is subsampled so that the class distribution in Y is equal.
#'                  If false, features informative to minority classes may be eliminated earlier.
#' @return A data frame of the classification error rates for each iteration:
#'          Columns: elements, in order of elimination (worst to best)
#'          Rows: Random Forest classification error rates per run
#'
random_forest_RFE <- function(X, Y, n_trees=1000, n_runs=20, subsample=TRUE) {
  library(randomForest)
  n_elements <- ncol(X)
  
  # List of lists to store the RF's error rates per run, for each iteration.
  all_error_rates <- vector(mode = "list", n_elements)
  
  # Tracks which element was removed after each iteration.
  removed_elements <- vector(mode = "character", n_elements)
  
  # Outer loop: removing worst element
  for (i in 1:n_elements) {
    # Inner loop: evaluate current element choice
    importances <- vector(mode = "list", n_runs)
    error_rates <- vector(mode = "numeric", n_runs)  
    set.seed(42)
    for (j in 1:n_runs) {
      if (subsample) {
        model <- randomForest(X[, !names(X) %in% removed_elements, drop = FALSE], y = Y, ntree = n_trees, importance = TRUE, sampsize = rep(min(table(Y)), nlevels(Y)))
      } else {
        model <- randomForest(X[, !names(X) %in% removed_elements, drop = FALSE], y = Y, ntree = n_trees, importance = TRUE)
      }
      
      importances[[j]] <- importance(model, type=1, scale=FALSE)
      # mean OOB error rates over all trees
      error_rates[[j]] <- mean(model$err.rate[,'OOB'])
    }
    
    all_error_rates[[i]] <- error_rates
    
    # Get element with lowest median importance over runs
    worst_el <- tail(names(sort(apply(as.data.frame(importances), 1, median), decreasing = TRUE)), n=1)
    removed_elements[[i]] <- worst_el
    
    print(paste("Mean error rate:", mean(error_rates)))
    print(paste('Worst performing element:', worst_el))
    
  }
  
  ret <- as.data.frame(t(do.call(rbind, all_error_rates)))
  colnames(ret) <- removed_elements
  
  # Data frame containing error rates
  # Each row is the error rate for a run
  # Column headers give the element with lowest median importance in that iteration
  return(ret)
}

#' Creates a plot showing the classification error rate and lowest ranked feature for each iteration.
#'
#' @param error_rates The dataframe returned by 'random_forest_RFE'.
#'
plot_RFE <- function(error_rates) {
  library(ggplot2)
  # Compute the y-values for error rates: mean (middle) and error bars. The
  # error bars represent the 95% confidence interval based on t-distribution 
  #   with n_runs -1 degrees of freedom.
  lower_middle_upper <- function(vals) {
    av <- mean(vals)
    ci <- qt(0.975, df=length(vals) - 1) * sd(vals) / sqrt(length(vals))
    return (c(
      lower  = mean(vals) - ci,
      middle = mean(vals),
      upper  = mean(vals) + ci
    ))
  }
  
  plot.df <- as.data.frame(t(sapply(error_rates, lower_middle_upper)))
  plot.df$NumElements <- ncol(X):1
  
  y_label_nudge <- 0.05 # Places the element label above the point on the chart
  ggplot(plot.df, aes(x = NumElements)) +
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    geom_point(aes(y = middle), shape=21, fill="white") +
    geom_text(aes(label = rownames(plot.df), y = middle + y_label_nudge), size = 3) +
    scale_x_reverse(breaks = seq(1, ncol(X),1)) + 
    labs(x = 'No. elements not yet eliminated', y = 'Out-of-bag classification error rate\nusing remaining elements') + 
    theme_bw()
}

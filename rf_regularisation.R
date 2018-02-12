#' Predicts labels Y from data X using a Random Forest with minimum terminal node size 'nodesize'.
#'
#' @param X Input data frame with features as columns and instances as rows.
#' @param Y A factor holding the classification labels for X
#' @param nodesize Minimum terminal node size for the Random Forest. Higher values result in shallower trees
#'                 and therefore a more regularised forest.
#' @param n_trees The number of trees used in the Random Forest (recommended: 1000)
#' @param subsample If true, X is subsampled so that the class distribution in Y is equal.
#'                  If false, features informative to minority classes may be underutilised.
#' @return The Random Forest's predicted label for each instance, aligned to Y.
#'
predict_Ys <- function(X, Y, nodesize, n_trees=1000, subsample=TRUE) {
  library(randomForest)
  
  set.seed(42)
  if (subsample) {
    return(randomForest(x = X, y = Y, ntree = 1000, nodesize = nodesize, sampsize = rep(min(table(Y)), nlevels(Y)))$predicted)
  } else {
    return(randomForest(x = X, y = Y, ntree = 1000, nodesize = nodesize)$predicted)
  }  
}
#' Applies t-SNE to the input data.
#' Input data is ilr-transformed, which is equivalent to t-SNE using Aitchison distance instead of the Euclidean.
#' Multiple (n_runs) embeddings are created, and that with least error is returned.
#' 
#' @param X The input data frame
#' @param n_runs The number of candidate embeddings to create
#' @param use_elements Column names of elements to use (if blank, all are used)
#' @return The lowest-error 2D embedding as a data frame.
#'
best.tsne <- function(X, n_runs=10, use_elements=c()) {
  library(robCompositions)
  library(Rtsne)  
  
  # Import imputed composition
  print(colnames(X))
  
  # Subset of elements (if chosen)
  if (length(use_elements) > 0) {
    print('Filtering out the following elements')
    print(use_elements)
    X <- X[,use_elements]
  }
  
  # Check for duplicates
  if (nrow(unique(X)) != nrow(X)) {
    print('Data contains duplicates')
    return(-1)
  }
  
  # Data transformation
  X_trans <- pivotCoord(X, 1)
  rownames(X_trans) <- rownames(X)  

  # Run t-sne n_runs times (stochastic)
  set.seed(42)
  tsnes <- vector("list", n_runs)
  for (i in 1:n_runs){
    print(paste0('Run #', i))
    tsne_out <- Rtsne(X_trans, initial_dims = ncol(X_trans), verbose=FALSE, check_duplicates = FALSE, max_iter = 5000, PCA = FALSE)
    tsnes[[i]] <- tsne_out
    print(paste0('Embedding error #', tail(tsne_out$itercosts, n=1)))
  }
  
  # Find the t_sne with the lowest embedding
  errors <- vector(mode="numeric", length=n_runs)
  i <- 1
  for (tsne in tsnes) {
    errors[[i]] <- tail(tsne$itercosts, n=1)
    i <- i + 1
  }
  best_i <- which.min(errors)
  tsne_out <- tsnes[[best_i]]
  
  # Export results
  xy <- as.data.frame(tsne_out$Y)
  rownames(xy) <- rownames(X_trans)
  return(xy)
}

#' Plots the embedding overlain with each element's concentration (log-scaled).
#' 
#' @param embedding The t-SNE embedding (2D data frame)
#' @param X The original data (used for colouring)
#' @param layout_cols How many columns the plot grid has
#' 
#' @return A ggplot of the embedding coloured by all columns in X.
#'
plot_concentrations <- function(embedding, X, layout_cols) {
  library(pals) # For kovesi.rainbow
  require(scales) # for squish
  
  normalise <- function(x) { # logs and then normalises
    logx <- log(x)
    # drop top and bottom percent for plotting purposes
    trimmed <- logx[logx > quantile(logx, 0.01)]
    trimmed <- trimmed[trimmed < quantile(logx, 0.99)]
    
    return ((logx - min(trimmed)) / (max(trimmed) - min(trimmed)))
  }
  
  X_normalised <- as.data.frame(lapply(X, normalise))
  df.plot <- as.data.frame(cbind(embedding, X_normalised))
  df.plot <- reshape(
    df.plot, 
    direction = "long", 
    varying = colnames(X), 
    v.names = 'concentration', 
    timevar='element', 
    times=colnames(X))
  
  return(
    ggplot(data=df.plot, aes(x=V1, y=V2)) +
    geom_point(aes(colour=concentration), size=0.5) +
    scale_colour_gradientn(colours = kovesi.rainbow(13), limits=c(0,1.01), oob=squish, breaks=c(0,1), minor_breaks=NULL, labels=c('min', 'max')) + 
    theme_bw() + 
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 1,
          legend.position = "bottom",
          labels=NULL) +
    facet_wrap(~element, ncol = layout_cols) + 
    labs(colour = NULL)
  )
}

#' Plots the t-SNE embedding coloured by the labels Y.
#' 
#' @param embedding The t-SNE embedding (2D data frame)
#' @param Y The labels (used for colouring)
#' @return A ggplot of the embedding coloured by Y
plot_labels <- function(embedding, Y) {
  library(pals) # For kovesi.rainbow
  require(scales) # for squish
  
  df.plot <- as.data.frame(cbind(embedding, Y))
  return(
    ggplot() + 
    geom_point(data = df.plot, aes(x=V1, y=V2, color=Y)) +
    scale_fill_manual(values=kovesi.rainbow(4)) +
    coord_fixed() + 
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          plot.margin=grid::unit(c(0,0,-1,0), "mm"))
  )
}

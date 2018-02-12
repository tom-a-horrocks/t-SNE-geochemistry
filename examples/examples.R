library(stringr) # used for making filenames only

# We will be predicting which river an assay was sourced from.
# X: Water sample assays
# Y: Name of river sample was sourced from
data("Hydrochem", package = "compositions")
X = Hydrochem[c("H", "Na", "K", "Mg", "Ca", "Sr", "Ba", "NH4", "Cl", "NO3", "PO4", "SO4", "HCO3", "TOC")]
Y = Hydrochem[,"River"] # This must be a factor

# =====================================================
# Example: RFE w/ Random Forest
# Which elements are relevant for river classification?
# =====================================================
source("../fs.R")

# Run RFE and save error rates for plotting
error_rates <- random_forest_RFE(X, Y, n_trees=50, n_runs=10, subsample=TRUE)
print(sapply(error_rates, mean)) # mean error rates w/ current least important element

# Chart each iteration's error rates with 95% confidence level bars
plot_RFE(error_rates)
ggsave('./test_plot.png')

# Classification error rate is only substantially effected by removal of SO4, Sr, Ca, and Mg.

# ======================================================
# Example: t-SNE w/ Aitchison distance & element subsets
# How are elements related to source river?
# ======================================================
source("../tsne.R")

# Create two embeddings: one with all elements, one with the predictive subset from previous example.
# The function chooses the best-of-n_runs embedding (according to embedding error)
# The input dataset is ilr-transformed, which is equivalent to t-SNE using the Aitchison distance.
els <- c('SO4', 'Sr', 'Ca', 'Mg')
full_embedding   <- best.tsne(X, n_runs = 10)
subset_embedding <- best.tsne(X, n_runs = 10, use_elements = els)

# Plot the embeddings coloured by elements and by river source
plot_concentrations(full_embedding, X, layout_cols = 5)
ggsave('./full_embedding_concentrations.png', scale = 1, limitsize = TRUE)
plot_labels(full_embedding, Y)
ggsave('./full_embedding_rivers.png', scale = 1, limitsize = TRUE)

# Same plots for subset embedding
plot_concentrations(subset_embedding, X[,els], layout_cols = 2)
ggsave('./subset_embedding.png', scale = 1, limitsize = TRUE)
plot_labels(subset_embedding, Y)
ggsave('./subset_embedding_rivers.png', scale = 1, limitsize = TRUE)

# The subset embedding labels are generally better clustered

# =====================================================
# Example: Regularising RF by visualising w/ embeddings
# =====================================================
source("../rf_regularisation.R")

# Predict river sources for varying nodesize, and plot
# predictions on the subset embedding.
node_sizes <- c(1, 2, 4, 8, 16, 32, 64, 128, 256)
for (nodesize in node_sizes) {
  Y_pred <- predict_Ys(X, Y, nodesize, n_trees=1000, subsample=TRUE)
  
  plot_labels(subset_embedding, Y_pred) + 
    labs(title = paste0('Node size = ',nodesize))
  
  ggsave(paste0('./rf', '_', str_pad(nodesize, 3, "left", "0"), '.png'))
}

# Label noise is not an issue for this dataset (river is known), so nodesize=1 is fine.

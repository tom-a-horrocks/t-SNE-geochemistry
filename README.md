# t-SNE-geochemistry
This repository holds scripts demonstrating methods for the journal article "Geochemical characterisation of rock hydration processes using t-SNE" (submitted). The dataset used within the journal article is not open file, so a public chemical assay database has been used in this repository.

## How to use
The "examples/examples.R" file walks through the available functions with an open file chemical assay database (see https://www.rdocumentation.org/packages/compositions/versions/1.40-1/topics/Hydrochem).

## Testing
The "examples/examples.R" file should create figures which match those in "examples/examples_out".

## Walkthrough
We apply the techniques in "Geochemical characterisation of rock hydration processes using t-SNE" to a hydrochemical dataset containing water sample assays and their source river name.

### Random Forest recursive feature elimination
The figure below shows that the Mg, Ca, Sr, and SO4 concentrations are highly predictive of the river source.
![RFE](/examples/examples_out/test_plot.png)

### t-SNE embeddings
The embeddings below were generated using Rtsne, and have used the Aitchison distance rather than the Euclidean distance. The 'subset embedding' uses only Mg, Ca, Sr, and SO4 as input.
#### Full embedding
![tsne full embedding concentrations](/examples/examples_out/full_embedding_concentrations.png)
![tsne full embedding labels](/examples/examples_out/full_embedding_rivers.png)
#### Subset embedding
![tsne subset embedding concentrations](/examples/examples_out/subset_embedding.png)
![tsne subset embedding labels](/examples/examples_out/subset_embedding_rivers.png)

### Regularised Random Forest
The Random Forest is regularised for increasing minimum terminal node sizes. As this dataset presumably has little label noise (the river is known), the regularisation is ultimately unneccessary.
![](/examples/examples_out/rf_001.png)
![](/examples/examples_out/rf_008.png)
![](/examples/examples_out/rf_064.png)

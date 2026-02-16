rm(list = ls())
install.packages("tidyverse")
library(tidyverse)

# read in result files
eigenValues <- read_delim("PATH/Sheep05_afterQC_pruned_pca.eigenval", delim = " ", col_names = F)
eigenVectors <- read_delim("PATH/Sheep05_afterQC_pruned_pca.eigenvec", delim = " ", col_names = F)
eigenVectors
eigenValues
## Proportion of variation captured by each vector
eigen_percent <- round((eigenValues / (sum(eigenValues))*100), 2)

# PCA plot
ggplot(data = eigenVectors) +
  geom_point(mapping = aes(x = X3, y = X4,  color = X1, shape = X1), size = 2, show.legend = T ) +
  geom_hline(yintercept = 0, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(title = "PCA",
       x = paste0("Principal component 1 (",eigen_percent[1,1]," %)"),
       y = paste0("Principal component 2 (",eigen_percent[2,1]," %)"),
       colour = "Breed:", shape = "Breed:") +
  theme_minimal()


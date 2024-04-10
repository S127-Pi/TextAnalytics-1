library(data.table)
library(quanteda)
library(tm)
library(dplyr)
library(SnowballC)
library(tidytext)
library(text2vec)
library(factoextra)

df <- fread("data/fully_cleaned_reviews.csv")

corpus <- Corpus(VectorSource(df$Review))

tdm <- TermDocumentMatrix(corpus)
tdm_matrix <- as.matrix(tdm)

counts <- rowSums(tdm_matrix) 
sortedcount <- counts%>% sort(decreasing=TRUE)
nwords<-200
sortednames <- names(sortedcount[1:nwords])

dtm_matrix <- t(tdm_matrix)

pca_results <- prcomp(dtm_matrix, 
                      scale = FALSE, # Scale is false because all data is on the same scale so we don't have to rescale
                      rank. = 40) # Rank specifies maximum number of principal components

pca_results_backup <- pca_results 

fviz_screeplot(pca_results,ncp=40)

ncomp <- 4

j<-1 # For first dimension
toplist <- abs(pca_results$rotation[,j]) %>% sort(decreasing=TRUE) %>% head(10)
topwords <- (names(toplist)) # Save most important words
for (j in 2:ncomp){
  toplist <- abs(pca_results$rotation[,j]) %>% sort(decreasing=TRUE) %>% head(10)
  topwords <-cbind( topwords , (names(toplist))) # Add most important words of the dimension
}

# Graph variables of PCA: Colors indicate amount of variation explained by the dimensions
#axeslist <- c(1, 2)
#fviz_pca_var(pca_results, axes=axeslist 
#             ,geom.var = c("arrow", "text")
#             ,col.var = "contrib", # Color by contributions to the PC
#             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # colors to use
#             repel = TRUE     # Avoid text overlapping
#)


# Get factor loadings (relatedness) and do rotation to new coordinate system
rawLoadings     <- pca_results$rotation[sortednames,1:ncomp] %*% diag(pca_results$sdev, ncomp, ncomp)
rotated <- varimax(rawLoadings) # rotate loading matrix
pca_results$rotation <- rotated$loadings 
pca_results$x <- scale(pca_results$x[,1:ncomp]) %*% rotated$rotmat

# Select the most important words per rotated dimension
j<-1 # For first dimension
toplist <- abs(pca_results$rotation[,j]) %>% sort(decreasing=TRUE) %>% head(10)
topwords <- (names(toplist)) # Save most important words
for (j in 2:ncomp){
  toplist <- abs(pca_results$rotation[,j]) %>% sort(decreasing=TRUE) %>% head(10)
  topwords <-cbind( topwords , (names(toplist))) # Add most important words of the dimension
}

# Display
topwords

# Use smaller dataset: Keep only 200 reviews to plot
pca_results_backup <- pca_results 
pca_results_small <- pca_results
pca_results_small$x <- pca_results_small$x[1:200,] 
pca_results <- pca_results_small

# Plot PCA results after rotation - variables
axeslist <- c(1, 2)
fviz_pca_var(pca_results, axes=axeslist 
             ,geom.var = c("arrow", "text")
             ,col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # colors to use
             repel = TRUE     # Avoid text overlapping
)


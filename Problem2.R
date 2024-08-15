##Problem 2
library(psych)

# Importing the data
wine <- read.csv("C:\\Users\\HP\\Desktop\\AP\\winequality-red.csv",sep=";") #Here we are using sep() function to separate the columns in the wine dataset

# finding the optimal number of factors

  #1 by checking Eigenvalues
  eg <- eigen(cor(wine))
  eigenvalues <- eg$values
  eigenvalues
  
  #2 by checking scree plot
  PCA <- prcomp(wine)
  screeplot(PCA, type='l')

#fitting orthogonal factor model
fit <- factanal(wine, factors=4, rotation='varimax')
fit

#Factor loadings
fa.diagram(fit$loadings)

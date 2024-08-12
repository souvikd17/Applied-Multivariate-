## Problem 1

#Importing the data
Concrete <- read.csv("C:\\Users\\HP\\Desktop\\AP\\Concrete_Data.csv")
which(is.na(Concrete))

#Calculating dispersion matrix (S) and correlation matrix (R)
S <- cov(Concrete)
R <- cor(Concrete)

#Principle Component Analysis with dispersion matrix (S)
egS <- eigen(S)
eigenvaluesS <- egS$values
eigenvectorsS <- egS$vectors

sd_pcaS <- sqrt(eigenvaluesS)
var_pcaS <- eigenvaluesS
explainedVar_S <- eigenvaluesS/ sum(eigenvaluesS)
CumExpVarS <- cumsum(explainedVar_S)

PCAwS <- NULL
PCAwS$sdev <- sd_pcaS
screeplot(PCAwS, type ='l')

#Principle Component Analysis with Correlation matrix (R)
egR <- eigen(R)
eigenvaluesR <- egR$values
eigenvectorsR <- egR$vectors

sd_pcaR <- sqrt(eigenvaluesR)
var_pcaR <- eigenvaluesR
explainedVar_R <- eigenvaluesR / sum(eigenvaluesR)
CumExpVarR <- cumsum(explainedVar_R)

PCAwR <- NULL
PCAwR$sdev <- sd_pcaR
screeplot(PCAwR, type ='l')

#Principle Component Analysis using prcomp
PCAprcomp <- prcomp(Concrete)
PCAprcomp
summary(PCAprcomp)

screeplot(PCAprcomp, type = 'l')

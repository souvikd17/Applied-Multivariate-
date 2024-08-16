#Problem 5

# Importing and cleaning the data
fitness <- read.csv("C:\\Users\\HP\\Desktop\\AP\\FitnessClub.csv")
fitness <- fitness[,-1]

#Splitting the data set
physiological <- fitness[,c(1:3)]
exercise <- fitness[,c(4:6)]

#performing Canonical Correlation Analysis
cca_result <- cancor(physiological, exercise)
cca_result
#test of significance for checking independence
library(candisc)
sig_test <- Wilks(cancor(physiological, exercise))
sig_test

#correlation matrix of 6 variables
cor_mat <- cor(fitness)
cor_mat

#canonical correlation
cc <- cca_result$cor
squared_cc <- cca_result$cor^2
data.frame(cc, squared_cc)

#explicit expressions for the canonical variates
cca_result$xcoef
cca_result$ycoef

#canonical variates
cv_phys1 <- as.matrix(physiological)%*% cca_result$xcoef[,1]
cv_phys2 <- as.matrix(physiological)%*% cca_result$xcoef[,2]
cv_phys3 <- as.matrix(physiological)%*% cca_result$xcoef[,3]
cv_exer1 <- as.matrix(exercise)%*% cca_result$ycoef[,1]
cv_exer2 <- as.matrix(exercise)%*% cca_result$ycoef[,2]
cv_exer3 <- as.matrix(exercise)%*% cca_result$ycoef[,3]

#correlation between physiological variables and their canonical variates
cor(physiological, cbind(cv_phys1, cv_phys2, cv_phys3))

#correlation between exercise variable and their canonical variates
cor(exercise, cbind(cv_exer1, cv_exer2, cv_exer3))

#correlation between physiological variables and canonical variates of exercise variables
cor(physiological, cbind(cv_exer1, cv_exer2, cv_exer3))

#correlation between exercise variable and canonical variates of physiological variables
cor(exercise, cbind(cv_phys1, cv_phys2, cv_phys3))

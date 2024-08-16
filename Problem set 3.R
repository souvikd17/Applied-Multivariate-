# Problem 3
#loading required packages
library(MASS)

#importing and clearing given data
Pottery <- read.csv("C:\\Users\\HP\\Desktop\\AP\\Pottery.csv") 
Pottery <- Pottery[c(1:45),]
data <- Pottery[-c(1)]

#creating distance matrix
DistMatrx <- dist(data)
DistMatrx

#performing Metric Multidimensional Scaling
mds <- cmdscale(DistMatrx, k = 2, eig = TRUE)

#Creating MDS plot
plot(mds$points[,1], mds$points[,2], type='n', 
     xlab = 'MDS Dimension 1', ylab = 'MDS Dimension 2',
     main = 'Metric MDS Plot')
points(mds$points[,1], mds$points[,2], pch = 21, bg = 'cyan',)
text(mds$points[,1], mds$points[,2], pos = 2, cex = 0.7)

#modifying the plot for different kiln
kiln = c(rep(4, 21), rep(8, 12), rep(3, 2), rep(7, 5), rep(6, 5))
plot(mds$points[,1], mds$points[,2], type='p', 
     xlab = 'MDS Dimension 1', ylab = 'MDS Dimension 2',
     main = 'Metric MDS Plot with different kiln',
     col = kiln)

#modifying the plot for different regions
region = c(rep(4, 21), rep(9, 14), rep(3, 10))
plot(mds$points[,1], mds$points[,2], type='p', 
     xlab = 'MDS Dimension 1', ylab = 'MDS Dimension 2',
     main = 'Metric MDS Plot with different region',
     col = region)

# use cmdscale
# use isoMDS under MASS package
# use cancor
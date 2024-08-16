# Problem 4
#loading required packages
library(MASS)

#importing the data
Garden <- read.csv("C:\\Users\\HP\\Desktop\\AP\\GardenFlowers.csv")

#removing missing values
which(is.na(Garden))
Garden <- Garden[,-1]

#performing Non-Metric Multidimensional Scaling
DM <- dist(Garden)
nmds2d <- isoMDS(DM, y = cmdscale(DM, 2), k = 2)
nmds2d

#Creating MDS plot
plot(nmds2d$points[,1], nmds2d$points[,2], type='n', 
     xlab = 'MDS Dimension 1', ylab = 'MDS Dimension 2',
     main = 'Non Metric MDS Plot')
points(nmds2d$points[,1], nmds2d$points[,2], pch = 21, bg = 'cyan',)
text(nmds2d$points[,1], nmds2d$points[,2], pos = 2, cex = 0.7)

#Calculating Kruskal's stress for different dimensions
stress_values <- NULL
dimension <- c(1:5)
for (dim in dimension)
{
  nmds <- isoMDS(DM, k=dim)
  stress_values <- c(stress_values, nmds$stress/100)
}
stress_data <- data.frame('Dimension' = dimension, 'Kruskal stress' = stress_values)
stress_data

#Generating a scree plot for Stress vs Dimensions
plot(stress_data, type = 'b', main = "Scree Plot for Kruskal stress vs Dimension")

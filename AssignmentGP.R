##### Assignment #####

library(reshape2)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(smacof)
library(factoextra)
library(vegan)

## 1st part
view(PAQ_Estelle)
describe(PAQ_Estelle)
PAQ_E_wide=dcast(PAQ_Estelle, id ~ var, value.var="value")
view(PAQ_E_wide)
summary(PAQ_E_wide)
describe(PAQ_E_wide)

# exclude the data with N/A

PAQ_E_wide = PAQ_E_wide[-c(21),]
view(PAQ_E_wide)
describe(PAQ_E_wide)

# check the data set by creating histograms 

PAQ_E_wide %>% 	
  ggplot() +	
  aes(x = age) +	
  geom_histogram()

PAQ_E_wide %>% 	
  ggplot() +	
  aes(x = Q1_cry) +	
  geom_histogram( bins = 50)

PAQ_E_wide %>% 	
  ggplot() +	
  aes(x = Q2_help) +	
  geom_histogram( bins = 50)

PAQ_E_wide %>% 	
  ggplot() +	
  aes(x = Q3_breathe) +	
  geom_histogram( bins = 50)

PAQ_E_wide %>% 	
  ggplot() +	
  aes(x = Q4_freeze) +	
  geom_histogram( bins = 50)

PAQ_E_wide %>% 	
  ggplot() +	
  aes(x = Q5_alien) +	
  geom_histogram( bins = 50)

PAQ_E_wide %>% 	
  ggplot() +	
  aes(x = Q6_inferior) +	
  geom_histogram( bins = 50)

PAQ_E_wide %>% 	
  ggplot() +	
  aes(x = Q7_weep) +	
  geom_histogram( bins = 50)

PAQ_E_wide %>% 	
  ggplot() +	
  aes(x = Q8_Support) +	
  geom_histogram( bins = 50)

PAQ_E_wide %>% 	
  ggplot() +	
  aes(x = Q9_Nerd) +	
  geom_histogram( bins = 50)

# Excluding age, id and sex because we don't need those variables for this study
PAQ_E <- PAQ_E_wide[-c(1, 2, 12)]
view(PAQ_E)
  
# Now examine the principle components extracted from correlations and covariances among the variables
dataE_pcacor=princomp(PAQ_E,cor=TRUE)
summary(dataE_pcacor, loadings=TRUE)

dataE_pcacov=princomp(PAQ_E,cor=FALSE)
summary(dataE_pcacov, loadings=TRUE)

# Plots
plot(dataE_pcacor$scores[,1])
barplot(dataE_pcacor$scores[,1])

## Scree diagram
plot(DataEstelle_pca$sdev^2, xlab = "component number", ylab = "component variable")
plot(dataE_pcacor$sdev^2, xlab = "Component number",
     ylab = "Component variance", 
     type = "l", 
     main = "Scree diagram")

## Log(eigenvalue) diagram
plot(log(dataE_pcacor$sdev^2), 
     xlab = "Component number", 
     ylab = "log(Component variance)", 
     type="l", 
     main = "Log(eigenvalue) diagram")

DataEstelle_pca <- prcomp(PAQ_E, scale = TRUE)
plot(DataEstelle_pca, main = "Barplot of the variances explained by the principal components")
print(DataEstelle_pca)
summary(DataEstelle_pca)

# Deciding on the number of components
# We choose to keep the two first components because:
# we retain just enough components to explain some specified large percentage of the total variation of the original values 
# & we retain just those components with eigenvalues >=1
# So, we retain only the first two components

## Biplot
# I choose to run the biplot with 2 different functions to get a better perspective/view
# about the plot. No matter which function I choose, the result, the plot is the same
ggbiplot(DataEstelle_pca, main = "Biplot of the first two principle components")
fviz_pca_biplot(DataEstelle_pca, repel = TRUE, col.var = "black", col.ind = "violet")
biplot(DataEstelle_pca, col = c("gray", "black"))





## 2nd part

library(smacof)
library(MASS)
library(ggpubr)

view(Nations)
describe(Nations)
summary(Nations)

# Change similarities to dissimilarities
Nations2 <- sim2diss(Nations, method = 7, to.dist = TRUE)

# Use multidimensional scaling to examine the students’ perceived dissimilarities between the nations
isoMDS(Nations2, k=2)
Nations2_mds$stress
Nations2_mds <- isoMDS(Nations2)

# Plot about two-dimensional solution from non-metric multidimensional scaling
x <- Nations2_mds$points[,1]
y <- Nations2_mds$points[,2]

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", xlim = range(x)*1.2, type = "n", main = "Two-dimensional solution from non-metric multidimensional scaling") +
  text(x, y, labels = colnames(Nations), col="violet")

# Shepard diagram
Nations.3 <- as.matrix(Nations[, -1])
nations.dist <- dist(Nations.3)
nations.mds <- isoMDS(nations.dist)
plot(nations.mds$points, type = "n") 
nations.sh <- Shepard(nations.dist, nations.mds$points) 
plot(nations.sh, pch = ".", xlab = "Dissimilarity", ylab = "Distance", main ="Shepard diagram") 
lines(nations.sh$x, nations.sh$yf, type = "S", col="violet")

# Stressplot for a better visualization
stressplot(nations.mds, nations.dist)


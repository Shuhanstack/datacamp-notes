
# 1. Read data and preprocess --------------------------
url <- "https://assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv"

# Download the data: wisc.df
wisc.df = read.csv(url)

# Convert the features of the data: wisc.data
wisc.data = as.matrix(wisc.df[3:32])

# Set the row names of wisc.data
row.names(wisc.data) <- wisc.df$id

# Create diagnosis vector
diagnosis <- as.numeric(wisc.df$diagnosis == "M")

# 2. Exploratory analysis ----------------------------
# How many observations are in this dataset?
length(diagnosis)
#> [1] 569

# How many variables/features in the data are suffixed with _mean?
stringr::str_detect(colnames(wisc.data), "_mean") |> sum()
#> [1] 10

# How many of the observations have a malignant diagnosis?
sum(diagnosis)
#> [1] 212

# 3. PCA ---------------------------------------------
# Check column means and standard deviations
colMeans(wisc.data)
apply(wisc.data, 2, sd)

# Execute PCA, scaling if appropriate: wisc.pr
wisc.pr = prcomp(wisc.data, scale = TRUE, center = TRUE)

# Look at summary of results
summary(wisc.pr)
#> Importance of components:
#>                           PC1    PC2     PC3     PC4     PC5     PC6     PC7
#> Standard deviation     3.6444 2.3857 1.67867 1.40735 1.28403 1.09880 0.82172
#> Proportion of Variance 0.4427 0.1897 0.09393 0.06602 0.05496 0.04025 0.02251
#> Cumulative Proportion  0.4427 0.6324 0.72636 0.79239 0.84734 0.88759 0.91010
#>                            PC8    PC9    PC10   PC11    PC12    PC13    PC14
#> Standard deviation     0.69037 0.6457 0.59219 0.5421 0.51104 0.49128 0.39624
#> Proportion of Variance 0.01589 0.0139 0.01169 0.0098 0.00871 0.00805 0.00523
#> Cumulative Proportion  0.92598 0.9399 0.95157 0.9614 0.97007 0.97812 0.98335
#>                           PC15    PC16    PC17    PC18    PC19    PC20   PC21
#> Standard deviation     0.30681 0.28260 0.24372 0.22939 0.22244 0.17652 0.1731
#> Proportion of Variance 0.00314 0.00266 0.00198 0.00175 0.00165 0.00104 0.0010
#> Cumulative Proportion  0.98649 0.98915 0.99113 0.99288 0.99453 0.99557 0.9966
#>                           PC22    PC23   PC24    PC25    PC26    PC27    PC28
#> Standard deviation     0.16565 0.15602 0.1344 0.12442 0.09043 0.08307 0.03987
#> Proportion of Variance 0.00091 0.00081 0.0006 0.00052 0.00027 0.00023 0.00005
#> Cumulative Proportion  0.99749 0.99830 0.9989 0.99942 0.99969 0.99992 0.99997
#>                           PC29    PC30
#> Standard deviation     0.02736 0.01153
#> Proportion of Variance 0.00002 0.00000
#> Cumulative Proportion  1.00000 1.00000

# What is the minimum number of principal components required to explain 80% of the variance of the data? 
## Five (PC1 - PC5), see cumulative Proportion above

# For the first principal component, what is the component of the loading vector for the feature concave.points_mean? 
wisc.pr$rotation[,1]
#>   radius_mean            texture_mean          perimeter_mean 
#>             -0.21890244             -0.10372458             -0.22753729 
#>               area_mean         smoothness_mean        compactness_mean 
#>             -0.22099499             -0.14258969             -0.23928535 
#>          concavity_mean     concave.points_mean           symmetry_mean 
#>             -0.25840048             -0.26085376             -0.13816696 
#>  fractal_dimension_mean               radius_se              texture_se 
#>             -0.06436335             -0.20597878             -0.01742803 
#>            perimeter_se                 area_se           smoothness_se 
#>             -0.21132592             -0.20286964             -0.01453145 
#>          compactness_se            concavity_se       concave.points_se 
#>             -0.17039345             -0.15358979             -0.18341740 
#>             symmetry_se    fractal_dimension_se            radius_worst 
#>             -0.04249842             -0.10256832             -0.22799663 
#>           texture_worst         perimeter_worst              area_worst 
#>             -0.10446933             -0.23663968             -0.22487053 
#>        smoothness_worst       compactness_worst         concavity_worst 
#>             -0.12795256             -0.21009588             -0.22876753 
#>    concave.points_worst          symmetry_worst fractal_dimension_worst 
#>             -0.25088597             -0.12290456             -0.13178394 

# Create a biplot of wisc.pr
biplot(wisc.pr)

# Scatter plot observations by components 1 and 2
plot(wisc.pr$x[, c(1, 2)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC2")

# Repeat for components 1 and 3
plot(wisc.pr$x[, c(1, 3), col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC3")

# Variance explained: Scree plot
# Set up 1 x 2 plotting grid
par(mfrow = c(1, 2))

# Calculate variability of each component
pr.var = wisc.pr$sdev^2

# Variance explained by each principal component: pve
pve = pr.var / sum(pr.var)

# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")



# 4. Hierarchical clustering ------------------------
# Scale the wisc.data data: data.scaled
data.scaled = scale(wisc.data)

# Calculate the (Euclidean) distances: data.dist
data.dist = dist(data.scaled)

# Create a hierarchical clustering model: wisc.hclust
wisc.hclust = hclust(data.dist, method = "complete")
wisc.hclust
#> Call:
#> hclust(d = data.dist, method = "complete")
#> 
#> Cluster method   : complete 
#> Distance         : euclidean 
#> Number of objects: 569 

# Using the plot() function, what is the height at which the clustering model has 4 clusters?
plot(wisc.hclust)

# Cut tree so that it has 4 clusters: wisc.hclust.clusters
wisc.hclust.clusters = cutree(wisc.hclust, k = 4)

# Compare cluster membership to actual diagnoses
table(wisc.hclust.clusters, diagnosis)
#>                      diagnosis
#> wisc.hclust.clusters   0   1
#>                    1  12 165
#>                    2   2   5
#>                    3 343  40
#>                    4   0   2

# 5. Create kmeans model and compare with hierarchical model ----------
# Create a k-means model on wisc.data: wisc.km
wisc.km = kmeans(scale(wisc.data), centers = 2, nstart = 20)

# Compare k-means to actual diagnoses
table(wisc.km$cluster, diagnosis)
#>    diagnosis
#>      0   1
#>  1 342  23
#>  2   1  17

# Compare k-means to hierarchical clustering
table(wisc.km$cluster, wisc.hclust.clusters)
#>   wisc.hclust.clusters
#>      1   2   3   4
#>  1  68   5 365   0
#>  2 109   2  18   2

# 6. Use PCA to do hierarchical clustering --------------------------
# Create a hierarchical clustering model: wisc.pr.hclust
wisc.pr.hclust <- hclust(dist(wisc.pr$x[, 1:7]), method = "complete")

# Cut model into 4 clusters: wisc.pr.hclust.clusters
wisc.pr.hclust.clusters = cutree(wisc.pr.hclust, k = 4)

# Compare to actual diagnoses
table(wisc.pr.hclust.clusters, diagnosis)
#>                          diagnosis
#> wisc.pr.hclust.clusters     0   1
#>                         1   5 113
#>                         2 350  97
#>                         3   2   0
#>                         4   0   2

# Compare to k-means and hierarchical
table(wisc.km$cluster, diagnosis)
#>     diagnosis
#>      0   1
#>  1 343  37
#>  2  14 175
table(wisc.hclust.clusters, diagnosis)
#>                        diagnosis
#> wisc.hclust.clusters    0   1
#>                     1  12 165
#>                     2   2   5
#>                     3 343  40
#>                     4   0   2



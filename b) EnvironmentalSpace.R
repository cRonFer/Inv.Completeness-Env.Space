###======================================================### 
###             Environmental space analyses             ###
###              By Cristina Ronquillo 2022              ###
###======================================================### 
# Load packages and data ------------------------------------------------
library(rgdal)  # GIS 
library(sp)  # GIS 
library(raster)  # GIS 
library(maps)  # GIS
library(rSDM)
library(nFactors)
library(psych)
library(colorRamps)  # Visualization
library(ggplot2)  # Visualization
# Environment and data ------------------------
wd <- "" # Set working directory
setwd(wd)

# Load shapefile of study regions / global map
base <- readOGR(".shp")
# Read rasters of climate data (previously cut them by study region) and stack them
setwd("")  # WRITE HERE THE PATH THAT CONTAINS YOUR CLIMATIC LAYERS
rlist <- list.files(pattern = "*.tif$")
allrasters <- lapply(rlist, raster)
wdclim <- raster::stack(rlist)

# Now aggregate raster cells to your study resolution
r <- raster(wdclim, 1) # extract 1 raster to check resolution of cells
res(r) # cell res
wdclim_res <- raster::aggregate(wdclim, fact = 12, fun = mean) # aggregation = x12 
r <- raster(wdclim_res, 1) # extract 1 raster to check NEW resolution of cells
res(r)
res <- res(r)  # Save value for later analysis
climate <- wdclim_res # create climate stack of our new rasters

############ PCA  ########################
# We reduce all the variables to fewer variables using a PCA.
# 1) Here we standardize and prepare the data.
v <- as.data.frame(values(climate)) # get env values from rasters
str(v)
# 2) Standardization of the variables (Normalization - Mean= 0 and Std= 1)
std <- function(x){(x - mean(x, na.rm = T)) / sd(x, na.rm = T)} 
v2 <- apply(v, 2, std) # apply to dataframe of climate values
rem <- apply(is.na(v2), 1, any)
PCAdata <- as.data.frame(v2[!rem, ])
# 3) Run the PCA (2 axis)
mat <- matrix(runif(nrow(PCAdata) * ncol(PCAdata), 0.00001, 0.00009), 
              ncol = ncol(PCAdata)) # add a very small randomness to avoid singularity
PCAdata2 <- PCAdata + mat
myPCA <- principal(PCAdata2,
            nfactors = 2,
            rotate = "varimax",
            scores = T)
myPCA
# 4) Plot PCA
biplot.psych(myPCA, xlim.s = c(-4, 4), ylim.s = c(-3, 3)) # change limits to plot the whole pca values

############ PCA of our study area #################
# Instead of assuming the absolute values of all Worldclim variables, 
# the next map assumes values of the linear combinations between these variables (PCA scores).
v3 <- v2[, 1:2] # create a vector with the same length as v2 but 2 columns
v3[!rem, ] <- myPCA$scores # insert PCA_scores (2axis-2columns) to the new vector
climate_PCA <- subset(climate, 1:2) # extract 2 raster layer as a layerbase to insert our pca values
values(climate_PCA) <- v3 # insert pca values of v3 into raster
names(climate_PCA) <- c("PC1", "PC2") # rename
# Plot study area by each axis from PCA values
par(mfrow = c(1, 2))
plot(climate_PCA, 1, font = 2, font.lab = 2)  # PCA axis 1
map(base, add = TRUE)
plot(climate_PCA, 2, font = 2, font.lab = 2)  # PCA axis 2
map(base, add = TRUE)
dev.off()
############ Environmental space ##########################
# The next step is to create the environmental space using the two PCA scores.
# Create a Cartesian plan with PC1 and PC2 scores
v4 <- values(climate_PCA) # get env values from PCA
# Transform the two vars we want into the env space, 
# by taking the min and max scores (PCA score values) for each PCA axis
# and create a raster object. 
xmin <- min(v4[, 1], na.rm = TRUE) 
xmax <- max(v4[, 1], na.rm = TRUE)
ymin <- min(v4[, 2], na.rm = TRUE)
ymax <- max(v4[, 2], na.rm = TRUE)
# This function creates the cartesian plan comprising the min and max PCA scores
env_space <- raster(xmn = xmin, xmx = xmax, 
                    ymn = ymin, ymx = ymax,
                    res = res) # Resolution of the env cell can change
values(env_space) <- 0
env_space_area <- env_space # duplicate this object for the next step
### Insert our PCA values into this env_space
# Extract PC1 and PC2, convert it in "classes of values" to plot in the map
env_space_v <- raster::extract(env_space, 
                       y = na.omit(v4), 
                       cellnumbers = TRUE)[, 1]
n_env_space_v <- table(env_space_v) # this function counts the frequency of "climates" (PC scores)

values(env_space_area)[as.numeric(names(n_env_space_v))] <- n_env_space_v
area_values <- values(env_space_area)
area_values[area_values == 0] <- NA
values(env_space_area) <- area_values
pol <- rasterToPolygons(env_space_area, dissolve = FALSE)

# The following map shows the frequency of climates in the study area.
# The axes shows all the PC1 and PC2 scores. 
# Each cell "roughly" represents a climate type.
# The colors indicate the frequency of climate types.
plot(env_space_area, 
     xlab = "PC1", 
     ylab = "PC2",
     col = matlab.like(1000000),
     font = 2, font.lab = 2
)
plot(pol, add = TRUE)

# NOW we calculate the environmental space for our well-surveyed cells
# Using estimators output from 'InventoryCompleteness' script:
setwd(wd)
# Select 'well-surveyed' centroid coordinates
WS_cent <- WS_cent[, c('lon','lat')]

# Plot the well-sampled occurrences on the map
plot(r, main = "", col = "gray", legend = FALSE)
map(base, add = TRUE)
points(WS_cent, col = rgb(1, 0, 0, .5), pch = 20, cex = 2)
# Extract climate values of WellSurvey cells
values_WS <- raster::extract(climate, 
                             WS_cent,
                              cellnumbers = TRUE)[, 1]
coords_WS <- v4[values_WS, ]
cell_WS <- raster::extract(env_space, 
                           coords_WS,
                          cellnumbers = TRUE)[, 1]
n_WS <- table(cell_WS)
env_space_WS <- env_space
values(env_space_WS)[as.numeric(names(n_WS))] <- n_WS

# The following map shows the frequency of climates in the well-sampled cells 
# in the study area (legend of this figure is continuous BUT values are integer numbers, 
# so manually adapt the legend to each case)
plot(env_space_WS, 
     xlab = "PC1", 
     ylab = "PC2",
     col = c("transparent", matlab.like(1000)),
     font = 2, font.lab = 2)
plot(pol, add = TRUE)

############ Schoener's D #######################################
# Schoener's D: quantifies the overlap between the location of well-sampled 
# sites and cells with most frequent conditions 
# The Schoener's D index varies from zero (total lack of congruence) to one 
# (total congruence) 

# Transform the abundance of each cell into probabilities.
area_values <- area_values/sum(area_values, na.rm = TRUE) # Relative frequency of climate type for all the study area
WS_values <- values(env_space_WS)
WS_values <- WS_values/sum(WS_values, na.rm = TRUE) # Relative frequency of climate type for well-sampled cells

# Calculate the climate overlap using Schoener's D.
# D values close to 1 indicate that the location of well-sampled 
# sites coincide with climate conditions 
# that are frequently found in the study area
SchoenersD <- function(x, y) {
  sub_values <- abs(x - y)
  D <- 1 - (sum(sub_values, na.rm = TRUE) / 2)
  return(D)
}

D <- SchoenersD(area_values, WS_values)
print(paste("Climate overlap between well-sampled cells and the study area, given by the observed Schoener's D equals = ", 
            round(D,3), "%"))

# We create a null model to test if D values is different from a 
# random distribution of D values calculated from randomly sampling occurrence 
# records.
set.seed(0)
replications <- 1000 # Choose the number of replications
D_rnd <- numeric(replications)
for (i in 1:replications) {
  rnd <- sample(env_space_v, length(WS_cent), replace = TRUE)
  n_rnd <- table(rnd)
  env_space_rnd <- env_space
  values(env_space_rnd)[as.numeric(names(n_rnd))] <- n_rnd
  rnd_values <- values(env_space_rnd)
  rnd_values <- rnd_values/sum(rnd_values, na.rm = TRUE)
  D_rnd[i] <- SchoenersD(area_values, rnd_values)
}
# p-value from previous analysis
# If p <0.05, it means that the location of well-sampled sites does not 
# coincide with areas with climate conditions frequently found in your study area
p <- (sum(D > D_rnd) + 1) / (length(D_rnd) + 1) # Unicaudal test
print(paste("p value equals = ", round(p, 3)))
# Plot distribution of model values
hist(D_rnd, 10, # Write number of bins
     # xlim = c(0, 1), 
     main = (""),
     xlab = "D", 
     col = rgb(.5, .5, .5), 
     border = FALSE,
     font = 2, font.lab = 2)
abline(v = D, col = "red", lty = 2) # Red line show the observed Schoeners' D value

############ Kruskal-Wallis test ##################################
# The following map shows the distribution of well-sampled cells (red bars) 
# vs the study area cells (gray bars)
### For a) PC1
hist(myPCA$scores[, 1], 
     breaks = ncol(env_space),
     freq = F,
     col = "grey", 
     border = FALSE, 
     # xlim= c(-3, 3), 
     ylim = c(0, 1), 
     main = "",
     xlab = "PC1", 
     font = 2, font.lab = 2, 
     cex.lab = 1.2, cex.axis = 1.2)
lines(density(na.omit(myPCA$scores[, 1])), col = "black", lwd = 2)

hist(coords_WS[, 1],
     add = TRUE,
     col = rgb(1, 0, 0, .5),
     freq = F,
     border = FALSE)
lines(density(na.omit(coords_WS[, 1])), col = "red", lwd = 2)

# X axis is a probability density
# Kruskal-Wallis verifies whether 1) the distribution of well-sampled sites 
# is an unbiased subset of the entire climate conditions of the Atlantic forest. 
# If this is so, p > 0.05 
x_1 <- c(myPCA$scores[, 1], coords_WS[, 1])
g_1 <- as.factor(c(rep("area", length(myPCA$scores[, 1])),
                   rep("WS", length(coords_WS[, 1]))))
kruskal.test(x_1 ~ g_1)
# Kolmogorov smirnov test###
ks.test(myPCA$scores[, 1], coords_WS[, 1])

### For b) PC2
hist(myPCA$scores[, 2], 
     breaks = ncol(env_space),
     freq = F,
     col = "grey", 
     border = FALSE, 
     # xlim = c(-3, 3), 
     ylim = c(0, 1), 
     main = "",
     xlab = "PC2", 
     font = 2, font.lab = 2, 
     cex.lab = 1.2, cex.axis = 1.2)
lines(density(na.omit(myPCA$scores[, 2])), col = "black", lwd = 2)

hist(coords_WS[, 2],
     breaks = ncol(env_space), 
     add = TRUE,
     col = rgb(1, 0, 0, .5),
     freq = F,
     border = FALSE)
lines(density(na.omit(coords_WS[, 2])), col = "red", lwd = 2)

x_2 <- c(myPCA$scores[, 2], coords_WS[, 2])
g_2 <- as.factor(c(rep("area", length(myPCA$scores[, 2])),
                   rep("WS", length(coords_WS[, 2]))))
kruskal.test(x_2 ~ g_2)
# Kolmogorov smirnov test###
ks.test(myPCA$scores[, 2], coords_WS[, 2])

############ Rarity ################################
# We can check how many environmental space has been sampled 
# and how does these cells look like.
surface <- WS_values
surface[is.na(area_values) | area_values == 0] <- NA
sampled <- sum(surface > 0, na.rm = TRUE)/ sum(surface >= 0, na.rm = TRUE) * 100
percen <- round(sampled, 2)
prin <- paste0(percen, "% of our study area climate types covered by well-sampled cells")
print(prin)

# Is this env. space sampled corresponding to rare climates?
# First, we make values vary from 0 to 1 according to their rarity:
# Values close to 0, are very common, values close to 1 very rare
mini <- min(area_values, na.rm = TRUE) # less frequent value
# rarity index, also called Min-Max scalling
area_values01 <- abs(1 - (area_values - mini) / (max(area_values, na.rm = TRUE) - mini)) 
# see http://rasbt.github.io/mlxtend/user_guide/preprocessing/minmax_scaling/
hist(area_values01, 
     breaks = ncol(env_space), 
     freq = FALSE, col = "grey",
     main = "", 
     xlab = "Climate rarity", border = FALSE, 
     ylim = c(0, 4),
     font = 2, font.lab = 2,
     cex.lab = 1.6, cex.axis = 1.6)
lines(density(na.omit(area_values01)), col = "black", lwd = 2)

hist(area_values01[surface > 0], 
     breaks = 5, 
     freq = FALSE, 
     add = TRUE, 
     col = rgb(0, 1, 0, .5),
     border = FALSE)
lines(density(na.omit(area_values01[surface > 0])), col = "lightgreen", lwd = 2)

# Kruskal-Wallis test to see if the distribution of rarities for the sampled 
# occurrences differs from the distribution observed in the entire area
x <- c(area_values01, area_values01[surface > 0])
g <- as.factor(c(rep("area", length(area_values01)),
                 rep("ws", length(area_values01[surface > 0]))))
kruskal.test(x ~ g)
# Kolmogorov smirnov test
area_values03 <- (na.omit(area_values01))
area_values04 <- (na.omit(area_values01[surface > 0]))
ks.test(area_values03, area_values04)
# This result indicates that the under-sampled area is composed 
# mainly by rare climates. You can inspect the environmental space figures 
# to check which areas were not sampled.

# Finally MAP the climatic rarity
rarity_env <- env_space
values(rarity_env) <- area_values01
rarity_Percell <- raster::extract(rarity_env, v4)
rarity_map <- r
values(rarity_map) <- rarity_Percell

cols <- colorRampPalette(c('#e0f3db','#a8ddb5','#43a2ca'))
x11()
plot(rarity_map, 
     col = cols(10), 
     font = 2, font.lab = 2, 
     ylim = c(ymin(base), ymax(base)), 
     xlim = c(xmin(base), xmax(base))
)
maps::map(base, add = TRUE)
points(WS_cent, col = rgb(1, 0, 0, .5), pch = 19, cex = 1)


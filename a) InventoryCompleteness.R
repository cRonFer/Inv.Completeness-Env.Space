###======================================================### 
###           Completeness inventory analyses            ###
###              By Cristina Ronquillo 2022              ###
###======================================================### 
# Load packages ---------------------------------------------------------
library(data.table)  # big dataset manipulation
library(tidyverse)  # data manipulation
library(KnowBR) # completeness analysis
library(rgdal) # gis
library(patchwork) # plots composition
library(sf)  # GIS 
library(rnaturalearth)  # GIS 
library(rnaturalearthdata)  # GIS 

# Environment and data ----------------------------------------------------
rm(list = ls(all.names = TRUE))
wd <- "" # Set working directory
setwd(wd)

### Load shapefiles
data(adworld) # knowBR needs add world polygon to work 
# Shapefile of the world to use in map plot
world <- ne_countries(scale = "medium", returnclass = "sf") 
# Load study area grid shapefile
grid <- readOGR(".shp") # load as spatialPolygonDataFrame to use with knowBR
grid2 <- st_read(".shp") # load again as sf for plot purposes

### Load records from GBIF
data <- fread('.csv', sep=";", header = TRUE)
# Filter dataset to only records with species name and select necessary fields (species, newLon, newLat):
data <- data[!is.na(species), ][ ,c('species','lon','lat')]
# Add new field of abundance for knowBR
data$abundance <- 1

# Apply knowBR function in each case ---- 
# Set an specific working directory to save the output from knowBR
setwd('') 
# Apply KnowBR inventory completeness function 
KnowBPolygon(data = data, 
             shape = grid, admAreas = FALSE,  # Use predefined grid as personalized polygons  
             shapenames = "",  # WRITE HERE your unique "id" cell from the grid
             jpg = TRUE, dec = ".")

# Check the output ---- 
# Load output of estimators from knowBR analyses:
est <- read.csv('Estimators.CSV', header = TRUE, sep = ",")

# Plot completeness results establishing threshold of well-survey ----
plot <- function(var, x, xtitle){
  ggplot(est) +
    geom_point(aes(var, Completeness), pch = 19, size = 1) +
    geom_vline(xintercept = x, col = 'red', lwd = 1, lty = 2) +
    geom_hline(yintercept = 70, col = 'grey', lwd = 1, lty = 2) + # Here completeness threshold =70%
    theme_minimal() + 
    ylab('') +
    xlab(xtitle) +
    theme(strip.text.y = element_blank(),
          axis.text = element_text(size = 10, face = "bold"),
          axis.title = element_text(size = 10, face = "bold"))
}
# Distribution of completeness values by other estimators:
# Plot records value vs completeness (threshold set in 10):
(p1 <- plot(est$Records, 10, 'Records') + ylab('Completeness'))
# Plot ratio value vs completeness (threshold set min 2):
(p2 <- plot(est$Ratio, 2, 'Ratio'))
# Plot slope value vs completeness (threshold set in 0.1):  
(p3 <- plot(est$Slope, 0.1, 'Slope'))

completn <- p1|p2|p3
ggsave('completenessDistribution.png', completn)

### NOW filter est dataset based on your chosen thresholds selecting WELL-SURVEY CELLS only:
estWS <- est %>% filter(Records > ) %>% 
               filter(Ratio > ) %>% 
               filter(Slope < )
# Map of Completeness ----
comp_shp <- merge(grid2, estWS, by.x ='', by.y = 'Area') # Write here 'by.x' = the unique identifier name of your grid shapefile
comp_shp2 <- comp_shp %>% st_transform(4326)  # Transform to WGS84 projection
(map <- ggplot(comp_shp2) +
        geom_sf(data = world, fill ='lightgrey', color = 'darkgrey') +
        geom_sf(aes(fill = Completeness), color = "transparent") +
        scale_fill_viridis_c(limits = c(0, 100), option = "plasma", name = "", direction = -1) +
        theme_minimal() +
        theme(plot.background = element_rect(fill = "white", color = "transparent"),
              legend.position = "left", 
              plot.title = element_text(size = 16, face = "bold", hjust = 1.1))
  )
ggsave('completenessMap.png', map, dpi = 500)

# Extract centroids of Well Survey cells for the environmental Space analysis
WS_cent <- st_centroid(comp_shp2)
WS_cent <- WS_cent %>% dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                                    lat = sf::st_coordinates(.)[,2]) %>% 
                      dplyr::select(c(id, lon, lat))
WS_cent$geometry <- NULL

###  NEXT on 'climaticSpace' script
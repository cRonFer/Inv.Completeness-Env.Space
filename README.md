# This repository includes R code necessary to develop 
### a) **Inventory Completeness** analysis
Calculate well-survey cells based on species occurrence records using **knowBR** R package *Lobo, J.M., Hortal, J., Yela, J.L., Millán, A., Sánchez-Fernández, D., García-Roselló, E., … Guisande, C. (2018). KnowBR: An application to map the geographical variation of survey effort and identify well-surveyed areas from biodiversity databases. Ecological Indicators, 91: 241:248* [see here](https://www.sciencedirect.com/science/article/abs/pii/S1470160X18302322)
  
###### For the correct develop of this code, user needs to: 
* Create a grid of the study area
* Provide the occurrence records dataset of the taxa
 
###### Output:
  - Estimators data including Ratio, Number of records, Slope and Completeness values
  - Figure of the distribution of completeness values vs other estimators
  - Map of Completeness
  
## b) **Environmental Space** analysis
Calculate study area environmental space vs well-survey sampled space
###### For the correct develop of this code, user needs to: 
* Provide a polygon shapefile of the study area
* Provide rasters of climatic/ environmental values of the study area
###### Output:
   - PCA figure
   - Maps of the study area with PCA values for each axis
   - Figure of study area environmental space
   - Figure of a subset of the environmental space covered by well surveyed cells
   - Climate overlap between well-sampled cells and the study area using Schoeners' D
   - Figure of bars distribution of well-sampled cells vs study area cells for each PCA axis
   - Kruskal-Wallis analysis of overlap
   - % of our study area climate types covered by well-sampled cells
   - Climatic rarity represented by well-surveyed cells (histogram and map)
  
> The code was created and used in *Ronquillo, C., Alves-Martins, F., Mazimpaka, V., Sobral-Souza, T., Vilela-Silva, B., G. Medina, N. & Hortal, J. (2020). Assessing spatial and temporal biases and gaps in the publicly available distributional information of Iberian mosses. Biodiversity Data Journal 8: e53474* [see here](https://doi.org/10.3897/BDJ.8.e53474)

> It will also be part of the following manuscripts:
Vaz et al. (in prep) Fireflies inventory completeness assessment in the Atlantic Forest of Brazil

William et al. (in prep)

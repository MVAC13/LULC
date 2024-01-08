## README

## Land Use Land Cover (LULC) Classification of Malta

Overview
This repository contains a script for processing satellite imagery to classify land use and land cover in Malta. The script loads satellite images, clips them to the region of interest, prepares training data, performs supervised classification using Random Forest, and visualizes the results.

### Dependencies
The script requires the following R packages:

raster: For raster data manipulation
rgdal: For bindings to GDAL for spatial data operations
sp: For handling spatial data
ggplot2: For generating plots
sf: For handling simple features and spatial vector data
caret: For machine learning algorithms
ranger: For efficient Random Forest computation
rasterVis: For enhanced raster visualization
Please ensure these packages are installed before running the script.

### Data
The script utilises two sets of satellite imagery reflecting dry and wet conditions. It also uses a shapefile for the Malta region to clip the satellite images and align the training data. Ensure the data is correctly placed in the directory as specified in the script.

### Usage
To run the script, simply load it into your R environment and execute. The script is well-commented and divided into sections for ease of understanding and modification.

### Output
The script outputs:

Clipped raster images of Malta for dry and wet conditions.
A data frame with prepared training data.
Trained Random Forest models for both dry and wet conditions.
Confusion matrices and accuracy assessments for the models.
Predicted land cover classification maps for both conditions.
Enhanced visualizations of the classified rasters with color legends.
Contributing
Contributions to the script are welcome. Please fork the repository and submit a pull request with your suggested changes.

### License
This project is licensed under the [LICENSE NAME]. See the LICENSE file for more details.

### Contact
For any questions or support, please contact Ecostack Innovations at https://www.ecostackinnovations.com/.
#' Land Use Land Cover (LULC) Classification of Malta
#'
#' This script processes satellite imagery to classify land use and land cover in Malta. 
#' It involves loading satellite images, clipping to the region of interest, preparing training data,
#' performing supervised classification, and visualizing the results.
#'
#' @author EcoINN
#' @date "January 2024"
#' @return An object of class RasterLayer representing LULC Malta



# Load necessary packages
library(raster)     # For raster data manipulation
library(rgdal)      # Provides bindings to GDAL for spatial data
library(sp)         # Classes and methods for spatial data
library(ggplot2)    # For plotting
library(sf)         # Simple Features for handling spatial vector data
library(caret)      # For machine learning algorithms
library(ranger)     # For efficient Random Forest computation
library(rasterVis)  # To improve raster visualization



#' Loading and Processing Satellite Images
#'
#' This section focuses on loading satellite imagery for Malta and preparing them for further analysis. 
#' The images are first loaded as RasterBrick objects, then clipped to the boundaries of Malta using a shapefile. 
#' The process involves spatial alignment of the images with the shapefile, ensuring accurate geographic representation.
#'
#' Steps:
#' 1. Load satellite images: Dry and wet condition images are loaded as multi-layer raster objects.
#' 2. Read Malta boundary shapefile: The geographic boundary of Malta is loaded for spatial referencing.
#' 3. CRS alignment: The Coordinate Reference System (CRS) of the shapefile is aligned with that of the raster images.
#' 4. Clipping rasters: The satellite images are clipped to the extent of the Malta boundary.
#' 5. Image visualization: The clipped images are visualized to verify the process and for preliminary analysis.
#'
#' @param dry_image_path File path to the dry condition satellite image.
#' @param wet_image_path File path to the wet condition satellite image.
#' @param malta_shp_path File path to the Malta boundary shapefile.
#' @return Clipped RasterBrick objects of the dry and wet condition images.

# Load necessary libraries for this section
library(raster)  # For raster data manipulation
library(sf)      # Simple Features for handling spatial vector data
library(rgdal)   # For raster and vector data transformations

# Load raster files as RasterBrick objects for efficient handling of multi-layered data
dry_image <- brick("data/July17_dry.tif")
wet_image <- brick("data/March8_wet.tif")

# Read the Malta boundary shapefile as an 'sf' object and convert to a 'Spatial' object
malta_shp <- st_read("C:/Ecostack/09_Data/Malta_shp/Malta.shp")
malta_sp <- as(st_geometry(malta_shp), "Spatial")

# Ensure spatial alignment by matching the CRS of the shapefile with the satellite images
if (!st_crs(malta_sp) == crs(dry_image)) {
  malta_sp <- spTransform(malta_sp, crs(dry_image))
}

# Clip the satellite images to the shapefile's extent, focusing analysis on the Malta region
dry_image_clipped <- mask(dry_image, malta_sp)
wet_image_clipped <- mask(wet_image, malta_sp)

# Crop the images to remove any extra padding and align them precisely with the Malta boundary
dry_image <- crop(dry_image_clipped, extent(malta_sp))
wet_image <- crop(wet_image_clipped, extent(malta_sp))

# Visualize the clipped satellite images
plotRGB(dry_image, r=3, g=2, b=1, stretch="lin", main="Clipped Dry Image of Malta")
plotRGB(wet_image, r=3, g=2, b=1, stretch="lin", main="Clipped Wet Image of Malta")

# Note: The visualization helps in verifying the clipping process and provides an initial view of the land cover.



#' Training Data Preparation
#'
#' This section focuses on preparing the training data for land cover classification. 
#' The preparation involves loading spatial data, transforming it to match the Coordinate Reference System (CRS) of the satellite images,
#' and formatting the data for the classification process.
#'
#' Steps:
#' 1. Load training data: Training points are loaded from a Shapefile using the sf package.
#' 2. CRS Transformation: The training data's CRS is transformed to align with the CRS of the satellite images.
#' 3. Data Visualization: A preliminary visualization of the training data is performed to ensure spatial alignment.
#' 4. Data Cleaning: Z-dimension is removed from the geometry for compatibility with raster extraction.
#' 5. Class Assignment: Classes (land cover types) are assigned from the 'Type' column of the training data.
#' 6. Factor Conversion: The class labels are converted to a factor to facilitate their use in classification algorithms.
#'
#' @param training_data_shp_path File path to the training data Shapefile.
#' @param dry_image The dry condition satellite image for CRS reference.
#' @return A data frame with training points, class labels, and their corresponding CRS.


# Load SHP file using st_read() function from the sf package
training_data <- st_read("C:/Ecostack/02_Projects/09_LULC/lulc/data/training/training_data.shp")

# Transform training data to match the CRS of the satellite images
training_data <- st_transform(training_data, crs(dry_image))

# Preliminary visualization of the training data
plot(training_data, main="Training Data Spatial Distribution")

# Data Cleaning: Remove Z-dimension from the geometry
training_data <- st_zm(training_data)

# Class Assignment: Use the 'Type' column to assign classes
training_data$Class <- training_data$Type

# Convert 'Class' to a factor for classification algorithms
training_data$Class <- as.factor(training_data$Class)

# Check the distribution of classes
table(training_data$Class)

# Note: The table of classes provides insights into the balance or imbalance among different classes.
# This can guide further steps in the analysis, such as resampling or weighting classes in the model.



#' Supervised Classification with Random Forest
#'
#' This section describes the supervised classification process using the Random Forest algorithm. 
#' We utilize the training data obtained from labeled points on the satellite images to train the model.
#' The ranger package is employed for its efficient implementation of Random Forest. 
#' The classification is conducted separately for both dry and wet conditions to accommodate different seasonal characteristics.
#'
#' The steps are as follows:
#' 1. Pixel values are extracted from the raster images based on training data locations.
#' 2. A Random Forest model is trained on these values to classify land cover types.
#' 3. The model's performance is evaluated using a confusion matrix and accuracy assessments.
#'
#' @return A trained Random Forest model, a confusion matrix, and accuracy metrics for each classification scenario (dry and wet).


# Data Preparation
# Extract pixel values from the raster images corresponding to the training data points.
training_dry <- extract(dry_image, training_data, df = TRUE)
training_wet <- extract(wet_image, training_data, df = TRUE)

# Associate the correct class labels from the training data to the extracted pixel values.
training_dry$Class <- training_data$Class[training_dry$ID]
training_wet$Class <- training_data$Class[training_wet$ID]

# Remove any rows with NA values, which may cause issues during model training.
training_dry <- na.omit(training_dry)
training_wet <- na.omit(training_wet)

# Ensure class labels are factors, as required by the ranger function.
training_dry$Class <- as.factor(training_dry$Class)
training_wet$Class <- as.factor(training_wet$Class)


# Visualisations
# Subsets
garden <- subset(training_wet, Class == 'Garden')
agriculture <- subset(training_wet, Class == 'Agricultural field')
abandoned <- subset(training_wet, Class == 'Abandoned field')
garrigue <- subset(training_wet, Class == "Garrigue")
maquis <- subset(training_wet, Class == "Maquis")
steppe <- subset(training_wet, Class == "Steppe")
woodland <- subset(training_wet, Class == "Woodland")
greenhouse <- subset(training_wet, Class == "Greenhouse")
vineyard <- subset(training_wet, Class == "Vineyard")
urban <- subset(training_wet, class == "Urban")

# NDVI
dev.new(width = 8, height = 6)
par(mar=c(2, 2, 2, 1))
par(mfrow = c(9, 1), oma = c(2, 2, 2, 2), cex = 0.6, cex.axis = 0.6, cex.lab = 0.6)
hist(garrigue$NDVI, main = "Garden", xlab = "NDVI", xlim = c(0, 1), col = "green")
hist(garrigue$NDVI, main = "Agricultural field", xlab = "NDVI", xlim = c(0, 1), col = "brown")
hist(garrigue$NDVI, main = "Abandoned field", xlab = "NDVI", xlim = c(0, 1), col = "grey")
hist(garrigue$NDVI, main = "Garrigue", xlab = "NDVI", xlim = c(0, 1), col = "orange")
hist(maquis$NDVI, main = "Maquis", xlab = "NDVI", xlim = c(0, 1), col = "dark green")
hist(steppe$NDVI, main = "Steppe", xlab = "NDVI", xlim = c(0, 1), col = "light blue")
hist(woodland$NDVI, main = "Woodland", xlab = "NDVI", xlim = c(0, 1), col = "light green")
hist(greenhouse$NDVI, main = "Greenhouse", xlab = "NDVI", xlim = c(0, 1), col = "black")
hist(vineyard$NDVI, main = "Vineyard", xlab = "NDVI", xlim = c(0, 1), col = "purple")
hist(urban$NDVI, main = "Urban", xlab = "NDVI", xlim = c(0, 1), col = "yellow")

# Plot for the first class (e.g., Garden)
plot(B8 ~ B11, data = garden, pch = ".", col = "green", xlim = c(0, 0.4), ylim = c(0, 0.5), xlab = "Band 8", ylab = "Band 11")

# Add points for other classes
points(B8 ~ B11, data = agriculture, pch = ".", col = "brown")
points(B8 ~ B11, data = abandoned, pch = ".", col = "grey")
points(B8 ~ B11, data = garrigue, pch = ".", col = "orange")
points(B8 ~ B11, data = maquis, pch = ".", col = "dark green")
points(B8 ~ B11, data = steppe, pch = ".", col = "light blue")
points(B8 ~ B11, data = woodland, pch = ".", col = "light green")
points(B8 ~ B11, data = greenhouse, pch = ".", col = "black")
points(B8 ~ B11, data = vineyard, pch = ".", col = "purple")
points(B8 ~ B11, data = urban, pch = ".", col = "urban")

# Add a legend
legend("topright", legend = c("Garden", "Agricultural field", "Abandoned field", "Garrigue", "Maquis", "Steppe", "Woodland", "Greenhouse", "Vineyard", "Urban"), 
       col = c("green", "brown", "grey", "orange", "dark green", "light blue", "light green", "black", "purple", "yellow"), pch = ".", cex = 0.8)



# Remove the 'ID' column from the training datasets before model training
training_dry <- training_dry[, -which(names(training_dry) == "ID")]
training_wet <- training_wet[, -which(names(training_wet) == "ID")]

# Model Training
# Train Random Forest models using the ranger package.
# Construct the random forest model
modelRF_dry <- ranger(x = training_dry[, 1:ncol(training_dry)-1], y = training_dry$Class,
                  importance = "permutation", seed = 0xfedbeef)

modelRF_wet <- ranger(x = training_wet[, 1:ncol(training_wet)-1], y = training_wet$Class,
                  importance = "permutation", seed = 0xfedbeef)

# Inspect the structure and element names of the resulting model
modelRF_dry
class(modelRF_dry)
str(modelRF_dry)
names(modelRF_dry)

modelRF_wet
class(modelRF_wet)
str(modelRF_wet)
names(modelRF_wet)

# Since we set importance = "permutation", we now also have information 
# on the statistical importance of each of our covariates which we can retrieve
# using the importance() command.
# The importance score is a measure of how much each variable contributes to the 
# accuracy of the model. Higher scores indicate that the variable is more important 
# for making accurate predictions. These scores are especially useful in understanding 
# which variables (or features) the model is relying on most to make its decisions.
importance(modelRF_dry)
importance(modelRF_wet)


# Accuracy Assessment
# Evaluate the model's performance using the out-of-bag (OOB) error estimate.
# The OOB error is a method of measuring prediction error in random forests.

# Inspect the confusion matrix of the OOB error assessment
modelRF_dry$confusion.matrix
modelRF_wet$confusion.matrix

# Function to calculate User's and Producer's Accuracy
calculate_accuracies <- function(conf_matrix) {
  user_accuracy <- diag(conf_matrix) / rowSums(conf_matrix)
  producer_accuracy <- diag(conf_matrix) / colSums(conf_matrix)
  return(list(user_accuracy = user_accuracy, producer_accuracy = producer_accuracy))
}

# Calculate accuracies for dry and wet models
accuracies_dry <- calculate_accuracies(modelRF_dry$confusion.matrix)
accuracies_wet <- calculate_accuracies(modelRF_wet$confusion.matrix)

# Print the accuracies
print("User's and Producer's Accuracy for Dry Model:")
print(accuracies_dry)

print("User's and Producer's Accuracy for Wet Model:")
print(accuracies_wet)



#' Accuracy and Kappa Statistic Assessment
#'
#' This section evaluates the model's performance using the out-of-bag (OOB) error estimate 
#' and calculates the Kappa statistic. The OOB error is a method of measuring prediction 
#' error in random forests. The Kappa statistic provides a measure of classification accuracy 
#' adjusted for chance agreement.
#'
#' @param confusion_matrix A confusion matrix from the Random Forest model prediction.
#' @return The Kappa statistic which indicates the classification accuracy.
#'
#' Example usage:
#' kappa_stats_dry <- confusionMatrix(as.table(modelRF_dry$confusion.matrix))
#' kappa_stats_wet <- confusionMatrix(as.table(modelRF_wet$confusion.matrix))

# Calculate Kappa statistic for dry model
conf_matrix_dry <- as.table(modelRF_dry$confusion.matrix)
kappa_stats_dry <- confusionMatrix(conf_matrix_dry)
kappa_dry <- kappa_stats_dry$overall['Kappa']

# Calculate Kappa statistic for wet model
conf_matrix_wet <- as.table(modelRF_wet$confusion.matrix)
kappa_stats_wet <- confusionMatrix(conf_matrix_wet)
kappa_wet <- kappa_stats_wet$overall['Kappa']

# Print the Kappa statistics
print(paste("Kappa Statistic for Dry Model:", kappa_dry))
print(paste("Kappa Statistic for Wet Model:", kappa_wet))



#' Predict Land Cover and Visualize Results
#'
#' After constructing the Random Forest models for both dry and wet datasets,
#' this section uses the trained models to predict land cover classes for each pixel in the satellite images.
#' Predictions are made separately for dry and wet conditions.
#' Following the prediction, new raster layers are visualized to inspect the classification output.
#'
#' The predict() function applies the Random Forest model to the raster data, and the output is a RasterLayer
#' with predictions for each pixel. These layers are then visualized using base plotting functions,
#' illustrating the spatial distribution of predicted land cover classes.
#'
#' @param dry_image A RasterBrick object representing the dry condition satellite image.
#' @param wet_image A RasterBrick object representing the wet condition satellite image.
#' @param modelRF_dry A Random Forest model trained on dry condition data.
#' @param modelRF_wet A Random Forest model trained on wet condition data.
#' @return Plots of the predicted land cover classifications for both dry and wet conditions.

# Predict land cover using the Random Forest model for the dry condition
predLC_dry <- predict(dry_image, modelRF_dry, fun = function(...) predict(...)$predictions)

# Predict land cover using the Random Forest model for the wet condition
predLC_wet <- predict(wet_image, modelRF_wet, fun = function(...) predict(...)$predictions)

# Visualize the predicted land cover classification for the dry condition
dev.new(width = 8, height = 6)
plot(predLC_dry, main="Predicted Land Cover for Dry Condition")

# Visualize the predicted land cover classification for the wet condition
dev.new(width = 8, height = 6)
plot(predLC_wet, main="Predicted Land Cover for Wet Condition")

# Note: The actual visualization function (plot) is quite basic here. Depending on the complexity
# and the requirements of the analysis, additional functions from packages like rasterVis can be used
# for a more enhanced visualization, including proper legends and color schemes.



#' Visualization of Classified Rasters
#'
#' This section defines a function to apply classification levels to a raster and visualize the results with a color legend. 
#' The function takes a raster layer and a matrix of classification levels with their corresponding labels. 
#' It uses these inputs to create a factor-level raster, which it then plots using the levelplot function from the rasterVis package, 
#' providing a clear and color-coded visualization of the land cover classification.
#'
#' @param raster_layer A RasterLayer object that contains the classified data.
#' @param class_levels A matrix defining the classes with their associated labels.
#' @return A plot of the classified raster with a color legend.
#' @examples
#' plot_classified_raster(predLC_dry, classification_scheme)
#' plot_classified_raster(predLC_wet, classification_scheme)

plot_classified_raster <- function(raster_layer, class_levels) {
  # Apply the classification levels and labels to the raster
  raster_layer <- ratify(raster_layer)
  rat <- levels(raster_layer)[[1]]
  rat$landcover <- class_levels[,3]
  levels(raster_layer) <- rat
  
  # Plot the classified raster with a color legend
  levelplot(raster_layer, col.regions=rev(terrain.colors(length(class_levels[,1]))), margin=FALSE,
            scales=list(draw=TRUE), xlab="Longitude", ylab="Latitude",
            main="Land Cover Classification",
            colorkey=list(labels=list(at=class_levels[,1], labels=class_levels[,3])))
}

# Define the classification levels and labels
classification_scheme <- cbind(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                               c("Abandoned field", "Agricultural field", "Garden", "Garrigue", 
                                 "Greenhouse", "Maquis", "Steppe", "Urban", "Vineyard", "Woodland"))

# Apply the function to both dry and wet classified rasters and visualize
plot_classified_raster(predLC_dry, classification_scheme)
plot_classified_raster(predLC_wet, classification_scheme)




#' Post-Classification Smoothing and Filtering
#'
#' This section applies spatial smoothing and majority filtering to the classified rasters.
#' Spatial smoothing is achieved through a focal operation that recalculates each pixel's value
#' to be the mean of its surrounding neighborhood. This is followed by a majority filter, which assigns
#' the most common value within a focal window to each pixel. These steps help to reduce the "salt-and-pepper"
#' effect common in classified images, leading to a cleaner visual representation.
#'
#' Optionally, the majority-filtered raster can be converted to vector polygons for further spatial analysis.
#' This polygonization step groups adjacent pixels of the same class and represents them as single polygon features,
#' which may be useful for analyses that require vector data formats.
#'
#' @param classified_raster A RasterLayer object that contains the classified data from the Random Forest model.
#' @return The smoothed and filtered raster, and optionally a SpatialPolygonsDataFrame of the raster data.


# Function to apply spatial smoothing and majority filtering
smooth_and_filter <- function(classified_raster) {
  # Define the modal function if not available
  modal <- function(x) {
    ux <- unique(na.omit(x))
    if (length(ux) == 0) return(NA)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Apply spatial smoothing
  smoothed_raster <- focal(classified_raster, w = matrix(1, 3, 3), fun = mean)
  
  # Apply majority filtering
  majority_raster <- focal(smoothed_raster, w = matrix(1, 3, 3), fun = modal)
  
  return(majority_raster)
}

# Apply the smoothing and filtering function to the classified rasters
majority_raster_dry <- smooth_and_filter(predLC_dry)
majority_raster_wet <- smooth_and_filter(predLC_wet)

# Assuming your majority-filtered rasters might still have floating point values
# Reclassify the raster by rounding the values and then converting to integers
reclassify_raster <- function(r) {
  r[] <- round(r[], 0)  # Round the values to the nearest whole number
  r <- calc(r, fun = as.integer)  # Convert the raster values to integers
  return(r)
}

majority_raster_dry <- reclassify_raster(majority_raster_dry)
majority_raster_wet <- reclassify_raster(majority_raster_wet)

# Set NA values if any exist. Replace '9999' with an appropriate value that does not clash with your valid class values.
majority_raster_dry[is.na(majority_raster_dry)] <- 9999
majority_raster_wet[is.na(majority_raster_wet)] <- 9999

# Set the NoData value to -9999 (or another appropriate number that isn't a valid class)
writeRaster(majority_raster_dry, filename="C:/Ecostack/02_Projects/09_LULC/lulc/output/raster_dry.tif",
            format="GTiff", datatype="INT4S", overwrite=TRUE, NAflag=-9999)
writeRaster(majority_raster_wet, filename="C:/Ecostack/02_Projects/09_LULC/lulc/output/raster_wet.tif",
            format="GTiff", datatype="INT4S", overwrite=TRUE, NAflag=-9999)

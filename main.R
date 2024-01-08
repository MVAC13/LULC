#' Tweets preprocessing and analysis
#' 
#' @author EcoINN
#' @date "July 2023"
#' @return LULC Malta


# Load necessary packages
library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(sf)
library(caret)
library(ranger)



#' Satellite images
#' 
#' Load satellite images and visualise them


# Load raster files
# Use the brick() function from the raster package to load the images.
# brick() creates a multi-layer raster (RasterBrick object) from a file.
dry_image <- brick("data/July17_dry.tif")
wet_image <- brick("data/March8_wet.tif")

# Plot the dry image
# Use plotRGB() from the raster package to plot a three-band (RGB) image. The parameters r, g, and b specify the bands to use for red, green, and blue, respectively. 
# The 'stretch' parameter controls the contrast stretching. 'lin' stands for linear stretching.
plotRGB(dry_image, r=3, g=2, b=1, stretch="lin")

# Plot the wet image
# Similar to the above, but for the wet image.
plotRGB(wet_image, r=3, g=2, b=1, stretch="lin")



#' Training data
#' 
#' @description The purpose of this section is to add a Coordinate Reference System (CRS) to the training data, 
#' which is essential for any geographic data analysis.


# Load KML training data
# Use the st_read() function from the sf package to load the KML file as a simple feature object.
training_data <- st_read("data/training_data.kml")

# Transform training data to match image CRS (Coordinate Reference System)
# The st_transform() function from the sf package is used to transform the CRS of the training data to match that of the images.
training_data <- st_transform(training_data, crs(dry_image))

# Add the training data
# The plot() function from base R is used to add the training data as an overlay on the image. 
# The parameter 'add=TRUE' specifies that the data should be added to the existing plot rather than creating a new one.
plot(training_data)

# Convert to data frame and view as a table
training_df <- as.data.frame(training_data)
View(training_df)

# Add a Class column to your training data, where class names are extracted from the Name column
training_data$Class <- gsub(" .*$", "", training_data$Name)

# Remove unnecessary columns
training_data <- subset(training_data, select = -c(Name, Description))

# Convert to factor
training_data$Class <- as.factor(training_data$Class)
table(training_data$Class)


#' Supervised Classification
#' 
#' @description We're going to execute a supervised classification using the Maximum Likelihood method. The process consists of the following steps:


# Extract pixel values
# Extract pixel values below the points into a dataframe
training_dry <- extract(dry_image, training_data, df = TRUE)
training_wet <- extract(wet_image, training_data, df = TRUE)

# Add a column specifying the class based on the polygon ID
training_dry$Class <- training_data$Class[training_dry$ID]
training_wet$Class <- training_data$Class[training_wet$ID]

# Remove the training polygon ID's from the dataframe
training_dry$ID <- NULL
training_wet$ID <- NULL


# Visualisations
# Subsets
garrigue <- subset(training_wet, Class == "Garrigue")
maquis <- subset(training_wet, Class == "Maquis")
steppe <- subset(training_wet, Class == "Steppe")
woodland <- subset(training_wet, Class == "Woodland")

# NDVI
par(mar=c(1,1,1,1))
hist(garrigue$NDVI, main = "Garrigue", xlab = "NDVI", xlim = c(0, 1), col = "orange")
hist(maquis$NDVI, main = "Maquis", xlab = "NDVI", xlim = c(0, 1), col = "dark green")
hist(steppe$NDVI, main = "Steppe", xlab = "NDVI", xlim = c(0, 1), col = "light blue")
hist(woodland$NDVI, main = "Woodland", xlab = "NDVI", xlim = c(0, 1), col = "light green")

# Another scatterplot
par(mfrow = c(1, 1))

# Scatterplot of bands 8a and 11 for the three classes
plot(B8 ~ B11, data = garrigue, pch = ".", col = "orange", xlim = c(0, 0.4), ylim = c(0, 0.5))
points(B8 ~ B11, data = maquis, pch = ".", col = "dark green")
points(B8 ~ B11, data = steppe, pch = ".", col = "light blue")
points(B8 ~ B11, data = woodland, pch = ".", col = "light blue")
legend("topright", legend = c("Garrigue", "Maquis", "Steppe", "Woodland"), fill = c("orange", "dark green", "light blue", 'light green'), bg = "white")


# Random Forest
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

# Inspect the confusion matrix of the OOB error assessment
modelRF_dry$confusion.matrix
modelRF_wet$confusion.matrix


# Since we set importance = "permutation", we now also have information 
# on the statistical importance of each of our covariates which we can retrieve
# using the importance() command.
importance(modelRF_dry)
importance(modelRF_wet)


# Prediction
# Predict land cover using the RF model
predLC_dry <- predict(dry_image, modelRF_dry, fun = function(...) predict(...)$predictions)
predLC_wet <- predict(wet_image, modelRF_wet, fun = function(...) predict(...)$predictions)

# Install necessary libraries
# install.packages("dplyr")
# install.packages("gstat")
# install.packages("ggplot2")
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("sp")
# install.packages("sf")

# Load necessary libraries
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)

# Create example spatial data
#set.seed(42)
n <- 25

# Get natural earth world country polygons

#?ne_countries

# Get the map data for Crete from rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")

#?st_crop
crete <- st_crop(world, xmin = 23.4, xmax = 26.3, ymin = 34.7, ymax = 35.7)

# Function to generate points within Crete
generate_points_in_crete <- function(n, crete) {
  points <- data.frame(lon = numeric(0), lat = numeric(0))
  
  while (nrow(points) < n) {
    # Generate random spatial coordinates within the bounding box
    coords <- data.frame(
      lon = runif(n, 23.4, 26.3), 
      lat = runif(n, 34.7, 35.7)
    )
    
    # To make sure our approach is robust, let's refine the code to guarantee that 
    # we generate points exclusively on land by iteratively generating points until
    # the desired number of valid points is obtained. This avoids reliance on initial
    # random points that might fall in the sea.
    
    # Convert the coordinates to an sf object
    coords_sf <- st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
    
    # Filter points to be within the land area of Crete
    coords_sf <- st_intersection(coords_sf, crete)
    
    # Add valid points to the points dataframe
    points <- rbind(points, as.data.frame(st_coordinates(coords_sf)))
  }
  
  return(points[1:n, ])
}

# Generate points within Crete
points_in_crete <- generate_points_in_crete(n, crete)
coords_sf <- st_as_sf(points_in_crete, coords = c("X", "Y"), crs = 4326)

# Calculate min(Y) and min(X)
min_Y <- 34.9 # min(points_in_crete$Y)
min_X <- 23.5 # min(points_in_crete$X)

# Plot the map with the spatial coordinates
ggplot() +
  geom_sf(data = crete, fill = "white", color = "black") +
  geom_sf(data = coords_sf, color = "red", shape = 17, size = 1) +
  geom_hline(yintercept = min_Y, color = "black") +
  geom_vline(xintercept = min_X, color = "black") +
  labs(title = "Spatial Coordinates of Groundwater Levels in Crete, Greece",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Center and bold title
    axis.title.x = element_text(face = "bold"),            # Bold x-axis label
    axis.title.y = element_text(face = "bold"),            # Bold y-axis label
    panel.grid.major = element_blank(),                    # Remove major grid lines
    panel.grid.minor = element_blank()                     # Remove minor grid lines
  )

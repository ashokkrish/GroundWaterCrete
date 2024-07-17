# Install necessary packages
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("viridis")
# install.packages("openxlsx")
# install.packages("akima")
# install.packages("sp")
# install.packages("sf")
# install.packages("ggspatial")

# rm(list = ls())

# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(viridis)
library(openxlsx)
library(akima)
library(sp)
library(sf)
library(ggspatial)

GW_df <- read_excel("CRETE_REGION_GW_DATA_EDIT.xlsx", sheet = "Sheet1")

dim(GW_df)
names(GW_df)

GW_df_clean <- GW_df %>%
  filter(GW < 1000, GW != 0) %>%
  rename(
    East = `Easting (X)`,
    North = `Northing (Y)`,
  )

dim(GW_df_clean)
names(GW_df_clean)

# Descriptive statistics
summary(GW_df_clean$GW)

# Histogram
ggplot(GW_df_clean, aes(x = GW)) +
  geom_histogram(binwidth = 50, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Groundwater Levels in Crete, Greece", x = "Groundwater Level (masl)", y = "Frequency") +
  theme_minimal() + # Use a minimal theme for a clean look
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid.major = element_blank(),                    # Remove major grid lines
    panel.grid.minor = element_blank(),                    # Remove minor grid lines
  )

#-------------------------------------------------#
# Generating frequency tables using dplyr: Part I #
#-------------------------------------------------#

# Define the bins for GW values
bins <- c(0, 100, 200, 300, 400, 500, 600)

# Create a new column 'GW_bin' that categorizes GW values into bins
GW_df_clean <- GW_df_clean %>%
  mutate(GW_bin = cut(GW, breaks = bins, right = FALSE))

# Create a frequency table for the bins
frequency_table <- GW_df_clean %>%
  group_by(GW_bin) %>%
  summarise(Frequency = n())

# Calculate the relative frequency
frequency_table <- frequency_table %>%
  mutate(Relative_Frequency = Frequency/sum(Frequency))

#--------------------------------------------------#
# Generating frequency tables using dplyr: Part II #
#--------------------------------------------------#

# Display the frequency table
print(frequency_table)

# Define the shorter bins for GW values

shorterbins <- c(0, 10, 25, 50, 75, 100, 250, 750)

# Create a new column 'GW_shorter_bin' that categorizes GW values into shorter bins
GW_df_clean <- GW_df_clean %>%
  mutate(GW_shorter_bin = cut(GW, breaks = shorterbins, right = FALSE))

# Create a frequency table for the shorter bins
frequency_table_shorter <- GW_df_clean %>%
  group_by(GW_shorter_bin) %>%
  summarise(Frequency = n())

# Calculate the relative frequency
frequency_table_shorter <- frequency_table_shorter %>%
  mutate(Relative_Frequency = Frequency/sum(Frequency))

# Display the frequency table
print(frequency_table_shorter)

#------------------------#
# Create the spatial map #
#------------------------#

ggplot(GW_df_clean, aes(x = East, y = North, color = GW)) +
  geom_point(size = 0.75, alpha = 0.7) + # Adjust the size of points if needed
  # geom_hline(yintercept = min(GW_df_clean$North), color = "black") +
  # geom_vline(xintercept = min(GW_df_clean$East), color = "black") +
  scale_color_viridis(name = "GW (masl)", option = "H") + # Use the viridis color scale
  labs(title = "Spatial Map of Groundwater Levels in Crete, Greece", x = "Easting (X)", y = "Northing (Y)") +
  theme_minimal() + # Use a minimal theme for a clean look
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    #legend.position = "bottom",                            # Move legend to the bottom
    # panel.grid.major = element_blank(),                    # Remove major grid lines
    # panel.grid.minor = element_blank(),                    # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1) # Add border to the plot
  ) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         height = unit(0.75, "cm"), width = unit(0.75, "cm")) #+
  # annotation_scale(location = "bl", width_hint = 0.5, 
  #                  pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"))


write.xlsx(GW_df_clean, "CRETE_REGION_GW_DATA_EDIT_Clean.xlsx", rowNames = FALSE)

###########################################################

# # Convert the data frame to an sf object
# GW_sf <- st_as_sf(GW_df_clean, coords = c("East", "North"), crs = 4326) # Use an appropriate CRS
# 
# # Transform to a projected CRS (e.g., UTM Zone 35N for Crete)
# GW_sf <- st_transform(GW_sf, crs = 32635)

###########################################################

# , style = north_arrow_fancy_orienteering
# # Interpolate the data for contour and heatmap
# interp_data <- with(GW_df_clean, interp(x = East, y = North, z = GW, duplicate = "mean"))
# 
# # Convert the interpolated data to a data frame
# interp_df <- as.data.frame(expand.grid(x = interp_data$x, y = interp_data$y))
# interp_df$z <- as.vector(interp_data$z)
# 
# # Remove rows with NA values
# interp_df <- interp_df %>% filter(!is.na(z))

###########################################################

# # Create heatmap
# ggplot(interp_df, aes(x = x, y = y, fill = z)) +
#   geom_tile() +
#   scale_fill_viridis(name = "GW (masl)", option = "H", limits = range(interp_df$z)) +  # Specify limits if necessary
#   labs(title = "Heatmap of Groundwater Levels in Crete, Greece", x = "Easting (X)", y = "Northing (Y)") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
#     axis.title = element_text(size = 14, face = "bold"),
#     axis.text = element_text(size = 12),
#     legend.title = element_text(size = 14, face = "bold"),
#     legend.text = element_text(size = 12),
#     legend.position = "bottom",
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
#   )

###########################################################

# # Convert to SpatialPointsDataFrame
# coordinates(GW_df_clean) <- c("East", "North")
# proj4string(GW_df_clean) <- CRS("+proj=longlat +datum=WGS84")
# 
# # Convert to spatial object
# coordinates(GW_df_clean) <- ~ East + North
# proj4string(GW_df_clean) <- CRS("+proj=longlat +datum=WGS84")
# 
# # Calculate empirical variogram
# emp_variogram <- variogram(GW ~ 1, data = GW_df_clean)
# plot(emp_variogram, main = "Empirical Variogram")
# 
# # Fit variogram model
# vgm_model <- fit.variogram(emp_variogram, model = vgm(psill = 1, model = "Sph", range = 500, nugget = 0))
# plot(emp_variogram, model = vgm_model, main = "Fitted Variogram Model")
# 
# # Perform ordinary kriging
# kriging_result <- autoKrige(GW ~ 1, GW_df_clean)
# plot(kriging_result)
# 
# # Extract kriging predictions
# kriging_predictions <- kriging_result$krige_output

###########################################################

# # Plot kriging predictions
# ggplot() +
#   geom_raster(data = as.data.frame(kriging_predictions), aes(x = coords.x1, y = coords.x2, fill = var1.pred)) +
#   scale_fill_viridis(name = "Predicted GW (masl)") +
#   labs(title = "Kriging Predictions of Groundwater Levels", x = "Easting (X)", y = "Northing (Y)") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
#     axis.title = element_text(size = 14, face = "bold"),
#     axis.text = element_text(size = 12),
#     legend.title = element_text(size = 14, face = "bold"),
#     legend.text = element_text(size = 12),
#     legend.position = "bottom",
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_rect(color = "black", fill = NA, size = 1)
#   )
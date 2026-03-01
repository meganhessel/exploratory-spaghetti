# Extract all distances (reach) from connectivity matrix
# NOTE: This uses ONLY CURRENT DAMS (future dams not included in connectivity_matrix)
# NOTE: Not yet directional
# Requires running connectivity_matrix.qmd --- Environemnt should have: connectivity_matrix, dam_nodes, net_with_dams 

# Load libraries 
library(tidyverse)
library(ggplot2)
library(dplyr)
#library(renv)
library(sfnetworks)

# snapping and creating nodes of dams 
net_with_dams <- net %>% 
  st_network_blend(nepal_current_dams, tolerance = 1000)


# Get hyriv_id for each dam node - use indexing instead of slice
dam_node_tibble <- net_with_dams %>%
  activate("nodes") %>%
  as_tibble()

# Extract IDs by row position (dam_nodes contains the row indices)
dam_hyriv_ids <- dam_node_tibble$hyriv_id[dam_nodes]  # Direct indexing

# Create dam labels
dam_labels <- paste0("Dam_", dam_hyriv_ids)

# # Get hyriv_id for each dam node directly from network
# dam_hyriv_id <- net_with_dams %>%
#   activate("nodes") %>%
#   as_tibble() %>%
#   slice(dam_nodes) %>%
#   pull(hyriv_id)
# 
# # Create dam labels using full hyriv_id
# dam_labels <- paste0("Dam_", dam_hyriv_id)

# Get number of dams (rows/columns in symmetric matrix)
n_dams <- nrow(connectivity_matrix)

# Create row indices for upper triangle: repeat 1, then 1,1, then 1,1,1, etc.
row_idx <- rep(1:(n_dams-1), times = (n_dams-1):1)

# Create column indices for upper triangle: 2:n, then 3:n, then 4:n, etc.
col_idx <- unlist(lapply(2:n_dams, function(i) i:n_dams))

# Extract distance values from matrix using row/column index pairs
distances <- connectivity_matrix[cbind(row_idx, col_idx)]

# Create dataframe with columns: from dam, to dam, and distance
reach_df <- data.frame(
  from_dam = dam_labels[row_idx],           # Source dam label (hyriv_id)
  to_dam = dam_labels[col_idx],             # Destination dam label (hyriv_id)
  distance_m = distances,                   # Distance value in meters
  stringsAsFactors = FALSE                  # Keep character columns as character
)

# Count pairs with zero distance (dams at same network node)
zero_pairs <- sum(reach_df$distance_m == 0, na.rm = TRUE)

# Count pairs with duplicate distances (same distance value appears multiple times)
duplicate_distances <- sum(duplicated(reach_df$distance_m[reach_df$distance_m > 0]))

# Filter out zeros (dams at same location) and NA/Inf values (unreachable pairs)
reach_df <- reach_df[reach_df$distance_m > 0 & !is.na(reach_df$distance_m) & is.finite(reach_df$distance_m), ]

# Sort dataframe by distance column (ascending order)
reach_df <- reach_df[order(reach_df$distance_m), ]

# Look at dataframe 
head(reach_df)

# Create histogram plot: x-axis is distance in km
ggplot(reach_df, aes(x = distance_m / 1000)) +
  # Add histogram bars with 30 bins, blue fill
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  # Set axis labels and title
  labs(
    x = "Reach Distance (km)",              # X-axis label (convert m to km)
    y = "Count",                            # Y-axis label (number of pairs)
    title = "Distribution of Reach Distances Between Current Dams"  # Plot title (current dams only)
  ) +
  # Use minimal theme (clean, simple appearance)
  theme_minimal()


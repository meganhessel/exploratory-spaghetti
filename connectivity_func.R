#' This is the title.
#'
#' This is the description.
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param basin_hybas_id The hybas_id from HydroBasin. 
#' @param river_network A description .
#' @param current_dam_df Data frame of current dams. Need to have geometry column of POINTS. 
#' @param future_dam_df Data frame of future dams. Need to have geometry column of POINTS. 
#' 
#' @returns The sum of `x` and `y`.
#' @export


connectivity_func <- function(basin_hybas_id, river_network, current_dam_df, future_dam_df) {
  
  
  ######### ----------------- SET UP --------------------#########
  
  # ------------------------- BASIN -------------------------------
  # Load in all ASAIN basins 
  basin_level3 <- read_sf("../../../../../capstone/netzerohydro/data/raw/hybas_as_lev01-12_v1c/hybas_as_lev03_v1c.shp") %>% 
    janitor::clean_names()
  
  # Filter for specific basin
  country_basin <- basin_level3 %>% 
    filter(hybas_id == basin_hybas_id) # 4030025450 is nepals [need to make this more user friendly later]
  
  country_basin <- st_make_valid(country_basin) # Make geometries valid  
  
  
  # tm_shape(basin_level3) + tm_polygons() 
  # tm_shape(country_basin) + tm_polygons() 
  
  # ------------------------- RIVER -------------------------------
  # Filter river for order the 3+ 
  river_network <- river_network %>% 
    filter(ord_stra >= 3)
  
  river_network <- st_make_valid(river_network) # Make geometries valid 
  
  # Crop river to basin polygon 
  river_network <- river_network %>%
    st_filter(y = country_basin, .predicate = st_intersects)
  
  # tmap::tm_shape(river_network) + tm_lines() # Map check 
  
  # Make sure river_network is LINESTRINGS (for sfnetwork)
  river_network <- st_cast(river_network , "LINESTRING")
  
  # ------------------- DAMS ------------------------------
  # Crop to basin 
  current_dams <- current_dam_df %>%
    st_filter(y = country_basin, .predicate = st_intersects)
  
  future_dams <- future_dam_df %>%
    st_filter(y = country_basin, .predicate = st_intersects)
  
  # Map check 
  # tm_shape(country_basin) +
  #   tm_polygons(fill_alpha = 0.4, lwd = 2.5) +
  #   tm_basemap('Esri.WorldTopoMap') +
  #   tm_shape(current_dams) +
  #   tm_dots(fill = "red") +
  #   tm_shape(future_dams) +
  #   tm_dots(fill = "blue") +
  #   tm_shape(river_network) +
  #   tm_lines() +
  #   tm_add_legend(
  #     type = "polygons",
  #     labels = c("Current Dams", "Future Dams"),
  #     fill = c("red", "blue"),
  #     title = "Dam Status"
  #   ) +
  #   tm_title(text = "Dams") +
  #   tm_compass() +
  #   tm_scalebar()
 
  
  
  
   ######### -------------------- CONNECTIVITY --------------------#########
  
  # ------------------- CREATE RIVER NETWORK ---------------------------
  
  net <- as_sfnetwork(river_network, direction = TRUE) %>% # convert river geom into spatial network
    tidygraph::activate("edges") %>%  # "Next operation applied to edges table (not node table)"
    mutate(weight = edge_length()) # calculate spatial length of each edge (river segment)
  
  net_df <- as_tibble(net) # makes sfnetwork a df 
  
 
  # ------------------- CURRENT + FUTURE DAMS ---------------------------
  
  # CRS test 
  if (st_crs(current_dam_df) == st_crs(future_dam_df)) {
    print("Dam CRS match")
  } else {
    warning("Dam CRS do not match")
  }
  
  # Mark which nodes are dams (current vs future) before blending & select similar columns
  current_dams <- current_dam_df %>% 
    mutate(is_current_dam = TRUE) 
  
  future_dams <- future_dam_df %>% 
    mutate(is_current_dam = FALSE)  

  # Combine future and current dam dfs
  all_dams <- rbind(current_dams, future_dams)
  
  net_with_dams <- net %>% 
    st_network_blend(all_dams, tolerance = 1000)
  
  
  # ------------------- GRAPH ---------------------------
  # --------- Something wrong here --------------------

  # Which nodes are dams and making them a df
  current_dam_nodes_sf <- net_with_dams %>%
    tidygraph::activate("nodes") %>%
    as_tibble() %>%
    filter(is_current_dam == TRUE) %>%
    st_as_sf()

  future_dam_nodes_sf <- net_with_dams %>%
    tidygraph::activate("nodes") %>%
    as_tibble() %>%
    filter(is_current_dam == FALSE) %>%
    st_as_sf()

  # Plotting every line edge and juntion from net_with_dams
  ggplot() +
    geom_sf(data = st_as_sf(net_with_dams, "edges"), color = "gray40", size = 0.2) +
    geom_sf(data = st_as_sf(current_dam_nodes_sf, "nodes"), color = "red", size = 0.5) +
    geom_sf(data = st_as_sf(future_dam_nodes_sf, "nodes"), color = "blue", size = 0.5) +
    theme_minimal() +
    labs(title = "Current dams that became nodes with ggplot2")

  # ------------------- CONNECTIVITY MATRIX --------------------------
  
  # Getting the current dam node ID 
  current_dam_nodes <- net_with_dams %>% 
    tidygraph::activate("nodes") %>% 
    mutate(node_id = row_number()) %>% # create new column with node ID
    as_tibble() %>% 
    filter(is_current_dam == TRUE) %>% 
    pull(node_id) # list of all dam nodes 
  
  # Getting the future dam node ID 
  future_dam_nodes <- net_with_dams %>% 
    tidygraph::activate("nodes") %>% 
    mutate(node_id = row_number()) %>% # create new column with node ID
    as_tibble() %>% 
    filter(is_current_dam == FALSE) %>% 
    pull(node_id) # list of all dam nodes 
  
  # Create connective matrix 
  connectivity_matrix  <- distances(  # library(igraph) - calculates shortest path 
    net_with_dams, 
    v = current_dam_nodes, # starting nodes 
    to = future_dam_nodes, # ending nodes - creates all pairwise combinations 
    weights = tidygraph::activate(net_with_dams, "edges") %>% # use river segment lengths to calculate shortest path 
      pull(weight), # extract the `weight` column
    mode = "out" # DIRECTIONAL CONNECTIVITY 
  )
  
  connectivity_matrix
  
  # ------------------- Data frame --------------------------
  
  
  
  
  
  
}

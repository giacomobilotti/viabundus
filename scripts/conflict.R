#### Conflict ####

# Miller & Shuvo Bakar 2022. Journal of Conflict resolution 67(2-3), 522-554. DOI:10.1177/00220027221119085
library(readr) # for some reasons it is better than base r read.csv
library(ggplot2)
library(tmap)
library(rnaturalearth)
library(dplyr)
library(spatstat)

HCED <- read_csv("data/raw_data/conflict/HCED Data v2.csv")

# Drop the first column (row numbers)
HCED <- HCED[,-1] 
HCED <- HCED[as.numeric(HCED$Year) <= 1800 & as.numeric(HCED$Year) >= 1200,]
HCED <- HCED[!is.na(HCED$Longitude),] 

## Make it spatial and crop it to the study region 
nodes <- read.csv(file = file.path('data', 'raw_data', 'Viabundus-1.3-CSV', 'Nodes.csv'), 
                  fileEncoding = "UTF-8") |>
  sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) 

conflict <- sf::st_as_sf(
  HCED, coords = c('Longitude', 'Latitude'), crs = 4326
) |>
  sf::st_crop(terra::ext(-5, 40, 40, 65))

### Visualisation 

conflict$Year_num <- as.numeric(conflict$Year)
## Simple Histogram
ggplot(conflict, aes(x = Year_num)) +
  geom_histogram(binwidth = 25, fill = "steelblue", colour = "white") +
  theme_minimal() +
  labs(title = "Conflict over time", x = "Year", y = "Count")
## Density
ggplot(conflict, aes(x = Year_num)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density of Battles", x = "Year", y = "Density")


## Spatial visualisation 

world <- ne_countries(scale = "medium", returnclass = "sf") |>
  sf::st_crop(terra::ext(-5, 40, 40, 65))

conflict$Year_num <- as.numeric(conflict$Year)
conflict$Century <- cut(conflict$Year_num,
                        breaks = seq(1000, 2000, 100),
                        labels = paste0(seq(11,20), "th c."))

tm_shape(world, bbox = c(-3, 43, 38, 64)) +
  tm_borders(fill = "grey95", col = "grey70") +
  tm_shape(conflict) + 
  tm_dots(fill = 'Century', size = .5) +
  tm_facets(by = 'Century') +
  tm_legend(show = FALSE) +
  tm_grid(n.x = 3, n.y = 3) 

## Winners / losers 

top_winners <- conflict |>
  count(Winner, sort = TRUE) |>
  slice_head(n = 30)

ggplot(top_winners, aes(x = reorder(Winner, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top Winners", x = "Winner", y = "Battles Won")

top_losers <- conflict |>
  count(Loser, sort = TRUE) |>
  slice_head(n = 30)

ggplot(top_losers[!is.na(top_losers$Loser),], aes(x = reorder(Loser, n), y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top losers", x = "loser", y = "Battles Won")

### Count per area 
conflict_proj <- st_transform(conflict, 3035)
world_proj <- st_transform(world, 3035)

grid <- st_make_grid(conflict_proj, cellsize = 250000, what = "polygons") %>%
  st_sf(id = 1:length(.))

# Spatial join (each conflict to a grid cell)
conflict_grid <- st_join(conflict_proj, grid, join = st_within)

# Count battles per cell + century
conflict_grid_count <- conflict_grid |>
  st_drop_geometry() |>
  count(id, Century) |>
  left_join(grid, by = "id") |>
  st_sf()

# Plot
tm_shape(world_proj) +
  tm_polygons(col = "grey95") +
  tm_shape(conflict_grid_count) +
  tm_polygons("n", palette = "Reds", title = "Conflicts") +
  tm_facets(by = "Century") +
  tm_layout(legend.outside = TRUE)

### Real density

densities <- lapply(
  X = unique(conflict_proj$Century),
  FUN = function(period) {
    tmp_pp <- ppp(
      x = sf::st_coordinates(conflict_proj[conflict_proj$Century == period,])[,1],
      y = sf::st_coordinates(conflict_proj[conflict_proj$Century == period,])[,2],
      window = as.owin(st_bbox(world_proj))
    )
    
    tmp_dens <- density.ppp(tmp_pp, sigma = 25000, eps = 10000) |>
      terra::rast()
    
    terra::crs(tmp_dens) <- terra::crs(world_proj)
    
    tmp_dens <- terra::mask(
      tmp_dens, 
      terra::vect(world_proj)
    )
    return(tmp_dens)
  })
names(densities) <- unique(conflict_proj$Century)

library(terra)

dens_stack <- terra::rast(densities)  

for(i in 1:nlyr(dens_stack)) {
  values(dens_stack[[i]]) <- values(dens_stack[[i]]) / max(values(dens_stack[[i]]), na.rm = TRUE)
}

tm_shape(world_proj) +
  tm_polygons(col = "grey95") +
  tm_shape(dens_stack) +
  tm_raster(
    col.scale = tm_scale_intervals(
      values = "YlOrRd",
      breaks = c(0,.05,.1,.15,.5,.75,1)
    ),
    col.legend = tm_legend(show = FALSE)
    ) 

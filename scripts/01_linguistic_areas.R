## Area creation 

library(sf)

areadir <- file.path('data', 'raw_data', 'Seshat-cliopatria')

# Read GeoJSON
cliopatria <- st_read(file.path(areadir, 'cliopatria_polities_only.geojson')) 

# Filter for the late middle ages / modern era and europe
cliopatria <- cliopatria[cliopatria$FromYear > 1299 & cliopatria$ToYear < 1651,] 

# Turn off spherical geometry
sf_use_s2(FALSE)

# Create bbox polygon
bbox_coords <- c(xmin = 0, ymin = 45, xmax = 40, ymax = 65)
bbox_poly <- st_as_sfc(st_bbox(bbox_coords, crs = st_crs(cliopatria)))

# Now filter
cliopatria <- cliopatria[cliopatria$FromYear > 1299 & cliopatria$ToYear < 1651,] |>
  st_make_valid() |>
  st_crop(bbox_poly)

## Make it spatial and crop it to the study region 
source('scripts/00_loading_data.R')

nodes <- st_as_sf(nodes, coords = c('Longitude', 'Latitude'), 
                  crs = 4326) |>
  st_transform(3035)

# Load modern countries for background maps
w <- rnaturalearthhires::map_units10[rnaturalearthhires::map_units10$REGION_UN == 'Europe',] |>
  st_transform(3035) |>
  st_crop(st_buffer(nodes, 1.5e+05))

spatial_pop <- merge(population,  nodes, by.x = 'Nodes_ID', by.y = 'ID', all.x = TRUE)
# reset it as spatial
spatial_pop <- st_as_sf(spatial_pop)

dutch <- st_union(cliopatria[grepl('Kingdom of Spain', cliopatria$Name) & cliopatria$FromYear == 1556,])

danish <- st_union(cliopatria[grepl('Denmark', cliopatria$Name) & cliopatria$FromYear == 1363,])

germanic <- cliopatria[grepl('Holy', cliopatria$Name) & cliopatria$FromYear == 1440,] |>
  st_difference(dutch) |>
  st_union()

slavic <- st_union(cliopatria[grepl('Liv|Russ|Pol|Jag', cliopatria$Name) & cliopatria$FromYear == 1556,1]) |> # it is a good approximation but
  st_difference(germanic)

## add liege to the netherlands
liege <- st_intersection(
  cliopatria[cliopatria$FromYear == 1556 & cliopatria$Name == 'Holy Roman Empire Minor States',],
  st_transform(st_union(w[grepl('Netherlands|Belgium|Lux', w$SOVEREIGNT) ,]), st_crs(cliopatria))
) |>
  st_cast('POLYGON')
liege$area <- st_area(liege)
liege <- liege[liege$area == max(liege$area),]

dutch <- st_union(dutch, liege) |>
  st_cast('POLYGON')
dutch$area <- st_area(dutch)
dutch <- dutch[dutch$area == max(dutch$area),]

areas <- c(dutch, danish, germanic, slavic)
areas$region <- c('dutch', 'danish', 'germanic', 'slavic')

st_write(
  obj = areas, 
  dsn  = file.path('data', 'derived_data', 'linguistic_areas.gpkg')
)

# #### Plot ####
# library(tmap)
# 
# tm_shape(w) +
#   tm_fill(fill = 'grey95') +
#   tm_shape(st_union(germanic)) +
#   tm_fill(fill = 'darkgrey', fill_alpha = .75) +
#   tm_shape(dutch) +
#   tm_fill(fill = 'darkseagreen4', fill_alpha = .75) +
#   tm_shape(danish) +
#   tm_fill(fill = 'steelblue', fill_alpha = .75) +
#   tm_shape(slavic) +
#   tm_fill(fill = 'darksalmon', fill_alpha = .75) +
#   tm_shape(w) +
#   tm_borders() +
#   tm_shape(spatial_pop, is.main = T) +
#   tm_dots(size = .1) +
#   tm_graticules() +
#   tm_scalebar(breaks = c(0, 250, 500, 750), position = c('RIGHT', 'BOTTOM'), bg.color = 'white', bg.alpha = .5) +
#   tm_add_legend(
#     labels = c('Danish', 'Dutch', 'Germanic', 'Slavic'),
#     type = 'symbols', shape = 22, fill = c('steelblue', 'darkseagreen4', 'darkgrey', 'darksalmon'),
#     title = 'Legend',
#     position = c('TOP', 'LEFT')
#   ) +
#   tm_layout(bg.color = rgb(0,.4,.8,.1), bg.alpha = .5, legend.bg.color = 'white')
# 
# 
# tm_shape(w) +
#   tm_fill(fill = 'grey95') +
#   tm_shape(cliopatria[grepl('Poland|Polish', cliopatria$Name) & cliopatria$FromYear == 1632,], is.main = TRUE,
#          bbox = st_bbox(st_buffer(st_union(w[grepl('Poland|Lithuania', w$SOVEREIGNT),]), 50000))
# ) +
#   tm_fill(fill = 'darksalmon', fill_alpha = .75) + 
#   tm_shape(w) +
#   tm_borders() +
#   tm_shape(st_union(w[grepl('Poland|Lithuania', w$SOVEREIGNT),])) +
#   tm_borders(col = 'darkorange', lwd = 3) +
#   tm_shape(spatial_pop) +
#   tm_dots() +
#   tm_add_legend(
#     labels = 'Polish-Lithuanian Commonwealth (1632)',
#     shape = 22, fill = 'darksalmon', title = 'Legend'
#   ) +
#   tm_add_legend(
#     labels = 'Modern Poland & Lithuania',
#     type = 'lines',
#     col  = 'darkorange'
#   ) +
#   tm_graticules(n.x = 4, n.y = 4) +
#   tm_scalebar(width = 25, position = c(.7, .04), bg.color = 'white', bg.alpha = .5) +
#   tm_layout(bg.color = rgb(0,.4,.8,.1), bg.alpha = .5, legend.bg.color = 'white',
#             legend.position = c('left', 'top')) 
# 
# 
# tm_shape(w) +
#   tm_fill(fill = 'grey95') +
#   tm_shape(cliopatria[grepl('Kingdom of Spain', cliopatria$Name) & cliopatria$FromYear == 1556,]) +
#   tm_fill(fill = 'darkseagreen4')  +
#   tm_shape(liege) +
#   tm_fill(fill = 'palevioletred2') +
#   tm_shape(st_union(w)) +
#   tm_borders() +
#   tm_shape((w[grepl('Netherlands|Belgium|Lux', w$SOVEREIGNT) ,]), is.main = T,
#            bbox = st_bbox(st_buffer(w[grepl('Netherlands|Belgium', w$SOVEREIGNT),], 50000))) +
#   tm_borders(col = 'darkolivegreen1', lwd = 3) +
#   # tm_facets(by = 'ToYear') +
#   tm_shape(spatial_pop) +
#   tm_dots() +
#   tm_add_legend(
#     labels = c('Low countries in 1556', 'Prince-Bishopric of Liege'),
#     shape = 22, fill = c('darkseagreen4', 'palevioletred2'), title = 'Legend'
#   ) +
#   tm_add_legend(
#     labels = 'Modern Belgium & The Netherlands',
#     type = 'lines',
#     col  = 'darkolivegreen1'
#   ) +
#   tm_graticules(n.x = 3, n.y = 4) +
#   tm_scalebar(width = 20, position = c(.7, .04), bg.color = 'white', bg.alpha = .5) +
#   tm_layout(bg.color = rgb(0,.4,.8,.1), bg.alpha = .5, legend.bg.color = 'white',
#             legend.position = c('left', 'top')) 
# 
# tm_shape(st_union(w)) +
#   tm_fill(fill = 'grey95') +
#   tm_shape(danish, is.main = T,
#              bbox = st_bbox(st_buffer(st_union(w[grepl('Denmark', w$SOVEREIGNT) ,]), 150000))
#     ) +
#   tm_fill(fill = 'steelblue') +
#   tm_shape(st_union(w)) +
#   tm_borders() +
#     tm_shape(st_union(w[grepl('Denmark', w$SOVEREIGNT) ,])) +
#   tm_borders(col = 'lightskyblue2', lwd = 3) +
#   # tm_facets(by = 'ToYear') +
#   tm_shape(spatial_pop) +
#   tm_dots() +
#   tm_add_legend(
#     labels = 'Kingdom of Denmark in 1363',
#     shape = 22, fill = 'steelblue', title = 'Legend'
#   ) +
#   tm_add_legend(
#     labels = 'Modern Denmark',
#     type = 'lines',
#     col  = 'lightskyblue2'
#   ) +
#   tm_graticules(n.x = 3, n.y = 4) +
#   tm_scalebar(breaks = c(0,50,100,150,200), position = c(.72, .04), bg.color = 'white', bg.alpha = .5) +
#   tm_layout(bg.color = rgb(0,.4,.8,.1), bg.alpha = .5, legend.bg.color = 'white',
#             legend.position = c('left', 'top')) 
# sf_use_s2(TRUE)

## Copenhagen is out of the shapefile due to resolution of the shape. Fix it manually or add a buffer when analysying DK
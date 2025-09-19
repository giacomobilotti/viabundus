## Load viabundus data

sourcedir <- file.path('data', 'raw_data', 'Viabundus-1.3-CSV')
targetdir <- file.path('data', 'derived_data')
if(!dir.exists(targetdir)) dir.create(targetdir)

population <- read.csv(file = file.path(sourcedir, 'Population.csv'))
# Nodes_ID connects population to nodes. in Nodes it corresponds to ID
nodes <- read.csv(file = file.path(sourcedir, 'Nodes.csv'), fileEncoding = "UTF-8")

# If you want to read the description, you might need to convert the HTML escapes to regular scripts
# install.packages('xml2')
# library(xml2)
# 
# # Decode HTML entities in a vector (e.g. nodes$Name)
# nodes$Settlement_Description <- vapply(
#   nodes$Settlement_Description,
#   function(x) xml_text(read_html(paste0("<x>", x, "</x>"))),
#   FUN.VALUE = character(1)
# )

edges <- read.csv(file = file.path(sourcedir, 'Edges.csv'), fileEncoding = "UTF-8")
# From_Node and To_Node contain the node ID

nodes <- st_as_sf(nodes, coords = c('Longitude', 'Latitude'), 
                  crs = 4326) |>
  st_transform(3035)

st_write(nodes, dsn = file.path(targetdir, 'nodes.gpkg'))
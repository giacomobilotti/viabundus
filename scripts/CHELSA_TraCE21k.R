#### Downloading and cropping CHELSA_TraCE21k data ####
# data can be downloaded from: https://envicloud.wsl.ch/#/?bucket=https%3A%2F%2Fos.zhdk.cloud.switch.ch%2Fchelsav1%2F&prefix=chelsa_trace%2F

# However, we only need a small subset of the data, between 1300 and 1700 AD

# Download links look like this:
# https://os.zhdk.cloud.switch.ch/chelsav1/chelsa_trace/bio/CHELSA_TraCE21k_bio01_13_V1.0.tif 

# what we need to change is the last part as follows:
# CHELSA-[model]_[variable]_[[month]]_[timeID]_[version].tif
# month is only available for pr, tasmax, tasmin, but not for annual values such as dem, swe, scd, glz, or bio


## For now we can focus on:

# bio01: annual mean T
# bio05: Max T of warmest month
# bio12: Min T of coldest
# bio12: annual precipitation (kg m-2 year-1)
# bio13: precipitation of the wettest month
# bio14: p of the driest

variables <- c('bio01', 'bio05', 'bio06', 'bio12', 'bio13', 'bio14')

## The time IDs are the numbers of the corresponding century:

centuries <- 13:18

tmp_link <-'https://os.zhdk.cloud.switch.ch/chelsav1/chelsa_trace/bio/CHELSA_TraCE21k_'

links <- lapply(
  X = centuries,
  FUN = function(timeID) {
    sapply(
      X = variables,
      FUN = function(bio) {
        tmp_name <- paste0(tmp_link, bio, '_', timeID, '_V1.0.tif') 
      })
  })
names(links) <- centuries

## Download and crop 

library(terra)

## Area 

# load data (nodes)
source('analyses/00_loading_data.R')
nodes <- st_as_sf(nodes, coords = c('Longitude', 'Latitude'), 
                  crs = 4326) 
terra::ext(nodes)

# Example: your list of links is called `inks`
# inks <- list(c("https://...13.tif", "https://...14.tif", ...), ...)

# Create output base folder
out_base <- "climate_data"
dir.create(out_base, showWarnings = FALSE)

# Define crop extent (example: xmin, xmax, ymin, ymax)
# Replace with your real bounding box
ext <- ext(-10, 40, 30, 70)  # example Europe

# Loop over list
for (cent in seq_along(links)) {
  for (url in links[[cent]]) {
    
    # print(url)
    # Get file name (the CHELSA naming already has num inside)
    fname <- basename(url)

    # Extract the number from index
    num <- names(links)[cent]

    # Create target folder
    out_dir <- file.path(out_base, num)
    if(!dir.exists(out_dir)) dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Temporary download
    tmpfile <- file.path(tempdir(), fname)
    if (!file.exists(tmpfile)) {
      download.file(url, tmpfile, mode = "wb")
    }

    # Read, crop, and save
    r <- rast(tmpfile)
    r_crop <- crop(r, ext)

    out_file <- file.path(out_dir, fname)
    writeRaster(r_crop, out_file, overwrite = TRUE)

    message("Saved: ", out_file)
  }
}


library(data.table)
library(openxlsx)
library(readxl)
library(lubridate)
library(sf)
library(sp)
library(rgdal)
library(maptools)

getZipFromUrl <- function(download_url, local_dir_name) {
  
  local_dir <- paste("./data-raw", local_dir_name, sep = "/")
  
  if (!dir.exists(local_dir)) {
    dir.create(local_dir)
  }
  
  temp <- tempfile(fileext = ".zip")
  download.file(download_url, temp, mode = "wb")
  
  files <- unzip(temp, exdir = local_dir)
  
  return(files)
}


createColnames <- function(cnames) {
  return(make.names(gsub(".", "_", make.names(gsub("[\\s]{2,}", " ", tolower(cnames))), fixed = TRUE), unique = TRUE))
}


# GP list data ------------------------------------------------------------
## April 2019

gp_list_lsoa_files <- getZipFromUrl("https://files.digital.nhs.uk/16/740C9E/gp-reg-pat-prac-lsoa-male-female-apr-19.zip", 
                       "gp_list_sex_lsoas")

gp_list_csv_urls <- c("https://files.digital.nhs.uk/83/EBDC33/gp-reg-pat-prac-sing-age-female.csv",
                      "https://files.digital.nhs.uk/E5/E97ADD/gp-reg-pat-prac-sing-age-male.csv")

gp_list_lsoa_file_list <- lapply(c(gp_list_lsoa_files, gp_list_csv_urls), fread)
gp_list_lsoa <- rbindlist(gp_list_lsoa_file_list, fill = TRUE)

setnames(gp_list_lsoa, createColnames(colnames(gp_list_lsoa)))
gp_list_lsoa <- gp_list_lsoa[substr(lsoa_code, 1, 1) == "E"]

gp_list_lsoa[, colnames(gp_list_lsoa)[!(colnames(gp_list_lsoa) %in% c("practice_code", 
                                                                      "lsoa_code", 
                                                                      "sex",
                                                                      "age",
                                                                      "number_of_patients"))] := NULL]

gp_list_lsoa[order(-number_of_patients), ':=' (pc = number_of_patients / sum(number_of_patients),
                                               rk = 1:.N), 
             by = .(sex, lsoa_code)]


# Opt-out data ------------------------------------------------------------
## March 2019




# Map data ----------------------------------------------------------------


lsoa_boundary_files <- getZipFromUrl("https://opendata.arcgis.com/datasets/e993add3f1944437bc91ec7c76100c63_0.zip?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D", 
                       "lsoa_boundaries")

shp_file <- lsoa_boundary_files[grep("\\.shp$", lsoa_boundary_files)]
layer_name <- substring(basename(shp_file), 1, nchar(basename(shp_file)) - 4)

lsoa_boundaries = st_read(shp_file,
                 query = paste0("SELECT LSOA11CD FROM \"", layer_name, "\" WHERE SUBSTR(LSOA11CD, 1, 1) = 'E'"),
                 stringsAsFactors = FALSE)


st_transform(x, crs


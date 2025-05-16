library(tidyverse)
library(odbc)
library(DBI)
library(getPass)
library(keyring)
library(lifecycle)
library(data.table)
library(crabpack)
library(INLA)
library(sdmTMB)
library(glmmTMB)
library(broom)
library(sf)
library(gstat)
library(rnaturalearth)
library(raster)
library(concaveman)
library(png)
library(mgcv)
library(dplyr)
library(mgcv)
library(ggplot2)
library("rnaturalearth")
library(patchwork)
library(gratia)
library(MuMIn)
library(DHARMa)
library(mgcViz)

# Set years
current_year <- 2024
years <- c(1988:2007, 2009:2013, 2015, 2017:2019, 2021:current_year)

# Specify directory
#dir <- "Y:/KOD_Research/Ryznar/Crab functional maturity"

data_dir <- "Y:/KOD_Survey/EBS Shelf/Data_Processing/Data/" # for survey data


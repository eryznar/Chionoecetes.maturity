# PURPOSE: 
# To get updated survey data by year for chela maturity processing

# Notes:
# 1) What is the best way to update data? Use the chela database and then specimen data each new year? Or will
# the chela database be updated in time?

# LOAD LIBS/PARAMS -----
source("./Scripts/load_libs_params.R")

# # Set channel
# channel <- "API"

# GET SNOW SURVEY DATA FROM CRABPACK ----
# # Pull specimen data
# species <- "SNOW"
# specimen_data <- crabpack::get_specimen_data(species = species,
#                                              region = "EBS",
#                                              years = years, # set in libs/params script
#                                              channel = channel)
# 
# saveRDS(specimen_data, "./Data/snow_survey_specimenEBS.rda")
# 
# # Calculate per-station CPUE by 1mm bins for weighting for ALL shell 2 males, not just chela msrd
# snow_cpue <- calc_cpue(crab_data = readRDS("./Data/snow_survey_specimenEBS.rda"),
#                        species = species,
#                        years = years, # set in libs/params script
#                        sex = "male",
#                        shell_condition = "new_hardshell",
#                        bin_1mm = TRUE) %>%
#   dplyr::select(YEAR, SPECIES, STATION_ID, LATITUDE, LONGITUDE, SIZE_1MM, CPUE)
# 
# saveRDS(snow_cpue, "./Data/snow_survey_CPUE_male1mm.rda")
# 
# # GET TANNER SURVEY DATA FROM CRABPACK ----
# # Pull specimen data
# species <- "TANNER"
# specimen_data <- crabpack::get_specimen_data(species = species,
#                                              region = "EBS",
#                                              years = years, # set in libs/params script
#                                              channel = channel)
# 
# saveRDS(specimen_data, "./Data/tanner_survey_specimenEBS.rda")
# 
# # Calculate per-station CPUE by 1mm bins for weighting for ALL shell 2 males, not just chela msrd
# tanner_cpue <- calc_cpue(crab_data = readRDS("./Data/tanner_survey_specimenEBS.rda"),
#                        species = species,
#                        years = years, # set in libs/params script
#                        sex = "male",
#                        shell_condition = "new_hardshell",
#                        bin_1mm = TRUE) %>%
#   dplyr::select(YEAR, SPECIES, STATION_ID, LATITUDE, LONGITUDE, SIZE_1MM, CPUE)
# 
# saveRDS(tanner_cpue, "./Data/tanner_survey_CPUE_male1mm.rda")

# LOAD AND PROCESS SURVEY DATA
snow_spec <- readRDS("./Data/snow_survey_specimenEBS.rda")$specimen %>%
                  filter(HAUL_TYPE !=17, SEX == 1, SHELL_CONDITION == 2, is.na(CHELA_HEIGHT) == FALSE,
                          YEAR %in% years) %>% # make sure filter for males, sh2, only chela msrd, not HT17
                   mutate(RATIO = SIZE/CHELA_HEIGHT) %>%
                   filter(RATIO > 2 & RATIO < 35) %>% # filter extreme measurements
                   dplyr::select(!c(RATIO))
                
snow_cpue <- readRDS("./Data/snow_survey_CPUE_male1mm.rda")

tanner_spec <- readRDS("./Data/tanner_survey_specimenEBS.rda")$specimen %>%
                    filter(HAUL_TYPE !=17, SEX == 1, SHELL_CONDITION == 2, is.na(CHELA_HEIGHT) == FALSE,
                           YEAR %in% years) %>% # make sure filter for males, sh2, only chela msrd, not HT17
                    mutate(RATIO = SIZE/CHELA_HEIGHT) %>%
                    filter(RATIO > 2 & RATIO < 35) %>% # filter extreme measurements
                    dplyr::select(!c(RATIO))
                    
tanner_cpue <- readRDS("./Data/tanner_survey_CPUE_male1mm.rda")


# Chela database data compiled by shannon
chela_db <- read.csv(paste0(data_dir, "specimen_chela.csv")) %>% # already != HT 17, only shell 2, no special projects
                    filter(HAUL_TYPE !=17, SEX == 1, SHELL_CONDITION == 2, is.na(CHELA_HEIGHT) == FALSE,
                           YEAR %in% years) %>% # filter for males, sh2, only chela msrd, not HT17
                    mutate(RATIO = SIZE/CHELA_HEIGHT) %>%
                    filter(RATIO > 2 & RATIO < 35) %>% # filter extreme measurements
                    dplyr::select(!c(RATIO)) %>%
                    mutate(LN_CH = log(CHELA_HEIGHT),
                          LN_CW = log(SIZE))


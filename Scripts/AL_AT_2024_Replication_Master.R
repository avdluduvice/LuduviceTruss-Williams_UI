# -------------------------------------------------------------------------------
# Title: How Insured Are Workers Against Unemployment? Unemployment 
# Insurance and the Distribution of Liquid Wealth 
# Purpose: Replication codes for FRBC's Economic Commentary
# File: Master script for running replication package
# Authors: Andre  Victor D. Luduvice and Anaya Truss-Williams
# Cleveland, September 2024
# -------------------------------------------------------------------------------

#### Startup --------------------------------------------------------------------

library(tidyverse)  # used for general data manipulation and transformations
library(data.table) # used for general data manipulation and transformations
library(readxl)     # required to read in CPI inflation data
library(Hmisc)      # required to weight summary stats
library(vtable)     # required to generate summary table
library(ggthemes)   # required for ggplot theme
library(fredr)      # required for recession shading
library(ecm)        # required for recession shading
library(zoo)        # requires for date transformations
library(writexl)    # required for writing tables


#### Setting up directory and creating folders ----------------------------------

setwd("filepath/LuduviceTruss-Williams_UI-main")   # add filepath to directory

# set paths for charts and tables
Chart_Path <- paste0(getwd(), "/Charts/")
Tables_Path <- paste0(getwd(), "/Tables/")

# create directories for charts and tables
dir.create(paste0(getwd(), Chart_Path))
dir.create(paste0(getwd(), Tables_Path))

#### Data Input -----------------------------------------------------------------

# identifiers (for all, cases are converted to uppercase to match csv file)
id_dem <- toupper(c("shhadid", "spanel", "ssuid", "swave", "pnum", "monthcode", "wpfinwgt", "erelrpe"))

# demographic characteristics
id_dem <- toupper(c(id_dem, "tage", "esex", "erace", "eeduc", "eorigin"))

# income and earnings variables
earnings_inc <- toupper(c("thtotinc","tptotinc", "tpearn", "eedftpt"))

# asset variables
assets <- toupper(c("thnetworth", "tnetworth", "tval_ast", "tval_bank", "tval_bond", "tval_bus", 
                    "tval_esav", "tval_home", "tval_oth", "tval_re", "tval_rent",
                    "tval_ret", "tval_rmu", "tval_stmf", "tval_veh"))

# debt variables
debt <- toupper("tdebt_cc")

# equity variables
equity <- toupper(c("teq_veh", "tveh1val"))

# unemployment variables
unemployment <- toupper(c("enjflag", "enj_layoff", "enj_nowrk9", "enj_layotyp", "eucany", 
                          "ejb1_jborse", "euctyp1yn", "euctyp2yn", "euctyp3yn",
                          "enj_lkwrk", "tuc1amt", "euc1mnyn", "euc2mnyn", "euc3mnyn"))


# download data using the following links:
# https://www2.census.gov/programs-surveys/sipp/data/datasets/2018/pu2018_csv.zip
# https://www2.census.gov/programs-surveys/sipp/data/datasets/2019/pu2019_csv.zip
# https://www2.census.gov/programs-surveys/sipp/data/datasets/2020/pu2020_csv.zip
# https://www2.census.gov/programs-surveys/sipp/data/datasets/2021/pu2021_csv.zip 
# https://www2.census.gov/programs-surveys/sipp/data/datasets/2022/pu2022_csv.zip
# https://www2.census.gov/programs-surveys/sipp/data/datasets/2023/pu2023_csv.zip 

# reading data from assigned directory and selecting relevant columns
pu2018 <- fread(paste0(getwd(),"/Data/pu2018.csv"), select = 
                  c(id_dem, earnings_inc, assets, debt, equity, unemployment)) %>% 
  mutate(Year = 2017)
gc()

pu2019 <- fread(paste0(getwd(), "/Data/pu2019.csv"), select = 
                  c(id_dem, earnings_inc, assets, debt, equity, unemployment)) %>%  
  mutate(Year = 2018)
gc()

pu2020 <- fread(paste0(getwd(), "/Data/pu2020.csv"), select = 
                  c(id_dem, earnings_inc, assets, debt, equity, unemployment)) %>%  
  mutate(Year = 2019)
gc()

pu2021 <- fread(paste0(getwd(), "/Data/pu2021.csv"), select =
                  c(id_dem, earnings_inc, assets, debt, equity, unemployment)) %>% 
  mutate(Year = 2020)
gc()

pu2022 <- fread(paste0(getwd(), "/Data/pu2022.csv"), select =
                  c(id_dem, earnings_inc, assets, debt, equity, unemployment)) %>% 
  mutate(Year = 2021)
gc()

pu2023 <- fread(paste0(getwd(), "/Data/pu2023.csv"), select =
                  c(id_dem, earnings_inc, assets, debt, equity, unemployment)) %>% 
  mutate(Year = 2022)
gc()


# adjusting variable types 
pu2018$ssuid <- as.character(pu2018$ssuid)
pu2018$shhadid <- as.character(pu2018$shhadid)
pu2018$spanel <- as.character(pu2018$spanel)
pu2018$eeduc <- as.numeric(pu2018$eeduc)
pu2018$theq_veh <- as.numeric(pu2018$theq_veh)

pu2019$ssuid <- as.character(pu2019$ssuid)
pu2019$shhadid <- as.character(pu2019$shhadid)
pu2019$spanel <- as.character(pu2019$spanel)
pu2019$eeduc <- as.numeric(pu2019$eeduc)
pu2019$theq_veh <- as.numeric(pu2019$theq_veh)

pu2020$ssuid <- as.character(pu2020$ssuid)
pu2020$shhadid <- as.character(pu2020$shhadid)
pu2020$spanel <- as.character(pu2020$spanel)
pu2020$eeduc <- as.numeric(pu2020$eeduc)
pu2020$theq_veh <- as.numeric(pu2020$theq_veh)

pu2021$ssuid <- as.character(pu2021$ssuid)
pu2021$shhadid <- as.character(pu2021$shhadid)
pu2021$spanel <- as.character(pu2021$spanel)
pu2021$eeduc <- as.numeric(pu2021$eeduc)
pu2021$theq_veh <- as.numeric(pu2021$theq_veh)

pu2022$ssuid <- as.character(pu2022$ssuid)
pu2022$shhadid <- as.character(pu2022$shhadid)
pu2022$spanel <- as.character(pu2022$spanel)
pu2022$eeduc <- as.numeric(pu2022$eeduc)
pu2022$theq_veh <- as.numeric(pu2022$theq_veh)

pu2023$ssuid <- as.character(pu2023$ssuid)
pu2023$shhadid <- as.character(pu2023$shhadid)
pu2023$spanel <- as.character(pu2023$spanel)
pu2023$eeduc <- as.numeric(pu2023$eeduc)
pu2023$theq_veh <- as.numeric(pu2023$theq_veh)

# creating an unique person identifier variable, pid
pu2018$pid <- paste0(pu2018$SSUID, pu2018$PNUM)
pu2019$pid <- paste0(pu2019$SSUID, pu2019$PNUM)
pu2020$pid <- paste0(pu2020$SSUID, pu2020$PNUM)
pu2021$pid <- paste0(pu2021$SSUID, pu2021$PNUM)
pu2022$pid <- paste0(pu2022$SSUID, pu2022$PNUM)
pu2023$pid <- paste0(pu2023$SSUID, pu2023$PNUM)

# appending the data sets
pu_all <- rbind(pu2018, pu2019)
gc()
pu_all <- rbind(pu_all, pu2020)
gc()
pu_all <- rbind(pu_all, pu2021)
gc()
pu_all <- rbind(pu_all, pu2022)
gc()
pu_all <- rbind(pu_all, pu2023)
gc()

# creating a merge-friendly coding for the month variable
pu_all <- pu_all %>% mutate(                        
  MONTHCODE = case_when(
    MONTHCODE == "1" ~ "01",
    MONTHCODE == "2" ~ "02",
    MONTHCODE == "3" ~ "03",
    MONTHCODE == "4" ~ "04",
    MONTHCODE == "5" ~ "05",
    MONTHCODE == "6" ~ "06",
    MONTHCODE == "7" ~ "07",
    MONTHCODE == "8" ~ "08", 
    MONTHCODE == "9" ~ "09",
    MONTHCODE == "10" ~ "10",
    MONTHCODE == "11" ~ "11",
    MONTHCODE == "12" ~ "12"
  ) 
)

# creating a year-month variable, for when we later merge with the cpi deflator
pu_all <- pu_all %>% 
  mutate(yearmonth = paste(Year, MONTHCODE, sep="_"))

# creates a year-person variable that makes each person unique at each year
pu_all <- pu_all %>% 
  mutate(yearpid = paste0(Year, pid))


#### Figure 1  ------------------------------------------------------------------
source(paste0(getwd(),"/AL_AT_2024_Replication_Figures_UR.R"), local = TRUE)

#### Data Transformations -------------------------------------------------------
source(paste0(getwd(),"/AL_AT_2024_Replication_Transformations_UI.R"), local = TRUE)

#### Figures 2-3, 5-10 ----------------------------------------------------------
source(paste0(getwd(),"/AL_AT_2024_Replication_Figures.R"), local = TRUE)

#### Generate Appendix Tables ---------------------------------------------------
source(paste0(getwd(),"/AL_AT_2024_Replication_Appendix.R"), local = TRUE)

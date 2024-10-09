# -------------------------------------------------------------------------------
# Title: How Insured Are Workers Against Unemployment? Unemployment 
# Insurance and the Distribution of Liquid Wealth 
# Purpose: Replication codes for FRBC's Economic Commentary
# File: Script to execute data transformations and calculations
# Authors: Andre  Victor D. Luduvice and Anaya Truss-Williams
# Cleveland, September 2024
# -------------------------------------------------------------------------------

#### Deflating ------------------------------------------------------------------

# US BLS-CPI file found in replication package can be retrieved at:
# https://data.bls.gov/timeseries/CUSR0000SA0&output 

cpi <- read_excel(paste0(getwd(), "/Data/BLS_CPI-U_1947_2024.xlsx"))

cpi <- cpi %>% select(Year, Period, Value) # dropping series ID
cpi <- cpi %>% mutate(Year = as.factor(Year))
cpi <- cpi %>% filter(Year == 2017 | Year == 2018 | Year == 2019 |
                        Year == 2020 | Year == 2021 | Year == 2022) 

# recoding to match monthcode in the pu_all dataset
cpi <- cpi %>% mutate(                        
  Period = case_when(
    Period == "M01" ~ "01",
    Period == "M02" ~ "02",
    Period == "M03" ~ "03",
    Period == "M04" ~ "04",
    Period == "M05" ~ "05",
    Period == "M06" ~ "06",
    Period == "M07" ~ "07",
    Period == "M08" ~ "08", 
    Period == "M09" ~ "09", 
    Period == "M10" ~ "10", 
    Period == "M11" ~ "11", 
    Period == "M12" ~ "12"
  ) 
)

# convert back to data table
cpi <- as.data.table(cpi)

# adding new index value variable, dec2017
cpi$dec2017 <- cpi[Year == 2017 & Period == 12]$Value 

# defining our base year-month deflator
cpi$deflator <- cpi$Value/cpi$dec2017

# creating a new yearmonth var to merge on since pu_all covers 5 years
cpi <- cpi %>% 
  mutate(yearmonth = paste(Year, Period, sep="_")) %>% 
  select(yearmonth, deflator) 

pu_all <- left_join(pu_all, cpi, by="yearmonth") # merging deflator with SIPP data

#pu_save <- pu_all # uncomment this in order to have a backup dataset of the raw data merged with CPI

# creating a variable to deflate stock variables only with the December value of each year
pu_all <- pu_all %>%
  mutate(deflatorDec = if_else(MONTHCODE == 12, deflator, NA ))
  
pu_all <- pu_all %>%
  group_by(Year) %>%
  mutate(deflatorDec = if_else(is.na(deflatorDec), first(na.omit(deflatorDec)), deflatorDec))

# nearly-identical process as above for weight to be applied to annual variables according to SIPP's guidelines
pu_all <- pu_all %>%
  mutate(weightDec = if_else(MONTHCODE == 12, WPFINWGT, NA ))

pu_all <- pu_all %>%
  group_by(Year, pid) %>%
  mutate(weightDec = mean(weightDec, na.rm = T)) # brute force approach to avoid attrition asymmetries


# deflating flow and stock vars 
pu_all <- pu_all %>% 
  mutate(
    TPEARN = TPEARN/deflator,
    TPTOTINC = TPTOTINC/deflator, 
    TUC1AMT = TUC1AMT/deflator,
  )

# deflating flow and stock vars 
pu_all <- pu_all %>% 
  mutate(
    TNETWORTH = TNETWORTH/deflatorDec,
    TVAL_BANK = TVAL_BANK/deflatorDec,
    TVAL_BOND = TVAL_BOND/deflatorDec,
    TVAL_OTH = TVAL_OTH/deflatorDec,
    TVAL_STMF = TVAL_STMF/deflatorDec,
    TEQ_VEH = TEQ_VEH/deflatorDec,
    TDEBT_CC = TDEBT_CC/deflatorDec,
  )

# creating measures of liquid wealth and net liquid wealth
pu_all <- pu_all %>%
  mutate(liq_wealth = TVAL_BANK + TVAL_BOND + TVAL_OTH + TVAL_STMF + TEQ_VEH,
         net_liq_wealth = liq_wealth - TDEBT_CC )

#### Annualizing ----------------------------------------------------------------

# agg earnings (wage x time) at the person level
pu_all <- pu_all %>%
  group_by(yearpid) %>%
  mutate(TPEARN_annual = sum(TPEARN, na.rm = T)) 

# agg income (wage x time plus return from assets) at the person level
pu_all <- pu_all %>% 
  group_by(yearpid) %>% 
  mutate(TPTOTINC_annual = sum(TPTOTINC, na.rm = T))

# agg ui benefits (ui benefit x time) at the person level
pu_all <- pu_all %>% 
  group_by(yearpid) %>% 
  mutate(TUC1AMT_annual = sum(TUC1AMT, na.rm = T))
 
#### Trimming -------------------------------------------------------------------

pu_all <- data.table(pu_all)
 
# trim pu_all's annual person level income and net worth to drop bottom 0.25 percent
bottom_income_trim <- 0.0025
pu_all <- pu_all[TPTOTINC_annual >= wtd.quantile(pu_all$TPTOTINC_annual, weights = pu_all$weightDec, 
                                                 probs = bottom_income_trim, na.rm = TRUE)]
bottom_wealth_trim <- 0.0025
pu_all <- pu_all[TNETWORTH >= wtd.quantile(pu_all$TNETWORTH, weights = pu_all$weightDec,
                                                             probs = bottom_wealth_trim, na.rm = TRUE)]

#### Creating quartiles ---------------------------------------------------------

# selecting relevant variables for analysis
dataGraphs <- pu_all %>%
  select(ENJFLAG, ENJ_LAYOFF, EUCANY, TUC1AMT, TUC1AMT_annual, EUCTYP1YN, EUCTYP2YN, EUCTYP3YN, 
         EUC1MNYN, EUC2MNYN, EUC3MNYN,TPEARN_annual, TAGE, ENJ_LKWRK, EJB1_JBORSE,
         liq_wealth, net_liq_wealth, TNETWORTH, TPTOTINC_annual, yearpid, pid, WPFINWGT, weightDec)

# filtering for augmented prime-age, negative annual earnings
dataGraphs <- dataGraphs %>% filter(TAGE >= 24 & TAGE <= 64)
dataGraphs <- dataGraphs %>% filter(TPEARN_annual >= 0)

# # calculating median annual personal income of the initial sample for quote in the text
# wtd.quantile(dataGraphs$TPTOTINC_annual, weights = dataGraphs$weightDec, probs = 0.5)

# moving to data.table
dataGraphs <- as.data.table(dataGraphs)

# household annual income quartiles
tpinc_quarts <- wtd.quantile(dataGraphs$TPTOTINC_annual, weights = dataGraphs$weightDec, probs = (0:4)/4, na.rm = TRUE)

# assignment of quartiles
dataGraphs[TPTOTINC_annual <= tpinc_quarts[[2]], tpinc_quartile := "1st Quartile"] #0-25
dataGraphs[TPTOTINC_annual <= tpinc_quarts[[3]] & TPTOTINC_annual > tpinc_quarts[[2]], tpinc_quartile := "2nd Quartile"] #25-50
dataGraphs[TPTOTINC_annual <= tpinc_quarts[[4]] & TPTOTINC_annual > tpinc_quarts[[3]], tpinc_quartile := "3rd Quartile"] #51-75
dataGraphs[TPTOTINC_annual > tpinc_quarts[[4]], tpinc_quartile := "4th Quartile"] #76-100

dataGraphs$tpinc_quartile <- factor(dataGraphs$tpinc_quartile, 
                                    levels = c("1st Quartile", "2nd Quartile", 
                                               "3rd Quartile", "4th Quartile"))

# household annual income halfs
tpinc_halfs <- wtd.quantile(dataGraphs$TPTOTINC_annual, weights = dataGraphs$weightDec, probs = 0.5, na.rm = TRUE)

# assignment of quartiles
dataGraphs[TPTOTINC_annual <= tpinc_halfs, tpinc_half := "Bottom Half"] #0-50
dataGraphs[TPTOTINC_annual > tpinc_halfs, tpinc_half := "Top Half"] #51-100

dataGraphs$tpinc_half <- factor(dataGraphs$tpinc_half, 
                                    levels = c("Bottom Half", "Top Half"))

#### Creating sample of unemployed individuals ----------------------------------

# selecting not self-employed, laid-off, looking for work or receiving UI
pu_unemp <- dataGraphs %>%
  filter(EJB1_JBORSE != 2 | is.na(EJB1_JBORSE)) %>% #excluding the self-employed at each month
  #filter(ENJFLAG == 1) %>% # keeps only workers who have been through a no-job spell (useful check)
  filter(ENJ_LAYOFF == 1)  %>% # keeping only workers laid-off on that month
  filter(ENJ_LKWRK == 1  | EUC1MNYN == 1) # keeping only workers who either were looking for work or received regular UI or both

# keeping those only receiving regular UI benefits
pu_unemp <- pu_unemp %>%
  filter(EUC2MNYN != 1 | is.na(EUC2MNYN)) %>%
  filter(EUC3MNYN != 1 | is.na(EUC3MNYN))

# to allow for better graphing and labels, this creates EUCANY as a factor var

pu_unemp <- pu_unemp %>% 
  mutate(EUCANY.f = factor(EUCANY, labels = c("Ever Received UI", "Never Received UI")))

# reorganizing the sample for so that each observation is an individual

# define sample with same individuals in different years as different obs.
pu_unemp_unique <- pu_unemp %>%
  distinct(yearpid, .keep_all = T) %>%
  as.data.table(pu_unemp_unique)

# define sample only with distinct individuals, drops the longitudinal dimension
pu_unemp_unique_strict <- pu_unemp %>%
  distinct(pid, .keep_all = T) %>%
  as.data.table(pu_unemp_unique_strict)

# # final check of UI recipiency
# pu_unemp_unique_recip <- pu_unemp_unique %>%
#   filter(EUCANY == 1)
# 
# pu_unemp_unique_nonrecip <- pu_unemp_unique %>%
#   filter(EUCANY == 2)
# 
# summary(pu_unemp_unique_nonrecip$TUC1AMT_annual) # needs to be all zeroes; it is!
# summary(pu_unemp_unique_recip$TUC1AMT_annual)  # needs to have no zeroes; it does not!
# 
# pu_hard_check <- pu_unemp_unique_recip %>%
#   filter(TUC1AMT_annual == 0)                  # more than zero obs

# redefine samples excluding individuals who claim to have received UI but report zero amount received
pu_unemp_unique <- pu_unemp_unique %>%
  filter(!(EUCANY == 1 & TUC1AMT_annual == 0))

pu_unemp_unique_strict <- pu_unemp_unique_strict %>%
  filter(!(EUCANY == 1 & TUC1AMT_annual == 0))

#### Counting -------------------------------------------------------------------

# count how many individuals per quartile
table(pu_unemp_unique %>% select(tpinc_quartile))
table(pu_unemp_unique_strict %>% select(tpinc_quartile))

# count how many individuals received vs didn't receive UI in each quartile
table(pu_unemp_unique %>% filter(tpinc_quartile == "1st Quartile") %>% select(EUCANY.f))
table(pu_unemp_unique %>% filter(tpinc_quartile == "2nd Quartile") %>% select(EUCANY.f))
table(pu_unemp_unique %>% filter(tpinc_quartile == "3rd Quartile") %>% select(EUCANY.f))
table(pu_unemp_unique %>% filter(tpinc_quartile == "4th Quartile") %>% select(EUCANY.f))

# count how many individuals received vs didn't receive UI in each quartile in the strictest sample
table(pu_unemp_unique_strict %>% filter(tpinc_quartile == "1st Quartile") %>% select(EUCANY.f))
table(pu_unemp_unique_strict %>% filter(tpinc_quartile == "2nd Quartile") %>% select(EUCANY.f))
table(pu_unemp_unique_strict %>% filter(tpinc_quartile == "3rd Quartile") %>% select(EUCANY.f))
table(pu_unemp_unique_strict %>% filter(tpinc_quartile == "4th Quartile") %>% select(EUCANY.f))

# Note: The Census requires at least 50 observations to display the median on wealth data for the SIPP.
#       All quartiles displayed in the final sample have at least 124 observations.

#### Preparing data for the graphs ----------------------------------------------

pu_Graphs <- pu_unemp_unique %>%
  select(pid, EUCANY, EUCANY.f, TPTOTINC_annual, net_liq_wealth, TNETWORTH, tpinc_quartile, weightDec) %>%
  group_by(EUCANY, tpinc_quartile) %>%
  mutate(TPTOTINC_quarts = wtd.quantile(TPTOTINC_annual, weights = weightDec, probs = 0.5, na.rm = T)) %>%
  mutate(net_liq_wealth_quarts = wtd.quantile(net_liq_wealth, weights = weightDec, probs = 0.5, na.rm = T)) %>%
  mutate(TNETWORTH_quarts = wtd.quantile(TNETWORTH, weights = weightDec, probs = 0.5, na.rm = T))

#### Extra calculations for text

net_liq_wealth_bottom_recip <- pu_unemp_unique %>% 
  filter(EUCANY == 1 & tpinc_half == "Bottom Half") %>%
  summarise(net_liq_wealth_bottom_recip = 
              wtd.quantile(net_liq_wealth, weights = weightDec, probs = 0.5, na.rm = T))

net_liq_wealth_bottom_nonrecip <- pu_unemp_unique %>% 
  filter(EUCANY == 2 & tpinc_half == "Bottom Half") %>%
  summarise(net_liq_wealth_bottom_nonrecip = 
              wtd.quantile(net_liq_wealth, weights = weightDec, probs = 0.5, na.rm = T))

# # calculating the ratio of median net liquid wealth btw recipients and non-recipients at the bottom half
# net_liq_wealth_bottom_recip/net_liq_wealth_bottom_nonrecip 


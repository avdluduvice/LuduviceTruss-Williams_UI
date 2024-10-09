# -------------------------------------------------------------------------------
# Title: How Insured Are Workers Against Unemployment? Unemployment 
# Insurance and the Distribution of Liquid Wealth 
# Purpose: Replication codes for FRBC's Economic Commentary
# File: Script to generate Tables A.1-A.3 of the appendix
# Authors: Andre  Victor D. Luduvice and Anaya Truss-Williams
# Cleveland, September 2024
# -------------------------------------------------------------------------------

#### Housekeeping ---------------------------------------------------------------

# defining weights by UI recipiency and quartiles for each table
weights_recip <- pu_unemp_unique[EUCANY == 1]$weightDec
weights_nonrecip <- pu_unemp_unique[EUCANY == 2]$weightDec

weights_recip_first <- pu_unemp_unique[EUCANY == 1 & tpinc_quartile == "1st Quartile"]$weightDec
weights_recip_second <- pu_unemp_unique[EUCANY == 1 & tpinc_quartile == "2nd Quartile"]$weightDec
weights_recip_third <- pu_unemp_unique[EUCANY == 1 & tpinc_quartile == "3rd Quartile"]$weightDec
weights_recip_fourth <- pu_unemp_unique[EUCANY == 1 & tpinc_quartile == "4th Quartile"]$weightDec

weights_nonrecip_first <- pu_unemp_unique[EUCANY == 2 & tpinc_quartile == "1st Quartile"]$weightDec
weights_nonrecip_second <- pu_unemp_unique[EUCANY == 2 & tpinc_quartile == "2nd Quartile"]$weightDec
weights_nonrecip_third <- pu_unemp_unique[EUCANY == 2 & tpinc_quartile == "3rd Quartile"]$weightDec
weights_nonrecip_fourth <- pu_unemp_unique[EUCANY == 2 & tpinc_quartile == "4th Quartile"]$weightDec


# selecting relevant variables
x <- pu_unemp_unique[,.(EUCANY,tpinc_quartile,TPTOTINC_annual,
                        TPEARN_annual,net_liq_wealth,TNETWORTH)]
## defining labels
labs <- data.frame(varnames = colnames(x),
                   tabnames = c("UI Recipient", "Income Quartile","Income",
                                "Annual Earnings", "Net Liquid Wealth","Net Worth"))

#### Table 1 - Recipients -------------------------------------------------------

tab_one_recip <- st(pu_unemp_unique[EUCANY == 1,.(TPTOTINC_annual,TPEARN_annual,TNETWORTH,net_liq_wealth)], 
     out = "csv", file = "Tables/TableA.1.csv",
   summ = c('min(x)', 'max(x)', 
            'wtd.mean(x, weights = weights_recip, na.rm = T)', 
            'wtd.quantile(x, weights = weights_recip, probs = 0.25)', 
            'wtd.quantile(x, weights = weights_recip, probs = 0.5)',
            'wtd.quantile(x, weights = weights_recip, probs = 0.75)',
            'notNA(x)'),
   summ.names = c("Min", "Max", "Mean", "0.25", "0.5", "0.75", "N"),
   title = "Summary Statistics - UI Recipients",
   labels = labs)

#### Table 1 - Non-recipients ---------------------------------------------------

tab_one_nonrecip <- st(pu_unemp_unique[EUCANY == 2,.(TPTOTINC_annual,TPEARN_annual,TNETWORTH,net_liq_wealth)], 
    out = "csv", file = "Tables/TableA.2.csv",
   summ = c('min(x)', 'max(x)', 
            'wtd.mean(x, weights = weights_nonrecip, na.rm = T)', 
            'wtd.quantile(x, weights = weights_nonrecip, probs = 0.25)', 
            'wtd.quantile(x, weights = weights_nonrecip, probs = 0.5)',
            'wtd.quantile(x, weights = weights_nonrecip, probs = 0.75)',
            'notNA(x)'),
   summ.names = c("Min", "Max", "Mean", "0.25", "0.5", "0.75", "N"),
   title = "Summary Statistics - Non-Recipients",
   labels = labs)

#### Table 2 - Recipients -------------------------------------------------------

tab_two_q1 <- st(pu_unemp_unique[EUCANY == 1 & tpinc_quartile == "1st Quartile",.(TPTOTINC_annual,TPEARN_annual,TNETWORTH,net_liq_wealth)], 
     out = "csv", file = "Tables/TableB.1.csv",
   summ = c('min(x)', 'max(x)', 
            'wtd.mean(x, weights = weights_recip_first, na.rm = T)', 
            'wtd.quantile(x, weights = weights_recip_first, probs = 0.25)', 
            'wtd.quantile(x, weights = weights_recip_first, probs = 0.5)',
            'wtd.quantile(x, weights = weights_recip_first, probs = 0.75)',
            'notNA(x)'),
   summ.names = c("Min", "Max", "Mean", "0.25", "0.5", "0.75", "N"),
   title = "First Income Quartile",
   labels = labs)

tab_two_q2  <-st(pu_unemp_unique[EUCANY == 1 & tpinc_quartile == "2nd Quartile",.(TPTOTINC_annual,TPEARN_annual,TNETWORTH,net_liq_wealth)], 
     out = "csv", file = "Tables/TableB.2.csv",
   summ = c('min(x)', 'max(x)', 
            'wtd.mean(x, weights = weights_recip_second, na.rm = T)', 
            'wtd.quantile(x, weights = weights_recip_second, probs = 0.25)', 
            'wtd.quantile(x, weights = weights_recip_second, probs = 0.5)',
            'wtd.quantile(x, weights = weights_recip_second, probs = 0.75)',
            'notNA(x)'),
   summ.names = c("Min", "Max", "Mean", "0.25", "0.5", "0.75", "N"),
   title = "Second Income Quartile",
   labels = labs)

tab_two_q3 <- st(pu_unemp_unique[EUCANY == 1 & tpinc_quartile == "3rd Quartile",.(TPTOTINC_annual,TPEARN_annual,TNETWORTH,net_liq_wealth)], 
     out = "csv", file = "Tables/TableB.3.csv",
   summ = c('min(x)', 'max(x)', 
            'wtd.mean(x, weights = weights_recip_third, na.rm = T)', 
            'wtd.quantile(x, weights = weights_recip_third, probs = 0.25)', 
            'wtd.quantile(x, weights = weights_recip_third, probs = 0.5)',
            'wtd.quantile(x, weights = weights_recip_third, probs = 0.75)',
            'notNA(x)'),
   summ.names = c("Min", "Max", "Mean", "0.25", "0.5", "0.75", "N"),
   title = "Third Income Quartile",
   labels = labs)

tab_two_q4 <- st(pu_unemp_unique[EUCANY == 1 & tpinc_quartile == "4th Quartile",.(TPTOTINC_annual,TPEARN_annual,TNETWORTH,net_liq_wealth)], 
     out = "csv", file = "Tables/TableB.4.csv",
   summ = c('min(x)', 'max(x)', 
            'wtd.mean(x, weights = weights_recip_fourth, na.rm = T)', 
            'wtd.quantile(x, weights = weights_recip_fourth, probs = 0.25)', 
            'wtd.quantile(x, weights = weights_recip, probs = 0.5)',
            'wtd.quantile(x, weights = weights_recip, probs = 0.75)',
            'notNA(x)'),
   summ.names = c("Min", "Max", "Mean", "0.25", "0.5", "0.75", "N"),
   title = "Fourth Income Quartile",
   labels = labs)


#### Table 3 - Non-Recipients ---------------------------------------------------

tab_three_q1 <- st(pu_unemp_unique[EUCANY == 2 & tpinc_quartile == "1st Quartile",.(TPTOTINC_annual,TPEARN_annual,TNETWORTH,net_liq_wealth)], 
     out = "csv", file = "Tables/TableC.1.csv",
   summ = c('min(x)', 'max(x)', 
            'wtd.mean(x, weights = weights_nonrecip_first, na.rm = T)', 
            'wtd.quantile(x, weights = weights_nonrecip_first, probs = 0.25)', 
            'wtd.quantile(x, weights = weights_nonrecip_first, probs = 0.5)',
            'wtd.quantile(x, weights = weights_nonrecip_first, probs = 0.75)',
            'notNA(x)'),
   summ.names = c("Min", "Max", "Mean", "0.25", "0.5", "0.75", "N"),
   title = "First Income Quartile",
   labels = labs)


tab_three_q2 <- st(pu_unemp_unique[EUCANY == 2 & tpinc_quartile == "2nd Quartile",.(TPTOTINC_annual,TPEARN_annual,TNETWORTH,net_liq_wealth)], 
     out = "csv", file = "Tables/TableC.2.csv",
   summ = c('min(x)', 'max(x)', 
            'wtd.mean(x, weights = weights_nonrecip_second, na.rm = T)', 
            'wtd.quantile(x, weights = weights_nonrecip_second, probs = 0.25)', 
            'wtd.quantile(x, weights = weights_nonrecip_second, probs = 0.5)',
            'wtd.quantile(x, weights = weights_nonrecip_second, probs = 0.75)',
            'notNA(x)'),
   summ.names = c("Min", "Max", "Mean", "0.25", "0.5", "0.75", "N"),
   title = "Second Income Quartile",
   labels = labs)

tab_three_q3 <- st(pu_unemp_unique[EUCANY == 2 & tpinc_quartile == "3rd Quartile",.(TPTOTINC_annual,TPEARN_annual,TNETWORTH,net_liq_wealth)], 
     out = "csv", file = "Tables/TableC.3.csv",
   summ = c('min(x)', 'max(x)', 
            'wtd.mean(x, weights = weights_nonrecip_third, na.rm = T)', 
            'wtd.quantile(x, weights = weights_nonrecip_third, probs = 0.25)', 
            'wtd.quantile(x, weights = weights_nonrecip_third, probs = 0.5)',
            'wtd.quantile(x, weights = weights_nonrecip_third, probs = 0.75)',
            'notNA(x)'),
   summ.names = c("Min", "Max", "Mean", "0.25", "0.5", "0.75", "N"),
   title = "Third Income Quartile",
   labels = labs)

tab_three_q4 <- st(pu_unemp_unique[EUCANY == 2 & tpinc_quartile == "4th Quartile",.(TPTOTINC_annual,TPEARN_annual,TNETWORTH,net_liq_wealth)], 
     out = "csv", file = "Tables/TableC.4.csv",
   summ = c('min(x)', 'max(x)', 
            'wtd.mean(x, weights = weights_nonrecip_fourth, na.rm = T)', 
            'wtd.quantile(x, weights = weights_nonrecip_fourth, probs = 0.25)', 
            'wtd.quantile(x, weights = weights_nonrecip_fourth, probs = 0.5)',
            'wtd.quantile(x, weights = weights_nonrecip_fourth, probs = 0.75)',
            'notNA(x)'),
   summ.names = c("Min", "Max", "Mean", "0.25", "0.5", "0.75", "N"),
   title = "Fourth Income Quartile",
   labels = labs)

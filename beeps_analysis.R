
#BEEPS IV-V PANEL DATA ANALYSIS
#Author: Ezgi Ozcelik
#Date: 09/21/2025

# SETUP----

## opt out of scientific notation----
options(scipen = 999)

## set working directory----
setwd("/Users/ezgiozcelik/Desktop/businessgovernment")

## upload libraries----
library(tidyverse)
library(janitor)
library(countrycode) 
library(lme4)
library(texreg)

## add file paths----
PATH_BEEPS <- "beeps_iv_v_panel.csv"
PATH_FX    <- "worldbank_exchangerate.csv"
PATH_VDEM  <- "vdem/vdem.csv"

# DATA PREPARATION----

## load data----
beeps <- read.csv(PATH_BEEPS)

## rename relevant variables----
beeps_clean <- beeps %>%
  rename(rd_binary = ecaoo3, #binary research and development spending
         rd = ecaoo4, #research and development value in LCU
         direct_expo = d3c, #percentage of direct exports
         indirect_expo = d3b, #percentage of indirect exports
         expo = ecad8a, #export value in LCU
         empl = l1, #number of employees
         sales = d2, #sales value in LCU
         fixed_mac = n7a, #value of machinery, vehicles, and equipment in LCU
         fixed_land = n7b, #value of land and buildings in LCU
         govt_loan = k9, #if 2, firms most recent loan is from either state-owned banks or government agency
         cont_binary = j6a, #government contract binary variable
         year = a14y,
         found_y = b5, #business founding year
         main_product = d1a2_new, #main product 4digitcode
         sector = a4b, #categorical industry variable
         last_fy = fy, #last fiscal year
         insp_cost = ecaw2, #cost of inspection in LCU
         insp_count = j4, #number of inspections in past year
         domestic_owned = b2a, #percentage of domestic ownership
         foreign_owned = b2b, #percentage of foreign ownership
         state_owned = b2c, #percentage of state ownership
         )


## select relevant variables----
beeps_clean <- beeps_clean %>%
  select(idstd, id, qnrno, id2005, id2007, es2007, id2009, 
         panel_var, a0, a1, country, a2x,
         year, last_fy, found_y, lcu,
         main_product, sector, 
         d1a1x,
         domestic_owned, foreign_owned, state_owned,
         sales, empl, expo, direct_expo, indirect_expo, 
         rd_binary, rd, fixed_mac, fixed_land,
         insp_cost, insp_count, cont_binary,
         govt_loan)

## clean invalid survey codes---- 
#(-9 don't know, -8 refused, -7 not applicable)
beeps_clean <- beeps_clean %>%
  mutate(across(where(is.numeric), ~ ifelse(. %in% c(-9, -8, -7), NA, .)))

## recode variables----
# binary variables from 1 & 2 to 1 & 0
beeps_clean <- beeps_clean %>%
  mutate(
    rd_binary   = ifelse(rd_binary == 1, 1, ifelse(rd_binary == 2, 0, NA)),
    cont_binary = ifelse(cont_binary == 1, 1, ifelse(cont_binary == 2, 0, NA))
    )

# recode and create a binary export variable
beeps_clean <- beeps_clean %>%
  mutate(expo = ifelse(is.na(expo) & direct_expo == 0 & indirect_expo == 0, 0, expo))

beeps_clean$expo_binary <- ifelse(is.na(beeps_clean$expo), NA, 
                                  ifelse(beeps_clean$expo==0, 0, 1))

# recode categorical loan variable as government loan binary variable
# govt loan=1, private and other loans=0
beeps_clean <- beeps_clean %>%
  mutate(
    govt_loan = case_when(govt_loan == 2 ~ 1,
                          govt_loan %in% c(1,3,4) ~ 0,
                          TRUE ~ NA_real_)
  )

# EXCHANGE RATE FOR CURRENCY CONVERSION---- 

# add exchange rate information to later convert relevant variables from local currency unit to USD
# for this, use World Bank Exchange rates data
exchangerates <- read.csv(PATH_FX, header = TRUE)

## common country code---- 

#some country names in beeps and World Bank Exchange rates data do not match. 
# create a common country code variable for merge
beeps_clean <- beeps_clean %>%
  clean_names() %>%
  mutate(
    iso3c = countrycode(country, "country.name", "iso3c"))

exchangerates <- exchangerates %>%
  clean_names() %>%  
  mutate(
    iso3c = countrycode(country_name, "country.name", "iso3c")) %>%
  filter(!is.na(iso3c))

# select and revise years
exchangerates <- exchangerates %>%
  select(iso3c, x2006, x2007, x2008, x2009, x2010, x2011, x2012, x2013, x2014, x2016)

exchangerates <- exchangerates %>%
  rename_with(~ gsub("^x", "", .x), starts_with("x"))

exchangerates <- exchangerates %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"),   # all 4-digit year columns
    names_to = "year",
    values_to = "exchange_rate"
  ) %>%
  mutate(year = as.integer(year))

## merge beeps and exchange rate datasets----
# by iso3c (common country code) and year 
beeps_clean <- beeps_clean %>%
  left_join(exchangerates, by = c("iso3c", "year"))

## diagnostics---- 
# (any exchange rate NAs?)
beeps_clean %>%
  filter(is.na(exchange_rate)) %>%
  distinct(iso3c, country, year) %>%
  arrange(iso3c, year)

## add missing values manually---- 
# for Estonia, Slovakia, Slovenia. They switched from LCU to Euro around then
# Kosovo also defacto used Euro by then.
manual_rates <- tribble(
  ~country, ~year, ~exchange_rate,
  "Estonia", 2013, 1.33,
  "Slovak Rep.", 2009, 1.38,
  "Slovak Rep.", 2013, 1.33,
  "Slovak Rep.", 2014, 1.33,
  "Slovenia", 2008, 1.47,
  "Slovenia", 2009, 1.38,
  "Slovenia", 2013, 1.33,
  "Greece", 2016, 1.10,
  "Cyprus", 2016, 1.10,
  "Kosovo", 2008, 1.47,
  "Kosovo", 2009, 1.38,
  "Kosovo", 2013, 1.33,
  "Uzbekistan", 2008, 1314
)

beeps_clean <- beeps_clean %>%
  rows_update(manual_rates, by = c("country", "year"))

# VARIABLE CONSTRUCTION----

## fixed asset value----
# (inspired from Kerner and Sumner)
beeps_clean <- beeps_clean %>%
  mutate(fixed = fixed_mac + fixed_land)

# convert fixed asset value in LCU to USD and take a log
beeps_clean <- beeps_clean %>%
  mutate(fixed_usd = fixed / exchange_rate)

beeps_clean <- beeps_clean %>%
  mutate(
    fixed_log = log(fixed_usd + 1))

## sector----
# where manufacturing = 1 and service = 0
beeps_clean <- beeps_clean %>%
  mutate(
    industry = case_when(
      sector >= 15 & sector <= 37 ~ 1,
      sector %in% c(45, 50, 51, 52, 55, 60, 63, 64, 72) ~ 0,
      TRUE ~ NA_real_
    )
  )

## exports----
# convert export in LCU to USD and take a log
beeps_clean <- beeps_clean %>%
  mutate(exports_usd = expo / exchange_rate)

beeps_clean <- beeps_clean %>%
  mutate(
    expo_log = log(exports_usd + 1))

## research and development----
# impute rd = 0 for firms that did not do R&D (rd_binary == 0)
beeps_clean <- beeps_clean %>%
  mutate(rd = ifelse(is.na(rd) & rd_binary == 0, 0, rd))

#convert r&d in LCU to USD and take a log
beeps_clean <- beeps_clean %>%
  mutate(rd_usd = rd / exchange_rate)

beeps_clean <- beeps_clean %>%
  mutate(rd_log = log(rd_usd + 1))

## sales----
# convert sales in LCU to usd
beeps_clean <- beeps_clean %>%
  mutate(sales_usd = sales / exchange_rate)

# impute sales_usd NAs
beeps_clean <- beeps_clean %>%
  group_by(country, year, sector) %>%  
  mutate(
    sales_usd = if_else(
      is.na(sales_usd),
      median(sales_usd, na.rm = TRUE),  
      sales_usd
    )
  ) %>%
  ungroup()

# take log of sales_usd
beeps_clean <- beeps_clean %>%
  mutate(sales_log = log(sales_usd +1))

## number of employees----
#take a log of number of employees
beeps_clean <- beeps_clean %>%
  mutate(empl_log = log(empl + 1))

#impute missing r&d and export data. use mean values of that year and that country
beeps_clean <- beeps_clean %>%
  group_by(country, year) %>%
  mutate(
    expo_log = ifelse(is.na(expo_log), median(expo_log, na.rm = TRUE), expo_log)
  ) %>%
  ungroup()

beeps_clean <- beeps_clean %>%
  group_by(country) %>%
  mutate(
    rd_log = ifelse(is.na(rd_log), median(rd_log, na.rm = TRUE), rd_log)
  ) %>%
  ungroup()

## company age----
beeps_clean <- beeps_clean %>%
  mutate(age = year - found_y)

## Inspection Pressure Index----

# convert insp_cost in LCU to USD
beeps_clean <- beeps_clean %>%
  mutate(insp_cost_usd = insp_cost / exchange_rate)

# insp cost has too many 0s
# it is also highly right skewed
beeps_clean$insp_cost_t <- log1p(beeps_clean$insp_cost_usd)

# drop INSP COUNT outliers (3 outliers removed)
beeps_clean <- beeps_clean %>%
  dplyr::filter(insp_count < 500 | is.na(insp_count))

#scale insp_cost and insp_count variables
beeps_clean$insp_cost_z  <- scale(beeps_clean$insp_cost_t)
beeps_clean$insp_count_z <- scale(beeps_clean$insp_count)

beeps_clean$insp_index <- rowMeans(
  cbind(beeps_clean$insp_cost_z, beeps_clean$insp_count_z),
  na.rm = TRUE
)

beeps_clean$insp_freedom_index <- -1 * beeps_clean$insp_index

## regime type----
# Adding the regime type, and competitive authoritarian dimensions with V-Dem data
# to be used in marginal effects plots

vdem <- read.csv(PATH_VDEM)

vdem_ca <- vdem %>%
  filter(between(year, 2006, 2016)) %>%
  mutate(
    ca = as.integer(
      v2x_regime == 1 &
        !(v2expathhg %in% 0:5) &
        v2x_suffr >= 0.75 &
        !(v2elmulpar_ord %in% c(0, 1))
    ),
    regime_ca = case_when(
      ca == 1 ~ "Competitive Authoritarian",
      v2x_regime == 0 | (v2x_regime == 1 & ca == 0) ~ "Full Autocracy",
      v2x_regime %in% c(2, 3) ~ "Democracy"
    )
  ) %>%
  select(COWcode, year, ca, regime_ca, v2x_regime) %>%
  distinct()

vdem_ca <- vdem_ca %>%
  mutate(iso3c = countrycode(COWcode, origin = "cown", destination = "iso3c"))

#left join beeps_clean with vdem_ca data
beeps_merged <- beeps_clean %>%
  left_join(vdem_ca, by = c("iso3c", "year"))

beeps_merged <- beeps_merged %>%
  mutate(ca = ifelse(is.na(ca), 0, ca))

##scale the variables----
#(the scales of predictors are too different)
beeps_merged <- beeps_merged %>%
  group_by(country) %>%
  mutate(
    expo_w = expo_log - mean(expo_log, na.rm = TRUE),   
    rd_w   = rd_log   - mean(rd_log, na.rm = TRUE),
    empl_w = empl_log - mean(empl_log, na.rm = TRUE),
    sales_w = sales_log - mean(sales_log, na.rm=TRUE),
    age_w = age - mean(age, na.rm=TRUE),
    fixed_w = fixed_log - mean(fixed_log, na.rm=TRUE)
  ) %>% 
  ungroup() %>%
  mutate(
    expo_w_s  = scale(expo_w),
    rd_w_s    = scale(rd_w),
    empl_w_s  = scale(empl_w),
    sales_w_s = scale(sales_w),
    age_w_s = scale(age_w),
    fixed_w_s = scale(fixed_w)
  )

beeps_merged <- beeps_merged %>%
  mutate(scaled_state_owned = scale(state_owned))

# MODELS (hierarchical random effects)----

## Government Contract Models----
hre1 <- glmer(cont_binary ~ age_w_s + scaled_state_owned + industry
              + (1 | country),
              data=beeps_merged,
              family=binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 200000))
)


hre2 <- glmer(cont_binary ~ age_w_s + scaled_state_owned + industry + rd_binary + empl_w_s*industry + fixed_w_s*industry 
              + (1 | country),
              data=beeps_merged,
              family=binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 200000))
)


hre3 <- glmer(cont_binary ~ age_w_s + scaled_state_owned + industry + rd_binary*regime_ca + 
                empl_w_s*regime_ca + fixed_w_s*regime_ca 
                                  + (1 | country),
                                  data=beeps_merged,
                                  family=binomial,
                                  control = glmerControl(optimizer = "bobyqa",
                                                         optCtrl = list(maxfun = 200000))
)

texreg(
  list(hre1, hre2, hre3),
  include.se = TRUE,
  caption = "GLMM Results",
  booktabs = TRUE
)

## Government Loan Models----
hre21 <- glmer(govt_loan ~ age_w_s + scaled_state_owned + industry
               + (1 | country),
              data=beeps_merged,
              family=binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 200000))
)


hre22 <- glmer(govt_loan ~ age_w_s + scaled_state_owned + industry + rd_binary + expo_binary + empl_w_s*industry + fixed_w_s*industry
               + (1 | country),
              data=beeps_merged,
              family=binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 200000))
)

hre23 <- glmer(govt_loan ~ age_w_s + scaled_state_owned + industry + rd_binary*regime_ca + 
                 expo_binary*regime_ca + empl_w_s*regime_ca + fixed_w_s*regime_ca
               + (1 | country),
               data=beeps_merged,
               family=binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 200000))
)

texreg(
  list(hre21, hre22, hre23),
  include.se = TRUE,
  caption = "Government Loans Results",
  booktabs = TRUE
)

## Freedom from Inspection Index Models----
hre31 <- lmer(insp_freedom_index ~ age_w_s + scaled_state_owned + industry
              + (1 | country),
              data=beeps_merged)

hre32 <- lmer(insp_freedom_index ~ age_w_s + scaled_state_owned + industry + rd_binary + expo_binary + empl_w_s*industry + fixed_w_s*industry 
              + (1 | country),
              data=beeps_merged)

hre33 <- lmer(insp_freedom_index ~ age_w_s + scaled_state_owned + industry + rd_binary*regime_ca + 
                expo_binary*regime_ca + empl_w_s*regime_ca + fixed_w_s*regime_ca
              + (1 | country),
              data=beeps_merged)

texreg(
  list(hre31, hre32, hre33),
  include.se = TRUE,
  caption = "Inspection Pressure per year",
  booktabs = TRUE
)

## Combined Regression Table----

# create combined regression table of structural power key variable effects 
#without regime comparison
texreg(
  list(hre1, hre2, hre21, hre22, hre31, hre32),
  include.se = TRUE,
  caption = "Effects of Key Structural Power Variables on Business Influence Outcomes",
  booktabs = TRUE
)

#hre3, hre23, and hre33 models are used/visualized in marginal effects plots

# SESSION INFORMATION----
# for reproducibility
sessionInfo()


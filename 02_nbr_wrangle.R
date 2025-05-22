# NBR Borderlands Project: Roping and wrangling that ornery high plains data

# Created 2025-05-20
# Updated 2025-05-22
# Author: Bryan Burgess

# Header ------------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(janitor)

data_dir <- "/Users/bryan/Documents/nbr_borderlands/raw_data/"
datawrapper <- "/Users/bryan/Documents/nbr_borderlands/datawrapper_sets/"

options(scipen = 999)

# Country list ------------------------------------------------------------

country_list <- c("Afghanistan",
                  "Afghanistan, Islamic Rep. of",
                  "Bhutan",
                  "Brunei", 
                  "Brunei Darussalam",
                  "Democratic People's Republic of Korea",
                  "Korea, Dem. People's Rep. of",
                  "India", 
                  "Indonesia", 
                  "Japan", 
                  "Kazakhstan", 
                  "Kazakhstan, Rep. of",
                  "Kyrgyzstan", 
                  "Kyrgyz Republic",
                  "Kyrgyz Rep.",
                  "Laos", 
                  "Lao People's Democratic Republic",
                  "Lao People's Dem. Rep.",
                  "Malaysia",
                  "Mongolia",
                  "Myanmar",
                  "Nepal",
                  "North Korea", 
                  "Pakistan", 
                  "Philippines",
                  "Republic of Korea", 
                  "Korea, Rep. of",
                  "Russia", 
                  "Russian Federation",
                  "South Korea",
                  "Tajikistan",
                  "Tajikistan, Rep. of",
                  "Vietnam",
                  "Viet Nam"
)



# Read raw data ---------------------------------------------------------

setwd(data_dir)

centroids <- readRDS("borderlands_centroids.rds")
deflators <- read_csv("deflators.csv")

# Read TUFF and deflate to 2024
setwd("./AidDatas_Global_Chinese_Development_Finance_Dataset_Version_3_0")
tuff <- readxl::read_excel("AidDatasGlobalChineseDevelopmentFinanceDataset_v3.0.xlsx", 
                           sheet = "GCDF_3.0") |>
  janitor::clean_names() |>
  mutate(adjusted_amount_constant_usd_2024 = (adjusted_amount_constant_usd_2021 *100)/ 89.38908) |>
  select(-c("adjusted_amount_constant_usd_2021"))|> 
  filter(recipient %in% country_list)

# Public Diplomacy
setwd(data_dir)
setwd("./ChinesePublicDiplomacyExport")
pub_dip <- read_csv("ChinesePublicDiplomacy.csv") |>
  janitor::clean_names() |> 
  filter(receiving_country %in% country_list)
# Fixing non-numeric columns
pub_dip$year <- as.numeric(pub_dip$year)

cis_ccs <- read_csv("Confucius-Institutes.csv") |>
  janitor::clean_names() |> 
  filter(country %in% country_list)

setwd(data_dir)
# Sipri
sipri <- read_csv("sipri-trade-register.csv", skip = 11) |>
  janitor::clean_names() |> 
  filter(recipient %in% country_list)

# Military Engagement 
mil_dip <- openxlsx::read.xlsx("China Military Diplomacy DB 2002-2024 v498 02-5-2025 LSE.xlsx",
                               sheet = 2) |>
  janitor::clean_names() |> 
  filter(partner_country %in% country_list)

# Currency Swap
# NEED BASE DATA COLLECTION

# FDI
fdi <- read_csv("CDIS_04-09-2025 12-29-05-21_timeSeries.csv") |>
  filter(`Country Name` %in% c("China, P.R.: Hong Kong", 
                             "China, P.R.: Macao",
                             "China, P.R.: Mainland"))

# Special Economic Zones
# Projects located within SEZs
sez <- openxlsx::read.xlsx("Bdls_PRCSEZ_Clean.xlsx", sheet = 2) |>
  janitor::clean_names() %>%
  mutate(amount_constant_usd_2024 = (amount_constant_usd2017 * 100)/ 79.84376)

# United Front Presence
# NEED BASE DATA COLLECTION 

# Judicial Engagement
judicial <- openxlsx::read.xlsx("Bdls_SPCPRCJudicialDiplomacy.xlsx", 
                                sheet = 2) |>
  janitor::clean_names() 


# Security Indicators -----------------------------------------------------
## sec_01_arms_transfers -----------------------------------------------------
# From SIPRI Documentation:
# Transfers of major conventional arms from All countries  to All countries. Deals with deliveries made for the year range 'Not specified' to 'Not specified' 
# A '?' in a column indicates uncertain data. The 'Number delivered' and the 'Year(s) of deliveries' refer only to deliveries in the selected year(s).
# An empty field for 'Number ordered' indicates that data is not yet available.
# SIPRI trend-indicator values (TIVs) are in millions.
# An empty field for 'SIPRI TIV for total order' indicates that data (on the number ordered and/or the TIV per unit) is not available.
# A '0' for 'SIPRI TIV of delivered weapons' indicates that the volume of deliveries is between 0 and 0.5 million SIPRI TIV; and an empty field indicates that no deliveries have been identified.
# Figures may not add up to stated totals due to the conventions of rounding.
# For the method used for the SIPRI TIV and explanations of the conventions; abbreviations and acronyms see <https://www.sipri.org/databases/armstransfers/sources-and-methods>.

sec_01_arms_transfers <- sipri %>%
  filter(supplier == "China",
        year_of_order > 1999,
        recipient %in% country_list) %>%
  group_by(recipient, year_of_order) %>%
  summarise(sec_01_arms_transfer_tiv = sum(sipri_tiv_for_total_order, na.rm = T),
            sec_01_arms_transfer_tiv_delivered = sum(sipri_tiv_of_delivered_weapons, na.rm = T),
            sec_01_arms_transfer_orders_ct = n()) %>%
  mutate(sec_01_missing_transfer_tiv = sec_01_arms_transfer_tiv - sec_01_arms_transfer_tiv_delivered) %>%
  select(year = year_of_order,
         everything())

## sec_02_surveillance -----------------------------------------------------

surveillance <- tuff %>%
  filter(grepl("surveill*|safe city|scanner|police", tolower(title)) | 
           grepl("surveill*|safe city|scanner|police", tolower(description)))

table(surveillance$sector_name)
# 22 projects are health projects,       
surveillance <- surveillance %>%
  filter(recommended_for_aggregates == "Yes") %>%
  filter(((((sector_name == "GOVERNMENT AND CIVIL SOCIETY" | sector_name == "OTHER MULTISECTOR") | 
           sector_name == "TRADE POLICIES AND REGULATIONS") | sector_name == "TRANSPORT AND STORAGE") |
           aid_data_record_id == 96256) | aid_data_record_id == 63248)

sec_02_surveillance <- surveillance %>%
  group_by(commitment_year, recipient) %>%
  summarise(sec_02_surveillance_usd = sum(adjusted_amount_constant_usd_2024, na.rm = T),
            sec_02_surveillance_ct = n()) %>%
  select(year = commitment_year,
         recipient, 
         sec_02_surveillance_usd, 
         sec_02_surveillance_ct)

## sec_03_military_engagement -------------------------------------------------

sec_03_military_engagement <- mil_dip %>%
  filter(year >= 1999,
         activity_category == "Naval Port Call" |  activity_category == "Senior Level Visit") %>%
  group_by(year, partner_country) %>%
  summarise(sec_03_military_engagement_ct = n()) %>%
  select(year, 
         recipient = partner_country,
         sec_03_military_engagement_ct)

## sec_04_joint_exercise -----------------------------------------------------

sec_04_joint_exercise <- mil_dip %>%
  filter(year >= 1999,
         activity_category == "Military Exercise") %>%
  group_by(year, partner_country) %>%
  summarise(sec_04_joint_exercise_ct = n()) %>%
  select(year, 
         recipient = partner_country,
         sec_04_joint_exercise_ct)

# Development Indicators --------------------------------------------------
## dev_01_currency_swap -----------------------------------------------------
# From https://www.cfr.org/tracker/central-bank-currency-swaps-tracker#chapter-title-0-6

dev_01_currency_swap <- tribble(
  ~year, ~recipient, ~dev_01_currency_swap_line_extant, ~dev_01_currency_swap_usd,
  2010, "South Korea", "Y", 0,
  2010, "Indonesia", "Y", 0,
  2010, "Malaysia", "Y", 0,
  2011, "Kazakhstan", "Y", 0,
  2011, "Mongolia", "Y", 0,
  2011, "South Korea", "Y", 0, 
  2011, "Indonesia", "Y", 0,
  2011, "Malaysia", "Y", 0,
  2011, "Pakistan", "Y", 0,
  2012, "Mongolia", "Y", 330000000,
  2012, "South Korea", "Y", 0,
  2012, "Indonesia", "Y", 0, 
  2012, "Malaysia", "Y", 0,
  2012, "Pakistan", "Y", 0,
  2012, "Kazakhstan", "Y", 0, 
  2013, "Mongolia", "Y", 640000000,
  2013, "South Korea", "Y", 10000000,
  2013, "Indonesia", "Y", 0,
  2013, "Pakistan", "Y", 820000000,
  2013, "Kazakhstan", "Y", 0,
  2014, "Mongolia", "Y", 640000000,
  2014, "Russia", "Y", 1500000,
  2014, "South Korea", "Y", 10000000,
  2014, "Indonesia", "Y", 0,
  2014, "Malaysia", "Y", 0, 
  2014, "Pakistan", "Y", 820000000,
  2014, "Kazakhstan", "Y", 0, 
  2015, "Mongolia", "Y", 640000000,
  2015, "Russia", "Y", 130000000,
  2015, "South Korea", "Y", 10000000,
  2015, "Indonesia", "Y", 0,
  2015, "Malaysia", "Y", 0,
  2015, "Pakistan", "Y", 820000000,
  2015, "Tajikistan", "Y", 100000,
  2015, "Kazakhstan", "Y", 0,
  2016, "Mongolia", "Y", 640000000,
  2016, "Russia", "Y", 130000000,
  2016, "South Korea", "Y", 10000000,
  2016, "Indonesia", "Y", 0,
  2016, "Malaysia", "Y", 0,
  2016, "Pakistan", "Y", 820000000,
  2016, "Tajikistan", "Y", 100000, 
  2016, "Kazakhstan", "Y", 0,
  2017, "Mongolia", "Y", 640000000,
  2017, "Russia", "Y", 130000000,
  2017, "South Korea", "Y", 10000000,
  2017, "Indonesia", "Y", 0,
  2017, "Malaysia", "Y", 0,
  2017, "Pakistan", "Y", 820000000,
  2017, "Tajikistan", "Y", 100000, 
  2017, "Kazakhstan", "Y", 0,
  2018, "Mongolia", "Y", 640000000,
  2018, "Russia", "Y", 130000000,
  2018, "South Korea", "Y", 10000000,
  2018, "Japan", "Y", 0,
  2018, "Indonesia", "Y", 0,
  2018, "Malaysia", "Y", 0,
  2018, "Pakistan", "Y", 1500000000,
  2018, "Tajikistan", "Y", 100000,
  2018, "Kazakhstan", "Y", 0,
  2019, "Mongolia", "Y", 640000000,
  2019, "Russia", "Y", 130000000,
  2019, "Japan", "Y", 0,
  2019, "South Korea", "Y", 10000000,
  2019, "Indonesia", "Y", 0,
  2019, "Malaysia", "Y", 0,
  2019, "Pakistan", "Y", 1500000000,
  2019, "Kazakhstan", "Y", 0,
  2020, "Mongolia", "Y", 640000000,
  2020, "Russia", "Y", 130000000,
  2020, "South Korea", "Y", 10000000,
  2020, "Japan", "Y", 0,
  2020, "Indonesia", "Y", 0,
  2020, "Malaysia", "Y", 1100000000,
  2020, "Lao People's Democratic Republic", "Y", 300000000,
  2020, "Pakistan", "Y", 1500000000,
  2020, "Kazakhstan", "Y", 0,
  2021, "Mongolia", "Y", 640000000,
  2021, "Russia", "Y", 130000000,
  2021, "South Korea", "Y", 10000000,
  2021, "Japan", "Y", 0,
  2021, "Indonesia", "Y", 0,
  2021, "Malaysia", "Y", 1100000000,
  2021, "Lao People's Democratic Republic", "Y", 300000000,
  2021, "Pakistan", "Y", 1500000000,
  2021, "Kazakhstan", "Y", 0,
  2022, "Mongolia", "Y", 640000000,
  2022, "Russia", "Y", 130000000,
  2022, "South Korea", "Y", 10000000,
  2022, "Japan", "Y", 0,
  2022, "Indonesia", "Y", 0,
  2022, "Malaysia", "Y", 1100000000,
  2022, "Lao People's Democratic Republic", "Y", 300000000,
  2022, "Pakistan", "Y", 1500000000,
  2022, "Kazakhstan", "Y", 0,
  2023, "Mongolia", "Y", 640000000,
  2023, "Russia", "Y", 130000000,
  2023, "South Korea", "Y", 10000000,
  2023, "Japan", "Y", 0,
  2023, "Indonesia", "Y", 0,
  2023, "Malaysia", "Y", 1100000000,
  2023, "Lao People's Democratic Republic", "Y", 300000000,
  2023, "Pakistan", "Y", 1500000000,
  2023, "Kazakhstan", "Y", 0,
  2024, "Mongolia", "Y", 640000000,
  2024, "Russia", "Y", 130000000,
  2024, "South Korea", "Y", 10000000,
  2024, "Japan", "Y", 0,
  2024, "Indonesia", "Y", 0,
  2024, "Malaysia", "Y", 1100000000,
  2024, "Pakistan", "Y", 1500000000,
  2024, "Kazakhstan", "Y", 0
)

dev_01_currency_swap <- left_join(dev_01_currency_swap, deflators,
                                  by = "year") %>%
  mutate(dev_01_currency_swap_usd = (dev_01_currency_swap_usd * 100) / gdpdef) %>%
  select(-gdpdef)

## dev_02_infrastructure -----------------------------------------------------
dev_02_infrastructure <- tuff %>%
  filter(infrastructure == "Yes") %>%
  group_by(commitment_year, recipient) %>%
  summarise(dev_02_infrastructure_usd = sum(adjusted_amount_constant_usd_2024, na.rm = T),
            dev_02_infrastructure_ct = n()) %>%
  select(year = commitment_year,
         recipient,
         dev_02_infrastructure_usd,
         dev_02_infrastructure_ct)

## dev_03_fdi -----------------------------------------------------
# Mirror data should be used with caution as
# they have limitations. For example, some relevant counterparts of the compiling
# economy may not participate in the CDIS, or may not provide information because of
# confidentiality reasons or because the data fall below a reporting threshold.

# What does derived data mean?
#   Derived data for a given economy are data for this economy calculated based on mirror
# information. For a given economy A with inward investment from economy B, its
# derived inward data would be the outward investment reported by B in economy A.
# Similarly, for this given economy A with outward investment in economy B, its derived
# outward data would be the inward investment reported by B from economy A. The CDIS
# database contains indicators with derived data (inward direct investment positions,
# derived; inward equity positions, derived; etc.). Users can build their own queries with
# derived data (See Query tab).

fdi_cats <- fdi %>%
  select(-`...23`) %>%
  pivot_longer(cols = c(8:22), names_to = "year", values_to = "usd") %>%
  janitor::clean_names() %>%
  filter(country_name %in% c("China, P.R.: Hong Kong",
                             "China, P.R.: Macao",
                             "China, P.R.: Mainland"),
         counterpart_country_name %in% country_list)
table(fdi_cats$counterpart_country_name)
table(fdi_cats$indicator_name)

dev_03_fdi <- fdi %>%
  select(-`...23`) %>%
  pivot_longer(cols = c(8:22), names_to = "year", values_to = "usd") %>%
  janitor::clean_names() %>%
  filter(attribute == "Value",
         country_name %in% c("China, P.R.: Hong Kong",
                             "China, P.R.: Macao",
                             "China, P.R.: Mainland"),
         counterpart_country_name %in% country_list,
         indicator_name %in% c("Outward Direct Investment Positions, Derived, US Dollars",
                               "Outward Direct Investment Positions, US Dollars")) %>%
  pivot_wider(names_from = "indicator_name", values_from = "usd") %>%
  janitor::clean_names() %>%
  mutate(outward_direct_investment_positions_us_dollars = as.numeric(outward_direct_investment_positions_us_dollars),
         outward_direct_investment_positions_derived_us_dollars = as.numeric(outward_direct_investment_positions_derived_us_dollars))

dev_03_fdi <- dev_03_fdi %>%
  group_by(year, counterpart_country_name) %>%
  summarise(dev_03_fdi_usd = sum(outward_direct_investment_positions_us_dollars, na.rm = T),
            dev_03_fdi_derived_usd = sum(outward_direct_investment_positions_derived_us_dollars, na.rm = T))
dev_03_fdi$dev_03_fdi_usd[dev_03_fdi$dev_03_fdi_usd == 0] <- NA
dev_03_fdi$dev_03_fdi_derived_usd[dev_03_fdi$dev_03_fdi_derived_usd == 0] <- NA
sum(is.na(dev_03_fdi$dev_03_fdi_usd))
sum(is.na(dev_03_fdi$dev_03_fdi_derived_usd))

dev_03_fdi <- dev_03_fdi %>%
  left_join(deflators, by = c("year")) %>%
  mutate(dev_03_fdi_usd = (dev_03_fdi_usd * 100) / gdpdef, 
         dev_03_fdi_derived_usd = (dev_03_fdi_derived_usd * 100) / gdpdef)
  select(year, 
         recipient = counterpart_country_name, 
         dev_03_fdi_usd,
         dev_03_fdi_derived_usd) %>%
  mutate(year = as.numeric(year))

## dev_04_sez -----------------------------------------------------
# Need to do some creative calculating of unique SEZs here. 
# Confirm against IISS data: https://chinaconnects.iiss.org

sez_locations <- tribble(
  ~project_id, ~economic_zone,
  46420, "Bitung Special Economic Zone",
  61426, "Bitung Special Economic Zone",
  61427, "Sei Mangkei Special Economic Zone",
  66200, "Morowali Industrial Park",
  69125, "Sei Mangkei Special Economic Zone",
  73314, "China-Indonesia Economic and Trade Cooperation Zone",
  40299, "Park of Innovative Technologies Almaty",
  55029, "National Industrial Petrochemical Technopark Special Economic Zone",
  70646, "Chemical Park Taraz Special Economic Zone",
  54374, "Kara-Suu Industrial Zone",
  64770, "Saysettha Comprehensive Development Zone",
  64771, "Saysettha Comprehensive Development Zone",
  64777, "Savan-Seno Special Economic Zone",
  67552, "Saysettha Comprehensive Development Zone",
  67560, "Mohan-Boten Economic Zone",
  62477, "Perai Free Industrial Zone",
  62478, "Perai Free Industrial Zone",
  68942, "Zamyn-Uud Free Zone", 
  72020, "Khan-Uul District Development Zone",
  63820, "Thilawa Special Economic Zone",
  64310, "Thilawa Special Economic Zone",
  73299, "Thaton Township Industrial Zone",
  65016, "Rason Special Economic Zone",
  35306, "Duddar Export Processing Zone",
  53723, "Gwadar Port Free Zone and Export Processing Zone",
  63671, "Clark Freeport Zone",
  34478, "Ninh Phuc Industrial Zone",
  46222, "Zhongtai Dangara Agriculture and Textile Industrial Park",
  52904, "Port Qasim Industrial Park",
  62336, "Malaysia-China Kuantan Industrial Park",
  62338, "Malaysia-China Kuantan Industrial Park",
  63303, "My Phuoc Industrial Park",
  63674, "Long Giang Industrial Park",
  66207, "Morowali Industrial Park",
  66216, "Morowali Industrial Park",
  69488, "Morowali Industrial Park",
  85817, "Morowali Industrial Park", 
  86021, "My Phuoc Industrial Park", 
  86372, "Salambigar Industrial Park"
)

sez <- left_join(sez, sez_locations, by = c("project_id"))

sez_val_ct <- sez %>%
  group_by(commitment_year, recipient) %>%
  summarise(dev_04_sez_usd = sum(amount_constant_usd_2024),
            dev_04_sez_proj_ct = n())

sez_unique <- sez %>%
  group_by(commitment_year, recipient, economic_zone) %>%
  summarise(zone_ct = n()) %>%
  ungroup() %>%
  group_by(commitment_year, recipient) %>%
  summarise(dev_04_sez_unique_site = n())

dev_04_sez <- full_join(sez_val_ct, sez_unique,
                        by = c("commitment_year", "recipient")) %>%
  select(year = commitment_year,
         everything())

# Civilization Indicators -------------------------------------------------
## civ_01_united_front -----------------------------------------------------
# 2019 data derived from: https://jamestown.org/program/the-united-front-work-department-goes-global-the-worldwide-expansion-of-the-council-for-the-promotion-of-the-peaceful-reunification-of-china/

civ_01_united_front <- tribble(
  ~recipient, ~civ_01_united_front_presence, 
   "Afghanistan", "N", 
   "Bhutan", "N", 
   "Brunei Darussalam", "N", 
   "Democratic People's Republic of Korea", "N",
   "India", "N",
   "Indonesia", "Y", 
   "Japan", "Y", 
   "Kazakhstan", "N",
   "Kyrgyz Republic", "Y", 
   "Lao People's Democratic Republic", "Y", 
   "Malaysia", "Y", 
   "Mongolia", "N", 
   "Myanmar", "N", 
   "Nepal", "N",
   "Pakistan","N", 
   "Philippines", "Y", 
   "Russia", "Y", 
   "South Korea", "Y", 
   "Tajikistan", "N",
   "Viet Nam", "N"
)

## civ_02_healthcare -----------------------------------------------------
civ_02_healthcare <- tuff %>%
  filter(sector_name == "HEALTH") %>%
  group_by(commitment_year, recipient) %>%
  summarise(civ_02_healthcare_usd = sum(adjusted_amount_constant_usd_2024, na.rm = T),
            civ_02_healthcare_ct = n()) %>%
  select(year = commitment_year,
         everything())

## civ_03_hlpv -----------------------------------------------------
# outbound_political_visits The total number of visits by heads of state and heads of government from China to
# target countries
# inbound_political_visits The total number of visits by heads of state and heads of government from target
# countries to China.
pub_dip$outbound_political_visits <- as.numeric(pub_dip$outbound_political_visits)
pub_dip$inbound_political_visits <- as.numeric(pub_dip$inbound_political_visits)

civ_03_hlpv <- pub_dip %>%
  filter(receiving_country %in% country_list,
         !is.na(year)) %>%
  select(year, 
         recipient = receiving_country,
         civ_03_outbound_visits_ct = outbound_political_visits,
         civ_03_inbound_visits_ct = inbound_political_visits) %>%
  mutate(civ_03_total_visits_ct = civ_03_outbound_visits_ct + civ_03_inbound_visits_ct)

## civ_04_csps -----------------------------------------------------
# Number of new content sharing partnerships established in a country in a given year.
pub_dip$content_sharing_partnerships <- as.numeric(pub_dip$content_sharing_partnerships)

civ_04_csps <- pub_dip %>%
  filter(receiving_country %in% country_list,
         !is.na(year)) %>%
  select(year, 
         recipient = receiving_country,
         civ_04_csps_ct = content_sharing_partnerships)

## civ_05_judicial_engagement --------------------------------------
civ_05_judicial_engagement <- judicial %>%
  group_by(year, bdl_country) %>%
  summarise(civ_05_judicial_engagement_ct = n()) %>%
  select(year,
         recipient = bdl_country,
         civ_05_judicial_engagement_ct) 
  

## civ_06_ciccs -----------------------------------------------------
# Because CIs and CC data have closures, a bit more complex, confirm across data.
cis_open <- cis_ccs %>%
  filter(country %in% country_list) %>%
  group_by(country, year_opened) %>%
  summarise(ci_open = n())
cis_close <- cis_ccs %>%
  filter(country %in% country_list) %>%
  group_by(country, year_opened) %>%
  summarise(ci_close = n())
# Looks good, pub dip easier. 

civ_06_ciccs <- pub_dip %>%
  filter(receiving_country %in% country_list,
         !is.na(year)) %>%
  select(year, 
         recipient = receiving_country,
         civ_06_ci_ct = outbound_political_visits,
         civ_06_cc_ct = inbound_political_visits) %>%
  mutate(civ_06_ci_cc_ct = civ_06_ci_ct + civ_06_cc_ct) %>%
  select(year,
         everything())


# Fix countries -----------------------------------------------------------
# Standardize to TUFF
table(tuff$recipient)

table(sec_01_arms_transfers$recipient)
sec_01_arms_transfers$recipient[sec_01_arms_transfers$recipient == "Laos"] <- "Lao People's Democratic Republic"

# Tuff
table(sec_02_surveillance$recipient)

table(sec_03_military_engagement$recipient)
sec_03_military_engagement$recipient[sec_03_military_engagement$recipient == "Brunei"] <- "Brunei Darussalam"
sec_03_military_engagement$recipient[sec_03_military_engagement$recipient == "Kyrgyzstan"] <- "Kyrgyz Republic"
sec_03_military_engagement$recipient[sec_03_military_engagement$recipient == "Laos"] <- "Lao People's Democratic Republic"
sec_03_military_engagement$recipient[sec_03_military_engagement$recipient == "North Korea"] <- "Democratic People's Republic of Korea"
sec_03_military_engagement$recipient[sec_03_military_engagement$recipient == "Vietnam"] <- "Viet Nam"

table(sec_04_joint_exercise$recipient)
sec_04_joint_exercise$recipient[sec_04_joint_exercise$recipient == "Brunei"] <- "Brunei Darussalam"
sec_04_joint_exercise$recipient[sec_04_joint_exercise$recipient == "Kyrgyzstan"] <- "Kyrgyz Republic"
sec_04_joint_exercise$recipient[sec_04_joint_exercise$recipient == "Laos"] <- "Lao People's Democratic Republic"
sec_04_joint_exercise$recipient[sec_04_joint_exercise$recipient == "North Korea"] <- "Democratic People's Republic of Korea"
sec_04_joint_exercise$recipient[sec_04_joint_exercise$recipient == "Vietnam"] <- "Viet Nam"
# Tuff, it's fine
table(dev_02_infrastructure$recipient)

table(dev_03_fdi$recipient)
dev_03_fdi$recipient[dev_03_fdi$recipient == "Afghanistan, Islamic Rep. of"] <- "Afghanistan"
dev_03_fdi$recipient[dev_03_fdi$recipient == "Kazakhstan, Rep. of"] <- "Kazakhstan"
dev_03_fdi$recipient[dev_03_fdi$recipient == "Korea, Dem. People's Rep. of"] <- "Democratic People's Republic of Korea"
dev_03_fdi$recipient[dev_03_fdi$recipient == "Korea, Rep. of"] <- "South Korea"
dev_03_fdi$recipient[dev_03_fdi$recipient == "Kyrgyz Rep."] <- "Kyrgyz Republic"
dev_03_fdi$recipient[dev_03_fdi$recipient == "Lao People's Dem. Rep."] <- "Lao People's Democratic Republic"
dev_03_fdi$recipient[dev_03_fdi$recipient == "Russian Federation"] <- "Russia"
dev_03_fdi$recipient[dev_03_fdi$recipient == "Tajikistan, Rep. of"] <- "Tajikistan"
dev_03_fdi$recipient[dev_03_fdi$recipient == "Vietnam"] <- "Viet Nam"

table(dev_04_sez$recipient)
dev_04_sez$recipient[dev_04_sez$recipient == "Brunei"] <- "Brunei Darussalam"
dev_04_sez$recipient[dev_04_sez$recipient == "Kyrgyzstan"] <- "Kyrgyz Republic"
dev_04_sez$recipient[dev_04_sez$recipient == "Laos"] <- "Lao People's Democratic Republic"
dev_04_sez$recipient[dev_04_sez$recipient == "North Korea"] <- "Democratic People's Republic of Korea"
dev_04_sez$recipient[dev_04_sez$recipient == "Vietnam"] <- "Viet Nam"

# Tuff, it's fine
table(civ_02_healthcare$recipient)
table(civ_03_hlpv$recipient)
civ_03_hlpv$recipient[civ_03_hlpv$recipient == "Brunei"] <- "Brunei Darussalam"
civ_03_hlpv$recipient[civ_03_hlpv$recipient == "Kyrgyzstan"] <- "Kyrgyz Republic"
civ_03_hlpv$recipient[civ_03_hlpv$recipient == "Laos"] <- "Lao People's Democratic Republic"
civ_03_hlpv$recipient[civ_03_hlpv$recipient == "North Korea"] <- "Democratic People's Republic of Korea"
civ_03_hlpv$recipient[civ_03_hlpv$recipient == "Vietnam"] <- "Viet Nam"

table(civ_04_csps$recipient)
civ_04_csps$recipient[civ_04_csps$recipient == "Brunei"] <- "Brunei Darussalam"
civ_04_csps$recipient[civ_04_csps$recipient == "Kyrgyzstan"] <- "Kyrgyz Republic"
civ_04_csps$recipient[civ_04_csps$recipient == "Laos"] <- "Lao People's Democratic Republic"
civ_04_csps$recipient[civ_04_csps$recipient == "North Korea"] <- "Democratic People's Republic of Korea"
civ_04_csps$recipient[civ_04_csps$recipient == "Vietnam"] <- "Viet Nam"

table(civ_05_judicial_engagement$recipient)
civ_05_judicial_engagement$recipient[civ_05_judicial_engagement$recipient == "Brunei"] <- "Brunei Darussalam"
civ_05_judicial_engagement$recipient[civ_05_judicial_engagement$recipient == "India "] <- "India"
civ_05_judicial_engagement$recipient[civ_05_judicial_engagement$recipient == "Kyrgyzstan"] <- "Kyrgyz Republic"
civ_05_judicial_engagement$recipient[civ_05_judicial_engagement$recipient == "Kyrgyzstan "] <- "Kyrgyz Republic"
civ_05_judicial_engagement$recipient[civ_05_judicial_engagement$recipient == "Laos"] <- "Lao People's Democratic Republic"
civ_05_judicial_engagement$recipient[civ_05_judicial_engagement$recipient == "Myanmar "] <- "Myanmar"
civ_05_judicial_engagement$recipient[civ_05_judicial_engagement$recipient == "Pakistan "] <- "Pakistan"
civ_05_judicial_engagement$recipient[civ_05_judicial_engagement$recipient == "Russia "] <- "Russia"
civ_05_judicial_engagement$recipient[civ_05_judicial_engagement$recipient == "Taijikistan "] <- "Tajikistan"
civ_05_judicial_engagement$recipient[civ_05_judicial_engagement$recipient == "Tajikistan "] <- "Tajikistan"
civ_05_judicial_engagement$recipient[civ_05_judicial_engagement$recipient == "The Philippines"] <- "Philippines"
civ_05_judicial_engagement$recipient[civ_05_judicial_engagement$recipient == "The Phillippines "] <- "Philippines"
civ_05_judicial_engagement$recipient[civ_05_judicial_engagement$recipient == "The Phillipines "] <- "Philippines"
civ_05_judicial_engagement$recipient[civ_05_judicial_engagement$recipient == "Uzbekistan "] <- "Uzbekistan"
civ_05_judicial_engagement$recipient[civ_05_judicial_engagement$recipient == "Uzebekistan "] <- "Uzbekistan"
civ_05_judicial_engagement$recipient[civ_05_judicial_engagement$recipient == "Vietnam"] <- "Viet Nam"
civ_05_judicial_engagement$recipient[civ_05_judicial_engagement$recipient == "Vietnam "] <- "Viet Nam"

civ_05_judicial_engagement <- civ_05_judicial_engagement %>%
  filter(recipient != "Cambodia",
         recipient != "Singapore",
         recipient != "Thailand",
         recipient != "Uzbekistan")

table(civ_06_ciccs$recipient)
civ_06_ciccs$recipient[civ_06_ciccs$recipient == "Brunei"] <- "Brunei Darussalam"
civ_06_ciccs$recipient[civ_06_ciccs$recipient == "Kyrgyzstan"] <- "Kyrgyz Republic"
civ_06_ciccs$recipient[civ_06_ciccs$recipient == "Laos"] <- "Lao People's Democratic Republic"
civ_06_ciccs$recipient[civ_06_ciccs$recipient == "North Korea"] <- "Democratic People's Republic of Korea"
civ_06_ciccs$recipient[civ_06_ciccs$recipient == "Vietnam"] <- "Viet Nam"

# Join together -----------------------------------------------------------

dash_data <- full_join(sec_01_arms_transfers, sec_02_surveillance, 
                       by = c("year", "recipient")) %>%
  full_join(sec_03_military_engagement, 
            by = c("year", "recipient")) %>%
  full_join(sec_04_joint_exercise, 
            by = c("year", "recipient")) %>%
  full_join(dev_01_currency_swap, 
            by = c("year", "recipient")) %>%
  full_join(dev_02_infrastructure,
            by = c("year", "recipient")) %>%
  full_join(dev_03_fdi,
            by = c("year", "recipient")) %>%
  full_join(dev_04_sez,
            by = c("year", "recipient")) %>%
  full_join(civ_02_healthcare, 
            by = c("year", "recipient")) %>%
  full_join(civ_03_hlpv,
            by = c("year", "recipient")) %>%
  full_join(civ_04_csps, 
            by = c("year", "recipient")) %>%
  full_join(civ_05_judicial_engagement,
            by = c("year", "recipient")) %>%
  full_join(civ_06_ciccs,
            by = c("year", "recipient")) %>%
  full_join(civ_01_united_front, 
            by = c("recipient")) %>%
  left_join(centroids, by = c("recipient")) %>%
  select("year", 
         "recipient", 
         "sec_01_arms_transfer_tiv", 
         "sec_01_arms_transfer_tiv_delivered", 
         "sec_01_arms_transfer_orders_ct", 
         "sec_01_missing_transfer_tiv", 
         "sec_02_surveillance_usd", 
         "sec_02_surveillance_ct", 
         "sec_03_military_engagement_ct", 
         "sec_04_joint_exercise_ct", 
         "dev_01_currency_swap_line_extant", 
         "dev_01_currency_swap_usd", 
         "dev_02_infrastructure_usd", 
         "dev_02_infrastructure_ct", 
         "dev_03_fdi_usd", 
         "dev_03_fdi_derived_usd", 
         "dev_04_sez_usd", 
         "dev_04_sez_proj_ct", 
         "dev_04_sez_unique_site", 
         "civ_01_united_front_presence", 
         "civ_02_healthcare_usd", 
         "civ_02_healthcare_ct", 
         "civ_03_outbound_visits_ct", 
         "civ_03_inbound_visits_ct", 
         "civ_03_total_visits_ct", 
         "civ_04_csps_ct", 
         "civ_05_judicial_engagement_ct", 
         "civ_06_ci_ct", 
         "civ_06_cc_ct", 
         "civ_06_ci_cc_ct", 
         "lat", 
         "lon") %>%
  arrange(year, recipient)

table(dash_data$recipient)
dash_data$dev_01_currency_swap_line_extant[is.na(dash_data$dev_01_currency_swap_line_extant)] <- "N"

# Write data --------------------------------------------------------------
setwd(datawrapper)

# v0.1 2025-04-23 
# write_csv(dash_data, "borderlands_datawrapper_v0.1.csv")

# v0.2 2025-05-21
# write_csv(dash_data, "borderlands_datawrapper_v0.2.csv")

# v1.0 2025-05-21
# write_csv(dash_data, "borderlands_datawrapper_v1.0.csv")

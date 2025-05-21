# NBR Borderlands Project

# Created 2025-05-20
# Updated 2025-05-21
# Author: Bryan Burgess

# Reading and wrangling all the data. 

# Header ------------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(janitor)

data_dir <- "/Users/bryan/Documents/nbr_borderlands/raw_data/"
datawrapper <- "/Users/bryan/Documents/nbr_borderlands/datawrapper_sets/"

options(scipen = 999)

# Country list ------------------------------------------------------------

country_list <- c("Afghanistan",
                  "Bhutan",
                  "Brunei", 
                  "Brunei Darussalam",
                  "Democratic People's Republic of Korea",
                  "India", 
                  "Indonesia", 
                  "Japan", 
                  "Kazakhstan", 
                  "Kyrgyzstan", 
                  "Kyrgyz Republic",
                  "Laos", 
                  "Lao People's Democratic Republic",
                  "Malaysia",
                  "Mongolia",
                  "Myanmar",
                  "Nepal",
                  "North Korea", 
                  "Pakistan", 
                  "Philippines",
                  "Republic of Korea", 
                  "Russia", 
                  "South Korea",
                  "Tajikistan",
                  "Vietnam",
                  "Viet Nam"
)



# Read raw data ---------------------------------------------------------

setwd(data_dir)

centroids <- readRDS("borderlands_centroids.rds")

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
  janitor::clean_names()

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


## dev_04_sez -----------------------------------------------------
# NNeed to do some crative calculating of unique SEZs here. 
# Confirm against IISS data: https://chinaconnects.iiss.org

# Civilization Indicators -------------------------------------------------
## civ_01_united_front -----------------------------------------------------
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
  mutate(civ_06_ci_cc_ct = civ_06_ci_ct + civ_06_cc_ct)


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
  full_join(dev_02_infrastructure,
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
  left_join(centroids, by = c("recipient"))

table(dash_data$recipient)


# Write data --------------------------------------------------------------
setwd(datawrapper)

# v0.1 2025-04-23 
# write_csv(dash_data, "borderlands_datawrapper_v0.1.csv")

# v0.2 2025-05-21
write_csv(dash_data, "borderlands_datawrapper_v0.2.csv")


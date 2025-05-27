# NBR Borderlands Project: Writing a codebook for John

# Created 2025-05-22
# Updated 2025-05-27
# Author: Bryan Burgess

# Run right after 02_nbr_wrangle.R
# Header ------------------------------------------------------------------

library(tidyverse)
library(openxlsx)

data_dir <- "/Users/bryan/Documents/nbr_borderlands/raw_data/"
datawrapper <- "/Users/bryan/Documents/nbr_borderlands/datawrapper_sets/"


# Codebook ----------------------------------------------------------------

borderlands_codebook <- tribble(
  ~variable_name, ~human_friendly_name, ~definition, ~source, ~years_coverage, ~values_unit, ~stock_flow, ~include_in_dash_yn, ~calculation_method_notes,
  "year", "Year", "Year", "All data sources", "1999-2024", "Years", NA, "Y", NA,
  "recipient", "Recipient", "Recipient or partner country name. Standardized to GCDF 3.0 Country Names", "All data sources", "1999-2024", NA, NA, "Y", NA,
  "sec_01_arms_transfer_tiv", "Official Arms Transfers Committed", "Value of total arms transfers committed per year, in SIPRI trend-indicator values (TIVs).", "SIPRI", "1948-2024", "Sipri TIV Millions", "Flow", "N", NA,
  "sec_01_arms_transfer_tiv_delivered", "Official Arms Transfers Delivered", "Value of total arms transfers delivered per year, in SIPRI trend-indicator values (TIVs).", "SIPRI", "1948-2024", "Sipri TIV Millions", "Flow", "Y", NA,
  "sec_01_arms_transfer_orders_ct", "Official Arms Orders Count", "Count of total arms transfers committed per year, in SIPRI trend-indicator values (TIVs).", "SIPRI", "1948-2024", "Count", "Flow", "N", NA,
  "sec_01_missing_transfer_tiv", "Missing Official Arms Transfer Deliveries", "Value of total arms transfers that were not delivered per year, in SIPRI trend-indicator values (TIVs).", "SIPRI", "1948-2024", "Sipri TIV Millions", "Flow", "N", NA,
  "sec_02_surveillance_usd", "Surveillance Technology and Systems Project Value", "Value of PRC OF projects to surveillance systems, safe city projects, x-ray or port scanners, and police units, by year.", "GCDF 3.0", "2001-2021", "USD 2024 Constant", "Flow", "Y", "Text matching for 'surveill*|safe city|scanner|police', then filtering out health, ag, and social sector projects, restricitng outputs to Government and Civil Society, Trade, Transport and Storage, and Other Multisector.", 
  "sec_02_surveillance_ct", "Surveillance Technology and Systems Project Count", "Count of PRC OF projects to surveillance systems, safe city projects, x-ray or port scanners, and police units, by year.", "GCDF 3.0", "2001-2021", "Count", "Flow", "Y", "Text matching for 'surveill*|safe city|scanner|police', then filtering out health, ag, and social sector projects, restricitng outputs to Government and Civil Society, Trade, Transport and Storage, and Other Multisector.",
  "sec_03_military_engagement_ct", "Senior-level Military Engagements Count", "Count of meetings between PRC and the border country’s senior-level officials. Includes both Naval Port Calls and Senior Level Visits", "Center for the Study of Chinese Military Affairs, Chinese Military Diplomacy Database", "1999-2024", "Count", "Flow", "Y", NA,
  "sec_04_joint_exercise_ct" , "Joint Exercises Count", "Count of military exercices between PRC and the border country.", "Center for the Study of Chinese Military Affairs, Chinese Military Diplomacy Database", "1999-2024", "Count", "Flow", "Y", NA,
  "dev_01_currency_swap_line_extant", "Currency Swap Agreement In Place", "Existance of People's Bank of China currency swap lines per year.", "CFR", "2009-2024", "Y/N Binary", "Stock", "Y", NA,
  "dev_01_currency_swap_usd" , "Currency Swap Agreement Drawn Value", "Maximum amount drawn in USD equivalent from People's Bank of China currency swap lines.", "CFR", "2009-2024", "USD 2024 Constant", "Flow", "Y", "Deflated adjustment from CFR estimates.",
  "dev_02_infrastructure_usd", "Infrastructure Project Value", "Value of PRC OF projects to infrastructure by year.", "GCDF 3.0", "2001-2021", "USD 2024 Constant", "Flow", "Y", "Filter on GCDF 3.0 'Infrastructure' tag.", 
  "dev_02_infrastructure_ct" , "Infrastructure Project Count", "Count of PRC OF projects to infrastructure by year.", "GCDF 3.0", "2001-2021", "Count", "Flow", "Y", "Filter on GCDF 3.0 'Infrastructure' tag.",
  "dev_03_fdi_usd", "Chinese FDI Value", "Chinese Outward FDI Position", "IMF CDIS", "2009-2024", "USD 2024 Constant", "Stock", "Y", "Reported FDI from CDIS, less coverage but more reliable.",
  "dev_03_fdi_derived_usd", "Derived Chinese FDI Value", "Derived Chinese Outward FDI Position", "IMF CDIS", "2009-2024", "USD 2024 Constant", "Stock", "N", "Derived FDI from CDIS, less reliable but better coverage.",
  "dev_04_sez_usd", "Special Economic Zone Project Value", "Value of China OF Projects in SEZs or Industrial Zones.", "GCDF 3.0", "2012-2021", "USD 2024 Constant", "Flow", "Y", "Entries from the original dataset that represent a form of SEZ, EEZ, or industrial zone-related financing by China to its maritime of land border neighbors since 2012. The presence of 'SEZ,' 'EEZ,', 'zone,' and 'industrial zone' in the original dataset's 'description' variable was used to indicate whether the data point represented a relevant engagement. Data points filtered by this criteria were reviewed to confirm they accurately represented engagements during the time period of interest.",
  "dev_04_sez_proj_ct", "Special Economic Zone Project Count", "Count of China OF Projects in SEZs or Industrial Zones.", "GCDF 3.0", "2012-2021", "Count", "Flow", "Y", "Entries from the original dataset that represent a form of SEZ, EEZ, or industrial zone-related financing by China to its maritime of land border neighbors since 2012. The presence of 'SEZ,' 'EEZ,', 'zone,' and 'industrial zone' in the original dataset's 'description' variable was used to indicate whether the data point represented a relevant engagement. Data points filtered by this criteria were reviewed to confirm they accurately represented engagements during the time period of interest.",
  "dev_04_sez_unique_site", "Unique Special Economic Zone Touchpoints", "Number of unique SEZs or Industrial Zones engaged by China per year.", "GCDF 3.0", "2012-2021", "Count", "Flow", "N", "Entries from the original dataset that represent a form of SEZ, EEZ, or industrial zone-related financing by China to its maritime of land border neighbors since 2012. The presence of 'SEZ,' 'EEZ,', 'zone,' and 'industrial zone' in the original dataset's 'description' variable was used to indicate whether the data point represented a relevant engagement. Data points filtered by this criteria were reviewed to confirm they accurately represented engagements during the time period of interest. Reviewed for unique SEZ locations.",
  "civ_01_united_front_presence", "United Front Presence", "Presence of United Front chapter (Chapters of the Council for the Promotion of the Peaceful Reunification of China).", "Jamestown Foundation", "2019", "Binary Y/N", NA, "Y", "Indicator applied to all years, though data is only contemporary as of 2019.",
  "civ_02_healthcare_usd", "Healthcare Project Value", "Value of PRC OF projects to health sector by year.", "GCDF 3.0", "2001-2021", "USD 2024 Constant", "Flow", "Y", "Filter on GCDF 3.0 'HEALTH' sector tag.", 
  "civ_02_healthcare_ct", "Healthcare Project Count", "Count of PRC OF projects to health sector by year.", "GCDF 3.0", "2001-2021", "Count", "Flow", "Y", "Filter on GCDF 3.0 'HEALTH' sector tag.", 
  "civ_03_outbound_visits_ct", "Outbound High-level Political Visits Count", "The total number of visits by heads of state and heads of government from China to target countries.", "China’s Global Public Diplomacy Dashboard Dataset (Version 3.0)", "1999-2021", "Count", "Flow", "N", NA,
  "civ_03_inbound_visits_ct", "Inbound High-level Political Visits Count", "The total number of visits by heads of state and heads of government from target countries to China.", "China’s Global Public Diplomacy Dashboard Dataset (Version 3.0)", "1999-2021", "Count", "Flow", "N", NA,
  "civ_03_total_visits_ct", "High-level Political Visits Count", "Combined visits by heads of state and heads of government from target countries to China and from China to target countries.", "China’s Global Public Diplomacy Dashboard Dataset (Version 3.0)", "1999-2021", "Count", "Flow", "Y", NA,
  "civ_04_csps_ct", "Content Sharing Partnerships Count", "Number of new content sharing partnerships established in a country in a given year.", "China’s Global Public Diplomacy Dashboard Dataset (Version 3.0)", "1999-2021", "Count", "Flow", "Y", NA,
  "civ_05_judicial_engagement_ct", "Judicial Engagements Count", "Count of PRC-led judicial diplomacy engagements from 2014-2025 between the Supreme People's Court (SPC) and its foreign counterparts in China's 20 land and maritime border neighbors. The data was pulled from press releases on the websites of the SPC, the Shanghai Cooperation Organization (SCO), BRICS, and ASEAN. Where additional information was needed, we also consulted press releases on the webpages of the SPC's relevant foreign counterparts.", "NBR", "2014-2024", "Count", "Flow", "Y", NA,
  "civ_06_ci_ct", "Confucius Institutes Operating Count", "Cumulative number of Confucius Institutes operating within each receiving country per year. Confucius Institutes (CIs) are non-profit, but government-operated organizations with the mandate to promote Chinese language and culture. CIs are usually set up as a partnership with a local university in the receiving country (or secondary schools in the case of Confucius Classrooms) and have the additional objective to promote local cooperation with Chinese businesses.", "China’s Global Public Diplomacy Dashboard Dataset (Version 3.0)", "1999-2021", "Count", "Stock", "N", NA,
  "civ_06_cc_ct", "Confucius Classrooms Operating Count", "The total number of Confucius Classrooms (CCs) operating in a receiving country each year.", "China’s Global Public Diplomacy Dashboard Dataset (Version 3.0)", "1999-2021", "Count", "Stock", "N", NA,
  "civ_06_ci_cc_ct", "Confucius Institutes and Classrooms Operating Count", "Combined number of Confucius Institutes(CIs) and Confucius Classrooms (CCs) operating in a receiving country each year.", "China’s Global Public Diplomacy Dashboard Dataset (Version 3.0)", "1999-2021", "Count", "Stock", "Y", NA,
  "lat", "Latitude", "Country centroid latitude", "Open Street Map", "1999-2024", NA, NA, "Y", "Used label node from OSM.",
  "lon", "Longitiude", "Country centroid longitude", "Open Street Map", "1999-2024", NA, NA, "Y", "Used label node from OSM."
)

# v1.0 2025-05-27
write_csv(borderlands_codebook, "nbr_borderlands_codebook.csv")

v1_write <- list("source" = dash_data,
                 "codebook" = borderlands_codebook)

openxlsx::write.xlsx(v1_write, "borderlands_datawrapper_v1.0.xlsx")

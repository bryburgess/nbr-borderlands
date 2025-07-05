# NBR Borderlands Project: Making some quick centroids for the dashboard

# Created 2025-04-23
# Updated 2025-07-05
# Author: Bryan Burgess

# Header ------------------------------------------------------------------

library(tidyverse)

data_dir <- "/Users/bryan/Documents/nbr_borderlands/raw_data/"
datawrapper <- "/Users/bryan/Documents/nbr_borderlands/datawrapper_sets/"

setwd(data_dir)


# Country Centroids -------------------------------------------------------

centroids <- tribble(~recipient, ~lat, ~lon, 
  "Afghanistan", 33.7680065, 66.2385139,
  "Bhutan", 27.5495110, 90.5119273,
  "Brunei Darussalam", 4.4137155, 114.5653908,
  "India", 22.3511148, 78.6677428,
  "Indonesia", -2.4833826, 117.8902853,
  "Japan", 36.5748441, 139.2394179,
  "Kazakhstan", 48.1012954, 66.7780818,
  "Kyrgyz Republic", 41.5089324, 74.7240910,
  "Lao People's Democratic Republic", 20.0171109, 103.3782530,
  "Malaysia", 4.5693754, 102.2656823,
  "Mongolia", 46.8250388, 103.8499736,
  "Myanmar", 17.1750495, 95.9999652,
  "Nepal", 28.3780464, 83.9999901,
  "Democratic People's Republic of Korea", 40.3736611, 127.0870417, 
  "Pakistan", 30.3308401, 71.2474990,
  "Philippines", 14.5904492, 120.9803621,
  "Russia", 64.6863136, 97.7453061, 
  "South Korea", 36.6383920, 127.6961188,
  "Tajikistan", 38.6281733, 70.8156541,
  "Viet Nam", 15.9266657, 107.9650855,
  # Additional countries for Map Testing
  "Thailand", 14.8971921, 100.8327300,
  "Cambodia", 12.5433216, 104.8144914,
  "Bangladesh", 24.4769288, 90.2934413,
  "Singapore", 1.3571070, 103.8194992, 
  "Turkmenistan", 39.3763807, 59.3924609,
  "Uzbekistan", 41.3237300, 63.9528098,
  "Sri Lanka", 7.5554942, 80.7137847,
  "Maldives", 3.7203503, 73.2244152
)


# Save centroids ----------------------------------------------------------
write_rds(centroids, "borderlands_centroids.rds")
write_csv(centroids, "borderlands_centroids.csv")

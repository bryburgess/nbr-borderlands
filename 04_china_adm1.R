# NBR Borderlands Project: Making Domestic China Map

# Created 2025-07-14
# Updated 2025-07-14
# Author: Bryan Burgess

# Header ------------------------------------------------------------------
library(tidyverse)
library(openxlsx)
library(janitor)
library(rgeoboundaries)

data_dir <- "/Users/bryan/Documents/nbr_borderlands/raw_data/"
datawrapper <- "/Users/bryan/Documents/nbr_borderlands/datawrapper_sets/"

# Function ----------------------------------------------------------------

#### Custom function to save plots ####
# This function is also just syntactic sugar
# We need multiple versions of graphs
# print => pdf;
# twitter => png;
# html => svg
# Additionally, we *may* need different aspect ratios
# Full page fits on 8.5" x 11" w/ room for margins, figure title, notes
# Half page fits on 8.5" x 5.5 w/ room for margins, figure title, notes
# This function creates each of these varients ...
# and saves them in a Google Drive folder.


# INPUT PARAMETERS:
# "plt" is the ggplot that you want to save (i.e. the chart)
# plt_name is the base name of the file. Do NOT put a file extension
plt_to_drive <- function(plt, plt_name, svg=T, png=T, pdf=T, half_page=T, full_page=T) {
  
  half_page_pdf_name <- paste0(plt_name, "_hp.pdf")
  half_page_png_name <- paste0(plt_name, "_hp.png")
  half_page_svg_name <- paste0(plt_name, "_hp.svg")
  full_page_pdf_name <- paste0(plt_name, "_fp.pdf")
  full_page_png_name <- paste0(plt_name, "_fp.png")
  full_page_svg_name <- paste0(plt_name, "_fp.svg")
  
  # ggsave() creates files on the "local" drive
  # you can view these in the file explorer on the left
  if(half_page == T & pdf==T) {
    hp_pdf <- ggsave(half_page_pdf_name,
                     plot = plt,
                     width = 4,
                     height = 4.5,
                     dpi = 300)
  }
  
  if(half_page == T & png==T) {
    hp_png <- ggsave(half_page_png_name,
                     plot = plt,
                     width = 4,
                     height = 4.5,
                     dpi = 300)
  }
  
  
  if(half_page == T & svg==T) {
    hp_svg <- ggsave(half_page_svg_name,
                     plot = plt,
                     width = 4,
                     height = 4.5,
                     dpi = 300)
  }
  
  
  if(full_page == T & pdf==T) {
    fp_pdf <- ggsave(full_page_pdf_name,
                     plot = plt,
                     width = 7.5,
                     height = 10,
                     dpi = 300)
  }
  
  if(full_page == T & png==T) {
    fp_png <- ggsave(full_page_png_name,
                     plot = plt,
                     width = 7.5,
                     height = 10,
                     dpi = 300)
  }
  
  if(full_page == T & svg==T) {
    fp_svg <- ggsave(full_page_svg_name,
                     plot = plt,
                     width = 7.5,
                     height = 10,
                     dpi = 300)
  }
}


# Read in data ------------------------------------------------------------
# Per Nadège: With regards to the primary map covering the geographic extent of 
# the study, it occurred to me that we should not only include the countries 
# that share a land and/or maritime border with China, but also the Chinese 
# autonomous regions and provinces that are located on the Chinese side of the 
# border line. It’s precisely that belt, connecting the inside of China to the 
# outside world, that really is at the core of the project’s scope. 
# Provinces/Autonomous regions/Municipalities located along China’s land and 
# maritime borders include Liaoning, Jilin, Heilongjiang, Inner Mongolia, Gansu, 
# Xinjiang, Tibet, Yunnan, Guangxi, Guangdong, Fujian, Zhejiang, Jiangsu, 
# Shandong, Hebei and Tianjin.

china_boundaries <- rgeoboundaries::gb_adm1(country = "China")


china_boundaries <- china_boundaries %>%
  mutate(highlight = if_else(shapeName %in% c(
    "Fujian Province",
    "Gansu Province",
    "Guangxi Zhuang Autonomous Region",
    "Guangzhou Province",
    "Hebei Province",
    "Heilongjiang Province",
    "Inner Mongolia Autonomous Region",
    "Jiangsu Province",
    "Jilin Province",
    "Liaoning Province",
    "Shandong Province",
    "Tianjin Municipality",
    "Tibet Autonomous Region",
    "Xinjiang Uyghur Autonomous Region",
    "Yunnan Province",
    "Zhejiang Province"), "Yes", "No"))


# Map ---------------------------------------------------------------------
# Red: #84344e Gold: #cab64a Grey: #dddddd
(china_border_plot <- ggplot() +
    geom_sf(data = china_boundaries, 
            aes(fill =  highlight), 
            color = "#FFFFFF",
            lwd = 0,
            ) + 
    scale_fill_manual(values = c("#dddddd", "#84344e")) + 
    labs(x = "",
         y = "",
         fill = "Border Province") +
    theme(panel.grid.minor = element_line(), 
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.key = element_blank())
)

setwd("./china_border_provinces_map")
plt_to_drive(china_border_plot, "china_border_provinces_map", 
             svg = T, pdf = T, png = T, full_page = T, half_page = T)

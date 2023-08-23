
# SCRIPT INFORMATION 
# ------------------------------------------------
# script: screen_publications
# author: a schultze
# description: this program illustrates how we screened the pubs using metagear
# the screening itself was manual, and saved locally 
# the screened data are uploaded to Github for reference so screening decisions can be tracked
#
# ------------------------------------------------
# 0. HOUSEKEEPING --------------------------------

# load core packages
if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(tidyverse, janitor, readr)
p_load(pubmedR, metagear)

# homepath (assumes an R project in the appropriate parent folder)
path <- getwd() 
data_path <- paste0(path, "/data")

# 4. SCREEN ABSTRACTS  ----------------------------------------------------

# keywords to highlight for the screening to aid in screening of reviews or difficult articles
keywords <- c("code", "program", "package", "Github")
# note; update to name of each reviewer screening 
effort_file <- c("/effort_Anna.csv")

abstract_screener(file = paste0(data_path, effort_file), 
                  aReviewer = "Anna", 
                  highlightKeywords = keywords)

# note: this launches a graphical GUI which enables abstract screening 
# note: reason for exclusion not recorded, but outside of scope of our project 
# results are saved to a csv locally 


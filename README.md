# Sharing is caring? A literature review of programming code sharing in pharmacoepidemiological studies

This repo contains the code and data for our review of code sharing and other open science indicators in the journal pharmacoepidemiology and drug safety. 

The protocol can be found in the docs folder. 

## Navigation 

You can find the code used to identify publications, screen them, and clean them in the 'R' folder. The programs should be run in the following order: 

1. identify_publications.R
2. screen_publications.R
3. merge_publications.R
4. data_cleaning.R
5. preliminary_results_icpe.Rmd

Note that the screening is manual, and full-text extraction completed after screening resulting in a dataset (final_results.csv) which can be found in the data folder. The data_cleaning.R file completes basic cleaning of the variables which results in a **clean dataset** (clean_data.csv). 

# Sharing is caring? A literature review of programming code sharing in pharmacoepidemiological studies

This repo contains the code and data for our review of code sharing and other open science indicators in the journal pharmacoepidemiology and drug safety. 

The protocol can be found in the docs folder. The data and code which underlie our ICPE 2023 presentation can be found in the latest release: [![DOI](https://zenodo.org/badge/641333677.svg)](https://zenodo.org/badge/latestdoi/641333677)

## Screening 

You can find all code used in the 'R' folder. The following scripts were used in the screening stage: 

1. R/identify_publications.R
2. R/screen_publications.R

The package we used to allocate the screening (`{metagear}`) produces two csvs with allocated PMIDs beginning with the word 'effort'. Screening is manual, and we provide the finalised effort files for reference in the data folder. After screening was completed, we conducted manual data extraction as described in our protocol, with data entered into a google form. The raw data downloaded from the google form can be found in the file final_results.csv. 

## Analysis 
We then cleaned the data and generated the results using the following scripts: 

1. R/data_cleaning.R
2. preliminary_results_icpe.Rmd

The data_cleaning.R file completes basic cleaning of the variables which results in a clean dataset (clean_data.csv). 

## Re-using this data 
As this is a work in progress, we want to flag that we have not yet developed complete metadata describing the variables and note that this is currently **not a locked dataset**, as we are yet to complete final QC of the data (schedueled Sept 2023). Our intention is to make the cleaned dataset avaialble under an MIT license with associated metadata during Q3 2023. 

Issues and pull requests are welcome, or email us any questions or feedback! 

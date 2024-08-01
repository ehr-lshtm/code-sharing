
# SCRIPT INFORMATION 
# ------------------------------------------------
# script: data_cleaning.R 
# author: a schultze
# description: clean the data after full extraction 
# ------------------------------------------------
# 0. HOUSEKEEPING --------------------------------

# load core packages
if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(tidyverse, janitor, readr, lubridate, maps, visR)

# homepath (assumes an R project in the appropriate parent folder)
path <- getwd() 
data_path <- paste0(path, "/data")

# 1. READ IN DATA  --------------------------------------------------------
results_filename <- paste0(path, "/data/final_results.csv")
results <- read_csv(results_filename, show_col_types = F)

# 2. CLEAN VAR NAMES -----------------------------------------------------
results <- results %>% 
  clean_names() 

# shorten the variable names 
clean_results <- results %>% 
  rename(pmid = pmid_from_spreadsheet, 
         doi = doi_from_spreadsheet, 
         code_sharing_possible = could_the_article_share_programming_code, 
         article_type_pds = publication_type_pds,  
         article_type_aim = publication_aim, 
         simulation = is_the_analysis_based_on_simulated_data, 
         covid = is_this_covid_19_research_is_covid_19_or_sars_co_v_2_in_title_or_abstract, 
         funder = who_was_the_funder, 
         industry_funder = is_this_industry_funding, 
         database = which_database_s_were_used_leave_blank, 
         country = which_country_ies_does_the_database_cover_na_if_not_does_not_apply, 
         language = what_language_or_platform_was_used_for_the_analyses, 
         affiliation = author_affiliation_check_all_that_apply, 
         shared_code = is_code_supporting_the_analysis_or_method_linked_or_referenced_in_the_manuscript_or_in_supporting_information, 
         accessible_code = is_the_published_code_accessible_without_further_contacting_authors, 
         code_location = where_is_code_shared, 
         pipeline_coverage = what_parts_of_the_analysis_pipeline_are_covered_by_the_code, 
         instructions = does_the_code_include_instructions_for_how_to_run_it, 
         synthetic = does_the_article_contain_synthetic_data_to_enable_code_to_be_run_by_others, 
         does_it_run = if_there_is_synthetic_data_or_it_is_a_simulation_study_does_the_code_run, 
         license = does_the_code_have_a_license, 
         license_type = if_yes_what_is_the_license, 
         preprint = does_the_article_specify_preprinting, 
         data_sharing = are_underlying_data_shared, 
         codelist_sharing = are_codelists_shared, 
         preregister = did_the_article_specify_whether_the_research_was_pre_registered, 
         guideline = does_the_article_specify_following_a_certain_reporting_guideline, 
         guideline_type = if_yes_what_reporting_guidelines) 

# duplicates? 
check_dupes <- get_dupes(clean_results, pmid) 
# two duplicates, likely accidentally duplicated extraction 

# keep the first row of duplicates 
clean_results <- clean_results %>% 
  distinct(pmid, .keep_all = TRUE)

# 3. MERGE SCREENED WITH SOURCE FILE -------------------------------------------
# read in all source files 
source_filename <- paste0(path, "/data/all_pubs.csv")
source_pubs <- read_csv(source_filename, show_col_types = F)

source_pubs <- source_pubs %>% 
  select(AUTHORS, YEAR, TITLE, ABSTRACT, PMID) %>% 
  rename(pmid = PMID)

all_results <- source_pubs %>% 
  left_join(clean_results, by = "pmid", keep = TRUE) %>% 
  select(-pmid.y) %>% 
  rename(pmid = pmid.x)

# INCLUSION CRITERIA 
all_results <- all_results %>% 
  mutate(code_sharing_possible = case_when(
    str_detect(code_sharing_possible, "Yes") ~ "Yes", 
    TRUE ~ code_sharing_possible)) %>% 
  # create logical variables to check eligiblity 
  # based on two criteria, could code be shared and is the article type eligible 
  mutate(include_article_type = case_when(
    article_type_pds == "Commentaries" ~ FALSE, 
    article_type_pds == "Letters to the Editor" ~ FALSE, 
    TRUE ~ TRUE), 
    include_code_sharing = case_when(
      code_sharing_possible == "Yes" ~ TRUE, 
      TRUE ~ FALSE) 
  ) 

# FLOWCHART ---------------------------------------------------------------

# Need to merge in effort files
anna_screen <- paste0(path, "/data/effort_Anna.csv")
effort_anna <- read_csv(anna_screen, show_col_types = F)

john_screen <- paste0(path, "/data/effort_John.csv")
effort_john <- read_csv(john_screen, show_col_types = F)

effort <- rbind(effort_anna, effort_john)

effort <- effort %>% 
  select(PMID, INCLUDE, REVIEWERS) %>% 
  mutate(full_screen = case_when(
    INCLUDE == "NO" ~ FALSE, 
    TRUE ~ TRUE 
  )) %>% 
  rename(pmid = PMID)

flowchart <- all_results %>% 
  left_join(effort, by = "pmid", keep = TRUE) %>% 
  select(-pmid.y) %>% 
  rename(pmid = pmid.x)

attrition <- visR::get_attrition(flowchart,
                                 criteria_descriptions = c("1. Code sharing possible (abstract)", 
                                                           "2. Code sharing possible (full)"
                                 ),
                                 criteria_conditions   = c("flowchart$full_screen", 
                                                           "all_results$include_code_sharing & all_results$include_article_type"),
                                 subject_column_name   = "pmid")

attrition$Complement <- c("NA", "Abstract only, letter/commentary, or no data analysis", "Letter/commentary, or no data analysis")
attrition$Criteria[1] <- "Records identified through database search"
png("results/flowchart.png", width = 1500, height = 1000)
attrition %>%
  visR::visr("Criteria", "Remaining N", "Complement", font_size = 31)
dev.off()

# apply the criteria 
clean_results <- flowchart %>% 
  filter(!is.na(timestamp)) %>% 
  filter(include_code_sharing & include_article_type)

# 4. MISSINGNESS -------------------------------------------------------------

# drop variables that were abandoned during data extraction due to futility 
clean_results <- clean_results %>% 
  select(-funder, -database)

# investigate missingess 
sapply(clean_results, function(x) sum(is.na(x)))

# missingness v. low, data extraction errors 
# assume missing is No for interim results generation
# QC to be implemented post ICPE to manually supplement missing data 
# missningess slightly higher for 'country', because methodological studies/reviews 

clean_results <- clean_results %>% 
  mutate(article_type_pds = ifelse(is.na(article_type_pds), "Original Article", article_type_pds)) %>%
  mutate(simulation = ifelse(is.na(simulation), "No", simulation), 
         covid = ifelse(is.na(covid), "No", covid), 
         industry_funder = ifelse(is.na(industry_funder), "No", industry_funder), 
         language = ifelse(is.na(language), "No", language), 
         preprint = ifelse(is.na(preprint), "No", preprint), 
         data_sharing = ifelse(is.na(data_sharing), "No", data_sharing), 
         codelist_sharing = ifelse(is.na(codelist_sharing), "No", codelist_sharing)) 

# 5. VARIABLE RECORDING ---------------------------------------------------

# Article Characteristics 
clean_results <- clean_results %>% 
  mutate(timestamp = mdy_hms(timestamp), 
         article_type_pds_grouped = case_when(
           article_type_pds == "Original Article" ~ "Original Article", 
           article_type_pds == "Brief Report" ~ "Brief Report", 
           TRUE ~ "Review"
         )) %>% 
  # categorise articles as methodological vs not  
  mutate(article_aim_clean = case_when(
    article_type_aim ==  "Methodological" ~ "Methodological", 
    str_detect(article_type_aim, "Validation") ~ "Validation", 
    str_detect(article_type_aim, "Review") ~ "Review", 
    str_detect(article_type_aim, "Editorial") ~ "Other", 
    str_detect(article_type_aim, "Guidance") ~ "Methodological", 
    str_detect(article_type_aim, "Data") ~ "Other", 
    TRUE ~ "Applied")) %>% 
  # simulation study? 
  mutate(simulation = case_when(
    simulation == "Yes" ~ "Yes", 
    simulation == "Partially" ~ "Yes", 
    TRUE ~ "No")
  ) %>% 
  # covid study vs not 
  mutate(covid = case_when(
    covid == "Yes" ~ "Yes", 
    TRUE ~ "No")) %>% 
  # create a test variable which scrapes relevant terms and recode data entry errors
  mutate(covid_detect = case_when(
    str_detect(tolower(title), "covid") ~ "Yes", 
    str_detect(tolower(title), "sars-") ~ "Yes",  
    TRUE ~ "No"
  )) %>% 
  # one paper misclassified as not covid when it was covid, recode 
  mutate(covid = case_when(
    covid_detect == "Yes" ~"Yes", 
    TRUE ~ covid
  )) %>% 
  # categorize funding as industry funding 
  # this will ignore other categories/missing 
  mutate(reported_industry_funds = case_when(
    industry_funder == "Yes" ~ "Yes", 
    TRUE ~ "No"
  )) %>% 
  # affiliation should be industry vs not 
  mutate(reported_industry_affiliation = case_when(
    str_detect(affiliation, "Industry") ~ "Yes", 
    TRUE ~ "No")) %>% 
  mutate(reported_academic_affiliation = case_when(
    str_detect(affiliation, "Academic") ~ "Yes", 
    TRUE ~ "No")) %>% 
  mutate(reported_reggov_affiliation = case_when(
    str_detect(affiliation, "Regulatory") ~ "Yes", 
    str_detect(affiliation, "Government") ~ "Yes", 
    TRUE ~ "No")) 

# 60+ different answers to language, cannot display each one separately 
# have to do some kind of grouping to create the main options 

# first, string cleaning of options entered 
clean_results <- clean_results %>%
  mutate(language = str_replace(language,"A platform, specify in other, ", "")) %>% 
  mutate(language = str_replace(language,"SAS, SAS", "SAS")) %>% 
  mutate(language = case_when(
    str_detect(language, "CIDA") ~ "Sentinel tools", 
    str_detect(language, "Sentinel") ~ "Sentinel tools", 
    str_detect(language, "entinel") ~ "Sentinel tools", 
    str_detect(language, "Joinpoint") ~ "Joinpoint Regression", 
    str_detect(language, "Jointpoint") ~ "Joinpoint Regression", 
    str_detect(language, "OpenEpi") ~ "OpenEpi", 
    str_detect(language, "JMP") ~ "JMP", 
    str_detect(language, "\\bNo\\b") ~ "None/Not Specified", 
    str_detect(language, "Stats Direct") ~ "StatsDirect", 
    str_detect(language, "VigiRank") ~ "VigiRank", 
    str_detect(language, "GraphPad") ~ "Graphpad Prism", 
    TRUE ~ language)) %>% 
  mutate(language = str_replace(language,"A platform, specify in other, ", "")) %>%
  mutate(language = str_replace(language,"A platform, specify in other", "")) 

clean_results <- clean_results %>% 
  mutate(language_r = case_when(
    str_detect(language, '\\bR\\b') ~ "Yes", 
    TRUE ~ "No"), 
    language_sas = case_when(
      str_detect(language, '\\bSAS\\b') ~ "Yes", 
      TRUE ~ "No"), 
    language_stata = case_when(
      str_detect(language, '\\bStata\\b') ~ "Yes", 
      TRUE ~ "No"),
    language_spss = case_when(
      str_detect(language, '\\bSPSS\\b') ~ "Yes",
      TRUE ~ "No"),
    language_excel = case_when(
      str_detect(language, '\\bExcel\\b') ~ "Yes", 
      TRUE ~ "No"),
    language_python = case_when(
      str_detect(language, '\\bPython\\b') ~ "Yes", 
      TRUE ~ "No"),
    language_sentinel = case_when(
      str_detect(language, '\\bSentinel\\b') ~ "Yes", 
      TRUE ~ "No"), 
    language_none = case_when(
      str_detect(language, '\\bNone\\b') ~ "Yes", 
      TRUE ~ "No")) %>%
    mutate(language_none = ifelse(language_sas=="Yes", "No", language_none))
  

# countries
clean_results <- clean_results %>% 
  mutate(country = str_replace(country, "United States", "USA"), 
         country = str_replace(country, "nited States", "USA"), 
         country = str_replace(country, "\\bus\\b", "USA"), 
         country = str_replace(country, "\\bUS\\b", "USA"), 
         country = str_replace(country, "USAA", "USA"),
         country = str_replace(country, "Australian", "Australia"), 
         country = str_replace(country, "the Netherlands", "Netherlands"), 
         country = str_replace(country, "The Netherlands", "Netherlands"), 
         country = str_replace(country, "Malaysian", "Malaysia"), 
         country = str_replace(country, "Finalnd", "Finland"), 
         country = str_replace(country, "\\bustria\\b", "Austria"), 
         country = str_replace(country, "United Kingdom", "UK"), 
         country = str_replace(country, "the UK", "UK"), 
         country = str_replace(country, "European Union", "EU"), 
         country = str_replace(country, "EU5", "EU"), 
         country = str_replace(country, "EUPAS", "EU"), 
         country = str_replace(country, "Europe", "EU"), 
         country = str_replace(country, "EUan", "EU"), 
         country = str_replace(country, "Kosova", "Kosovo"), 
         country = str_replace(country, "Latin America", "South America"), 
         country = str_replace(country, "South America", "South America")) %>% 
  mutate(country = str_trim(country)) %>% 
  mutate(country = ifelse(country == "NA", NA, country))

# Shared code or not? (OUTCOME)
clean_results <- clean_results %>% 
  # code sharing
  mutate(shared_code = case_when(str_detect(shared_code, "Yes") ~ "Yes", TRUE ~ "No")) %>% 
  # code accessibility
  mutate(accessible_code = case_when(str_detect(accessible_code, "Yes") ~ "Yes", 
                                     TRUE ~ "No")) %>% 
  # combine these two into results variable 
  mutate(available_code = case_when(shared_code == "Yes" & accessible_code == "Yes" ~ TRUE, 
                                    TRUE ~ FALSE)) 
# other details and ORPs 
clean_results <- clean_results %>% 
  mutate(license = ifelse(str_detect(license, "Not possible"), NA, license)) %>% 
  mutate(preprint = ifelse(is.na(preprint), "No", preprint)) %>% 
  mutate(guideline = ifelse(str_detect(guideline, "Not applicable"), "No", guideline)) %>% 
  mutate(guideline = ifelse(str_detect(guideline_type, "ood") & !is.na(guideline_type), "No", guideline)) %>% 
  mutate(codelist_sharing = case_when(str_detect(codelist_sharing, "All") ~ "At least one", 
                                      TRUE ~ codelist_sharing)) 
# fix the demographics 
clean_results <- clean_results %>% 
  rename(year = YEAR) %>% 
  mutate(year = as.numeric(year))

# QC of code sharing  -----------------------------------------------------
# At peer review: requested additional QC of code sharing papers 
# we therefore manually QC'd all papers flagged as sharing code and corrected any potential errors

clean_results <- clean_results %>% 
  # did reference a specific and named code module, but the link is broken
  mutate(shared_code = case_when(doi == "10.1002/pds.5168" ~ "Yes", 
                                 TRUE ~ shared_code)) %>% 
  # also referenced a specific and named code module, but the link is broken
  mutate(shared_code = case_when(doi == "10.1002/pds.4375" ~ "Yes", 
                                 TRUE ~ shared_code)) %>% 
  # on review, reclassified two papers as "referencing open source code", rather than "code sharing" 
  mutate(shared_code = case_when(doi == "10.1002/pds.4392" ~ "No", 
                                 TRUE ~ shared_code), 
         accessible_code = case_when(doi == "10.1002/pds.4392" ~ "No", 
                                     TRUE ~ accessible_code),
         available_code = case_when(doi == "10.1002/pds.4392" ~ FALSE, 
                                    TRUE ~ available_code)) %>% 
  mutate(shared_code = case_when(doi == "10.1002/pds.4343" ~ "No", 
                                 TRUE ~ shared_code), 
         accessible_code = case_when(doi == "10.1002/pds.4343" ~ "No", 
                                     TRUE ~ accessible_code),
         available_code = case_when(doi == "10.1002/pds.4343" ~ FALSE, 
                                    TRUE ~ available_code))

# Save file
clean_filename <- paste0(path, "/data/clean_results.csv")
write_csv(clean_results, clean_filename)
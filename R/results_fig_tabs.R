# SCRIPT INFORMATION 
# ------------------------------------------------
# script: results_fig_tabs.R 
# author: a schultze/j tazare (adapted from 
#         preleminary_results_icpe.Rmd)
# description: prepare results figs/tabs 
# ------------------------------------------------
# 0. HOUSEKEEPING --------------------------------

# load core packages
if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(tidyverse, janitor, readr, lubridate, maps, visR, stringr, 
       gt, RColorBrewer, tableone, kableExtra, cowplot)

# homepath (assumes an R project in the appropriate parent folder)
path <- getwd() 
data_path <- paste0(path, "/data")

# create subdir for exporting tables, if it does not already exist
tab_path <- paste0(path, "/results/tabs")

if (!dir.exists(tab_path)){
  dir.create(tab_path)
} else {
  print("export directory already exists")
}

# 1. READ IN DATA  --------------------------------------------------------
results_filename <- paste0(path, "/data/clean_results.csv")
results <- read_csv(results_filename, show_col_types = F)

# 2. FIGURES ---------------------------------------

## Declared code sharing over time 
declared_sharing_over_time <- results %>% 
  group_by(year, shared_code) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(shared_code = ifelse(shared_code == "Yes", n, 0) ) %>% 
  group_by(year) %>% 
  summarise(total_pub = sum(n), code_share=sum(shared_code), perc = round(100*code_share/total_pub, 2)) %>% 
  ungroup()

# export as csv
write.csv(declared_sharing_over_time, file = "results/tabs/declared_code_sharing_over_time.csv")

ggplot(data = declared_sharing_over_time, aes(x=year, y=perc)) + 
  geom_line(color = "cornflowerblue") +
  geom_point(color = "cornflowerblue") +
  theme_classic() + 
  labs(x ="Publication Year") +
  labs(y= "Percentage of code sharing (%)") + 
  ylim(0,15)

ggsave("results/figs/declared_code_sharing_over_time.png", width = 15, height = 10, units = "cm")

## Actual code sharing over time 
sharing_over_time <- results %>% 
  group_by(year, available_code) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(available_code = ifelse(available_code == TRUE, n, 0) ) %>% 
  group_by(year) %>% 
  summarise(total_pub = sum(n), code_share=sum(available_code), perc = round(100*code_share/total_pub, 2)) %>% 
  ungroup()

# export as csv
write.csv(sharing_over_time, file = "results/tabs/actual_code_sharing_over_time.csv")

ggplot(data = sharing_over_time, aes(x=year, y=perc)) + 
  geom_line(color = "cornflowerblue") +
  geom_point(color = "cornflowerblue") +
  theme_classic() + 
  labs(x ="Publication Year") +
  labs(y= "Percentage of code sharing with code accessible (%)") + 
  ylim(0,15)

ggsave("results/figs/actual_code_sharing_over_time.png", width = 15, height = 10, units = "cm")

## Open science over time 
other_orps <- results %>% 
  select(preregister,data_sharing, codelist_sharing, guideline, year, shared_code) %>%
  mutate(data_sharing = case_when(
    str_detect(data_sharing, 'access') ~ "Data access procedures described or available on request", 
    str_detect(data_sharing, 'Yes') ~ "Yes", 
    TRUE ~ "No")) %>% 
  mutate(protocol_sharing = case_when(
    str_detect(preregister, 'Yes') ~ "Yes", 
    TRUE ~ preregister)) %>% 
  select(-preregister) 

# data
data_sharing_over_time <- other_orps %>% 
  group_by(year, data_sharing) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(shared_data = ifelse(data_sharing == "Yes" | data_sharing == "Data access procedures described or available on request", n, 0) ) %>% 
  group_by(year) %>% 
  summarise(total_pub = sum(n), data_share=sum(shared_data), perc = round(100*data_share/total_pub, 2)) %>% 
  mutate(label = "Data sharing") %>% 
  select(year, label, perc)

# codelist
codelist_sharing_over_time <- other_orps %>% 
  group_by(year, codelist_sharing) %>% 
  count() %>% 
  ungroup() %>% 
  filter(codelist_sharing != "Not applicable") %>% # i.e. restrict to articles where codelist sharing was applicable
  mutate(shared_codelist = ifelse(codelist_sharing == "At least one", n, 0) ) %>% 
  group_by(year) %>% 
  summarise(total_pub = sum(n), codelist_share=sum(shared_codelist), perc = round(100*codelist_share/total_pub, 2)) %>% 
  mutate(label = "Codelist sharing") %>% 
  select(year, label, perc)

# guideline
guideline_sharing_over_time <- other_orps %>% 
  group_by(year, guideline) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(rep_guideline = ifelse(guideline == "Yes", n, 0) ) %>% 
  group_by(year) %>% 
  summarise(total_pub = sum(n), guideline_rep=sum(rep_guideline), perc = round(100*guideline_rep/total_pub, 2)) %>% 
  mutate(label = "Guideline reported") %>% 
  select(year, label, perc)

# protocol_sharing
protocol_sharing_sharing_over_time <- other_orps %>% 
  group_by(year, protocol_sharing) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(rep_protocol_sharing = ifelse(protocol_sharing == "Yes", n, 0) ) %>% 
  group_by(year) %>% 
  summarise(total_pub = sum(n), protocol_sharing_rep=sum(rep_protocol_sharing), perc = round(100*protocol_sharing_rep/total_pub, 2)) %>% 
  mutate(label = "Preregistration") %>% 
  select(year, label, perc)

orp_over_time <- rbind(data_sharing_over_time, codelist_sharing_over_time, guideline_sharing_over_time, protocol_sharing_sharing_over_time) %>%
  select(year, label, perc)

# plot as a bar chart 
share1 <- orp_over_time %>% 
  filter(label == "Data sharing") %>%
  ggplot(aes(x=year, y=perc)) +
  geom_line(color = "brown1") +
  geom_point(color = "brown1") +
  theme_classic() + 
  labs(y = "Percentage of Data Sharing (%)", 
       x = "Publication Year") +
  scale_y_continuous(limits=c(0,20), breaks=seq(0,20,5))

share2 <- orp_over_time %>% 
  filter(label == "Codelist sharing") %>%
  ggplot(aes(x=year, y=perc)) +
  geom_line(color = "darkorange1") +
  geom_point(color = "darkorange1") +
  theme_classic() + 
  labs(y = "Percentage of Codelist Sharing (%)", 
       x = "Publication Year") +
  scale_y_continuous(limits=c(0,80),breaks=seq(0,80,20)) 

share3 <- orp_over_time %>% 
  filter(label == "Preregistration") %>%
  ggplot(aes(x=year, y=perc)) +
  geom_line(color = "darkolivegreen3") +
  geom_point(color = "darkolivegreen3") +
  theme_classic() +  
  labs(y = "Percentage of Preregistration (%)", 
       x = "Publication Year") +
  scale_y_continuous(limits=c(0,10),breaks=seq(0,10,2)) 

share4 <- orp_over_time %>% 
  filter(label == "Guideline reported") %>%
  ggplot(aes(x=year, y=perc)) +
  geom_line(color = "cornflowerblue") +
  geom_point(color = "cornflowerblue") +
  theme_classic() + 
  labs(y = "Percentage of Guideline reporting (%)", 
       x = "Publication Year") +
  scale_y_continuous(limits=c(0,10),breaks=seq(0,10,2)) 

plot_grid(share1, share2, share3, share4, labels = c('A', 'B', 'C', 'D'), label_size = 12)

ggsave("results/figs/orps_over_time.png", width = 20, height = 20, units = "cm")

## Code sharing location
# create separate data set of code location as that could be more than one (for plotting)
code_location <- results %>% 
  filter(available_code) %>% 
  select(code_location) %>% 
  separate_rows(code_location, sep = ",") %>% 
  mutate(code_location = str_trim(code_location)) %>% 
  mutate(code_location = case_when(str_detect(code_location, "entinel") ~ "Sentinel", 
                                   str_detect(code_location, "Personal") ~ "Personal Webpage", 
                                   str_detect(code_location, "Pseudocode") ~ "Supplementary materials (doc or pdf)",
                                   str_detect(code_location,"equest") ~ 	"Available on request", 
                                   str_detect(code_location,"dataverse") ~ "Harvard Dataverse",  
                                   TRUE ~ code_location)) %>% 
  mutate(code_location = case_when(str_detect(code_location, "doc") ~ "Supplementary doc or pdf", 
                                   str_detect(code_location, "program") ~ "Supplementary programming files",
                                   TRUE ~ code_location)) %>% 
  group_by(code_location) %>% 
  tally() %>% 
  arrange((n))

# set the order of the factors to be the count 
code_location$code_location <- factor(code_location$code_location, levels=code_location$code_location)  

# plot as a bar chart 
ggplot(code_location, aes(x=code_location, y=n, fill=code_location)) + 
  geom_col() +
  scale_fill_brewer(palette = "PuBuGn", direction = 1) + 
  theme_classic() + 
  labs(y = "Count", 
       x = "") + 
  theme(legend.position="none") + 
  coord_flip()

ggsave("results/figs/code_sharing_location.png", width = 15, height = 10, units = "cm")

## Reporting guidelines
guidelines <- results %>% 
  filter(guideline == "Yes") %>% 
  select(guideline_type) %>% 
  separate_rows(guideline_type, sep = ",") %>% 
  mutate(guideline_type = str_trim(guideline_type)) %>% 
  # cochrane handbook not a guideline, recode 
  mutate(is_guideline = case_when(str_detect(guideline_type, '\\bCochrane\\b') ~ "No", 
                                  TRUE ~ "Yes")) %>% 
  filter(is_guideline == "Yes") %>% 
  group_by(guideline_type) %>% 
  tally() %>% 
  arrange((n))

# set the order of the factors to be the count 
guidelines$guideline_type <- factor(guidelines$guideline_type, levels=guidelines$guideline_type)  

# plot as a bar chart 
ggplot(guidelines, aes(x=guideline_type, y=n, fill=guideline_type)) + 
  geom_col() +
  scale_fill_brewer(palette = "PRGn", direction = 1) + 
  theme_classic() + 
  labs(y = "Count", 
       x = "") + 
  theme(legend.position="none") + 
  coord_flip()

ggsave("results/figs/reporting_guideline.png", width = 15, height = 10, units = "cm")

# 3. TABLES ---------------------------------------

## Characteristics of included papers
part1 <- results %>% 
  mutate(any_language = ifelse(language_none == "Yes", "No", "Yes")) %>% 
  mutate(language_spss_excel = case_when(language_spss == "Yes" ~ "Yes", 
                                         language_excel == "Yes" ~ "Yes", 
                                         TRUE ~ "No")) %>% 
  select(year, article_type_pds_grouped, article_aim_clean, simulation, covid, reported_industry_affiliation, reported_industry_funds, 
         any_language, language_sas, language_r, language_stata, language_spss_excel) %>%
  mutate(year = as.factor(year)) %>% 
  rename(
    Year = year,
    "Publication type (PDS)" = article_type_pds_grouped,
    "Article type" = article_aim_clean,
    "Simulation study" = simulation, 
    "COVID-19 research" = covid,
    "Industry affiliation reported" = reported_industry_affiliation,
    "Industry funding reported" = reported_industry_funds, 
    "Reported programming language" = any_language, 
    "SAS" = language_sas, 
    "R" = language_r, 
    "Stata" = language_stata, 
    "SPSS/Excel" = language_spss_excel
  )

part2 <- part1 %>% select(-c("SAS","R","Stata","SPSS/Excel"))
tb <- CreateTableOne(data = part2)
# Prep for export
tab1a <- print(tb$CatTable)

# Export
write.csv(tab1a, file = "results/tabs/paper_characteristics_1a.csv")

# Among articles with langugae reported what was the language
part3 <- part1 %>% 
  filter(part1$`Reported programming language` == "Yes") %>% 
  select("SAS","R","Stata","SPSS/Excel") 

tb2<- CreateTableOne(data = part3)
# Prep for export
tab1b <- print(tb2$CatTable)

# Export 
write.csv(tab1b, file = "results/tabs/paper_characteristics_1b.csv")

## Code sharing characteristics
code_sharing_chars <- results %>% 
  select(article_aim_clean, simulation, covid, reported_industry_funds, available_code, language_r, language_stata, language_sas,available_code, pipeline_coverage, instructions, synthetic, license, language_r, language_sas, language_stata) %>% 
  filter(available_code) %>%
  # Pipeline coverage
  mutate(pipeline_stat = case_when(
    str_detect(pipeline_coverage, 'analysis') ~ "Yes", 
    TRUE ~ "No"),
    pipeline_meth = case_when(
      str_detect(pipeline_coverage, 'package') ~ "Yes", 
      TRUE ~ "No"),
    pipeline_ill = case_when(
      str_detect(pipeline_coverage, 'Illustrative') ~ "Yes", 
      TRUE ~ "No"),
    pipeline_creat = case_when(
      str_detect(pipeline_coverage, 'Creation') ~ "Yes", 
      TRUE ~ "No"),
  ) %>% 
  mutate(synthetic = ifelse(synthetic == "Yes", "Yes", "No or not possible to assess")) %>% 
  select(-pipeline_coverage) %>% 
  rename("Shared code" = available_code,
         "Pipeline coverage: statistical analysis" = pipeline_stat,
         "Pipeline coverage: methods" = pipeline_meth, 
         "Pipeline coverage: illustrative" = pipeline_ill, 
         "Pipeline coverage: data management" = pipeline_creat, 
         "Was synthetic data provided? " = synthetic, 
         "Were there instructions for how to run the code?" = instructions, 
         "Was there a license?" = license, 
         "SAS" = language_sas, 
         "R" = language_r, 
         "Stata" = language_stata,
         "Article type" = article_aim_clean,
         "COVID-19 research" = covid,
         "Industry funding reported" = reported_industry_funds,
         "Simulation study" = simulation)

tb <-  CreateTableOne(data = code_sharing_chars)
tab2 <- print(tb$CatTable)

# Export
write.csv(tab2, file = "results/tabs/code_sharing_characteristics.csv")

## Other open research practices
orps <- other_orps %>% 
  select(protocol_sharing,data_sharing, codelist_sharing, guideline)

tb3 <-  CreateTableOne(data = orps)
k2 <- print(tb3$CatTable)

# Export
write.csv(k2, file = "results/tabs/orp_characteristics.csv")

## Other open research practices over time
# data
data_sharing_over_time <- other_orps %>% 
  group_by(year, data_sharing) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(shared_data = ifelse(data_sharing == "Yes" | data_sharing == "Data access procedures described or available on request", n, 0) ) %>% 
  group_by(year) %>% 
  summarise(total_pub = sum(n), data_share=sum(shared_data), perc = round(100*data_share/total_pub, 2)) %>% 
  mutate(label = "Data sharing") %>% 
  select(year, total_pub, label, perc, data_share) %>% 
  rename(freq = data_share)

# codelist
codelist_sharing_over_time <- other_orps %>% 
  group_by(year, codelist_sharing) %>% 
  count() %>% 
  ungroup() %>% 
  filter(codelist_sharing != "Not applicable") %>% # i.e. restrict to articles where codelist sharing was applicable
  mutate(shared_codelist = ifelse(codelist_sharing == "At least one", n, 0) ) %>% 
  group_by(year) %>% 
  summarise(total_pub = sum(n), codelist_share=sum(shared_codelist), perc = round(100*codelist_share/total_pub, 2)) %>% 
  mutate(label = "Codelist sharing") %>% 
  select(year, total_pub, label, perc, codelist_share) %>% 
  rename(freq = codelist_share)

# guideline
guideline_sharing_over_time <- other_orps %>% 
  group_by(year, guideline) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(rep_guideline = ifelse(guideline == "Yes", n, 0) ) %>% 
  group_by(year) %>% 
  summarise(total_pub = sum(n), guideline_rep=sum(rep_guideline), perc = round(100*guideline_rep/total_pub, 2)) %>% 
  mutate(label = "Guideline reported") %>% 
  select(year, total_pub, label, perc, guideline_rep) %>% 
  rename(freq = guideline_rep)

# protocol_sharing
protocol_sharing_sharing_over_time <- other_orps %>% 
  group_by(year, protocol_sharing) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(rep_protocol_sharing = ifelse(protocol_sharing == "Yes", n, 0) ) %>% 
  group_by(year) %>% 
  summarise(total_pub = sum(n), protocol_sharing_rep=sum(rep_protocol_sharing), perc = round(100*protocol_sharing_rep/total_pub, 2)) %>% 
  mutate(label = "Preregistration") %>% 
  select(year, total_pub, label, perc,  protocol_sharing_rep) %>% 
  rename(freq =  protocol_sharing_rep)

orp_over_time <- rbind(data_sharing_over_time, codelist_sharing_over_time, guideline_sharing_over_time, protocol_sharing_sharing_over_time)  %>%
  select(label, year, total_pub, freq, perc)

write.csv(orp_over_time, file = "results/tabs/orp_characteristics_over_time.csv")

## Code sharing by other open research practices

code_shar_orp <- results %>% 
  select(available_code,preregister,data_sharing, codelist_sharing, guideline, year, shared_code) %>%
  mutate("Code Shared" = ifelse(available_code ==TRUE, "Yes", "No")) %>% 
  mutate(data_sharing = case_when(
    str_detect(data_sharing, 'access') ~ "Data access procedures described or available on request", 
    str_detect(data_sharing, 'Yes') ~ "Yes", 
    TRUE ~ "No")) %>% 
  mutate(protocol_sharing = case_when(
    str_detect(preregister, 'Yes') ~ "Yes", 
    TRUE ~ preregister)) %>% 
  select(-preregister) 

tb5 <- CreateTableOne(data = code_shar_orp, strata = "Code Shared",test = FALSE)
k4 <- print(tb5$CatTable)

write.csv(k4, file = "results/tabs/code_sharing_by_orp.csv")

# 4. DESCRIPTIVES ---------------------------------------
code_shared <- nrow(results %>% filter(available_code))

# Code location
code_location_desc <- results %>% 
  filter(available_code) %>% 
  select(code_location) %>% 
  separate_rows(code_location, sep = ",") %>% 
  mutate(code_location = str_trim(code_location)) %>% 
  mutate(code_location = case_when(str_detect(code_location, "entinel") ~ "Sentinel", 
                                   str_detect(code_location, "Personal") ~ "Personal Webpage", 
                                   str_detect(code_location, "Pseudocode") ~ "Supplementary materials (doc or pdf)",
                                   str_detect(code_location,"equest") ~ 	"Available on request", 
                                   str_detect(code_location,"dataverse") ~ "Available on request",  
                                   TRUE ~ code_location)) %>% 
  mutate(code_location = case_when(str_detect(code_location, "doc") ~ "Supplementary doc or pdf", 
                                   str_detect(code_location, "program") ~ "Supplementary programming files",
                                   TRUE ~ code_location)) %>% 
  group_by(code_location) %>% 
  count() %>% 
  mutate(perc = 100*n/code_shared)

# Licenses

license <- results %>% 
  filter(available_code) %>% 
  filter(license == "Yes") %>% 
  select(license_type) %>%
  mutate(license_type = case_when(str_detect(license_type, "GP") | str_detect(license_type, "GLP v3") ~ "GPL v3.0 (https://www.gnu.org/licenses/gpl-3.0.txt)", 
                                  TRUE ~ license_type)) %>% 
  group_by(license_type) %>% 
  count()

# Table 3b descriptives
# article type
article_type_row <- results %>% 
  group_by( article_aim_clean, available_code) %>% 
  count() %>% 
  ungroup() %>%
  group_by(article_aim_clean) %>% 
  mutate(tot = sum(n), per = n/tot*100) %>% 
  filter(available_code) 
 
# simulation
simulation_row <- results %>% 
  group_by( simulation, available_code) %>% 
  count() %>% 
  ungroup() %>%
  group_by(simulation) %>% 
  mutate(tot = sum(n), per = n/tot*100) %>% 
  filter(available_code) 

# covid
covid_row <- results %>% 
  group_by( covid, available_code) %>% 
  count() %>% 
  ungroup() %>%
  group_by(covid) %>% 
  mutate(tot = sum(n), per = n/tot*100) %>% 
  filter(available_code) 

# reported_industry_funds
reported_industry_funds_row <- results %>% 
  group_by( reported_industry_funds, available_code) %>% 
  count() %>% 
  ungroup() %>%
  group_by(reported_industry_funds) %>% 
  mutate(tot = sum(n), per = n/tot*100) %>% 
  filter(available_code) 

#  language_stata
language_stata_row <- results %>% 
  group_by( language_stata, available_code) %>% 
  count() %>% 
  ungroup() %>%
  group_by( language_stata) %>% 
  mutate(tot = sum(n), per = n/tot*100) %>% 
  filter(available_code) 

# language_r
language_r_row <- results %>% 
  group_by(language_r, available_code) %>% 
  count() %>% 
  ungroup() %>%
  group_by(language_r) %>% 
  mutate(tot = sum(n), per = n/tot*100) %>% 
  filter(available_code) 


# language_sas
language_sas_row <- results %>% 
  group_by( language_sas, available_code) %>% 
  count() %>% 
  ungroup() %>%
  group_by(language_sas) %>% 
  mutate(tot = sum(n), per = n/tot*100) %>% 
  filter(available_code) 


# 5. POST PEER REVIEW: REQUESTED TABLES ----------------------------------------------

# each individual language listed 
language_all <- results %>% 
  select(language) %>%
  # put each instance into a separate row and count them
  separate_rows(language, sep = ",") %>% 
  separate_rows(language, sep = "\\band\\b") %>% 
  # remove leading blanks 
  mutate(language = trimws(language)) %>% 
  drop_na() 

  postreview_suptb <- CreateTableOne(data = language_all)
  # Prep for export
  postreview_suptb1 <- print(postreview_suptb$CatTable)

  # Export 
  write.csv(postreview_suptb1, file = "results/tabs/all_languages.csv")

  
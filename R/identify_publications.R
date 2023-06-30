# SCRIPT INFORMATION 
# ------------------------------------------------
# script: identify_publications.R
# author: a schultze
# description: download publications from pubmed, split between reviewers 
# outputs: csvs of results, one complete and one split between reviewers 
#
# ------------------------------------------------
# 0. HOUSEKEEPING ---------------------------------------------------------

# load core packages
if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(tidyverse, janitor, readr)
p_load(pubmedR, metagear)

# homepath (assumes an R project in the appropriate parent folder)
path <- getwd() 
data_path <- paste0(path, "/data")

# 1. FUNCTIONS ---------------------------------------------------------------

capitalizeAfterFullStop <- function(text) {
  
  # Convert the text to lowercase
  text <- tolower(text)
  
  # Use regular expressions to find the first letter after a full stop and convert it to uppercase
  text <- gsub("(?<=\\.\\s|^)(\\w)", "\\U\\1", text, perl = TRUE)
  
  return(text)
  
}

# 2. DOWNLOAD DATA FROM PUBMED --------------------------------------------

# download relevant data from pubmed using pubmedr 
query <- '"Pharmacoepidemiology and drug safety"[Journal] AND (("2017/01/01"[Date - Publication] : "2022/12/31"[Date - Publication]))'
pubs_id <- pmApiRequest(query, limit=10000, api_key = NULL)
pubs_df <- pmApi2df(pubs_id, format = "bibliometrix")

# look at the two different years to check an appropriate one is used 
check_year <- pubs_df %>% 
  filter(PY != PY_IS) %>% 
  select(PMID, PY, PY_IS, DI)

# PY is the electronic publication, PY_IS is publication issue. Have assumed we want the issue. 

# check for duplicates; none found in terms of PMID 
pubs_df %>% get_dupes(PMID) 

# process the records into format needed for abstract screening 
# note; I've used the format of variable names that metagear uses 
all_pubs <- pubs_df %>% 
  mutate(AUTHORS = str_to_title(AU), 
         YEAR = PY_IS, 
         TITLE = capitalizeAfterFullStop(TI), 
         JOURNAL = str_to_title(SO), 
         DOI = DI, 
         ABSTRACT = capitalizeAfterFullStop(AB), 
         PMID = as.numeric(PMID)) %>% 
  select(AUTHORS, YEAR, TITLE, JOURNAL, DOI, ABSTRACT, PMID) 

print("number of records with downloaded data")
nrow(all_pubs)

# check that you've got the same number as a manual search 
# I downloaded this csv based on the same query but manually (note: does not have abstracts)
manual_filename <- paste0(path, "/data/csv-Pharmacoep-set.csv")
manual_pubs <- read_csv(manual_filename, show_col_types = F)

missed_pubs <- manual_pubs %>% 
  anti_join(all_pubs, by = "PMID")

print("number of publications without data downloaded")
nrow(missed_pubs)

# the only paper "missed" is a corrigendum 
# check if other corrigendums are downloaded 
pattern <- "^corrigendum"
corrigendums <- all_pubs[grepl(pattern, all_pubs$TITLE, ignore.case = TRUE),]

print("number of other corrigendums downloaded")
nrow(corrigendums)

# other corrigendums are downloaded 
# it seems that the only articles missed is the most recent one
# it's not a greater than or equal to issue, as the epub date is Dec 29. 
# weirdly the data from pubs_id specifies that 1190 records are downloaded 

# I checked the github https://github.com/massimoaria/pubmedR and I think other people also miss 1 paper
# I reproduced the error by changing the year to 2021 and confirming that the last article is missed again
# Suggest we raise an issue on the github but ignore for now as this one article wouldn't be eligible 
# Assuming we use pub issue date, rather than the epub date 

# exclude journal with an epub date after the date cut-off 
all_pubs <- all_pubs %>% 
  filter(YEAR %in% c("2017", "2018", "2019", "2020", "2021", "2022"))

print("Number of journal articles with issue publication year in our study timeframe")
nrow(all_pubs)

# save the file for record keeping 
filename <- paste0(path, "/data/all_pubs.csv")
write_csv(all_pubs, file = filename)

# 3. ASSIGN EFFORT  -------------------------------------------------------
# assign these articles randomly between the two of us using metagear 

# split the references between the two reviewers 
# this adds 3 columns needed to split the pubs out 
assigned_pubs <- effort_initialize(all_pubs)
names(assigned_pubs)

team <- c("Anna", "John")

assigned_pubs_split <- effort_distribute(assigned_pubs, 
                                         reviewers = team,
                                         save_split = TRUE, 
                                         directory = data_path)

# this automatically splits the pubs and outputs these into csvs in the data path 



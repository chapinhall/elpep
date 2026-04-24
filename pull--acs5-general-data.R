#--------------------------------------------------------------------#

# This code downloads ACS data from the Census Bureau, and constructs community-level
# variables for race and other characteristics. This code is adapted from code shared by
# Nick Mader ("develop-new-estimate-methods.Rmd").

# AUTHOR : Michele Carter and Nick Mader
# DATE CREATED : 2020-10-22

# This code requires an internet connection (and therefore may need to be run from
# personal computers, and not high security servers that are not connected to the internet)

#--------------------------------------------------------------------#

# Import functions

library(glue)
source(glue("{code_path}settings--main.R"))
source(glue("{code_path}method--read-customization-file.R"))
source(glue("{code_path}method--general-helper-functions.R"))

my_state_fip <- 
  fips_codes %>% 
  filter(state == my_state_abbr) %>% 
  pull(state_code) %>% 
  unique()

# Set local variable values

update_meta <- TRUE
update_acs_pulls <- TRUE

Sys.setenv(CENSUS_KEY = census_key)

# ------------------------------------------------------ #
# Pull ACS metadata ----
# ------------------------------------------------------ #

# ------------------------------------ #
# Identify tables of interest in the ACS
# ------------------------------------ #

# Identify tables/years of data that are available through Census API 

apis <- listCensusApis()
apis %>% filter(grepl("acs5", name)) %>% with(table(vintage))

# Identify tables of interest from ACS on Social Explorer (https://www.socialexplorer.com/data/ACS2019/metadata/?ds=ACS19) 

## Demographic Types

# [B01001A-I] Sex by Age
# [B09001] Population Under 18 Years of Age

## Predictors in Small Area Estimation Model

# [B03002] Hispanic or Latino Origin by Race
#	[B08006] Sex of Workers by Means of Transportation to Work
#	[B08013] Aggregate Travel Time to Work (in Minutes) of Workers by Sex
# [B08303] Travel Time to Work
#	[B10051] Grandparents Living with Own Grandchildren Under 18 Years by Responsibility for Own Grandchildren by 
#			 Presence of Parent of Grandchildren and Age of Grandparent
# [B10063] Households with Grandparents Living with Own Grandchildren Under 18 Years by Responsibility for Own 
#      Grandchildren and Presence of Parent of Grandchildren
#	[B23001] Sex by Age by Employment Status for Population 16 Years and Over
#	[C15002A-I] Sex by Educational Attainment for the Population 25 Years and Older
# [B17001A-I] Poverty Status in Past 12 Months by Sex by Age
# [B17012] Poverty Status in Past 12 Months of Families by Household Type by Number of Related Children under 18 Years
# [B17024] Age By Ratio Of Income To Poverty Level In The Past 12 Months
# [B17026] Ratio of Income to Poverty Level of Families in the Last 12 Months
#	[B01003] Total Population

# ------------------------------------ #
# Pull ACS metadata for tables of interest
# ------------------------------------ #

# Pull metadata for ACS 5-year tables

acs5_years <- {acs5_year}:{acs5_year}
acs5_tables <-  c("B01001", "B09001", "B17024", "B17026",
                  "B03002", "B08006", "B08013", "B10051", "B23001", "C15002", 
                  paste0("B17001", c("", LETTERS[1:9])),
                  "B17012", "B01003", "B10063", "B08303")

if (update_meta) {

	## Pull ACS 5-year data
	
	acs5_meta <- NULL
	for (y in acs5_years) {
		print(paste("Pulling metadata for ACS 5-year data with endyear", y))
	  apiname <- ifelse(y > 2009, "acs/acs5","acs5")
		meta <- 
			listCensusMetadata(name = apiname, vintage = y) %>%
			mutate(endyear = y, source = "acs5")
			
		acs5_meta <- bind_rows(acs5_meta, meta)
		}

	save(acs5_meta, 
	     file = glue("{input_path}acs5_metadata_{my_output_tag}.Rda"))
	
} else {
  load(file = glue("{input_path}acs5_metadata_{my_output_tag}.Rda"))
}


# ------------------------------------ #
# Subset metadata to tables of interest, and check for differences in coding across years
# ------------------------------------ #

# Subset metadata to tables of interest

meta_sub <- acs5_meta %>% filter(grepl(paste(acs5_tables, collapse="|"), name))

# Cross-tabulate tables and sources

meta_sub  %>% with(table(group, source))

# Check that there are no differences in the semantic meaning of the variables across time or data source

  ## "Concept" describes table, and sometimes contains info about subpopulation (e.g. WHITE ALONE) that table pertains to.
  ## If concept is ever non-missing, keep only rows for that combination of field name and label.

meta_sub_q <-
	meta_sub %>%
	dplyr::select(group, name, label, concept, source) %>%
	group_by(group, name, label) %>%
	mutate(n_nonNA_concept = sum(!is.na(concept))) %>%
	filter(ifelse(n_nonNA_concept>=1, !is.na(concept), TRUE)) %>%
	dplyr::select(-n_nonNA_concept) %>%
	unique()

# Print variables with more than one set of meta data
	
meta_sub_q %>%
	group_by(name, source) %>%
	summarize(n=n()) %>%
	filter(n>1)

# Proceed using deduplicated metadata information

meta_sub <- meta_sub_q

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# Develop ACS metadata ---------------------------------------------------------
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Develop metadata for [B01001A-I] Sex by Age ----------------------------------
# ---------------------------------------------------------------------------- #

meta_renames_B01001 <-
  meta_sub %>% 
  filter(grepl("^B01001", group)) %>% 
  filter(grepl("years?", label)) %>% # This keeps only the deepest level of detail
  separate(label, into = c("est", "count", "gender", "ageraw"), sep = "!!") %>% 
  mutate(age = gsub("\\D*(\\d+) (to|and) (\\d+)\\D*", "\\1to\\3", ageraw) %>% 
           gsub("\\D*Under (\\d+)\\D*", "lt\\1", x = .) %>% 
           gsub("(\\d+)\\D*and over", "\\1plus", x = .) %>% 
           gsub("\\D*(\\d+) \\D*", "\\1to\\1", x = .),
         raceeth = case_when(grepl("^SEX BY AGE$",    concept) ~ "All",
                             grepl("INDIAN",          concept) ~ "AmInd",
                             grepl("ASIAN",           concept) ~ "As",
                             grepl("BLACK.+ALONE",    concept) ~ "Aa",
                             grepl("\\(HISPANIC",     concept) ~ "Hi",
                             grepl("ISLANDER",        concept) ~ "PacIs",
                             grepl("OTHER",           concept) ~ "Oth",
                             grepl("TWO OR MORE",     concept) ~ "Mult",
                             grepl("WHITE ALONE\\)",  concept) ~ "Wh",
                             grepl("WHITE.+NOT HISP", concept) ~ "Wh_NonH"),
         gender = gender %>% str_replace(":", "")) %>% 
  ungroup()

# Verify that variable names were correctly constructed, and save variable-to-variable name crosswalk

meta_renames_B01001 %>% ungroup() %>% arrange(name) %>% dplyr::select(ageraw, age) %>% unique()
meta_renames_B01001 %>% ungroup() %>% arrange(name) %>% dplyr::select(concept, raceeth) %>% unique()
meta_renames_B01001 <- meta_renames_B01001 %>% dplyr::select(-group, -ageraw, -concept, -est, -count)

# ---------------------------------------------------------------------------- #
# Develop metadata for [B09001] Population Under 18 Years of Age ---------------
# ---------------------------------------------------------------------------- #

meta_renames_B09001 <-
  meta_sub %>% 
  filter(grepl("^B09001", name)) %>% 
  filter(grepl("years?$", label)) %>% # This keeps only the deepest level of detail
  separate(label, into = c("estimate", "total", "hh", "ageraw"), sep = "!!") %>% 
  mutate(age = gsub("\\D*(\\d+) (to|and) (\\d+)\\D*", "\\1to\\3", ageraw) %>% 
           gsub("\\D*Under (\\d+)\\D*", "lt\\1", x = .) %>% 
           gsub("\\D*(\\d+) \\D*", "\\1to\\1", x = .)) %>% 
  ungroup()

# Check on mappings in the data and success of creating variable names

meta_renames_B09001 %>% ungroup() %>% arrange(name) %>% dplyr::select(ageraw, age) %>% unique()
meta_renames_B09001 <- meta_renames_B09001 %>% dplyr::select(-group, -concept, -estimate, -total, -hh, -ageraw)

# ---------------------------------------------------------------------------- #
# Develop metadata for [B17001A-I] Poverty Level by Sex and By Age  ------
# ---------------------------------------------------------------------------- #

meta_renames_B17001 <-
  meta_sub %>%
  filter(grepl("B17001", group)) %>%
  separate(label, into = c("est", "count", "pov", "gender", "ageraw"), sep = "!!") %>%
  filter(ageraw %in% c("Under 5 years", "5 years")) %>% 
  mutate(pov_ind = str_replace(pov, ".+((above|below) poverty).+", "\\1"),
         raceeth = case_when(str_detect(concept, "BY AGE$") ~ "All",
                             str_detect(concept, "INDIAN") ~ "AmInd",
                             str_detect(concept, "ASIAN") ~ "As",
                             str_detect(concept, "BLACK.+ALONE") ~ "Aa",
                             str_detect(concept, "\\(HISPANIC") ~ "Hi",
                             str_detect(concept, "ISLANDER") ~ "PacIs",
                             str_detect(concept, "OTHER") ~ "Oth",
                             str_detect(concept, "TWO OR MORE") ~ "Mult",
                             str_detect(concept, "WHITE ALONE\\)") ~ "Wh",
                             str_detect(concept, "WHITE.+NOT HISP") ~ "Wh_NonH"),
         suffix = str_replace(group, "B17001", ""),
         age = case_when(ageraw == "Under 5 years" ~ "le5",
                         ageraw == "5 years"       ~ "5")) %>% 
  ungroup()

# Check on the mapping of subtables to race and ethnicity
if (FALSE) {
  meta_renames_B17001 %>% 
    dplyr::select(suffix, raceeth) %>% 
    unique() %>% 
    arrange(suffix)
}

# ---------------------------------------------------------------------------- #
# Develop metadata for [B17024] Age by Ratio of Income to Poverty Level in the Past 12 Months  ------
# ---------------------------------------------------------------------------- #

meta_renames_B17024 <-
  meta_sub %>%
  filter(grepl("B17024", group)) %>%
  separate(label, into = c("est", "count", "ageraw", "rawratio"), sep = "!!") %>%
  mutate(age = gsub("\\D*(\\d+) (to|and) (\\d+)\\D*", "\\1to\\3", ageraw) %>% 
               gsub("\\D*Under (\\d+)\\D*", "lt\\1", x = .) %>% 
               gsub("\\D*(\\d+) \\D*", "\\1to\\1", x = .) %>% 
               replace_na("All"),
         ratio = gsub("(\\d*).(\\d+) to (\\d*).(\\d+)", "r\\1\\2to\\3\\4", rawratio) %>%
                 gsub("(\\d*).(\\d+) and over", "r\\1\\2plus", x = .) %>%
                 gsub("Under (\\d*).(\\d+)", "r0to\\1\\2", x = .) %>%
                 replace_na("All")) %>%
  ungroup()

# Verify that variable names were correctly constructed, and save variable-to-variable name crosswalk

meta_renames_B17024 %>% ungroup() %>% arrange(name) %>% select(ratio, rawratio) %>% unique()
meta_renames_B17024 %>% ungroup() %>% arrange(name) %>% select(age, ageraw) %>% unique()
meta_renames_B17024 <- meta_renames_B17024 %>% select(-group, -concept, -est, -count, -ageraw, -rawratio)

# ---------------------------------------------------------------------------- #
# Develop metadata for [B17026] Poverty Level of Families Past 12 Months  ------
# ---------------------------------------------------------------------------- #

meta_renames_B17026 <-
  meta_sub %>%
  filter(grepl("B17026", group)) %>%
  separate(label, into = c("est", "count", "rawratio"), sep = "!!") %>%
  mutate(ratio = gsub("(\\d*).(\\d+) to (\\d*).(\\d+)", "r\\1\\2to\\3\\4", rawratio) %>%
           gsub("(\\d*).(\\d+) and over", "r\\1\\2plus", x = .) %>%
           gsub("Under (\\d*).(\\d+)", "r0to\\1\\2", x = .) %>%
           replace_na("All")) %>%
  ungroup()

# Verify that variable names were correctly constructed, and save variable-to-variable name crosswalk

meta_renames_B17026 %>% ungroup() %>% arrange(name) %>% dplyr::select(ratio, rawratio) %>% unique()
meta_renames_B17026 <- meta_renames_B17026 %>% dplyr::select(-group, -concept, -est, -count, -rawratio)

# ---------------------------------------------------------------------------- #
# Develop metadata for [B03002] Hispanic or Latino Origin by Race --------------
# ---------------------------------------------------------------------------- #

meta_renames_B03002 <-
  meta_sub %>%
  filter(grepl("B03002", group)) %>%
  separate(label, into=c("est", "count", "hisp", "race1", "race2"), sep= "!!") %>%
  filter((!is.na(race1) & is.na(race2)) | (is.na(hisp) & is.na(race1))) %>% # Keep preferred level of aggregation
  mutate(race1 = tolower(race1)) %>%
  mutate(race = case_when(is.na(race1) ~ "all",
                          grepl("white", race1) ~ "wh",
                          grepl("black", race1) ~ "bl", 
                          grepl("indian", race1) ~ "amind", 
                          grepl("hawaiian", race1) ~ "pacisl",
                          grepl("asian", race1) ~ "as",
                          grepl("two or more", race1) ~ "mult",
                          grepl("some other", race1) ~ "oth"),
         hispanic = case_when(is.na(hisp) ~ "all",
                              grepl("^Hispanic", hisp) ~ "h",
                              grepl("^Not Hispanic", hisp) ~ "nonh")) %>%
  unite(raceeth, race, hispanic) %>%
  ungroup()  

# Verify that variable names were correctly constructed, and save variable-to-variable name crosswalk

meta_renames_B03002 %>% ungroup() %>% arrange(name) %>% dplyr::select(race1, hisp, raceeth) %>% unique()
meta_renames_B03002 <- meta_renames_B03002 %>% dplyr::select(-group, -concept, -est, -count, -race1, -race2, -hisp)

# ---------------------------------------------------------------------------- #
# Develop metadata for [B08006] Sex of Workers by Transportation to Work -------
# ---------------------------------------------------------------------------- #

meta_renames_B08006 <-
    meta_sub %>%
    filter(grepl("B08006", group)) %>%
    separate(label, into=c("est", "count", "cat1", "cat2", "cat3", "cat4"), sep = "!!") %>%
    filter(is.na(cat1) | (!is.na(cat1) & is.na(cat2) & !(cat1=="Male:" | cat1=="Female:"))) %>%
    mutate(cat1 = tolower(cat1)) %>%
    mutate(transportwork = case_when(is.na(cat1) ~ "All",
                                     grepl("worked from home", cat1) ~ "WorkAtHome",
                                     grepl("walked", cat1) ~ "Walk",
                                     grepl("bicycle", cat1) ~ "Bike",
                                     grepl("public", cat1) ~ "PublicTransport",
                                     grepl("car", cat1) ~ "Car",
                                     grepl("other means", cat1) ~ "Other" )) %>%
    ungroup()
  
# Verify that variable names were correctly constructed, and save variable-to-variable name crosswalk
 
meta_renames_B08006 %>% ungroup() %>% arrange(name) %>% dplyr::select(cat1, transportwork) %>% unique()
meta_renames_B08006 <- meta_renames_B08006 %>% dplyr::select(-group, -concept, -est, -count, -cat1, -cat2, -cat3, -cat4)

# ---------------------------------------------------------------------------- #
# Develop metadata for [B08013] Aggregate Travel Time to Work by Sex -----------
# ---------------------------------------------------------------------------- #

# Obtain count of universe of workers for this table from [B08303]

meta_renames_B08013 <-
  meta_sub %>%
  filter(grepl("B08013", group)) %>%
  separate(label,
           into = c("est", "count", "gender"), 
           sep = "!!") %>%
  # We only want estimates for all workers, so we discard estimates that are sex-specific
  filter(is.na(gender)) %>% 
  ungroup()

# Verify that variable names were correctly constructed, and save variable-to-variable name crosswalk

meta_renames_B08013 <- meta_renames_B08013 %>% dplyr::select(-group, -concept, -est, -count, -gender)

# ---------------------------------------------------------------------------- #
# Develop metadata for [B08303] Travel Time to Work ----------------------------
# ---------------------------------------------------------------------------- #

meta_renames_B08303 <-
  meta_sub %>%
  filter(grepl("B08303", group)) %>%
  separate(label, into=c("est", "count", "rawtraveltime"), sep = "!!") %>%
  mutate(traveltime = gsub("(\\d*.\\d+) to (\\d*.\\d+) minutes", "\\1to\\2min", rawtraveltime) %>%
           gsub("(\\d) to (\\d) minutes", "\\1to\\2min", x = .) %>%
           gsub("(\\d*.\\d+) or more minutes", "\\1minplus", x = .) %>%
           gsub("Less than (\\d) minutes", "0to\\1min", x = .) %>%
           replace_na("All")) %>%
  ungroup()

# Verify that variable names were correctly constructed, and save variable-to-variable name crosswalk

meta_renames_B08303 %>% ungroup() %>% arrange(name) %>% dplyr::select(traveltime, rawtraveltime) %>% unique()
meta_renames_B08303 <- meta_renames_B08303 %>% dplyr::select(-group, -concept, -est, -count, -rawtraveltime)

# ---------------------------------------------------------------------------- #
# Develop metadata for [B10051] Grandparents Living with Own Grandchildren -----
#   by Responsibility for Own Grandchildren by  Presence of Parent of -------- #
#   Grandchildren and Age of Grandparent ------------------------------------- #
# ---------------------------------------------------------------------------- #

meta_renames_B10051 <-
  meta_sub %>%
  filter(grepl("B10051", group)) %>%
  separate(label, into=c("est", "count", "resp", "age"), sep = "!!") %>%
  filter(!is.na(resp) & is.na(age) &
         concept=="GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN BY PRESENCE OF PARENT OF GRANDCHILDREN AND AGE OF GRANDPARENT") %>% #This eliminates by-race counts
  mutate(
    gptype = case_when(str_detect(resp, "Grandparent not responsible") ~ "GCNoR",
                       str_detect(resp, "Grandparent responsible")     ~ "GCR" )) %>%
  ungroup()

# Verify that variable names were correctly constructed, and save variable-to-variable name crosswalk

meta_renames_B10051 %>% ungroup() %>% arrange(name) %>% dplyr::select(gptype, resp) %>% unique()
meta_renames_B10051 <- meta_renames_B10051 %>% dplyr::select(-group, -concept, -est, -count, -age, -resp)

# ---------------------------------------------------------------------------- #
# Develop metadata for [B10063] Households with Grandparents Living with -------
#   Own Grandchildren Under 18 Years by Responsibility for Own --------------- #
#   Grandchildren and Presence of Parent of Grandchildren -------------------- #
# ---------------------------------------------------------------------------- #

meta_renames_B10063 <-
  meta_sub %>%
  filter(grepl("B10063", group)) %>%
  separate(label, 
           into = c("est", "count", "type", "resp", "respshare"), 
           sep = "!!") %>%
  filter(is.na(type) | 
          str_detect(type, "Household without grandparents") | 
         (str_detect(type, "Household with grandparents") & 
            !is.na(resp) & 
            is.na(respshare))) %>%
  mutate(
      hhtype = case_when(str_detect(type, "Household with grandparents") &
                         str_detect(resp, "Household with grandparent responsible") ~ "GCR",
                         str_detect(type, "Household with grandparents") &
                         str_detect(resp, "Household with grandparent not responsible") ~ "GCNoR",
                         str_detect(type, "Household without grandparents") ~ "NoGC",
                         is.na(type) ~ "All")) %>%
  ungroup()

# Verify that variable names were correctly constructed, and save variable-to-variable name crosswalk

meta_renames_B10063 %>% ungroup() %>% arrange(name) %>% dplyr::select(hhtype, type, resp) %>% unique()
meta_renames_B10063 <- meta_renames_B10063 %>% dplyr::select(-group, -concept, -est, -count, -type, -resp, -respshare)

# ---------------------------------------------------------------------------- #
# Develop metadata for [B23001] Sex by Age by Employment Status ----------------
# ---------------------------------------------------------------------------- #

meta_renames_B23001 <-
  meta_sub %>%
  filter(grepl("B23001", group)) %>%
  separate(label, 
           into = c("est", "count", "gender", "ageraw", "lfstatus", "lftype", "empstatus"), 
           sep = "!!") %>%
  # Keep records that either represent all counts within a gender, or
  # are gender and age specific, 
  filter((!is.na(gender) &  is.na(ageraw)) | 
         (!is.na(gender) & !is.na(ageraw) & 
            (str_detect(lfstatus, "Not in labor force") | 
            (
             str_detect(lfstatus, "In labor force") &
              (
               !is.na(empstatus) | 
               str_detect(lftype, "In Armed Forces"))
              )
            )
          )
         )  %>%
  mutate(age = str_replace(ageraw, 
                           "\\D*(\\d+) (to|and) (\\d+)\\D*:",
                           "\\1to\\3") %>% 
               str_replace("75 years and over:",
                           "75plus") %>%
               replace_na("All"),
         gender = gender %>% str_replace(":", ""),
         lfstatus = lfstatus %>% str_replace(":", ""),
         lftype   = lftype   %>% str_replace(":", "")) %>%
  ungroup()

# Verify that variable names were correctly constructed, and save variable-to-variable name crosswalk

meta_renames_B23001 %>% ungroup() %>% arrange(name) %>% dplyr::select(ageraw, age) %>% unique()
meta_renames_B23001 <- meta_renames_B23001 %>% dplyr::select(-group, -concept, -est, -count, -ageraw)

# ---------------------------------------------------------------------------- #
# Develop metadata for [C15002A-I] Sex by Educational Attainment ---------------
# ---------------------------------------------------------------------------- #

meta_renames_C15002 <-
  meta_sub %>%
  filter(grepl("^C15002", group)) %>%
  filter(group!="C15002") %>%
  separate(label, into=c("est", "count", "gender", "educraw"), sep = "!!") %>% 
  filter(!is.na(gender) &  !is.na(concept)) %>%
  mutate(raceeth = 
           case_when(str_detect(str_to_upper(concept), "INDIAN")          ~ "amind",
                     str_detect(str_to_upper(concept), "ASIAN")           ~ "as",
                     str_detect(str_to_upper(concept), "BLACK.+ALONE")    ~ "bl",
                     str_detect(str_to_upper(concept), "\\(HISPANIC")     ~ "h",
                     str_detect(str_to_upper(concept), "ISLANDER")        ~ "pacisl",
                     str_detect(str_to_upper(concept), "OTHER")           ~ "oth",
                     str_detect(str_to_upper(concept), "TWO OR MORE")     ~ "mult",
                     str_detect(str_to_upper(concept), "WHITE ALONE\\)")  ~ "wh",
                     str_detect(str_to_upper(concept), "WHITE.+NOT HISP") ~ "wh_nonh",
                     str_to_upper(concept)=="SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER" ~ "all"),
         educ = 
           case_when(str_detect(educraw, "High school graduate")               ~ "hsgrad", 
                     str_detect(educraw, "Bachelor's degree or higher")        ~ "coll", 
                     str_detect(educraw, "Some college or associate's degree") ~ "somecoll", 
                     str_detect(educraw, "Less than high school diploma")      ~ "lesshs", 
                     str_detect(educraw, "Associate's degree")                 ~ "somecoll",
                     str_detect(educraw, "Bachelor's degree")                  ~ "coll", 
                     str_detect(educraw, "Graduate or professional degree")    ~ "coll",
                     str_detect(educraw, "Less than 9th grade")                ~ "lesshs",
                     str_detect(educraw, "9th to 12th grade, no diploma")      ~ "lesshs",
                     str_detect(educraw, "Some college, no degree")            ~ "somecoll",
                     is.na(educraw) ~ "all"),
         gender = str_replace(gender, ":", "")) %>%
  ungroup() 

# Verify that variable names were correctly constructed, and save variable-to-variable name crosswalk

meta_renames_C15002 %>% ungroup() %>% arrange(name) %>% dplyr::select(raceeth, concept) %>% unique()
meta_renames_C15002 %>% ungroup() %>% arrange(name) %>% dplyr::select(educ, educraw) %>% unique()
meta_renames_C15002 <- meta_renames_C15002 %>% dplyr::select(-group, -concept, -est, -count, -educraw)

# ---------------------------------------------------------------------------- #
# Develop metadata for [B17012] Poverty Status in Past 12 Months of Families ----
#   by Household Type by Number of Related Children under 18 Years ----------- #
# ---------------------------------------------------------------------------- #

meta_renames_B17012_pov <-
  meta_sub %>%
  filter(grepl("B17012", group)) %>%
  separate(label, into=c("est", "count", "incpov", "family", "type", "kids"), sep = "!!") %>%
  filter(is.na(incpov) | (is.na(family)  & !is.na(incpov))) %>%
  mutate( 
      hhpov = case_when(grepl("Income in the past 12 months at or above poverty level", incpov) ~ "AtAbovePov",
                grepl("Income in the past 12 months below poverty level", incpov)               ~ "BelowPov",
                is.na(incpov) ~ "All")) %>%
  ungroup()

# Verify that variable names were correctly constructed, and save variable-to-variable name crosswalk

meta_renames_B17012_pov %>% ungroup() %>% arrange(name) %>% dplyr::select(hhpov, incpov) %>% unique()
meta_renames_B17012_pov <- meta_renames_B17012_pov %>% dplyr::select(-group, -concept, -est, -count, -family, -type, -kids, -incpov)

### Separate development of this table for measures of ------------------------#
### presence of adults with kids ----------------------------------------------#

meta_renames_B17012_fam <-
  meta_sub %>%
  filter(grepl("B17012", group)) %>%
  separate(label, into=c("est", "count", "incpov", "family", "type", "kids"), sep = "!!") %>%
  filter((str_detect(family, "Married") & str_detect(type, "child")) | 
         (str_detect(family, "Other ")  & str_detect(kids, "child"))) %>%
  mutate( 
    hhpov = case_when(grepl("Income in the past 12 months at or above poverty level", incpov) ~ "AtAbovePov",
                      grepl("Income in the past 12 months below poverty level", incpov)       ~ "BelowPov",
                      is.na(incpov) ~ "All"),
    fam_type = case_when(str_detect(family, "Married") ~ "Married",
                         str_detect(type,   "Female")  ~ "Female_noSp",
                         str_detect(type,   "Male")    ~ "Male_noSp")) %>%
  ungroup() %>% 
  arrange(fam_type, hhpov)
  
# Verify that variable names were correctly constructed, and save variable-to-variable name crosswalk

meta_renames_B17012_fam %>% ungroup() %>% arrange(name) %>% dplyr::select(hhpov, incpov) %>% unique()
meta_renames_B17012_fam <- meta_renames_B17012_fam %>% dplyr::select(-group, -concept, -est, -count, -family, -type, -kids, -incpov)

### Combine these measures
meta_renames_B17012 <- 
  bind_rows(meta_renames_B17012_pov,
            meta_renames_B17012_fam)

# ---------------------------------------------------------------------------- #
# Develop metadata for [B01003] Total Population -------------------------------
# ---------------------------------------------------------------------------- #

meta_renames_B01003 <-
  meta_sub %>%
  filter(grepl("B01003", group)) %>%
  separate(label, into=c("est", "count"), sep = "!!") %>%
  mutate(count="totalpop") %>%
  ungroup()

# Verify that variable names were correctly constructed, and save variable-to-variable name crosswalk

meta_renames_B01003 <- meta_renames_B01003 %>% dplyr::select(-group, -concept, -est)
  
# ---------------------------------------------------------------------------- #
# Add margin of error variables to metadata (modify last letter of variable name, store modified variable names in table)
# ---------------------------------------------------------------------------- #

tablenums <- 
  acs5_tables %>% 
  str_replace("[A-I]$", "") %>% 
  unique()

for (tablenum in tablenums){
  meta_name <- paste0("meta_renames_", tablenum)
  d_est <- get(meta_name)
  d_moe <- 
    d_est %>% 
    ungroup() %>%
    mutate(name = gsub("E$", "M", name))
  d_aug <- bind_rows(d_est %>% mutate(stat = "est"),
                     d_moe %>% mutate(stat = "moe"))
  assign(meta_name, d_aug)
}

table_fields <- meta_sub %>% ungroup() %>% dplyr::select(group, name, source)
all_fields <- 
  bind_rows(table_fields,
            table_fields %>% mutate(name = gsub("E$", "M", name)))

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# PULL ACS DATA ----------------------------------------------------------------
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Pull ACS 5-year data (tract-level geographies are available for 5-year data)
# ---------------------------------------------------------------------------- #

if (update_acs_pulls) {
  
  acs5 <- data.frame()
  
  for (year in acs5_years){
    
    print(paste("Pulling ACS 5-year data for endyear", year))
    apiname <- ifelse(year > 2009, "acs/acs5","acs5")
    # Note, if we plan to pull earlier data
    # The ACS changed their naming convention for their API between 2009 and 2010 - and the package does not handle that for us
    # https://api.census.gov/data/2009/acs5
    # https://api.census.gov/data/2010/acs/acs5/
    acs5pull <- getCensus(name = apiname, 
                          vintage = year, 
                          vars = c("NAME", all_fields %>% filter(source == "acs5") %>% pull(name)),
                          region = "tract:*", 
                          regionin = glue("state:{my_state_fip}"))
    #setnames(acs5pull, acs5names$field, acs5names$newname)
    
    # Note that Census tracts are uniquely identified by combination of state, county and tract identifier codes
    # https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html
    # https://api.census.gov/data/2009/acs5/geography.json
    
    acs5pull_long <-
      acs5pull %>% 
      #dplyr::select(-state, -county) %>%
      mutate(source = "acs5",
             endyear = year,
             geo = "tract") %>% 
      mutate(geo_val = paste0(state, county, tract)) %>%
      gather(field, val, -source, -endyear, -geo, -NAME, -state, -county, -tract, -geo_val) %>% 
      dplyr::select(source, endyear, geo, NAME, field, val, geo_val)
    
    # Make data wide by stat (estimate and moe)
    acs5pull_wide.stat <-
      acs5pull_long %>% 
      separate(field, 
               into = c("field", "stat"),
               sep = -1) %>% 
      mutate(stat = case_when(stat == "E" ~ "est",
                              stat == "M" ~ "moe")) %>% 
      pivot_wider(names_from = "stat",
                  values_from = "val")
    
    acs5 <- bind_rows(acs5, acs5pull_wide.stat)
  } # End of loop across years
}

# ---------------------------------------------------------------------------- #
# Process ACS tables -----------------------------------------------------------
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Parse, rename and save the various tables from 5-year data
# ---------------------------------------------------------------------------- #

if (update_acs_pulls) {
  tableList <- NULL
  source_tables <- 
    acs5_tables %>% 
    str_replace("[A-I]$", "") %>% 
    unique()
  source <- "acs5"
  
  for (tablenum in source_tables){
    
    print(paste("Working on table number", tablenum))
    
    # Prep metadata
    meta_table <-
      get(paste0("meta_renames_", tablenum)) %>% 
      dplyr::select(-source) %>% 
      unique() %>% 
      # Remove last character (should just be "E") which inappropriately 
      # indicates only "e"stimates rather than the full field name
      mutate(field = str_replace(name, "\\w$", "")) %>% 
      dplyr::select(-matches("^name$|^stat$|^est$"))
    
    # Merge in metadata and reshape to long by measure, wide by stat (i.e. est vs moe)
    d <- 
      get(source) %>% 
      filter(str_detect(field, paste0("^", tablenum))) %>% 
      merge(meta_table,
            by = "field") %>% 
      # We're performing an inner join here. This keeps only fields in the metadata we've
      # developed in effect drops source data columns which correspond to counts that are
      # less than the full depth. E.g in by-age variables, we don't keep the field which
      # is the sum across all ages.
      dplyr::select(-field) %>% 
      unique()
    
    tableName <- paste0("acs5_", tablenum)
    assign(tableName, d)
    tableList <- c(tableList, tableName)
  } #  End of loop across table numbers
  save(list = tableList,
       file = glue("{input_path}acs_tables_{my_output_tag}.Rda"))
} else {
  load(glue("{input_path}acs_tables_{my_output_tag}.Rda"))
}

# -----------------------------------------------------------------------------#
# Produce new variables for ACS data set ---------------------------------------
# -----------------------------------------------------------------------------#

# I construct three types of variables:

# *_est   -- share or average variable
# *_se    -- standard error for *_est variable (or, if no _est variable, for _count variable)
# *_count -- underlying population count for *_est variable (i.e., denominator value for calculated statistic)

# Note that margin of error equals 1.645 * standard error in ACS tables.
# Note that *_est is missing if the *_count equals 0 or is missing. If *_count is missing, this indicates that
# the data is suppressed in the original ACS tables.

# ------------------------------------ #
# Age distribution
# ------------------------------------ #

# Extract variables of interest.

acs_age <-
  acs5_B01001 %>% 
  filter(raceeth == "All")

age_list <- unique(acs_age$age)

acs_age <-
  acs_age %>%
  group_by(source, endyear, geo, geo_val, NAME, age) %>%
  summarize(est = sum(est),
            se = se_sum(moe)) %>%  
  gather(stat, val, est, se) %>%
  unite(colname, age, stat) %>%
  mutate(colname = paste0("a", colname)) %>%
  spread(colname, val) 

acs_age_all <-
  acs5_B01001 %>% 
  filter(raceeth == "All") %>%
  group_by(source, endyear, geo, geo_val, NAME) %>%
  summarize(All_est = sum(est), All_se = se_sum(moe))  

acs_age <- merge(acs_age, 
                 acs_age_all, 
                 by = c("source", "endyear", "geo", "geo_val", "NAME"),
                 all.x = TRUE,
                 all.y = FALSE)

# Prepare age distribution variables.

# /!\ NSM: Easier if data were reshaped?
for (r in age_list) {
  
  ## Prepare variable names
  
  num      <- paste0("a", r, "_est")
  denom    <- "All_est"
  num_se   <- paste0("a", r, "_se")
  denom_se <- "All_se"
  
  newvar       <- paste0("agedist_", r, "_est")
  newvar_se    <- paste0("agedist_", r, "_se")
  newvar_count <- paste0("agedist_", r, "_count")
  
  ## Prepare new variables
  
  acs_age <-
    acs_age %>%
    mutate(
      !!newvar       := eval(parse(text=num))/eval(parse(text=denom)),
      !!newvar_se    := se_proportion(eval(parse(text=num)), 
                                      eval(parse(text=denom)),
                                      eval(parse(text=num_se)), 
                                      eval(parse(text=denom_se))),
      !!newvar_count := eval(parse(text=denom)))
}

acs_age <-
  acs_age %>%
  dplyr::select(., 1:5, contains("agedist_"))

# Check summary statistics.

exclude_vars <- names(acs_age) %in% c("source", "endyear", "geo", "geo_val", "NAME")
summary(acs_age[!exclude_vars])

# ------------------------------------ #
# Population Under 18 Years of Age
# ------------------------------------ #

# Extract variables of interest.

acs_childpop <-
  acs5_B09001 %>%
  mutate(se = moe/1.645) %>%
  dplyr::select(-moe) %>%
  rename(count = est) %>%
  gather(stat, val, count, se) %>%
  unite(colname, age, stat) %>%
  mutate(colname = paste0("age_", colname)) %>%
  spread(colname, val) %>% 
  # /!\ Note that these field names do not follow conventions. At present,
  # "_count" denotes denominators (while "_est" is the rate of num/denom). 
  mutate(age_3to5_count  = age_3to4_count + age_5to5_count,
         age_0to5_count  = age_lt3_count  + age_3to5_count,
         age_6to12_count = age_6to8_count + age_9to11_count + age_12to14_count*(1/3),
         age_6to13_count = age_6to8_count + age_9to11_count + age_12to14_count*(2/3),
         
         age_3to5_se     = sqrt(age_3to4_se^2 + age_5to5_se^2),
         age_0to5_se     = sqrt(age_lt3_se^2 + age_3to5_se^2),
         age_6to12_se    = sqrt(age_6to8_se^2 + age_9to11_se^2 + (age_12to14_se*(1/3))^2),
         age_6to13_se    = sqrt(age_6to8_se^2 + age_9to11_se^2 + (age_12to14_se*(2/3))^2))

# Check summary statistics.

exclude_vars <- names(acs_childpop) %in% c("source", "endyear", "geo", "geo_val", "NAME")
summary(acs_childpop[!exclude_vars])

# ------------------------------------ #
# Ratio of income to poverty line -- by race/ethnicity
# ------------------------------------ #

# NSM: This code--not yet working--was put on pause, in favor of bringing in
# and updating the `pull 5-year data on poverty and race.R` from the old repo.
# In the future, rather than getting that pull to join this file, I believe it
# will be more worthwhile getting more robust pull and refinement code working
# for ACS data

# acs_incpov_raceeth <-
#   acs5_B17001 %>%  
#   gather(stat, val, est, moe) %>%
#   unite(colname, ratio, stat) %>%
#   spread(colname, val) 


# ------------------------------------ #
# Age by Ratio of Income to Poverty Line (Universe: population for who poverty is determined)
# ------------------------------------ #

acs_incpov_age <- 
  acs5_B17024 %>% 
  mutate(se = moe/1.645) %>% 
  select(-moe) %>% 
  filter(age %in% c("lt6", "6to11", "12to17"))
  
acs_incpov_age_allratio <-
  acs_incpov_age %>% 
  filter(ratio == "All") %>% 
  select(geo_val, age, est_all = est, se_all = se)

unite_fpls <- function(range_label, ranges) {
  stopifnot(all(ranges %in% unique(acs5_B17024$ratio)))
  acs_incpov_age %>% 
    filter(ratio %in% ranges) %>% 
    group_by(source, endyear, geo, NAME, geo_val, age) %>% 
    summarize(est = sum(est),
              se  = sqrt(sum(se^2))) %>% 
    mutate(ratio = range_label)
}
acs_incpov_age_myranges <- 
  bind_rows(unite_fpls("r0to50",    c("r0to50")),
            unite_fpls("r0to74",    c("r0to50", "r50to74")),
            unite_fpls("r50to100",  c("r50to74", "r75to99")),
            unite_fpls("r0to100",   c("r0to50", "r50to74", "r75to99")),
            unite_fpls("r75to149",  c("r75to99", "r100to124", "r125to149")),
            unite_fpls("r150to199", c("r150to174", "r175to184", "r185to199")),
            unite_fpls("r100to184", c("r100to124", "r125to149", "r150to174", "r175to184")),
            unite_fpls("r100to199", c("r100to124", "r125to149", "r150to174", "r175to184", "r185to199")),
            unite_fpls("r100to199", c("r100to124", "r125to149", "r150to174", "r175to184", "r185to199")),
            unite_fpls("r200to299", c("r200to299")),
            unite_fpls("r300to399", c("r300to399")),
            unite_fpls("r200to399", c("r200to299", "r300to399")),
            unite_fpls("r400plus",  c("r400to499", "r500plus")))

acs_incpov_age_out <- 
  acs_incpov_age_myranges %>% 
  merge(acs_incpov_age_allratio,
        by = c("geo_val", "age")) %>% 
  mutate(est = est / est_all,
         se  = se_proportion(est, est_all, se, se_all),
         prefix = "incpov") %>% 
  select(-se_all) %>% 
  rename(count = est_all) %>% 
  pivot_longer(cols = c(est, se, count)) %>% 
  unite("label", prefix, age, ratio, name) %>% 
  unique() %>% 
  pivot_wider(names_from = "label",
              values_from = "value")

# Check values
if (FALSE) {
  my_geo <- acs_incpov_age$geo_val[100]
  (check_num <- acs_incpov_age %>% filter(geo_val == my_geo, age == "lt6", ratio %in% c("r0to50", "r50to74")) %>% select(age, ratio, est, se))
  (check_denom <- acs_incpov_age_allratio %>% filter(geo_val == my_geo, age == "lt6"))
  (check_final <- acs_incpov_age_out %>% filter(geo_val == my_geo) %>% select(incpov_lt6_r0to74_est, incpov_lt6_r0to74_se))
  (sum(check_num$est) / check_denom$est_all) == check_final$incpov_lt6_r0to74_est
}


# ------------------------------------ #
# Ratio of income to poverty line (Universe: all families)
# Total number of families in Census tract
# ------------------------------------ #

# Extract variables of interest.

acs_incpov <-
  acs5_B17026 %>%
  gather(stat, val, est, moe) %>%
  unite(colname, ratio, stat) %>%
  spread(colname, val) 

# Prepare income-to-poverty ratio variables.

# /!\ NSM: Easier if data were reshaped?
for (r in c("r0to50", "r50to74", "r75to99", "r100to124", "r125to149", "r150to174", "r175to184", 
            "r185to199", "r200to299", "r300to399", "r400to499", "r500plus")) {
  
  ## Prepare variable names
  
  num       <- paste0(r, "_est")
  denom     <- "All_est"
  num_moe   <- paste0(r, "_moe")
  denom_moe <- "All_moe"
  
  newvar       <- paste0("incpov_", r, "_est")
  newvar_se    <- paste0("incpov_", r, "_se")
  newvar_count <- paste0("incpov_", r, "_count")
  
  ## Prepare new variables
  
  acs_incpov <-
    acs_incpov %>%
    mutate(
      !!newvar       := eval(parse(text=num))/eval(parse(text=denom)),
      !!newvar_se    := se_proportion(eval(parse(text=num)), 
                                      eval(parse(text=denom)),
                                      eval(parse(text=num_moe))/1.645, 
                                      eval(parse(text=denom_moe))/1.645),
      !!newvar_count := eval(parse(text=denom)))
}

# Prepare family count variable.

acs_incpov <-
  acs_incpov %>%
  mutate(tot_families_count = All_est,
         tot_families_se = All_moe/1.645) %>%
  dplyr::select(., 1:5, contains("incpov"), contains("tot_families"))

# Build certain aggregates
# /!\ All of these calculations deserve a function, maybe operating in long form
acs_incpov <- 
  acs_incpov %>% 
  mutate(incpov_r0to74_count   =      incpov_r0to50_count  + incpov_r50to74_count,
         incpov_r0to74_est     =      incpov_r0to50_est    + incpov_r50to74_est,
         incpov_r0to74_se      = sqrt(incpov_r0to50_se^2   + incpov_r50to74_se^2),
         
         incpov_r50to100_count =                             incpov_r50to74_count  + incpov_r75to99_count,
         incpov_r50to100_est   =                             incpov_r50to74_est    + incpov_r75to99_est,
         incpov_r50to100_se    = sqrt(                       incpov_r50to74_se^2   + incpov_r75to99_se^2),
         
         incpov_r0to100_count  =      incpov_r0to50_count  + incpov_r50to74_count  + incpov_r75to99_count,
         incpov_r0to100_est    =      incpov_r0to50_est    + incpov_r50to74_est    + incpov_r75to99_est,
         incpov_r0to100_se     = sqrt(incpov_r0to50_se^2   + incpov_r50to74_se^2   + incpov_r75to99_se^2),
         
         incpov_r75to149_count =      incpov_r75to99_count + incpov_r100to124_count + incpov_r125to149_count,
         incpov_r75to149_est   =      incpov_r75to99_est   + incpov_r100to124_est   + incpov_r125to149_est,
         incpov_r75to149_se    = sqrt(incpov_r75to99_se^2  + incpov_r100to124_se^2  + incpov_r125to149_se^2),
         
         incpov_r150to199_count =      incpov_r150to174_count + incpov_r175to184_count + incpov_r185to199_count,
         incpov_r150to199_est   =      incpov_r150to174_est   + incpov_r175to184_est   + incpov_r185to199_est,
         incpov_r150to199_se    = sqrt(incpov_r150to174_se^2  + incpov_r175to184_se^2  + incpov_r185to199_se^2),
         
         incpov_r100to184_count =      incpov_r100to124_count + incpov_r125to149_count + 
                                       incpov_r150to174_count + incpov_r175to184_count,
         incpov_r100to184_est   =      incpov_r100to124_est   + incpov_r125to149_est + 
                                       incpov_r150to174_est   + incpov_r175to184_est,
         incpov_r100to184_se    = sqrt(incpov_r100to124_se^2  + incpov_r125to149_se^2 + 
                                       incpov_r150to174_se^2  + incpov_r175to184_se^2),
         
         incpov_r100to199_count = incpov_r100to124_count + incpov_r125to149_count + 
                                  incpov_r150to174_count + incpov_r175to184_count + incpov_r185to199_count,
         incpov_r100to199_est   = incpov_r100to124_est + incpov_r125to149_est + 
                                  incpov_r150to174_est + incpov_r175to184_est + incpov_r185to199_est,
         incpov_r100to199_se    = sqrt(incpov_r100to124_se^2 + incpov_r125to149_se^2 + 
                                       incpov_r150to174_se^2 + incpov_r175to184_se^2 + incpov_r185to199_se^2))

# Check summary statistics.

exclude_vars <- names(acs_incpov) %in% c("source", "endyear", "geo", "geo_val", "NAME")
summary(acs_incpov[!exclude_vars])

# Check NA's
acs_incpov %>% 
  filter(is.na(acs_incpov$incpov_r0to50_est)) %>% 
  select(geo_val) %>% 
  unique()
# NA's for 9 geo_val (17031381700, 17097863005, 17097863006, 17031980000, 17197980000, 17201980000, 17031980100, 17031990000, 17097990000)

# ------------------------------------ #
# Travel time from home to work
# ------------------------------------ #

# Obtain count of number of workers in universe for table [B08013], where universe 
# is "Workers 16 years and over who did not work at home" for table [B08013] and
# universe is "Workers 16 years and over" for table [B08006].

acs_count <-
  acs5_B08006 %>%
  group_by(source, endyear, geo, geo_val, NAME) %>%
  filter(transportwork=="All" | transportwork=="WorkAtHome") %>%
  gather(stat, val, est, moe) %>%
  unite(colname, transportwork, stat) %>%
  spread(colname, val) %>%
  mutate(count_est = All_est - WorkAtHome_est,
         # Formula for variance of difference between two random variables, 
         # where cov(All, WorkAtHome) = var(WorkAtHome): https://en.wikipedia.org/wiki/Variance
         count_se = sqrt((All_moe/1.645)^2 + (WorkAtHome_moe/1.645)^2 - 2*(WorkAtHome_moe/1.645)^2)) %>% 
  dplyr::select(-All_est, -All_moe, -WorkAtHome_est, -WorkAtHome_moe)
  # /!\ There are cases of negative variance, i.e. where WorkAtHome_moe > All_moe,
  # e.g. for source == "acs5", endyear == 2019, geo_val == 17043844701

# Produce average travel time variables.

acs_timetransport <-
  left_join(acs5_B08013, acs_count) %>% 
  mutate(
    timetransport_est = ifelse(count_est!=0, est/count_est, NA),
    timetransport_se = se_ratio(est, count_est, (moe/1.645), count_se),
    timetransport_count = count_est) %>%
  dplyr::select(-est, -moe, -count_est, -count_se)

rm(acs_count)

# Check summary statistics.

exclude_vars <- names(acs_timetransport) %in% c("source", "endyear", "geo", "geo_val", "NAME")
summary(acs_timetransport[!exclude_vars])

# ------------------------------------ #
# Mode of transportation from home to work
# ------------------------------------ #

# Produce share variables for mode of transportation.

acs_modetransport <-
  acs5_B08006 %>%
  gather(stat, val, est, moe) %>%
  unite(colname, transportwork, stat) %>%
  spread(colname, val) %>%
  mutate(
    modetrans_bike_est   = Bike_est/All_est,
    modetrans_bike_se    = se_proportion(Bike_est, All_est, Bike_moe/1.645, All_moe/1.645),
    modetrans_bike_count = All_est,
    modetrans_car_est    = Car_est/All_est,
    modetrans_car_se     = se_proportion(Car_est, All_est, Car_moe/1.645, All_moe/1.645),
    modetrans_car_count  = All_est,
    modetrans_walk_est   = Walk_est/All_est,
    modetrans_walk_se    = se_proportion(Walk_est, All_est, Walk_moe/1.645, All_moe/1.645),
    modetrans_walk_count = All_est,
    modetrans_pubtrans_est   = PublicTransport_est/All_est,
    modetrans_pubtrans_se    = se_proportion(PublicTransport_est, All_est, PublicTransport_moe/1.645, All_moe/1.645),
    modetrans_pubtrans_count = All_est,
    modetrans_workhome_est   = WorkAtHome_est/All_est,
    modetrans_workhome_se    = se_proportion(WorkAtHome_est, All_est, WorkAtHome_moe/1.645, All_moe/1.645),
    modetrans_workhome_count = All_est,
    modetrans_other_est      = Other_est/All_est,
    modetrans_other_se       = se_proportion(Other_est, All_est, Other_moe/1.645, All_moe/1.645),
    modetrans_other_count    = All_est
    ) %>%
  dplyr::select(-Bike_est, -Bike_moe, -Car_est, -Car_moe, -Walk_est, -Walk_moe, -PublicTransport_est, -PublicTransport_moe, 
            -WorkAtHome_est, -WorkAtHome_moe, -Other_est, -Other_moe, -All_est, -All_moe)

# Check summary statistics.

exclude_vars <- names(acs_modetransport) %in% c("source", "endyear", "geo", "geo_val", "NAME")
summary(acs_modetransport[!exclude_vars])

# ------------------------------------ #
# Employment rate by gender (employment/labor force participation)
# ------------------------------------ #

# Produce labor market rate variables.

acs_emp <- 
  acs5_B23001 %>%
  gather(stat, val, est, moe) %>%
  unite(colname, gender, stat) %>%
  spread(colname, val) %>%
  group_by(source, endyear, geo, geo_val, NAME) %>% 
  # NSM: could consider making a sumNA <- function(...) sum(..., na.rm = TRUE)
  summarize( 
    ## Collapse across age categories
    pop_f_est =    sum(ifelse(is.na(lfstatus) & age=="All", Female_est, NA), na.rm = TRUE),
    pop_f_se  = se_sum(ifelse(is.na(lfstatus) & age=="All", Female_moe, NA)),
    lf_f_est  =    sum(ifelse((lfstatus == "In labor force"), Female_est, NA), na.rm = TRUE),
    lf_f_se   = se_sum(ifelse((lfstatus == "In labor force"), Female_moe, NA)), 
    emp_f_est =    sum(ifelse((lftype == "In Armed Forces" | empstatus == "Employed"), Female_est, NA), na.rm = TRUE),
    emp_f_se  = se_sum(ifelse((lftype == "In Armed Forces" | empstatus == "Employed"), Female_moe, NA)),
    pop_m_est =    sum(ifelse(is.na(lfstatus) & age=="All", Male_est, NA), na.rm = TRUE),
    pop_m_se  = se_sum(ifelse(is.na(lfstatus) & age=="All", Male_moe, NA)),
    lf_m_est  =    sum(ifelse((lfstatus == "In labor force"), Male_est, NA), na.rm = TRUE),
    lf_m_se   = se_sum(ifelse((lfstatus == "In labor force"), Male_moe, NA)), 
    emp_m_est =    sum(ifelse((lftype == "In Armed Forces" | empstatus == "Employed"), Male_est, NA), na.rm = TRUE),
    emp_m_se  = se_sum(ifelse((lftype == "In Armed Forces" | empstatus == "Employed"), Male_moe, NA))) %>%
  mutate(
    employrate_f_est   = emp_f_est/lf_f_est,
    employrate_f_se    = se_proportion(emp_f_est, lf_f_est, emp_f_se, lf_f_se),
    employrate_f_count = lf_f_est,
    lfrate_f_est       = ifelse(pop_f_est!=0, lf_f_est/pop_f_est, NA),
    lfrate_f_se        = se_proportion(lf_f_est, pop_f_est, lf_f_se, pop_f_se),
    lfrate_f_count     = pop_f_est,
    employrate_m_est   = emp_m_est/lf_m_est,
    employrate_m_se    = se_proportion(emp_m_est, lf_m_est, emp_m_se, lf_m_se),
    employrate_m_count = lf_m_est,
    lfrate_m_est       = ifelse(pop_m_est!=0, lf_m_est/pop_m_est, NA),
    lfrate_m_se        = se_proportion(lf_m_est, pop_m_est, lf_m_se, pop_m_se),
    lfrate_m_count     = pop_m_est
  ) %>%
  dplyr::select(-pop_f_est, -pop_f_se, -lf_f_est, -lf_f_se, -emp_f_est, -emp_f_se, 
         -pop_m_est, -pop_m_se, -lf_m_est, -lf_m_se, -emp_m_est, -emp_m_se )

acs_emp_age25to34 <- 
  acs5_B23001 %>%
  gather(stat, val, est, moe) %>%
  unite(colname, gender, stat) %>%
  spread(colname, val) %>%
  filter(age %in% c("25to29", "30to34")) %>% 
  group_by(source, endyear, geo, geo_val, NAME) %>% 
  # NSM: could consider making a sumNA <- function(...) sum(..., na.rm = TRUE)
  summarize( 
    ## Collapse across age categories
    pop_f_a2534_est =    sum(Female_est, na.rm = TRUE),
    pop_f_a2534_se  = se_sum(Female_moe),
    lf_f_a2534_est  =    sum(ifelse((lfstatus == "In labor force"), Female_est, NA), na.rm = TRUE),
    lf_f_a2534_se   = se_sum(ifelse((lfstatus == "In labor force"), Female_moe, NA)), 
    emp_f_a2534_est =    sum(ifelse((lftype == "In Armed Forces" | empstatus == "Employed"), Female_est, NA), na.rm = TRUE),
    emp_f_a2534_se  = se_sum(ifelse((lftype == "In Armed Forces" | empstatus == "Employed"), Female_moe, NA)),
    pop_m_a2534_est =    sum(Male_est, NA, na.rm = TRUE),
    pop_m_a2534_se  = se_sum(Male_moe),
    lf_m_a2534_est  =    sum(ifelse((lfstatus == "In labor force"), Male_est, NA), na.rm = TRUE),
    lf_m_a2534_se   = se_sum(ifelse((lfstatus == "In labor force"), Male_moe, NA)), 
    emp_m_a2534_est =    sum(ifelse((lftype == "In Armed Forces" | empstatus == "Employed"), Male_est, NA), na.rm = TRUE),
    emp_m_a2534_se  = se_sum(ifelse((lftype == "In Armed Forces" | empstatus == "Employed"), Male_moe, NA))) %>%
  mutate(
    employrate_f_a2534_est   = emp_f_a2534_est/lf_f_a2534_est,
    employrate_f_a2534_se    = se_proportion(emp_f_a2534_est, lf_f_a2534_est, emp_f_a2534_se, lf_f_a2534_se),
    employrate_f_a2534_count = lf_f_a2534_est,
    lfrate_f_a2534_est       = ifelse(pop_f_a2534_est!=0, lf_f_a2534_est/pop_f_a2534_est, NA),
    lfrate_f_a2534_se        = se_proportion(lf_f_a2534_est, pop_f_a2534_est, lf_f_a2534_se, pop_f_a2534_se),
    lfrate_f_a2534_count     = pop_f_a2534_est,
    employrate_m_a2534_est   = emp_m_a2534_est/lf_m_a2534_est,
    employrate_m_a2534_se    = se_proportion(emp_m_a2534_est, lf_m_a2534_est, emp_m_a2534_se, lf_m_a2534_se),
    employrate_m_a2534_count = lf_m_a2534_est,
    lfrate_m_a2534_est       = ifelse(pop_m_a2534_est!=0, lf_m_a2534_est/pop_m_a2534_est, NA),
    lfrate_m_a2534_se        = se_proportion(lf_m_a2534_est, pop_m_a2534_est, lf_m_a2534_se, pop_m_a2534_se),
    lfrate_m_a2534_count     = pop_m_a2534_est
  ) %>%
  dplyr::select(-pop_f_a2534_est, -pop_f_a2534_se, -lf_f_a2534_est, -lf_f_a2534_se, -emp_f_a2534_est, -emp_f_a2534_se, 
         -pop_m_a2534_est, -pop_m_a2534_se, -lf_m_a2534_est, -lf_m_a2534_se, -emp_m_a2534_est, -emp_m_a2534_se )


# Check summary statistics.

exclude_vars <- names(acs_emp) %in% c("source", "endyear", "geo", "geo_val", "NAME")
summary(acs_emp[!exclude_vars])

  ## /!\ There are a few PUMAs where lf participation rate > 1.
  ## I print these here, but I make no effort to correct
  ## these errors since they are the product of errors in the raw ACS data:
  
  filter(acs_emp, 
         (lfrate_f_est>1) | (lfrate_m_est>1)) %>% 
    as.data.frame()
  
  ## /!\ There are also cases with 0 labor force counts that need to be investigated
  
  filter(acs_emp, (lfrate_f_count == 0 | lfrate_m_count == 0)) %>% as.data.frame()

# Check NA's
acs_emp %>% 
  ungroup() %>% 
  filter(is.na(employrate_f_est) | is.na(employrate_m_est)) %>% 
  select(geo_val) %>% 
  unique()
  # NA's for 8 geo_val (17031381700, 17097863006, 17031980000, 17197980000, 17201980000, 17031980100, 17031990000, 17097990000)
  # Similar tracts to missing counts in `acs_incpov`

# ------------------------------------ #
# Education level by gender (by race/ethnicity)
# ------------------------------------ #

# /!\ NSM: Easier if data were reshaped?
acs_educ <- list()
for (gender_value in c("Female", "Male")) {
  
  gender_stub <- switch(gender_value, 
                        "Female" = "f",
                        "Male"   = "m")
    
  # Extract female race/ethnicity -by- education category variables.
  
  acs_educfem <-
    acs5_C15002 %>% 
    filter(gender == gender_value) %>%
    pivot_longer(cols = c(est, moe),
                 names_to = "stat",
                 values_to = "val") %>%
    unite(colname, raceeth, educ, stat) %>%
    pivot_wider(names_from = colname, 
                values_from = val)
    
  # acs_educfem <-
  #   acs5_C15002 %>%  
  #   filter(gender == gender_value) %>%
  #   gather(stat, val, est, moe) %>%
  #   unite(colname, raceeth, educ, stat) %>%
  #   spread(colname, val)
  
  # Construct "all" variables for each race (sum across education categories). 
  # I construct these from scratch instead of using the
  # "all" variables in the raw data because I found several disrepancies between the "all"
  # variables and the sum of the component variables for the "all" variables. These discrepancies 
  # led to some share variables exceeding one in value.
  
  for (r in c("amind", "bl", "wh", "pacisl", "mult", "oth", "h", "wh_nonh", "as")) {
    
    ## Prepare variable names
    
    newvar_est <- paste0(r, "_all_est")
    newvar_moe <- paste0(r, "_all_moe")
    
    ## Construct new race total variables
    
    acs_educfem <- 
      acs_educfem %>%
      mutate(
        new_var1 = (eval(parse(text = paste0(r, "_lesshs_moe")))/1.645)^2,
        new_var2 = (eval(parse(text = paste0(r, "_hsgrad_moe")))/1.645)^2,
        new_var3 = (eval(parse(text = paste0(r, "_somecoll_moe")))/1.645)^2,
        new_var4 = (eval(parse(text = paste0(r, "_coll_moe")))/1.645)^2,
        ) %>%
      mutate(
        !!newvar_est := rowSums(dplyr::select(., paste0(r, "_lesshs_est"), 
                                          paste0(r, "_hsgrad_est"), 
                                          paste0(r, "_somecoll_est"), 
                                          paste0(r, "_coll_est")), na.rm = FALSE),
        !!newvar_moe := 1.645 * sqrt(rowSums(dplyr::select(., "new_var1", "new_var2", "new_var4", "new_var4"), na.rm = FALSE))
        ) %>%
      dplyr::select(-"new_var1", -"new_var2", -"new_var3", -"new_var4")
    }
  
  # Construct female education category variables (as share of female universe).
  
    ##  For some reason that I have not been able to pin down, I cannot extract aggregate (not by-race) education variables directly 
    ##  from ACS - adding this table to the metadata produces an error in the previous section of code. For now, I construct aggregate
    ##  variables from by-race variables.
  
  for (g in c("lesshs", "hsgrad", "somecoll", "coll")) {
    
    ## Prepare variable names
    
    as    <- paste0("as_", g, "_est")
    amind <- paste0("amind_", g, "_est")
    bl    <- paste0("bl_", g, "_est")
    wh    <- paste0("wh_", g, "_est")
    pacisl<- paste0("pacisl_", g, "_est")
    mult  <- paste0("mult_", g, "_est")
    oth   <- paste0("oth_", g, "_est")
    
    as_moe    <- paste0("as_", g, "_moe")
    amind_moe <- paste0("amind_", g, "_moe")
    bl_moe    <- paste0("bl_", g, "_moe")
    wh_moe    <- paste0("wh_", g, "_moe")
    pacisl_moe<- paste0("pacisl_", g, "_moe")
    mult_moe  <- paste0("mult_", g, "_moe")
    oth_moe   <- paste0("oth_", g, "_moe")
    
    newvar      <- paste0(gender_stub, "_", g, "_est")
    newvar_se   <- paste0(gender_stub, "_", g, "_se")
    newvar_count<- paste0(gender_stub, "_", g, "_count")
    
    ## Construct education category shares
    
    acs_educfem <-
      acs_educfem %>%
      mutate(
        ### Prepare variances
        as_var    = (eval(parse(text=as_moe))/1.645)^2,
        amind_var = (eval(parse(text=amind_moe))/1.645)^2,
        bl_var    = (eval(parse(text=bl_moe))/1.645)^2,
        wh_var    = (eval(parse(text=wh_moe))/1.645)^2,
        pacisl_var = (eval(parse(text=pacisl_moe))/1.645)^2,
        mult_var   = (eval(parse(text=mult_moe))/1.645)^2,
        oth_var    = (eval(parse(text=oth_moe))/1.645)^2,
        as_all_var = (as_all_moe/1.645)^2,
        amind_all_var = (amind_all_moe/1.645)^2,
        bl_all_var    = (bl_all_moe/1.645)^2,
        wh_all_var    = (wh_all_moe/1.645)^2,
        pacisl_all_var = (pacisl_all_moe/1.645)^2,
        mult_all_var   = (mult_all_moe/1.645)^2,
        oth_all_var    = (oth_all_moe/1.645)^2) %>%
      mutate(
        ### Construct new variables
        !!newvar := ifelse(rowSums(dplyr::select(., "as_all_est", "amind_all_est", "bl_all_est", 
                                          "wh_all_est", "pacisl_all_est", "mult_all_est", 
                                          "oth_all_est"), na.rm = FALSE)!=0,
                            rowSums(dplyr::select(., as, amind, bl, wh, pacisl, mult, oth), na.rm = FALSE) / 
                            rowSums(dplyr::select(., "as_all_est", "amind_all_est", "bl_all_est", 
                                                "wh_all_est", "pacisl_all_est", "mult_all_est", 
                                                "oth_all_est"), na.rm = FALSE),
                            NA),
        !!newvar_se := se_proportion(rowSums(dplyr::select(., as, amind, bl, wh, pacisl, mult, oth), na.rm = FALSE), 
                                     rowSums(dplyr::select(., "as_all_est", "amind_all_est", "bl_all_est", 
                                                       "wh_all_est", "pacisl_all_est", "mult_all_est", 
                                                       "oth_all_est"), na.rm = FALSE),
                                     sqrt(rowSums(dplyr::select(., as_var, amind_var, bl_var, wh_var, pacisl_var, mult_var, oth_var), na.rm = FALSE)),
                                     sqrt(rowSums(dplyr::select(., as_all_var, amind_all_var, bl_all_var, wh_all_var, pacisl_all_var, mult_all_var, oth_all_var), na.rm = TRUE))
                                     ),
        !!newvar_count := rowSums(dplyr::select(., "as_all_est", "amind_all_est", "bl_all_est", 
                                         "wh_all_est", "pacisl_all_est", "mult_all_est", 
                                         "oth_all_est"))) %>%
      dplyr::select(-as_var, -amind_var, -bl_var, -wh_var, -pacisl_var, -mult_var, -oth_var, 
             -as_all_var, -amind_all_var, -bl_all_var, -wh_all_var, -pacisl_all_var, -mult_all_var, -oth_all_var)
  
  } 
  
  # Construct female education-by-race variables (where each share represents the share within the specified race
  # category that belong to the specified education category).
  
  for (r in c("amind", "bl", "wh", "pacisl", "mult", "oth", "h", "wh_nonh", "as")){
    for (g in c("lesshs", "hsgrad", "somecoll", "coll")){
  
    ## Prepare new variable names
      
    num          <- paste0(r,"_",g, "_est")
    denom        <- paste0(r, "_all_est")
    num_moe      <- paste0(r,"_",g, "_moe")
    denom_moe    <- paste0(r, "_all_moe")
    newvar       <- paste0(gender_stub, "_", g, "_", r ,"_est")
    newvar_se    <- paste0(gender_stub, "_", g, "_", r ,"_se")
    newvar_count <- paste0(gender_stub, "_", g, "_", r, "_count")
    
    ## Construct new variables
  
    acs_educfem <-
    acs_educfem %>%
      mutate( !!newvar       := ifelse(eval(parse(text=denom))!=0,
                                       eval(parse(text=num))/eval(parse(text=denom)), 
                                       NA),
              !!newvar_se    := se_proportion(eval(parse(text=num)), 
                                              eval(parse(text=denom)),
                                              eval(parse(text=num_moe))/1.645, 
                                              eval(parse(text=denom_moe))/1.645),
              !!newvar_count := eval(parse(text=denom))
              )
  
  }
  }
  
  # Remove count variables from final data set.
  
  acs_educfem <-
    acs_educfem %>%
    dplyr::select(-amind_all_est, -amind_all_moe, -amind_lesshs_est, -amind_lesshs_moe,
           -amind_hsgrad_est, -amind_hsgrad_moe, -amind_somecoll_est, -amind_somecoll_moe, -amind_coll_est, -amind_coll_moe,
           -bl_all_est, -bl_all_moe, -bl_lesshs_est, -bl_lesshs_moe,
           -bl_hsgrad_est, -bl_hsgrad_moe, -bl_somecoll_est, -bl_somecoll_moe, -bl_coll_est, -bl_coll_moe,
           -wh_all_est, -wh_all_moe, -wh_lesshs_est, -wh_lesshs_moe,
           -wh_hsgrad_est, -wh_hsgrad_moe, -wh_somecoll_est, -wh_somecoll_moe, -wh_coll_est, -wh_coll_moe,
           -pacisl_all_est, -pacisl_all_moe, -pacisl_lesshs_est, -pacisl_lesshs_moe,
           -pacisl_hsgrad_est, -pacisl_hsgrad_moe, -pacisl_somecoll_est, -pacisl_somecoll_moe, -pacisl_coll_est, -pacisl_coll_moe,
           -mult_all_est, -mult_all_moe, -mult_lesshs_est, -mult_lesshs_moe,
           -mult_hsgrad_est, -mult_hsgrad_moe, -mult_somecoll_est, -mult_somecoll_moe, -mult_coll_est, -mult_coll_moe,
           -oth_all_est, -oth_all_moe, -oth_lesshs_est, -oth_lesshs_moe,
           -oth_hsgrad_est, -oth_hsgrad_moe, -oth_somecoll_est, -oth_somecoll_moe, -oth_coll_est, -oth_coll_moe,
           -h_all_est, -h_all_moe, -h_lesshs_est, -h_lesshs_moe,
           -h_hsgrad_est, -h_hsgrad_moe, -h_somecoll_est, -h_somecoll_moe, -h_coll_est, -h_coll_moe,
           -wh_nonh_all_est, -wh_nonh_all_moe, -wh_nonh_lesshs_est, -wh_nonh_lesshs_moe,
           -wh_nonh_hsgrad_est, -wh_nonh_hsgrad_moe, -wh_nonh_somecoll_est, -wh_nonh_somecoll_moe, -wh_nonh_coll_est, -wh_nonh_coll_moe,
           -as_all_est, -as_all_moe, -as_lesshs_est, -as_lesshs_moe,
           -as_hsgrad_est, -as_hsgrad_moe, -as_somecoll_est, -as_somecoll_moe, -as_coll_est, -as_coll_moe, -gender)
  
  acs_educ[[gender_value]] <- acs_educfem
  
}
# /!\ Note -- the code is generating a informative message of:
  # Note: Using an external vector in selections is ambiguous.
  # i Use `all_of(as)` instead of `as` to silence this message.
  # i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
  # This message is displayed once per session.

acs_educ <- merge(acs_educ[["Female"]], 
                  acs_educ[["Male"]],
                  by = c("source", "endyear", "geo", "geo_val", "NAME"), 
                  all.x = TRUE, 
                  all.y = TRUE)

# Check summary statistics.

exclude_vars <- names(acs_educfem) %in% c("source", "endyear", "geo", "geo_val", "NAME")
summary(acs_educfem[!exclude_vars])

sapply(dplyr::select(acs_educfem, matches("est$")), 
       pct_nonmiss) %>% 
  #hist()
  sort() %>% round(2)
  # /!\ There are still some fields--primarily with smaller race groups--that
  # are missing many estimates, presumably due to low denominator counts

# ------------------------------------ #
# Density in population profile by race/ethnicity
# ------------------------------------ #

# Extract race/ethnicity variables.

acs_raceeth <-
  acs5_B03002 %>%
  gather(stat, val, est, moe) %>%
  unite(colname, raceeth, stat) %>%
  spread(colname, val) 
  
for (r in c("amind", "as", "bl", "mult", "oth", "pacisl", "wh")){
  
  ## Prepare variable names
  
  num          <- paste0(r,"_h_est")
  denom        <- "all_all_est"
  num_moe      <- paste0(r,"_h_moe")
  denom_moe    <- "all_all_moe"
  num_nonh     <-  paste0(r,"_nonh_est")
  num_nonh_moe <- paste0(r,"_nonh_moe")
  
  newvar       <- paste0("raceeth_", r, "_h_est")
  newvar_se    <- paste0("raceeth_", r, "_h_se")
  newvar_count <- paste0("raceeth_", r, "_h_count")
  
  newvar_nonh       <-paste0("raceeth_", r, "_nonh_est")
  newvar_nonh_se    <- paste0("raceeth_", r, "_nonh_se")
  newvar_nonh_count <- paste0("raceeth_", r, "_nonh_count")
  
  ## Construct new variables
  
  acs_raceeth <-
    acs_raceeth %>%
    mutate( !!newvar    := eval(parse(text=num))/eval(parse(text=denom)),
            !!newvar_se := se_proportion(eval(parse(text=num)), 
                                         eval(parse(text=denom)),
                                         eval(parse(text=num_moe))/1.645, 
                                         eval(parse(text=denom_moe))/1.645),
            !!newvar_count := eval(parse(text=denom)),
            !!newvar_nonh    := eval(parse(text=num_nonh))/eval(parse(text=denom)),
            !!newvar_nonh_se := se_proportion(eval(parse(text=num_nonh)), 
                                              eval(parse(text=denom)),
                                              eval(parse(text=num_nonh_moe))/1.645, 
                                              eval(parse(text=denom_moe))/1.645),
            !!newvar_nonh_count := eval(parse(text=denom)))
}
  
acs_raceeth <-
  acs_raceeth %>%
  dplyr::select(., 1:5, contains("raceeth"))

# Check summary statistics.

exclude_vars <- names(acs_raceeth) %in% c("source", "endyear", "geo", "geo_val", "NAME")
summary(acs_raceeth[!exclude_vars])

# ---------------------------------------------------------------------------- #
# Grandparents living with grandchildren (share of households)
# ---------------------------------------------------------------------------- #

# Extract variables of interest, and construct new share variables.

acs_grandparents <-
  acs5_B10063 %>% 
  gather(stat, val, est, moe) %>%
  unite(colname, hhtype, stat) %>%
  spread(colname, val) %>%
  mutate(
    gpresp_est   = GCR_est/All_est,
    gpresp_se    = se_proportion(GCR_est, All_est, GCR_moe/1.645, All_moe/1.645),
    gpresp_count = All_est) %>%
  dplyr::select(., 1:5, contains("gpresp"))
# Check summary statistics.

exclude_vars <- names(acs_grandparents) %in% c("source", "endyear", "geo", "geo_val", "NAME")
summary(acs_grandparents[!exclude_vars])

# ---------------------------------------------------------------------------- #
# Percent of families at or below federal poverty line
# ---------------------------------------------------------------------------- #

# Extract variables of interest, and construct new variables.

acs_povline <-
  acs5_B17012 %>%
  filter(is.na(fam_type)) %>% 
  gather(stat, val, est, moe) %>%
  unite(colname, hhpov, stat) %>%
  spread(colname, val) %>%
  mutate(
    belowpov_est   = BelowPov_est/All_est,
    belowpov_se    = se_proportion(BelowPov_est, All_est, BelowPov_moe/1.645, All_moe/1.645),
    belowpov_count = All_est) %>%
  dplyr::select(., 1:5, contains("belowpov_"), -BelowPov_est, -BelowPov_moe)

# Check summary statistics.

exclude_vars <- names(acs_povline) %in% c("source", "endyear", "geo", "geo_val", "NAME")
summary(acs_povline[!exclude_vars])

# ---------------------------------------------------------------------------- #
# Percent of households with children by family type (married, or female- or male-only)
# ---------------------------------------------------------------------------- #

# Extract variables of interest, and construct new variables.
# /!\ Note -- by adding `hhpov` back to the `group_by()` it would be possible to
# calculate family type by poverty status. For now, however, we focus on just
# family type.

acs_famtype <-
  acs5_B17012 %>%
  filter(!is.na(fam_type)) %>% 
  group_by(source, endyear, geo, geo_val, NAME, fam_type) %>% # hhpov, 
  summarize(est = sumNA(est),
            se = se_sum(moe)) %>% 
  ungroup() %>% 
  gather(stat, val, est, se) %>%
  unite(colname, fam_type, stat) %>% # hhpov, 
  spread(colname, val) %>%
  mutate(
    famtype_tot_est =      Female_noSp_est  + Male_noSp_est  + Married_est,
    famtype_tot_se  = sqrt(Female_noSp_se^2 + Male_noSp_se^2 + Married_se^2),
    
    pctFemale_noSp_est = Female_noSp_est / famtype_tot_est,
      pctMale_noSp_est =   Male_noSp_est / famtype_tot_est,
        pctMarried_est =     Married_est / famtype_tot_est,
    
    pctFemale_noSp_se = se_proportion(Female_noSp_est, famtype_tot_est, Female_noSp_se, famtype_tot_se),
      pctMale_noSp_se = se_proportion(  Male_noSp_est, famtype_tot_est,   Male_noSp_se, famtype_tot_se),
        pctMarried_se = se_proportion(    Married_est, famtype_tot_est,     Married_se, famtype_tot_se)) %>%
  dplyr::select(., 1:5, matches("^pct"))

# Check summary statistics.

exclude_vars <- names(acs_famtype) %in% c("source", "endyear", "geo", "geo_val", "NAME")
summary(acs_famtype[!exclude_vars])

# ---------------------------------------------------------------------------- #
# Combine tables and export ----------------------------------------------------
# ---------------------------------------------------------------------------- #

# Append all tables.

acs_final <- 
  acs_timetransport %>% 
  full_join(acs_modetransport) %>%
  full_join(acs_emp) %>%
  full_join(acs_emp_age25to34) %>% 
  full_join(acs_raceeth) %>%
  full_join(acs_grandparents) %>%
  full_join(acs_incpov_age_out) %>%
  full_join(acs_incpov) %>%
  full_join(acs_povline) %>%
  full_join(acs_famtype) %>%
  full_join(acs_childpop) %>%
  full_join(acs_age) %>%
  full_join(acs_educ)

# Produce 5-year ACS (tract-level) file.

acs_final5 <-
  acs_final %>%
  filter(source == "acs5")

#save(acs_final5, 
#     file = glue("{output_path}acs5_variables.Rda"))
write.csv(acs_final5, glue("{output_path}acs5_variables_{my_output_tag}.csv"))

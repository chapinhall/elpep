#--------------------------------------------------------------------#

# This code downloads ACS data from the Census Bureau, and constructs community-level
# variables for race and other characteristics. This code is adapted from code shared by
# Nick Mader ("develop-new-estimate-methods.Rmd").

# AUTHOR : Hyein Kang and Nick Mader
# DATE CREATED : 2022-11-19

# This code requires an internet connection (and therefore may need to be run from
# personal computers, and not high security servers that are not connected to the internet)

# See the below links for references to relevant package and data sources
## https://walker-data.com/tidycensus/reference/get_pums.html
## https://cran.r-project.org/web/packages/tidycensus/tidycensus.pdf - see p.8
## https://www.census.gov/programs-surveys/acs/microdata/documentation.html

#--------------------------------------------------------------------#


# Note: this code is automatically run from the `run--01b--prep-acs1-data.Rmd`
# script if ACS1 data has not yet been pulled for the current run specification.
# If desiring to run this manually, run the following scripts to load relevant
# libraries and functions:
# - settings--main.R
# - settings--config.R
# - method--general-helper-functions.R

#list necessary variables
rel_var <- ifelse(base_year <= 2018,
                  "RELP",
                  "RELSHIPP")

st_var <- ifelse(base_year <= 2022,
                 "ST",
                 "STATE")
pull_vars <- 
  c("SERIALNO", st_var, "PUMA", "FS", "SPORDER", rel_var, "SEX", "AGEP", 
    "RAC2P", "HISP", "SCH", "SCHL", "ESR", "COW", "OCCP", "INDP", "FINCP", 
    "POVPIP", "TYPEHUGQ")

# View all available variables for acs1
if (FALSE) {
  my_pums <- 
    pums_variables %>% 
    filter(year == base_year,
           survey == "acs1"
    )
  View(my_pums)
}

# Print variables already selected
if (FALSE) {
  my_pums %>% 
    filter(var_code %in% pull_vars)
}

# Pull ACS data
acs1 <- 
  get_pums(variables = pull_vars,
           state     = my_state_abbr, #'all' for all states
           year      = base_year,
           survey    = "acs1",
           recode    = TRUE,
           key       = census_key)

# Quick examination of fields
if (FALSE) {
  for (v in setdiff(str_subset(cn(acs1), "_label$"), "ST_label")) {
    cat(v)
    print(table(acs1[[v]]))
  }
}

#creating variables
acs1$YEAR <- base_year
#acs1$SAMPLE <- glue("{base_year}01")
#acs1$STRATA <- str_c(acs1$PUMA, acs1$ST)

acs1 <- 
  acs1 %>% 
  mutate(
    TYPEHUGQ = as.numeric(TYPEHUGQ),
    
    SERIALNO = SERIALNO %>% 
      str_replace("HU", "00") %>%
      str_replace("GQ", "01") %>%
      as.numeric(),

    WGTP     = as.numeric(WGTP),
    AGEP     = as.numeric(AGEP),
    PUMA     = as.numeric(PUMA),
    st_var   = as.numeric(get(st_var)),
    FS       = as.numeric(FS),
    rel_var  = as.numeric(get(rel_var)),
    SEX      = as.numeric(SEX),
    RAC2P    = as.numeric(RAC2P),
    HISP     = as.numeric(HISP),
    INDP     = as.numeric(INDP),
    #SAMPLE   = as.numeric(SAMPLE),
    #STRATA   = as.numeric(STRATA),
    SCHL     = recode(SCHL,
                      `bb` = "0"),
    SCHL     = as.numeric(SCHL),
    SCH      = recode(SCH,
                      `b` = "0"),
    SCH      = as.numeric(SCH),
    ESR      = recode(ESR,
                      `b` = "0"),
    ESR      = as.numeric(ESR),
    COW      = recode(COW,
                      `b` = "0"),
    COW      = as.numeric(COW),
    POVPIP   = recode(as.numeric(POVPIP),
                      `0` = 1,
                      `-1` = 0)
  ) %>% 
  data.table()

write.csv(acs1, file = glue("{input_path}census_acs1_{base_year}_{my_output_tag}.csv"))


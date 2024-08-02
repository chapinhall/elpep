### Read parameters from an input interface ------------------------------------

if (!exists("elpep_customization_file")) {
  stop(paste0(
    "To run this code, you need to specify the name of an Excel interface file ",
    "that specifies necessary details for your run. This can be done by ",
    "going to the project repository https://www.github.com/chapinhall/elpep/ ",
    "to \n\n",
    "(1) download the 'Interface' file,\n",
    "(2) to download the full codebase (which you likely have done if you are ",
    "at this stage, and\n",
    "(3) open the `run-elpep.Rmd` script to specify `elpep_customization_file <- '/path/to/file/interface.xlsm'"))
}


### Function for harvesting customizations from the Excel spec file -----------#
  
getRange <- function(file, range, istable = FALSE, header = TRUE) {
  if (istable) {
    val <- read.xlsx(file, namedRegion = range, colNames = header)
  } else {
    val <- read.xlsx(file, namedRegion = range, colNames = FALSE)[, 1]
  }
  return(val)
}
getInfo <- function(i, ...) getRange(file = elpep_customization_file, i, ...)


### Read all customization parameters from the Excel spec file ----------------#

elpep_spec_names <- getNamedRegions(elpep_customization_file)

elpep_params <- NULL
for (i in 1:length(elpep_spec_names)) {
  esn <- elpep_spec_names[i]
  
  istable <- 
    str_detect(
      attr(elpep_spec_names, "position")[i], 
      "\\w+\\d+:\\w+\\d+")
  
  suppressWarnings(assignment_try <- try(assign(esn, getInfo(esn, istable = istable))))
  
  # Delete objects that are NAs, to conform with the rest of the codebase that
  # typically checks `exists(<object name>)` to see if default handling should be
  # used.
  if (is.null(assignment_try) ||
      (all(is.na(get(esn))) || 
       all(is.null(get(esn))))) {
         rm(list = esn)
       }
}


### Set years for pulling data ------------------------------------------------#

if (!exists("base_year")) base_year <- 2022
base_year <- min(base_year, 2022)

acs5_year <- min(base_year + 2, 2022)


### Fix potential issues with file paths --------------------------------------#

fix_path <- function(x) { 
  x %>% 
    # Replace backslashes with forward slashes
    str_replace_all("\\\\+", "/") %>% 
    # Ensure final forward-slash
    paste0("/") %>% 
    # Remove duplicate forward slashes
    str_replace_all("/+", "/") 
}
code_path   <- fix_path(code_path)
input_path  <- fix_path(input_path)
output_path <- fix_path(output_path)


### Clean specific values -----------------------------------------------------#

census_key <- str_trim(census_key)
fred_key   <- str_trim(fred_key)

### Recast to correct data types ----------------------------------------------#

inputs_to_numericize <- c("kid_age_thres_p")
for (i in inputs_to_numericize) {
  assign(i, as.numeric(get(i)))
}


### recode true and false -----------------------------------------------------#

inputs_tf <- c("use_only_sae_model_estimates",
               "developer_mode",
               "rerun_sae")

for (i in inputs_tf) {
  if (!get(i) %in% c(TRUE, FALSE)) {
    assign(i,
           case_match(get(i),
                      "True"  ~ TRUE,
                      "False" ~ FALSE,
                      TRUE    ~ NA)) 
  }
}


### vectorize comma-separated inputs ------------------------------------------#

inputs_to_vectorize <- 
  c("local_ccdf_incratio_cuts",
    "output_fpl_cuts")

for (i in inputs_to_vectorize) {
  assign(i,
         str_split(get(i), pattern = ",") %>% unlist() %>%  trim() %>% as.numeric())
}


### check validity of custom income inputs ------------------------------------#

# If nothing has been provided, delete the table so that code handles it as missing
if (all(is.na(custom_income_thresh$inc_thresh))) {
  rm(custom_income_thresh)
} else {
  custom_income_thresh <- 
    custom_income_thresh %>% 
    filter(!is.na(inc_thresh))
}

### prepare inputs for age aggregation ----------------------------------------#

kindergarten_cutoff <- 
  ifelse(exists("kindergarten_date_cutoff"), 
         kindergarten_date_cutoff,
         "09-01")

end_month_pre_schoolage <- 
  (interval(ymd(glue("{base_year+1}-01-01")),
            ymd(glue("{base_year+1}-{kindergarten_cutoff}"))) %/%
     days(1) /  # Get difference in days
     30.5 +     # Get difference in (roughly) monthly
     60)        # Add to number of months up until age 5
  # Note: this does not produce a round number, which is fine for later 
  # calculations, which prorate the counts of youth by ages and are already in
  # decimal (not integer) values

# Round to the nearest half month
end_month_pre_schoolage <- 
  round(end_month_pre_schoolage/0.5)*0.5

prep_age_month <- function(x, is_high_month = FALSE) {
  # We subtract 1 from the "low month" replacements to the kindergarten cutoff 
  # because we want to avoid overlap.
  # ensures that 
  x[str_detect(x, "kind")] <- end_month_pre_schoolage - 1*(is_high_month)
  as.numeric(x)
}

age_aggs <- 
  age_aggs %>% 
  mutate(low_month  = prep_age_month(low_month),
         high_month = prep_age_month(high_month, is_high_month = TRUE))


### prepare inputs for map generation -----------------------------------------#

if (exists("map_geos")) {
  map_geos <- 
    map_geos %>% 
    str_split(pattern = ",") %>% 
    unlist() %>% 
    str_trim()  
}

if (exists("map_geo_level")) {
  map_geo_level <-
    case_match(map_geo_level,
               "School District(s)"                      ~ "school",
               "Zip Code(s)"                             ~ "zcta",
               "County (or Counties)"                    ~ "county",
               "Custom Geography (to be provided below)" ~ "aux")
}

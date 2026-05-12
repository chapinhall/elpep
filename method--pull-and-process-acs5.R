#------------------------------------------------------------------------------#
### Load relevant packages -----------------------------------------------------
#------------------------------------------------------------------------------#

# This code also installs packages if necessary

packages.list <- c("dplyr", "tidyr", "data.table", "tidycensus", "stringr", "glue")
for (p in packages.list) {
  if (!p %in% installed.packages()[, "Package"]) install.packages(p)
  library(p, 
          character.only = TRUE,
          quietly = TRUE)
}

#------------------------------------------------------------------------------#
### Small helper functions -----------------------------------------------------
#------------------------------------------------------------------------------#

cn <- function(x) colnames(x)
sub_cn <- function(x, pattern) str_subset(cn(x), pattern)
ep <- function(x) eval(parse(text = x))

#------------------------------------------------------------------------------#
### Pull Census data -----------------------------------------------------------
#------------------------------------------------------------------------------#

pull_table <- function(table_name, 
                       geography = "tract", 
                       state = "IL", 
                       county = "Cook", 
                       survey = "acs5", 
                       year = 2021,
                       cache_table = TRUE,
                       add_meta = TRUE) {
  
  table_pull <- 
    get_acs(table = table_name,
            geography = geography,
            state = state,
            county = county,
            survey = survey, 
            year = year,
            cache_table = cache_table) %>% 
    select(-NAME) %>% 
    mutate(se = moe / 1.645)
  
  # Develop and apply metadata
  if (add_meta) {
    my_meta <- 
      get_acs5_metadata(year)
    
    if (FALSE) {
      my_meta <- 
        get_acs5_metadata(year) %>% 
        filter(str_detect(name, table_name))
    }
    
    table_pull <-
      merge(table_pull,
            my_meta %>% 
              filter(str_detect(name, table_name)) %>% 
              develop_meta() %>% 
              select(-label, -concept) %>% 
              rename(variable = name),
            by = "variable",
            all.x = TRUE)
  }
  
  return(table_pull)
}

# /!\ Make sure that there's an way easier to read/reference name for the concept
#     which is added to the data, e.g. "Grandparents Living with Own Grandchildren
#     Under 18 Years by Responsibility for Own Grandchildren by Presence of Parent"
#     of Grandchildren and Age of Grandparent" has to get cut down. Can do that
#     via 

# Apply these to the meta data, and merge that information (in left join)
# to each of the tables

#------------------------------------------------------------------------------#
### Develop Meta Data ----------------------------------------------------------
#------------------------------------------------------------------------------#

check_meta <- function(year, table_name, survey = "acs5") {
  load_variables(year, "acs5") %>% 
    filter(str_detect(name, table_name))
}

develop_meta <- function(table_meta, verbose = FALSE) {
  
  if (verbose) print(glue("Working on table {table_meta$name[1]}"))
  
  # This function is developed for recoding a single table
  
  table_meta <- 
    table_meta %>% 
    mutate(
      table = str_replace(name, "(\\w\\d+)\\w?_.+", "\\1")
    ) 
  
  if (n_distinct(table_meta$table) > 1) stop("Expected only a single census table")
  
  my_concepts <- str_to_upper(unique(table_meta$concept))
  if (length(my_concepts) > 1) message("Encountered multiple concepts for the given table")
  # Note: this is possible e.g. in the case of a table that has letter suffixes
  # which are specific to race/ethnicity subpopulations that do not always have
  # the same level of detail, and that different concept descriptions.
  # See e.g. table B10051
  
  # Recode Sex -----------------------------------------------------------------
  if (any(str_detect(my_concepts, "SEX"))) {
    table_meta <-
      table_meta %>% 
      mutate(sex = case_when(str_detect(label, "Male")   ~ "Male",
                             str_detect(label, "Female") ~ "Female") %>% 
               replace_na("All"))
  }
  
  # Recode Age -----------------------------------------------------------------
  if (any(str_detect(my_concepts, "(^| )AGE( |$)"))) {
    #table_meta <- filter(my_meta, str_detect(concept, "AGE"))
    table_meta <-
      table_meta %>% 
      # Note, we include "years" in each string to avoid matches with other
      # fields with numeric ranges, e.g. income to poverty ratio of "0.50 to 0.75"
      mutate(age = 
               label %>% 
               str_replace(".*!!(\\d+) (to|and) (\\d+) years.*", "Age\\1to\\3") %>% 
               str_replace(".*!!Under (\\d+) years.*",           "Age0to\\1") %>% 
               str_replace(".*!!(\\d+) years and over years.*",  "AgeGe\\1") %>% 
               str_replace(".*!!(\\d+) years.*",                 "Age\\1to\\1") %>% 
               str_replace("Estimate.+",                         "All"))
    if (any(str_detect(my_concepts, "AGE OF GRANDPARENT"))) {
      table_meta <- rename(table_meta, gpar_age = age)
    }
  }
  
  # Recode Youth Residence -----------------------------------------------------
  if (any(str_detect(my_concepts, "POPULATION UNDER 18 YEARS"))) {
    #table_meta <- filter(my_meta, str_detect(name, "B09001"))
    table_meta <-
      table_meta %>% 
      mutate(residence_type = 
               case_when(str_detect(label, "In households")     ~ "InHH",
                         str_detect(label, "In group quarters") ~ "InGQ") %>% 
               replace_na("All")
      )
  }
  
  # Recode Race/Ethnicity ------------------------------------------------------
  
  # If any table name values end in character (and thus immediately precede the
  # underscore, which separates table name from element number), that indicates
  # by-racial subset of the data. That provides a cue to develop race indicators
  # based on 
  
  if (any(str_detect(table_meta$name, "\\D_"))) {
    table_meta <- 
      table_meta %>% 
      mutate(raceeth = 
               case_when(str_detect(concept, "INDIAN")          ~ "AmInd",
                         str_detect(concept, "ASIAN")           ~ "As",
                         str_detect(concept, "BLACK.+ALONE")    ~ "Aa",
                         str_detect(concept, "\\(HISPANIC")     ~ "Hi",
                         str_detect(concept, "ISLANDER")        ~ "PacIs",
                         str_detect(concept, "OTHER")           ~ "Oth",
                         str_detect(concept, "TWO OR MORE")     ~ "Mult",
                         str_detect(concept, "WHITE ALONE\\)")  ~ "Wh",
                         str_detect(concept, "WHITE.+NOT HISP") ~ "WhNonH") %>% 
               replace_na("All"))
  }
  
  # Another situation for recoding is when race is directly included as part
  # of the table
  if (any(str_detect(my_concepts, " RACE"))) {
    table_meta <- 
      table_meta %>% 
      mutate(race = 
               case_when(str_detect(label, "Indian")          ~ "AmInd",
                         str_detect(label, "Asian")           ~ "As",
                         str_detect(label, "Black.+alone")    ~ "Aa",
                         str_detect(label, "Islander")        ~ "PacIs",
                         str_detect(label, "Some other race alone") ~ "Oth",
                         str_detect(label, "Two races including Some other race") ~ "Mult_inclOther",
                         str_detect(label, "Two races excluding Some other race, and three or more races") ~ "Mult_3ormore",
                         str_detect(label, "Two or more")     ~ "Mult",
                         str_detect(label, "White alone")     ~ "Wh") %>% 
               replace_na("All"))
  }
  
  # Recode Hispanic Identity ---------------------------------------------------
  if (any(str_detect(my_concepts, "HISPANIC OR LATINO"))) { 
    #table_meta <- filter(my_meta, str_detect(name, "B03002"))
    table_meta <- 
      table_meta %>% 
      mutate(hispanic = 
               case_when(str_detect(label, "!!Not Hispanic or Latino") ~ "NonHisp",
                         str_detect(label, "!!Hispanic or Latino")     ~ "Hisp") %>% 
               replace_na("All"))
  }
  
  # Recode Income to Poverty Ratio ---------------------------------------------
  if (any(str_detect(my_concepts, "RATIO OF INCOME TO POVERTY LEVEL"))) {
    #table_meta <- filter(my_meta, str_detect(name, "B17026"))
    table_meta <- 
      table_meta %>% 
      mutate(
        inc_to_pov_ratio = 
          label %>% 
          str_replace(".*(!!|^)(\\d*)\\.(\\d+) to (\\d*)\\.(\\d+)(!!|$)", "IncPov\\2\\3to\\4\\5") %>%
          str_replace(".*(!!|^)(\\d*)\\.(\\d+) and over.*",               "IncPov\\2\\3plus") %>%
          str_replace(".*(!!|^)Under (\\d*)\\.(\\d+).*",                  "IncPov0to\\2\\3") %>%
          str_replace("^Estimate.+",                                      "All"))
  }
  
  # Recode Income to Poverty Status --------------------------------------------
  if (any(str_detect(my_concepts, "POVERTY STATUS"))) {
    #table_meta <- filter(my_meta, str_detect(name, "B17012"))
    table_meta <- 
      table_meta %>% 
      mutate(pov_status =
               case_when(str_detect(label, "at or above poverty") ~ "NonPovInc",
                         str_detect(label, "below poverty")       ~ "PovInc") %>% 
               replace_na("All"))
  }
  
  # Recode Presence of own children --------------------------------------------
  if (any(str_detect(my_concepts, "PRESENCE OF OWN CHILDREN UNDER 18"))) {
    #table_meta <- filter(my_meta, str_detect(name, "B23007"))
    table_meta <- 
      table_meta %>% 
      mutate(own_kids_under18 =
               case_when(str_detect(label, "With own children under 18 years") ~ "SomeKidsUnder18",
                         str_detect(label, "No children under 18 years")       ~ "NoKidsUnder18") %>% 
               replace_na("All"))
  }
  
  # Recode Number of Related Children Under 18 Years ---------------------------
  if (any(str_detect(my_concepts, "NUMBER OF RELATED CHILDREN UNDER 18 YEARS"))) {
    #table_meta <- filter(my_meta, str_detect(name, "B17012"))
    table_meta <- 
      table_meta %>% 
      mutate(
        own_kids_under18 =
          case_when(
            str_detect(label, "No child")           ~ "NoKidsUnder18",
            str_detect(label, "1 or 2 children")    ~ "1or2KidsUnder18",
            str_detect(label, "3 or 4 children")    ~ "3or4KidsUnder18",
            str_detect(label, "5 or more children") ~ "5plusKidsUnder18") %>% 
          replace_na("All"))
  }
  
  # Recode Grandparent Presence ------------------------------------------------
  if (any(str_detect(my_concepts, "GRANDPARENTS LIVING WITH OWN GRANDCHILDREN"))) {
    #table_meta <- filter(my_meta, str_detect(name, "B10051"))
    table_meta <- 
      table_meta %>% 
      mutate(gp_resp = 
               case_when(str_detect(label, "Grandparent not responsible") ~ "GParNotResponsible",
                         str_detect(label, "Grandparent responsible")     ~ "GParIsResponsible" ) %>% 
               replace_na("All"))
    if (any(str_detect(my_concepts, "BY PRESENCE OF PARENT"))) {
      table_meta <- 
        table_meta %>% 
        mutate(gp_parent = 
                 case_when(str_detect(label, "no parent of grandchildren") ~ "NoParentPresent",
                           str_detect(label, "Other grandparents")         ~ "OtherGPPresent" ) %>% 
                 replace_na("All"))
    }
  }
  
  # Recode Household Type ------------------------------------------------------
  if (any(str_detect(my_concepts, "HOUSEHOLD TYPE"))) {
    #table_meta <- filter(my_meta, str_detect(name, "B17016"))
    table_meta <-
      table_meta %>% 
      mutate(
        family_type = 
          case_when(
            str_detect(label, "[Mm]arried-couple family")         ~ "Married",
            str_detect(label, "[Ff]emale householder, no spouse") ~ "UnmarriedFemaleHh",
            str_detect(label, "[Mm]ale householder, no spouse")   ~ "UnmarriedMaleHh",
            str_detect(label, "[Oo]ther famil(y|ies)")            ~ "Unmarried") %>% 
          replace_na("All")
      )
  }
  
  # Recode Labor Force, Employment Status, and Family Type ---------------------
  if (any(str_detect(my_concepts, "FAMILY TYPE|EMPLOYMENT"))) { 
    if (any(str_detect(my_concepts, "FAMILY TYPE BY EMPLOYMENT"))) {
      table_meta <- 
        table_meta %>% 
        mutate(
          family_type = 
            case_when(str_detect(label, "[Mm]arried-couple family")         ~ "Married",
                      str_detect(label, "[Ff]emale householder, no spouse") ~ "UnmarriedFemaleHh",
                      str_detect(label, "[Mm]ale householder, no spouse")   ~ "UnmarriedMaleHh",
                      str_detect(label, "[Oo]ther family")                  ~ "Unmarried") %>% 
            replace_na("All"),
          lfp = 
            case_when(
              family_type == "Married" ~
                case_when(str_detect(label, "Husband in.+Wife in")         ~ "HusbandInLfpWifeInLfp",
                          str_detect(label, "Husband in.+Wife not in")     ~ "HusbandInLfpWifeNotInLfp",
                          str_detect(label, "Husband not in.+Wife in")     ~ "HusbandNotInLfpWifeInLfp",
                          str_detect(label, "Husband not in.+Wife not in") ~ "HusbandNotInLfpWifeNotInLfp",
                          str_detect(label, "Husband in")                  ~ "HusbandInLfp",
                          str_detect(label, "Husband not in")              ~ "HusbandNotInLfp") %>% 
                replace_na("All"),
              TRUE ~ 
                case_when(str_detect(label, "[Nn]ot in labor force") ~ "NotInLF",
                          str_detect(label, "[Ii]n labor force")     ~ "InLF") %>% 
                replace_na("All")) %>% 
            replace_na("All"),
          status = 
            case_when(
              family_type == "Married" ~
                case_when(str_detect(label, "Husband in.+Empl.+Wife in.+Empl")  ~ "HusbandEmplWifeEmpl",
                          str_detect(label, "Husband in.+Empl.+Wife in.+Unem")  ~ "HusbandEmplWifeUnempl",
                          str_detect(label, "Husband in.+Unem.+Wife in.+Empl")  ~ "HusbandUnemplWifeEmpl",
                          str_detect(label, "Husband in.+Unem.+Wife in.+Unem")  ~ "HusbandUnemplWifeUnempl",
                          str_detect(label, "Husband not in.+.+Wife in.+Unem")  ~ "WifeUnempl",
                          str_detect(label, "Husband not in.+.+Wife in.+Empl")  ~ "WifeEmpl",
                          str_detect(label, "Husband in.+Empl")                 ~ "HusbandEmpl",
                          str_detect(label, "Husband in.+Unem")               ~ "HusbandUnempl") %>% 
                replace_na("All"),
              TRUE ~ 
                case_when(str_detect(label, "Employed")   ~ "Empl",
                          str_detect(label, "Unemployed") ~ "Unempl")) %>% 
            replace_na("All"))
    } else if (any(str_detect(my_concepts, "EMPLOYMENT STATUS"))) {  
      #table_meta <- filter(my_meta, str_detect(name, "B23001"))
      table_meta <- 
        table_meta %>% 
        mutate(lfp = 
                 case_when(str_detect(label, "[Nn]ot in labor force") ~ "NotInLF",
                           str_detect(label, "[Ii]n labor force")     ~ "InLF") %>% 
                 replace_na("All"),
               pop = 
                 case_when(str_detect(label, "Civilian") ~ "Civilian",
                           str_detect(label, "In Armed") ~ "Military") %>% 
                 replace_na("All"),
               status = 
                 case_when(str_detect(label, "Employed")   ~ "Empl",
                           str_detect(label, "Unemployed") ~ "Unempl") %>% 
                 replace_na("All"))
    } else if (any(str_detect(my_concepts, "FAMILY TYPE"))) { 
      table_meta <- 
        table_meta %>% 
        mutate(family_type = 
                 case_when(str_detect(label, "[Mm]arried-couple family")         ~ "Married",
                           str_detect(label, "[Oo]ther family")                  ~ "Other", 
                           str_detect(label, "[Ff]emale householder, no spouse") ~ "UnmarriedFemaleHh",
                           str_detect(label, "[Mm]ale householder, no spouse")   ~ "UnmarriedMaleHh") %>% 
                 replace_na("All"))
    }
  } 
  
  # Recode Education Attainment ------------------------------------------------
  if (any(str_detect(my_concepts, "EDUCATIONAL ATTAINMENT"))) { 
    table_meta <- 
      table_meta %>% 
      mutate(
        ed_attain = 
          case_when(
            str_detect(label, "No schooling")         ~ "EdLtHs", # "EdNoSch", 
            str_detect(label, "Nursery to 4th")       ~ "EdLtHs", # "EdLt4th", 
            str_detect(label, "5th and 6th")          ~ "EdLtHs", # "EdGr5or6", 
            str_detect(label, "7th and 8th")          ~ "EdLtHs", # "EdGr7or8", 
            str_detect(label, "9th grade")            ~ "EdLtHs", # "EdGr9", 
            str_detect(label, "10th grade")           ~ "EdLtHs", # "EdGr10", 
            str_detect(label, "11th grade")           ~ "EdLtHs", # "EdGr11", 
            str_detect(label, "12th grade, no dipl")  ~ "EdLtHs", # "EdGr12NoDipl", 
            str_detect(label, "High school graduate") ~ "EdHs", 
            str_detect(label, "Less than (high|9th)|Not high school graduate") ~
              "EdLtHs", 
            str_detect(label, "Some college")         ~ "EdSomeColl", 
            str_detect(label, "Associate's")          ~ "EdAssoc",
            str_detect(label, "Bachelor's")           ~ "EdColl", 
            str_detect(label, "Graduate")             ~ "EdGrad",
            str_detect(label, "Master's")             ~ "EdGrad",
            str_detect(label, "Professional")         ~ "EdGrad",
            str_detect(label, "Doctorate")            ~ "EdGrad",
            str_detect(label, "Less than 9th")        ~ "EdLtHs"
          ) %>% 
          replace_na("All"))
  }
  
  # Recode Means of Transportation ---------------------------------------------
  if (any(str_detect(my_concepts, "MEANS OF TRANSPORTATION TO WORK"))) {
    table_meta <-
      table_meta %>% 
      mutate(work_transp = 
               case_when(str_detect(label, "!!Worked at home")     ~ "WorkTranspHome",
                         str_detect(label, "!!Walked")             ~ "WorkTranspWalk",
                         str_detect(label, "!!Bicycle")            ~ "WorkTranspBike",
                         str_detect(label, "!!Public")             ~ "WorkTranspPublic",
                         str_detect(label, "!!Car, truck, or van") ~ "WorkTranspCar",
                         str_detect(label, "!!Taxicab")            ~ "WorkTranspTaxi",
                         str_detect(label, "!!Other means")        ~ "WorkTranspOther") %>% 
               replace_na("All"),
             transp_detail = 
               case_when(str_detect(label, "!!Drove alone$")    ~ "Drove_alone",
                         str_detect(label, "!!Carpooled($|!!)") ~ "Carpooled",
                         str_detect(label, "!!Carpooled!!In 2") ~ "Carpooled_2person",
                         str_detect(label, "!!Carpooled!!In 3") ~ "Carpooled_3person",
                         str_detect(label, "!!Carpooled!!In 4") ~ "Carpooled_4ormore",
                         str_detect(label, "!!Bus")          ~ "Bus",
                         str_detect(label, "!!Streetcar")    ~ "Streetcar",
                         str_detect(label, "!!Subway")       ~ "Subway",
                         str_detect(label, "!!Railroad")     ~ "Railroad",
                         str_detect(label, "!!Ferry")        ~ "Ferry",
                         str_detect(label, "!!Other means")  ~ "Other") %>% 
               replace_na("All"))
  }
  
  # Recode Disability Status ---------------------------------------------------
  if (any(str_detect(my_concepts, "DISABILITY STATUS"))) { 
    table_meta <- 
      table_meta %>% 
      mutate(
        disab_status = 
          case_when(
            str_detect(str_to_lower(label), "with a disability") ~ "Disab", 
            str_detect(str_to_lower(label), "no disability")     ~ "NoDisab") %>% 
          replace_na("All"))
  }
  
  # Recode Health Insurance Status ---------------------------------------------
  if (any(str_detect(my_concepts, "HEALTH INSURANCE COVERAGE"))) { 
    table_meta <- 
      table_meta %>% 
      mutate(
        health_ins_cov = 
          case_when(
            str_detect(str_to_lower(label), "with health insurance coverage") ~ "HasHealthIns", 
            str_detect(str_to_lower(label), "no health insurance coverage")   ~ "NoHealthIns") %>% 
          replace_na("All"))
  }
  
  # Recode Public and Private Health Insurance Status --------------------------
  if (any(str_detect(my_concepts, "(PUBLIC|PRIVATE) HEALTH INSURANCE"))) { 
    table_meta <- 
      table_meta %>% 
      mutate(
        pub_health_ins = 
          case_when(
            str_detect(str_to_lower(label), "with public coverage") ~ "HasPublicIns", 
            str_detect(str_to_lower(label), "no public coerage")    ~ "NoPublicIns") %>% 
          replace_na("All"),
        priv_health_ins = 
          case_when(
            str_detect(str_to_lower(label), "with private health") ~ "HasPrivIns", 
            str_detect(str_to_lower(label), "no private health")   ~ "NoPrivIns") %>% 
          replace_na("All"))
  }
  # Recode Birth History -------------------------------------------------------
  if (any(str_detect(my_concepts, "HAD A BIRTH IN THE PAST 12 MONTHS"))) { 
    table_meta <- 
      table_meta %>% 
      mutate(birth_12mo = 
               case_when(str_detect(label, "had a birth")          ~ "HadBirth", 
                         str_detect(label, "did not have a birth") ~ "NoBirth") %>% 
               replace_na("All"))
  }
  
  # Recode Marital Status -------------------------------------------------------
  if (any(str_detect(my_concepts, "MARITAL STATUS"))) { 
    table_meta <- 
      table_meta %>% 
      mutate(marital_status = 
               case_when(str_detect(label, "Unmarried")   ~ "single", 
                         str_detect(label, "Now married") ~ "married") %>% 
               replace_na("All"))
  }
  
  # Recode Language Spoken at Home ---------------------------------------------
  if (any(str_detect(my_concepts, "LANGUAGE SPOKEN AT HOME"))) { 
    table_meta <- 
      table_meta %>% 
      mutate(home_lang = 
               case_when(str_detect(label, "Speak only English")    ~ "English", 
                         str_detect(label, "Speak Spanish")         ~ "Spanish",
                         str_detect(label, "Speak other Indo-Euro") ~ "IndoEuro",
                         str_detect(label, "Speak Asian and Pac")   ~ "AsianPac",
                         str_detect(label, "Speak other")           ~ "Other") %>% 
               replace_na("All"))
  }
  
  # Recode Ability to Speak English --------------------------------------------
  if (any(str_detect(my_concepts, "ABILITY TO SPEAK ENGLISH"))) { 
    table_meta <- 
      table_meta %>% 
      mutate(english_ability = 
               case_when(str_detect(label, '"very well"')  ~ "SpeakEnglVeryWell", 
                         str_detect(label, '"well"')       ~ "SpeakEnglWell",
                         str_detect(label, '"not well"')   ~ "SpeakEnglNotWell",
                         str_detect(label, '"not at all"') ~ "SpeakEnglNotAtAll") %>% 
               replace_na("All"))
  }
  
  # Recode Child Relationship to Householder -----------------------------------
  if (any(str_detect(my_concepts, "RELATIONSHIP TO HOUSEHOLDER FOR CHILDREN"))) { 
    table_meta <- 
      table_meta %>% 
      mutate(child_relate = 
               case_when(str_detect(label, "Own child")             ~ "ChildIsOwn",
                         str_detect(label, "Grandchild")            ~ "ChildIsGrandkid",
                         str_detect(label, "Other relatives")       ~ "ChildIsOtherRel",
                         str_detect(label, "Foster child or other") ~ "ChildIsFosterOrOther") %>% 
               replace_na("All"),
             own_child_relate = 
               case_when(str_detect(child_relate, "All") ~ "OwnChildAll",
                         str_detect(label, "Biological") ~ "OwnChildBio",
                         str_detect(label, "Adopted")    ~ "OwnChildAdopted",
                         str_detect(label, "Step")       ~ "OwnChildStep") %>% 
               replace_na("NotOwnChild"))
  }
  
  # Recode Veteran status ------------------------------------------------------
  if (any(str_detect(my_concepts, "VETERAN STATUS"))) { 
    table_meta <- 
      table_meta %>% 
      mutate(
        vet_status = 
          case_when(
            str_detect(label, "Nonveteran") ~ "NonVet",
            str_detect(label, "Veteran")    ~ "Vet") %>% 
          replace_na("All"))
  }
  
  
  ### Check for duplicates -----------------------------------------------------
  
  error_msg <- glue("Table {table_meta$table[1]} has label values that are not fully ",
                    "recoded")
  if (nrow(table_meta) > 
      table_meta %>% select(-name, -table, -label) %>% unique() %>% nrow()) {
    warning(error_msg)
  }
  
  return(table_meta)  
}


#------------------------------------------------------------------------------#
# Functions to Assist Census Data Pulls ----------------------------------------
#------------------------------------------------------------------------------#

inspect_fields <- function(df, show_all_combos = FALSE) {
  cat_vars <- setdiff(colnames(df), c("variable", "GEOID", "estimate", "moe", "se", "table", "geography"))
  meta_table <- 
    df %>% 
    select(one_of(cat_vars)) %>% 
    unique()
  
  if (show_all_combos) {
    return(meta_table)
  } else {
    return(sapply(meta_table, unique))
  }
}

# /!\ Generate a (geo x year x source) x (measure estimate/moe) wide file
# ... may only be useful/makes sense as a list of objects rather than a long
# and sparse data.frame

# /!\ Create a script to generate new constructions by specifying num/denom
# Can do this smartly to look for list objects that have both measures, and
# create this construction for them all

# Example: for labor force by gender
# Request: subset to lfp == 1, by sex (but not age), numerator is status
# Process: execute the subset immediately. Keep all values of sex. Subset to
#          values of "all" for all other fields. Then perform proportion and
#          moe calculations for values of the numerator over the "all".
# Output: data.frame with (geo x year/source x measure) x (stat: est, moe, n, denom)
#         requires approach to naming the variable


construct_fields <- 
  function(
    df,                    # this is the processed Census data table with relevant variables
    subset_cond  = NULL,   # logical statement 
    denom_subset = NULL,   # logical statement, optionally defining a denominator condition
    subset_descr = NULL,   # This is a back door to labeling the subset, currently
    #   necessary because the logic for auto-translating the `subset_cond`
    #   isn't robust to handle multiple conditions, or `!=`s
    by_vars = NULL,        # Character vector with fields to use as drill-downs
    numerator,             # Character value with name of field that represents the numerator
    numerator_vals = NULL, # Optional: a vector or regex expression for the numerator
    #   variable to match to indicate a single key condition
    numerator_lab = NULL,  # Optional: if numerator_vals is specified, this value 
    #   will be used to label the condition for output
    wide = TRUE            # Indicate whether output should be wide vs long
  ) {
    
    ### Rename numerator and fix conditions
    df <- copy(df)
    setnames(df, numerator, "numerator")
    df <- filter(df, numerator != "All")
    if (!is.null(numerator_vals)) {
      
      # Provide a numerator label if not specified
      if (is.null(numerator_lab)) {
        numerator_lab <- "SelectedCondition"
      }
      
      if (length(numerator_vals) > 1) {
        df <- 
          df %>% 
          mutate(numerator = 
                   ifelse(numerator %in% numerator_vals,
                          numerator_lab,
                          glue("Not{numerator_lab}")))
      } else {
        df <- 
          df %>% 
          mutate(numerator = 
                   ifelse(numerator %like% numerator_vals,
                          numerator_lab,
                          glue("Not{numerator_lab}")))
      }
    }
    
    ### Check on specification
    cat_vars <- setdiff(colnames(df), c("variable", "GEOID", "estimate", "moe", "se", "table"))
    if (!all(by_vars %in% cat_vars)) stop("'by' variables weren't specified properly")
    
    ### Apply subset
    df_numer <- df
    df_denom <- df
    if (!is.null(subset_cond)) {
      df_numer <- filter(df, eval(parse(text = subset_cond)))
      if (!is.null(denom_subset)) {
        df_denom <- filter(df, eval(parse(text = denom_subset)))
      } else {
        df_denom <- df_numer
      }
    } 
    
    ### Subset to "All" values for unused variables
    # /!\ Could maybe be simplified with something like pmin(.SD == "All") which
    # would check for all "All"s in the applicable columns (using some data.table
    # terminology)
    unused_vars <- setdiff(cat_vars, c(by_vars, "numerator"))
    if (length(unused_vars) > 0) {
      for (v in unused_vars) {
        
        # Check that "All" exists in the subset. It may not, e.g. if subsetting 
        # to only those with active labor force participation all have "pop" value
        # of civilian, rather than "All"
        if ("All" %in% df_numer[, v]) {
          # see this approach in this "programming with dplyr" article: https://dplyr.tidyverse.org/articles/programming.html
          df_numer <- df_numer %>% filter(.data[[v]] == "All") %>% select(-one_of(v))
        }
        if ("All" %in% df_denom[, v]) {
          # see this approach in this "programming with dplyr" article: https://dplyr.tidyverse.org/articles/programming.html
          df_denom <- df_denom %>% filter(.data[[v]] == "All") %>% select(-one_of(v))
        }
      }
    }
    
    ### Generate estimates
    denom <- 
      df_denom %>% 
      summarize(.by = c("GEOID", by_vars),
                denom_n = sum(estimate),
                denom_se = se_sum(se) %>% replace_na(replace = 0))
    
    numer <- 
      df_numer %>% 
      summarize(.by = c("GEOID", by_vars, "numerator"),
                numer_n = sum(estimate),
                numer_se = se_sum(se) %>% replace_na(replace = 0))
    
    calc <-
      numer %>% 
      merge(denom,
            by = c("GEOID", by_vars)) %>% 
      mutate(r = numer_n / denom_n,
             r_se = se_ratio(numer_n, denom_n, numer_se, denom_se)) %>% 
      select(one_of("GEOID", by_vars, "numerator", "r", "r_se", "numer_n", "numer_se", "denom_n")) %>% 
      rename(se_r = r_se, 
             n = numer_n,
             se_n = numer_se,
             w = denom_n) %>% 
      pivot_longer(cols = c("r", "se_r", "n", "se_n", "w"), 
                   names_to = "stat") 
    
    if (wide) {
      calc <- 
        calc %>% 
        mutate(fieldname = paste0(stat, numerator, "_", apply(.[, by_vars], 1, paste, collapse = "_")) %>% str_replace("_$", ""))
      
      if (!is.null(subset_cond)) {
        subset_tag <- paste0("__Among", 
                             subset_cond %>% 
                               str_replace_all("\\S*(.+)", "\\1") %>% # Remove the first part of the condition
                               str_replace_all(" |=|\'|\"", "")) # remove all equals operations or quotes from the value
        if (!is.null(subset_descr)) subset_tag <- subset_descr
        calc <- 
          calc %>% 
          mutate(fieldname = paste0(fieldname, subset_tag))
      }
      
      calc <- 
        calc %>% 
        pivot_wider(id_cols = GEOID,
                    names_from = fieldname,
                    values_from = value)
    } else {
      calc <- 
        calc %>% 
        pivot_wider(names_from = stat,
                    values_from = value)
      setnames(calc, "numerator", numerator)
    }
    
    return(calc)
    #var_name = <statistic prefix><numerator condition>_<sequence of by values>__Among<subset>
  }


#------------------------------------------------------------------------------#
# Functions to Assist Census Data Processing -----------------------------------
#------------------------------------------------------------------------------#

# FORMULAS PROVIDED BY CENSUS (IGNORE COVARIANCE BETWEEN COMPONENT RANDOM VARIABLES)

# Source: https://www2.census.gov/programs-surveys/acs/tech_docs/statistical_testing/2015StatisticalTesting5year.pdf,
#         https://www.census.gov/content/dam/Census/library/publications/2008/acs/ACSGeneralHandbook.pdf

# "All methods in this section are approximations and users should be cautious in using them.
# This is because these methods do not consider the correlation or covariance between the basic
# estimates. They may be overestimates or underestimates of the derived estimate's standard error,
# depending on whether the two basic estimates are highly correlated in either the positive or
# negative direction."

# Census defines margin of error as 1.64 * standard error (10% confidence interval band). Divide by 1.64
# to convert margin of error to standard error in Census/ACS data.

# FORMULAS DERIVED BY DELTA METHOD (ACCOUNT FOR COVARIANCE BETWEEN COMPONENT RANDOM VARIABLES)

# Source: http://statweb.stanford.edu/~susan/courses/s200/lectures/lect5.pdf

# Here, we use the delta method to derive the formula for the variance of a proportion ((taking the variance of the Taylor series expansion)): 
# var(A/B) = [d(A/B)/dA]^2 * var(A) + [d(A/B)/dB]^2 * var(B) + d(A/B)/dA * d(A/B)/dB * cov(A,B)
#          = [1/B]^2 * var(A) + [-A/B^2]^2 *  var(B)  + [1/B] * [-A/B^2] * cov(A, B)
#          = [1/B]^2 * var(A) + [-A/B^2]^2 *  var(B)  + [1/B] * [-A/B^2] * var(A) ((since B = A + not A))
#          = [1/B]^2 * [[1-[A/B]] * var(A) + [A/B]^2 * var(B)]

moe_to_se <- function(moe) {
  moe / 1.645
}

# STANDARD ERROR FOR AGGREGATED COUNT DATA (COLLAPSE)

# Aggregate over the set of variables specified in "by" statement.
# This is only an approximation of the true SE, since we do not observe the covariance between
# the components of the sum in the published ACS data (requires unpublished microdata).

se_sum <- function(se) {
  return(sqrt(sum((se)^2, na.rm = TRUE)))
}

# STANDARD ERROR FOR PRODUCT OF RANDOM VARIABLES

se_product <- function(A, B, A_se, B_se) {
  
  # Using the delta-method formula
  ## var(A*B) = [d(A*B)/dA]^2 * var(A) + [d(A*B)/dB]^2 * var(B) + 2 * d(A*B)/dA * d(A*B)/dB * cov(A,B)
  ##          = [B]^2 * var(A) + [A]^2 *  var(B)  + 2 * [AB] * cov(A, B)
  
  # Note -- this assumes zero covariance
  sqrt(A^2*B_se^2 + B^2*A_se^2)
}

# STANDARD ERROR FOR DERIVED PROPORTION

# See derivation using Delta Method, above.

se_proportion <- function(var1, var2, se1, se2) {
  return((1/var2)*sqrt( (1-(var1/var2))*(se1)^2 + (var1/var2)^2*(se2)^2) )
}

# STANDARD ERROR FOR (NON-PROPORTION) RATIO

# This is only an approximation of the true SE, since we do not observe the covariance between the components of the
# ratio in the published ACS data.

se_ratio <- function(var1, var2, se1, se2) {
  return((1/var2)*sqrt( (se1)^2 + (var1/var2)^2*(se2)^2) )
}

# SUM OF STANDARD ERRORS

se_col_sum <- function(...) {
  args <- list(...)
  args_2 <- lapply(args, function(x) x^2)
  Reduce(`+`, args_2) %>% sqrt()
}

# Test this function
if (FALSE) {
  with(mtcars, se_col_sum(mpg, hp, wt))
}

### Functions to get and to apply metadata -------------------------------------

get_acs5_metadata <- function(year) {
  meta_name <- glue("acs5_meta_{year}")
  # If this metadata object isn't in the global environment, create it in that
  # environment
  if (!meta_name %in% ls(pos = 1)) {
    assign(meta_name,
           load_variables(year, "acs5"), 
           pos = 1) 
  }
  return(get(meta_name))
}


### Wrapper for full pull ------------------------------------------------------

pick_acs5 <- 
  function(table,
           geography = "tract", 
           state = "IL", 
           county = "Cook", 
           survey = "acs5", 
           year = 2021,
           cache_table = TRUE,
           subset_cond  = NULL,
           denom_subset = NULL,
           subset_descr = NULL,
           by_vars = NULL,
           numerator) {
    
    # Pull the data
    table_pull <- 
      pull_table(table = table,
                 geography = geography,
                 state = state,
                 county = county,
                 survey = survey,
                 year = year,
                 cache_table = cache_table)
    
    # Develop the data
    acs5_constr <- 
      construct_fields(df = table_pull,
                       subset_cond = subset_cond,
                       denom_subset = denom_subset,
                       subset_descr = subset_descr,
                       by_vars = by_vars,
                       numerator = numerator)
    
    return(acs5_constr)
  }


### Test sample implementation -------------------------------------------------

if (FALSE) {
  # Get statistics on foster care versus other child relationship
  foster_pull <- pull_table("B09018")
  foster_calcs <- 
    pick_acs5(table = "B09018", 
              subset_cond = "child_relate != 'All'",
              subset_descr = "",
              numerator = "child_relate")
  
  # https://www.socialexplorer.com/data/ACS2021_5yr/metadata/?ds=ACS21_5yr&table=B03002
  raceeth_pull <- 
    pull_table("B03002")
  inspect_fields(raceeth_pull, show_all_combos = TRUE)
  
  
  # Pull and inspect 
  check_meta(2021, "B17024")
  pov_data <- pull_table("B17024")
  inspect_fields(pov_data)
}

### To Dos ---------------------------------------------------------------------

# Function to investigate all values present in the data, by table
#   - Perhaps also a display to help show some of the values, that they can subset by
# Create a wrapper that easily allows all columns to be a percentage of a given
#   other value, e.g.families in each income bin as a % of total. Or, educational
#   attainment as a percentage of same-sex population.
#   ... This was an old note, and I think that by_var allows calculations by sex
#       and numerator allows for all calculations of another given value

# Add key to .Renviron

if (FALSE) {
  vars <- c("B09001_002",
            "B09001_003",
            "B09001_004",
            "B09001_005",
            "B09001_006",
            "B09001_007",
            "B09001_008",
            "B09001_009")
  
  acs_0017 <- 
    get_acs(geography = "tract",
            variables = vars,
            state = "IL",
            county = "Cook",
            year = 2020,
            output = "wide")
  
}

### Install packages -----------------------------------------------------------
# Due to potential name conflicts (e.g. often with select() or filter()), the
# data manipulation packages--notably dplyr--are loaded last to ensure that
# their versions of these functions are not masked

# Start by installing a package to help search for whether already-installed
# packages are the most recent
# install.packages("pkgsearch")
# library(pkgsearch)

packages.list <- 
  c("knitr", "ggplot2", "Rcpp", "Hmisc", "VGAM", "stringr", "glmnet", "censusapi",
    "plotly", "crosstalk", "DT", "kableExtra", "sae", "emdi", "glue", "forcats", 
    "zoo", "moments", "leaflet", "RColorBrewer", "ggalluvial", "tigris",
    "extrafont", "remotes", "broom", "sf", "lwgeom", "foreach", "doParallel",
    "tidycensus", "fredr", "ipumsr", "tsibble", "forecast", "fable", #"xlsx", 
    "boot", "ggtext", "data.table", "tidyr", "dplyr", "scales", "janitor", "R.utils",
    "lubridate", "readxl", "openxlsx", "officer", "spatial", "ggthemes", "jsonlite")

for (p in packages.list) {
  if (!p %in% installed.packages()[, "Package"]) {
    suppressMessages(install.packages(p))
  }
  
  # Load the package quietly
  suppressMessages(library(p, 
                           character.only = TRUE,
                           verbose = FALSE))
}

# Suppress warnings from dplyr::summarise() with group_by()s
options(dplyr.summarise.inform = FALSE)

# Do not use scientific notation in output
options(scipen = 999)

# Custom functions

meanNA <- function(x, ...) mean(x = x[!is.infinite(x)], ..., na.rm = TRUE)
 sumNA <- function(x, ...)  sum(x = x[!is.infinite(x)], ..., na.rm = TRUE)
 minNA <- function(x, ...)  min(x = x[!is.infinite(x)], ..., na.rm = TRUE)
 maxNA <- function(x, ...)  max(x = x[!is.infinite(x)], ..., na.rm = TRUE)
 varNA <- function(x, ...)  var(x = x[!is.infinite(x)], ..., na.rm = TRUE)
  sdNA <- function(x, ...)   sd(x = x[!is.infinite(x)], ..., na.rm = TRUE)
n_nonmiss   <- function(x) sum(!is.na(x))
pct_nonmiss <- function(x) mean(!is.na(x))
bound_val <- function(x, lower, upper) pmin(x, upper) %>% pmax(., lower)
paste_ <- function(...) paste(..., sep = "_")
 
whats_big <- function() {
  sapply(ls(), 
         function(x) object.size(get(x))) %>% 
    sort()
}

### Generate aliases for select functions --------------------------------------
cn <- function(x) colnames(x)
sub_cn <- function(x, pattern) str_subset(cn(x), pattern)


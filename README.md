# Background

This is a project intended to estimate counts of children eligible for a range of child care subsidies and supports. This is relevant to Chapin Hall's work as part of its federally-funded Child Care Policy Research Partnership (CCPRP) project in partnership with the Illinois Department of Human Services (IDHS) as well as local work in collaboration with the Chicago Department of Family and Support Services (DFSS). Each agency is tasked with managing public funding to ensure adequate access to quality childcare in their jurisdictions, and understanding the quantity of need--and eligibility for public supports--is a key consideration.

E-mail [Nick Mader](mailto:nsmader@gmail.com) and [Hyein Kang](mailto:kanghyein@hotmail.com) for questions related to this method or this codebase.

## Suggested Citation

Mader, N., Kang, H.. (2025). Method for Estimates of Local Populations Eligible for Programs (ELPEP) for near-present conditions in small geographies. GitHub. https://github.com/chapinhall/elpep

# Methodology

Click [this link](https://github.com/chapinhall/elpep/raw/refs/heads/main/ELPEP%20Methodology%20Summary.docx) to download a summary of ELPEP's methodology.

A full technical report, including fully typeset description of methods, but to run all data pulls, development, analysis methods, related methods showing results and diagnostics for a given run will be "rendered" when running the "run-elpep.Rmd" script after specifying run details using the `Interface for Specifying Custom ELPEP Eligibility Estimates.xlsm` interface. This process will simultaneous output files with table and map output.

# Running This Code

#### Step 1. Create an input folder and an output folder. The path of these folders will be used in Step 4.

#### Step 2. Download datasets manually

1. Go to [IPUMS CPS](https://cps.ipums.org/cps/) website
2. Create account and log in
3. Click "Browse and select data" under "Data" section
4. Click "Select Samples", make sure "cross-sectional" is selected for the sample selections. Under "BASIC MONTHLY", select January 2007-December 2009 and January 2019-most recent month, and click "Submit Sample Selections"
5. Select the following variables:
* HOUSEHOLD - CORE
	- TECHNICAL: YEAR, SERIAL, MONTH, HWTFINL, ASECFLAG
	- GEOGRAPHIC: STATEFIP, COUNTY, METAREA, METRO
	- ECONOMIC CHARACTERISTICS: HHINCOME, FAMINC
* PERSON - CORE
	- DEMOGRAPHICS: RELATE, AGE, SEX, RACE, MARST
	- FAMILY INTERRELATIONSHIPS: MOMLOC, MOMLOC2, POPLOC, POPLOC2, SPLOC, FAMUNIT
	- ETHNICITY/NATIVITY: HISPAN
	- WORK: EMPSTAT, LABFORCE, OCC, IND, CLASSWKR
	- EDUCATION: EDUC, SCHLCOLL
	- TECHNICAL: PERNUM, WTFINL, FAMID
6. Click "View cart"
7. Click "Create Data Extract"
8. When the data extract is complete, download the data and the DDI file to the input folder. The file extension of the data file should appear as `csv.gz` in the input folder. For the DDI file, right-click on the "DDI" link to save the file in the input folder.
9. Download 2010 Block Groups to 2020 Census tracts crosswalk from NHGIS (IPUMS log in required). Click this [url](https://data2.nhgis.org/crosswalks/nhgis_bg2010_tr2020.zip) and a pop-up window for download will appear. Save the file in the input folder
10. Download 2010 Block to 2020 Block (GEOID Identifiers) crosswalk from NHGIS (IPUMS log in required). Click this [url](https://data2.nhgis.org/crosswalks/nhgis_blk2010_blk2020_ge.zip) and a pop-up window for download will appear. Save the file in the input folder

You will now have the following four files in the input folder: CPS data, CPS documentation, and two crosswalk files.

#### Step 3. Create APIs (if you already have APIs for the Census and FRED, this step may be skipped)

1. Create Census API [here](https://api.census.gov/data/key_signup.html).
2. Create FRED API [here](https://fred.stlouisfed.org/docs/api/api_key.html).

#### Step 4. Running this code requires 64-bit Java. If Java is not installed or you have 32-bit Java installed, 64-bit Java can be downloaded [here](https://www.java.com/en/download/manual.jsp).

#### Step 5. Open the `Interface for Specifying Custom ELPEP Eligibility Estimates.xlsm` file in Excel, which will allow you to supply necessary details and set a wide range of options to run ELPEP in your jurisdiction's context. This includes setting current--or hypothetical--income eligibility thresholds, custom age groups, and custom geographies for aggregation. 

This Excel workbook uses macros to check that any entries are valid, and to provide help menus that describe the relevance and allowable values for each field. To be able to use the interface with these macros, you will need to click on any prompts to "Enable content" to confirm that the macro is from a trusted source.

#### Step 6. Open `run-elpep.Rmd` file in RStudio, and replace the text "<full/path/to/the/Excel/file.xlsm>" with the full file path to the Excel file you have just created. **If you are a PC users, you must use forward-slashes instead of back-slashes. For example, instead of "C:\users\myname\elpep\interface.xlsm" you must enter "C:/users/myname/elpep/interface.xlsm".**

Note that you may have multiple runs of ELPEP set up by creating multiple copies of the Excel Interface file, and changing this one line to point to various input files. Next, in the RStudio Code menu, select "Run Region >> Run All".

Note that running this code for the first time may take several hours, and the amount of time may vary depending on specified region (state/county) or CPU/RAM of the computer.

# Updating the Estimates

After running the codebase once, updating the estimates with the most recent CPS or ACS data is simple. 

To update with new CPS basic monthly file:

1. Go to [IPUMS CPS](https://cps.ipums.org/cps/) website
2. Log in and click "My Data"
3. Click "Revise" of your most recent data
4. Click "Change" for the samples
5. Add newly released sample
6. Click "Submit Sample Selections"
7. Download the data and DDI in the input folder.
8. Update the data file name (cps_000XX) in the Excel Interface file
9. Run the code

To update with new ACS 1-year or 5-year:

1. Update the "base year" field in the Excel Interface file
2. Run the code

# Data Sources and their Uses

## American Community Survey (ACS) 1-Year Data

ACS 1-Year data (ACS1) microdata has information about each member of surveyed households, including age, employment/school attendance status, and income. These are used to build estimated counts and shares of children in CCAP-eligible households.
However, ACS1 data are only as geographically specific as the Census Public Use Microdata Code (PUMA) level, which reflects areas containing a minimum of 100,000 individuals. For example, there are 17 PUMAs inside of Chicago, but also 15 PUMAs in Illinois that span multiple counties.

## American Community Survey (ACS) 5-Year Data

ACS 5-Year data (ACS5) are aggregated across five consecutive years of ACS survey data, gaining sample size--and thus greater ability and validity--for reporting at geographically more granular levels. We focus on tables released at the Census tract level, which are intended to represent areas about 4,000 inhabitants on average. With that definition, Census tracts naturally vary in size across urban and rural contexts. However, the ACS5 data are released as aggregate tables -- such as estimated number of kids under each federal poverty level -- and not microdata. While CCAP eligibility is not reflected, the ACS5 data provides information useful for distinguishing the general socioeconomic characteristics of smaller geographic areas.

## Current Population Survey (CPS)

CPS collects information about each member of surveyed households on a monthly basis, providing recent information, only 1-2 months lagged. However, CPS does not provide information at a geographically granular level. CPS is used to build statistical relationships between household socioeconomic status baseline year and CCAP eligibility as of its most recent available data, which is applied to estimate Census-tract level CCAP eligibility as of the most recent month of CPS data.

The CPS data were downloaded interactively from the [IPUMS CPS](https://cps.ipums.org/cps/) (originally "Integrated Public Use Microdata Series") website. Annual Supplementary Economic Characteristics (ASEC) data and Basic Monthly Data were pulled for 2019 and all months forward for the current exercise, and for both 2007-2009 for the sake of validating our method using data from throughout the Great Recession.

## Poverty Guidelines data

Poverty Guidelines information is a key determinant of eligibility for child care supports. This information is drawn from [this source](https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/).


# Code Files

Code files in this repository include:

## Settings Scripts

* `settings--main.R` -- This script loads libraries, functions, and visual standards whose use is common across multiple scripts below.
* `settings--config.R` -- This script sets parameters specific to each user's context, both in terms of computing, and in terms of geographic state of focus. See the `Running This Code` section above.

## Pull Scripts

These files can be run manually, but are generally set up to be run automatically by the `run` scripts below that require their output.

* `pull--acs1-microdata.R` -- automatically pulls ACS1 microdata using the Census API
* `pull--acs5-general-data.R` -- pulls and processes many ACS5 tables of interest, used in the Small Area Estimation method
* `pull--acs5-data-on-poverty-by-race.R` -- a select subset of ACS5 data with information about poverty-by-race data, using in postprocessing steps to provide estimated racial breakdowns of eligibility counts. In the future, this can incorporated into the `pull--acs5-general-data.R` script.

## Run Scripts

* `run--00--main-doc.Rmd` -- run and incorporate all following scripts into a main document
* `run--01a--prep-geo-data.Rmd` -- this auto-pulls shape files from the Census TIGER service based on state--and if specified, county--indications in the `settings--config.R` script, and produces useful cross-walks between tracts and PUMA, zip codes, school districts, and (if specified) custom geographies
* `run--01b--prep-acs1-data.Rmd` -- reads pulls of ACS1 microdata and develops necessary structures for use in the Small Area Estimation method
* `run--01c--prep-cps-data.Rmd` -- reads pulls of CPS microdata (instructions provided above) and develops necessary structures for use in the "now-casting" method
* `run--01d--prep-acs5-data.Rmd` -- adds onto the output from the `pull--acs5-general-data.R` script, adding some additional fields, and aggregating calculations and standard errors up to the PUMA level
* `run--01e--prep-pop-by-age-data.Rmd` -- pulls data from the Census Demographic and Housing Characteristics File (DHC) and Census Summary File 1 (SF1) to get population by age group to use in projecting population counts for young children by the most recent year.
* `run--02a--run-and-validate-SAE.Rmd` -- run a range of SAE specifications for a range of measures--including share of households by income-to-poverty ratio, and measures related to predicting CCDF eligibility--and examine their properties to gauge their individual reliability, and compare their output
* `run--02b--run-and-validate-nowcasting.Rmd` -- run a range of "now-casting" analyses, apply them to baseline+SAE measures to estimate counts, and examine patterns and maps of the output.
* `run--03a--postestimation-display-and-output.Rmd` -- after selecting sensitivities based on output guidance from the `02` scripts, this script examines patterns of the results
* `run--03b--postestimation-disaggregate-estimates.Rmd` -- disaggregates the estimates into smaller age ranges, and aggregates all of the results up to various geographic levels including zip code, school district, and county
* `run--03c--gen-ouput -- outputs the final estimates into a formatted Excel workbook and Powerpoint deck

## Methods Scripts

* `method--general-helper-functions.R` -- these are general functions used throughout the scripts, including processing for ACS data, calculating income-to-poverty ratios, and converting age, income, and industry codes into categorical groups/classifications
* `method--small-area-estimation-functions.Rmd` -- run Small Area Estimation (SAE) method to estimate, in prior years, counts of program-eligible children for small geographies
* `method--nowcasting-functions.Rmd` -- these are functions that use CPS data to "now-cast" the Small Area counts to a recent month
* `method--pull-and-process-acs5.R` -- these are functions to automatically pull and process ACS5 data

## Other Diagnostic Scripts

The diagnostic scripts generally represent one-time examinations that were run to examine a phenomenon, motivate a choice, or build a comparison (e.g. of output sensitivities) on occasions of interest. While these were neither common or central enough to be of interest in the main report (although perhaps several could belong as appendix materials in the future), they were meaningful enough to keep around for recordkeeping or occasional use.

* `diagnostic--basictabs_cps.Rmd` -- this investigates the size of the CPS, and in effect establishes the fact that given a focus on children under age 5, it is necessary to pool households across the US to maximize sample size to characterize recent trends in household economic dynamics
* `diagnostic--analyze_familyincome_cps.Rmd` -- this investigates the ability for the CPS to provide income characterizations useful for determining CCAP program eligibility
* `diagnostic--validate_estimates.R` -- build comparison between new statistical estimates and ACS microdata estimates
* `diagnostic--validate_estimates.Rmd` -- displays comparisons built in `diagnostic--validate_estimates.R`

## Sandbox Scripts

* `sandbox--emdi_package_functionality.Rmd` -- this file used as a place to explore functions, output, speed, and specifications of functions in the `emdi` package, which generates the Small Area Estimation output

## Notes on the Scripts

Throughout the codebase, there are two marker that signal attention to users:
* /!\\ -- this is used in comments that are primarily advisory, explaining a key assumption, decision, or opportunity for future reconsideration
* /*\\ -- this is used to invite users to alter or add to the code. Examples include diagnostics where some local information can be provided, or titling or explanation of figures to describe the patterns seen in the user's own data. Efforts have been made to ensure that the code is structured to allow for multiple users to contribute to a common codebase (e.g. by using conditional statements that select the appropriate figure title based on the `my_output_tag` value relevant to the local context), but for cases where larger deviation is required (e.g. where the codebase is modified to focus on a different public program requiring different data development) we recommend "forking" this repository for parallel development.


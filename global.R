library(shiny)
library(tidyverse)
library(gridExtra)

source("helpers.R")

##################
## READ IN DATA ##
##################

# UI SELECTORS

# LEA data
lea_data <- read.csv("./data/2017-18 Public-Use Files/Data/LEA/CRDC/CSV/LEA Characteristics.csv")

# School data
source_dir = "./data/2017-18 Public-Use Files/Data/SCH/CRDC/CSV/"

# Note: This file populates our school selector
school_characteristics <- read.csv(paste0(source_dir, 'School Characteristics.csv')) %>%
  mutate(
    elementary_school = ifelse(SCH_GRADE_G01 == 'Yes' | SCH_GRADE_G02 == 'Yes' | SCH_GRADE_G03 == 'Yes' | SCH_GRADE_G04 == 'Yes' | SCH_GRADE_G05 == 'Yes', 'Yes', 'No'),
    middle_school = ifelse(SCH_GRADE_G06 == 'Yes' | SCH_GRADE_G07 == 'Yes' | SCH_GRADE_G08 == 'Yes', 'Yes', 'No'),
    high_school = ifelse(SCH_GRADE_G09 == 'Yes' | SCH_GRADE_G10 == 'Yes' | SCH_GRADE_G11 == 'Yes' | SCH_GRADE_G12 == 'Yes', 'Yes', 'No'),
    other_level = ifelse(SCH_GRADE_PS == 'Yes' | SCH_GRADE_KG == 'Yes' | SCH_GRADE_UG == 'Yes', 'Yes', 'No')
  )

# ENROLLMENT DEMOGRAPHICS
enrollment <- read.csv(paste0(source_dir, 'Enrollment.csv'))

# ACADEMIC OPPORTUNITIES
gifted_and_talented <- read.csv(paste0(source_dir, 'Gifted and Talented.csv'))
advanced_placement <- read.csv(paste0(source_dir, 'Advanced Placement.csv'))
international_baccalaureate <- read.csv(paste0(source_dir, 'International Baccalaureate.csv'))
advanced_mathematics <- read.csv(paste0(source_dir, 'Advanced Mathematics.csv'))
physics <- read.csv(paste0(source_dir, 'Physics.csv'))
chemistry <- read.csv(paste0(source_dir, 'Chemistry.csv'))
calculus <- read.csv(paste0(source_dir, 'Calculus.csv'))

# SCHOOL CLIMATE
harassment_and_bullying <- read.csv(paste0(source_dir, 'Harassment and Bullying.csv'))

# STUDENT SUPPORT
school_support <- read.csv(paste0(source_dir, 'School Support.csv'))



#################################
## DEFINE VARIABLES TO EXTRACT ##
#################################
# We will need to specify these based on their grain of disaggregation, because each grain
# requires a different set of transformations to make them useful for plotting
# Note: We won't actually extract these until a specific school is selected

# ETHNICITY + SEX

ethnicity_sex_disagg_vars_to_extract <- list(
  # Enrollment Counts
  list(
    code = 'SCH_ENR',
    var_name = 'Total Enrollment', 
    pattern = 'SCH_ENR_(HI|AM|AS|HP|BL|WH|TR)', 
    source_file = 'enrollment_data'
  ),
  list(
    code = 'SCH_LEPENR',
    var_name = 'Students w/ Limited Engl. Proficiency (LEP)', 
    pattern = 'SCH_LEPENR_(HI|AM|AS|HP|BL|WH|TR)', 
    source_file = 'enrollment_data'
  ),
  list(
    code = 'SCH_IDEAENR',
    var_name = 'Students w/ disabilities (IDEA)', 
    pattern = 'SCH_IDEAENR_(HI|AM|AS|HP|BL|WH|TR)',
    source_file = 'enrollment_data'
  ),
  
  # Academics
  list(
    code = 'SCH_GTENR',
    var_name = 'Gifted & Talented Enrollment',
    pattern = 'SCH_GTENR_(HI|AM|AS|HP|BL|WH|TR)',
    source_file = 'gifted_and_talented'
  ),
  list(
    code = 'SCH_APENR',
    var_name = 'In at least 1 AP course',
    pattern = 'SCH_APENR_(HI|AM|AS|HP|BL|WH|TR)',
    source_file = 'advanced_placement'
  ),
  list(
    code = 'SCH_IBENR',
    var_name = 'Intl. Bacc. Enrollment',
    pattern = 'SCH_IBENR_(HI|AM|AS|HP|BL|WH|TR)',
    source_file = 'international_baccalaureate'
  ),
  # list(
  #   code = 'SCH_MATHENR_ADVM',
  #   var_name = 'Enrolled in Advanced Mathematics',
  #   pattern = 'SCH_MATHENR_ADVM_(HI|AM|AS|HP|BL|WH|TR)',
  #   source_file = 'advanced_mathematics'
  # ),
  list(
    code = 'SCH_SCIENR_PHYS',
    var_name = 'Enrolled in Physics',
    pattern = 'SCH_SCIENR_PHYS_(HI|AM|AS|HP|BL|WH|TR)',
    source_file = 'physics'
  ),
  list(
    code = 'SCH_SCIENR_CHEM',
    var_name = 'Enrolled in Chemistry',
    pattern = 'SCH_SCIENR_CHEM_(HI|AM|AS|HP|BL|WH|TR)',
    source_file = 'chemistry'
  ),
  list(
    code = 'SCH_MATHENR_CALC',
    var_name = 'Enrolled in Calculus',
    pattern = 'SCH_MATHENR_CALC_(HI|AM|AS|HP|BL|WH|TR)',
    source_file = 'calculus'
  ),
  
  # School Climate
  list(
    code = 'SCH_HBREPORTED_RAC',
    var_name = 'Reported as harassed/bullied on basis of race, color or national origin',
    pattern = 'SCH_HBREPORTED_RAC_(HI|AM|AS|HP|BL|WH|TR)',
    source_file = 'harassment_and_bullying'
  ),
  list(
    code = 'SCH_HBREPORTED_DIS',
    var_name = 'Reported as harassed/bullied on basis of disability',
    pattern = 'SCH_HBREPORTED_DIS_(HI|AM|AS|HP|BL|WH|TR)',
    source_file = 'harassment_and_bullying'
  )
)


# SCHOOL

school_vars_to_extract <- list(
  list(
    code = 'SCH_OFFENSE',
    var_name = 'Incidents of ',
    pattern = 'SCH_OFFENSE',
    detail_list = list(
      'RAPE' =	'rape or attempted rape',
      'BATT' =	'other sexual assault',
      'ROBWW'	= 'robbery with a weapon',
      'ROBWX' =	'robbery with a firearm or explosive device',
      'ROBWOW' =	'robbery without a weapon',
      'ATTWW' =	'physical attack or fight w/ weapon',
      'ATTWX'	= 'physical attack or fight w/ firearm or explosive device',
      'ATTWOW' = 'physical attack or fight w/o a weapon',
      'THRWW' = 'threats of physical attack w/ weapon',
      'THRWX' =	'threats of physical attack w/ firearm or explosive device',
      'THRWOW' =	'threats of physical attack without a weapon',
      'POSSWX' =	'possession of a firearm or explosive device'
    ),
    source_file = 'offenses'
  ),
  list(
    code = 'SCH_HBALLEGATIONS',
    var_name = 'Allegations of harassment or bullying on the basis of ',
    pattern = 'SCH_HBALLEGATIONS',
    detail_list = list(
      'SEX' = 'sex',
      'RAC' = 'race, color, or national origin',
      'DIS' = 'disability',
      'ORI' = 'sexual orientation',
      'REL' = 'religion'
    ),
    source_file = 'harassment_and_bullying'
  )
)
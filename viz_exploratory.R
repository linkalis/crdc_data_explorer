source_dir = "../2017-18 Public-Use Files/Data/SCH/CRDC/CSV/"

school_characteristics <- read.csv(paste0(source_dir, 'School Characteristics.csv'))
enrollment_data <- read.csv(paste0(source_dir, 'Enrollment.csv'))
advanced_placement <- read.csv(paste0(source_dir, 'Advanced Placement.csv'))
international_baccalaureate <- read.csv(paste0(source_dir, 'International Baccalaureate.csv'))
advanced_mathematics <- read.csv(paste0(source_dir, 'Advanced Mathematics.csv'))
physics <- read.csv(paste0(source_dir, 'Physics.csv'))
harassment_and_bullying <- read.csv(paste0(source_dir, 'Harassment and Bullying.csv'))
offenses <- read.csv(paste0(source_dir, 'Offenses.csv'))

school_support <- read.csv(paste0(source_dir, 'School Support.csv'))

# School examples:
filter(LEAID == '2711250' & SCHID == '510') %>% # Edina



# VARTIABLE TYPE 1: ETHNICITY, SEX DISAGGREGATED #########################################################

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
    var_name = 'Students who are Limited English Proficient (LEP)', 
    pattern = 'SCH_LEPENR_(HI|AM|AS|HP|BL|WH|TR)', 
    source_file = 'enrollment_data'
    ),
  list(
    code = 'SCH_IDEAENR',
    var_name = 'Students with disabilities served under IDEA', 
    pattern = 'SCH_IDEAENR_(HI|AM|AS|HP|BL|WH|TR)',
    source_file = 'enrollment_data'
    ),
  
  # Academics
  list(
    code = 'SCH_APENR',
    var_name = 'Enrolled in at least one AP Course',
    pattern = 'SCH_APENR_(HI|AM|AS|HP|BL|WH|TR)',
    source_file = 'advanced_placement'
    ),
  list(
    code = 'SCH_IBENR',
    var_name = 'International Baccalaureate Diploma Programme Enrollment',
    pattern = 'SCH_IBENR_(HI|AM|AS|HP|BL|WH|TR)',
    source_file = 'international_baccalaureate'
    ),
  list(
    code = 'SCH_MATHENR_ADVM',
    var_name = 'Enrolled in Advanced Mathematics',
    pattern = 'SCH_MATHENR_ADVM_(HI|AM|AS|HP|BL|WH|TR)',
    source_file = 'advanced_mathematics'
    ),
  list(
    code = 'SCH_SCIENR_PHYS',
    var_name = 'Enrolled in Physics',
    pattern = 'SCH_SCIENR_PHYS_(HI|AM|AS|HP|BL|WH|TR)',
    source_file = 'physics'
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

ethnicity_sex_disagg_vars <- data.frame(
  LEAID = character(),
  SCHID = character(),
  VAR_ORIGINAL = character(),
  STUDENT_COUNT = numeric(),
  VARIABLE_NAME = character(),
  VARIABLE_CODE = character(),
  ETHNICITY = character(),
  ETHNICITY_DESCR = character(),
  GENDER = character(),
  GENDER_DESCR = character()
)

for (var in ethnicity_sex_disagg_vars_to_extract) {
  tmp <- get(var$source_file) %>%
    #read.csv(paste0(source_dir, var$source_file)) %>%
    #filter(LEAID == '2711250' & SCHID == '510') %>% # Edina
    filter(LEA_STATE == 'CA' & SCHID == '7868') %>% # School in CA w/ lots of bullying
    select(
      LEAID,
      SCHID,
      matches(var$pattern)
    ) %>%
    gather(-LEAID, -SCHID, key = 'VAR_ORIGINAL', value='STUDENT_COUNT') %>%
    mutate(
      VARIABLE_NAME = var$var_name,
      VARIABLE_CODE = var$code,
      ETHNICITY = str_sub(VAR_ORIGINAL, -4, -3),
      ETHNICITY_DESCR = case_when(
        ETHNICITY == 'HI' ~ 'Hispanic',
        ETHNICITY == 'AM' ~ 'American Indian/Alaska Native',
        ETHNICITY == 'AS' ~ 'Asian',
        ETHNICITY == 'HP' ~ 'Native Hawaiian/Pacific Islander',
        ETHNICITY == 'BL' ~ 'Black',
        ETHNICITY == 'WH' ~ 'White',
        ETHNICITY == 'TR' ~ 'Two or More Races',
        TRUE ~ 'Unknown'
      ),
      GENDER = str_sub(VAR_ORIGINAL, -1),
      GENDER_DESCR = case_when(
        GENDER == 'M' ~ 'Male',
        GENDER == 'F' ~ 'Female',
        TRUE ~ 'Unknown'
      )
    )
    
    ethnicity_sex_disagg_vars <- bind_rows(tmp, ethnicity_sex_disagg_vars)
    rm(tmp)
}


# VARIABLE TYPE 2: ETHNICITY, SEX, DISABILITY STATUS DISAGGREGATED ####################################

ethnicity_sex_disability_disagg_vars_to_extract <- list(
  # Enrollment
  list(
    code = 'SCH_REF',
    var_name = 'Students referred to a law enforcement agency or official', 
    pattern = c('SCH_DISCWDIS_REF_(HI|AM|AS|HP|BL|WH|TR)', 'SCH_DISCWDIS_REF_IDEA_(HI|AM|AS|HP|BL|WH|TR)'),
    source_file = 'enrollment'
  )
)





# VARIABLE TYPE 3: OTHER ###############################################################################

school_vars_to_extract <- list(
  list(
    code = 'SCH_GRADE',
    var_name = 'Grades with Students Enrolled',
    pattern = 'SCH_GRADE',
    detail_list = list(
      'PS' = 'Preschool',
      'KG' = 'Kindergarten',
      'G01' = 'Grade 1',
      'G02' = 'Grade 2',
      'G03' = 'Grade 3',
      'G04' = 'Grade 4',
      'G05' = 'Grade 5',
      'G06' = 'Grade 6',
      'G07' = 'Grade 7',
      'G08' = 'Grade 8',
      'G09' = 'Grade 9',
      'G10' = 'Grade 10',
      'G11' = 'Grade 11', 
      'G12' = 'Grade 12',
      'UG' = 'Ungraded'
    ),
    source_file = 'school_characteristics'
  ),
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

school_vars <- data.frame(
  LEAID = character(),
  SCHID = character(),
  VAR_ORIGINAL = character(),
  COUNT = numeric(),
  VARIABLE_NAME = character(),
  VARIABLE_CODE = character(),
  VARIABLE_DETAIL_CODE = character(),
  VARIABLE_DETAIL_DESCR = character()
)

for (var in school_vars_to_extract) {
  tmp <- get(var$source_file) %>%
    filter(LEA_STATE == 'CA' & SCHID == '7868') %>% # School in CA w/ lots of bullying
    select(
      LEAID,
      SCHID,
      matches(var$pattern)
    ) %>%
    gather(-LEAID, -SCHID, key = 'VAR_ORIGINAL', value='COUNT') %>%
    mutate(
      VARIABLE_NAME = var$var_name,
      VARIABLE_CODE = var$code,
      VARIABLE_DETAIL_CODE = sub(paste0(var$pattern, '_'), '', VAR_ORIGINAL)
    ) %>%
    # Need this weird group_by hack to trick it into doing a list lookup, b/c list lookups don't
    # jive with dplyrs style of vectorization
    # https://stackoverflow.com/questions/38385536/accessing-list-elements-within-mutate
    group_by(VARIABLE_DETAIL_CODE) %>%
    mutate(
      VARIABLE_DETAIL_DESCR = var$detail_list[[unique(VARIABLE_DETAIL_CODE)]]
    ) %>%
    ungroup()
  
  school_vars <- bind_rows(tmp, school_vars)
  rm(tmp)
}



# ENROLLMENT DEMOGRAPHICS

# By ethnicity
# Counts
ethnicity_sex_disagg_vars %>%
  filter(VARIABLE_CODE %in% c('SCH_ENR', 'SCH_LEPENR', 'SCH_IDEAENR')) %>%
  arrange(VARIABLE_NAME) %>%
ggplot(., aes(x=VARIABLE_NAME, y=STUDENT_COUNT, fill=ETHNICITY_DESCR)) +
  geom_bar(stat="identity", position="stack")

# Proportions
ethnicity_sex_disagg_vars %>%
  filter(VARIABLE_CODE %in% c('SCH_ENR', 'SCH_LEPENR', 'SCH_IDEAENR')) %>%
  arrange(VARIABLE_NAME) %>%
  ggplot(., aes(x=VARIABLE_NAME, y=STUDENT_COUNT, fill=ETHNICITY_DESCR)) +
  geom_bar(stat="identity", position="fill") +
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank())

# By gender
ethnicity_sex_disagg_vars %>%
  filter(VARIABLE_CODE %in% c('SCH_ENR', 'SCH_LEPENR', 'SCH_IDEAENR')) %>%
  arrange(VARIABLE_NAME) %>%
  ggplot(., aes(x=VARIABLE_NAME, y=STUDENT_COUNT, fill=GENDER_DESCR)) +
  geom_bar(stat="identity", position="stack")

ethnicity_sex_disagg_vars %>%
  filter(VARIABLE_CODE %in% c('SCH_ENR', 'SCH_LEPENR', 'SCH_IDEAENR')) %>%
  arrange(VARIABLE_NAME) %>%
  ggplot(., aes(x=VARIABLE_NAME, y=STUDENT_COUNT, fill=GENDER_DESCR)) +
  geom_bar(stat="identity", position="fill")



## ACADEMIC OPPORTUNITY
## SIGNIFICANCE PLOTS

# Define a function that uses Fisher's exact test to do a vectorized comparison of proportions;
# spit out a result label that is one of the following:
# 1) 'base' (if the data on the row being compared represents the base category),
# 2) 'Significant Difference' (if the significance test's p-value is < 0.05), or 
# 3) 'No Significant Difference'
test_significance <- function(base_category, base_target_count, base_pop_count, comparison_category, comparison_target_count, comparison_pop_count) {
  base <- c(base_target_count, base_pop_count - base_target_count)
  comparison <- c(comparison_target_count, comparison_pop_count - comparison_target_count)
  contingency_table <- as.matrix(rbind(base, comparison))
  
  if (comparison_target_count < 0 | is.na(comparison_target_count)) {
    result <- NA
  } else if (base_category == comparison_category) {
    result <- 'base'
  } else {
    test_result <- fisher.test(contingency_table)
    result <- ifelse(test_result$p.value < 0.05, 'Significant Difference', 'No Significant Difference')
  }
  return(result)
}

# Prep data for significance tests

# Input: One row per ethnicity, gender
# Output: One row per ethnicity, with data from the base group (White) populated on each row

significance_test_by_ethnicity_data_transform <- function(base_ethnicity, ethnicity_sex_disagg_df, target_vars, pop_var) {
  # Get population counts for the base group; we'll join this to every row in our final df
  base_pop_data <- ethnicity_sex_disagg_df %>%
    filter(VARIABLE_CODE == pop_var & ETHNICITY == base_ethnicity) %>%
    group_by(LEAID, SCHID, ETHNICITY, ETHNICITY_DESCR) %>%
    summarise(STUDENT_COUNT = sum(STUDENT_COUNT)) %>%
    ungroup() %>%
    select(LEAID, SCHID, STUDENT_COUNT, ETHNICITY)
  
  all_pop_data <- ethnicity_sex_disagg_df %>%
    filter(VARIABLE_CODE == pop_var) %>%
    group_by(LEAID, SCHID, ETHNICITY, ETHNICITY_DESCR) %>%
    summarise(STUDENT_COUNT = sum(STUDENT_COUNT)) %>%
    select(LEAID, SCHID, STUDENT_COUNT)
  
  # Get target variable counts for base group for each target var; we'll join this to our final df, 
  # using VAR_CODE as a join condition so we get the right context for each variable
  base_target_data <- ethnicity_sex_disagg_df %>%
    filter(VARIABLE_CODE %in% target_vars & ETHNICITY == base_ethnicity) %>%
    group_by(VARIABLE_CODE, LEAID, SCHID, ETHNICITY, ETHNICITY_DESCR) %>%
    summarise(STUDENT_COUNT = sum(STUDENT_COUNT)) %>%
    ungroup() %>%
    select(VARIABLE_CODE, LEAID, SCHID, STUDENT_COUNT)
  
  # Set up the dataframe so it's ready for significance tests, joining:
  # 1) the base population data to each row
  # 2) the base group's count for each target variable to the all corresponding target variable rows
  comparison_df <- ethnicity_sex_disagg_df %>%
    filter(VARIABLE_CODE %in% target_vars) %>%
    group_by(VARIABLE_CODE, VARIABLE_NAME, LEAID, SCHID, ETHNICITY, ETHNICITY_DESCR) %>%
    summarise(STUDENT_COUNT = sum(STUDENT_COUNT)) %>%
    inner_join(base_pop_data, by=c('LEAID', 'SCHID'), suffix=c('', '_BASE_POP')) %>%
    inner_join(all_pop_data, by=c('LEAID', 'SCHID', 'ETHNICITY'), suffix=c('', '_COMPARISON_POP')) %>%
    inner_join(base_target_data, by=c('LEAID', 'SCHID', 'VARIABLE_CODE'), suffix=c('', '_BASE_TARGET_VAR'))
  
  comparison_df <- comparison_df %>%
    mutate(
      SIG_DIFFERENCE = test_significance(ETHNICITY_BASE_POP, STUDENT_COUNT_BASE_TARGET_VAR, STUDENT_COUNT_BASE_POP, ETHNICITY, STUDENT_COUNT, STUDENT_COUNT_COMPARISON_POP)
    )
}

sig_test_by_ethnicity_data <- significance_test_by_ethnicity_data_transform('BL', ethnicity_sex_disagg_vars, c('SCH_MATHENR_ADVM', 'SCH_SCIENR_PHYS', 'SCH_APENR', 'SCH_IBENR'), 'SCH_ENR')


# Significance test plots

sig_test_by_ethnicity_data %>%
  mutate(
    PCT_OF_STDNT_WITHIN_COMPARISON_POP_WITHIN_TGT_VAR = round(ifelse(STUDENT_COUNT < 0, 0, STUDENT_COUNT) / STUDENT_COUNT_COMPARISON_POP * 100, 3),
    PCT_OF_STDNT_WITHIN_BASE_POP_WITHIN_TGT_VAR = round(ifelse(STUDENT_COUNT_BASE_TARGET_VAR < 0, 0, STUDENT_COUNT_BASE_TARGET_VAR) / STUDENT_COUNT_BASE_POP * 100, 3)
    ) %>%
ggplot(., aes(x=PCT_OF_STDNT_WITHIN_COMPARISON_POP_WITHIN_TGT_VAR, y=ETHNICITY_DESCR, col=ETHNICITY_DESCR)) +
  geom_point(aes(shape=SIG_DIFFERENCE), size = 5) +
  geom_vline(aes(xintercept = PCT_OF_STDNT_WITHIN_BASE_POP_WITHIN_TGT_VAR)) +
  scale_shape_manual(values = c(15, 1, 8)) +
  facet_grid(VARIABLE_NAME ~ .) +
  xlab("Percent of students within ethnicity group with target characteristic") +
  ylab(NULL)



# SCHOOL CLIMATE

# Harassment/bullying reports are very rare
harassment_and_bullying %>%
  #filter(TOT_HBREPORTED_RAC_M = 0 & TOT_HBREPORTED_RAC_F = 0) %>%
  group_by(LEA_STATE) %>%
  summarise(
    schools_w_no_bullying = sum(ifelse(TOT_HBREPORTED_RAC_M == 0 & TOT_HBREPORTED_RAC_F == 0, 1, 0)),
    n = n(),
    mean = mean(TOT_HBREPORTED_RAC_M + TOT_HBREPORTED_RAC_F),
    max = max(TOT_HBREPORTED_RAC_M + TOT_HBREPORTED_RAC_F),
    min = min(TOT_HBREPORTED_RAC_M + TOT_HBREPORTED_RAC_F),
    min(ifelse(SCHID== '99999', NA, SCHID)),
    max(ifelse(SCHID== '99999', NA, SCHID))
    )

# AL, SCHID = 2398
harassment_and_bullying %>%
  filter(LEA_STATE == 'CA' & (TOT_HBREPORTED_RAC_M + TOT_HBREPORTED_RAC_F) > 10) %>%
  select(SCHID)

ethnicity_sex_disagg_vars %>%
  filter(VARIABLE_CODE %in% c('SCH_HBREPORTED_RAC', 'SCH_HBREPORTED_DIS')) %>%
ggplot(., aes(x=ETHNICITY_DESCR, y=STUDENT_COUNT, fill=GENDER_DESCR)) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ VARIABLE_NAME) +
  labs(x=NULL, y="Student count") +
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1))


school_vars %>%
  filter(VARIABLE_CODE == 'SCH_HBALLEGATIONS') %>%
ggplot(., aes(x=VARIABLE_DETAIL_DESCR, y=COUNT)) +
  geom_bar(stat='identity') +
  ylim(c(0, NA)) +
  facet_wrap(~VARIABLE_NAME) +
  labs(x=NULL, y="Count of allegations") +
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  coord_flip()

school_vars %>%
  filter(VARIABLE_CODE == 'SCH_OFFENSE') %>%
ggplot(., aes(x=VARIABLE_DETAIL_DESCR, y=COUNT)) +
  geom_bar(stat='identity') +
  ylim(c(0, NA)) +
  facet_wrap(~VARIABLE_NAME) +
  labs(x=NULL, y="Count of incidents") +
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  coord_flip()



# STUDENT SUPPORT

# Find all other schools of similar level in the state for comparison

school_support_per_student_state_avg <- school_characteristics %>%
  filter(LEA_STATE == 'MN' & high_school == 'Yes') %>%
  inner_join(enrollment %>% 
         mutate(TOTAL_ENROLLMENT = TOT_ENR_M + TOT_ENR_F) %>% 
         select(LEA_STATE, LEAID, SCHID, TOTAL_ENROLLMENT), 
       by=c('LEAID', 'SCHID')) %>%
  inner_join(school_support , by=c('LEAID', 'SCHID')) %>%
  mutate(
    RATIO_STUDENT_TO_TEACHER = ifelse(SCH_FTETEACH_TOT <= 0, 0, (TOTAL_ENROLLMENT /  SCH_FTETEACH_TOT)) ,
    RATIO_STUDENT_TO_COUNSELOR = ifelse(SCH_FTECOUNSELORS <= 0, 0, (TOTAL_ENROLLMENT /  SCH_FTECOUNSELORS)),
    RATIO_STUDENT_TO_LAWENFOFFICER = ifelse(SCH_FTESECURITY_LEO <= 0, 0, (TOTAL_ENROLLMENT / SCH_FTESECURITY_LEO)),
    RATIO_STUDENT_TO_SECURITYGUARD = ifelse(SCH_FTESECURITY_GUA <= 0, 0, (TOTAL_ENROLLMENT / SCH_FTESECURITY_GUA)),
    RATIO_STUDENT_TO_NURSE = ifelse(SCH_FTESERVICES_NUR <= 0, 0, (TOTAL_ENROLLMENT / SCH_FTESERVICES_NUR)),
    RATIO_STUDENT_TO_PSYCHOLOGIST = ifelse(SCH_FTESERVICES_PSY <= 0, 0, (TOTAL_ENROLLMENT / SCH_FTESERVICES_PSY)),
    RATIO_STUDENT_TO_SOCIALWORKER = ifelse(SCH_FTESERVICES_SOC <= 0, 0, (TOTAL_ENROLLMENT / SCH_FTESERVICES_SOC))
  ) %>%
  select(LEA_STATE, LEAID, SCHID, TOTAL_ENROLLMENT, starts_with('RATIO_')) %>%
  gather(-LEA_STATE, -LEAID, -SCHID, -TOTAL_ENROLLMENT, key='VAR_ORIGINAL', value='STAFFING_RATIO') %>%
  mutate(
    LEVEL_OF_DETAIL = 'Statewide Average',
    VARIABLE_CODE = 'RATIO_STUDENT_TO',
    VARIABLE_NAME = 'Student to staff ratio',
    VARIABLE_DETAIL_CODE = str_replace(VAR_ORIGINAL, 'RATIO_STUDENT_TO_', ''),
    VARIABLE_DETAIL_DESCR = case_when(
      VARIABLE_DETAIL_CODE == 'TEACHER' ~ 'Teacher',
      VARIABLE_DETAIL_CODE == 'COUNSELOR' ~ 'School counselor',
      VARIABLE_DETAIL_CODE == 'LAWENFOFFICER' ~ 'Law enforcement officer',
      VARIABLE_DETAIL_CODE == 'SECURITYGUARD' ~ 'Security guard',
      VARIABLE_DETAIL_CODE == 'NURSE' ~ 'Nurse',
      VARIABLE_DETAIL_CODE == 'PSYCHOLOGIST' ~ 'Psychologist',
      VARIABLE_DETAIL_CODE == 'SOCIALWORKER' ~ 'Social worker',
      TRUE ~ as.character(VARIABLE_DETAIL_CODE)
    )
  ) %>%
  group_by(LEA_STATE, LEVEL_OF_DETAIL, VAR_ORIGINAL, VARIABLE_CODE, VARIABLE_NAME, VARIABLE_DETAIL_CODE, VARIABLE_DETAIL_DESCR) %>%
  summarise(STAFFING_RATIO = mean(STAFFING_RATIO))

school_support_per_student <- enrollment %>%
  filter(LEAID == '2711250' & SCHID == '510') %>%
  mutate(TOTAL_ENROLLMENT = TOT_ENR_M + TOT_ENR_F) %>%
  select(LEAID, SCHID, TOTAL_ENROLLMENT) %>%
  inner_join(school_support , by=c('LEAID', 'SCHID')) %>%
  mutate(
    RATIO_STUDENT_TO_TEACHER = TOTAL_ENROLLMENT / SCH_FTETEACH_TOT ,
    RATIO_STUDENT_TO_COUNSELOR = TOTAL_ENROLLMENT / SCH_FTECOUNSELORS,
    RATIO_STUDENT_TO_LAWENFOFFICER = TOTAL_ENROLLMENT / SCH_FTESECURITY_LEO,
    RATIO_STUDENT_TO_SECURITYGUARD = TOTAL_ENROLLMENT / SCH_FTESECURITY_GUA,
    RATIO_STUDENT_TO_NURSE = TOTAL_ENROLLMENT / SCH_FTESERVICES_NUR,
    RATIO_STUDENT_TO_PSYCHOLOGIST = TOTAL_ENROLLMENT / SCH_FTESERVICES_PSY,
    RATIO_STUDENT_TO_SOCIALWORKER = TOTAL_ENROLLMENT / SCH_FTESERVICES_SOC
  ) %>%
  select(LEAID, SCHID, TOTAL_ENROLLMENT, starts_with('RATIO_')) %>%
  gather(-LEAID, -SCHID, -TOTAL_ENROLLMENT, key='VAR_ORIGINAL', value='STAFFING_RATIO') %>%
  mutate(
    LEVEL_OF_DETAIL = 'Selected School',
    VARIABLE_CODE = 'RATIO_STUDENT_TO',
    VARIABLE_NAME = 'Student to staff ratio',
    VARIABLE_DETAIL_CODE = str_replace(VAR_ORIGINAL, 'RATIO_STUDENT_TO_', ''),
    VARIABLE_DETAIL_DESCR = case_when(
      VARIABLE_DETAIL_CODE == 'TEACHER' ~ 'Teacher',
      VARIABLE_DETAIL_CODE == 'COUNSELOR' ~ 'School counselor',
      VARIABLE_DETAIL_CODE == 'LAWENFOFFICER' ~ 'Law enforcement officer',
      VARIABLE_DETAIL_CODE == 'SECURITYGUARD' ~ 'Security guard',
      VARIABLE_DETAIL_CODE == 'NURSE' ~ 'Nurse',
      VARIABLE_DETAIL_CODE == 'PSYCHOLOGIST' ~ 'Psychologist',
      VARIABLE_DETAIL_CODE == 'SOCIALWORKER' ~ 'Social worker',
      TRUE ~ as.character(VARIABLE_DETAIL_CODE)
    )
  )

school_support_per_student_w_state_avg <- school_support_per_student %>%
  select(-LEAID, -SCHID, -TOTAL_ENROLLMENT) %>%
  bind_rows(school_support_per_student_state_avg %>% select(-LEA_STATE))


ggplot(school_support_per_student_w_state_avg, aes(x=NA, y=STAFFING_RATIO, fill=LEVEL_OF_DETAIL)) +
  geom_bar(stat="identity", position="dodge") +
  facet_grid(~VARIABLE_DETAIL_DESCR, scales="free") +
  scale_fill_manual(values=c('#ffcc33', '#cccccc'), name=NULL) +
  coord_flip() +
  labs(
    x=NULL,
    y="Student to staff ratio (lower is better)"
  ) +  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())


  
# Pct of teaching staff that is in their first few years of teaching
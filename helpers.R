# ACADEMIC OPPORTUNITIES COMPARISON

# STEP 1: Define a helper function that uses Fisher's exact test to do a vectorized comparison of proportions;
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
    result <- 'Base Group'
  } else {
    test_result <- fisher.test(contingency_table)
    result <- ifelse(test_result$p.value < 0.05, 'Significant Difference from Base Group', 'No Significant Difference from Base Group')
  }
  return(result)
}

# STEP 2: Define a function to transform data and conduct significance tests using helper function from above

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
    inner_join(base_target_data, by=c('LEAID', 'SCHID', 'VARIABLE_CODE'), suffix=c('', '_BASE_TARGET_VAR')) %>%
    mutate(SIG_DIFFERENCE = test_significance(ETHNICITY_BASE_POP, STUDENT_COUNT_BASE_TARGET_VAR, STUDENT_COUNT_BASE_POP, ETHNICITY, STUDENT_COUNT, STUDENT_COUNT_COMPARISON_POP))
}
library(shiny)
library(gridExtra)
source("helpers.R")

server <- function(input, output) {
  
  ##################
  ## UI SELECTORS ##
  ##################
  
  # LEA selector
  # Note: We want to send a named list to the selectInput, so it will use the names for display and the IDs for selecting 
  output$leaSelector <- renderUI({
    leas <- lea_data %>%
      filter(LEA_STATE == input$selected_state) %>%
      arrange(LEA_NAME) %>%
      summarise(lea_list = list(as.list(setNames(LEAID, LEA_NAME))))

    selectInput("selected_lea_id", "Choose a District/LEA:", leas$lea_list[[1]])
  })
  
  # School selector
  output$schoolSelector <- renderUI({
    schools <- school_characteristics %>%
      select(LEA_STATE, LEAID, SCHID, SCH_NAME) %>%
      filter(LEA_STATE == input$selected_state) %>%
      filter(LEAID == input$selected_lea_id) %>%
      # filter(if (!input$elementaryschool) (SCH_GRADE_G01 == 'No' & SCH_GRADE_G02 == 'No' & SCH_GRADE_G03 == 'No' & SCH_GRADE_G04 == 'No' & SCH_GRADE_G05 == 'No') else TRUE) %>%
      # filter(if (!input$middleschool) (SCH_GRADE_G06 == 'No' & SCH_GRADE_G07 == 'No' & SCH_GRADE_G08 & 'No') else TRUE ) %>%
      # filter(if (!input$highschool) (SCH_GRADE_G09 == 'No' & SCH_GRADE_G10 == 'No' & SCH_GRADE_G11 == 'No' & SCH_GRADE_G12 == 'No') else TRUE ) %>%
      # filter(if (!input$otherschool) (SCH_GRADE_PS == 'No' & SCH_GRADE_UG == 'No') else TRUE ) %>%
      # filter(if (!input$magnetcharter) (SCH_STATUS_MAGNET == 'No' & SCH_STATUS_CHARTER == 'No') else TRUE ) %>%
      # filter(if (!input$altspecialed) (SCH_STATUS_ALT == 'No' & SCH_STATUS_SPED == 'No') else TRUE ) %>%
      arrange(SCH_NAME) %>%
      summarise(school_list = list(as.list(setNames(SCHID, SCH_NAME))))

    selectInput("selected_school_id", "Choose a School:", schools$school_list[[1]])
  })
  
  # Confirm choices
  output$selected_lea <- renderText({ paste("Selected LEAID:", input$selected_lea_id ) })
  output$selected_school <- renderText({ paste("Selected SCHID:", input$selected_school_id ) })
  
  
  ##############
  ## GET DATA ##
  ##############
  
  # Extract data when school is selected
  
  # METADATA
  
  school_metadata <- reactive({
    school_characteristics %>%
      filter(LEAID == input$selected_lea_id & SCHID == input$selected_school_id) %>%
      mutate(
        elementary_school = ifelse(SCH_GRADE_G01 == 'Yes' | SCH_GRADE_G02 == 'Yes' | SCH_GRADE_G03 == 'Yes' | SCH_GRADE_G04 == 'Yes' | SCH_GRADE_G05 == 'Yes', 'Yes', 'No'),
        middle_school = ifelse(SCH_GRADE_G06 == 'Yes' | SCH_GRADE_G07 == 'Yes' | SCH_GRADE_G08 == 'Yes', 'Yes', 'No'),
        high_school = ifelse(SCH_GRADE_G09 == 'Yes' | SCH_GRADE_G10 == 'Yes' | SCH_GRADE_G11 == 'Yes' | SCH_GRADE_G12 == 'Yes', 'Yes', 'No')
      )
  })
  
  output$schoolmeta_elementary <- renderText({
    paste("Elementary grade levels?:", school_metadata()$elementary_school)
  })
  
  output$schoolmeta_middleschool <- renderText({
    paste("Middle school grade levels?:", school_metadata()$middle_school)
  })
  
  output$schoolmeta_highschool <- renderText({
    paste("High school grade levels?:", school_metadata()$high_school)
  })
  
  output$schoolmeta_specialed <- renderText({
    paste("Special Education School:", school_metadata()$SCH_STATUS_SPED)
  })
  
  output$schoolmeta_alternative <- renderText({
    if (school_metadata()$SCH_STATUS_ALT == 'Yes') {
      paste("Alternative School:", school_metadata()$SCH_STATUS_ALT, '(', school_metadata()$SCH_ALTFOCUS, ')', sep = ' ')
    } else {
      paste("Alternative School:", school_metadata()$SCH_STATUS_ALT, sep = ' ')
    }
  })
  
  output$schoolmeta_charter <- renderText({
    paste("Charter School:", school_metadata()$SCH_STATUS_CHARTER, sep=' ')
  })
  
  output$schoolmeta_magnet <- renderText({
    if (school_metadata()$SCH_STATUS_MAGNET == 'Yes') {
      paste("Magnet School:", school_metadata()$SCH_STATUS_MAGNET, '(', school_metadata()$SCH_MAGNETDETAIL, ')', sep=' ')
    } else {
      paste("Magnet School:", school_metadata()$SCH_STATUS_MAGNET, sep = ' ')
    }
  })
  
  # ETHNICITY + SEX
  
  ethnicity_sex_disagg_df <- reactive({
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
        filter(LEAID == input$selected_lea_id & SCHID == input$selected_school_id) %>%
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
            ETHNICITY == 'AM' ~ 'Am. Ind./Alaska Nat.',
            ETHNICITY == 'AS' ~ 'Asian',
            ETHNICITY == 'HP' ~ 'Nat. Hawaiian/Pacific Is.',
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
    return(ethnicity_sex_disagg_vars)
  })
  
  
  # SCHOOL
  
  school_df <- reactive({
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
        filter(LEAID == input$selected_lea_id & SCHID == input$selected_school_id) %>%
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
    return(school_vars)
  })
  
  
  
  #############################
  ## ENROLLMENT DEMOGRAPHICS ##
  #############################
  
  # By Ethnicity
  output$enrollment_by_ethnicity <- renderPlot({
    p1 <- ethnicity_sex_disagg_df() %>%
      filter(VARIABLE_CODE %in% c('SCH_ENR', 'SCH_LEPENR', 'SCH_IDEAENR')) %>%
      arrange(VARIABLE_NAME) %>%
      ggplot(., aes(x=VARIABLE_NAME, y=STUDENT_COUNT, fill=ETHNICITY_DESCR)) +
      geom_bar(stat="identity", position="stack") +
      labs(
        x = NULL, 
        y = "Count of Students"
      ) +
      coord_flip() +
      theme(legend.position = "none")
    
    p2 <- ethnicity_sex_disagg_df() %>%
      filter(VARIABLE_CODE %in% c('SCH_ENR', 'SCH_LEPENR', 'SCH_IDEAENR')) %>%
      arrange(VARIABLE_NAME) %>%
      ggplot(., aes(x=VARIABLE_NAME, y=STUDENT_COUNT, fill=ETHNICITY_DESCR)) +
      geom_bar(stat="identity", position="fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        x = NULL, 
        y = "Percent of Students",
        fill = "Student Ethnicity"
      ) +
      coord_flip() +
      theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
    
    grid.arrange(p1, p2, ncol=2)
  })
  
  # By Gender
  output$enrollment_by_gender <- renderPlot({
    p1 <- ethnicity_sex_disagg_df() %>%
      filter(VARIABLE_CODE %in% c('SCH_ENR', 'SCH_LEPENR', 'SCH_IDEAENR')) %>%
      arrange(VARIABLE_NAME) %>%
      ggplot(., aes(x=VARIABLE_NAME, y=STUDENT_COUNT, fill=GENDER_DESCR)) +
      geom_bar(stat="identity", position="stack") +
      labs(
        x = NULL, 
        y = "Count of Students"
      ) +
      coord_flip() +
      theme(legend.position = "none")
    
    p2 <- ethnicity_sex_disagg_df() %>%
      filter(VARIABLE_CODE %in% c('SCH_ENR', 'SCH_LEPENR', 'SCH_IDEAENR')) %>%
      arrange(VARIABLE_NAME) %>%
      ggplot(., aes(x=VARIABLE_NAME, y=STUDENT_COUNT, fill=GENDER_DESCR)) +
      geom_bar(stat="identity", position="fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        x = NULL, 
        y ="Percent of Students",
        fill = "Student Gender"
      ) +
      coord_flip() +
      theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
    
    grid.arrange(p1, p2, ncol=2)
  })
  
  
  #######################################
  ## ACADEMIC OPPORTUNITIES COMPARISON ##
  #######################################
  
  # STEP 1 & 2: See helpers.R ...
  # STEP 3: Reactive function to pipe data through significance test pipeline and create plots
  
  output$academic_opportunities_comparison_plot <- renderPlot({
    prepped_data <- significance_test_by_ethnicity_data_transform(
      input$selected_ethnicity_base_group, # based on user selection in UI
      ethnicity_sex_disagg_df(),
      c('SCH_GTENR', 'SCH_APENR', 'SCH_IBENR', 'SCH_MATHENR_CALC', 'SCH_SCIENR_PHYS', 'SCH_SCIENR_CHEM'), # Academic opportunities variables
      'SCH_ENR' # Overall Enrollment variable
      )
    
    prepped_data %>%
      mutate(
        PCT_OF_STDNT_WITHIN_COMPARISON_POP_WITHIN_TGT_VAR = round(ifelse(STUDENT_COUNT < 0, 0, STUDENT_COUNT) / STUDENT_COUNT_COMPARISON_POP * 100, 3),
        PCT_OF_STDNT_WITHIN_BASE_POP_WITHIN_TGT_VAR = round(ifelse(STUDENT_COUNT_BASE_TARGET_VAR < 0, 0, STUDENT_COUNT_BASE_TARGET_VAR) / STUDENT_COUNT_BASE_POP * 100, 3)
      ) %>%
      ggplot(., aes(x=PCT_OF_STDNT_WITHIN_COMPARISON_POP_WITHIN_TGT_VAR, y=ETHNICITY_DESCR, col=ETHNICITY_DESCR)) +
      geom_point(aes(shape=SIG_DIFFERENCE), size = 5) +
      geom_vline(aes(xintercept = PCT_OF_STDNT_WITHIN_BASE_POP_WITHIN_TGT_VAR)) +
      scale_shape_manual(values = c(15, 16, 17)) +
      facet_grid(VARIABLE_NAME ~ .) +
      labs(
        x = "Percent of students within ethnicity group with target characteristic",
        y = NULL,
        colour = 'Student Ethnicity',
        shape ="Statistically significant difference?"
        )
  })
  
  
  ####################
  ## SCHOOL CLIMATE ##
  ####################
  
  output$allegations_plot <- renderPlot({
    school_df() %>%
      filter(VARIABLE_CODE == 'SCH_HBALLEGATIONS') %>%
    ggplot(., aes(x=VARIABLE_DETAIL_DESCR, y=COUNT)) +
      geom_bar(stat='identity') +
      ylim(c(0, NA)) +
      facet_wrap(~VARIABLE_NAME) +
      labs(x=NULL, y="Count of allegations") +
      theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      coord_flip()
  })
  
  output$incident_plot <- renderPlot({
    school_df() %>%
      filter(VARIABLE_CODE == 'SCH_OFFENSE') %>%
    ggplot(., aes(x=VARIABLE_DETAIL_DESCR, y=COUNT)) +
      geom_bar(stat='identity') +
      ylim(c(0, NA)) +
      facet_wrap(~VARIABLE_NAME) +
      labs(x=NULL, y="Count of incidents") +
      theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      coord_flip()
  })
  
  
  #############################
  ## STUDENT SUPPORT PROFILE ##
  #############################
  
  
  
  
}
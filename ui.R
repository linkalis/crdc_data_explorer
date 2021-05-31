library(shiny)
library(shinydashboard)

ui <- dashboardPage(skin = "yellow",
  
  dashboardHeader(title = "CRDC Data Explorer"),
  
  dashboardSidebar(
      
      selectInput("selected_state", "Select a State:", choices = unique(lea_data$LEA_STATE), selected = 'MN'),
      
      uiOutput("leaSelector"),
      uiOutput("schoolSelector"),
      
      # p("Filter schools:"),
      # checkboxInput('elementaryschool', 'Elementary (1-5)', value = TRUE),
      # checkboxInput('middleschool', 'Middle school (6-8)', value = TRUE),
      # checkboxInput('highschool', 'High school (9-12)', value = TRUE),
      # checkboxInput('otherschool', 'Other level', value = TRUE),
      # checkboxInput('magnetcharter', 'Magnet / charter', value = TRUE),
      # checkboxInput('altspecialed', 'Alternative / special ed', value = TRUE),
      
      hr(),
      selectInput(
        "selected_ethnicity_base_group", "Select which ethnicity group to use as a base for comparisons:",
        choices = list(
          'Hispanic' = 'HI', 
          'American Indian/Alaska Native' = 'AM',
          'Asian' = 'AS',
          'Native Hawaiian/Pacific Islander' = 'HP',
          'Black' = 'BL',
          'White' = 'WH',
          'Two or More Races' = 'TR'
          ),
        selected = 'WH'
        ),
      
      hr(),
      p("Data source: "),
      tags$a(href = "https://www2.ed.gov/about/offices/list/ocr/docs/crdc-2017-18.html",
        "U.S. Department of Education, Civil Rights Data Collection, 2017-2018.")
        ),
    
    dashboardBody(
      h2("School Characteristics"),
      fluidRow(
        column(width = 6, 
               textOutput("selectedLea"),
               textOutput("selectedSchool"),
               textOutput("schoolmetaElementary"),
               textOutput("schoolmetaMiddleschool"),
               textOutput("schoolmetaHighschool")
               ),
        column(width = 6,
               textOutput("schoolmetaSpecialed"),
               textOutput("schoolmetaAlternative"),
               textOutput("schoolmetaCharter"),
               textOutput("schoolmetaMagnet")
               )
      ),
      
      h2("Enrollment Demographics"),
      # By Ethnicity
      box(
        width = 12, 
        title = "By ethnicity",
        plotOutput("enrollment_by_ethnicity", height = '200px')
      ),
      # By Gender
      box(
        width = 12,
        title = "By gender",
        plotOutput("enrollment_by_gender", height = '200px')
      ),
      
      h2("Academic Opportunities Comparison"),
      p("Are all students in the school similarly represented in the school's academic opportunities? 
        If students are equitably represented, we expect the percentage of students in each 
        academic opportunity to be roughly the same across ethnicities.  If this is the case,
        then", strong("the points in each chart below should all lie along the same line"),".  If students are
        not equitably represented, we will see certain groups lying above or below the 'base' line."),
      p("The 'triangle' symbol denotes places whether there is enough evidence to suggest that 
        there is likely a statistically meaningful difference between a particular ethnicity's 
        representation in an opportunity, compared to the 'base' group's participation in that same 
        opportunity. The 'circle' symbol denotes places where subgroups of students are too small to draw 
        statistically meaningful conclusions from.  Use the selector on the side panel to select the 
        base group used for comparison."),
      box(
        width = 12, 
        title = "Enrollment rates in school academic opportunities",
        plotOutput("academic_opportunities_comparison_plot", height = '700px')
      ),
      
      h2("Student Support Profile"),
      p("How does the school's staffing levels compare to other comparable schools across the state?  
        The yellow bars below show the selected school's student-to-staff ratios for various support staff roles.
        The comparison group--represented by grey bars below--includes data from all schools within the state that
        that share the same school characteristics and that have the respective support role staffed at
        some level.  (Note: Schools that do not have a particular support role staffed do not factor into the
        comparison group when calculating median staffing levels for a given role.)"),
      p(strong("Lower student-to-staff ratios are better."), " Yellow bars that are ", em("shorter than"),
        "the comparison grey bars indicate that that the selected school has ", em("more"), 
        "staff available per student than the state median staffing level for a given role.  Yellow bars that are ",
        em("longer than"), "the comparison grey bars indicate that the selected school has ", em("fewer"), 
        "staff available per student than the state median staffing level for a given role."),
      box(
        width = 12, 
        title = "Student-to-staff ratios for school staff roles",
        plotOutput("student_support_plot", height = '150px')
      ),
      
      h2("School Climate"),
      p("How many allegations of bullying and/or incidents of violence were reported during the annual reporting
        period?  These are cumulative counts based on the entire 2017-18 regular school year, not including 
        intersession or summer."),  
      p(strong("Allegations of harassment or bullying: "), "Allegations can be reported by anyone (e.g., alleged victim; 
        parents of alleged victim). The harassment or bullying can be carried out by students, school employees, 
        or non-employee third parties. Alleged victims must be students.)"),
      p(strong("Incidents: "), "Includes incidents of violence that occurred before, during, or after normal school hours.
        Counts incidents regardless of whether any disciplinary action was taken, and regardless of whether 
        students or non-students were involved."),
      box(width = 5, plotOutput("allegations_plot", height = '200px')),
      box(width = 7, plotOutput("incident_plot", height = '200px'))
    )
)
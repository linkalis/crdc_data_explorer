library(shiny)

ui <- fluidPage(
  
  titlePanel("Dept of Ed - Civil Rights Data Collection - Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
      #"Select a state, district, and school:",
      
      selectInput("selected_state", "Select a State:", choices = unique(lea_data$LEA_STATE), selected = 'MN'),
      
      uiOutput("leaSelector"),
      uiOutput("schoolSelector"),
      
      p("Filter schools:"),
      checkboxInput('elementaryschool', 'Elementary (K-5)', value = TRUE),
      checkboxInput('middleschool', 'Middle school (6-8)', value = TRUE),
      checkboxInput('highschool', 'High school (9-12)', value = TRUE),
      checkboxInput('otherschool', 'Other level', value = TRUE),
      checkboxInput('magnetcharter', 'Magnet / charter', value = TRUE),
      checkboxInput('altspecialed', 'Alternative / special ed', value = TRUE),
      
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
        )
      ),
    
    
    mainPanel(
      fluidRow(
        column(width = 6, 
               textOutput("selected_lea"),
               textOutput("selected_school"),
               textOutput("schoolmeta_elementary"),
               textOutput("schoolmeta_middleschool"),
               textOutput("schoolmeta_highschool")
               ),
        column(width = 6,
               textOutput("schoolmeta_specialed"),
               textOutput("schoolmeta_alternative"),
               textOutput("schoolmeta_charter"),
               textOutput("schoolmeta_magnet")
               )
      ),
      
      hr(),
      h2("Enrollment Demographics"),
      # By Ethnicity
      fluidRow(
        column(width = 12, plotOutput("enrollment_by_ethnicity", height = '200px'))
      ),
      # By Gender
      fluidRow(
        column(width = 12, plotOutput("enrollment_by_gender", height = '200px'))
      ),
      
      hr(),
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
      fluidRow(
        column(width = 12, plotOutput("academic_opportunities_comparison_plot", height = '700px'))
      ),
      
      hr(),
      h2("School Climate"),
      fluidRow(
        column(width = 6, plotOutput("allegations_plot")),
        column(width = 6, plotOutput("incident_plot"))
      ),
      
      hr(),
      h2("Student Support Profile"),
      # per capita counts...
      
      
    )
  )
  
)
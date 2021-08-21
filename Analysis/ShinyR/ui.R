library(shiny)
library(shinythemes)

# This ShinyR application captures the trend of vaccination rate at census tracts and 
# counties of the Wisconsin state. Interventions in the form of vaccine encouragement 
# programs are collected and analysed to find their significance in the vaccine rate.

shinyUI(
  
    # The CSS theme considered is "simplex" from shinythemes
    fluidPage(theme=shinytheme("simplex"),
    
    titlePanel("Vaccine Encouragements in Wisconsin"),
    
    navbarPage("",
     
    tabPanel("Vaccinations",
    
    # This is a placeholder for adding some text about Vaccinations in Wisconsin
    p(style="text-align: justify;", "Lorem ipsum dolor sit amet, consectetur 
      adipiscing elit. Suspendisse pretium enim odio, eu suscipit dolor fringilla et. 
      In ipsum nisi, ornare vitae purus at, maximus sodales nibh. Pellentesque 
      bibendum lectus et nisl tristique, et efficitur nulla aliquam. Sed sit amet 
      dui ut dui vestibulum sodales. Cras tempor nisi efficitur leo malesuada, eget 
      volutpat lorem vulputate. Suspendisse in pulvinar est. Phasellus feugiat ut 
      est et lacinia."),
    
    sidebarLayout( 
    
    sidebarPanel(
        
    # The inputs and outputs in tab1, tab2 and tab3 are named with _1, _2 and _3 respectively
    selectInput(inputId = 'type_1',
                label = "Geography Type",
                choices = c("Census Tracts", "Counties")), 
    
    # The choices in Location will be refilled according to the specific geography 
    # type selected
    selectInput(inputId = 'loc_1',
                label = "Location",
                choices = c("Loading")),
    
    radioButtons("gender_1", "Gender", c("All" = "all", 
                                         "Male" = "male", "Female" = "female"), 
                                          inline=T),
    
    radioButtons("age_1", "Age group", c("All" = "all_age", "Above 65" = "a65", 
                                         "Below 18" = "b18"), inline=T),
    
    radioButtons("ethnicity_1", "Hispanic Ethnicity", c("All" = "all_eth", 
                                                        "Yes" = "yes", "No" = "no", 
                                                        "Unknown"="unknown"), inline=T),
    
    selectInput(inputId = 'race_1',
                label = "Racial group",
                choices = c("All", "White", "Other Race", "Asian", "Unknown", "American Indian or Alaska Native", "Black or African-American", "Native Hawaiian or Other Pacific Islander")),
    
    actionButton("submit_1", "Run")),
  
    mainPanel( plotOutput("plot_1"), uiOutput("text_1"))
    )),
    
    tabPanel("Encouragements",
             HTML("<p align=justify>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse pretium enim odio, eu suscipit dolor fringilla et. In ipsum nisi, ornare vitae purus at, maximus sodales nibh. Pellentesque bibendum lectus et nisl tristique, et efficitur nulla aliquam. Sed sit amet dui ut dui vestibulum sodales. Cras tempor nisi efficitur leo malesuada, eget volutpat lorem vulputate. Suspendisse in pulvinar est. Phasellus feugiat ut est et lacinia.
</p>
                <h5><i>Sample Sub heading</i></h5>
                <ul><li>Lorem ipsum dolor sit amet</li><li>consectetur adipiscing elit</li></ul>
                <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse pretium enim odi.</p>"
              ),
     sidebarLayout( 
               
     sidebarPanel(
                 
     selectInput(inputId = 'type_2',
                label = "Geography Type",
                choices = c("Census Tracts", "Counties")), 
                 
     selectInput(inputId = 'loc_2',
                label = "Location",
                choices = c("Loading")),
                 
     actionButton("submit_2", "Run")),
               
     mainPanel( dataTableOutput('table_2'), uiOutput("text_2"))
             
    )),
    
    tabPanel("Encouragement Analysis",
             p(style="text-align: justify;", "Lorem ipsum dolor sit amet, 
               consectetur adipiscing elit. Suspendisse pretium enim odio, eu 
               suscipit dolor fringilla et. In ipsum nisi, ornare vitae purus at, 
               maximus sodales nibh. Pellentesque bibendum lectus et nisl tristique, 
               et efficitur nulla aliquam. Sed sit amet dui ut dui vestibulum 
               sodales. Cras tempor nisi efficitur leo malesuada, eget volutpat 
               lorem vulputate. Suspendisse in pulvinar est. Phasellus feugiat ut 
               est et lacinia."),
             
     sidebarLayout( 
               
     sidebarPanel(
                 
     selectInput(inputId = 'type_3',
                label = "Geography Type",
                choices = c("Census Tracts", "Counties")), 
                 
     selectInput(inputId = 'loc_3',
                label = "Location",
                choices = c("Loading")),
                 
     selectInput(inputId = 'enco_3',
                label = "Vaccine Encouragements",
                choices = c("Select")),
                 
     textInput(inputId = 'period_3', label = "Period around encouragement", 
               value="", width = NULL, placeholder = "in days"),
                 
     radioButtons("gender_3", "Gender", c("All" = "all", "Male" = "male", 
                                          "Female" = "female"), inline=T),
                 
     radioButtons("age_3", "Age group", c("All" = "all_age", "Above 65" = "a65", 
                                          "Below 18" = "b18"), inline=T),
                 
     radioButtons("ethnicity_3", "Hispanic Ethnicity", c("All" = "all_eth", 
                                                         "Yes" = "yes", "No" = "no", 
                                                         "Unknown"="unknown"), inline=T),
                 
     selectInput(inputId = 'race_3',
                             label = "Racial group",
                             choices = c("All", "White", "Other Race", "Asian", "Unknown", "American Indian or Alaska Native", "Black or African-American", "Native Hawaiian or Other Pacific Islander")),
                 
     actionButton("submit_3", "Analyze Encouragements")),
               
     mainPanel( plotOutput("plot_3"), uiOutput("text_3"))
  
    )))
))
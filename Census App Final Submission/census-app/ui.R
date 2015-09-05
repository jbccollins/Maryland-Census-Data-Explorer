library(shiny)

shinyUI(fluidPage(

  titlePanel("Maryland Census Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style("form.well {background-color: white; }")
      ),
      tabsetPanel(
        tabPanel("Quantitative Filters",       
                 sliderInput("ages", label= "Age range",
                             min = 16, max = 100, value = c(18, 23)),
                 sliderInput("incomes", label= "Income range",
                             min = 1000, max = 250000, value = c(1000, 50000), step=1000)
        ),
        
        tabPanel("Classification Filters", 
                 selectInput("race", 
                             label = "Race",
                             choices = c("All", "White", "Black", "American Indian", "Asian", "Pacific Islander", "Other", "Two or more races"),
                             selected = "All"),
                 selectInput("sex", 
                             label = "Sex",
                             choices = c("All", "Male", "Female"),
                             selected = "All"),
                 selectInput("degree", 
                              label = "Degree Type", 
                              choices = c(
                                       "All",
                                       "Bachelor's", 
                                       "Master's", 
                                       "Professional (Beyond Master's)" ,
                                       "Doctorate"
                                             ),
                              selected = "All"),
                 selectInput("majorCategory",
                             label = "Major Category",
                             choices = c(
                                     "All",
                                     "Agriculture & Natural Resources",
                                     "Arts",                               
                                     "Biology & Life Science",
                                     "Business",            
                                     "Communications & Journalism",
                                     "Computers & Mathematics",      
                                     "Education",
                                     "Engineering",       
                                     "Health",
                                     "Humanities & Liberal Arts",                               
                                     "Industrial Arts & Consumer Services Interdisciplinary",              
                                     "Law & Public Policy",
                                     "Physical Sciences",                  
                                     "Psychology & Social Work",
                                     "Social Science"
                               ),
                             selected = "All")
                                
        )
      )
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram", 
                 plotOutput("histPlot"),
                 fluidRow(
                          column(6,
                                 sliderInput("bins",
                                             "Number of bins:",
                                             min = 1,
                                             max = 50,
                                             value = 30)
                                 )
                          )
                ),
        tabPanel("Q-Q plot", plotOutput("qqPlot"),
                 fluidRow(
                   column(6,
                          selectInput("qqMode",
                                      label = "Q-Q Mode",
                                      choices = c(
                                        "Raw Income",
                                        "Log of Income"
                                      ),
                                      selected = "Raw Income")
                   )
                 )),
        tabPanel("Map", 
                 plotOutput("geogPlot"),
                 fluidRow(
                   column(6,
                          selectInput("mapDisplay",
                                      label = "Map Display",
                                      choices = c(
                                        "Observation Count",
                                        "Observation Mean",
                                        "Observation Sum"
                                      ),
                                      selected = "Observation Count")
                   )
                 )),
        tabPanel("Model Summary", 
                 verbatimTextOutput("model_summary"),
                 fluidRow(
#                            column(6,                 
#                                sliderInput("k",
#                                              "Select a k for KNN",
#                                              min = 1,
#                                              max = 100,
#                                              step = 1,
#                                              value = 50)
#                            ),
                            column(6,                 
                                 sliderInput("bucketSize",
                                             "Select a bucket size for error",
                                             min = 0,
                                             max = 50000,
                                             step = 100,
                                             value = 5000)
                            )
                          )
                ),
        tabPanel("Information", 
                 tags$div(
                            HTML("
<h3>
  About this app:
</h3>
<div>
This application allows the user to subset and examine U.S. census data for the state of
Maryland in the years 2012 and 2013. The sidebar panes allow the user to create filters
that will be applied to all views in the panes of the main panel. The panes in the main
panel have more customization options which will be discussed later. Note that by default
all observations have an Age greater than fifteen and a non-NA Degree Type and an income
greater than 0.
</div>
<h3>
Main Panel Information
</h3>
<div>
<b>Histogram:</b> This panel simply displays a histogram of incomes for the selected
subset of census data. The number of bins displayed in the histogram can be adjusted using the
slider directly beneath the histogram.
<br><br>
<b>Q-Q Plot:</b> This panel displays a Q-Q plot of incomes for the selected subset of census data.
The user can choose to plot raw income or log of incomes using the select widget directly beneath
the plot.
<br><br>
<b>Map:</b> This panel displays a map of the geographic distribution of the selected subset of census data. 
The map is divided using PUMA area codes given in the census data. More information about PUMA area codes can 
be found <a href='https://www.census.gov/geo/reference/puma.html' target='_blank'>here.</a>
The user can select what to display on the map using the select widget directly beneath the map.
If 'Observation Count' is selected then the scale goes from white to green with white indicating the lowest number
of observations in a PUMA area and the darkest green indicating the PUMA area with the greatest number of observations.
If 'Observation Mean' is selected then the scale goes from white to green with white indicating the lowest mean and
and the darkest green indicating the highest mean. Likewise for 'Observation Sum'.
<br><br>
<b>Model Summary:</b> This panel displays a brief summary of a K-Nearest Neighbors prediction of 2013 incomes
using data from 2012 as a training set. The user can decide what an acceptable prediction error is using the 
bucket size slider. If a prediction falls within +- the user defined error of the actual value then is is 
considered a succesful prediction.
</div>
<br>
                                 ")
                          )
          )
        
      ),
      textOutput("text1")
    )
  )
))
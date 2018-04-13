library(leaflet)
library(shinythemes)
library(shiny)
library(shinyBS)
library(markdown)
#library(DT)
#library(plotly)

# Define UI for application that draws a histogram
shinyUI(
  
  fluidPage(theme = shinytheme("flatly"),   
    
    # Application Title
    navbarPage("MCI Models for Optimal Geospatial Location Simulation",
               
#---------------------------------------------------------------------------------------------------------------------------------#  

               # 1st tab - Catchment Area Analysis
               tabPanel("Catchment Area Analysis", tabName = "huff", icon = icon("map-marker"),
                       sidebarPanel(
                         width = 3,
                         selectizeInput("lib", "Select a Library", choices = c("All", libName),
                                        multiple = FALSE, selected = "Ang Mo Kio Public Library"),hr(),
                         sliderInput("buffer", "Buffer (km) ", 0.1, 1.5, value = 1.0, step = 0.1),hr(),
                         h4("No. of Surrounding Facilities:"),
                         tableOutput("amenitiesTable"), hr()
                         
                       ),
                       # the width and height here need to be responsive
                       mainPanel(leafletOutput("generalMap",width = 985, height = 600)
                       )
                        
               ),

#---------------------------------------------------------------------------------------------------------------------------------#  
               
      # 2nd tab - Huff Visualization
      tabPanel("MCI Visualization", tabName = "huff", icon = icon("male"),
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   selectizeInput("libHuff", "Select a Library", choices = libName,
                                  multiple = FALSE, selected = "Ang Mo Kio Public Library"),
                   selectizeInput("ageBinSelect", "Select Population Age Bin", choices = c("All","Kindergarten and Preschool", "Primary School", "Secondary and Post-Sceondary School", 
                                                                                "Univeristy ", "Working Adults", "Senior Citizens"),
                                  multiple = FALSE, selected = "All"),
                   selectizeInput("sexSelect", "Select Population Sex", choices = c("Both","Male", "Female"),
                                  multiple = FALSE, selected = "Both"),
                   selectizeInput("raceSelect", "Select Population Race", choices = c("All","Chinese", "Indian", "Malay", "Others"),
                                  multiple = FALSE, selected = "Both"),
                   selectizeInput("toCalibrate", label = "Select Attractiveness for MCI Calibration",
                                  choices= c("Branch.Gross.Floor.Area", "Collection.Size", "No..of.Tuition.Centres.Within.1KM",
                                             "type_if_mall", "type_if_standalone"),
                                  multiple = TRUE),
                   actionButton("calibrate", label = "Calibrate MCI Model",style='padding:5px; height:80%'), hr(),
                   selectizeInput("attractiveness", label = "Select Attractiveness for Market Share Calculation",
                                  choices= c("Branch.Gross.Floor.Area", "Collection.Size", "No..of.Tuition.Centres.Within.1KM",
                                             "type_if_mall", "type_if_standalone"),
                                  multiple = TRUE),
                   actionButton("visualizeHuff", label = "Visualize Patronage Percentage (MCI)",style='padding:5px; height:80%'), hr()
                   # selectInput("radio", label = "Library Update Type",
                   #             choices = c("None"= "none", "Add new library location" = "newLibLocation",
                   #                         "Remove existing library location" = "removeLibLocation",
                   #                         "Change existing Library location" = "changeLibLocation"),
                   #             selected = "none"),
                   # uiOutput('dynamicUpdateLib'),
                   # uiOutput('dynamicSaveLib'),
                   # textOutput("huffErrorText")
                 ),
                 
                 
                 mainPanel(
                   
                   tabsetPanel(type = "tabs", selected="Actual Patron Flow",
                         tags$head(
                           tags$style(type='text/css',
                                      ".nav-tabs {font-size: 13px} ")),
                         
                         tabPanel("Actual Patron Flow",
                            leafletOutput("actualMap",width = 985, height = 621)
                                  
                         ),
                         
                         tabPanel("Predicted Patron Flow",
                              # the width and height here need to be responsive
                            leafletOutput("huffMap",width = 985, height = 621)
                         ), 
                         
                         tabPanel("Patron Flow Difference",
                              # the width and height here need to be responsive
                            leafletOutput("diffMap",width = 985, height = 621)
                         )
                               
                               
                   ),
                   bsModal("mciSummary", "MCI Calibration Result", "calibrate", size = "large",
                           verbatimTextOutput("disSummary"))
                 )
               )
            ),

#---------------------------------------------------------------------------------------------------------------------------------#        
      
      # 3rd tab - Patron Segmentation
      tabPanel("Patron Segmentation", tabName = "clustering", icon = icon("bar-chart"),
               sidebarLayout(
                 sidebarPanel(width = 3, 
                              status = "primary", solidHeader = FALSE, 
                              h6("Step 1: Review the distribution"),
                              selectizeInput("rawVar", label = "Raw Variables", choices= colVariables, multiple = TRUE),
                              selectizeInput("transVar", label = "Transform Variables", choices= colVariables, multiple = TRUE),
                              #hr(),
                              
                              h6("Step 2: Choose variables(s) to find optimal number of cluster"),
                              selectizeInput("transformVar", label = "To be transformed", choices= colVariables, multiple = TRUE),
                              selectizeInput("clusterVar", label = "NOT to be transformed", choices= colVariables, multiple = TRUE),
                              textOutput("errorText"), 
                              p(),
                              actionButton("trans", label = "Find optimal cluster",style='padding:5px; font-size:90%; height:80%'),
                              
                              #hr(),
                              h6("Step 3: Choose number of cluster",em("(K-means Clustering)")),
                              numericInput("numCluster", label = "No. of Cluster", value = 6),
                              textOutput("errorTextCluster"), 
                              actionButton("cluster", label = "Cluster",style='padding:5px; font-size:90%; height:80%')
                 ),
                 mainPanel(
                          tabsetPanel(type = "tabs", selected="Raw Variables",
                                tags$head(
                                  tags$style(type='text/css', 
                                             ".nav-tabs {font-size: 13px} ")),    
                                  
                               tabPanel("Raw Variables", 
                                        plotOutput("distPlot", height = 571, width = 800)), 
                               tabPanel("Transformation",
                                        plotOutput("transformPlot", height = 571, width = 800)), 
                               tabPanel("Optimal Cluster",
                                        plotOutput("opclusterPlot", height = 571, width = 800)),
                               tabPanel("Clustering Results (Overview)",
                                        #DT::dataTableOutput('mytable')
                                        fluidRow(
                                            plotOutput("clusterPlot", height = 400, width = 800),
                                            dataTableOutput('mytable')
                                        )
                                      ),
                               tabPanel("Clustering Results (By Library)",
                                        selectizeInput("libClustering", h5("Select a Library"), choices = c(libName),
                                                       multiple = FALSE, selected = "Ang Mo Kio Public Library"),
                                        plotOutput("clusterPlot2", height = 571, width = 600)
                                     )
                                )
                               
                           )

                      )
                   
              )
      

      
      
          
      
    )
  )
)


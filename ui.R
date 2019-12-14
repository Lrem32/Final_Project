#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("superhero"),
    

    # Application title
    titlePanel("Understanding Crime in Chicago"),
    
    navbarPage("UCC App",
               tabPanel(h2("Introduction"),br(),
                        
                        title = "Introduction", 
                                h5("If you want to check out the dataset for", 
                                a("chicago crime", href = "https://data.cityofchicago.org/Public-Safety/Crimes-2019/w98m-zvie"), 
                                ", its available online, they have a really cool interface for browsing through the data."), br(),
                                h4("Goal of the UCC App"), 
                                p(h5("
                                This UCC App looks to help a user navigate the expansive Chicago Crime dataset,
                                while also helping them implement certain modeling methods and approaches. The
                                overall goal is to better understand how crime patterns change depending on
                                different factors, additionally we'd like to test our ability to predict the 
                                likelyhood of certain crimes based on the factors the user will be
                                able to explore!
                                    ")), br(),
                                h4("About the Dataset"),
                                p(h5("
                                The City of Chicago does an incredibly good job at compiling much
                                of it's crime data. One can download it or even look at it and 
                                visualize it on their respective website. We are going to be downloading
                                the dataset directly from their website. After some formatting and data 
                                cleaning the data will be presented to the user for exploration. 
                                This app will focus on having 4 main uses when exploring the Chicago Crime
                                Dataset."), br(),
                                h5("1) A Data exploration page where we can see some numeric
                                   and graphical summaries using some user designated variables", style = "position"
                                   ), 
                                h6("- Here the user will be able to define variables and types of graphs to create as well as save and export
                                   the plots as well as any subset datasets they create to a .png and .csv file respectively."), br(),
                                h6("2) A page with clustering model approach where the user can specify 
                                   aspects of the algorithm"
                                   ),
                                h6("3) A page for modeling, where there will be two supervised learning models
                                   with dynamic features for exploration of how different variables 
                                   affect modeling"),
                                h6("4) A simple representation of or dataset for the user to observe it. 
                                   Additionally they will be able to subset on this dataset to look for
                                   more precise sections."
                                   ))
                                   ),
               # This will be our Data Exploration tab allowing users to explore the data and produce some basic graphs. 
               tabPanel("Data Exploration",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("plote", h4("Graphing Options:"), c("Overview Of Crime Type"="crime", "Crimes Base On Time Of Day"="time",
                                                                                      "Crimes Based On Weekday"="day", "Crimes Based On Month" = "month")),
                                downloadButton("downloadData", "Download Dataset"),
                                downloadButton("downloadPlot", "Download Generated Plot"),
                                checkboxInput("selector_Option", h4("Show Optional Crime Variables")),
                                conditionalPanel(
                                    condition = "input.selector_Option ==1",
                                                checkboxGroupInput("crime_Selector", h4("Crime Variables Considered:"),
                                                                    choices = c("Arson" = "Arson", "Assault" = "Assault", 
                                                                    "Battery" = "Battery", "Burglary" = "Burglary", 
                                                                    "Children Involvement" = "Child", "Damage" = "Damage", 
                                                                    "Drug Crime" = "Drug", "Fraud" = "Fraud", "Homicide" = "Homicide",
                                                                    "Motor Vehicle Theft" = "MVT", 
                                                                    "Non-Violent Crime" = "NV-Crime", "Other" = "Other",
                                                                    "Robbery" = "Robbery", "Sex Crime" = "Sex", "Theft" =  "Theft",
                                                                    "Human Trafficking" = "Traffick", "Tresspassing" = "Tresspass"),
                                                                    selected = c("Arson", "Assault", "Battery", "Drug", "MVT", "Other"))),
                                checkboxInput("data_Filter", h4("Show Dataset Filtering Options")),
                                conditionalPanel(
                                    condition = "input.data_Filter == 1",
                                                checkboxGroupInput("variable_Selector", h4("Variables In Dataset"),
                                                                   choices = c("ID" = "ID", "Case" = "Case Number", "Full Date" = "Date", "Date" = "Date 2", 
                                                                               "Month" = "Month", "Time" = "Time", "Block" = "Block", "Crime" = "Crime", 
                                                                               "IUCR" = "IUCR", "Primary Type" = "Primary Type", 
                                                                               "Result in Arrest" = "Arrest", "Domestic Violence" = "Domestic",
                                                                               "Police Beat Area" = "Beat", "District" = "District", 
                                                                               "Ward" = "Ward", "FBI Code" = "FBI Code",
                                                                               "Year" = "Year", "Latitude" = "Latitude",
                                                                               "Longitude" = "Longitude"),
                                                                   selected = c("Case Number", "Date 2", "Time", "Crime", "Arrest"))
                                )
                            ),
                            mainPanel(
                                plotOutput("plot"),
                                dataTableOutput("exploreTable")
                                #textOutput("summaryStats")
                            )
                        )),
               # This is our Modeling Tab for letting users implement supervised learning methods.
               tabPanel("Supervised Learning Methods",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("modelType", h4("Modeling Options:"), c("Linear Regression Model"="lm", 
                                                                                "Random Forest Model" = "rf")),
                                conditionalPanel(
                                    condition = "input.modelType == 'glm'",
                                        selectInput("responseVar", h4("Select response variable"), c("Ward" = "Ward", "Type of Crime" = "Primary Type",
                                                                                          "Arrest" = "Arrest"))),
                                conditionalPanel(
                                    condition = "input.responseVar == 'Ward'",
                                        radioButtons("predictorVars", "Predictors to use in model:", c("Case Number" = "`Case Number`", "Date" = "`Date 2`", 
                                                                                                         "Primary Date" = "`Primary Date`", "Arrest" = "Arrest", 
                                                                                                         "Domestic" = "Domestic", "Beat" = "Beat",
                                                                                                         "District" = "District", "Ward" = "Ward", 
                                                                                                         "FBI Code" = "`FBI Code`", "Year" = "Year", 
                                                                                                         "Latitude" = "Latitude", "Longitude" = "Longitude"),
                                                     select = "Beat"),
                                        checkboxGroupInput("predictorVarsQuad", "Predictors to use in model(Quadratic):", c("Case Number" = "I(`Case Number`^2)", "Date" = "I(Date^2)", 
                                                                                                     "Primary Date" = "I(`Primary Date`^2)", "Arrest" = "I(Arrest^2)", 
                                                                                                     "Domestic" = "I(Domestic^2)", "Beat" = "I(Domestic^2)",
                                                                                                     "District" = "I(District^2)", "Ward" = "I(Ward^2)", 
                                                                                                     "FBI Code" = "I(`FBI Code`^2)", "Year" = "I(Year^2)", 
                                                                                                     "Latitude" = "I(Latitude^2)", "Longitude" = "I(Longitude^2)"),
                                        )
                                    
                                )
                            ),
                            mainPanel(
                                verbatimTextOutput("glmModel")
                                    
                                )
                        )),
               tabPanel("Clustering")
)))
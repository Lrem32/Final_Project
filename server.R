#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
library(chron)
library(lubridate)
library(hms)
library(stats)


##################################################################################################################
# DATA PREP FOR DATA EXPLORATION TAB

# We need to read in our dataset to be used throughout the RShiny App! 

# We'll access it from online, we'll also clean up the data a bit to avoid mising values,
# and fix a bit of formatting! 

#crimeData <- read_csv("https://data.cityofchicago.org/api/views/x2n5-8w5q/rows.csv?accessType=DOWNLOAD")
crimeData <- read_csv("https://data.cityofchicago.org/api/views/w98m-zvie/rows.csv?accessType=DOWNLOAD")

# Our data has some missing values, so we should subset to avoid these!
crimeData<- subset(crimeData, !is.na(crimeData$Latitude))
crimeData <- subset(crimeData, !is.na(crimeData$Longitude))
crimeData <- subset(crimeData, !duplicated(crimeData$`Case Number`))

# Now we need to adjust some formatting issues. We want to extract the date and time from our instances. 
crimeData$`Date 2` <- substr(crimeData$`Date`, 1, 10)
crimeData$Time <- substr(crimeData$`Date`, 12, 22)

crimeData$`Date 2` <- as.Date(crimeData$`Date 2`, format = "%m/%d/%Y")
crimeData$Time <- format(strptime(crimeData$Time, "%I:%M:%S %p"), "%H:%M:%S")

crimeData$Time <- times(crimeData$Time)

# Now that we have our date and time values separated, we are going to want to create some bins to store
# ranges of time for use in our analysis. 

timeBins <- chron(times = c("00:00:00", "06:00:00", "12:00:00", "18:00:00", "23:59:00"))
crimeData$`Time Bins` <- cut(crimeData$Time, breaks = timeBins, labels = c("00-06", "06-12", "12-18", "18-00"), 
                           include.lowest = TRUE)

# Just a check to see the distribution of crime across time periods throughout the day! 
# table(crimeData$`TIME BINS`)

# We also want to extract the days and months of the dates so we can plot with them later on. 
crimeData$Day <- weekdays(crimeData$`Date 2`, abbreviate = TRUE)
crimeData$Month <- months(crimeData$`Date 2`, abbreviate = TRUE)

# Lastly let's group some crime types, since there are so many we need to consolidate them! 

crimeData$Crime <- as.character(crimeData$`Primary Type`)
crimeData$Crime <- ifelse(crimeData$Crime %in% c("CRIM SEXUAL ASSAULT", "PROSTITUTION", 
                                                 "SEX OFFENSE"), 'Sex', crimeData$Crime)
crimeData$Crime <- ifelse(crimeData$Crime %in% c("MOTOR VEHICLE THEFT"), 'MVT', crimeData$Crime)

crimeData$Crime <- ifelse(crimeData$Crime %in% c("GAMBLING", "INTEFERE WITH PUBLIC OFFICER",
                                                 "INTERFERENCE WITH PUBLIC OFFICER",
                                                 "INTIMIDATION", "LIQUOR LAW VIOLATION", 
                                                 "OBSCENITY", "NON-CRIMINAL", "PUBLIC PEACE VIOLATION", 
                                                 "PUBLIC INDECENCY", "STALKING", 
                                                 "NON-CRIMINAL (SUBJECT SPECIFIED)", 
                                                 "CONCEALED CARRY LICENSE VIOLATION"), 'NV-Crime',
                                                 crimeData$Crime)
crimeData$Crime <- ifelse(crimeData$Crime == "CRIMINAL DAMAGE", 'Damage', crimeData$Crime)
crimeData$Crime <- ifelse(crimeData$Crime == "CRIMINAL TRESPASS", 'Tresspass', crimeData$Crime)
crimeData$Crime <- ifelse(crimeData$Crime %in% c("NARCOTICS", "OTHER NARCOTIC VIOLATION"), 
                                                 'Drug', crimeData$Crime)
crimeData$Crime <- ifelse(crimeData$Crime == "DECEPTIVE PRACTICE", "Fraud", crimeData$Crime)
crimeData$Crime <- ifelse(crimeData$Crime %in% c("OTHER OFFENSE"), "Other", crimeData$Crime)
crimeData$Crime <- ifelse(crimeData$Crime %in% c("KIDNAPPING", "WEAPONS VIOLATION", "OFFENSE 
                                                 INVOLVING CHILDREN", "OFFENSE INVOLVING CHILDREN"), 'Child',
                                                 crimeData$Crime)
crimeData$Crime <- ifelse(crimeData$Crime == "ARSON", 'Arson', crimeData$Crime)
crimeData$Crime <- ifelse(crimeData$Crime == "BATTERY", 'Battery', crimeData$Crime)
crimeData$Crime <- ifelse(crimeData$Crime == "THEFT", 'Theft', crimeData$Crime)
crimeData$Crime <- ifelse(crimeData$Crime == "ASSAULT", 'Assault', crimeData$Crime)
crimeData$Crime <- ifelse(crimeData$Crime == "ROBBERY", 'Robbery', crimeData$Crime)
crimeData$Crime <- ifelse(crimeData$Crime == "BURGLARY", 'Burglary', crimeData$Crime)
crimeData$Crime <- ifelse(crimeData$Crime == "HUMAN TRAFFICKING", 'Traffick', 
                          crimeData$Crime)
crimeData$Crime <- ifelse(crimeData$Crime == "HOMICIDE", 'Homicide', crimeData$Crime)

# We want to add factors to our dates, times and months to make plotting easier.
crimeData$Day <- factor (crimeData$Day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
crimeData$Month <- factor(crimeData$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                                                      "Aug", "Sep", "Oct", "Nov", "Dec"))

##################################################################################################################
# DATA PREP FOR SUPERVISED MODELING METHODS TAB

# This poriton will be focused on producing the necessary data we'll need for our modeling needs! 

# We've already cleaned up the data quite a bit, but lets really ensure there are no NA values. 
crimeData <- na.exclude(crimeData)

# Let's change our Arrest values from (TRUE/FALSE) to (1/0).
for(value in nrow(crimeData)){
    ifelse(crimeData$Arrest == TRUE, crimeData[value, 9] <- 1, crimeData[value, 9] <- 0)
}
for(value in nrow(crimeData)){
    ifelse(crimeData$Arrest == TRUE, crimeData[value, 10] <- 1, crimeData[value, 10] <- 0)
}

# Let's ensure there are no geographical outliers in the data! 
# This is commented out since it's not necessary for the application to be displayed. 
# g <- ggplot(crimeData, aes(x = Longitude, y = Latitude)) + geom_point()
# g

# We only want 12 variables for our models, so we'll reduce the original dataset once more. 
modelingCrimeData <- crimeData[ , c(2, 23, 6, 9, 10, 11, 12, 13, 15, 18, 20, 21)]
#modelingCrimeData$`Case Number` <- as.factor(modelingCrimeData$`Case Number`)
#modelingCrimeData$`Date 2` <- as.factor(modelingCrimeData$`Date 2`)
#modelingCrimeData$`Primary Type` <- as.factor(modelingCrimeData$`Primary Type`)
#modelingCrimeData$`Arrest` <- as.factor(modelingCrimeData$`Arrest`)
#modelingCrimeData$`Domestic` <- as.factor(modelingCrimeData$`Domestic`)
#modelingCrimeData$`Beat` <- as.factor(modelingCrimeData$`Beat`)
#modelingCrimeData$`District` <- as.factor(modelingCrimeData$`District`)
#modelingCrimeData$`Ward` <- as.factor(modelingCrimeData$`Ward`)
#modelingCrimeData$`FBI Code` <- as.factor(modelingCrimeData$`FBI Code`)
#modelingCrimeData$`Year` <- as.factor(modelingCrimeData$`Year`)
#modelingCrimeData$`Latitude` <- as.factor(modelingCrimeData$`Latitude`)
#modelingCrimeData$`Longitude` <- as.factor(modelingCrimeData$`Longitude`)

##################################################################################################################
# FITTING OUR MODELS AND PREPARINT EVERYTHING. 

# First we need to set a seed.
set.seed(100)

# Now lets indicate the indices to split on.
train <- sample(1:nrow(modelingCrimeData), size = nrow(modelingCrimeData) *0.8)
test <- dplyr::setdiff(1:nrow(modelingCrimeData), train)

# Lets subset our data into the training and testing portions. 
crimeTrain <- modelingCrimeData[train, ]
crimeTest <- modelingCrimeData[test, ]

# This was a check to ensure my variables were factors, for the linear regression model.  
#(l <- sapply(modelingCrimeData, function(x) is.factor(x)))

#Testing
##################################################################################################################
# ACTUAL SERVER SIDE OPERATIONS POST-DATA PROCESSING

# Let's get to plotting! 
shinyServer(function(input, output, session) {
    
    # Used in the creation of our subset data to be downloaded in our Data Exploration Tab
    # We want a dynamically changing dataset, we can achieve this with the following:
    filterData <- reactive(
        newData<- crimeData %>% select(input$variable_Selector)
    )
    
    # This helps us produce the needed dataset for generating the respective graphs within our Data Exploration Tab. 
    graphData <- reactive(
        if(input$plote == "crime"){
            newData <- crimeData %>% filter(crimeData$Crime %in% input$crime_Selector)
            newData <- newData$Crime
        }else{
            if(input$plote == "time"){
                newData <- crimeData %>% filter(crimeData$Crime %in% input$crime_Selector)
                newData <- newData$`Time Bins`
            }else{
                if(input$plote == "day"){
                    newData <- crimeData %>% filter(crimeData$Crime %in% input$crime_Selector)
                    newData <- newData$Day
                }else{
                    if(input$plote == "month"){
                        newData <- crimeData %>% filter(crimeData$Crime %in% input$crime_Selector)
                        newData <- newData$Month
                    }
                }
            }
        }
    )

    # This will take the dataset produced from 'graphData' and create a quickplot with that dataset! This and the previous datasets all work dynamically. 
    generateGraph <- reactive(
        desiredPlot<- qplot(graphData(), xlab = input$plote, main = paste("Graphing data by", input$plote)) + scale_y_continuous("Number of Crimes")
    )
    
    # This will allow the user to choose the interactions to be chosen for our models. 
    chooseInteractions <- reactive({
        interactions <- paste(input$predictorVars, collapse = "+")
        quadratics <- paste(input$predictorVarsQuad, collapse = "+")
        if (quadratics != "" &&  interactions != ""){
             predictors <- paste(paste(input$predictorVars, collapse = "+"), quadratics, sep = "+")
        }else{
            if(interactions != ""){
                predictors <- interactions
            }else{
                if(interactions == "" && quadratics != ""){
                    predictors <- paste(input$predictorVarsQuad, collapse = "+")
                }
            }
        }
    })
    
    # Now we can fit our model with the previous reactive function. 
    fitLinearModel <- reactive({
        fit <- lm(as.formula(paste(input$responseVar, "~", chooseInteractions())), data = crimeTrain)
        return(fit)
    })
    output$plot <- renderPlot({
        # Let's bring in the dynamically changing dataset! 
        # Here our data will only display the types of crimes that are selected by the user! 
        qplot(graphData(), xlab = input$plote, main = paste("Graphing data by", input$plote)) + scale_y_continuous("Number of Crimes")
        

    })
    
    output$exploreTable <- renderDataTable(
        filterData(),
        options = list(pageLength = 10)
    )
    
    # This will allow our users to download the respective dataset that is created when subset on desired variables. 
    output$downloadData <- downloadHandler(
        filename = function() {"UCC_Data.csv"},
        
        # This function should write data to a file given to it by
        # the argument 'file'.
        content = function(fname) {
            # Write to a file specified by the 'file' argument
            write.csv(filterData(), fname)
        }
    )
    
    # This will allow our users to download a .png file with our created plots! 
    output$downloadPlot <- downloadHandler(
        filename = function() { paste("UCC_Plot", '.png', sep='') },
        content = function(file) {
            device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
            ggsave(file, plot = generateGraph(), device = device)
        }
    )
    
    # This will output the results from our Linear Regression Model
    output$glmModel <- renderPrint({
        summary(fitLinearModel())
    })
    
    # Plot our model! 
    output$glmPlot <- renderPlot({
        abline(lm(as.formula(paste(input$responseVar, "~", chooseInteractions())), data = crimeTrain))
        
    })
})
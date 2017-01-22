
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(caret)
library(dplyr)
library(plotly)
library(randomForest)
library("e1071")

# LOAD DATA
data <- read.csv('titanic.csv')

# MISSING VALUES
clean <- na.omit(data)

# FEATURE ENGINEERING
splitName <- function(name,idx=1){
  strsplit(as.character(name), split="[,.] ")[[1]][idx]
}
clean$Firstname <- sapply(clean$Name, splitName, 3)
clean$Title <- sapply(clean$Name, splitName, 2)
clean$Surname <- sapply(clean$Name, splitName)

rareTitle <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
clean$Title[clean$Title == 'Mlle'] <- 'Miss' 
clean$Title[clean$Title == 'Ms'] <- 'Miss'
clean$Title[clean$Title == 'Mme'] <- 'Mrs' 
clean$Title[clean$Title %in% rareTitle]  <- 'Elite'

# FEATURE SELECTION
fullSurvived <- clean %>% select(Survived, Pclass, Sex, Age, Title)
fullSurvived$Survived <- as.factor(fullSurvived$Survived)

fullFare <- clean %>% select(Pclass, Sex, Age, Title, Fare)


# PARTITION
inTraining <-  createDataPartition(y = fullSurvived$Survived, p = .8, list = FALSE)
train <- fullSurvived[inTraining,]
test  <- fullSurvived[-inTraining,]

# CLASSIFICATION
set.seed(566)

#płeć, wiek i klasa biletu
ctrlSurvived <- trainControl(
  method = "repeatedcv",
  number = 2,
  repeats = 5)
fitSurvived <- train(factor(Survived) ~ .,
             data = fullSurvived,
             method = "rf",
             trControl = ctrlSurvived,
             ntree = 10)

#predykcja
predict(fitSurvived, newdata = test, type = "prob")

# REGRESSION
fitFare <- train(Fare ~ .,
                 data = fullFare,
                 method = "rf",
                 trControl = ctrlSurvived,
                 ntree = 10)


# SERVER
shinyServer(function(input, output) {
  v <- reactiveValues(probs = NULL, fare = NULL)
  
  observeEvent(input$submit, {
    case <- data.frame(Pclass=as.numeric(input$class), Sex=input$sex, Age = as.numeric(input$age), Title = input$title)
    probs <- predict(fitSurvived, newdata = case, type = "prob")
    v$probs <- data.frame(Survived=c("Nie przeżyjesz", "Przeżyjesz"), Chance=c(probs[[1]]*100,probs[[2]]*100))
    v$fare <- predict(fitFare, newdata = case)
  })
  
  output$plot <- renderPlotly({
    validate(
      need(input$firstname != '', 'Podaj swoje imię!'),
      need(input$surname != '', 'Podaj swoje nazwisko!'),
      need(v$probs != '', '')
    )
    colors <- c('rgb(211,94,96)', 'rgb(98,211,94)')
    plot_ly(v$probs, labels = ~Survived, values = ~Chance, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~Survived,
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1))) %>%
      layout(title = 'Twoje szanse na przeżycie',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$msg <- renderText({
    validate(
      need(v$probs != '',''),
      need(input$firstname != '',''),
      need(input$surname != '','')
    )
    deathChance <- v$probs$Chance[1]
    result <- ""
    if(deathChance > 90){
      result <- paste("Przykro mi ", input$firstname, " ", input$surname, ". Wygląda na to, że podzielisz los Leonardo ;(")
    } else if(deathChance > 80) {
      result <- "Przeżyjesz jeśli Rosie się przesunie."
    } else if(deathChance > 40) {
      result <- "Może warto zaryzykować?"
    } else if(deathChance == 0) {
      result <- "Ma się te znajomości przy szalupach, co?"
    } else {
      result <- "Dziś jest Twój szczęśliwy dzień!"
    }
    result
  })
  output$fare <- renderText({
    validate(
      need(input$firstname != '',''),
      need(input$surname != '',''),
      need(v$fare != '', '')
    )
    paste("Koszt biletu: ", round(v$fare, digits = 2), "$")
  })

})
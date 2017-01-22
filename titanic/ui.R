
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)

shinyUI(fluidPage(
  div(img(src="img/titanic.png", width="100"), class="pull-left"),
  tagList(tags$head(tags$title("Twoja podróż statkiem Titanic"))),
  h2("Twoja podróż statkiem Titanic", style="text-align: center"),

  fluidRow(
    column(width = 6, class = "col-md-3",
           wellPanel(
             selectInput("title", label = h5("Tytuł"), 
                         choices = list("Mr." = "Mr", "Mrs." = "Mrs", "Miss" = "Miss", "Master" = "Master", "Cpt." = "Elite")
                         , selected = "Mr"),
             textInput("firstname", label = h5("Imię")),
             textInput("surname", label = h5("Nazwisko")),
             radioButtons("sex", label = h5("Płeć"),
                          choices = list("Mężczyzna" = "male", "Kobieta" = "female")
                          , selected = "male"),
             sliderInput("age", label = h5("Wiek"),
                         min = 1, max = 120, value = 18),
             selectInput("class", label = h5("Klasa"), 
                         choices = list("Klasa I" = 1, "Klasa II" = 2, "Klasa III" = 3), selected = 2),
             actionButton("submit", "Sprawdź czy przeżyjesz!")
           )),
    column(width = 6, class = "col-md-9",
           h3(textOutput("fare")),
           plotlyOutput("plot"),
           h3(textOutput("msg"))
          )
  )
))

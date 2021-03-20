rm(list=objects())

# shiny dashboard tutorial:
# http://rstudio.github.io/shinydashboard/get_started.html
# commit this code: git push https://Flavelloni@github.com/Flavelloni/OFC.git

library(shiny)
library(shinydashboard)
library(shinyWidgets) #fuer select input picker
library(DT) # for big tables (search, filter and so on)
library(rhandsontable)
library(formattable) #percent
source("ofc.R")
source("probs_calculator_input.R")
source("performanceAnalysis.R")

# shiny icons: https://fontawesome.com/icons?d=gallery&p=2, 
# id did not make these work: lib="glyphicon": https://icons.getbootstrap.com/


ui <- shinydashboard::dashboardPage(
  
  skin = "black",
  
  # header
  shinydashboard::dashboardHeader(title = "Flavelloni's Pineapple-Rechner"),
  
  # sidebar
  shinydashboard::dashboardSidebar(
    sidebarMenu(
      menuItem(text = "W'keits-Rechner", tabName = "probsCalculator", icon = icon("calculator"))
      , menuItem(text = "Performance-Analyse", tabName = "performanceAnalysis", icon = icon("trophy"))
    )
  ),   
      
  # body via tabItems
  shinydashboard::dashboardBody(
    
    shinydashboard::tabItems(
      tabItem(
        tabName = "probsCalculator",
        fluidRow(probs_calculator_input()),
        fluidRow(box(rHandsontableOutput("probs")))
      )
      
      
      , tabItem(
        tabName = "performanceAnalysis",
        fluidRow(performanceAnalysisUIInput()),
        fluidRow(box("Die Wahrscheinlichkeit, dass bei gleich guten Spielern
                     eine solche (oder groessere) Punktedifferenz besteht,
                     liegt bei: ")),
        fluidRow(valueBoxOutput("performanceBox"))
        
      )
    )
    
  ) # dashboardBody
)

################################################################################

server <- function(input, output) {
  
  probs <- reactive({
    
    n_players <- as.numeric(input$numOfPlayers)
    round <- as.numeric(input$round)
    outs <- as.numeric(input$outs)
    runners <- as.numeric(input$runners)
    
    res <- data.frame(
      Position=1:3
      , Probs=overview(round, outs, runners)[, n_players] %>% 
        formattable::percent() %>% 
        as.character()
    )
    
    
    
    return(res)
  })
  
  output$probs <- renderRHandsontable(
    rhandsontable(probs())
  )
  
  # performance Analysis
  performanceAnalysisResult <- reactive({
    res <- performanceAnalysisOutput(input$totalPoints, input$nbrOfRounds)
  })
  
  output$performanceBox <- shinydashboard::renderValueBox({
    res <- performanceAnalysisResult()
    if(res<0.05){
      color <- "red"
    } else if(res<0.1){
      color <- "orange"
    } else {
      color <- "green"
    }
      
    res <- res %>% formattable::percent()
    
    valueBox(
      value = res,
      subtitle = "Performance Bewertung",
      icon = icon("glasses"),
      color=color
    )
  })
    
  
  
}


################################################################################
shinyApp(ui, server)
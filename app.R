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
        performanceAnalysisOutput(totalPoints=9999, nbrOfRounds=9999)
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
  
  #output$performanceAnalysisResult <- 999
  
  
}


################################################################################
shinyApp(ui, server)
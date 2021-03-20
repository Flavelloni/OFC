performanceAnalysisUIInput <- function(){
  shinydashboard::box(
    
    shiny::sliderInput(
      inputId = "totalPoints",
      label = "Was ist der Punktestand?",
      min = -1000, max = 1000, value = 0
    )
    
    
    , shiny::sliderInput(
      inputId = "nbrOfRounds",
      label = "Wie viele Runden hast du schon gezockt?",
      min = -1000, max = 1000, value = 0
    )
    

  ) #box
}


performanceAnalysisOutput <- function(totalPoints, nbrOfRounds){
  999
}
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
      min = 20, max = 1000, value = 20
    )
    

  ) #box
}


performanceAnalysisOutput <- function(totalPoints, nbrOfRounds){
  
  # https://www.pokernews.com/strategy/nikolai-yakovenko-on-the-state-of-open-face-ii-19859.htm
  sigma <- 18
  
  # central limit theorem: S is standard normally distributed
  S <- totalPoints/sqrt(nbrOfRounds)/sigma

  # two-sided probability
  result <- 2*(1 - pnorm(abs(S)))
}
rm(list=ls())
library(dplyr)
library(pdftools) # read pdfs for comparisons or so
library(plot3D)

# benchmark <- pdf_text("Pineapple-Outs-Calcs-2Handed-RCP-2.pdf")

cnt_path <- paste0(getwd(), "/ofc_counts.xlsx")
global_tbl_cnts <- readxl::read_excel(cnt_path, skip=1)

# probability of hitting alt least 'runners' outs
total_probability <- function(outs, ctc, known, runners=1){
  
  unknown <- 52 - known
  inverse_outs <- unknown - outs
  
  ## probability of less than (runners) good cards
  ## i.e. 0 or 1 or (runners-1)
  inverse_probability_sum <- 0
  for(k in 1:runners - 1){
    inverse_probability <- exact_probability(outs, ctc, known, k)
    inverse_probability_sum <- inverse_probability_sum + inverse_probability
  }
  
  # for three runners, if all of them come in one draw, its useless as you
  # have to throw one card away
  if(runners==3){
    inverse_probability_sum <- inverse_probability_sum + 
                                specialcase_probability_3runners(outs, ctc, known)
  }
  
  # probability of at least (runners) good cards
  probability <- 1 - inverse_probability_sum

}

specialcase_probability_3runners <- function(outs, ctc, known){
  
  # example 3 runners, ctc=6: 
  # there are ctc/3=2 bad combinations of the 3 runners: all in
  # first draw or all in second draw.
  # in total, there are 6*5*4 / 3*2 = choose(6, 3) combinations
  
  number_of_combinations_all_in_one_draw <- ctc/3
  total_number_of_combinations <- choose(6, 3)
    
  inverse_probability <- 
    exact_probability(outs, ctc, known, 3)*
    number_of_combinations_all_in_one_draw/
    total_number_of_combinations
}

# probability of hitting exactly k outs
exact_probability <- function(outs, ctc, known, k){
  
  unknown <- 52 - known
  inverse_outs <- unknown - outs# example:
  
  # probability of exactly 1 good card with 9 outs in 52 cards
  # 9/52 * 43/51 * 42/50 ... = 9*43*42.../52*51*50...
  commutations <- choose(ctc, k)
  numerator_good_cards <- seq(outs, by=-1, length.out=k)
  numerator_bad_cards <- seq(inverse_outs, by=-1, length.out = ctc-k) 
  numerator <- c(numerator_good_cards, numerator_bad_cards)
  denominator <- seq(unknown, by=-1, length.out = ctc)
  inverse_probability <- prod(numerator/denominator) * commutations
}

street_ods <- function(n_players=1, street_var=1, outs=11, runners=3){
  if(is.na(street_var)) return(NA)
  ctc <- global_tbl_cnts %>% filter(street==street_var) %>% pull(paste0("ctc", n_players))
  known <- global_tbl_cnts %>% filter(street==street_var) %>% pull(paste0("known", n_players))
  total_probability(outs=outs, ctc=ctc, known=known,runners=runners)
}

#street_ods(1, 1, 11, 3)

overview <- function(rnd, outs, runners){
  # depending of the number of players, the streets differ per round
  # the vector the function is applied to contains the corresponding streets
  if(rnd==1){
    params <- tibble(
      `againstFL` = list(1, c(1, NA, NA))
      ,`2Handed`  = list(2, c(1, 2, NA))
      ,`3Handed`  = list(3, c(1, 2, 3))
    )
  }
  if(rnd==2){
    params <- tibble(
      `againstFL` = list(1, c(2, NA, NA))
      ,`2Handed`  = list(2, c(3, 4, NA))
      ,`3Handed`  = list(3, c(4, 5, 6))
    )
  }
  if(rnd==3){
    params <- tibble(
      `againstFL` = list(1, c(3, NA, NA))
      ,`2Handed`  = list(2, c(5, 6, NA))
      ,`3Handed`  = list(3, c(7, 8, 9))
    )
  }
  if(rnd==4){
    params <- tibble(
      `againstFL` = list(1, c(4, NA, NA))
      ,`2Handed`  = list(2, c(7, 8, NA))
      ,`3Handed`  = list(3, c(10, 11, 12))
    )
  }
  
  result <- sapply(params, function(x) sapply(x[[2]], function(s) street_ods(x[[1]], s, outs, runners)))
  #print(result)
}


regression_model <- function(data_in, max_outs=8){
  dat <- data.frame(
    "odds"=matrix(data_in, ncol=1)
    ,"outs"=rep(1:max_outs, 4)
    ,"round"=c(rep(1, max_outs), rep(2, max_outs), rep(3, max_outs), rep(4, max_outs))
  )
  
  linearModel <- lm(odds ~ outs+round, data=dat)
}

regression_model2 <- function(data_in, max_outs=8){
  dat <- data.frame(
    "odds"=matrix(data_in, ncol=1)
    ,"outs"=rep(1:max_outs, 2)
    ,"round"=c(rep(2, max_outs), rep(3, max_outs))
  )
  
  linearModel <- lm(odds ~ outs+round, data=dat)
}

plots <- function(data_in, max_outs=8){ # consider only the outs up to a certain number

  z <- data_in
  
  # approximation
  regr <- regression_model(z)
  coefs <- coefficients(regr)
  z_prime <- sapply(1:4, function(s) sapply(1:max_outs, function(o) coefs[1]+o*coefs[2]+s*coefs[3]))
  
  persp3D(1:max_outs, 1:4, z, 
        xlab="outs", ylab="street", zlab="odds", 
        zlim=c(0,100),
        theta=10, phi=20,
        col="darkgreen",
        shade=0.5) -> res
  
  persp3D(1:max_outs, 1:4, z_prime,
        xlab="outs", ylab="street", zlab="odds",
        theta=10, phi=40,
        col="red",
        add=T)
  
  persp(1:max_outs, 1:4, z, 
          xlab="outs", ylab="street", zlab="odds", 
          zlim=c(0,100),
          theta=10, phi=20,
          col="blue") -> res
  
  # lines(trans3d(1:max_outs, 1, z_prime[,1], pmat=res), col="red")
  # lines(trans3d(1:max_outs, 2, z_prime[,2], pmat=res), col="red")
  # lines(trans3d(1:max_outs, 3, z_prime[,3], pmat=res), col="red")
  # lines(trans3d(1:max_outs, 4, z_prime[,4], pmat=res), col="red")
  
  
  z <- sapply(1:8, function(s) sapply(1:max_outs, function(o) street_ods(outs=o, street_var=s, n_players=2, runners=1)))*100
  #z <- sapply(1:8, function(s) sapply(2:7, function(o) street_ods(outs=o, street_var=s, n_players=2, runners=1)))*100
  # TODO: try using outer(x,y, f)
  
  # summarize streets to rounds
  z <- cbind(rowMeans(cbind(z[,1], z[,2]))
             , rowMeans(cbind(z[,3], z[,4]))
             , rowMeans(cbind(z[,5], z[,6]))
             , rowMeans(cbind(z[,7], z[,8]))
  )
  
}


regression <- function(max_outs=8){
  z <- sapply(1:8, function(s) sapply(1:max_outs, function(o) street_ods(outs=o, street_var=s, n_players=2, runners=1)))*100
  #z <- sapply(1:8, function(s) sapply(2:7, function(o) street_ods(outs=o, street_var=s, n_players=2, runners=1)))*100
  # TODO: try using outer(x,y, f)
  
  # summarize streets to rounds
  z <- cbind(rowMeans(cbind(z[,1], z[,2]))
             , rowMeans(cbind(z[,3], z[,4]))
             , rowMeans(cbind(z[,5], z[,6]))
             , rowMeans(cbind(z[,7], z[,8]))
  )
  
  model <- regression_model(z, max_outs=max_outs)
  print(coefficients(model))

  #plots(z)
}

# result:
# regression for 2nd and 3rd round, outs 1 to 4
# coefficients: 30 + 14*outs - 9*round
regression2 <- function(max_outs=8){
  z <- sapply(3:6, function(s) sapply(1:max_outs, function(o) street_ods(outs=o, street_var=s, n_players=2, runners=1)))*100

  # summarize streets to rounds
  z <- cbind(rowMeans(cbind(z[,1], z[,2]))
             , rowMeans(cbind(z[,3], z[,4]))
  )
  
  model <- regression_model2(z, max_outs=max_outs)
  print(coefficients(model))
  print(residuals(model))
  print(z-cbind(30+14*1:4-9*2, 30+14*1:4-9*3))
  print(model$fitted.values)
  print(z)
}

#regression2(4)


# result:
# outs necessary to pass the 50% mark per round: 6, 7, 9, Inf
flush_plot <- function(n_players=2){
  n_streets <- switch(as.character(n_players), "2"=8, "3"=12)

  z <- sapply(1:n_streets, function(s) sapply(1:10, function(o) street_ods(outs=o, street_var=s, n_players=n_players, runners=2)))*100
  #round(z)
  persp(1:10, 1:n_streets, z, 
        xlab="outs", ylab="street", zlab="odds", 
        zlim=c(0,100),
        theta=10, phi=20,
        col="blue") -> res
  
  
  persp(1:10, 1:n_streets, z, 
        xlab="outs", ylab="street", zlab="odds", 
        zlim=c(0,100),
        theta=30, phi=20,
        col="blue") -> res
}

directOuts1Handed <- function(){
  install.packages("writexl")
  library("writexl")
  a <- round( sapply(1:4, function(s) sapply(1:10, function(o) street_ods(outs=o, street_var=s, n_players=1, runners=1)))*100)
  write_xlsx(as.data.frame(a),"1Handed_directOuts.xlsx")
}

#flush_plot(n_players=2)
#flush_plot(n_players=3)

fl_ev <- function(){
  dat <- readxl::read_excel("C:/Users/d60017/Documents/Flavio/OFC/FantasyLandEV.xlsx", col_names="ev")
  num <- dat$ev[order(dat$ev)]
  
  # remove the outliers
  outlier <- boxplot.stats(num)$out
  out_pos <- which(num==outlier)
  mean(num[-c(out_pos)])
  
  hist(dat)
}
  
# deploy on shinyapps.io
# install.packages('rsconnect')
# install.packages('rsconnect')
# rsconnect::setAccountInfo(
#   name='flavelloni', 
#   token='5E05E4376BE8F599E0030B045E1832F0', 
#   secret='8KPggQqJdUAJJXBb4NoiuqDiak3ieCJba39329K2'
# )


library(rsconnect)
rsconnect::deployApp()

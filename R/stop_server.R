stop.server <- function(){
  rstudioapi::sendToConsole("blogdown::stop_server()",execute = T)
}
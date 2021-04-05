open.highlighted <- function(){
  ctx = rstudioapi::getSourceEditorContext()
  slct = rstudioapi::getSourceEditorContext()$selection[[1]]
  # print()
  a <- slct$text
  if(a == ""){
    a <- "."
  }
  rstudioapi::sendToConsole(paste0("system('open ",a,"')"),execute = T)
}

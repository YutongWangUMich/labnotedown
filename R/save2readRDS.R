save2readRDS <- function(){
  ctx = rstudioapi::getSourceEditorContext()
  rng = ctx$selection[[1]]$range
  line.number <- rng$start[1]
  
  rng <- rstudioapi::document_range(c(line.number, 1),
                                    c(line.number, Inf))
  rstudioapi::setSelectionRanges(rng)
  slct = rstudioapi::getSourceEditorContext()$selection[[1]]
  
  # print()
  save.str <- slct$text
  if(!grepl("saveRDS",save.str)){
    stop("Are you sure this is an save object?")
  }
  read.str <- strsplit(strsplit(save.str,
                                split="(",
                                fixed = T)[[1]][2],
                       split = ",",
                       fixed=T)[[1]]
  read.str <- unlist(lapply(X = read.str,
                            FUN=function(x){trimws(x, which = "both")}
                            ))
  read.str <- paste0(read.str[1]," <- readRDS(",read.str[2])
  
  
  
  rstudioapi::modifyRange(rng, read.str)
  
  # rstudioapi::insertText(rng, read.str)
  
  # rstudioapi::sendToConsole(paste0("system('open ",slct$text,"')"),execute = T)
}
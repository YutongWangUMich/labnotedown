#' An RStudio addin for inserting newest image into a blogdown post
#'
#' insert.thumbnail()

get.rng.from.line.number <- function(line.number){
  return(rstudioapi::document_range(c(line.number, 1),
                                    c(line.number, Inf)))
}

get.text.from.line.number <- function(line.number){
  rng <- get.rng.from.line.number(line.number)
  rstudioapi::setSelectionRanges(rng)
  slct = rstudioapi::getSourceEditorContext()$selection[[1]]
  return(slct$text)
}


insert.thumbnail <- function(){
  ctx = rstudioapi::getSourceEditorContext()
  ctx = rstudioapi::getSourceEditorContext()
  
  rng = ctx$selection[[1]]$range
  # original.rng <- rng
  original.line.number <- rng$start[1]
  line.number <- original.line.number
  line.content <- get.text.from.line.number(line.number)
  while(!(1 %in% grep("```{r",line.content,fixed = T))){
    line.number <-  line.number-1
    if(line.number < 1){
      stop("Is this a R notebook?")
    }
    line.content <- get.text.from.line.number(line.number)
  }
  
  current.thumbnail.line.number <- grep("```{r thumbnail",ctx$contents,fixed = T)
  for(i in current.thumbnail.line.number){
    line.content <- get.text.from.line.number(i)
    rng <- get.rng.from.line.number(i)
    rstudioapi::modifyRange(rng,
                            gsub(x = line.content,pattern = "```{r thumbnail",replacement = "```{r",fixed = T))
  }
  line.content <- get.text.from.line.number(line.number)
  rng <- get.rng.from.line.number(line.number)
  
  rstudioapi::modifyRange(rng,
                          gsub(x = line.content,replacement = "```{r thumbnail",pattern = "```{r",fixed = T))
  
  if (ctx$path == '') stop(
    'Please select the blog post source file before using this addin', call. = FALSE
  )
  ctx_ext = tolower(xfun::file_ext(ctx$path))
  
  path = normalizePath(ctx$path)
  imgdir = paste0(xfun::sans_ext(basename(path)), '_files')
  datapath =  paste0("/post/",imgdir,"/figure-html/thumbnail-1.png")
  ### 
  txt = ctx$contents
  res = blogdown:::split_yaml_body(txt)
  yml = res$yaml_list
  rng = res$yaml_range
  if (!"thumbnail" %in% names(yml)){
    original.line.number <- original.line.number + 1
    rstudioapi::setSelectionRanges(list(c(rng[1] + 1, 1, rng[2], 1)))
    slct = rstudioapi::getSourceEditorContext()$selection[[1]]
    yml$thumbnail <- datapath
    seq_keys = Filter(function(key) {
      identical(attr(yml[[key]], 'yml_type'), 'seq')
    }, names(yml))
    seq_keys = unique(c(seq_keys, 'categories', 'tags'))
    
    
    for (i in seq_keys) yml[[i]] = if (length(yml[[i]]) > 0) as.list(yml[[i]])
    if (!getOption('blogdown.yaml.empty', TRUE)) yml = blogdown:::filter_list(yml)
    rstudioapi::modifyRange(
      slct$range, blogdown:::as.yaml(yml, .trim_ws = FALSE)
    )
  }
  rstudioapi::setSelectionRanges(get.rng.from.line.number(original.line.number))
}


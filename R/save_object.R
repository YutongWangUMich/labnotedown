save.object <- function() {
  # check if the string is fit for a filename
  isValidName <- function(string) {
    grepl("^([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*$",
          string)
  }

  isValidAndUnreservedName <- function(string) {
    make.names(string) == string
  }

  testValidity <- function(string) {
    valid <- isValidName(string)
    unreserved <- isValidAndUnreservedName(string)
    reserved <- (valid & !unreserved)
    list("Valid" = valid,
         "Unreserved" = unreserved,
         "Reserved" = reserved)
  }

  if (!dir.exists("aux_files")) {
    dir.create("aux_files/")
  }
  ctx = rstudioapi::getSourceEditorContext()
  rng = ctx$selection[[1]]$range
  line.number <- rng$start[1]

  rng <- rstudioapi::document_range(c(line.number, 1),
                                    c(line.number, Inf))
  rstudioapi::setSelectionRanges(rng)
  slct = rstudioapi::getSourceEditorContext()$selection[[1]]

  # name of the thing to be saved
  my.variable.name <- blogdown:::trim_ws(slct$text)


  if (isValidName(my.variable.name)) {
    if (ctx$path == '')
      stop('Please select the blog post source file before using this addin',
           call. = FALSE)
    ctx_ext = tolower(xfun::file_ext(ctx$path))

    path = normalizePath(ctx$path)

    path <- paste0("aux_files/", xfun::sans_ext(basename(path)))
    if (!dir.exists(path)) {
      dir.create(path)
    }

    absolute_path <- paste0(getwd(), "/", path)

    # Add the saveRDS(file = "...") line
    output <- paste0(absolute_path,
                     "/",
                     my.variable.name,".Rds")
    output <- paste0("saveRDS(",
                     my.variable.name,
                     ", file = \"",
                     output,
                     "\")")

    rstudioapi::modifyRange(rng, "")

    rstudioapi::insertText(rng, output)

    if(!check_if_yaml_exists(ctx, "aux_files")){
    line.number <- line.number +
      add.yaml(ctx,"aux_files",list())
    }

    ctx = rstudioapi::getSourceEditorContext()
    line.number <- line.number +
      add.yaml(ctx,"aux_files",paste0(my.variable.name,".Rds"))

    ctx = rstudioapi::getSourceEditorContext()
    line.number <- line.number +
      add.yaml(ctx,
               "aux_files_dir",
               paste0(absolute_path,
                      "/"),
               append_if_exist = F)

    rng <- rstudioapi::document_range(c(line.number, 1),
                                      c(line.number, Inf))

    rstudioapi::setSelectionRanges(rng)
  }else{
    stop("Not a variable R variable. Is your variable name on a line by itself?")
  }

}


check_if_yaml_exists <- function(ctx, key){
  txt = ctx$contents
  res = blogdown:::split_yaml_body(txt)
  yml = res$yaml_list
  return(key %in% names(yml))
}

# add key : val pair to yml if key does not yet exist. Otherwise, append it as a list.
# returns the number of lines that were changed
add.yaml <- function(ctx, key, val, append_if_exist = T){
  txt = ctx$contents
  res = blogdown:::split_yaml_body(txt)
  yml = res$yaml_list
  rng = res$yaml_range
  if (length(yml) == 0)
    return(warning(
      "The current document does not seem to contain YAML metadata",
      call. = FALSE
    ))
  if ((key %in% names(yml)) & append_if_exist) {
    if(val %in% yml[[key]]){
      val <- yml[[key]]
    }else{
      val <- append(yml[[key]], val)
    }
  }
  yml[[key]] <- val
  seq_keys = Filter(function(key) {
    identical(attr(yml[[key]], 'yml_type'), 'seq')
  }, names(yml))
  seq_keys = unique(c(seq_keys, 'categories', 'tags'))


  for (i in seq_keys)
    yml[[i]] = if (length(yml[[i]]) > 0)
      as.list(yml[[i]])
  if (!getOption('blogdown.yaml.empty', TRUE))
    yml = blogdown:::filter_list(yml)

  # update the yml
  rstudioapi::setSelectionRanges(list(c(rng[1] + 1, 1, rng[2], 1)))
  slct = rstudioapi::getSourceEditorContext()$selection[[1]]
  rstudioapi::modifyRange(slct$range, blogdown:::as.yaml(yml, .trim_ws = FALSE))

  ctx = rstudioapi::getSourceEditorContext()
  txt = ctx$contents
  res = blogdown:::split_yaml_body(txt)
  # yml = res$yaml_list
  new.rng = res$yaml_range
  return((new.rng[2]-new.rng[1])- (rng[2]-rng[1]))
}

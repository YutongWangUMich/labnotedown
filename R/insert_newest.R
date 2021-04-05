#' An RStudio addin for inserting newest image into a blogdown post
#'
#' insert.newest.image.from.desktop()

insert.newest.image.from.desktop <- function(){
  ctx = rstudioapi::getSourceEditorContext()
  if (ctx$path == '') stop(
    'Please select the blog post source file before using this addin', call. = FALSE
  )
  ctx_ext = tolower(xfun::file_ext(ctx$path))
  
  path = normalizePath(ctx$path)
  imgdir = file.path(
    'static', dirname(gsub('.*content/', '', path)),
    paste0(xfun::sans_ext(basename(path)), '_files')
  )
  
  datapath =  system("ls -t ~/Desktop/*.{jpg,jpeg,gif,png} | head -1",intern=TRUE)
  if (length(datapath)==0){
    stop("no file with extension {jpg,jpeg,gif,png} found on the desktop.")
  }
  newImgName = basename(datapath)
  target = file.path(imgdir, newImgName)
  target_dir = dirname(target)
  dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)
  copy_check = file.copy(
    datapath, target,
    overwrite = TRUE
  )
  
  image_code = function() {
    s = paste0(
      ifelse(getOption('blogdown.insertimage.usebaseurl', FALSE),
             blogdown:::load_config()$baseurl, "/"),
      basename(dirname(target_dir)), "/",
      basename(target_dir), "/", basename(target)
    )
    paste0('![](', s, ')')
  }
  
  rstudioapi::insertText(as.character(image_code()), id = ctx$id)
  
}

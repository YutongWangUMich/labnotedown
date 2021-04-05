clone.post <- function(){
  tags = htmltools::tags
  txt_input = function(..., width = '100%') shiny::textInput(..., width = width)
  sel_input = function(...) shiny::selectizeInput(
    ..., width = '100%', multiple = TRUE, options = list(create = TRUE)
  )
  meta = blogdown:::collect_yaml()

  ctxt = rstudioapi::getSourceEditorContext()
  
  txt = ctxt$contents
  res = blogdown:::split_yaml_body(txt)
  yml = res$yaml_list
  rng = res$yaml_range
  if (length(yml) == 0) return(
    warning("The current document does not seem to contain YAML metadata", call. = FALSE)
  )
  rstudioapi::setSelectionRanges(list(c(rng[2]+1, 1,Inf,1)))
  # slct.body = rstudioapi::getSourceEditorContext()$selection[[1]]
  # 
  # rstudioapi::setSelectionRanges(list(c(rng[1] + 1, 1, rng[2], 1)))
  slct = rstudioapi::getSourceEditorContext()$selection[[1]]

  if (length(yml) == 0) yml = list()
  yml = blogdown:::filter_list(yml)
  if (is.null(yml[['title']])) yml$title = ''
  if (is.null(yml[['author']])) yml$author = getOption('blogdown.author', '')
  if (is.null(yml[['date']])) yml$date = Sys.Date()
  shiny::runGadget(
    miniUI::miniPage(miniUI::miniContentPanel(
      txt_input('title', 'Title', value = paste(yml[['title']], "clone")),
      shiny::fillRow(
        txt_input('author', 'Author', value = yml[['author']], width = '98%'),
        shiny::dateInput('date', 'Date', Sys.Date(), width = '98%'),
        txt_input(
          'subdir', 'Subdirectory', getOption('blogdown.subdir', 'post'),
          '(optional)', width = '98%'
        ),
        height = '70px'
      ),
      shiny::fillRow(
        sel_input('cat', 'Categories', meta$categories),
        sel_input('tag', 'Tags', meta$tags,selected = yml[['tags']]),
        shiny::selectInput(
          'kind', 'Archetype', width = '98%',
          choices = unique(c('default', xfun::sans_ext(dir('archetypes', '\\.md$'))))
        ),
        height = '70px'
      ),
      shiny::fillRow(
        txt_input('file', 'Filename', '', 'automatically generated (edit if you want)'),
        height = '70px'
      ),
      shiny::fillRow(txt_input('slug', 'Slug', '', '(optional)'), height = '70px'),
      shiny::fillRow(
        shiny::radioButtons(
          'format', 'Format', inline = TRUE,
          c('Markdown' = '.md', 'R Markdown (.Rmd)' = '.Rmd', 'R Markdown (.Rmarkdown)' = '.Rmarkdown'),
          selected = getOption('blogdown.ext', '.md')
        ),
        height = '70px'
      ),
      miniUI::gadgetTitleBar(NULL)
    )),
    server = function(input, output, session) {
      empty_title = shiny::reactive(grepl('^\\s*$', input$title))
      shiny::observe({
        # update subdir in according to the title
        if (is.function(subdir_fun <- getOption('blogdown.subdir_fun'))) shiny::updateTextInput(
          session, 'subdir', value = subdir_fun(input$title)
        )
        # calculate file path
        if (!empty_title()) shiny::updateTextInput(
          session, 'file', value = blogdown:::post_filename(
            input$title, input$subdir, shiny::isolate(input$format), input$date
          )
        )
      })
      shiny::observe({
        if (!grepl('^\\s*$', input$file)) shiny::updateTextInput(
          session, 'slug', value = blogdown:::post_slug(input$file)
        )
      })
      shiny::observeEvent(input$format, {
        f = input$file
        if (f != '') shiny::updateTextInput(
          session, 'file', value = xfun::with_ext(f, input$format)
        )
      }, ignoreInit = TRUE)
      shiny::observeEvent(input$done, {
        if (grepl('^\\s*$', input$file)) return(
          warning('The filename is empty!', call. = FALSE)
        )
        if (is.null(getOption('blogdown.author'))) options(blogdown.author = input$author)
        blogdown::new_post(
          input$title, author = input$author, ext = input$format,
          categories = input$cat, tags = input$tag,
          file = gsub('[-[:space:]]+', '-', input$file),
          slug = input$slug, subdir = input$subdir, date = input$date,
          kind = input$kind
        )
        shiny::stopApp()
        # print()
        # print(getwd())
        # print(gsub('[-[:space:]]+', '-', input$file))
        
        # shift our focus to the new file that has been cloned
        file.edit(paste0("content/",
                         gsub('[-[:space:]]+', '-', input$file)))
        
        Sys.sleep(1)
        # rstudioapi::setSelectionRanges(list(c(rng[2]+1, 1,Inf,1)))
        # slct = rstudioapi::getSourceEditorContext()$selection[[1]]
        rstudioapi::insertText(Inf,slct$text)
        add.yaml(rstudioapi::getSourceEditorContext(),
                 "aux_files",
                 yml$aux_files,
                 append_if_exist = F)
        add.yaml(rstudioapi::getSourceEditorContext(),
                 "aux_files_dir",
                 yml$aux_files_dir,
                 append_if_exist = F)
        if("aux_files_dir" %in% names(yml)){
          ctxt = rstudioapi::getSourceEditorContext()
          txt = ctxt$contents
          aux.path = normalizePath(ctxt$path)
          aux.path <- paste0("../../aux_files/", xfun::sans_ext(basename(aux.path)),"/")
          
          txt.new <- unlist(lapply(txt,function(x) gsub(yml$aux_files_dir,aux.path,x)))
          
          rstudioapi::setSelectionRanges(list(c(1, 1, Inf, Inf)))
          slct = rstudioapi::getSourceEditorContext()$selection[[1]]
          rstudioapi::modifyRange(slct$range, 
                                  paste(txt.new,collapse ="\n"))
        }
        # rstudioapi::setSelectionRanges(list(c(1, 1, Inf, Inf)))
        # rstudioapi::insertText(paste(txt.new,collapse ="\n"),id=ctxt$id)
        
      })
      shiny::observeEvent(input$cancel, {
        shiny::stopApp()
      })
    },
    stopOnCancel = FALSE, viewer = shiny::dialogViewer('New Post', height = 500)
  )
}


# 
# 
# # add key : val pair to yml if key does not yet exist. Otherwise, append it as a list.
# # returns the number of lines that were changed
# add.yaml <- function(ctx,key,val,append_if_exist = T){
#   txt = ctx$contents
#   res = blogdown:::split_yaml_body(txt)
#   yml = res$yaml_list
#   rng = res$yaml_range
#   if (length(yml) == 0)
#     return(warning(
#       "The current document does not seem to contain YAML metadata",
#       call. = FALSE
#     ))
#   if ((key %in% names(yml)) & append_if_exist) {
#     if(val %in% yml[[key]]){
#       val <- yml[[key]]
#     }else{
#       val <- append(yml[[key]], val)
#     }
#   }
#   yml[[key]] <- val
#   seq_keys = Filter(function(key) {
#     identical(attr(yml[[key]], 'yml_type'), 'seq')
#   }, names(yml))
#   seq_keys = unique(c(seq_keys, 'categories', 'tags'))
#   
#   
#   for (i in seq_keys)
#     yml[[i]] = if (length(yml[[i]]) > 0)
#       as.list(yml[[i]])
#   if (!getOption('blogdown.yaml.empty', TRUE))
#     yml = blogdown:::filter_list(yml)
#   
#   # update the yml
#   rstudioapi::setSelectionRanges(list(c(rng[1] + 1, 1, rng[2], 1)))
#   slct = rstudioapi::getSourceEditorContext()$selection[[1]]
#   rstudioapi::modifyRange(slct$range, blogdown:::as.yaml(yml, .trim_ws = FALSE))
#   
#   ctx = rstudioapi::getSourceEditorContext()
#   txt = ctx$contents
#   res = blogdown:::split_yaml_body(txt)
#   # yml = res$yaml_list
#   new.rng = res$yaml_range
#   return((new.rng[2]-new.rng[1])- (rng[2]-rng[1]))
# }
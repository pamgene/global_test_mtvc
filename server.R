library(shiny)
library(tercen)
library(dplyr)
library(globaltest)

############################################
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################

server = shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    getDataFrame(session)
  })
  
  cFactors = reactive({
    c("<None>", getColumnFactors(session))
  })
  
  mode = reactive({getMode(session)})
  msgReactive = reactiveValues(msg = "")

  observe({
    
    df = dataInput()
    updateSelectInput(session, "gv", choices = levels(df$response))
    updateSelectInput(session, "sf", choices = cFactors())
    
    observeEvent(input$runBtn, {
      
      shinyjs::disable("runBtn")
      
      msgReactive$msg = "Running ... please wait ..."
      
      tryCatch({
        ctx = getCtx(session)
        result = setData() %>%
          group_by(Stratification.Factor) %>% 
          do(runMTvC(., input$standardize, input$directional))
        result %>%
          ctx$addNamespace() %>%
          ctx$save()
        
        msgReactive$msg = "Done"
        
      }, error = function(e) {
        msgReactive$msg = paste0("Failed : ", toString(e))
        print(paste0("Failed : ", toString(e)))
      })
    })
    
    output$mode = renderText({ 
      mode()
    })
    
    output$msg = renderText({ 
      msgReactive$msg
    })
    
    setData = reactive({
      ds = dataInput() %>%
        mutate(response = relevel(response, ref = input$gv))
      if(input$sf == "<None>"){
        ds = data.frame(ds, Stratification.Factor = "<None>")
      } else {
        s = ds %>%
          select(Stratification.Factor = input$sf)
        ds = ds %>%
          bind_cols(s)
      }
      ds
    })
    
    output$design = renderTable({
      
      m = mode()
      if (!is.null(m) && m == 'run'){
        shinyjs::enable("runBtn")
      }
      
      if (input$sf == "") return()
      aTab = setData() %>%
        group_by(Stratification.Factor) %>%
        dplyr::summarise(Test.Design = response %>%
                           droplevels() %>%
                           levels() %>%
                           paste(collapse = "/")
        )
    })
    
  })
})

getColumnFactors = function(session){
  ctx = getCtx(session)
  ctx$cnames %>%
    names()
}

getDataFrame = function(session){
  ctx <- getCtx(session)
  if(!ctx$hasXAxis) stop("Define variables using an x-axis in Tercen")
  if(length(ctx$colors) != 1) stop("Define grouping / response using a single variable as color in Tercen")
  if(length(ctx$colors) != 1) stop("Define observations using a single variable as label in Tercen")
  df = ctx %>% 
    select( .ri,.ci, .x, .y)
  df = df %>% 
    bind_cols(ctx$select(ctx$colors))
  colnames(df) = c(colnames(df)[1:4], "response")
  columns = ctx$cselect() %>%
    mutate(.ci = 0:(n()-1))
  df = df %>%
    mutate(response = response %>% as.factor) %>%
    left_join(columns, by = ".ci")
  
  df$obs = (ctx$select(ctx$labels)) %>%
    as.matrix() %>%
    apply(1, paste, collapse = "-")
  
  return(df)
}

runMTvC = function(df, standardize  = FALSE, directional = FALSE){
  control.df = df %>%
    filter(response == levels(response)[1])
  df %>%
    filter(response != levels(response)[1]) %>%
    group_by(.ri, .ci) %>%
    do(gtest(., control.df, standardize, directional)) %>%
    ungroup()
}

gtest = function(df.test, df.control, standardize = FALSE, directional = FALSE){
  df = bind_rows(df.test, df.control)
  grp.info = df %>%
    mutate(response = droplevels(response)) %>%
    distinct(obs, response)
  aGt = df %>%
    reshape2::dcast(obs ~ .x, value.var = ".y") %>%
    left_join(grp.info, by = "obs") %>%
    select(-obs) %>%
    gt(response ~ . , data = ., model = "logistic", standardize = standardize, directional = directional)
  ddf = df %>%
    group_by(response) %>%
    summarize(grpm = mean(.y))
  delta = ddf$grpm %>%
    diff()
  result = data.frame(p = p.value(aGt), globtest.statistic = aGt@result[2], nVariables = aGt@result[5], delta) 
}

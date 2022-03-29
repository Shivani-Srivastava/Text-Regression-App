library(markdown)

server <- function(input, output,session) {
  
  tr_data <-  reactive({
    req(input$tr_data$datapath)
    df <- read.csv(input$tr_data$datapath,stringsAsFactors = FALSE)
    return(df)
  })
  
 
  tr_cols <- reactive({
    req(input$tr_data$datapath)
    return(colnames(tr_data()))
  })
  
  output$y_ui <- renderUI({
    req(input$tr_data$datapath)
    selectInput(inputId = 'sel_y',label = "Select Y (Target Variable)",choices = tr_cols(),multiple = FALSE)
  })
  
  x_col <- reactive({
    req(input$tr_data$datapath)
    x <- match(input$sel_y,tr_cols())
    y_col <- tr_cols()[-x]
    return(y_col)
  })
  
  output$x_ui <- renderUI({
    req(input$tr_data$datapath)
    selectInput(inputId = "sel_x",label="Select X (Features)",choices = x_col(),multiple = TRUE,selected = x_col())
  })
  
  text_col <- reactive({
    req(input$tr_data$datapath)
    x1 <- c(input$sel_x,input$sel_y)
    x <- match(x1,tr_cols())
    text_col <- tr_cols()[-x]
    return(text_col)
  })
  
  output$text_ip_ui <- renderUI({
    req(input$tr_data$datapath)
    selectInput(inputId = "sel_text",label="Select Text column (Feature)",choices = text_col(),multiple = FALSE,selected = text_col())
  })
  
  #----Tab-2 Data Summary----#
  
  output$d_dim <- renderPrint({
    req(input$tr_data$datapath)
    size <- dim(tr_data())
    paste0("Uploaded data dimenisons: ",size[1],"(rows) X ",size[2]," (columns)")
  })
  
  output$samp <- DT::renderDataTable({
    req(input$tr_data$datapath)
    DT::datatable(head(tr_data()),
                  #filter = "top"
                  options = list(lengthMenu = list(c(10,25,50,-1),c("5","25","50","All")),
                                 autoWidth = TRUE),
                  caption = "Table 1: Sample of Data"
    )
  })
  
  output$data_str <- renderPrint({
    str(tr_data())
  })
  

  
  #---------DTM tab----------#
output$tar_dis <- renderPrint({
  req(input$tr_data$datapath)
  summary(tr_data()[,input$y])
})

reg_react <- reactiveValues()


observeEvent(input$apply,{
  progress <- shiny::Progress$new(min = 0,max = 10)
  progress$set(message = "Generating DTM..", value = 3)
  
  #my_dtm <- reactive({
    tfidf_ui <- ifelse(input$en_scheme=="tfidf",TRUE,FALSE)
    text_col_ui <- tr_data()[,input$sel_text]
    my_dtm = text_regn_main(text_col_ui, input$min_occ, 
                            input$max_occ, tfidf_ui = tfidf_ui)
  #  my_dtm
 # })
  
  reg_react$my_dtm <- my_dtm
  
  progress$set(message = "Creating sample..", value = 6)
  
  set.seed(123)
  a00 <- tr_data()
  a0_ix = sample(seq(1:nrow(a00)), size = round(nrow(a00)*input$tr_per,0))
  
  a00_train = a00[a0_ix,]; dtm_train = my_dtm[a0_ix,]
  a00_test = a00[!a0_ix,]; dtm_test = my_dtm[!a0_ix,]
  
  y = a00[,input$sel_y]  # from UI
  X = a00[,input$sel_x]
  # X = data.frame(category = a00$category, # from UI, variable selection
  #                goal = a00$goal, backers = a00$backers) 
  # df0 is analysis DF from here
  df0_train = data.frame(y=y[a0_ix], X[a0_ix,], dtm_train[,1:input$n_terms])
  df0_test = data.frame(X[!a0_ix,], dtm_test[,1:input$n_terms])
  
  progress$set(message = "Training Model...", value = 8)
  a0 = lm(y~., data=df0_train, na.rm=TRUE) # 1.8s for n=250
  progress$set(message = "Model Trained", value = 8)
  on.exit(progress$close())
  reg_react$a0 = a0
})

output$dtm_head <- DT::renderDataTable({
  datatable(head(reg_react$my_dtm)[,1:10])
})

output$dtm_size <- renderPrint({
  size <- dim(reg_react$my_dtm)
  paste0("DTM dimenisons: ",size[1],"(rows) X ",size[2]," (columns)")
})

a1 <- reactive({
  req(input$tr_data$datapath)
  a1 <- as.data.frame(summary(reg_react$a0)$coefficients%>% round(., 3))
  a1
})

output$mod_coef <- DT::renderDataTable({
  req(input$tr_data$datapath)
  #a1 <- as.data.frame(summary(reg_react$a0)$coefficients%>% round(., 3))
  datatable(a1())
})

output$r_sq <- renderPrint({
  req(input$tr_data$datapath)
  cat("The R-square is: ", summary(reg_react$a0)$r.squared)
})

output$f_st <- renderPrint({
  req(input$tr_data$datapath)
  cat("The F-statistic is: ", summary(reg_react$a0)$fstatistic)
  
})

output$dwnld <- downloadHandler(
  filename = function() { "model_summary.csv" },
  content = function(file) {
    write.csv(a1(), file,row.names=TRUE)
  }
)

  # 
}

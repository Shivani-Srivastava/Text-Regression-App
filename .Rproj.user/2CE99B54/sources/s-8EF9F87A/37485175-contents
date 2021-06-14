shinyUI(fluidPage(
  
  title = "Text Regression",
  titlePanel(title=div(img(src="logo.png",align='right'),"Text Regression")),
  sidebarPanel(
   
      conditionalPanel(condition="input.tabselected==1",
                     fileInput("tr_data","Upload Training Dataset",placeholder = "No File Selected"),
                     uiOutput("y_ui"),
                     uiOutput("x_ui"),
                     uiOutput("text_ip_ui"),
                     sliderInput("tr_per",
                                 label = "Percentage of training data",
                                 min = 0,
                                 max = 1,
                                 value = 0.7,
                                 step = 0.05)
                
                     ),
      conditionalPanel(condition="input.tabselected==2",
                       radioButtons('en_scheme',"Select text encoding scheme",choices = list("TF-IDF" = "tfidf",
                                                                                             "TF" = "tf")),
                       sliderInput("min_occ","Minimum Occurence",min = 0,max = 0.7,step = 0.01,value = 0.1),
                       sliderInput("max_occ","Maximum Occurence",min = 0.5,max = 1,step = 0.01,value = 0.5),
                       numericInput("n_terms","Number of Terms",min = 10,max = 1000,value=250),
                       actionButton("apply","Run Regression")
                       
                       )
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Overview & Example Dataset", value=1, 
               includeMarkdown("overview.md")
      ),
      tabPanel("Data Summary", value=1,
               h4("Data Dimension"),
               verbatimTextOutput("d_dim"),
               hr(),
               h4("Sample of uploaded dataset"),
               DT::dataTableOutput("samp"),
               hr(),
               h4("Data Structure"),
               verbatimTextOutput("data_str")
          ),
               
        tabPanel("DTM construction", value=2,
               
               h4("DTM size"),
               verbatimTextOutput("dtm_size"),
               h4("Sample DTM"),
               DT::dataTableOutput("dtm_head"),

      ),
      tabPanel("Regression Results",value=2,
               #h4("Distribution of Target Variable (Y)"),
              # verbatimTextOutput("tar_dis"),
                hr(),
               downloadButton("dwnld","Download Model Summary"),
               hr(),
               h4("Model Coefficients"),
               DT::dataTableOutput("mod_coef"),
               hr(),
               verbatimTextOutput("r_sq"),
               verbatimTextOutput("f_st")
      ),
      id = "tabselected"
    )
  )
))

# Dashboard of anomaly scoring of time series, 
# Guillermo C. Granados-Garcia
# Lancaster University
# Department of Mathematics and Statistics
# Project: Reducing End Use Energy Demand in Commercial Settings Through 
# Digital Innovation

library(shiny)
library(bslib)
library(markdown)
source("matplot_plotscore.R")

# Define UI for application that draws a histogram
ui <- page_sidebar(
  div(
    id = "welcome-message",
    class = "alert alert-info",
    style = "text-align: center; font-size: 16px; margin-top: 20px;",
    HTML("<strong>Welcome!</strong> To get started and load your CSV file, click the <strong>Browse</strong> button."),
    actionButton("dismiss_message", "Got it", class = "btn btn-primary", style = "margin-top: 10px;")
  ),
    # Application title
    titlePanel("Multivariate Time Series Anomaly Scoring"),

    # Sidebar layout with input and output definitions ----
 
      # Sidebar panel for inputs ----
     sidebar= sidebar(
       tags$style(type='text/css', ".checkbox {font-size: 12px !important} "),
       tags$style("#num {font-size:15px;}"),
        # Input: Select a file ----
        fileInput("file1", "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),
       downloadButton("downloadData", "Download", style="color: #fff; background-color: green; border-color: Black;"),
        # Input: Checkbox if file has header ----
        
        
       checkboxGroupInput("standarization", label = h6("Standardization"), 
                          choices = list('Standard deviation scaling' = 1,
                                         'Mean subtraction' = 2
                          )),
       
       checkboxInput("cblind", "Alternative color palette", TRUE),
       
        # Input: number of nearest neighbors
  
        
        #Input selection of distance
        checkboxGroupInput("checkGroup", label = h6("Distace"), 
                           choices = list('Cort' = 1,
                                          'Wasserstein' = 2, 
                                          'Mahalanobis' = 3,
                                          'CortNorm'=4,
                                          'Coherence'=5,
                                          'PDC'=6,
                                          'CGCI'=7,
                                          'RGPDC'=8,
                                          'PMIME'=9,
                                          'mvLWS'=10,
                                          'Band depth'=11
                                          ),
                           selected = 1)
      ),
    
    navset_card_underline( 
    # Main panel for displaying outputs ----
   
    nav_panel("Analysis", 
              
              layout_columns(
              card(plotOutput("tabplot",  width = "100%",fill = T)     ),
              layout_columns(
              card( plotOutput("scoreplot",  width = "100%") ),
              card( plotOutput("allplot") ,  width = "100%"),col_widths = c(12, 12)
              
              ),col_widths = c(3, 9)
              )
              
              ),

    nav_panel("Parameters", 
               style = "font-size:15px;",
              
              layout_columns(
                card(card_header("All Distances"), numericInput("num", "Number of nearest neighbors", value = 4 )),
                card(card_header("Cort"),numericInput("cortK", "k", value = 3)),
                card(card_header("Normalized cort"),numericInput("cortnormK", "k", value = 3)),
                card(card_header("CGCI"),numericInput("cgcipmax", "Maximum lag", value = 10))
              ),
              
              layout_columns(
              card(card_header("RGPDC"),numericInput("rgpdcpmax", "Maximum lag", value = 10),numericInput("rgpdcperiod", "Period", value = 5)),
              card(card_header("PMIME"),numericInput("pmimepmax", "Maximum lag", value = 10),numericInput("pmimeahead", "Steps ahead", value = 1),numericInput("pmimennei", "# Density neighbors", value = 10),numericInput("pmimethres", "Threshold", value = .95) ),
              card(card_header("Coherence"),numericInput("coherencespan1", "Span 1", value = 3),numericInput("coherencespan2", "Span 2", value = 3),numericInput("coherenceperiod", "Period", value = 5) ),
              card(card_header("PDC"), textInput('pdclags', 'Autoregressive lags (comma delimited)', "1,2"),numericInput("pdcperiod", "Period", value = 5)  )
              )
              
              ),
    
    nav_panel("Instructions", includeMarkdown("Instructions.md") )
  
      # Output: Data file ----
  #    card( plotOutput("scoreplot",  width = "100%") ),
   #   card( plotOutput("allplot") ,  width = "100%"),
      
    ),
    
    tags$footer(strong("The dashboard only keeps your data for the duration of your session and solely for the purpose of conducting your analysis. Once you close this tab or your session ends, no data is retained or stored in any way. We do not keep any copies of your uploaded data. Please note that while the dashboard aims to provide accurate and useful analysis, it does not guarantee the accuracy, completeness, or reliability of the results. Users should independently verify any results obtained through this tool."), # strong() = bold
                align = "left", 
                style = "
                 position:relative;
                 bottom:0px;
                 left:0px;
                 width:100%;
                 height:30px; 
                 color: black;
                 font-size: 11px;
                ")

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$dismiss_message, {
    # Hide the welcome message
    removeUI(selector = "#welcome-message")
  })
  
 mydata <- reactive({
   # input$file1 will be NULL initially. After the user selects
   # and uploads a file, head of that data file by default,
   # or all rows if selected, will be shown.
   req(input$file1)
   # when reading semicolon separated files,
   # having a comma separator causes `read.csv` to error
   tryCatch(
     {
       
       if(input$header){
       df <- read.csv(input$file1$datapath,
                      header =  input$header   )
       }else{
         df <- read.csv(input$file1$datapath,
                        header =  input$header   );
         myncols<-ncol(df);
         names(df)<-paste0("Series_",1:myncols   );
         
       }
       
       
       
     },
     error = function(e) {
       # return a safeError if a parsing error occurs
       stop(safeError(e))
     }
   )
   
   standvec= as.numeric(input$standarization)
   lstand=length(standvec)
   cs_true=c(T,T)
   cs_default=c(F,F)
   if(lstand>0){cs_default[standvec]=cs_true[standvec] }  
    
  df=scale(as.matrix(df), center = cs_default[2] , scale = cs_default[1])
   
     return(df)
 })
 
 dparams <- reactive({ 
   par=list(
     list(unit=mydata(), k=input$cortK),
     list(unit=mydata() ),
     list(unit=mydata() ),
     list(unit=mydata(), k=input$cortnormK),
     list(unit=mydata(), span1=input$coherencespan1, span2=input$coherencespan2, period = input$coherenceperiod),
     list(unit=mydata(), ar=as.numeric(unlist(strsplit(input$pdclags,","))), period = input$pdcperiod ),
     list(unit=mydata() , pmax=input$cgcipmax ),
     list(unit=mydata() , pmax=input$rgpdcpmax, period=input$rgpdcperiod),
     list(unit=mydata(), Lmax=input$pmimepmax, Tl=input$pmimeahead, nnei=input$pmimennei, A=input$pmimethres ),
     list(unit=mydata()  ),
     list(unit=mydata() )
   )
   return(  par )  
 }) 
 
scores <- reactive({ 
return(  frameallscores(unit=mydata(), knn=as.numeric(input$num),measures=as.numeric(input$checkGroup),dparams=dparams() ) )  
  })

    output$scoreplot <- renderPlot({
   plot_score(frame_scores=scores(),measures=as.numeric(input$checkGroup),input$cblind)
    })
    
    output$allplot<-renderPlot({
      plotseries(unit=mydata(),frame_scores=scores(),measures=as.numeric(input$checkGroup),colorblind=input$cblind)  
    }, height = 300)
    
    output$tabplot<-renderPlot({
      plot_onlytable(frame_scores=scores(),measures=as.numeric(input$checkGroup),colorblind=input$cblind)  
    }, height = 800)
    
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Framescores", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(scores(), file, row.names = FALSE)
      }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

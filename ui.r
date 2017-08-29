## MatrixEQTL Shiny App##
#########################

library(shiny)
library(shinydashboard)


header <- dashboardHeader( title = "eQTL analysis via MatrixEQTL",
                           titleWidth = 350
                           )


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Inputs and settings", tabName = "inputs", icon = icon("folder-open"),
             badgeLabel = "Important!", badgeColor = "red"),
    menuItem("Results", tabName = "results", icon = icon("file-text")),
    menuItem("Source code", icon = icon("file-code-o"), 
             href = "https://github.com/IgorHut/Shiny_MatrixEQTL", newtab = TRUE)
  )
)

body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "inputs", 
            # Boxes need to be put in a row (or column)
            fluidRow(
                tabBox(
                  title = "Input files",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1",
                  width = 12,
                  tabPanel("SNP file", "Explanation...",
                           fileInput("snp_file", "Choose SNP file",
                                     buttonLabel = "Browse...", 
                                     placeholder = "No file selected",
                                     accept = "text/tab-separated-values")
                           ),
                  tabPanel("GE file", "Explanation...",
                           fileInput("ge_file", "Choose GE file",
                                     buttonLabel = "Browse...", 
                                     placeholder = "No file selected",
                                     accept = "text/tab-separated-values")
                  ),
                  tabPanel("SNPs locations","Explanation...",
                           fileInput("snpsloc_file", "Choose snpsloc file",
                                     buttonLabel = "Browse...", 
                                     placeholder = "No file selected",
                                     accept = "text/tab-separated-values")
                           ),
                  tabPanel("Gene locations","Explanation...",
                           fileInput("geneloc_file", "Choose geneloc file",
                                     buttonLabel = "Browse...", 
                                     placeholder = "No file selected",
                                     accept = "text/tab-separated-values")
                           ),
                  tabPanel("Covariates","Explanation...",
                           fileInput("covarates_file", "Choose covariates file",
                                     buttonLabel = "Browse...", 
                                     placeholder = "No file selected",
                                     accept = "text/tab-separated-values")
                  )
                )
            ),
            
          fluidRow(
                tabBox(
                  # Title can include an icon
                  title = "Settings",
                  id = "tabset2",
                  width = 12,
                  tabPanel("Model",
                           "Choose a model to use. 
                           Avalilable models are: modelANOVA, 
                           modelLINEAR, or modelLINEAR_CROSS",
                           selectizeInput(
                             'select_1', 'Select a model',
                             choices = c("modelANOVA", 
                                         "modelLINEAR", 
                                         "modelLINEAR_CROSS")
                           )
                  ),
                  tabPanel("P-value thresholds",
                           "Only associations significant at this level will be saved",
                           numericInput("p_value_cis", 
                                        "P-value threshold for cis eQTLs",
                                        step = 0.001, value = 0.01,
                                        max = 0.1, min = 0),
                           numericInput("p_value_trans", 
                                        "P-value threshold for trans eQTLs",
                                        step = 0.001, value = 0.01,
                                        max = 0.1, min = 0)
                           ),
                  
                  tabPanel("Cis distance",
                           numericInput("cis_distance", 
                                        "Distance for local gene-SNP pairs",
                                        step = 1e5, value = 1e6,
                                        max = 1e10, min = 0)
                           )
                )
              )
        ),
    
    

    #Second tab content
    tabItem(tabName = "results",
            h2("Widgets tab content",
               fluidRow(
                 box(
                   title = "Random box",
                   "Box content here", br(), "More box content",
                   textInput("text", "Some text here:")
                 ),
                 box(
                   title = "Histogram", status = "warning", solidHeader = TRUE,
                   collapsible = TRUE,
                   plotOutput("plot2", height = 250)
                 )
               )
               )
)
))

  


dashboardPage(header, sidebar, body)



## MatrixEQTL Shiny App ##
##########################

library(shiny)
library(shinydashboard)
library(MatrixEQTL)
library(shinyFiles)





header <- dashboardHeader( title = "eQTL analysis via MatrixEQTL",
                           titleWidth = 350
                           )


sidebar <- dashboardSidebar(
  width = 350,
  sidebarMenu(
    menuItem("Instructions", tabName = "instructions", icon = icon("book")),
    menuItem("Inputs and settings", tabName = "inputs", icon = icon("folder-open"),
             badgeLabel = "Important!", badgeColor = "red"),
    # actionButton("calculate1", "Calculation", icon = icon("play-circle")),
    menuItem("Results", tabName = "results", icon = icon("file-text")),
    menuItem("Plots", tabName = "plots", icon = icon("picture-o")),
    menuItem("Source code", icon = icon("file-code-o"), 
             href = "https://github.com/IgorHut/Shiny_MatrixEQTL", newtab = TRUE)
  ),
  
  br(),
  br(),
  br(),
  br(),
  br(),
  hr(),
  
  tags$span(style = "color:darkorange",
            tags$footer(("Built by"), a( href = "http://igorhut.com/", target = "_blank", "Igor Hut"),
                        br(),
                        ("Code licence:"), a( href = "https://www.r-project.org/Licenses/MIT", 
                                              target = "_blank", "MIT")
            )
  )
)

body <- dashboardBody(
  tabItems(
    #First tab content
    tabItem(tabName = "instructions",
            h1("Instructions"),
            br(),
            h3("Description"),
            "Matrix eQTL is designed for fast eQTL analysis of large datasets. 
            Matrix eQTL can test for association between genotype and gene expression 
            using linear regression with either additive or ANOVA (additive and dominant) 
            genotype effects. The models can include covariates to account for such factors 
            as population stratification, gender, clinical variables, and surrogate variables. 
            Matrix eQTL also supports models with heteroscedastic and/or correlated errors, 
            false discovery rate estimation and separate treatment of local (cis) and distant 
            (trans) eQTLs."
            ),
    # Second tab content
    tabItem(tabName = "inputs", 
            # Boxes need to be put in a row (or column)
            fluidRow(
                tabBox(
                  title = "Input files",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1",
                  width = 12,
                  tabPanel("SNP file", "Explanation...",
                           br(),
                           br(),
                           shinyFilesButton("snp_file", "Choose the SNP file" ,
                                            title = "Please select a file:", multiple = FALSE,
                                            buttonType = "primary", class = NULL),
                           br(),
                           textOutput("txt_file1")
                           ),
                  tabPanel("GE file", "Explanation...",
                           br(),
                           br(),
                           shinyFilesButton("ge_file", "Choose the GE file" ,
                                            title = "Please select a file:", multiple = FALSE,
                                            buttonType = "primary", class = NULL),
                           br(),
                           textOutput("txt_file2")
                  ),
                  tabPanel("SNPs locations", "Explanation...",
                           br(),
                           br(),
                           shinyFilesButton("snpsloc_file", "Choose snpsloc file" ,
                                            title = "Please select a file:", multiple = FALSE,
                                            buttonType = "primary", class = NULL),
                           br(),
                           textOutput("txt_file3")
                  ),
                  tabPanel("Gene locations", "Explanation...",
                           br(),
                           br(),
                           shinyFilesButton("geneloc_file", "Choose geneloc file" ,
                                            title = "Please select a file:", multiple = FALSE,
                                            buttonType = "primary", class = NULL),
                           br(),
                           textOutput("txt_file4")
                  ),
                  tabPanel("Covariates", "Explanation...",
                           br(),
                           br(),
                           shinyFilesButton("covariates_file", "Choose covariates file" ,
                                            title = "Please select a file:", multiple = FALSE,
                                            buttonType = "primary", class = NULL),
                           br(),
                           textOutput("txt_file5")
                  )
                  
               )
            ),
            
          fluidRow(
                tabBox(
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
              ),
          fluidRow(
            box(
              background = "blue",
              actionButton("calculate1", "Calculation", 
                           width = '100%', icon = icon("play-circle"))
              
            )
            
          )
        ),
    
    

    #Third tab content
    tabItem(tabName = "results",
            fluidRow(
              tabBox(
                title = "Results",
                id = "tabset3",
                width = 12,
                tabPanel(
                  "Cis eQTLs",
                  DT::dataTableOutput('tbl_cis')
                ),
                tabPanel(
                  "Trans eQTLs",
                  DT::dataTableOutput('tbl_trans')
              )
              )
            )
            ),
            #Fourth tab content
            tabItem(tabName = "plots",
                    fluidRow(
                      tabBox(
                        title = "Plots",
                        id = "tabset4",
                        width = 12,
                        tabPanel(
                          "P-values histogram",
                          plotOutput("pvalue_plot")
                        ),
                        tabPanel(
                          "Q-Q plot for p-values",
                          plotOutput("qq_plot")
                        ),
                        tabPanel("Heatmap for cis eQTLs",
                                 d3heatmapOutput("heatmap_cis")
                                 # plotOutput("heatmap_cis")

                        )
                        
                        

                      )
                    )
                    
                    )
  
)
)

  


dashboardPage(header, sidebar, body)



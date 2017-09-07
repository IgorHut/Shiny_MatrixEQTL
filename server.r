## MatrixEQTL Shiny App ##
##########################

library(shiny)
library(shinydashboard)
library(MatrixEQTL)
library(shinyFiles)


server <- function(input, output, session) { 
  
 # Mockup plot 
  output$second_plot <- renderPlot({
    plot(1:10, 1:10, pch=0:10, col=1:10, cex=1:10, main = input$mytext)
  })
############################################################################# 
  
  roots = c(wd ='/Users/igorhut/Documents/eQTL/Code/')
  observe({  
    shinyFileChoose(input, "snp_file", roots = roots, session = session, 
                    filetypes=c('', 'txt'))
    
    shinyFileChoose(input, "ge_file", roots = roots, session = session, 
                    filetypes=c('', 'txt'))
    
    shinyFileChoose(input, "snpsloc_file", roots = roots, session = session, 
                    filetypes=c('', 'txt'))
    
    shinyFileChoose(input, "geneloc_file", roots = roots, session = session, 
                    filetypes=c('', 'txt'))
    
    shinyFileChoose(input, "covariates_file", roots = roots, session = session, 
                    filetypes=c('', 'txt'))
    
    if(!is.null(input$snp_file)){
      # browser()
      file_selected1<-parseFilePaths(roots, input$snp_file)
      output$txt_file1 <- renderText(as.character(file_selected1$datapath))
    }
    
    if(!is.null(input$ge_file)){
      # browser()
      file_selected2<-parseFilePaths(roots, input$ge_file)
      output$txt_file2 <- renderText(as.character(file_selected2$datapath))
    }
    
    if(!is.null(input$snpsloc_file)){
      # browser()
      file_selected3<-parseFilePaths(roots, input$snpsloc_file)
      output$txt_file3 <- renderText(as.character(file_selected3$datapath))
    }
    
    if(!is.null(input$geneloc_file)){
      # browser()
      file_selected4<-parseFilePaths(roots, input$geneloc_file)
      output$txt_file4 <- renderText(as.character(file_selected4$datapath))
    }
    
    if(!is.null(input$covariates_file)){
      # browser()
      file_selected5<-parseFilePaths(roots, input$covariates_file)
      output$txt_file5 <- renderText(as.character(file_selected5$datapath))
    }
    
  })
  
  SNP_file_name <- reactive({
    as.character(parseFilePaths(roots, input$snp_file)$datapath)
  })
  
  expression_file_name <- reactive({
    as.character(parseFilePaths(roots, input$ge_file)$datapath)
  })
  
  snps_location_file_name <- reactive({
    as.character(parseFilePaths(roots, input$snpsloc_file)$datapath)
  })
  
  gene_location_file_name <- reactive({
    as.character(parseFilePaths(roots, input$geneloc_file)$datapath)
  })
  
  covariates_file_name <- reactive({
    as.character(parseFilePaths(roots, input$covariates_file)$datapath)
  })
    
#     if(!is.null(input$snp_file)){
#       # browser()
#       file_selected <- parseFilePaths(roots, input$snp_file)
#       SNP_file_name <- as.character(file_selected$datapath)
#     }
# })

  
  model <- reactive({input$select_1})
  p_value_cis <- reactive({input$p_value_cis})
  p_value_trans <- reactive({input$p_value_trans})
  cis_distance <- reactive({input$cis_distance})




observeEvent(input$calculate1, {
    ## Settings
     # Linear model to use, modelANOVA, modelLINEAR, or modelLINEAR_CROSS
     if(model() == "modelLINEAR"){
       useModel = modelLINEAR
     } else if (model() == "modelANOVA"){
       useModel = modelANOVA
     } else {
       useModel = modelLINEAR_CROSS
     }
     

     # Only associations significant at this level will be saved
     pvOutputThreshold_cis = p_value_cis();
     pvOutputThreshold_tra = p_value_trans();
     # Error covariance matrix
     # Set to numeric() for identity.
     errorCovariance = numeric();

     # errorCovariance = read.table("Sample_Data/errorCovariance.txt");
     # Distance for local gene-SNP pairs
     cisDist = cis_distance()

     # Output file name
     output_file_name_cis = tempfile();
     output_file_name_tra = tempfile();

     ## Load genotype data
     snps = SlicedData$new();
     snps$fileDelimiter = "\t"; # the TAB character
     snps$fileOmitCharacters = "NA"; # denote missing values;
     snps$fileSkipRows = 1; # one row of column labels
     snps$fileSkipColumns = 1; # one column of row labels
     snps$fileSliceSize = 2000; # read file in slices of 2,000 rows
     snps$LoadFile(SNP_file_name());

     ## Load gene expression data
     gene = SlicedData$new();
     gene$fileDelimiter = "\t"; # the TAB character
     gene$fileOmitCharacters = "NA"; # denote missing values;
     gene$fileSkipRows = 1; # one row of column labels
     gene$fileSkipColumns = 1; # one column of row labels
     gene$fileSliceSize = 2000; # read file in slices of 2,000 rows
     gene$LoadFile(expression_file_name());

     ## Load covariates
     cvrt = SlicedData$new();
     cvrt$fileDelimiter = "\t"; # the TAB character
     cvrt$fileOmitCharacters = "NA"; # denote missing values;
     cvrt$fileSkipRows = 1; # one row of column labels
     cvrt$fileSkipColumns = 1; # one column of row labels
     if(length(covariates_file_name())>0) {
       cvrt$LoadFile(covariates_file_name());
     }

     ## Run the analysis
     snpspos = read.table(snps_location_file_name(), header = TRUE, stringsAsFactors = FALSE);
     genepos = read.table(gene_location_file_name(), header = TRUE, stringsAsFactors = FALSE);
     
     # Calculation - histogram
     meh = Matrix_eQTL_main(
       snps = snps,
       gene = gene,
       cvrt = cvrt,
       output_file_name = output_file_name_tra,
       pvOutputThreshold = pvOutputThreshold_tra,
       useModel = useModel,
       errorCovariance = errorCovariance,
       verbose = FALSE,
       output_file_name.cis = output_file_name_cis,
       pvOutputThreshold.cis = pvOutputThreshold_cis,
       snpspos = snpspos,
       genepos = genepos,
       cisDist = cisDist,
       pvalue.hist = TRUE,
       min.pv.by.genesnp = FALSE,
       noFDRsaveMemory = FALSE);
     
     meq = Matrix_eQTL_main(
       snps = snps,
       gene = gene,
       cvrt = cvrt,
       output_file_name = output_file_name_tra,
       pvOutputThreshold = pvOutputThreshold_tra,
       useModel = useModel,
       errorCovariance = errorCovariance,
       verbose = FALSE,
       output_file_name.cis = output_file_name_cis,
       pvOutputThreshold.cis = pvOutputThreshold_cis,
       snpspos = snpspos,
       genepos = genepos,
       cisDist = cisDist,
       pvalue.hist = "qqplot",
       min.pv.by.genesnp = FALSE,
       noFDRsaveMemory = FALSE); 

     unlink(output_file_name_tra);
     unlink(output_file_name_cis);

     ## Results:
     output$tbl_cis <-  DT::renderDataTable(
       meh$cis$eqtls, options = list(lengthChange = FALSE)
     )
     
     output$tbl_trans <-  DT::renderDataTable(
       meh$trans$eqtls, options = list(lengthChange = FALSE)
     )
     
     ## Make the histogram of local and distant p-values
     output$first_plot <- renderPlot({
       plot(meh)
     })
     
     output$second_plot <- renderPlot({
       plot(meq)
     })


    })

  


}


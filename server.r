## MatrixEQTL Shiny App ##
##########################

library(shiny)
library(shinydashboard)
library(MatrixEQTL)



server <- function(input, output, session) { 
  
  
  output$tbl_cis <-  DT::renderDataTable(
    iris, options = list(lengthChange = FALSE)
  )
  
  output$tbl_trans <-  DT::renderDataTable(
    iris, options = list(lengthChange = FALSE)
  )
  
  output$first_plot <- renderPlot({
    plot(1:10, 1:10, pch=10:20, col=1:10, cex=1:10, main = input$mytext)
  })
  
  output$second_plot <- renderPlot({
    plot(1:10, 1:10, pch=0:10, col=1:10, cex=1:10, main = input$mytext)
  })
  # SNP_file_name <-  input$snp_file
  # snps_location_file_name <- input$snpsloc_file
  # expression_file_name <-  input$ge_file
  # gene_location_file_name <- input$geneloc_file
  # covariates_file_name <- input$covarates_file
  # 
  # 
  # 
  #   # Output file name
  #   output_file_name_cis = tempfile();
  #   output_file_name_tra = tempfile();
  # 
  #   ## Settings
  #   # Linear model to use, modelANOVA, modelLINEAR, or modelLINEAR_CROSS
  #   useModel = modelLINEAR; # modelANOVA, modelLINEAR, or modelLINEAR_CROSS
  # 
  #   # Only associations significant at this level will be saved
  #   pvOutputThreshold_cis = 2e-2;
  #   pvOutputThreshold_tra = 1e-2;
  #   # Error covariance matrix
  #   # Set to numeric() for identity.
  #   errorCovariance = numeric();
  # 
  #   # errorCovariance = read.table("Sample_Data/errorCovariance.txt");
  #   # Distance for local gene-SNP pairs
  #   cisDist = 1e6
  # 
  #   ## Load genotype data
  #   snps = SlicedData$new();
  #   snps$fileDelimiter = "\t"; # the TAB character
  #   snps$fileOmitCharacters = "NA"; # denote missing values;
  #   snps$fileSkipRows = 1; # one row of column labels
  #   snps$fileSkipColumns = 1; # one column of row labels
  #   snps$fileSliceSize = 2000; # read file in slices of 2,000 rows
  #   snps$LoadFile(SNP_file_name);
  # 
  #   ## Load gene expression data
  #   gene = SlicedData$new();
  #   gene$fileDelimiter = "\t"; # the TAB character
  #   gene$fileOmitCharacters = "NA"; # denote missing values;
  #   gene$fileSkipRows = 1; # one row of column labels
  #   gene$fileSkipColumns = 1; # one column of row labels
  #   gene$fileSliceSize = 2000; # read file in slices of 2,000 rows
  #   gene$LoadFile(expression_file_name);
  # 
  #   ## Load covariates
  #   cvrt = SlicedData$new();
  #   cvrt$fileDelimiter = "\t"; # the TAB character
  #   cvrt$fileOmitCharacters = "NA"; # denote missing values;
  #   cvrt$fileSkipRows = 1; # one row of column labels
  #   cvrt$fileSkipColumns = 1; # one column of row labels
  #   if(length(covariates_file_name)>0) {
  #     cvrt$LoadFile(covariates_file_name);
  #   }
  # 
  #   ## Run the analysis
  #   snpspos = read.table(snps_location_file_name, header = TRUE, stringsAsFactors = FALSE);
  #   genepos = read.table(gene_location_file_name, header = TRUE, stringsAsFactors = FALSE);
  #   me = Matrix_eQTL_main(
  #     snps = snps,
  #     gene = gene,
  #     cvrt = cvrt,
  #     output_file_name = output_file_name_tra,
  #     pvOutputThreshold = pvOutputThreshold_tra,
  #     useModel = useModel,
  #     errorCovariance = errorCovariance,
  #     verbose = TRUE,
  #     output_file_name.cis = output_file_name_cis,
  #     pvOutputThreshold.cis = pvOutputThreshold_cis,
  #     snpspos = snpspos,
  #     genepos = genepos,
  #     cisDist = cisDist,
  #     pvalue.hist = TRUE,
  #     min.pv.by.genesnp = FALSE,
  #     noFDRsaveMemory = FALSE);
  # 
  #   unlink(output_file_name_tra);
  #   unlink(output_file_name_cis);
  # 
  #   ## Results:
  #   cat('Detected local eQTLs:', capture.output(me$cis$eqtls), file = 'output_cis_eqtls.txt', sep = '\n')
  #   cat('Detected distant eQTLs:', capture.output(me$trans$eqtls), file = 'output_trans_eqtls.txt', sep = '\n')
  # 
  #   ## Make the histogram of local and distant p-values
  #   pdf("hist.pdf")
  #   plot(me)
  #   dev.off()
  # })
}


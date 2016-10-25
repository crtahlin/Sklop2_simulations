#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$select_folder <- renderUI({
    # read folders holding results
    folders <- list.dirs("../results")
    # generate select input
    selectInput("selected_folder",
                "Select folder",
                choices = folders,
                selected = NULL)
  })
  
  results_index <- reactive({
    # read results in selected folder
    load(file = paste0(input$selected_folder, "/results_index.Rdata"))
    return(results_index)
  })
  
  output$results_index <- DT::renderDataTable({
    result <- datatable(results_index(),
                        filter = 'top',
                        selection = "single")
    return(result)
  }, server = TRUE)

  results <- reactive({
   # browser()
    
    selected <- input$results_index_rows_selected
    hashes <- results_index()[selected, "hash"]
    # only take the first selected row
    hash <- hashes[1]
    load(paste0(input$selected_folder, "/", hash, ".Rdata"))
    return(result)
  })
  # 
  # output$x4 = renderPrint({
  #   s = input$x3_rows_selected
  #   if (length(s)) {
  #     cat('These rows were selected:\n\n')
  #     cat(s, sep = ', ')
  #   }
  # })
  
  # output$select_simluation <- renderUI({
  #   selectInput("selected_simulation",
  #               "Select simulation:",
  #               choices = results_hashes(),
  #               selected = NULL)
  # })
  # 
  # results <- reactive({
  #   result <- results_list()[[input$selected_simulation]]
  #   return(result)
  # })
  # 
  output$plot_probability <- renderPlot({
    curve(
      sine_function(x,
                    par1sin = results()$par1sin,
                    par2sin = results()$par2sin,
                    par3sin = results()$par3sin,
                    par4sin = results()$par4sin,
                    par5sin = results()$par5sin,
                    par1trend = results()$par1trend,
                    par2trend = results()$par2trend,
                    par3trend = results()$par3trend,
                    par4trend = results()$par4trend,
                    par5trend = results()$par5trend,
                    add_trend = results()$add_trend,
                    max_prob_value = results()$max_prob_value,
                    min_prob_value = results()$min_prob_value),
      from = 0,
      to = results()$tmax,
      main = "Probability function used in simulation",
      ylab = "Probability")

  })
  
  output$averages <- DT::renderDataTable({
    # calculate average from data saved in my.res
    averages <- apply(X= results()$my.res, FUN = function(x) {mean(x)}, MARGIN = 2)
    averages <- signif(averages, 4)
    # make the averages into a data frame with named rows/columns
    attach(as.list(averages))
    averages_dt <- data.frame(
      rcs = c(brier.rcs.train, brier.rcs, AUCTrainEst.rcs, AUCNewEst.rcs, cal.rcs.1, cal.rcs.2, lrt.p.value.rcs.train, score.p.value.rcs.train),
      rcs.per = c(brier.rcs.per.train, brier.rcs.per, AUCTrainEst.rcs.per, AUCNewEst.rcs.per, cal.rcs.per.1, cal.rcs.per.2, lrt.p.value.rcs.per.train, score.p.value.rcs.per.train),
      cs.per = c(brier.cs.per.train, brier.cs.per, AUCTrainEst.cs.per, AUCNewEst.cs.per, cal.cs.per.1, cal.cs.per.2, lrt.p.value.cs.per.train, score.p.value.cs.per.train),
      row.names = c("Brier - train", "Brier - test", "AUC - train", "AUC - test", "Calibration intercept", "Calibration slope", "LRT P value", "Score P value")
    ) 
    
  })
  
})

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
  
})

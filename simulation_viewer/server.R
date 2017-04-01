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
library(dplyr)

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
    results_index$knots <- as.character(results_index$knots)
    return(results_index)
  })
  
  # TODO: add menus on the left: B, knots
  
  output$select_B_variants <- renderUI({
    B_variants <- unique(results_index()$B)
    
    selectInput("selected_B_variant",
                label = "Filter on B (number of simulations):",
                choices = B_variants)
  })
  
  output$select_knots_variants <- renderUI({
    knot_variants <- unique(results_index()$knots)
    
    selectInput("selected_knot_variant",
                label = "Filter on knot variant:",
                choices = knot_variants)

  })
  
  results_index_filtered <- reactive({
    result <- results_index()[(results_index()[, "knots"] == input$selected_knot_variant & 
                                 results_index()[, "B"] == input$selected_B_variant), ]
    return(result)
  })
  
  output$results_index <- DT::renderDataTable({
    result <- datatable(results_index_filtered(),
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
  
  output$power_stats <- DT::renderDataTable({
    power_lrt_rcs <- sum(results()$my.res[,"lrt.p.value.rcs.train"] < 0.05) / length(results()$my.res[,"lrt.p.value.rcs.train"])
    power_lrt_rcs.per <- sum(results()$my.res[,"lrt.p.value.rcs.per.train"] < 0.05) / length(results()$my.res[,"lrt.p.value.rcs.per.train"])
    power_lrt_cs.per <- sum(results()$my.res[,"lrt.p.value.cs.per.train"] < 0.05) / length(results()$my.res[,"lrt.p.value.cs.per.train"])
    power_score_rcs <- sum(results()$my.res[,"score.p.value.rcs.train"] < 0.05) / length(results()$my.res[,"score.p.value.rcs.train"])
    power_score_rcs.per <- sum(results()$my.res[,"score.p.value.rcs.per.train"] < 0.05) / length(results()$my.res[,"score.p.value.rcs.per.train"])
    power_score_cs.per <- sum(results()$my.res[,"score.p.value.cs.per.train"] < 0.05) / length(results()$my.res[,"score.p.value.cs.per.train"])
    
    power_dt <- data.frame(
      rcs = c(power_lrt_rcs, power_score_rcs),
      rcs.per = c(power_lrt_rcs.per, power_score_rcs.per),
      cs.per = c(power_lrt_cs.per, power_score_cs.per),
      row.names = c("LRT P (<0.05)", "Score P (<0.05)")
    )
    
    return(power_dt)
  })
  
  output$plot_simulations_estimates <- renderPlot({
    
    # prepare parameters
    par(mfrow=c(2,3))
    set.seed(results()$set_seed)
    sample_size <- 30
    which.use <- sample(results()$B, sample_size)
    x.test.points <- results()$x.test.points
    lp.coverage.rcs.train <- results()$lp.coverage.rcs.train
    lp.coverage.rcs.per.train <- results()$lp.coverage.rcs.per.train
    lp.coverage.cs.per.train <- results()$lp.coverage.cs.per.train
    num.test.points <- results()$num.test.points
    x.transf.2.lp <- results()$x.transf.2.lp
    x <- results()$x
    
    # multiple estimated curves
    # RCS 
    matplot(x.test.points, 
            t(lp.coverage.rcs.train[which.use, (1+2*num.test.points):(3*num.test.points)]),
            type="l", main="RCS", ylim=c(-4,4), col="gray", xlab="x", ylab="Estimated LP",
            cex.main=1, cex.lab=1, cex.axes=1)
    lines(x[order(x)], x.transf.2.lp[order(x)], lwd=4)
    
    # RCS.PER
    matplot(x.test.points,
            t(lp.coverage.rcs.per.train[which.use, (1+2*num.test.points):(3*num.test.points)]),
            type="l", main="RCS Periodic", ylim=c(-4,4), col="gray", xlab="x", ylab="Estimated LP",
            cex.main=1, cex.lab=1, cex.axes=1)
    lines(x[order(x)], x.transf.2.lp[order(x)], lwd=4)
    
    # CS.PER
    matplot(x.test.points,
            t(lp.coverage.cs.per.train[which.use, (1+2*num.test.points):(3*num.test.points)]),
            type="l", main="CS Periodic", ylim=c(-4,4), col="gray", xlab="x", ylab="Estimated LP",
            cex.main=1, cex.lab=1, cex.axes=1)
    lines(x[order(x)], x.transf.2.lp[order(x)], lwd=4)
    
    
    # average estimated curve
    # RCS
    my.mat=apply(lp.coverage.rcs.train[, -c(1:num.test.points)], 2, mean)
    plot(x.test.points, my.mat[1:num.test.points], type="l", ylim=c(-4,4), lty=2, main="RCS", ylab="Estimated LP", xlab="x")
    lines(x.test.points,my.mat[(2*num.test.points+1):(3*num.test.points)], lty=2)
    
    lines(x[order(x)], x.transf.2.lp[order(x)], lwd=4)
    lines(x.test.points, my.mat[(num.test.points+1):(2*num.test.points)], type="l", lwd=4, col="gray")
    
    # RCS.PER
    my.mat=apply(lp.coverage.rcs.per.train[, -c(1:num.test.points)], 2, mean)
    plot(x.test.points, my.mat[1:num.test.points], type="l", ylim=c(-4,4),  lty=2, main="RCS periodic", ylab="Estimated LP", xlab="x")
    lines(x.test.points,my.mat[(2*num.test.points+1):(3*num.test.points)],  lty=2)
    
    lines(x[order(x)], x.transf.2.lp[order(x)], lwd=4)
    lines(x.test.points, my.mat[(num.test.points+1):(2*num.test.points)], type="l", lwd=4, col="gray")
    
    # CS.PER
    my.mat=apply(lp.coverage.cs.per.train[, -c(1:num.test.points)], 2, mean)
    plot(x.test.points, my.mat[1:num.test.points], type="l", ylim=c(-4,4), lty=2, main="CS periodic", ylab="Estimated LP", xlab="x")
    lines(x.test.points,my.mat[(2*num.test.points+1):(3*num.test.points)], lty=2)
    
    lines(x[order(x)], x.transf.2.lp[order(x)], lwd=4)
    lines(x.test.points, my.mat[(num.test.points+1):(2*num.test.points)], type="l", lwd=4, col="gray")
  })
  
  output$plot_coverage <- renderPlot({
    # establish parameters
    # browser()
    my.res <- results()
    # x.test.points <- results()$x.test.points
    # lp.coverage.rcs.train <- results()$lp.coverage.rcs.train
    # lp.coverage.rcs.per.train <- results()$lp.coverage.rcs.per.train
    # lp.coverage.cs.per.train <- results()$lp.coverage.cs.per.train
    # num.test.points <- results()$num.test.points
    # 
    par(mfrow=c(1,3))
    
    # RCS
    plot(my.res$x.test.points,
         apply(my.res$lp.coverage.rcs.train[,1:my.res$num.test.points], 2, mean) ,
         ylim=c(0.80, 1), xlab="x", ylab="Coverage", main="RCS", cex.main=2, cex.lab=1.5, cex.axix=2)
    
    abline(v=my.res$knots, lty=2)
    abline(h=0.95, lty=2)
    
    # RCS PER
    plot(my.res$x.test.points,
         apply(my.res$lp.coverage.rcs.per.train[,1:my.res$num.test.points], 2, mean),
         ylim=c(0.80, 1), xlab="x", ylab="Coverage", main="RCS Periodic", cex.main=2, cex.lab=1.5, cex.axix=2)
    
    abline(v=my.res$knots, lty=2)
    abline(h=0.95, lty=2)
    
    # CS.PER
    plot(my.res$x.test.points,
         apply(my.res$lp.coverage.cs.per.train[,1:my.res$num.test.points], 2, mean),
         ylim=c(0.80, 1), xlab="x", ylab="Coverage", main="CS Periodic", cex.main=2, cex.lab=1.5, cex.axix=2)
    
    abline(v=my.res$knots.cs, lty=2)
    abline(h=0.95, lty=2)
  })
  
})

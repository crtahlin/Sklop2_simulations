#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(peRiodic)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("simulation results viewer"),
  
  sidebarPanel(
    uiOutput("select_folder"),
    uiOutput("select_simluation"),
    uiOutput("select_B_variants"),
    uiOutput("select_knots_variants")
  ),
  
  mainPanel(
    DT::dataTableOutput("results_index"),
    #  textOutput("results_hashes"),
    plotOutput("plot_probability"),
    plotOutput("plot_simulations_estimates"),
    plotOutput("plot_coverage"),
    DT::dataTableOutput("averages"),
    DT::dataTableOutput("power_stats")
  )
  
  
))

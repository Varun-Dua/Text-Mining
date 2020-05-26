#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI 
shinyUI(fluidPage(

    # Application title
    titlePanel("Text Mining"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h3("Choice of plot:"),
            radioButtons("radio1", h3("Plots:"),
                         choices = list("Word Cloud - Most Common" = 1, "Word Cloud - Positive & Negative" = 2,
                                        "Frequency Plot" = 3, "Lexical Diversity Plot" = 4, 
                                        "Dendogram" = 5),selected = 1),
            sliderInput("slider1", "Number of words/sentences", 0, 100, 0),
            radioButtons("radio2", h3("Confusion Matrix:"),
                         choices = list("Sentiment Comparison based on Lexical Dictionary" = 1, 
                                        "naive Bayes Classification Model" = 2
                                        ),selected = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("Plot")),
                        tabPanel("Matrix Output", verbatimTextOutput("Confusion")))
        )
    )
))

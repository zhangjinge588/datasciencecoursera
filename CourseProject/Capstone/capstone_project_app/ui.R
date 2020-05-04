#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel(h1("Type your Sentence", align = "center")),

    # Sidebar to choose how many words to show.
    sidebarLayout(
        sidebarPanel(
            sliderInput("topN",
                        "Top N Hint:",
                        min = 1,
                        max = 100,
                        value = 10),
            sliderInput("discount",
                        "Discount",
                        min = 0,
                        max = 1,
                        value = 0.4)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textInput('textInput', "", value = "", width = NULL,
                      placeholder = NULL),
            actionButton("submit", "Go!"),
            tableOutput("value")
        )
    )
))

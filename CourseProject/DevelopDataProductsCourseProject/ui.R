#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

data("mtcars")

factor_columns = c("cyl", "vs", "am", "gear", "carb")
names(factor_columns) = c("Number of Cylinders",
                          "Engine (0 = V-shaped, 1 = straight)",
                          "Transmission (0 = automatic, 1 = manual)", 
                          "Number of Forward Gears", 
                          "Number of Carburetors")

for (c in factor_columns) {
    mtcars[, c] = as.factor(mtcars[,c])
}


lst <- names(mtcars)

names(lst) <- c("Miles/(US) gallon", "Number of Cylinders", 
                "Displacement (cu.in.)", "Gross Horsepower", 
                "Rear Axle Ratio", "Weight (1000 IBs)", 
                "1/4 Mile Time", "Engine (0 = V-shaped, 1 = straight)", "Transmission (0 = automatic, 1 = manual)", 
                "Number of Forward Gears", "Number of Carburetors")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel(h1("Explore Dataset 'mtcars'", align='center')),

    sidebarPanel(
        selectInput(
            "select",
            label = h3("Select the Variable to plot against Miles / (US) Gallon"),
            choices = names(lst[-c(1)])
        ),
        selectInput(
            "select1",
            label = h3("Choose the Variable for additional Color Coding"),
            choices = names(factor_columns)
        )
    ),
    

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("myPlot")
        )
    )
)

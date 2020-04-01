#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(ggplot2)

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


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    

    output$myPlot <- renderPlot({
        
        input_column <- lst[input$select]
        
        color_coding_column <- factor_columns[input$select1]

        # plot(mtcars[,input_column], mtcars$mpg, xlab=input$select, ylab=names(lst)[1])
        
        my_plot <- ggplot(mtcars, aes_string(x=input_column, y="mpg")) +
            xlab(input$select) +
            ylab(names(lst)[1])
        
        if (class(mtcars[, input_column]) == 'factor') {
            my_plot = my_plot + geom_boxplot(aes_string(color=color_coding_column))
        } else {
            my_plot = my_plot + geom_point(aes_string(color=color_coding_column))
        }
        
        my_plot  +
            labs(color=input$select1)

    })

})

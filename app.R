#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library("shiny")
library("ggplot2")
library("quantmod")
library("scales")
library("dplyr")
library("tidyr")
library("tibble")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Stock Market Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          textInput("date", "starting Date: ", value = "2022-01-03"),
          dateInput("dateBreaks", "Date breaks: ",
          selectInput("dateBreaks", "Date breaks: ",
                      choices = c("1 day", "1 week",
                                  '1 month, "1 year")),
          actionButton("button", "Compute", class = "btn-block")
        ),
        
        # Show a plot
                                
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

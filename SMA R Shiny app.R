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
library("TTR")


# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Stock Analysis"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      textInput("ticker", "Ticker: "),
      dateInput("date", "Starting date: ", value = "2022-01-03"),
      selectInput("dateBreaks", "Date breaks: ",
                  choices = c("1 day", "1 week",
                              "1 month", "1 year")),
      selectInput("ma", "Moving average: ",
                  choices = c("None", "SMA", "EMA", "WMA", "EVWMA",
                              "VWAP", "BB")),
      numericInput("window", "window: ", value = 1),
      actionButton("button", "Compute", class = "btn-block")
    ),
    
    # Show plot and data
    mainPanel(
      fluidRow(
        column(12,
               plotOutput("candlePlot", brush = "plot_brush"))
      ),
      fluidRow(
        column(12,
               tableOutput("data"))
      )
      
    )
  )
)

# Define server logic 
server <- function(input, output, session) {
  
  
  stock <- eventReactive(input$button, {
    req(input$ticker, input$date, input$ma, input$window)
    
    getSymbols(input$ticker, 
               from = input$date,
               auto.assign = FALSE) %>%
      na.locf() %>%
      as.data.frame() %>%
      `colnames<-`(c("open", "high", "low", "close", "volume", "adjusted")) %>%
      rownames_to_column(var = "date") %>%
      mutate(date = as.Date(date)) %>% 
      mutate(greenRed=ifelse(open-close>0,
                             "Red",
                             "Green")) %>%
      mutate(TP = (high + low + close)/3) %>% # Typical Price
      mutate(movAvg = case_when( # new code to compute MA
        input$ma == "SMA" ~ SMA(close, input$window),
        input$ma == "EMA" ~ EMA(close, input$window),
        input$ma == "WMA" ~ WMA(close, input$window),
        input$ma == "EVWMA" ~ EVWMA(close, volume, input$window),
        input$ma == "VWAP" ~ VWAP(close, volume, input$window)
      )) %>%
      mutate(BOLU = case_when(
        # BOLU=Upper Bollinger Band
        # BOLD=Lower Bollinger Band
        input$ma == "BB" ~ SMA(TP, input$window) +
          2*rollapply(TP, input$window, sd, align = "right")),
        BOLD = case_when(
        input$ma == "BB" ~ SMA(TP, input$window) - 
          2*rollapply(TP, input$window, sd, align = "right"))
      ) 
    
  }
  )
  
  
  cp <- eventReactive(input$button, {
    req(input$ticker, input$date, input$dateBreaks)
    
    # Code for candlestick plot with ggplot2 from:        
    # https://www.r-bloggers.com/2021/09/robservations-12-making-a-candlestick-plot-with-the-ggplot2-and-tidyquant-packages/        
    
    ggplot(stock()) +
      geom_segment(aes(x = date,
                       xend = date,
                       y = open,
                       yend = close,
                       colour = greenRed),
                   linewidth = 3) +
      geom_segment(aes(x = date,
                       xend = date,
                       y = high,
                       yend = low,
                       colour = greenRed)) +
      geom_col(aes(x = date,
                   y = (((max(close) - min(close)) * ((volume - min(volume)) / (max(volume) - min(volume)))) + min(close))),
               # To constrain volume to be in the range of close: (((max(new) - min(new)) * ((x - min(old)) / (max(old) - min(old)))) + min(new))),
               fill = "red", alpha = 0.1) +
      theme_bw() +
      scale_color_manual(values = c("Forest Green","Red")) +
      labs(title = paste0(input$ticker, ": from ", stock()$date[1], 
                          " to ", stock()$date[nrow(stock())]),
           subtitle  =  paste0("Close price: $", round(stock()$close[nrow(stock())], 2),
                               "\n",
                               "Close date: ", stock()$date[nrow(stock())])) +
      theme(legend.position ="none",
            axis.title.y = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            plot.title= element_text(hjust=0.5)) +
      scale_y_continuous(labels = scales::label_dollar(),
                         position = "right") +
      scale_x_date(date_breaks = input$dateBreaks, date_labels = "%Y-%m-%d") +
      coord_cartesian(ylim = c(min(stock()$close)*0.9, max(stock()$close))*1.05) +
      geom_line(aes(x = date, y = movAvg)) + # new code to plot the MA
      geom_line(aes(x = date, y = BOLU), colour = "blue", linewidth = 1) +
      geom_line(aes(x = date, y = BOLD), colour = "blue", linewidth = 1) +
      geom_line(aes(x = date, y = SMA(TP, input$window)),
                linetype = "dashed", linewidth = 1)
  })
  
  
  output$candlePlot <- renderPlot({
    cp()
  }, res = 96)
  
  
  output$data <- renderTable({
    brushedPoints(stock(), input$plot_brush)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
### <-- packages --> ###
library(shiny)
library(tidyverse)
library(lubridate)
library(dplyr)
library(modelr)
library(scales)
library(readxl)
library(quantmod)
library(tidyquant)
library(corrplot)
library(ggplot2)
library(shinyWidgets)
library(plotly)
### <--------------> ###

################################################################################

### <-- Required Data --> ###
stocks <- c("BWX","DBC","DXY","IEF","IYR")
benchmarks <- c("^NDX","^GSPC")
econ_rate <- c("UNRATE", "CPIAUCSL", "DFF")

#Get asset's price 
prices <- tq_get(stocks, 
                 get  = "stock.prices",
                 from = '2007-10-31',
                 to   = '2022-10-31',
                 complete_cases = F) %>%
  select(symbol,date,close)

#Get benchmarks' prices
bench <- tq_get(benchmarks,
                get  = "stock.prices",
                from = '2007-10-31',
                to   = '2022-10-31') %>%
  select(symbol,date,close)

# Get Macro Data
rate <- tq_get(econ_rate,
               get  = "economic.data",
               from = '2007-10-31',
               to   = '2022-10-31') %>%
  select(symbol,date,price) 

# Combine Macro Data and Asset's Price 
colnames(rate) <- c('symbol','date','close')
prices =  bind_rows(prices, rate)
stocks <- c("BWX","DBC","DXY","IEF","IYR","UNRATE", "CPIAUCSL", "DFF")


### <-- APP --> ###
ui <- fluidPage(
  titlePanel("Asset Classes Price/ S&P 500/ Macro Econ Index Analysis"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 pickerInput(
                   inputId = "stocks",
                   label = h4("Factors"),
                   choices = c(
                     "BWX" = stocks[1], 
                     "DBC" = stocks[2],
                     "DXY" = stocks[3],
                     "IEF" = stocks[4],
                     "IYR" = stocks[5],
                     "Unemployment Rate" = stocks[6],
                     "Consumer Price Index" = stocks[7],
                     "Interest Rate" = stocks[8]
                   ),
                   selected = stocks[1],   
                   options = list(`actions-box` = TRUE), 
                   multiple = T #Select one or multiple factors
                 ),
                 radioButtons("period", label = h4("Period"),
                              choices = list("Financial Crisis" = 1, "Covid-19" = 2, "Middle Time" = 3, "Entire Period" = 4), 
                              selected = 1 #Select period
                 ),
                 radioButtons("benchmark", label = h4("Benchmark"),
                              choices = list("Nasdaq100" = 1, "S&P500" = 2,"None" = 3),
                              selected = 3) #Select benchmark
    ),
    mainPanel(
      plotlyOutput("plot",height=500),
      dataTableOutput("dt")
    )
    
  )
)

server <- function(input, output) {
  observeEvent(c(input$period,input$stocks,input$benchmark), {
    prices <- prices %>%
      filter(symbol %in% input$stocks)
    
    if (input$period == 1) {
      prices <- prices %>%
        filter(date < '2010-10-30' & date > '2008-02-29') }
    
    if (input$period == 2) {
      prices <- prices %>%
        filter(date < '2022-10-31' & date > '2020-02-28') }
    
    if (input$period == 3) {
      prices <- prices %>%
        filter(date < '2020-02-28' & date > '2010-10-30')}
    
    #if (input$period == 5) {
      #prices <- prices %>%
        #filter(year(date) == year(today())) }
    
    if (input$benchmark == 1) {
      bench <- bench %>%
        filter(symbol=="^NDX", (date >= min(prices$date)& date<= max(prices$date)))
      prices <- rbind(prices,bench) }
    
    if (input$benchmark == 2) {
      bench <- bench %>%
        filter(symbol=="^GSPC", (date >= min(prices$date)& date<= max(prices$date)))
      prices <- rbind(prices, bench) }
    
    output$plot <- renderPlotly({
      print(
        ggplotly(prices %>%
                   group_by(symbol) %>%
                   mutate(init_close = if_else(date == min(date),close,NA_real_)) %>%
                   #Get the initial value 
                   mutate(value = round(100 * close / sum(init_close,na.rm=T),1)) %>%
                   #Let the value in others' time to compare with initial value
                   ungroup() %>%
                   ggplot() +
                   geom_line(aes(date, value,colour = symbol)) +
                   theme_bw() +
                   labs(x = "Date", y = "Compare Its Current Value with Its Initial Value") +
                   theme_minimal()
        ) 
        
      )
    })
    output$dt <- renderDataTable({
      prices
    })
    
  })
}

app <- shinyApp(ui, server)
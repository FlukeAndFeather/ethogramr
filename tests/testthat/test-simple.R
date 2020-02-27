# Test manually

library(shiny)
library(tidyverse)

ui <- fillPage(
  eth_ui("ethogram", c("15%", "15%", "70%"))
)

server <- function(input, output, session) {
  df <- tibble(
    x = 1:1000,
    y = cumsum(rnorm(1000))
  )
  ethogram <- callModule(
    ethogram,
    "ethogram",
    data = df
  )
}

shinyApp(ui, server)

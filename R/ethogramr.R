#' @import dplyr, shiny

eth_ui <- function(id, heights) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    plotOutput(
      ns("full_plot"),
      brush = brushOpts(id = ns("full_brush"), direction = "x"),
      height = heights[1]
    ),
    plotOutput(
      ns("zoom_plot"),
      click = ns("zoom_click"),
      height = heights[2]
    ),
    plotOutput(
      ns("main_plot"),
      click = ns("main_click"),
      brush = brushOpts(id = ns("main_brush"), direction = "x"),
      height = heights[3]
    )
  )
}

ethogram <- function(input, output, session, data, spec = NULL) {
  # Maximum number of points in any plot
  max_rows <- 1e4

  # Data for full profile
  full_data <- reactive({
    if (nrow(data) > max_rows) {
      slice(data, seq(1, nrow(data), length.out = max_rows))
    } else {
      data
    }
  })

  # Data for zoomed-in profile
  zoom_data <- reactive({
    full_brush <- input$full_brush
    if (is.null(full_brush)) {
      data_sub <- slice(data, 0)
    } else {
      x_rng <- range(brushedPoints(full_data(), full_brush)$x)
      data_sub <- filter(data, between(x, x_rng[1], x_rng[2]))
    }
    limit_rows(data_sub, max_rows)
  })

  # Data for main plot
  main_data <- reactive({
    zoom_click <- input$zoom_click
    if (is.null(zoom_click)) {
      data_sub <- slice(data, 0)
    } else {
      buffer <- 20
      x_rng <- zoom_click$x + c(-buffer, buffer)
      data_sub <- filter(data, between(x, x_rng[1], x_rng[2]))
    }
    limit_rows(data_sub, max_rows)
  })

  # Render plots
  output$full_plot <- renderPlot(
    ggplot(full_data(), aes(x, y)) +
      geom_line() +
      theme_minimal()
  )
  output$zoom_plot <- renderPlot(
    ggplot(zoom_data(), aes(x, y)) +
      geom_line() +
      theme_minimal()
  )
  output$main_plot <- renderPlot(
    ggplot(main_data(), aes(x, y)) +
      geom_line() +
      theme_minimal()
  )
}

limit_rows <- function(df, max_rows) {
  if (nrow(df) > max_rows) {
    slice(df, seq(1, nrow(df), length.out = max_rows))
  } else {
    df
  }
}

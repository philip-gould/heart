library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
#library(DT) # recommended use is DT::fun() structure to reduce time

heart <- readRDS("data/heart.rds")

ui <- page_sidebar(
  title = tags$span(
    tags$img(src = "logo.png", height = "30px", style = "margin-right: 10px; filter: invert(1);"),
    "Heart Attack Dashboard"
  ),
  theme = bs_theme(bootswatch = "pulse"),
  sidebar = sidebar(
    selectInput(
      inputId = "outcome",
      label = "Outcome:",
      choices = c("All", "Survived", "Died")
    ),
    selectInput(
      inputId = "diagnosis",
      label = "Diagnosis:",
      choices = c("All", sort(unique(as.character(heart$DIAGNOSIS)))),
      selected = "All"
    ),
    selectInput(
      inputId = "drg",
      label = "DRG:",
      choices = c("All", sort(unique(as.character(heart$DRG)))),
      selected = "All"
    ),
    sliderInput(
      inputId = "age_range",
      label = "Age Range:",
      min = min(heart$AGE),
      max = max(heart$AGE),
      value = c(min(heart$AGE), max(heart$AGE))
    ),
    actionButton(
      inputId = "reset",
      label = "Reset",
      icon = bsicons::bs_icon("arrow-counterclockwise")
    )
    
  ),
  navset_tab(
    nav_panel(
      "Overview", 
      layout_column_wrap(
        width = 1/2,
        value_box(
          title = "Female Mortality",
          value = textOutput("f_mortality"),
          theme = "danger",
          showcase = bsicons::bs_icon("gender-female")
        ),
        value_box(
          title = "Male Mortality",
          value = textOutput("m_mortality"),
          theme = "primary",
          showcase = bsicons::bs_icon("gender-male")
        ),
        value_box(
          title = "All Mortality",
          value = textOutput("a_mortality"),
          theme = "primary",
          showcase = bsicons::bs_icon("gender-trans")
        ),
        card(
          card_header("Age Distribution"),
          plotOutput("age_hist")
        )
      )
    ),
    nav_panel(
      "Explore", 
      plotlyOutput("scatter_plot")
    ),
    nav_panel(
      "Data", 
      DT::dataTableOutput("data_table")
    )
  )
)

server <- function(input, output, session) {
  # Filter logic, Best Practice: Locate reactives at beginning of server
  filtered_data <- reactive({
    d <- heart
    if (input$outcome != "All") {
      d <- d[d$DIED == input$outcome, ]
    }
    if (input$diagnosis != "All") {
      d <- d[as.character(d$DIAGNOSIS) == input$diagnosis, ]
    }
    if (input$drg != "All") {
      d <- d[as.character(d$DRG) == input$drg, ]
    }
    d <- d[d$AGE >= input$age_range[1] & d$AGE <= input$age_range[2], ]
    d
  })
  
  # Data table
  output$data_table <- DT::renderDataTable({
    filtered_data()
  })

  # Female stats
  output$f_mortality <- renderText({
    d <- filtered_data()[filtered_data()$SEX == "Female", ]
    paste0(round(100 * sum(d$DIED == "Died") / nrow(d), 1), "%")
  })
  
  # Male stats
  output$m_mortality <- renderText({
    d <- filtered_data()[filtered_data()$SEX == "Male", ]
    paste0(round(100 * sum(d$DIED == "Died") / nrow(d), 1), "%")
  })
  # All stats
  output$a_mortality <- renderText({
    d <- filtered_data()
    paste0(round(100 * sum(d$DIED == "Died") / nrow(d), 1), "%")
  })
  # Age Histogram
  output$age_hist <- renderPlot({
    req(nrow(filtered_data()) >= 2)
    ggplot(filtered_data(), aes(x = AGE, fill = SEX)) +
      geom_density(alpha = 0.5) +
      labs(x = "Age", y = "Density", fill = "SEX") +
      #facet_wrap(~ SEX) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
  })
  # Plotly interactive
  output$scatter_plot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) >= 1)
    if(nrow(df) > 1000) {
      df <- df[sample(nrow(df), 1000), ]
    }
    p <- ggplot(df, aes(x = AGE, y = LOS, color = SEX)) +
      geom_point(alpha = 0.3) +
      labs(x = "Age", y = "Length of Stay (days)", color = "Sex") +
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal()
    ggplotly(p)
  })
  # Reset observation
  observeEvent(input$reset, {
    updateSelectInput(session, "outcome", selected = "All")
    updateSelectInput(session, "diagnosis", selected = "All")
    updateSelectInput(session, "drg", selected = "All")
    updateSliderInput(session, "age_range",
                      value = c(min(heart$AGE), max(heart$AGE)))
  })
  
}

shinyApp(ui = ui, server = server)

# ==============================
# Engagement Analysis Page
# ==============================

engagement_ui <- function() {
  
  tagList(
    
    div(
      class = "page-header",
      h2("Engagement Analysis"),
      p("Explore how content, timing and traffic sources influence engagement.")
    ),
    
    # ------------------------------
    # Filters
    # ------------------------------
    
    div(
      class = "filter-card",
      
      fluidRow(
        
        column(
          width = 4,
          selectInput(
            "engagement_category",
            "Content Category",
            choices = c("All", sort(unique(data$content_category))),
            selected = "All"
          )
        ),
        
        column(
          width = 4,
          selectInput(
            "engagement_media",
            "Media Type",
            choices = c("All", sort(unique(data$media_type))),
            selected = "All"
          )
        ),
        
        column(
          width = 4,
          selectInput(
            "engagement_day",
            "Day of Week",
            choices = c(
              "All", "Monday", "Tuesday", "Wednesday",
              "Thursday", "Friday", "Saturday", "Sunday"
            ),
            selected = "All"
          )
        )
      )
    ),
    
    br(),
    
    # ------------------------------
    # Category + Day
    # ------------------------------
    
    fluidRow(
      
      column(
        width = 6,
        div(
          class = "chart-card",
          h4("Engagement by Content Category"),
          plotlyOutput("engagement_category_plot", height = "320px")
        )
      ),
      
      column(
        width = 6,
        div(
          class = "chart-card",
          h4("Engagement by Day of Week"),
          plotlyOutput("engagement_day_plot", height = "320px")
        )
      )
    ),
    
    br(),
    
    # ------------------------------
    # Hour + Traffic Source
    # ------------------------------
    
    fluidRow(
      
      column(
        width = 6,
        div(
          class = "chart-card",
          h4("Engagement by Posting Hour"),
          plotlyOutput("engagement_hour_plot", height = "320px")
        )
      ),
      
      column(
        width = 6,
        div(
          class = "chart-card",
          h4("Average Engagement by Traffic Source"),
          plotlyOutput("traffic_source_plot", height = "320px")
        )
      )
    )
  )
}


# ==============================
# Engagement Server
# ==============================

engagement_server <- function(input, output, session) {
  
  # ------------------------------
  # Filtered Data
  # ------------------------------
  
  filtered_engagement_data <- reactive({
    
    filtered <- data
    
    if (input$engagement_category != "All") {
      filtered <- filtered[
        filtered$content_category == input$engagement_category,
      ]
    }
    
    if (input$engagement_media != "All") {
      filtered <- filtered[
        filtered$media_type == input$engagement_media,
      ]
    }
    
    if (input$engagement_day != "All") {
      filtered <- filtered[
        filtered$day_of_week == input$engagement_day,
      ]
    }
    
    filtered
  })
  
  
  # ------------------------------
  # Engagement by Content Category
  # ------------------------------
  
  output$engagement_category_plot <- renderPlotly({
    
    filtered <- filtered_engagement_data()
    
    category_data <- filtered %>%
      group_by(content_category) %>%
      summarise(
        total_engagement = sum(total_engagement, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_engagement))
    
    pretty_vbar(
      category_data,
      cat    = "content_category",
      val    = "total_engagement",
      ylab   = "Total Engagement",
      hover  = "<b>%{x}</b><br>Engagement: %{y:,}<extra></extra>",
      labels = fmt_short
    )
  })
  
  
  # ------------------------------
  # Engagement by Day
  # ------------------------------
  
  output$engagement_day_plot <- renderPlotly({
    
    filtered <- filtered_engagement_data()
    
    day_data <- filtered %>%
      group_by(day_of_week) %>%
      summarise(
        total_engagement = sum(total_engagement, na.rm = TRUE),
        .groups = "drop"
      )
    
    day_order <- c(
      "Monday", "Tuesday", "Wednesday", "Thursday",
      "Friday", "Saturday", "Sunday"
    )
    
    day_data$day_of_week <- factor(day_data$day_of_week, levels = day_order)
    
    day_data <- day_data %>%
      arrange(day_of_week)
    
    pretty_line(
      day_data,
      x     = "day_of_week",
      y     = "total_engagement",
      xlab  = "Day of Week",
      ylab  = "Total Engagement",
      hover = "<b>%{x}</b><br>Engagement: %{y:,}<extra></extra>"
    )
  })
  
  
  # ------------------------------
  # Engagement by Posting Hour
  # ------------------------------
  
  output$engagement_hour_plot <- renderPlotly({
    
    filtered <- filtered_engagement_data()
    
    hour_data <- filtered %>%
      group_by(post_hour) %>%
      summarise(
        total_engagement = sum(total_engagement, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(post_hour)
    
    pretty_line(
      hour_data,
      x     = "post_hour",
      y     = "total_engagement",
      xlab  = "Posting Hour",
      ylab  = "Total Engagement",
      hover = "<b>Hour %{x}</b><br>Engagement: %{y:,}<extra></extra>",
      dtick = 1
    )
  })
  
  
  # ------------------------------
  # Average Engagement by Traffic Source
  # ------------------------------
  
  output$traffic_source_plot <- renderPlotly({
    
    filtered <- filtered_engagement_data()
    
    traffic_data <- filtered %>%
      group_by(traffic_source) %>%
      summarise(
        avg_engagement = mean(total_engagement, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(avg_engagement)
    
    pretty_hbar(
      traffic_data,
      cat    = "traffic_source",
      val    = "avg_engagement",
      xlab   = "Average Engagement",
      hover  = "<b>%{y}</b><br>Average Engagement: %{x:.2f}<extra></extra>",
      labels = fmt_1dp
    )
  })
}
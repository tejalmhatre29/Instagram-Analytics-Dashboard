# ==============================
# Data Explorer Page
# ==============================

explorer_ui <- function() {
  
  tagList(
    
    div(
      class = "page-header",
      h2("Data Explorer"),
      p("Explore and filter individual Instagram posts from the dataset.")
    ),
    
    # ------------------------------
    # Filters
    # ------------------------------
    
    div(
      class = "filter-card",
      
      fluidRow(
        
        column(
          width = 3,
          
          selectInput(
            "explorer_category",
            "Content Category",
            choices = c(
              "All",
              sort(unique(data$content_category))
            ),
            selected = "All"
          )
        ),
        
        column(
          width = 3,
          
          selectInput(
            "explorer_media",
            "Media Type",
            choices = c(
              "All",
              sort(unique(data$media_type))
            ),
            selected = "All"
          )
        ),
        
        column(
          width = 3,
          
          selectInput(
            "explorer_account",
            "Account Type",
            choices = c(
              "All",
              sort(unique(data$account_type))
            ),
            selected = "All"
          )
        ),
        
        column(
          width = 3,
          
          selectInput(
            "explorer_traffic",
            "Traffic Source",
            choices = c(
              "All",
              sort(unique(data$traffic_source))
            ),
            selected = "All"
          )
        )
      )
    ),
    
    br(),
    
    # ------------------------------
    # Data Table
    # ------------------------------
    
    div(
      class = "chart-card",
      
      h4("Instagram Posts"),
      
      DTOutput("instagram_data_table")
    )
  )
}


# ==============================
# Data Explorer Server
# ==============================

explorer_server <- function(input, output, session) {
  
  # ------------------------------
  # Filtered Data
  # ------------------------------
  
  filtered_explorer_data <- reactive({
    
    filtered <- data
    
    # Content category
    if (input$explorer_category != "All") {
      
      filtered <- filtered[
        filtered$content_category ==
          input$explorer_category,
      ]
    }
    
    # Media type
    if (input$explorer_media != "All") {
      
      filtered <- filtered[
        filtered$media_type ==
          input$explorer_media,
      ]
    }
    
    # Account type
    if (input$explorer_account != "All") {
      
      filtered <- filtered[
        filtered$account_type ==
          input$explorer_account,
      ]
    }
    
    # Traffic source
    if (input$explorer_traffic != "All") {
      
      filtered <- filtered[
        filtered$traffic_source ==
          input$explorer_traffic,
      ]
    }
    
    filtered
  })
  
  
  # ------------------------------
  # Data Table
  # ------------------------------
  
  output$instagram_data_table <- renderDT({
    
    filtered <- filtered_explorer_data()
    
    # Select useful columns for the explorer
    table_data <- filtered %>%
      select(
        post_id,
        account_type,
        follower_count,
        media_type,
        content_category,
        traffic_source,
        post_hour,
        day_of_week,
        likes,
        comments,
        shares,
        saves,
        reach,
        impressions,
        engagement_rate,
        followers_gained,
        hashtags_count,
        total_engagement
      )
    
    datatable(
      table_data,
      
      rownames = FALSE,
      
      filter = "top",
      
      extensions = "Buttons",
      
      options = list(
        pageLength = 10,
        
        lengthMenu = c(
          10,
          25,
          50,
          100
        ),
        
        scrollX = TRUE,
        
        dom = "Bfrtip",
        
        buttons = c(
          "copy",
          "csv",
          "excel"
        )
      )
    )
  })
}
# ==============================
# Audience & Content Page
# ==============================

audience_brand <- "#6C63FF"

# ---- Look + interaction helpers (no data changes) ----

aud_style <- function(p) {
  p %>%
    layout(
      font          = list(family = "Inter, Segoe UI, sans-serif", color = "#475569"),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      hovermode     = "closest",
      hoverlabel    = list(bgcolor = "#0f172a",
                           font = list(color = "white", family = "Inter"),
                           bordercolor = "rgba(0,0,0,0)")
    ) %>%
    config(
      displaylogo = FALSE,
      displayModeBar = "hover",
      scrollZoom = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "toggleSpikelines")
    )
}

# Bars: hovered bar stays bright, the rest dim
aud_fx_bar <- function(p) {
  p %>%
    htmlwidgets::onRender("
      function(el) {
        el.on('plotly_hover', function(d) {
          var n = d.points[0].data.y.length, p = d.points[0].pointNumber;
          var op = Array.from({length: n}, function(_, i){ return i === p ? 1 : 0.4; });
          Plotly.restyle(el, {'marker.opacity': [op]});
        });
        el.on('plotly_unhover', function() {
          Plotly.restyle(el, {'marker.opacity': 1});
        });
      }
    ")
}

# Lines: hovered point grows and follows the cursor, with a dotted spike line
aud_fx_line <- function(p) {
  p %>%
    layout(xaxis = list(showspikes = TRUE, spikemode = "across",
                        spikecolor = audience_brand, spikethickness = 1,
                        spikedash = "dot")) %>%
    htmlwidgets::onRender("
      function(el) {
        var n0 = el.data[0].y.length;
        Plotly.restyle(el, {'marker.size': [Array(n0).fill(6)]});
        el.on('plotly_hover', function(d) {
          var n = d.points[0].data.y.length, p = d.points[0].pointNumber;
          var sz = Array.from({length: n}, function(_, i){ return i === p ? 15 : 6; });
          Plotly.restyle(el, {'marker.size': [sz]});
        });
        el.on('plotly_unhover', function() {
          var n = el.data[0].y.length;
          Plotly.restyle(el, {'marker.size': [Array(n).fill(6)]});
        });
      }
    ")
}

# Cursor-following tilt only (no expand button on this page)
audience_assets <- tagList(
  tags$style(HTML("
    .chart-card { transition: transform .15s ease, box-shadow .2s ease;
                  transform-style: preserve-3d; will-change: transform; }
    .chart-card:hover { box-shadow: 0 18px 40px rgba(108,99,255,.18); }
  ")),
  tags$script(HTML("
    (function() {
      $(document).off('.tiltfx');
      $(document).on('mousemove.tiltfx', '.chart-card', function(e) {
        var r = this.getBoundingClientRect();
        var x = (e.clientX - r.left) / r.width  - 0.5;
        var y = (e.clientY - r.top)  / r.height - 0.5;
        this.style.transform = 'perspective(1000px) rotateY(' + (x*4) + 'deg) rotateX(' + (-y*4) + 'deg) translateY(-3px)';
      });
      $(document).on('mouseleave.tiltfx', '.chart-card', function() {
        this.style.transform = '';
      });
    })();
  "))
)


# ---- UI ----

audience_ui <- function() {
  
  tagList(
    
    audience_assets,
    
    div(
      class = "page-header",
      h2("Audience & Content"),
      p("Understand reach, follower growth and content characteristics.")
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
            "audience_category",
            "Content Category",
            choices = c("All", sort(unique(data$content_category))),
            selected = "All"
          )
        ),
        
        column(
          width = 4,
          selectInput(
            "audience_media",
            "Media Type",
            choices = c("All", sort(unique(data$media_type))),
            selected = "All"
          )
        ),
        
        column(
          width = 4,
          selectInput(
            "audience_account",
            "Account Type",
            choices = c("All", sort(unique(data$account_type))),
            selected = "All"
          )
        )
      )
    ),
    
    br(),
    
    # ------------------------------
    # Reach + Followers
    # ------------------------------
    
    fluidRow(
      
      column(
        width = 6,
        div(
          class = "chart-card",
          h4("Average Reach by Content Category"),
          plotlyOutput("audience_reach_plot", height = "320px")
        )
      ),
      
      column(
        width = 6,
        div(
          class = "chart-card",
          h4("Average Followers Gained"),
          plotlyOutput("audience_followers_plot", height = "320px")
        )
      )
    ),
    
    br(),
    
    # ------------------------------
    # Hashtags + Engagement Rate
    # ------------------------------
    
    fluidRow(
      
      column(
        width = 6,
        div(
          class = "chart-card",
          h4("Hashtags vs Engagement"),
          plotlyOutput("audience_hashtag_plot", height = "320px")
        )
      ),
      
      column(
        width = 6,
        div(
          class = "chart-card",
          h4("Content Category Performance"),
          plotlyOutput("audience_performance_plot", height = "320px")
        )
      )
    )
  )
}


# ==============================
# Audience Server
# ==============================

audience_server <- function(input, output, session) {
  
  # ------------------------------
  # Filtered Data
  # ------------------------------
  
  filtered_audience_data <- reactive({
    
    filtered <- data
    
    # Category filter
    if (input$audience_category != "All") {
      filtered <- filtered[
        filtered$content_category == input$audience_category,
      ]
    }
    
    # Media filter
    if (input$audience_media != "All") {
      filtered <- filtered[
        filtered$media_type == input$audience_media,
      ]
    }
    
    # Account type filter
    if (input$audience_account != "All") {
      filtered <- filtered[
        filtered$account_type == input$audience_account,
      ]
    }
    
    filtered
  })
  
  
  # ------------------------------
  # Average Reach
  # ------------------------------
  
  output$audience_reach_plot <- renderPlotly({
    
    filtered <- filtered_audience_data()
    
    reach_data <- filtered %>%
      group_by(content_category) %>%
      summarise(
        avg_reach = mean(reach, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(avg_reach)
    
    plot_ly(
      reach_data,
      x = ~avg_reach,
      y = ~reorder(content_category, avg_reach),
      type = "bar",
      orientation = "h",
      marker = list(
        color = ~avg_reach,
        colorscale = list(c(0, "#C7C3FF"), c(1, audience_brand)),
        line = list(width = 0)
      ),
      hovertemplate =
        "<b>%{y}</b><br>Average Reach: %{x:,.0f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Average Reach", gridcolor = "#eef2f7",
                     zeroline = FALSE),
        yaxis = list(title = "", showgrid = FALSE),   # "" stops plotly auto-naming the axis
        bargap = 0.3,
        margin = list(l = 100, r = 20, b = 50, t = 10)
      ) %>%
      aud_style() %>%
      aud_fx_bar()
  })
  
  
  # ------------------------------
  # Average Followers Gained
  # ------------------------------
  
  output$audience_followers_plot <- renderPlotly({
    
    filtered <- filtered_audience_data()
    
    followers_data <- filtered %>%
      group_by(content_category) %>%
      summarise(
        avg_followers = mean(followers_gained, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(avg_followers)
    
    plot_ly(
      followers_data,
      x = ~content_category,
      y = ~avg_followers,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = audience_brand, width = 3),
      marker = list(color = audience_brand, size = 6),
      hovertemplate =
        "<b>%{x}</b><br>Average Followers Gained: %{y:.1f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "", tickangle = -45, showgrid = FALSE,
                     linecolor = "#e2e8f0"),
        yaxis = list(title = "Average Followers Gained", gridcolor = "#eef2f7",
                     zeroline = FALSE),
        margin = list(l = 60, r = 20, b = 80, t = 10)
      ) %>%
      aud_style() %>%
      aud_fx_line()
  })
  
  
  # ------------------------------
  # Hashtag Count vs Engagement
  # ------------------------------
  
  output$audience_hashtag_plot <- renderPlotly({
    
    filtered <- filtered_audience_data()
    
    hashtag_data <- filtered %>%
      group_by(hashtags_count) %>%
      summarise(
        avg_engagement = mean(total_engagement, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(hashtags_count)
    
    plot_ly(
      hashtag_data,
      x = ~hashtags_count,
      y = ~avg_engagement,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#FF5C8A", width = 3),
      marker = list(color = "#FF5C8A", size = 6),
      hovertemplate =
        "<b>Hashtags: %{x}</b><br>Average Engagement: %{y:.2f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Number of Hashtags", dtick = 1, showgrid = FALSE,
                     linecolor = "#e2e8f0"),
        yaxis = list(title = "Average Engagement", gridcolor = "#eef2f7",
                     zeroline = FALSE),
        margin = list(l = 60, r = 20, b = 50, t = 10)
      ) %>%
      aud_style() %>%
      aud_fx_line()
  })
  
  
  # ------------------------------
  # Content Category Performance
  # ------------------------------
  
  output$audience_performance_plot <- renderPlotly({
    
    filtered <- filtered_audience_data()
    
    performance_data <- filtered %>%
      group_by(content_category) %>%
      summarise(
        avg_engagement_rate = mean(engagement_rate, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(avg_engagement_rate)
    
    plot_ly(
      performance_data,
      x = ~avg_engagement_rate,
      y = ~reorder(content_category, avg_engagement_rate),
      type = "bar",
      orientation = "h",
      marker = list(
        color = ~avg_engagement_rate,
        colorscale = list(c(0, "#FFD3E0"), c(1, "#FF5C8A")),
        line = list(width = 0)
      ),
      hovertemplate =
        "<b>%{y}</b><br>Average Engagement Rate: %{x:.2f}%<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Average Engagement Rate", gridcolor = "#eef2f7",
                     zeroline = FALSE),
        yaxis = list(title = "", showgrid = FALSE),   # "" stops plotly auto-naming the axis
        bargap = 0.3,
        margin = list(l = 100, r = 20, b = 50, t = 10)
      ) %>%
      aud_style() %>%
      aud_fx_bar()
  })
}
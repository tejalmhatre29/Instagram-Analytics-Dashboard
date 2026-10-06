# ==============================
# Overview Page
# ==============================

brand <- c("#6C63FF", "#FF5C8A", "#FFA630", "#2EC4B6", "#4D96FF")

# ---- Chart styling helper (look + interaction config, no data changes) ----
style_plotly <- function(p) {
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

# ---- KPI card helper ----
kpi_card <- function(num, icon, label, output_id) {
  div(
    class = paste("kpi-card", paste0("kpi-", num)),
    div(class = "kpi-icon", icon),
    div(class = "kpi-label", label),
    div(class = "kpi-value", textOutput(output_id, inline = TRUE))
  )
}

# ---- Chart card helper (title + expand button) ----
chart_card <- function(title, plot_output) {
  div(
    class = "chart-card",
    div(
      class = "chart-card-head",
      h4(title),
      tags$button(class = "expand-btn", title = "Expand", HTML("&#10530;"))
    ),
    plot_output
  )
}

# ---- CSS + JS for expand view and cursor tilt ----
interaction_assets <- tagList(
  tags$style(HTML("
    .chart-card { transition: transform .15s ease, box-shadow .2s ease;
                  transform-style: preserve-3d; will-change: transform; }
    .chart-card:hover { box-shadow: 0 18px 40px rgba(108,99,255,.18); }
    .chart-card-head { display:flex; justify-content:space-between; align-items:center; }
    .expand-btn { border:none; background:#f1f5f9; color:#475569; width:32px; height:32px;
                  border-radius:8px; cursor:pointer; font-size:16px; transition:all .2s; }
    .expand-btn:hover { background:#6C63FF; color:white; transform:scale(1.1); }

    .chart-card.expanded { position:fixed; inset:3vh 3vw; z-index:9999; transform:none !important;
                           box-shadow:0 0 0 100vmax rgba(15,23,42,.65); overflow:hidden; }
    .chart-card.expanded .html-widget { height: calc(94vh - 90px) !important; }
  ")),
  tags$script(HTML("
    // Expand / collapse chart
    $(document).on('click', '.expand-btn', function() {
      var card = $(this).closest('.chart-card');
      card.toggleClass('expanded').css('transform', '');
      $(this).html(card.hasClass('expanded') ? '\\u2715' : '\\u2922');
      setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 60);
    });
    $(document).on('keydown', function(e) {
      if (e.key === 'Escape') $('.chart-card.expanded .expand-btn').click();
    });

    // Cursor-following tilt
    $(document).on('mousemove', '.chart-card:not(.expanded)', function(e) {
      var r = this.getBoundingClientRect();
      var x = (e.clientX - r.left) / r.width  - 0.5;
      var y = (e.clientY - r.top)  / r.height - 0.5;
      this.style.transform = 'perspective(1000px) rotateY(' + (x*4) + 'deg) rotateX(' + (-y*4) + 'deg) translateY(-3px)';
    });
    $(document).on('mouseleave', '.chart-card', function() { this.style.transform = ''; });
  "))
)

# ---- UI ----
overview_ui <- function() {
  tagList(
    interaction_assets,
    
    div(
      class = "page-header",
      h2("Overview"),
      p("A high-level view of Instagram content performance and engagement.")
    ),
    
    div(
      class = "kpi-grid",
      kpi_card(1, "📝", "Total Posts",          "kpi_posts"),
      kpi_card(2, "❤️", "Total Engagement",     "kpi_engagement"),
      kpi_card(3, "👁️", "Average Reach",        "kpi_reach"),
      kpi_card(4, "👥", "Avg Followers Gained", "kpi_followers")
    ),
    
    div(
      class = "chart-grid",
      chart_card("Engagement by Content Category", plotlyOutput("category_plot", height = "380px")),
      chart_card("Engagement by Media Type",       plotlyOutput("media_plot",    height = "380px"))
    )
  )
}

# ---- Server ----
overview_server <- function(input, output, session) {
  
  # KPI values (calculated from `data` in data.R)
  output$kpi_posts <- renderText({
    format(nrow(data), big.mark = ",")
  })
  
  output$kpi_engagement <- renderText({
    format(sum(data$total_engagement), big.mark = ",")
  })
  
  output$kpi_reach <- renderText({
    format(round(mean(data$reach)), big.mark = ",")
  })
  
  output$kpi_followers <- renderText({
    format(round(mean(data$followers_gained), 1), nsmall = 1)
  })
  
  # ---- Engagement by Content Category ----
  output$category_plot <- renderPlotly({
    
    d <- data %>%
      group_by(content_category) %>%
      summarise(total_engagement = sum(total_engagement), .groups = "drop") %>%
      arrange(desc(total_engagement))
    
    d$content_category <- factor(d$content_category, levels = d$content_category)
    
    plot_ly(
      d,
      x = ~content_category,
      y = ~total_engagement,
      type = "bar",
      text = ~paste0(round(total_engagement / 1e6, 2), "M"),
      textposition = "outside",
      cliponaxis = FALSE,
      marker = list(
        color = ~total_engagement,
        colorscale = list(c(0, "#C7C3FF"), c(1, "#6C63FF")),
        line = list(width = 0)
      ),
      hovertemplate = "<b>%{x}</b><br>Engagement: %{y:,.0f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "", tickangle = -30, showgrid = FALSE,
                     linecolor = "#e2e8f0"),
        yaxis = list(title = "Total Engagement", gridcolor = "#eef2f7",
                     zeroline = FALSE, tickformat = ".1s"),
        bargap = 0.35,
        margin = list(t = 30, b = 70)
      ) %>%
      style_plotly() %>%
      layout(xaxis = list(showspikes = TRUE, spikemode = "across", spikecolor = "#6C63FF",
                          spikethickness = 1, spikedash = "dot")) %>%
      htmlwidgets::onRender("
        function(el) {
          el.on('plotly_hover', function(d) {
            var n = d.points[0].data.x.length, p = d.points[0].pointNumber;
            var op = Array.from({length: n}, function(_, i){ return i === p ? 1 : 0.4; });
            Plotly.restyle(el, {'marker.opacity': [op]});
          });
          el.on('plotly_unhover', function() {
            Plotly.restyle(el, {'marker.opacity': 1});
          });
        }
      ")
  })
  
  # ---- Engagement by Media Type ----
  output$media_plot <- renderPlotly({
    
    d <- data %>%
      group_by(media_type) %>%
      summarise(total_engagement = sum(total_engagement), .groups = "drop")
    
    total_m <- round(sum(d$total_engagement) / 1e6, 2)
    
    plot_ly(
      d,
      labels = ~media_type,
      values = ~total_engagement,
      type = "pie",
      hole = 0.65,
      sort = FALSE,
      marker = list(colors = brand[c(1, 2, 4)],
                    line = list(color = "white", width = 3)),
      textinfo = "percent",
      textfont = list(color = "white", size = 14),
      hovertemplate = "<b>%{label}</b><br>%{value:,.0f}<br>%{percent}<extra></extra>"
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.05),
        annotations = list(
          list(
            text = paste0("<b>", total_m, "M</b><br><span style='font-size:12px;color:#94a3b8'>total</span>"),
            x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 24)
          )
        ),
        margin = list(t = 20, b = 40)
      ) %>%
      style_plotly() %>%
      htmlwidgets::onRender("
        function(el) {
          el.on('plotly_hover', function(d) {
            var n = d.points[0].data.values.length, p = d.points[0].pointNumber;
            var pull = Array.from({length: n}, function(_, i){ return i === p ? 0.08 : 0; });
            Plotly.restyle(el, {pull: [pull]});
          });
          el.on('plotly_unhover', function() {
            var n = el.data[0].values.length;
            Plotly.restyle(el, {pull: [Array(n).fill(0)]});
          });
        }
      ")
  })
}
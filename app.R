# ==============================
# Instagram Analytics Dashboard
# Main Application
# ==============================

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(DT)


# ==============================
# Load Application Files
# ==============================

source("R/data.R")
source("R/helpers.R")
source("R/overview.R")
source("R/engagement.R")
source("R/audience.R")
source("R/explorer.R")


# ==============================
# User Interface
# ==============================

ui <- page_sidebar(
  
  # ==============================
  # Custom CSS
  # ==============================
  
  tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "style.css"
  ),
  
  
  # ==============================
  # Sidebar
  # ==============================
  
  sidebar = sidebar(
    
    div(
      class = "sidebar-brand",
      
      h2("Social Media"),
      
      h2("Instagram"),
      h2("Analytics"),
      
      p(
        "Big Data Analytics",
        class = "sidebar-subtitle"
      )
    ),
    
    hr(),
    
    radioButtons(
      "page",
      NULL,
      
      choices = c(
        "Overview" = "overview",
        "Engagement Analysis" = "engagement",
        "Audience & Content" = "audience",
        "Data Explorer" = "explorer"
      ),
      
      selected = "overview"
    ),
    
    div(
      class = "sidebar-footer",
      
      p(
        "Hadoop • Pig • MapReduce • Hive • R"
      ),
      
      p(
        "Instagram Trend & Engagement Analytics"
      )
    )
  ),
  
  
  # ==============================
  # Main Content
  # ==============================
  
  div(
    class = "dashboard-content",
    
    uiOutput("main_page")
  )
)


# ==============================
# Server
# ==============================

server <- function(input, output, session) {
  
  
  # ==============================
  # Page Navigation
  # ==============================
  
  output$main_page <- renderUI({
    
    switch(
      
      input$page,
      
      "overview" =
        overview_ui(),
      
      "engagement" =
        engagement_ui(),
      
      "audience" =
        audience_ui(),
      
      "explorer" =
        explorer_ui()
    )
  })
  
  
  # ==============================
  # Page Servers
  # ==============================
  
  overview_server(
    input,
    output,
    session
  )
  
  engagement_server(
    input,
    output,
    session
  )
  
  audience_server(
    input,
    output,
    session
  )
  
  explorer_server(
    input,
    output,
    session
  )
}


# ==============================
# Run Application
# ==============================

shinyApp(
  ui = ui,
  server = server
)
library(shiny)
library(leaflet)
library(dplyr)
library(sf)

app_data     <- readRDS("moto_road_data.rds")
roads_rider  <- app_data$roads_rider
roads_all    <- app_data$roads_all
total_rider  <- app_data$total_rider
total_all    <- app_data$total_all

ui <- fluidPage(
  titlePanel("Illinois Motorcycle Fatality Map (2004-2023)"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("all_moto", "Include all crashes involving a motorcycle", value = FALSE),
      sliderInput("min_fatals", "Min fatalities per segment:",
                  min = 1, max = 10, value = 1, step = 1),
      checkboxInput("remove_outliers", "Remove outliers (top 2.5%)", value = FALSE),
      checkboxInput("top_100", "Show only top 100 segments", value = FALSE),
      hr(),
      uiOutput("stats"),
      hr(),
      tags$details(
        tags$summary(style = "cursor:pointer; font-weight:bold; font-size:13px;", "How to use this map"),
        tags$div(style = "font-size:12px; color:#555; margin-top:8px;",
          tags$p(tags$b("Min fatalities:"), "Show only segments with at least this many motorcycle fatalities."),
          tags$p(tags$b("Remove outliers:"), "Hides the top 2.5% of segments by fatality rate."),
          tags$p(tags$b("Top 100:"), "Shows only the 100 segments with the highest fatality rate."),
          tags$p(tags$b("Color:"), "Darker purple = higher fatality rate per 100,000 vehicles.",
                 "Rate = fatalities / (AADT × 365 × years active) × 100,000."),
          tags$hr(),
          tags$p(tags$b("Data sources")),
          tags$p(tags$b("All motorcycle crashes:"), "When checked, shows any crash where a motorcycle",
                 "was present — even if the fatality was a car driver. Unchecked shows only crashes",
                 "where the rider or passenger died."),
          tags$ul(
            tags$li("Crash data: NHTSA FARS (2004-2023)"),
            tags$li("Rider fatalities: crashes where a motorcyclist (PER_TYP 6/7) died"),
            tags$li("All motorcycle crashes: any crash with a motorcycle present (vehicle BODY_TYP 20-29)"),
            tags$li("Traffic volume: IDOT AADT Historical, 2023 layer"),
            tags$li("Crashes snapped to nearest road segment")
          )
        )
      ),
      width = 3
    ),
    mainPanel(
      leafletOutput("map", height = "85vh"),
      width = 9
    )
  )
)

server <- function(input, output, session) {

  active_roads <- reactive({
    if (input$all_moto) roads_all else roads_rider
  })

  active_total <- reactive({
    if (input$all_moto) total_all else total_rider
  })

  filtered <- reactive({
    s <- active_roads() %>% filter(fatalities >= input$min_fatals)
    if (input$remove_outliers) {
      cutoff <- quantile(s$rate_per_100k, 0.975)
      s <- s %>% filter(rate_per_100k <= cutoff)
    }
    if (input$top_100) s <- s %>% slice_max(rate_per_100k, n = 100)
    s
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -89.4, lat = 40.0, zoom = 7) %>%
      addControl(
        html = paste0(
          "<div style='background:white; padding:6px 10px; border-radius:4px; ",
          "border:1px solid #ccc; font-size:11px; color:#555; max-width:220px;'>",
          "<b>Data Sources</b><br>",
          "Crashes: NHTSA FARS (2004-2023)<br>",
          "Traffic: IDOT AADT Historical (2023)",
          "</div>"
        ),
        position = "bottomleft"
      )
  })

  observe({
    s     <- filtered()
    proxy <- leafletProxy("map") %>% clearShapes() %>% clearControls()
    if (nrow(s) == 0) return()

    pal <- colorNumeric(c("#d476ff", "#2d0044"), domain = s$rate_per_100k)

    popup_text <- paste0(
      "Fatalities: ", s$fatalities, "<br>",
      "Rate per 100k: ", round(s$rate_per_100k, 4), "<br>",
      "Years active: ", s$year_range, " (", s$n_years, " yrs)<br>",
      "AADT: ", round(s$AADT), "<br>",
      "Length: ", round(s$length_km, 2), " km"
    )

    proxy %>%
      addPolylines(
        data    = s,
        color   = ~pal(rate_per_100k),
        weight  = 3,
        opacity = 0.85,
        popup   = popup_text
      ) %>%
      addLegend("bottomright", pal = pal, values = s$rate_per_100k,
                title = "Motorcycle fatalities<br>per 100k vehicles")
  })

  output$stats <- renderUI({
    s <- filtered()

    n_removed <- 0
    if (input$remove_outliers) {
      pre    <- active_roads() %>% filter(fatalities >= input$min_fatals)
      cutoff <- quantile(pre$rate_per_100k, 0.975)
      n_removed <- sum(pre$rate_per_100k > cutoff)
    }

    displayed  <- sum(s$fatalities)
    pct        <- round(displayed / active_total() * 100, 1)
    total_km   <- round(sum(s$length_km), 1)
    label      <- if (input$all_moto) "all motorcycle-involved" else "motorcyclist"

    tags <- tagList(
      h4(paste0(pct, "% of IL ", label, " fatalities")),
      p(paste0(displayed, " of ", active_total(), " fatalities")),
      p(paste0("across ", nrow(s), " road segments")),
      p(paste0("Total road length: ", total_km, " km"))
    )

    if (n_removed > 0) {
      tags <- tagList(tags, hr(),
        p(style = "color: #888; font-size: 12px;",
          paste0(n_removed, " outlier segment(s) hidden")))
    }

    tags
  })
}

shinyApp(ui, server)

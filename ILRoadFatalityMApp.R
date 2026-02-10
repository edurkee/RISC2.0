library(shiny)
library(leaflet)
library(dplyr)
library(dbscan)
library(sf)

# Load precomputed data (run Analysis.R first to generate this)
app_data <- readRDS("cluster_app_data.rds")

snap_pts        <- app_data$snap_pts
fatals          <- app_data$fatals
crash_years     <- app_data$years
segment_ids     <- app_data$segment_ids
aadt_lookup     <- app_data$aadt_lookup
total_fatalities <- app_data$total_il_fatalities

ui <- fluidPage(
  titlePanel("Illinois Road Fatality Map (2004-2023)"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("eps", "Cluster radius (meters):",
                  min = 50, max = 1000, value = 300, step = 25),
      sliderInput("minPts", "Min crashes per cluster:",
                  min = 2, max = 10, value = 4, step = 1),
      radioButtons("display_mode", "Display style:",
                   choices = c("Circles" = "circles", "Polygons" = "polygons"),
                   selected = "circles", inline = TRUE),
      checkboxInput("remove_outliers", "Remove outliers (top 2.5%)", value = FALSE),
      checkboxInput("top_100", "Show only top 100 most dangerous", value = FALSE),
      hr(),
      uiOutput("stats"),
      hr(),
      tags$details(
        tags$summary(style = "cursor:pointer; font-weight:bold; font-size:13px;", "How to use this map"),
        tags$div(style = "font-size:12px; color:#555; margin-top:8px;",
          tags$p(tags$b("Clusters Parameters:"), "Define your clusters by max distance between fatalities",
                 "and minimum number of fatal crashes required to form a cluster."),
          tags$p(tags$b("Display style:"), "Use Circle display to identify cluster centroids.",
                 "Use polygon display (and tighter cluster distance) to identify whether risk focuses on an intersection or corridor."),
          tags$p(tags$b("Remove outliers:"), "Hides the top 2.5% of clusters by",
                 "fatality rate to reduce the effect of extreme values on the color scale."),
          tags$p(tags$b("Top 100:"), "Shows only the 100 clusters with the highest fatality rate."),
          tags$p(tags$b("Color:"), "Darker red = higher fatality rate per 100,000 vehicles.",
                 "Rate is calculated as fatalities / (AADT x 365 x years active) x 100,000."),
          tags$hr(),
          tags$p(tags$b("Data sources")),
          tags$ul(
            tags$li("Crash data: NHTSA FARS (Fatality Analysis Reporting System), 2004-2023"),
            tags$li("Traffic volume: IDOT AADT Historical, 2023 layer"),
            tags$li("Crash locations are snapped to the nearest road segment for clustering")
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

  # Debounce inputs so dragging sliders doesn't spam DBSCAN
  eps_d    <- debounce(reactive(input$eps), 300)
  minPts_d <- debounce(reactive(input$minPts), 300)

  clusters <- reactive({
    db <- dbscan(snap_pts, eps = eps_d(), minPts = minPts_d())
    cluster_id <- db$cluster

    # Build a lightweight data frame for summarization
    df <- data.frame(
      cluster = cluster_id,
      fatals = fatals,
      year = crash_years,
      segment_id = segment_ids,
      x = snap_pts[, 1],
      y = snap_pts[, 2]
    ) %>%
      filter(cluster > 0) %>%
      left_join(aadt_lookup, by = "segment_id")

    if (nrow(df) == 0) return(NULL)

    # Build convex hull polygons per cluster (buffered so small clusters are visible)
    hulls <- df %>%
      group_by(cluster) %>%
      group_map(~ {
        pts <- as.matrix(.x[, c("x", "y")])
        mp <- st_multipoint(pts)
        if (nrow(pts) >= 3) {
          # Convex hull + buffer for visible area
          geom <- st_buffer(st_convex_hull(mp), dist = 50)
        } else {
          # Too few points for a hull — just buffer the points
          geom <- st_buffer(mp, dist = 50)
        }
        geom
      })
    hull_sfc <- st_sfc(hulls, crs = 26916)

    # Summarize stats per cluster
    summary <- df %>%
      group_by(cluster) %>%
      summarize(
        fatalities = sum(fatals),
        n_crashes = n(),
        n_years = n_distinct(year),
        year_span = max(year) - min(year) + 1L,
        year_range = paste(min(year), max(year), sep = "-"),
        avg_aadt = mean(AADT, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        exposure = avg_aadt * 365 * year_span,
        rate_per_100k = (fatalities / exposure) * 100000
      ) %>%
      filter(is.finite(rate_per_100k))

    # Attach hull geometries, compute area in meters, then transform to WGS84
    summary_sf <- st_sf(summary, geometry = hull_sfc[summary$cluster])
    summary_sf$area_sq_km <- as.numeric(st_area(summary_sf)) / 1e6
    summary_sf <- st_transform(summary_sf, 4326)

    # Also compute centroids for circle mode
    centroids <- st_centroid(summary_sf)
    coords <- st_coordinates(centroids)
    summary_sf$lng <- coords[, 1]
    summary_sf$lat <- coords[, 2]

    list(
      summary_sf = summary_sf,
      cluster_fatalities = sum(summary_sf$fatalities),
      n_clusters = nrow(summary_sf)
    )
  })

  # Base map (rendered once)
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

  # Update map via proxy when sliders or display mode change
  observe({
    res <- clusters()
    proxy <- leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls()

    if (is.null(res) || res$n_clusters == 0) return()

    s <- res$summary_sf

    # Remove top 2.5% outliers if checked
    if (input$remove_outliers) {
      cutoff <- quantile(s$rate_per_100k, 0.975)
      s <- s %>% filter(rate_per_100k <= cutoff)
    }

    # Show only top 100 most dangerous if checked
    if (input$top_100) {
      s <- s %>% slice_max(rate_per_100k, n = 100)
    }

    if (nrow(s) == 0) return()

    pal <- colorNumeric(c("#ff7676", "#440000"), domain = s$rate_per_100k)

    popup_text <- paste0(
      "<b>Cluster ", s$cluster, "</b><br>",
      "Fatalities: ", s$fatalities, "<br>",
      "Crashes: ", s$n_crashes, "<br>",
      "Rate per 100k: ", round(s$rate_per_100k, 4), "<br>",
      "Years active: ", s$year_range, " (", s$n_years, " yrs)<br>",
      "Avg AADT: ", round(s$avg_aadt)
    )

    if (input$display_mode == "polygons") {
      proxy %>%
        addPolygons(
          data = s,
          fillColor = ~pal(rate_per_100k),
          fillOpacity = 0.6,
          color = "#333", weight = 1, opacity = 0.8,
          popup = popup_text
        ) %>%
        addLegend("bottomright", pal = pal, values = s$rate_per_100k,
                  title = "Fatalities per<br>100k Vehicles")
    } else {
      proxy %>%
        addCircleMarkers(
          lng = s$lng, lat = s$lat,
          radius = 6,
          color = pal(s$rate_per_100k),
          fillOpacity = 0.8,
          stroke = TRUE, weight = 1,
          popup = popup_text
        ) %>%
        addLegend("bottomright", pal = pal, values = s$rate_per_100k,
                  title = "Fatalities per<br>100k Vehicles")
    }
  })

  # Stats panel
  output$stats <- renderUI({
    res <- clusters()
    if (is.null(res)) {
      return(p("No clusters found with these parameters."))
    }

    s <- res$summary_sf
    n_removed <- 0
    if (input$remove_outliers) {
      cutoff <- quantile(s$rate_per_100k, 0.975)
      n_removed <- sum(s$rate_per_100k > cutoff)
      s <- s %>% filter(rate_per_100k <= cutoff)
    }

    if (input$top_100) {
      s <- s %>% slice_max(rate_per_100k, n = 100)
    }

    displayed_fatalities <- sum(s$fatalities)
    displayed_clusters <- nrow(s)
    pct <- round(displayed_fatalities / total_fatalities * 100, 1)

    total_area_km2 <- round(sum(s$area_sq_km), 2)

    tags <- tagList(
      h4(paste0(pct, "% of IL fatalities")),
      p(paste0(displayed_fatalities, " of ", total_fatalities, " fatalities")),
      p(paste0("across ", displayed_clusters, " clusters")),
      p(paste0("Total cluster area: ", total_area_km2, " km\u00B2"))
    )

    if (n_removed > 0) {
      tags <- tagList(tags,
        hr(),
        p(style = "color: #888; font-size: 12px;",
          paste0(n_removed, " outlier cluster(s) hidden"))
      )
    }

    tags
  })
}

shinyApp(ui, server)

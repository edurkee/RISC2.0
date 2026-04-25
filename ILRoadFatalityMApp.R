library(shiny)
library(leaflet)
library(dplyr)
library(dbscan)
library(sf)

app_data <- readRDS("cluster_app_data.rds")

snap_pts           <- app_data$snap_pts
fatals             <- app_data$fatals
crash_years        <- app_data$years
segment_ids        <- app_data$segment_ids
crash_type         <- app_data$crash_type
aadt_lookup        <- app_data$aadt_lookup
total_fatalities   <- app_data$total_il_fatalities
fatalities_by_type <- app_data$fatalities_by_type

type_colors <- list(
  car        = c("#ff7676", "#440000"),
  pedestrian = c("#76b8ff", "#002b6b"),
  motorcycle = c("#d476ff", "#2d0044")
)
type_labels <- c(car = "Car / Other Vehicle", pedestrian = "Pedestrian", motorcycle = "Motorcycle")

# Pre-split data by crash type so we don't re-filter on every slider change
data_by_type <- setNames(lapply(c("car", "pedestrian", "motorcycle"), function(t) {
  idx <- which(crash_type == t)
  list(
    snap_pts    = snap_pts[idx, , drop = FALSE],
    fatals      = fatals[idx],
    years       = crash_years[idx],
    segment_ids = segment_ids[idx]
  )
}), c("car", "pedestrian", "motorcycle"))

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
      checkboxGroupInput("crash_types", "Crash types:",
                         choices = c("Car / Other Vehicle" = "car",
                                     "Pedestrian" = "pedestrian",
                                     "Motorcycle" = "motorcycle"),
                         selected = c("car", "pedestrian", "motorcycle")),
      checkboxInput("remove_outliers", "Remove outliers (top 2.5%)", value = FALSE),
      checkboxInput("top_100", "Show only top 100 most dangerous", value = FALSE),
      hr(),
      uiOutput("stats"),
      hr(),
      tags$details(
        tags$summary(style = "cursor:pointer; font-weight:bold; font-size:13px;", "How to use this map"),
        tags$div(style = "font-size:12px; color:#555; margin-top:8px;",
          tags$p(tags$b("Cluster Parameters:"), "Define your clusters by max distance between fatalities",
                 "and minimum number of fatal crashes required to form a cluster."),
          tags$p(tags$b("Display style:"), "Use Circle display to identify cluster centroids.",
                 "Use polygon display (and tighter cluster distance) to identify whether risk focuses on an intersection or corridor."),
          tags$p(tags$b("Crash types:"), "Filter to Pedestrian (blue), Motorcycle (purple), or Car/Other (red).",
                 "Each type is clustered independently."),
          tags$p(tags$b("Remove outliers:"), "Hides the top 2.5% of clusters by",
                 "fatality rate to reduce the effect of extreme values on the color scale."),
          tags$p(tags$b("Top 100:"), "Shows only the 100 clusters with the highest fatality rate per type."),
          tags$p(tags$b("Color:"), "Darker = higher fatality rate per 100,000 vehicles.",
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

  eps_d    <- debounce(reactive(input$eps), 300)
  minPts_d <- debounce(reactive(input$minPts), 300)

  # Run DBSCAN independently for each selected crash type
  clusters <- reactive({
    selected <- input$crash_types
    if (length(selected) == 0) return(NULL)

    results <- setNames(lapply(selected, function(type) {
      fd <- data_by_type[[type]]
      if (nrow(fd$snap_pts) == 0) return(NULL)

      db <- dbscan(fd$snap_pts, eps = eps_d(), minPts = minPts_d())
      cluster_id <- db$cluster

      df <- data.frame(
        cluster    = cluster_id,
        fatals     = fd$fatals,
        year       = fd$years,
        segment_id = fd$segment_ids,
        x          = fd$snap_pts[, 1],
        y          = fd$snap_pts[, 2]
      ) %>%
        filter(cluster > 0) %>%
        left_join(aadt_lookup, by = "segment_id")

      if (nrow(df) == 0) return(NULL)

      hulls <- df %>%
        group_by(cluster) %>%
        group_map(~ {
          pts <- as.matrix(.x[, c("x", "y")])
          mp  <- st_multipoint(pts)
          if (nrow(pts) >= 3) st_buffer(st_convex_hull(mp), dist = 50)
          else                st_buffer(mp, dist = 50)
        })
      hull_sfc <- st_sfc(hulls, crs = 26916)

      summary <- df %>%
        group_by(cluster) %>%
        summarize(
          fatalities = sum(fatals),
          n_crashes  = n(),
          n_years    = n_distinct(year),
          year_span  = max(year) - min(year) + 1L,
          year_range = paste(min(year), max(year), sep = "-"),
          avg_aadt   = mean(AADT, na.rm = TRUE),
          .groups    = "drop"
        ) %>%
        mutate(
          exposure      = avg_aadt * 365 * year_span,
          rate_per_100k = (fatalities / exposure) * 100000
        ) %>%
        filter(is.finite(rate_per_100k))

      summary_sf <- st_sf(summary, geometry = hull_sfc[summary$cluster])
      summary_sf$area_sq_km <- as.numeric(st_area(summary_sf)) / 1e6
      summary_sf <- st_transform(summary_sf, 4326)

      centroids <- st_centroid(summary_sf)
      coords    <- st_coordinates(centroids)
      summary_sf$lng <- coords[, 1]
      summary_sf$lat <- coords[, 2]

      summary_sf
    }), selected)

    Filter(Negate(is.null), results)
  })

  current_total <- reactive({
    selected <- input$crash_types
    if (length(selected) == 0) return(0)
    sum(fatalities_by_type$fatalities[fatalities_by_type$crash_type %in% selected])
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

  # Redraw all layers when anything changes
  observe({
    res   <- clusters()
    proxy <- leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls()

    if (is.null(res) || length(res) == 0) return()

    for (type in names(res)) {
      s <- res[[type]]

      if (input$remove_outliers) {
        cutoff <- quantile(s$rate_per_100k, 0.975)
        s <- s %>% filter(rate_per_100k <= cutoff)
      }
      if (input$top_100) {
        s <- s %>% slice_max(rate_per_100k, n = 100)
      }
      if (nrow(s) == 0) next

      pal <- colorNumeric(type_colors[[type]], domain = s$rate_per_100k)

      popup_text <- paste0(
        "<b>", type_labels[type], " Cluster ", s$cluster, "</b><br>",
        "Fatalities: ", s$fatalities, "<br>",
        "Crashes: ", s$n_crashes, "<br>",
        "Rate per 100k: ", round(s$rate_per_100k, 4), "<br>",
        "Years active: ", s$year_range, " (", s$n_years, " yrs)<br>",
        "Avg AADT: ", round(s$avg_aadt)
      )

      if (input$display_mode == "polygons") {
        proxy <- proxy %>%
          addPolygons(
            data = s,
            fillColor = ~pal(rate_per_100k),
            fillOpacity = 0.6,
            color = "#333", weight = 1, opacity = 0.8,
            popup = popup_text
          )
      } else {
        proxy <- proxy %>%
          addCircleMarkers(
            lng = s$lng, lat = s$lat,
            radius = 6,
            color = pal(s$rate_per_100k),
            fillOpacity = 0.8,
            stroke = TRUE, weight = 1,
            popup = popup_text
          )
      }

      proxy <- proxy %>%
        addLegend("bottomright", pal = pal, values = s$rate_per_100k,
                  title = paste0(type_labels[type], "<br>per 100k vehicles"))
    }
  })

  # Stats panel
  output$stats <- renderUI({
    res <- clusters()
    if (is.null(res) || length(res) == 0) {
      return(p("No clusters found with these parameters."))
    }

    total <- current_total()
    if (total == 0) return(p("Select at least one crash type."))

    all_fatalities <- 0
    all_clusters   <- 0
    all_area       <- 0
    n_removed      <- 0

    for (type in names(res)) {
      s <- res[[type]]
      if (input$remove_outliers) {
        cutoff    <- quantile(s$rate_per_100k, 0.975)
        n_removed <- n_removed + sum(s$rate_per_100k > cutoff)
        s <- s %>% filter(rate_per_100k <= cutoff)
      }
      if (input$top_100) s <- s %>% slice_max(rate_per_100k, n = 100)

      all_fatalities <- all_fatalities + sum(s$fatalities)
      all_clusters   <- all_clusters   + nrow(s)
      all_area       <- all_area       + sum(s$area_sq_km)
    }

    pct <- round(all_fatalities / total * 100, 1)

    tags <- tagList(
      h4(paste0(pct, "% of selected fatalities")),
      p(paste0(all_fatalities, " of ", total, " fatalities")),
      p(paste0("across ", all_clusters, " clusters")),
      p(paste0("Total cluster area: ", round(all_area, 2), " km²"))
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

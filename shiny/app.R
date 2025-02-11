library(shiny)
library(leaflet)
library(DBI)
library(duckdb)
library(dplyr)
library(dbplyr)
library(terra)
library(viridis)
library(vscDebugger)

# Connect to DuckDB database
con <- dbConnect(duckdb(dbdir = "shiny.db"))
db <- tbl(con, "gbif")

db |> filter(if_any(kingdom:species, ~ . == "Bacteria"))

# Make taxonomy more accessible
taxonomy <- tibble(
  common_name = c(
    "Birds", "Lizards & Snakes", "Amphibians", "Mammals",
    "Slugs/Snails", "Arachnids", "Insects",
    # "Cartilaginous fish",
    # "Bony fish",
    "Mushrooms", "Bacteria",
    "Dicots", "Bryophytes", "Monocots", "Conifers"
  ),
  taxon_name = c(
    "Aves", "Squamata", "Amphibia", "Mammalia",
    "Gastropoda", "Arachnida", "Insecta",
    # "Chondrichthyes",
    # "Osteichthyes",
    "Basidiomycota", "Bacteria",
    "Magnoliopsida", "Bryopsida", "Liliopsida", "Pinopsida"
  ),
  taxon_level = c(
    "class", "class", "class", "class",
    "class", "class", "class",
    # "class", # Cartilaginous fish (e.g., sharks, rays)
    # "class", # Bony fish (includes ray-finned and lobe-finned fishes)
    "phylum", "kingdom",
    "class", "class", "class", "class"
  )
)

# Load unique taxonomic classes for dropdown
class_choices <- taxonomy |>
  pull(common_name)
class_choices <- c("All", class_choices) # Add "All" option

# Define UI
ui <- fluidPage(
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "dark_mode.css")
  # ),
  includeCSS("www/style.css"),
  titlePanel("GBIF Record Density Map - California"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_taxon", "Select Taxonomic Class:",
        choices = class_choices, selected = "Slugs/Snails"
      ),
      radioButtons("metric", "Aggregation Metric:",
        choices = c(
          "Raw Observations" = "obs",
          "Distinct Species" = "species"
        ),
        selected = "obs"
      ),
      width = 2 # A small sidebar leaves more space for the map
    ),
    mainPanel(
      leafletOutput("map", height = "85vh", width = "78vw")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive function to fetch and aggregate data.
  # We compute the raw counts (or distinct species counts) and then add a log‑transformed
  # column for color mapping.
  gbif_raster <- reactive({
    data_query <- db |>
      mutate(
        latitude = round(decimallatitude, 1),
        longitude = round(decimallongitude, 1)
      ) |>
      filter(!is.na(latitude) & !is.na(longitude))

    if (input$selected_taxon != "All") {
      selected_taxon_level <- taxonomy |>
        filter(common_name == input$selected_taxon) |>
        pull(taxon_level)
      scientific_name <- taxonomy |>
        filter(common_name == input$selected_taxon) |>
        pull(taxon_name)
      data_query <- data_query |>
        filter(!!sym(selected_taxon_level) == scientific_name)
    }

    if (input$metric == "species") {
      df <- data_query |>
        group_by(longitude, latitude) |>
        summarise(N = n_distinct(species)) |>
        ungroup() |>
        collect()
    } else {
      df <- data_query |>
        count(longitude, latitude) |>
        rename(N = n) |>
        collect()
    }

    # Create a raster using the log-transformed values.
    r <- rast(df[, c("longitude", "latitude", "N")],
      type = "xyz", crs = "epsg:4326"
    )

    list(raster = r)
  })

  # Render initial Leaflet Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -119.5, lat = 37, zoom = 6)
  })

  # Update map with raster heatmap when data changes.
  observe({
    data <- gbif_raster()
    r <- data$raster
    if (!is.null(r)) {
      # Define a palette using the log-transformed raster values.
      pal <- colorNumeric(viridis(100), domain = log(values(r)), na.color = "transparent")

      legend_title <- ifelse(input$metric == "species",
        "Distinct Species Count",
        "Observation Count"
      )

      leafletProxy("map") %>%
        clearImages() %>%
        clearControls() %>%
        addRasterImage(r,
          colors = function(x) pal(log(x)),
          opacity = 0.8
        ) %>%
        addLegend(
          pal = pal,
          values = log(values(r)),
          labFormat = labelFormat(transform = function(x) round(exp(x), 0)),
          title = legend_title,
          opacity = 0.8
        )
    }
  })
}

# Run the app
shinyApp(ui, server)

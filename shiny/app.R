library(shiny)
library(leaflet)
library(DBI)
library(duckdb)
library(dplyr)
library(dbplyr)
library(terra)
library(viridis)
library(shinycssloaders)
# library(vscDebugger)

# Connect to DuckDB database
con <- dbConnect(duckdb(dbdir = "shiny.db"))
con |> dbExecute("SET memory_limit = '750MB';")
db <- tbl(con, "gbif")

# db |> filter(if_any(kingdom:species, ~ . == "Bacteria"))

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

  # Wrap titlePanel in a div with our custom class for left margin
  div(
    class = "title-container",
    titlePanel("GBIF California Coverage")
  ),
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
  ),
  # Footer note with data download and author info
  tags$div(
    style = "text-align: right; font-size: 10px; padding: 10px; padding-right: 3vw;",
    "Data downloaded from GBIF on September 1st, 2024. App by Avery P. Hill, postdoc in the Center for Biodiversity and Community Science at the CalAcademy. ",
    tags$a(
      href = "https://github.com/avephill/gbif-ca-coverage",
      "(GitHub Repository)",
      target = "_blank" # Opens the link in a new tab
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive function to fetch and aggregate data.
  # We compute the raw counts (or distinct species counts) and then add a log‑transformed
  # column for color mapping.
  gbif_raster <- reactive({
    withProgress(message = "Processing GBIF data...", value = 0, {
      
      # Step 1: Prepare query and filter out missing data
      incProgress(0.1, detail = "Preparing and filtering data")
      data_query <- db |>
        mutate(
          latitude = round(decimallatitude, 1),
          longitude = round(decimallongitude, 1)
        ) |>
        filter(!is.na(latitude) & !is.na(longitude))
      
      # Step 2: Filter by selected taxonomy if not "All"
      if (input$selected_taxon != "All") {
        incProgress(0.1, detail = "Filtering by taxonomic group")
        selected_taxon_level <- taxonomy |>
          filter(common_name == input$selected_taxon) |>
          pull(taxon_level)
        scientific_name <- taxonomy |>
          filter(common_name == input$selected_taxon) |>
          pull(taxon_name)
        data_query <- data_query |>
          filter(!!sym(selected_taxon_level) == scientific_name)
      }
      
      # Step 3: Aggregate data based on the selected metric
      incProgress(0.3, detail = "Aggregating data")
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
      
      # Step 4: Create a raster from the aggregated data using a fixed California extent
      incProgress(0.3, detail = "Creating raster")
      r <- rast(df[, c("longitude", "latitude", "N")],
                type = "xyz", crs = "epsg:4326",
                ext = terra::ext(-125.5, -114, 31.5, 42.5) # CA extent
      )
      
      # Final step: Wrap up
      incProgress(0.2, detail = "Finalizing")
      
      list(raster = r)
    })
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
        "Species / 100 km<sup>2</sup>",
        "Observations / 100 km<sup>2</sup>"
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

  # Close the DuckDB connection when the session ends.
  session$onSessionEnded(function() {
    print("Session ended: Closing DuckDB connection.")
    dbDisconnect(con, shutdown = TRUE)
  })
}

# Run the app
shinyApp(ui, server)

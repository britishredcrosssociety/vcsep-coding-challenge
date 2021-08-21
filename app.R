# ---- Load libraries ----
# geographr is in development and can be installed from GitHub: 
# - https://github.com/britishredcrosssociety/geographr
library(shiny)
library(dplyr)
library(tidyr)
library(leaflet)
library(sf)
library(IMD)
library(geographr)

# ---- Prepare data ----
# Join the English Local Authortiy District IMD data set (exported from IMD) to 
# the corresponding boundary (shape) file (exported from geograhr)
imd_with_boundaries <-
  boundaries_lad |>
  right_join(imd_england_lad)

# ---- UI ----
ui <-
  fluidPage(

    # - Set CSS -
    includeCSS("www/styles.css"),

    # - Title -
    fluidRow(
      align = "center",
      titlePanel("IMD Explorer")
    ),

    # - Select Box -
    fluidRow(
      column(
        width = 12,
        align = "center",
        selectizeInput(
          "selectbox",
          label = NULL,
          choices = sort(imd_with_boundaries$lad_name),
          options = list(
            placeholder = "Select a Local Authority",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      )
    ),

    # - Map & Plot -
    fluidRow(

      # - Map -
      column(
        width = 6,
        align = "center",
        leafletOutput("map", height = 600)
      ),

      # - Table -
      column(
        width = 6,
        align = "center",
        tableOutput("imdTable")
      )
    )
  )

# ---- Server ----
server <-
  function(input, output, session) {

    # - Track selections -
    # Track which map polygons the user has clicked on
    selected_polygon <- reactiveVal("E06000001")

    observeEvent(input$map_shape_click, {
      input$map_shape_click$id |>
      selected_polygon()
    })

    # - Map -
    output$map <-
      renderLeaflet({
        leaflet() |>
        setView(lat = 52.75, lng = -2.0, zoom = 6) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addPolygons(
          data = imd_with_boundaries,
          layerId = ~lad_code,
          weight = 0.7,
          opacity = 0.5,
          # color = "#bf4aee",
          dashArray = "0.1",
          fillOpacity = 0.4,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = imd_with_boundaries$lad_name
        )
      })

    # - Table -
    output$imdTable <-
      renderTable(
        imd_england_lad |>
        filter(lad_code == selected_polygon()) |>
        pivot_longer(
          cols = !lad_code,
          names_to = "Variable",
          values_to = "Value"
        ) |>
        select(-lad_code)
      )
  }

# ---- Run App ----
shinyApp(ui = ui, server = server)
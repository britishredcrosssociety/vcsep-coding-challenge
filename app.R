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
        tableOutput("Table")
########
# Bug 2 - The above line was tableOutput("imdTable"), however the
#         table is just called "Table" below. Deleting the imd (or
#         inserting it in the table definition) fixes the problem
########
      )
    )
  )

# ---- Server ----
server <-
  function(input, output, session) {

    # - Track selections -
    # Track which map polygons the user has clicked on

# Changed to selected_all to implement the feature as name made more sense 
#    selected_polygon <- reactiveVal("E06000001")
    selected_all <- reactiveVal("E06000001")

########
# Feature - Need to insert some code to track the event of someone
#           clicking on the list, hence the below observeEvent.
#           Note that I use the same variable for both observed events
#           as either of them occurring should change the same table.
#           Also the input is more complicated as the selectbox outputs
#           a region name and we have to extract from the
#           imd_with_boundaries table the lad_code corresponding with
#           that region
########
    
    observeEvent(input$selectbox, {
      filter(imd_with_boundaries, lad_name==input$selectbox)$lad_code |>
        selected_all()
    })
    
    observeEvent(input$map_shape_click, {
      input$map_shape_click$id |>
      selected_all()
    })
########    
# Bug 1 - Missing } in the above, inserting this fixes the problem
########
    
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
    output$Table <-
      renderTable(
        imd_england_lad |>
# Change below to selected_all for feature, and had to add an if statement
# due to complications with the default value of the select box
        filter(if (length(selected_all())==0) lad_code=="E06000001" else lad_code == selected_all()) |>
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
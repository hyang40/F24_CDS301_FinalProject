

#author: "Giselle_Rahimi and Heewon Yang"
#date: "2024-10-11"

# Setting my working directory so that I can load the offender dataset
setwd("C:/Users/gisel/OneDrive - George Mason University - O365 Production/Desktop/clean_CDS/Worksheet_5_final_Rahimi_Yang")

getwd()

# Turning off automatic messages
options(warn=-1)

# 1. loading libraries ---------------------------------------------------------
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(grid)
library(plotly)
library(leaflet)
library(readr)
library(mapview)
library(tidyr)
library(gganimate)
library(colorspace)
library(ggrepel)
library(raster)
library(shiny)
library(gapminder)
library(data.table)
library(plotly)
library(ggplot2)
library(htmlwidgets)
library(ggthemes)
library(sf)
library(cartogram)
library(htmltools)
library(leaflet.extras)


# 2. list.files() --------------------------------------------------------------
# main data
offender <- read.csv("Sex_Offender_Registry.csv")

schools = read.csv("washington-dc-public-schools.csv")
police <- read.csv("Police_Stations.csv")

#shape data
shape = read_sf("ACS_5-Year_Economic_Characteristics_DC_Ward.shp")


# 3. data wrangling ------------------------------------------------------------
# offender 
offender <- offender %>% filter(!is.na(LATITUDE) & !is.na(LONGITUDE))           #remove NA values from longitude
offender <- offender %>% filter(TYPE == "HOME")                                 #only include offender homes
offender <- offender %>% filter(MAXCLASSIFICATION == 'B')                       #only child offenders = B
offender <- offender %>% mutate(group = 'criminal')                             #document as group criminal

# school 
schools <- schools %>% filter(FACUSE == 'Elementary School')                    #include only elementary school
schools <- schools %>% mutate(group = 'school')                                 #document as group school

# police officer
offender <- offender %>% dplyr::select(-CREATOR, -CREATED, -EDITOR, -EDITED, -GIS_ID)  #drop extra columns
police <- police %>% dplyr::select(-EDITOR, -EDITED, -CREATOR, -CREATED, -ROOM)        #drop extra columns
police <- police %>% filter(TYPE == 'Station' | TYPE == 'Substation')           #include only official stations
police <- police %>% mutate(group = 'police_station')                           #document as group police station
                 #ensure raster is in WGS84 (EPSG:4326)

# shp. DC Economic
shape = shape %>% dplyr::select(DP03_0051E, NAMELSAD, NAME)                     #only select useful columns
shape = shape %>% rename(income = 'DP03_0051E')                                 #rename column to 'income'
palette <- colorNumeric(palette = c('#F4F2EF','#2b0156'),domain = shape$income) #create numeric color palette using income

shape <- st_transform(shape, crs = 4326)                                        #transform shape to use EPGS:4326 crs
                                                               #clean map


# 4 leaflet text pop -----------------------------------------------------------
# offender pop up
offender_marker <-                                                              #Create HTML text popup for offenders
  paste(
    "First Name: ", offender$FIRSTNAME, "<br/>",                              
    "Last Name: ", offender$LASTNAME, "<br/>",
    "Height: ", offender$HEIGHTNUM, " Inches", "<br/>",
    "Weight: ", offender$WEIGHTNUM, " Pounds", "<br/>",
    "Eye Color: ", offender$EYECOLOR, "<br/>",
    "Hair Color: ", offender$HAIRCOLOR, "<br/>",
    "Zip Code: ", offender$ZIPCODE, sep = ""
  )%>% lapply(htmltools::HTML)

# school pop up
school_marker <-                                                                #Create HTML text popup for schools 
  paste(
    "Name: ", schools$NAME, "<br/>", 
    "Address: ", schools$ADDRESS, "<br/>",
    "Total students: ", schools$TOTAL_STUD, "<br/>",
    "Grades: ", schools$GRADES, sep = ""
  )%>% lapply(htmltools::HTML)

# police station pop up
station_marker <-                                                               #Create HTML text popup for police stations   
  paste(  
    "Name: ", police$NAME, "<br/>", 
    "Address: ", police$ADDRESS, "<br/>",
    "Phone Number: ", police$PHONE, "<br/>",
    "Contact Person: ", police$CONTACT, sep = ""
  )%>% lapply(htmltools::HTML)

#1b4f72
#d35400
#1e8449

#<svg xmlns="http://www.w3.org/2000/svg" height="24px" viewBox="0 -960 960 960" width="24px" fill="#1b4f72"><path d="M480-360q56 0 101-27.5t71-72.5q-35-29-79-44.5T480-520q-49 0-93 15.5T308-460q26 45 71 72.5T480-360Zm0-200q33 0 56.5-23.5T560-640q0-33-23.5-56.5T480-720q-33 0-56.5 23.5T400-640q0 33 23.5 56.5T480-560Zm0 374q122-112 181-203.5T720-552q0-109-69.5-178.5T480-800q-101 0-170.5 69.5T240-552q0 71 59 162.5T480-186Zm0 106Q319-217 239.5-334.5T160-552q0-150 96.5-239T480-880q127 0 223.5 89T800-552q0 100-79.5 217.5T480-80Zm0-480Z"/></svg>
#<svg xmlns="http://www.w3.org/2000/svg" height="24px" viewBox="0 -960 960 960" width="24px" fill="#d35400"><path d="M480-120 200-272v-240L40-600l440-240 440 240v320h-80v-276l-80 44v240L480-120Zm0-332 274-148-274-148-274 148 274 148Zm0 241 200-108v-151L480-360 280-470v151l200 108Zm0-241Zm0 90Zm0 0Z"/></svg>
#<svg xmlns="http://www.w3.org/2000/svg" height="24px" viewBox="0 -960 960 960" width="24px" fill="#1e8449"><path d="m368-336 112-84 110 84-42-136 112-88H524l-44-136-44 136H300l110 88-42 136ZM480-80q-139-35-229.5-159.5T160-516v-244l320-120 320 120v244q0 152-90.5 276.5T480-80Zm0-84q104-33 172-132t68-220v-189l-240-90-240 90v189q0 121 68 220t172 132Zm0-316Z"/></svg>  
offender_icon <- makeIcon(
  iconUrl = "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIGhlaWdodD0iMjRweCIgdmlld0JveD0iMCAtOTYwIDk2MCA5NjAiIHdpZHRoPSIyNHB4IiBmaWxsPSIjMWI0ZjcyIj48cGF0aCBkPSJNNDgwLTM2MHE1NiAwIDEwMS0yNy41dDcxLTcyLjVxLTM1LTI5LTc5LTQ0LjVUNDgwLTUyMHEtNDkgMC05MyAxNS41VDMwOC00NjBxMjYgNDUgNzEgNzIuNVQ0ODAtMzYwWm0wLTIwMHEzMyAwIDU2LjUtMjMuNVQ1NjAtNjQwcTAtMzMtMjMuNS01Ni41VDQ4MC03MjBxLTMzIDAtNTYuNSAyMy41VDQwMC02NDBxMCAzMyAyMy41IDU2LjVUNDgwLTU2MFptMCAzNzRxMTIyLTExMiAxODEtMjAzLjVUNzIwLTU1MnEwLTEwOS02OS41LTE3OC41VDQ4MC04MDBxLTEwMSAwLTE3MC41IDY5LjVUMjQwLTU1MnEwIDcxIDU5IDE2Mi41VDQ4MC0xODZabTAgMTA2UTMxOS0yMTcgMjM5LjUtMzM0LjVUMTYwLTU1MnEwLTE1MCA5Ni41LTIzOVQ0ODAtODgwcTEyNyAwIDIyMy41IDg5VDgwMC01NTJxMCAxMDAtNzkuNSAyMTcuNVQ0ODAtODBabTAtNDgwWiIvPjwvc3ZnPgo=",
  iconWidth = 13, iconHeight = 13
)

school_icon <- makeIcon(
  iconUrl = "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIGhlaWdodD0iMjRweCIgdmlld0JveD0iMCAtOTYwIDk2MCA5NjAiIHdpZHRoPSIyNHB4IiBmaWxsPSIjZDM1NDAwIj48cGF0aCBkPSJNNDgwLTEyMCAyMDAtMjcydi0yNDBMNDAtNjAwbDQ0MC0yNDAgNDQwIDI0MHYzMjBoLTgwdi0yNzZsLTgwIDQ0djI0MEw0ODAtMTIwWm0wLTMzMiAyNzQtMTQ4LTI3NC0xNDgtMjc0IDE0OCAyNzQgMTQ4Wm0wIDI0MSAyMDAtMTA4di0xNTFMNDgwLTM2MCAyODAtNDcwdjE1MWwyMDAgMTA4Wm0wLTI0MVptMCA5MFptMCAwWiIvPjwvc3ZnPgo=",
  iconWidth = 13, iconHeight = 13
)

police_icon <- makeIcon(
  iconUrl = "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIGhlaWdodD0iMjRweCIgdmlld0JveD0iMCAtOTYwIDk2MCA5NjAiIHdpZHRoPSIyNHB4IiBmaWxsPSIjMWU4NDQ5Ij48cGF0aCBkPSJtMzY4LTMzNiAxMTItODQgMTEwIDg0LTQyLTEzNiAxMTItODhINTI0bC00NC0xMzYtNDQgMTM2SDMwMGwxMTAgODgtNDIgMTM2Wk00ODAtODBxLTEzOS0zNS0yMjkuNS0xNTkuNVQxNjAtNTE2di0yNDRsMzIwLTEyMCAzMjAgMTIwdjI0NHEwIDE1Mi05MC41IDI3Ni41VDQ4MC04MFptMC04NHExMDQtMzMgMTcyLTEzMnQ2OC0yMjB2LTE4OWwtMjQwLTkwLTI0MCA5MHYxODlxMCAxMjEgNjggMjIwdDE3MiAxMzJabTAtMzE2WiIvPjwvc3ZnPiAgCg==",
  iconWidth = 20, iconHeight = 20
)


# 5. Get distance between two points on a sphere -------------------------------
havershine_formula <- function(lat1, long1, lat2, long2) {                      #haversine formula using longitude and latitude
  radius <- 6371                                                                #Earth's radius in kilometers
  
  # convert long, lat to radians
  lat1 <- lat1 * pi / 180
  lon1 <- long1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- long2 * pi / 180
  
  # calculate difference between two points of long and lat
  dlat <- lat2 - lat1
  dlong <- lon2 - lon1
  
  # apply haversine formula
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlong / 2)^2               #calculates the square of the chord length 
  # between two points on a sphere
  
  # compute angular distance in radians
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  # get distance in km 
  distance <- radius * c                                                        #multiply by the Earth's radius
  return(distance)
}

# Preallocate columns for efficiency
offender <- offender %>% mutate(distance = NA)                                  #column to assign the distance to closest school
offender <- offender %>% mutate(closest = NA)                                   #column to assign the name of the closest school
#offender$police_dist <- NA                                                      #col to assign the distance to closest police station
#offender$closest_station <- NA                                                  #col to assign the name of the closest police station

# Iterate over each offender
for (i in 1:nrow(offender)) {
  # Calculate all distances from this offender to all schools
  distances <- sapply(1:nrow(schools), function(j) {
    havershine_formula(
      offender$LATITUDE[i],                                                     #offender lat
      offender$LONGITUDE[i],                                                    #offender long
      schools$LATITUDE[j],                                                      #school lat
      schools$LONGITUDE[j]                                                      #school long
    )
  })
  
  # Find the minimum distance and corresponding school name
  min_index <- which.min(distances)
  offender$distance[i] <- distances[min_index]                                  #closest distance to the school
  offender$closest[i] <- schools$NAME[min_index]                                #name of the closest school
}

# Verify the unique schools assigned
unique(offender$closest)



# 6. Leaflet map v.06 ----------------------------------------------------------

# Create sf objects for schools and offenders                                   
schools_sf <- st_as_sf(schools, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
offender_sf <- st_as_sf(offender, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Transform to a projected CRS for accurate distance measurements (meters)
schools_sf <- st_transform(schools_sf, crs = 3857)
offender_sf <- st_transform(offender_sf, crs = 3857)



# Merge offender data with schools to get coordinates of closest schools
schools_subset <- schools %>% dplyr::select(NAME, LATITUDE, LONGITUDE)
offender_with_school_coords <- offender %>%
  left_join(schools_subset, by = c('closest' = 'NAME'), 
            suffix = c('_offender', '_school')
  )

# Recalculate the distances between offenders and their closest schools
# Create sf points for offenders and schools
offender_points <- 
  st_as_sf(offender_with_school_coords, 
           coords = c("LONGITUDE_offender", "LATITUDE_offender"), crs = 4326)
school_points <- 
  st_as_sf(offender_with_school_coords, 
           coords = c("LONGITUDE_school", "LATITUDE_school"), crs = 4326)

# Transform to projected CRS
offender_points_proj <- st_transform(offender_points, crs = 3857)
school_points_proj <- st_transform(school_points, crs = 3857)

# Calculate distances in meters
distances_meters <- 
  st_distance(offender_points_proj, school_points_proj, by_element = TRUE)

# Add distances in miles to the data frame
offender_with_school_coords$distance_miles <- as.numeric(distances_meters) / 1609.34

# Create LINESTRING geometries for lines between offenders and closest schools
line_sf <- st_sfc(lapply(1:nrow(offender_with_school_coords), function(i) {
  st_linestring(matrix(
    c(
      offender_with_school_coords$LONGITUDE_offender[i], offender_with_school_coords$LATITUDE_offender[i],
      offender_with_school_coords$LONGITUDE_school[i], offender_with_school_coords$LATITUDE_school[i]
    ), ncol = 2, byrow = TRUE)
  )
}), crs = 4326)  # WGS84

line_sf <- st_sf(offender_with_school_coords, geometry = line_sf)


#function for caculating whether it is high or low risk

buffer_sizes <- seq(0.1,1.5,by = 0.1)
overall_risk <- data.frame()
for (i in buffer_sizes){  
  buffer_radius_meters <- i * 1609.34  
  schools_buffers <- st_buffer(schools_sf, dist = buffer_radius_meters)
  
  intersections <- st_intersects(schools_buffers, offender_sf)
  offender_counts <- sapply(intersections, length)
  
  max_count <- max(offender_counts)
  risk <- case_when(
    offender_counts <= max_count / 3 ~ 3,
    offender_counts <= max_count * 2 / 3 ~ 2,
    TRUE ~ 1)
  
  holder = data.frame(
    NAME = schools$NAME,
    buffer_size = i,
    risk_level = risk
  )
  overall_risk <- rbind(overall_risk, holder)
  
}
overall_risk <- overall_risk %>%
  group_by(NAME) %>%
  summarize(
    biggest_risk = round(mean(risk_level))
  )

# 7. Shiny App (Final Output)
# Define the user interface ----------------------------------------------------
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Merriweather:ital,wght@0,300;0,400;0,700;0,900;1,300;1,400;1,700;1,900&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      * {
        font-family: 'Merriweather', serif;
      }
    "))
  ),
  tags$div(
    class = "card", style = "margin-bottom: 20px;",
    tags$div(
      class = "card-header",
      tags$h5(
        tags$a(
          href = "#dearUserContent",
          `data-toggle` = "collapse",
          `aria-expanded` = "false",
          "Dear User, Please click here for project information!"
        )
      )
    ),tags$div(
      id = "dearUserContent", class = "collapse",
      tags$div(
        class= "card-body",
      
      tags$h3("Welcome to the Shiny App by Giselle Rahimi and Heewon Yang"),
      tags$p(
        "This app, created as part of a Computational and Data Sciences project at George Mason University, 
    explores the spatial relationships between sex offenders, elementary schools, and police stations in Washington, D.C."
      ),
      
      tags$h4("Features:"),
      tags$ul(
        tags$li(tags$b("Interactive Map:"), " Explore locations of Class B sex offenders, elementary schools, and district police stations.",
                tags$br(), "Buffers show the radius around each school. Buffer colors indicate the number of offenders (darker = more offenders).",
                tags$br(), "Hover or click on elements for details."),
        tags$li(tags$b("Risk Level Calculation:"), " Risk levels (1 = High, 2 = Moderate, 3 = Low) are dynamically calculated based on offender counts within 0.1–1.5 miles of each school."),
        tags$li(tags$b("Heatmap:"), " Visualize offender density and school locations."),
        tags$li(tags$b("Economic Choropleth Overlay:"), 
                " View offender density with a choropleth of median income by ward (2022 data from the American Community Survey).")
      ),
      
      tags$h4("Navigation Tips:"),
      tags$ul(
        tags$li("Use the right-side panel to select map layers (e.g., Buffers, Heatmap, Income)."),
        tags$li("Use the left-side panel to select a school and adjust buffer radius with the slider."),
        tags$li("Hover over the histogram for offender names.")
      ),
      
      tags$h4("Data Source:"),
      tags$p(
        "All data is sourced from ", 
        tags$a(href = "https://opendata.dc.gov", "Open Data DC"), ". For more information on offender classifications, visit ",
        tags$a(href = "https://mpdc.dc.gov/service/offender-classifications", "MPDC's offender classifications page"), "."
      )
    )
    )
    
  ),
  
  titlePanel("Type B Sex offenders, District Police Stations, and Elementary Schools in Washington D.C, 2023"),
  
  sidebarLayout(                                                                #design side panel
    sidebarPanel(
      helpText("George Mason University",
               tags$br(),
               tags$br(),
               "Authors: Giselle Rahimi and Heewon Yang."),
      helpText("Data Source: https://opendata.dc.gov/."),
      
      selectInput("map_style", "Map Style:",
                  c("Main Map", "Heatmap", "Economic Choropleth Overlay")),
      
      selectInput("school_selection", "Select a School:",
                  choices = c("",sort(unique(schools$NAME))),
                  selected = ""),
    
      sliderInput("buffer_size",
                "Buffer Size (miles):",
                min = 0.1,
                max = 1.5,
                value = 0.5,
                step = 0.1)),
    mainPanel(
    leafletOutput("final_map"), # the app
    br(),
    uiOutput("nearest_station"),
    textOutput("risk"),
    plotlyOutput("distance_histogram"),
    br(),
    tableOutput("distance_table"),
    #plotOutput("distance_chart"),
    #verbatimTextOutput("nearest_station")
    )
  )
    )

# server -----------------------------------------------------------------------
server <- function(input, output) {
  output$final_map <- renderLeaflet({                                           #define the final_map that will be rendered
    if (input$map_style == "Main Map") {                                        #if the user selects "Main Map"
      
      buffer_radius_meters <- input$buffer_size * 1609.34  # Convert miles to meters
      
      schools_buffers <- st_buffer(schools_sf, dist = buffer_radius_meters)
      
      # Count offenders within each buffer
      intersections <- st_intersects(schools_buffers, offender_sf)
      offender_counts <- sapply(intersections, length)
      schools_buffers$offender_count <- offender_counts
      
      # Transform buffers back to WGS84 for leaflet
      schools_buffers <- st_transform(schools_buffers, crs = 4326)
      
      palette <- colorNumeric(
        palette = c("#D9E8F6", "#1A3A5F"),
        domain = schools_buffers$offender_count
      )
      
      max_count = max(schools_buffers$offender_count)
      
      schools_buffers <- schools_buffers %>%
        mutate(
          risk = case_when(
            offender_count <= max_count/3~3,
            offender_count <= max_count * 2/3 ~ 2,
            TRUE ~ 1)
          )
      

      if (!is.null(input$school_selection)) {
        # Filter for the selected school
        selected_school <- schools %>% filter(NAME == input$school_selection)
        
        if (nrow(selected_school) > 0) {
          lng <- selected_school$LONGITUDE
          lat <- selected_school$LATITUDE
          zoom_level <- 15  # Zoom in closer to the selected school
        } else {
          lng <- mean(c(mean(offender$LONGITUDE), mean(police$LONGITUDE), mean(schools$LONGITUDE)))
          lat <- mean(c(mean(offender$LATITUDE), mean(police$LATITUDE), mean(schools$LATITUDE)))
          zoom_level <- 11  # Default zoom level
        }
      }
        
      final_map <- leaflet() %>%
        leaflet(options = leafletOptions(attributionControl = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%                        #set tiles of the leaflet map
        setView(
          lng = lng,
          lat = lat,
          zoom = zoom_level
        ) %>%
        addMarkers(
          data = offender,
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          icon = offender_icon,
          popup = ~offender_marker,
          group = "Sex Offenders"
        ) %>%
        addMarkers(
          data = schools,
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          icon = school_icon,
          popup = ~school_marker,
          group = "Schools"
        ) %>%
        addMarkers(
          data = police,
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          icon = police_icon,
          popup = ~station_marker,
          group = "Police Stations"
        ) %>%
      
      addLegend(
        position = "bottomright",
        colors = c("#1b4f72", "#d35400", "#1e8449"),  # Updated symbol colors
        labels = c("Sex Offenders", "Schools", "Police Stations"),  # Corresponding labels
        title = "Legend",
        opacity = 1
      )%>%
        addMapPane("polygons", zIndex = 410) %>%                                #use z-index to set layer order
        addMapPane("markers", zIndex = 420) %>%
        addMapPane("buffers", zIndex = 300) %>%
        addMapPane("offenders", zIndex = 400) %>%
        addMapPane("schools", zIndex = 420) %>%
        addPolylines(
          data = line_sf,                                                       #use line_sf to draw lines on the map
          color = "#546E7A",
          weight = 1,
          fillOpacity = 1,
          options = pathOptions(pane = "polygons"),
          group = "Lines"
        ) %>%

      addPolygons(
        data = schools_buffers,
        color = ~palette(offender_count),
        fillOpacity = 0.7,
        weight = 1,
        label = ~paste(
          "Offender count within ", input$buffer_size, " mile(s): ", offender_count, "Risk level", schools_buffers$risk
        ),
        group = "Buffers",
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
      ) %>%
        addLayersControl(                                                       #create selection pane for different layers
          overlayGroups = c("Sex Offenders", "Police Stations", "Schools", "Lines", "Buffers"),
          options = layersControlOptions(collapsed = FALSE)
        )#%>%
        #addControl("Type B Sex offender, Police Station, and Elementary School Locations", "bottomleft") #add title

    } else if (input$map_style == "Heatmap") {                                  #if the user selects "Heatmap"
      final_map <- leaflet() %>%
        leaflet(options = leafletOptions(attributionControl = FALSE)) %>%
        
        addProviderTiles(providers$CartoDB.Positron) %>%                        #add the offender circlemarkers as above
        setView(
          lng = mean(c(mean(offender$LONGITUDE),
                       mean(police$LONGITUDE),
                       mean(schools$LONGITUDE))),
          lat = mean(c(mean(offender$LATITUDE),
                       mean(police$LATITUDE),
                       mean(schools$LATITUDE))),
          zoom = 11
        ) %>%
        addMarkers(
          data = offender,
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          icon = offender_icon,
          popup = ~offender_marker,
          group = "Sex Offenders"
        ) %>% 
        addHeatmap(                                                             #use addheatmap to create a heatmap of offender density
          data = offender,
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          blur = 5,
          max = 0.02,
          radius = 15,
          group = "Heatmap"                                                     #set group to heatmap
        ) %>%
        addMarkers(
          data = schools,
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          icon = school_icon,
          popup = ~school_marker,
          group = "Schools"
        ) %>% 
        addLegend(                                                              #create color legend
          "bottomright",
          colors = c("#1b4f72", "#d35400"),
          labels = c("Sex offenders", "Schools"),
          title = "Legend"
        ) %>%
        addLayersControl(                                                       #create layer selection pane
          overlayGroups = c("Sex Offenders", "Heatmap", "Schools"),
          options = layersControlOptions(collapsed = FALSE)
          
        )
    }else{
      
      final_map <- leaflet() %>%
        leaflet(options = leafletOptions(attributionControl = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%                        #add the offender circlemarkers as above
        setView(
          lng = mean(c(mean(offender$LONGITUDE),
                       mean(police$LONGITUDE),
                       mean(schools$LONGITUDE))),
          lat = mean(c(mean(offender$LATITUDE),
                       mean(police$LATITUDE),
                       mean(schools$LATITUDE))),
          zoom = 11
        ) %>%
        addMapPane("income",zIndex = 300)%>%                                    #use z index to adjust layers
        addPolygons(
          data = shape,                                                         #use the shape sf dataset for income by ward
          fillColor = ~palette(income),                                         #use the palette vector to color choropleth
          fillOpacity = 0.5,
          color = "black",
          weight = 1,
          options = pathOptions(pane = 'income'),
          label = ~paste(                                                      
            "The average income of ", NAMELSAD, "from 2017 to 2022 was $", income
          ),
          group = "Income"                                                    
          
        ) %>% 
        addHeatmap(                                                             
          
          data = offender,
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          blur = 5,
          max = 0.02,
          radius = 15,
          group = "Heatmap"
        )%>%
        addMarkers(
          data = offender,
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          icon = offender_icon,
          popup = ~offender_marker,
          group = "Sex Offenders"
        ) %>% 
        addLayersControl(
          
          overlayGroups = c("Income", "Heatmap","Sex Offenders"),
          options = layersControlOptions(collapsed = FALSE))
    }
    
    
  })
  
  output$distance_histogram <- renderPlotly({
    # Check for school selection
    
    validate(
      need(input$school_selection != "", "Please select a school to display the histogram.")
    )    
    # Filter the selected school
    selected_school <- schools %>%
      filter(NAME == input$school_selection)
    
    # Calculate distances using havershine_formula
    distances <- sapply(1:nrow(offender), function(i) {
      havershine_formula(
        selected_school$LATITUDE,
        selected_school$LONGITUDE,
        offender$LATITUDE[i],
        offender$LONGITUDE[i]
      )
    })
    
    # Convert distances to miles and filter for the desired range
    distances <- distances * 0.621371
    distance_data <- data.frame(
      Distance = distances,
      name = paste(offender$FIRSTNAME, offender$LASTNAME)
    ) %>%
      filter(Distance <= 5)
    
    # Bin the distances into intervals
    # distance_data <- distance_data %>%
    #   mutate(bin = cut(Distance, breaks = seq(0, 1.2, by = 0.1), include.lowest = TRUE))
    # 
    # Plot the histogram using ggplot2
    p <- ggplot(distance_data, aes(x = Distance, text = name)) +
      geom_histogram(fill = "#C3C3C3", binwidth = 0.5) +
      scale_x_continuous(limits = c(0,5),breaks = seq(0, 5, by = 0.3))+
      labs(
        title = "Offender Distances within 5 Miles",
        x = "Miles",
        y = "Offenders and identities"
      ) +
      theme_minimal()
    
    # Convert to interactive plot using ggplotly
    ggplotly(p, tooltip = "text")
  })
  

  # output$distance_table <- renderTable({
  #   selected <- schools %>% filter(NAME == input$school_selection)
  #   distances <- sapply(1:nrow(offender), function(i) {
  #     havershine_formula(
  #       selected$LATITUDE,
  #       selected$LONGITUDE,
  #       offender$LATITUDE[i],
  #       offender$LONGITUDE[i]
  #     )
  #   })
  #   distances = distances * 0.621371
  # 
  #   distance_data <- data.frame(Distance = distances)
  # 
  #   # Return the dataframe to display as a table
  #   min(distance_data$Distance)
  #   distance_data
  # })

output$nearest_station <- renderText({
  req(input$school_selection)
  validate(
    need(input$school_selection != "", "Please select a school to find the nearest police station.")
  )
  selected <- schools %>% filter(NAME == input$school_selection)
  if (nrow(selected) == 0) return(HTML("School not found. Please select a valid school."))
  


  distances <- sapply(1:nrow(police), function(i) {
    havershine_formula(
      selected$LATITUDE,
      selected$LONGITUDE,
      police$LATITUDE[i],
      police$LONGITUDE[i]
    )
  })
  distances = distances * 0.621371
  min_index <- which.min(distances)
  #nearest_station <- police$NAME[min_index]
  
  
  nearest_police <- police %>% filter(NAME == NAME[min_index])
  HTML(paste(
    "The closest Police Station to <b>", input$school_selection, "</b>:<br>",
    "Name: ", nearest_police$NAME, "<br>",
    "Address: ", nearest_police$ADDRESS, "<br>",
    "Phone Number: ", nearest_police$PHONE, "<br>",
    "Contact Person: ", nearest_police$CONTACT
  ))
})
output$risk <- renderText({
  req(input$school_selection)
  
  selected_risk <- isolate({overall_risk %>%
      filter(NAME == input$school_selection) %>%
      pull(biggest_risk)})
  paste(input$school_selection, "has the risk level of", selected_risk)
  
})

}
shinyApp(ui, server)





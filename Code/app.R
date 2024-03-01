#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(shinythemes)
library(tidyr)
library(dplyr)
library(janitor)
library(stringr)
library(DT)
library(leaflet)
library(htmltools)

# Start define rowCallback function
# This is to allow NAs to appear in datatables
# see https://stackoverflow.com/a/58526580/5593458
rowCallback <- c(
  "function(row, data){",
  "  for(var i=0; i<data.length; i++){",
  "    if(data[i] === null){",
  "      $('td:eq('+i+')', row).html('NA')",
  "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
  "    }",
  "  }",
  "}"
)
# End define rowCallback function

# Read in dataset
sites <- read_tsv(file = "https://raw.githubusercontent.com/norlab/ameriflux-analysis/main/Data/AmeriFlux-sites-202401161213.tsv", show_col_types = FALSE)

# Filter to just sites in Canada
sites <- sites |> 
  filter(Country == 'Canada') 

# Add some new columns
sites <- sites |> 
  mutate(first_year_of_data = str_split(`Years of AmeriFlux BASE Data`, ",", simplify = TRUE)[ , 1]) |> 
  mutate(last_year_of_data = str_split(`Years of AmeriFlux BASE Data`, ",", simplify = TRUE)[ , -1])


# Assign variables for use within a text block
total_sites <- sites |> 
  summarise(x = n_distinct(`Site ID`)) |> 
  pull()

total_sites_no_end <- sites |> 
  filter(is.na(`Site End`) | as.numeric(`Site End`) > 2023) |> 
  summarise(x = n_distinct(`Site ID`)) |> 
  pull()

total_pi <- sites |> 
  summarise(x = n_distinct(`Principal Investigator`)) |> 
  pull()

total_pi_no_end <- sites |> 
  filter(is.na(`Site End`) | as.numeric(`Site End`) > 2023) |> 
  summarise(x = n_distinct(`Principal Investigator`)) |> 
  pull()

# Define the labels for the leaflet map
# See https://stackoverflow.com/a/43155126
labs <- lapply(seq(nrow(sites)), function(i) {
  paste0('<b>Site ID:</b> ', 
          sites[i, "Site ID"], 
          '<br><b>Site Name:</b> ', 
          sites[i, "Name"],
          '<br><b>PI:</b> ', 
          sites[i, "Principal Investigator"],
          '<br><b>Site Start Year:</b> ', 
          sites[i, "Site Start"], 
          '<br><b>Site End Year:</b> ', 
          sites[i, "Site End"]) 
})

# Define UI for application

ui <- fluidPage(theme = shinytheme("flatly"),

    # Application title
    titlePanel("Canadian Eddy-Covariance Research Sites Registered on AmeriFlux"),

        mainPanel(fluidRow(column(12,
                                  h2("Summary"),
                                  
        )
        ),# End fluidrow 
        fluidRow(br()),# End fluidrow 
        fluidRow(column(12,
                        p(paste0("As of January 16, 2024, AmeriFlux listed ", total_sites, " unique Eddy-Covariance research sites across Canada associated with ", total_pi, " Principal Investigators. If we exclude research sites that have ended, there are currently ", total_sites_no_end, " active Canadian research sites associated with ", total_pi_no_end, " Principal Investigators.")),
                        
        )
        ),# End fluidrow 
                 fluidRow(column(8,
                                 h2("Map View")
                 ),
                 column(4)
                 ), # End fluidrow
                 fluidRow(column(12, 
                                 wellPanel(
                                   leafletOutput(outputId = "Map")
                                 )
                 ) # End wellPanel
                 ), # End fluidrow
                 fluidRow(column(8,
                                 h2("Table View")
                 ),
                 column(4)
                 ), # End fluidrow
                 fluidRow(column(12,
                                 DT::dataTableOutput(outputId = "Ameriflux_Table")
                 )
                 ) # End fluidRow
        ),
        # End National Dashboard tabPanel
    
    # Start footer
    fluidRow(br()),
    fluidRow(column(12,
                    tags$footer(
                      p(
                        "The data presented here were gathered from the ",
                        tags$a(href = 'https://ameriflux.lbl.gov/sites/site-search/#', 'AmeriFlux Site Search'),
                        " table on January 16, 2024."
                      ), 
                      class = "footer"
                    )
    )
    )
    # End footer
)


# Define server logic required 
server <- function(input, output) {
    
    # Begin Create NY5Z map
    # Disabled scroll wheel option. Instuctions a combo from https://gis.stackexchange.com/a/54925
    # and https://gis.stackexchange.com/a/231632
    output$Map <- renderLeaflet({
      
      leaflet(sites %>% 
                distinct(`Site ID`, Name, `Longitude (degrees)`, `Latitude (degrees)`),
              options = leafletOptions(scrollWheelZoom = FALSE)) %>% 
        addTiles() %>%
        addMarkers(lng = ~`Longitude (degrees)`, 
                   lat = ~`Latitude (degrees)`, 
                   label = lapply(labs, htmltools::HTML),
                   clusterOptions = markerClusterOptions()
                   )
      
    })
    # End Create NY5Z map
    
    # Start create Ameriflux_Table table
    output$Ameriflux_Table <- DT::renderDataTable({
      Ameriflux_Table_DT_object <- sites %>% 
        select(`Site ID`, Name, `Principal Investigator`, `Site Start`, `Site End`, `AmeriFlux BASE Data`, `AmeriFlux FLUXNET Data`)
      # DT info: https://datatables.net/reference/option/dom https://rstudio.github.io/DT/
      datatable(
        Ameriflux_Table_DT_object,
        #caption = 'E-Rate commitments made to Libraries and Library Systems including those applying as part of a Consortium or as a Non-Instructional Facility. State populations are retrieved from the US Census Bureau Population Estimates API and are specific to the year chosen. There are currently no 2021 or 2022 state population estimates and thus no 2021 or 2022 per capita commitments.',
        rownames = F,
        # bootstrap style must be added when using a shinytheme https://stackoverflow.com/a/60948767
        style = "bootstrap",
        # colnames = c(
        #   'State',
        #   'Category 1 Commitment',
        #   'Category 2 Commitment',
        #   'State Total',
        #   paste0('Pct of Total ', year_pick(), ' Library Commitments'),
        #   'Avg Per Entity Commitment',
        #   'Avg Per Capita Commitment',
        #   'Avg Per Square Foot Commitment'
        #),
        options = list(dom = 'lftipr', rowCallback = JS(rowCallback)) # This adds in null values
       ) #%>%
      #   formatCurrency(
      #     columns = c(
      #       'Category1_Commitment',
      #       'Category2_Commitment',
      #       'State_Total',
      #       'Per_Entity_Discount',
      #       'Per_Capita_Discount',
      #       'Per_SqFt_Discount'
      #     ),
      #     currency = "$",
      #     interval = 3,
      #     mark = ","
      #   ) %>%
      #   formatPercentage("Pct_of_Lib_Funding", 2)
    })
    # End create Ameriflux_Table table
}

# Run the application 
shinyApp(ui = ui, server = server)

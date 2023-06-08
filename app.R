library(shiny)
library(tidyverse)
library(maps)
library(mapproj)
library(sf)
library(rsconnect)
library(tidycensus)
library(tigris)
source("data/helpers.R")

# Create Dominicans data
dominicans_county <- get_acs(
  geography = "county",
  variables = c(Dominicans = "B03001_007"),
  geometry = TRUE,
  year = 2020,
  resolution = "20m",
  progress_bar = FALSE
) %>%
  mutate(
    "county" = NAME
  ) %>% 
  shift_geometry()

names(dominicans_county)

dominicans_state <- get_acs(
  geography = "state",
  variables = c(Dominicans = "B03001_007"),
  geometry = TRUE,
  year = 2020,
  resolution = "20m",
  progress_bar = FALSE
) %>%
  mutate(
    state = str_to_title(NAME),
    state = ifelse(state == "District Of Columbia", "District of Columbia", state)
  ) %>% 
  select(state)

# list of state choices 
state_names <- unique(dominicans_state$state) %>% 
str_sort()



#  User interface ---------------------------------- 
ui <- fluidPage(
  
  # Application name
  titlePanel("Dominican Population by State"),
  
  #Sidebar
  sidebarLayout(
    position = "left",
    
    sidebarPanel(
      
      # text above widget
      helpText("Create demographic maps with information from the 2020 US Census."),
      
      # Widget/dropdown menu for selecting state
      selectInput(
        inputId = "state",
        label = "Choose a state to display",
        choices = str_sort(state_names),
        selected = "New York"
      ),
      
    ),
    
    # return selections
    mainPanel(
      plotOutput("plot") 
    )
    
  ),
  
)


# Server logic --------------------------

server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    # subset data for selected state
    newdata <- dominicans_county %>% 
      mutate(
        "state" = str_extract(county, 
                            paste(state_names, 
                                  collapse = "|"))
        ) %>% 
      filter(state == input$state)
               

    
    # make map
    ggplot(data = newdata,
           aes(fill = estimate)) +
      geom_sf(
        show.legend = TRUE
      ) +
      scale_fill_gradient2(limits = c(0, 10000),
                           breaks = c(0, 5000, 10000),
                           labels = c("0", "5,000", " > 10,000"),
                           oob = scales::squish, 
                           high = "maroon",
                           low = "white",
                           mid = "darkblue",
                           midpoint = 5000
      ) +
      labs(
        subtitle = "2020 ACS 5-Year Estimates",
        caption = "Source: U.S. Census Bureau",
        fill = "Dominicans by county") +
      theme_void() +
      ggtitle(input$state) +
      theme(plot.title = element_text(size = 20,
                                      face = "bold"),
            legend.text = element_text(size = 10),
            legend.title = element_text(vjust = 5,
                                        face = "bold",
                                        size = 12))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

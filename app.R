# Brittany Russell
# March 2021
# R Shiny app with C19 data

library(shiny) # shiny app
library(dplyr) 
library(readr)
library(ggplot2) # plotting
library(magrittr) # for %<>%
library(zoo) # rollmean() for rolling avg

load("all_dat.RData")
###################
# My plotting app #
# Basic:
  # Select county from list
  # Output graphs of cases and deaths
  # select date range
# Intermediate:
  # add state and nation (add up cases and deaths across counties)
  # include more dependent vars (total vs daily vs daily change) in separate graphs
# Advanced
  # add significant events (holidays, government policies)
# Very advanced:
  # compare two places
  # incorporate model to estimate effects of policies
    # Need to decide what model, which policies, dependent vars
####################


## Part 1 ##
# Define UI for dataset viewer app ----
ui <- fluidPage(
  titlePanel(h1("Visualize the COVID-19 Pandemic")),

  sidebarLayout(position = "right",
                sidebarPanel("",
                             selectizeInput("my_state",
                                            label = "Select state/province:",
                                            choices = NULL),
                             uiOutput("my_county"),
                             dateRangeInput("date_range",
                                            label = "Select date range:",
                                            start = "2020-01-22",
                                            end = max(all_dat$date),
                                            min = "2020-01-22",
                                            max = max(all_dat$date),
                                            format = "m/dd/yy")),
                
                mainPanel(h4("Data from the COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University", align = "center"),
                          plotOutput("cases_plot"),
                          plotOutput("daily_cases_plot"),
                          plotOutput("deaths_plot"),
                          plotOutput("daily_deaths_plot"))
                )
  )

## Part 2 ##
# Define server logic to summarize and view selected dataset ---- 
server <- function(input, output, session) {

  # state selection
  updateSelectizeInput(session, "my_state", 
                       choices = sort(unique(all_dat$Province_State)), 
                       selected = "Alabama", server = TRUE)

  # reactive county selection
  county_options <- reactive({
    unique(all_dat$Combined_Key[all_dat$Province_State == input$my_state & !is.na(all_dat$Combined_Key)])
  })

  # county selection -- needs to depend on state
  output$my_county <- renderUI({
    counties_reactive <- county_options()
    selectizeInput("my_county", 
                   label = "Select county:",
                   choices = counties_reactive,
                   selected = counties_reactive[1])
  })
  
  # Make plots
  plot_input <- reactive({
    state_choice <- input$my_state
    county_choice <- input$my_county
    all_dat %>% filter(Type == "Nation" | (Type == "State" & Province_State == state_choice) | (Type == "County" & Combined_Key == county_choice)) %>%
      filter(date > input$date_range[1] & date < input$date_range[2])
  })
  
  output$cases_plot <- renderPlot({
    plot_data <- plot_input() 
    subset(plot_data, !is.na(cases_pc)) %>%
      ggplot(mapping = aes(x = date, y = cases_pc, group = Type)) + 
      geom_line(aes(col = Type)) + 
      labs(title = "Cumulative COVID-19 Cases", x = "Date", y = "total cases per 1000")
        
  })
  
  output$daily_cases_plot <- renderPlot({
    plot_data <- plot_input() 
    subset(plot_data, !is.na(rolling_daily_cases)) %>%
      ggplot(mapping = aes(x = date, y = rolling_daily_cases, group = Type)) + 
      geom_line(aes(col = Type)) + 
      labs(title = "Rolling Average of Daily COVID-19 Cases", x = "Date", y = "daily cases per 1000, 7-day rolling avg")
  })
  
  output$deaths_plot <- renderPlot({
    plot_data <- plot_input() 
    subset(plot_data, !is.na(deaths_pc)) %>%
      ggplot(mapping = aes(x = date, y = deaths_pc, group = Type)) + 
      geom_line(aes(col = Type)) + 
      labs(title = "Cumulative COVID-19 Deaths", x = "Date", y = "total deaths per 1000")
  })
  
  output$daily_deaths_plot <- renderPlot({
    plot_data <- plot_input() 
    subset(plot_data, !is.na(rolling_daily_deaths)) %>%
      ggplot(mapping = aes(x = date, y = rolling_daily_deaths, group = Type)) + 
      geom_line(aes(col = Type)) + 
      labs(title = "Rolling Average of Daily COVID-19 Deaths", x = "Date", y = "daily deaths per 1000, 7-day rolling avg")
  })
}

## Part 3 ##
# Create Shiny app
shinyApp(ui = ui, server = server)






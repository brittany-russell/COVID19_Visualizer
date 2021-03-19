# Brittany Russell
# March 2021
# R Shiny app with C19 data

library(shiny) # shiny app
library(dplyr) 
library(readr)
library(ggplot2) # plotting
library(magrittr) # for %<>%
library(zoo) # rollmean() for rolling avg

#load("plot_data/all_dat.RData")
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

# Data from CSSEGISandData (Johns Hopkins University)
confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
confirmed_long <- confirmed %>% gather("date", "cases", 12:ncol(confirmed))
confirmed_long$date <- parse_date(confirmed_long$date, format = "%m/%d/%y")

deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
deaths_long <- deaths %>% gather("date", "deaths", 13:ncol(deaths))
deaths_long$date <- parse_date(deaths_long$date, format = "%m/%d/%y")

# combined data set
long_dat <- deaths_long
long_dat$cases <- confirmed_long$cases
# remove all data with population of zero -- that doesn't seem useful
long_dat %<>% filter(Population != 0) # removes approx 47,000 obs
# per capita deaths and cases (per 1000)
long_dat %<>% mutate(cases_pc = 1000 * cases/Population, deaths_pc = 1000 * deaths/Population)
# time variable
long_dat %<>% mutate(days = date - min(date))
# might need to turn this numeric? now it's difftime

# More dependent vars
long_dat %<>% arrange(Combined_Key)
long_dat %<>% group_by(Combined_Key) %>% 
  mutate(daily_cases = cases - lag(cases), daily_deaths = deaths - lag(deaths)) %>%
  mutate(daily_cases_pc = 1000 * daily_cases/Population, daily_deaths_pc = 1000 * daily_deaths/Population) %>%
  add_column("Type" = "County")

# State data set (add up numbers across state)
state_dat <- long_dat %>% group_by(Province_State, date) %>% 
  summarize(cases = sum(cases), deaths = sum(deaths), Population = sum(Population)) %>%
  mutate(cases_pc = 1000 * cases/Population, deaths_pc = 1000 * deaths/Population) %>%
  mutate(daily_cases = cases - lag(cases), daily_deaths = deaths - lag(deaths)) %>%
  mutate(daily_cases_pc = 1000 * daily_cases/Population, daily_deaths_pc = 1000 * daily_deaths/Population) %>%
  add_column("Type" = "State")

# National numbers
nation_dat <- long_dat %>% group_by(date) %>% 
  summarize(cases = sum(cases), deaths = sum(deaths), Population = sum(Population)) %>%
  mutate(cases_pc = 1000 * cases/Population, deaths_pc = 1000 * deaths/Population) %>%
  mutate(daily_cases = cases - lag(cases), daily_deaths = deaths - lag(deaths)) %>%
  mutate(daily_cases_pc = 1000 * daily_cases/Population, daily_deaths_pc = 1000 *daily_deaths/Population) %>%
  add_column("Type" = "Nation")

## Daily cases and daily change are really jagged -- 7-day rolling avg is probably better visually
long_dat %<>% mutate(rolling_daily_cases = rollmean(daily_cases_pc, k = 7, fill = NA), rolling_daily_deaths = rollmean(daily_deaths_pc, k = 7, fill = NA))
state_dat %<>% mutate(rolling_daily_cases = rollmean(daily_cases_pc, k = 7, fill = NA), rolling_daily_deaths = rollmean(daily_deaths_pc, k = 7, fill = NA))
nation_dat %<>% mutate(rolling_daily_cases = rollmean(daily_cases_pc, k = 7, fill = NA), rolling_daily_deaths = rollmean(daily_deaths_pc, k = 7, fill = NA))

# Plotting national and state together
all_dat <- bind_rows(long_dat, state_dat, nation_dat)
all_dat$Type <- factor(all_dat$Type, levels = c("Nation", "State", "County"))


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






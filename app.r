library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(RCurl)
library(zoo)

#Data for county data plots
us_counties = read.csv(text = getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))
us_counties$date = as.Date(us_counties$date)
us_counties = us_counties[order(us_counties$county),] 

#world data for country by country cases and deaths
world_wide = read.csv(text = getURL("https://raw.githubusercontent.com/datasets/covid-19/master/data/countries-aggregated.csv"))
world_wide$Date = as.Date(world_wide$Date)
world_filt = world_wide %>%
  select("Date", "Country", "Confirmed", "Deaths") %>%
  filter(Country %in% c("Sweden", "US", "Belgium", "Canada", "New Zealand", "Brazil", "India", "Belgiam", "Italy", "Germany", "France", "United Kingdom")) %>%
  mutate(daily_cases = Confirmed - lag(Confirmed, default = first(Confirmed))) %>%
  mutate(daily_deaths = Deaths - lag(Deaths, default = first(Deaths))) %>%
  arrange("Date")

world_filt <- data.frame(lapply(world_filt, function(x) {
  gsub("US", "United States", x)
}))

#US Deaths- maybe make this filterable likek the county plots in the future
us_data = world_filt %>%
  filter(world_filt$Country == "United States")

us_data$Date = as.Date(us_data$Date)
us_data$daily_cases = as.numeric(us_data$daily_cases)
us_data$daily_deaths = as.numeric(us_data$daily_deaths)

pop_data = read.table("population.csv", stringsAsFactors = F,sep = ",", header=T)
pop_data_2018 =pop_data %>%
  filter(Year == 2018)

median_ages = read.table("median_age.txt", sep = "\t", header =F)
names(median_ages) = c("Line", "Country", "Median_Age")

all_data = merge(pop_data_2018,median_ages, by.x = 'Country.Name', by.y='Country')

all_pop_covid_data = merge(all_data,world_filt, by.x = 'Country.Name', by.y='Country')
all_pop_covid_data$Value = as.numeric(all_pop_covid_data$Value)
all_pop_covid_data$Confirmed = as.numeric(all_pop_covid_data$Confirmed)
all_pop_covid_data$Date = as.Date(all_pop_covid_data$Date)
all_pop_covid_data$Deaths = as.numeric(all_pop_covid_data$Deaths)

# Define UI for covid by county app 
ui <- navbarPage(theme = shinytheme("darkly"), 
      "Covid Tracking",
      tabPanel(
      "County Tracking",

      pageWithSidebar(
  
      # App title ----
      headerPanel("Covid-19 Daily Cases per County"),
  
      # Sidebar panel for inputs ----
      sidebarPanel(
    
        #State selector
        selectInput("state", "State:", 
                choices = sort(unique(us_counties$state))),
    
        #county selector
        selectInput("county", "County:", 
                choices = NULL)
    
      ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    # Output: Formatted text for caption ----
    h3(textOutput("caption")),
    
    plotOutput("countyPlot"),
   
    tagList("Data Source:",a("New York Times GitHub", href="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))
  )
 )
),
tabPanel("World Cases",
         plotOutput("worldCases")),
tabPanel("World Deaths",
         plotOutput("worldDeaths")),
tabPanel("US Deaths",
         plotOutput("usDeaths"))
)

# Define server logic to plot cases by county
server <- function(session,input, output) {
   
  observe({
    counties_in_state <- us_counties %>% 
    filter(state == input$state) %>% 
    select(county)
    updateSelectInput(session, "county", "County:", choices = unique(counties_in_state))
  })
  
  filtered_data<-reactive({
    us_counties %>% 
      filter(state==input$state, county==input$county) %>%
      arrange("date") %>%
      mutate(daily_cases = cases - lag(cases, default = first(cases))) %>%
      mutate(rollavg=rollapply(daily_cases,7,mean, align='right',fill=NA))
      
  })
  
  # Compute the formula text ----
  formulaText <- reactive({
    paste("Cases in", input$county, "county", input$state)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of daily cases by county
  output$countyPlot <- renderPlot({
    ggplot(filtered_data(), aes(x=date,y = daily_cases)) + 
      geom_col(fill = "dark blue") +
      xlab("Date") +
      ylab("New Cases") +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
      geom_line(data=filtered_data(), aes(x=date, y=rollavg), colour="red", size = 1.3) 
  })
  
  #World Death plot
  output$worldDeaths <- renderPlot({
    ggplot(data = all_pop_covid_data, aes(x =Date, y = Deaths/Value, col = Country.Name)) +
      geom_line(size = 1.2) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
  })
  
  #world cases plot
  output$worldCases <- renderPlot({
    ggplot(data = all_pop_covid_data, aes(x = Date, y = Confirmed/Value*100,col = Country.Name))+
      geom_line(size = 1.2) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
  })
  
  #US daily deaths
  output$usDeaths <- renderPlot({
  ggplot(data=us_data, aes(x=Date,y = daily_deaths/daily_cases)) + 
    geom_col(fill = "dark blue") +
    xlab("Date") +
    ylab("Death Rate") +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
  })
  
    }
  
shinyApp(ui, server)
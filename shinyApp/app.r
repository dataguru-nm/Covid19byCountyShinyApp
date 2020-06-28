library(shiny)
library(ggplot2)
library(dplyr)
library(RCurl)
library(zoo)

us_counties = read.csv(text = getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))
us_counties$date = as.Date(us_counties$date)
us_counties = us_counties[order(us_counties$county),] 

# Define UI for covid by county app 
ui <- pageWithSidebar(
  
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
    #HTML(
     ##     "<p>New York Times github<a href='https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'></a>!</p>"
    #  )
    tagList("Data Source:",a("New York Times GitHub", href="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))
  )
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
  
}

shinyApp(ui, server)
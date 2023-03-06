library(shiny)
library(tidyverse)
library(plotly)
library(ggplot2)

data <- read.csv("world_population.csv")

ui <- navbarPage("World Population",
                 tabPanel("Overview",
                          p("This app uses world population data from the " ,
                            em("UN and US census"),
                            p("The data set contains ", strong(nrow(data)), "rows and  ", strong(ncol(data)), "cols"),
                          ),
                          p("Here is a small random sample dataset: "),
                          mainPanel(
                            dataTableOutput("sample")
                          ),
                 ),
                 
                 tabPanel("Population Trend",
                          sidebarLayout(
                            
                            
                            tabPanel("Countries with the least amount of population",
                                     sidebarLayout(
                                       
                                       
                                       sidebarPanel(
                                         selectInput("continent", "Select continent:", 
                                                     choices = unique(data$Continent), selected = "Africa"),
                                         
                                         radioButtons("color", "color blindmode",
                                                      choices = c("yellow", "red", "blue", "purple"))
                                         
                                       ),
                                       
                                       # Main panel for displaying outputs ----
                                       mainPanel(
                                         plotlyOutput(outputId = "plot2"),
                                         textOutput("var")
                                       )
                                     )
                                     
                            ),
                            
                            
                            tabPanel("Summary")
                            
                          )
                 ),
                 tabPanel("DataTable",
                          titlePanel("World Population"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("continent2", "Select a continent", choices = unique(data$Continent), selected = "Asia"),
                              textOutput("averagegrowthrate")
                              
                            ),
                            
                            mainPanel(
                              tableOutput("info"),
                              
                            )
                          )
                 )       
)
# Define server logic required to draw a histogram
server <- function (session,input, output) {
  output$sample <- renderDataTable({
    data %>%
      sample_n(5)
  })
  output$var <- renderText({
    totalsum  <- data %>% filter(Continent == input$continent) %>%
      summarise(sumcountry = length(unique(Country))) %>%
      pull(sumcountry)
    paste("In this continent, there are a total of ", totalsum, "countries")
  })
  output$plot2<- renderPlotly({
    data %>% filter(Continent==input$continent) %>%
      arrange(desc(population_2022)) %>% tail(10) %>%
      ggplot(aes(x=reorder(Country,population_2022) , y=population_2022)) +
      geom_col(col=input$color) +
      labs(x="Country", y="Population", title=paste0("Countries with the least amount of population:  ", input$contient)) +
      coord_flip()
  })
  output$info <- renderTable({
    filtered <- data %>% filter(Continent == input$continent2) %>%
      select(Country,Continent,population_2022,Growth_Rate)
  })
  output$averagegrowthrate <- renderText({
    averageGR <- data %>% filter(Continent == input$continent2) %>%
      summarise(meanGR = mean(Growth_Rate)) %>%
      pull(meanGR)
    paste("In this continent, the total mean Growth Rate is ", averageGR)
  })
  
  
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)
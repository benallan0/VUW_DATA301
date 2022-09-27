# Ben Allan DATA301 Assignment 2
library(ggplot2)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(scales)
library(hrbrthemes)
library(viridis)
library(shiny)


anz <- read.csv("ANZ_Premiership_2017_2022.csv")
anz1 <- read.csv("ANZ_Premiership_2017_2022.csv")

str(anz)

tteams <- c("Southern Steel", "Central Pulse", "Northern Mystics", "Waikato Bay of Plenty Magic", "Northern Stars", "Mainland Tactix")
anz <- tidyr::gather(anz, key="winlossdraw", value="count", 5:7)


head(anz$Team)
##################
#Bare bones shiny app
##################

unique(anz$Team)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selector",
                         "Select a team",
                         choices = tteams,
                         selected= tteams),
      sliderInput("years", "Choose years", sep="",
                  min=2017, max=2022, value=c(2017,2022))
      
    ),
    mainPanel(
      h2("ANZ Chart Tabs"),
      tabsetPanel(
        tabPanel("Stacked Barplot", plotOutput("WinLose")),
        tabPanel("Standings", plotOutput("TotalPoints")),
        tabPanel("Goals for V Goals against", plotOutput("Bubble"))
        
      )
    )
  )
)

server <- function(input, output) {
  
  filterteam <- reactive({
    df.selection <- filter(anz, Team %in% input$selector, Year %in% (input$years[1]:input$years[2])) 
  }) 
  
  filterbubble <- reactive({
    df.selection <- filter(anz1, Team %in% input$selector, Year %in% (input$years[1]:input$years[2])) 
  }) 
  
  
  
  
  #Q1A
  output$WinLose <- renderPlot({
    filterteam() %>% 
      ggplot() + geom_bar(aes(x = Team, y= count, fill=winlossdraw), stat = "identity")+ coord_flip()
    
  })
  
  
  #Q1B/#2A
  output$TotalPoints <- renderPlot({
    filterteam() %>% 
      ggplot(aes(x=Year, y=Pts))+
      geom_line(aes(color=Team))+
      xlab("Year")+
      ylab("Total Points")
    
  })
  
  
  #Q2B pre much done?
  output$Bubble <- renderPlot({
    filterbubble() %>% 
      ggplot(aes(x=GF, y=GA, size = W, fill=Team, color=Team))+
      geom_point(alpha=0.6,shape=20)+
      scale_size(range = c(.1, 14),name="Number of Wins")+
      scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
      theme_ipsum() +
      theme(legend.position="bottom") +
      ylab("Goals For") +
      xlab("Goals Against")
  })
  
}


shinyApp(ui = ui, server = server)


#Changes 

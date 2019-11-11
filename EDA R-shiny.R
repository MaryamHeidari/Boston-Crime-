#install.packages("shiny")
library(shiny)
library(ggplot2)
df <- read.csv(file.choose(), stringsAsFactors = FALSE)
df <- subset(df, select = c(OFFENSE_DESCRIPTION,OFFENSE_CODE_GROUP,
                            DISTRICT, OCCURRED_ON_DATE,OCCURRED_ON_DATE,
                            YEAR,MONTH,DAY_OF_WEEK,HOUR,
                            UCR_PART, STREET))
df <- filter(df, UCR_PART != "")
df <- filter(df,  DISTRICT != "")
data <- unique(df)
head(data, n=2)
#Shiny Example 1: Histogram where you control the number of bins
ui<-shinyUI(fluidPage(
  
  #fluid page for dynamically adapting to screens of different resolutions.
  titlePanel("Dynamic Plot"),
  sidebarLayout(
    sidebarPanel(
      #implementing radio buttons
      radioButtons("x", "Select Time Categories:",
                   list("YEAR"='a', "MONTH"='b', "DAY_OF_WEEK"='c', "HOUR"='d'))
      
      # Show a plot of the generated distribution
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
))

#writing server function
server<-shinyServer(function(input, output) {
  
  #referring output distPlot in ui.r as output$distPlot
  output$distPlot <- renderPlot({
    
    data_reduced<- data[,c("YEAR","MONTH","DAY_OF_WEEK","HOUR","UCR_PART")]
    
    
    #referring input x in ui.r as input$x
    if(input$x=='a'){
      i<-1
    }
    
    if(input$x=='b'){
      i<-2
    }
    
    if(input$x=='c'){
      i<-3
    }
    
    if(input$x=='d'){
      i<-4
    }
    
    
    #producing histogram as output
    ggplot(data = data) +
    geom_bar(mapping = aes(x = data_reduced[[i]])) +
      xlab("")
  })
})

shinyApp(ui, server)
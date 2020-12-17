library(shiny)
library(ggplot2)


ui<-fluidPage(
  
  titlePanel("Association Rules Mining", tags$head()),
  
  sidebarPanel(
    
    sliderInput('sup', "Support", min = 0.001, max = 1, value = 0.25, step = 0.005),
    
    sliderInput('conf', 'Confidence', min = 0.01, max =1, value = 0.25, step = 0.005),
    
    sliderInput('len', 'Minimum Rule Length', min = 1, max =15, value = 3, step = 1),
    
    sliderInput('mlen', 'Maximum Rule Length', min = 1, max =15, value = 7, step = 1),
    
    sliderInput('time', 'Maximum Time Taken', min = 1, max =25, value = 3, step = 1)
    
    
  ),
  
  mainPanel(
    tabsetPanel(id = 'mytab',
                tabPanel('Rules_No', value = 'datatable',DT::dataTableOutput("rules")),
                tabPanel('Plot_No', value = 'graph',plotOutput('plot')),
                tabPanel('Rules_Yes', value = 'datatable',DT::dataTableOutput("rules1")),
                tabPanel('Plot1_Yes', value = 'graph',plotOutput('plot1')))
  )
)


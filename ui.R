library(shiny)

shinyUI(fluidPage(
        
        # Application title
        titlePanel("Health"),
        
        # Sidebar with a slider input for number of bins
        sidebarLayout(
                sidebarPanel(
                        #  textOutput("unreadtext")
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                        plotOutput("weightPlot")
                )
        )
))
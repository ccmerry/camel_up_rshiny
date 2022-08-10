#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Camel Cup"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons("camelBet",
                   "Camel", 
                   choices = camel_list,
                   selected = "blue"),
      
      actionButton("rollDice",
                   label = "Roll"),
      
      actionButton("pickCamel",
                   label = "Pick Camel"),
      
      actionButton("runSim",
                   label = "Run Simulation"),
      
      actionButton("learnSim",
                   label = "Learn"),
      
      actionButton("debugSim",
                   label = "Debug"),
      #<i class="fa-solid fa-circle-5"></i>
      actionButton("saveNeuron",
                   label = "Save Sim"),
      
      #img(src='test_five.png'),
      
      actionButton("resetGame",
                   label = "Reset")
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("pTurn"),
      #tableOutput("boardTable"),
      plotOutput("boardPlot",height=200),
      #textOutput("colorsLeftTitle"),
      #textOutput("colorsLeft"),
      fluidRow(
        column(6, align= "center",
               htmlOutput("unrolledTitle")),
        column(6, align="center",
               htmlOutput("lastRollTitle"))
      ),
      fluidRow(
        column(6, align="center",
               plotOutput("dicePlot", height = 100, width = 200)),
        column(6, align = "center",
               textOutput("currentRoll"))
      ),
      fluidRow(
        column(6, align="center",
               htmlOutput("camelPlacesTitle"))
      ),
      fluidRow(
        #column(6, align="center",
               #textOutput("currentRoll")),
        column(6, align="center",
               textOutput("camelPlaces"))
      ),
      #tableOutput("camelCards"),
      plotOutput("camelCardsPlot",height = 200, width = 200),
      fluidRow(
        column(6, align="center",
               plotOutput("p1CardsPlot",height = 200, width = 200)),
        column(6, align="center",
               plotOutput("p2CardsPlot",height = 200, width = 200))
      ),
      fluidRow(
        column(6, align="center",
               tableOutput("player1Hand")),
        column(6, align="center",
               tableOutput("player2Hand"))
      ),
      tableOutput("playerScores"),
      #plotOutput("boardPlot",height=200)
      tableOutput("boardTable"),
      #plotOutput("dicePlot", width = 200)
      textOutput("colorsLeftTitle"),
      textOutput("colorsLeft"),
      #plotOutput("camelCardsPlot",height = 200, width = 200)
      #plotOutput("p1CardsPlot",height = 200, width = 200)
      tableOutput("rankCamel")
      )
    )
  )
)

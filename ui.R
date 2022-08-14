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
shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
  
    # Application title
    titlePanel("Camel Cup"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          
          tabPanel("Move",
                   p(HTML(strrep(br(), 1))),
                   
                  radioButtons("camelBet",
                               "Camel", 
                               choices = camel_list,
                               selected = "blue"),
                  
                  fluidRow(
                    actionButton("pickCamel",
                                 label = "Pick Camel",
                                 style="width:50%;")),
                  
                  fluidRow(
                    actionButton("rollDice",
                                 label = "Roll",
                                 style="width:50%;")),
                  
                  
                  fluidRow(
                    selectInput("selectFinal", 
                                label = "Camel Win/Lose", 
                                choices = stable_camel, 
                                selected = 1)),
                  
                  fluidRow(
                    column(6, align = "center",
                           actionButton("winBet", 
                                        label = "Win",
                                        style="width:99%;")),
                    column(6, align = "center",
                           actionButton("loseBet", 
                                        label = "Lose",
                                        style="width:99%;"))),
                  
                  fluidRow(
                    actionButton("resetGame",
                                 label = "Reset",
                                 style="width:50%;")),
                  
          ),
          
          tabPanel("ML",
                   p(HTML(strrep(br(), 1))),
                   
                   fluidRow(
                     actionButton("runSim",
                                  label = "Run Simulation",
                                  style="width:50%;")),
                   
                   fluidRow(
                     actionButton("learnSim",
                               label = "Learn",
                               style="width:50%;")),
                   
                   fluidRow(
                     actionButton("debugSim",
                               label = "Debug",
                               style="width:50%;")),
                   
                   fluidRow(
                     actionButton("saveNeuron",
                               label = "Save Sim",
                               style="width:50%;")),
                   
                   fluidRow(
                     actionButton("printChoice",
                                  label = "AI choice",
                                  style="width:50%;"))
            )),
        width = 2),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          
          tabPanel("Game",
                   htmlOutput("spaceBreak2"),
                   
                   fluidRow(
                     column(4, align="center",
                            htmlOutput("scoreBoard", 
                                       style = "font-size:160%;")),
                     #column(4, align="center",
                            #textOutput("pTurn")),
                     column(4, align="left",
                            tableOutput("playerScores")),
                     column(4, align="center",
                            tableOutput("rankCamel"))
                     ),
                   
                   plotOutput("boardPlot",height=200),
                   htmlOutput("spaceBreak"),
                   
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
                     
                     column(4, align = "center",
                            htmlOutput("availableTitle"),
                            plotOutput("camelCardsPlot",height = 200, width = 200)),
                     
                     column(4, align="center",
                            htmlOutput("p1HandTitle"),
                            plotOutput("p1CardsPlot",height = 100, width = 200),
                            htmlOutput("p1WinTitle")),
                     
                     column(4, align="center",
                            htmlOutput("p2HandTitle"),
                            plotOutput("p2CardsPlot",height = 100, width = 200),
                            htmlOutput("p2WinTitle"))
                     )
                   ),
          
          tabPanel(
            "How to Play"
            )
          
          )
        )
      )
    )
  )

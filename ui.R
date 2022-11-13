#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
rm(list=ls())
library(shinyBS)
library(shiny)
library(ggimage)
library(shinyWidgets)
#install.packges("library(shinyWidgets)")

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    includeCSS("www/styles.css"),
    shinyjs::useShinyjs(),
    
  
    # Application title
    titlePanel("Camel Up"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          
          tabPanel("Move",
                   p(HTML(strrep(br(), 1)),style = "font-size: 2px;"),
                   
                   p(HTML(strrep(br(), 1)),style = "font-size: 2.5px;"),
                   
                  radioButtons("camelBet",
                               label = tags$span(
                                 "Camels", 
                                 style = "font-size:15px;",
                                 tags$i(
                                   class = "glyphicon glyphicon-info-sign", 
                                   style = "color:#0072B2;font-size:13px;",
                                   title = "Bet on the camel you expect to finish first for this leg of the race."
                                 )),
                               choices = camel_list,
                               selected = "blue"),
                  
                  fluidRow(
                    column(12, align = "center",
                    actionButton("pickCamel",
                                 label = "Pick Camel",
                                 style="width:75%;overflow:hidden;"))),
                  
                  p(HTML(strrep(br(), 1)),style = "font-size: .1px;"),
                  
                  fluidRow(
                    column(12, align = "center",
                    actionButton("rollDice",
                                 label = "Roll",
                                 style="width:75%;overflow:hidden;"))),
                  
                  p(HTML(strrep(br(), 1)),style = "font-size: 5px;"),
                  
                  
                  fluidRow(
                    column(12, align = "center",
                    selectInput("selectFinal", 
                                label = tags$span(
                                  "Camel Win/Lose", 
                                  style = "font-size:15px;",
                                  tags$i(
                                    class = "glyphicon glyphicon-info-sign", 
                                    style = "color:#0072B2;font-size:13px;",
                                    title = "Bet on the camel you expect to be first or last at the end of the game."
                                  )),
                                choices = stable_camel, 
                                selected = 1))),
                  
                  fluidRow(
                    column(6, align = "center",
                           actionButton("winBet", 
                                        label = "Win",
                                        style="width:99%;overflow:hidden;")),
                    column(6, align = "center",
                           actionButton("loseBet", 
                                        label = "Lose",
                                        style="width:99%;overflow:hidden;"))),
                  
                  p(HTML(strrep(br(), 1)),style = "font-size: 5px;"),
                  
                  fluidRow(
                    column(12, align = "center",
                           actionButton("aiTurnNoClick",
                                        label = "AI Move",
                                        style="width:50%;overflow:hidden;"))),
                  
                  fluidRow(
                    column(12, align = "center",
                           actionButton("aiTurnMove",
                                        label = "AI Move",
                                        style="width:50%;overflow:hidden;"))),
                  
                  p(HTML(strrep(br(), 1)),style = "font-size: .1px;"),
                  
                  fluidRow(
                    column(12, align = "center",
                    actionButton("resetGame",
                                 label = "New Game",
                                 style="width:75%;overflow:hidden;"))),
                  
          ),
          
          tabPanel("ML",
                   p(HTML(strrep(br(), 1))),
                   
                   #fluidRow(
                     #actionButton("runSim", 
                                  #label = HTML("Run Simulation"),
                                  #style="width:50%;overflow:hidden;")),
                   
                   #fluidRow(
                     #actionButton("learnSim",
                               #label = "Learn",
                               #style="width:50%;overflow:hidden;")),
                   
                   #fluidRow(
                     #actionButton("debugSim",
                               #label = "Debug",
                               #style="width:50%;overflow:hidden;")),
                   
                   #fluidRow(
                     #actionButton("printChoice",
                                  #label = "AI choice",
                                  #style="width:50%;overflow:hidden;")),
                   
                   #fluidRow(
                     #actionButton("testgraph",
                                  #label = "AI Move",
                                  #style="width:50%;overflow:hidden;")),
                   
                   fluidRow(
                     sliderInput("slider1", 
                                 label = "Hidden Layer 1",
                                 min = 1,
                                 max = 100,
                                 ticks = FALSE,
                                 value = 90,)),
                   
                   fluidRow(
                     sliderInput("slider2", 
                                 label = "Hidden Layer 2", 
                                 min = 1,
                                 max = 100,
                                 ticks = FALSE,
                                 value = 90)),
                   
                   fluidRow(
                     sliderInput("slider3", 
                                 label = "Hidden Layer 3", 
                                 min = 1,
                                 max = 100,
                                 ticks = FALSE,
                                 value = 90)),
                   
                   fluidRow(
                     column(12, align = "center",
                            actionButton("changeNeurons",
                                         label = "Change Neurons",
                                         style="width:75%;overflow:hidden;"),
                            tags$i(
                              class = "glyphicon glyphicon-info-sign", 
                              style = "color:#0072B2;font-size:13px;",
                              title = "Changes the neurons to the inputs above."
                            ))),
                   
                   p(HTML(strrep(br(), 1)),style = "font-size: 5px;"),
                   
                   fluidRow(
                     column(12, align = "center",
                            actionButton("learnSim",
                                         label = "Learn",
                                         style="width:75%;overflow:hidden;"),
                            tags$i(
                              class = "glyphicon glyphicon-info-sign", 
                              style = "color:#0072B2;font-size:13px;",
                              title = "After changing the neurons, this will train the model using the new neurons. Takes approximately 2 minutes to run."
                            ))),
                   
                   p(HTML(strrep(br(), 1)),style = "font-size: 5px;"),
                   
                   #fluidRow(
                     #column(12, align = "center",
                            #actionButton("saveNeuron",
                                         #label = "Save Neurons",
                                         #style="width:75%;overflow:hidden;"),
                            #tags$i(
                              #class = "glyphicon glyphicon-info-sign", 
                              #style = "color:#0072B2;font-size:13px;",
                              #title = "If you have changed the neurons and trained the neurons, this will save off the results."
                            #)))
            )),
        width = 2),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          
          tabPanel("Game",
                   htmlOutput("spaceBreak2"),
                   
                   fluidRow(
                     column(3, align="center",
                            htmlOutput("scoreBoard", 
                                       style = "font-size:160%;")),
                     column(3, align="left",
                            htmlOutput("pTurn",
                                       style = "font-size:120%;")),
                     column(3, align="center",
                            htmlOutput("winMsg",
                                       style = "font-size:120%;")),
                     column(3, align="center",
                            tableOutput("rankCamel"))
                     ),
                   
                   plotOutput("boardPlot", height = "100%", width = "100%"),
                   htmlOutput("spaceBreak"),
                   
                   fluidRow(
                     column(6, align= "center",
                            htmlOutput("unrolledTitle")),
                     column(6, align="center",
                            htmlOutput("lastRollTitle"))
                     ),
                   fluidRow(
                     column(6, align="center",
                            plotOutput("dicePlot", height = 50, width = 200)),
                     column(6, align = "center",
                            textOutput("currentRoll"))
                     ),
                   
                   p(HTML(strrep(br(), 1)),style = "font-size: 7px;"),
                   
                   fluidRow(
                     
                     column(4, align = "center",
                            htmlOutput("availableTitle"),
                            plotOutput("camelCardsPlot",height = 100, width = 200)),
                     
                     column(4, align="center",
                            htmlOutput("p1HandTitle"),
                            plotOutput("p1CardsPlot",height = 50, width = 200),
                            htmlOutput("p1WinTitle")),
                     
                     column(4, align="center",
                            htmlOutput("p2HandTitle"),
                            plotOutput("p2CardsPlot",height = 50, width = 200),
                            htmlOutput("p2WinTitle"))
                     )
                   ),
          
          tabPanel("How to Play",
                   fluidRow(
                     column(12, 
                            align= "left",
                            htmlOutput("howRules")))
                   ),
          
          tabPanel("AI Visualization",
                   p(HTML(strrep(br(), 1)),
                     style = "font-size: .1px;"),
                   plotOutput("plotLayers", height = 700),
                   p(HTML(strrep(br(), 1)),
                     style = "font-size: .1px;"),
                   p(HTML(strrep(br(), 1)),
                     style = "font-size: .1px;"),
                   fluidRow(
                     column(3, align = "right",
                            htmlOutput("neuronScore")),
                     column(9, align = "left",
                            tags$i(
                              class = "glyphicon glyphicon-info-sign", 
                              style = "color:#0072B2;font-size:13px;",
                              title = "A scoring method that I developed. The score is the sum of the difference of all output neurons from the simulations correct choice. A lower score is a better score. "
                              )))
                   ),
          
          tabPanel("About",
                   fluidRow(
                     column(12,
                            align = "left",
                            htmlOutput("aboutPage"),
                            uiOutput("tab")))
                   )
          )
        )
      )
    )
  )

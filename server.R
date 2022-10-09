#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output, session) {
  
  #reactive values
  ##########################################
  
  colors <- reactiveValues(colors_left = c("blue","green","orange","red","yellow"))
  
  neuron_num <- reactiveVal({neurons})
  
  camel_tiled <- reactiveValues(ct_df = data.frame(blue = c(5,3,2),
                                                   green = c(5,3,2),
                                                   orange = c(5,3,2),
                                                   red = c(5,3,2),
                                                   yellow = c(5,3,2)
                                                   )
  )
  
  
  p1s <- reactiveVal(0)
  p2s <- reactiveVal(0)
  
  player_score <- reactiveValues(ps_df = data.frame(player1score = c(0),
                                                    player2score = c(0)))
  
  turn1 <- reactive({"player1"})
  
  game_over <-reactiveVal({FALSE})
  
  
  p_hand_view <- reactiveValues(phandv = global_hand_plot())
  
  p1_hand_view <- reactiveValues(p1handv = 0)
  p2_hand_view <- reactiveValues(p2handv = 0)
  
  player1 <- reactiveValues(p1df = player1_df)
  player2 <- reactiveValues(p2df = player2_df)
  
  p1_wl_hand <- reactiveValues(p1w = "",
                               p1l = "",
                               p1ws = 0,
                               p1ls = 0)
  
  p2_wl_hand <- reactiveValues(p2w = "",
                               p2l = "",
                               p2ws = 0,
                               p2ls = 0)
  
  playerTurn <- reactiveVal({
    "player1"
  })
  
  turnShow <- reactiveVal({
    " Player 1"
  })
  
  winName <- reactiveVal({
    " "
  })
  
  camel_server <- reactiveVal({c("blue","green","orange","red","yellow")})
  
  input_act <- reactiveVal({""})
  act1 <- reactiveVal({""})
  act2 <- reactiveVal({""})
  act3 <- reactiveVal({""})
  act4 <- reactiveVal({""})
  
  ai_start_label <- reactiveVal({start_label})
  
  board_matrix <- sboard()
  main_matrix <- reactiveValues(df = board_matrix)
  
  training_set <- reactiveValues(ts = empty_training_set)
  
  wmat4r <- reactiveVal(wmat4)
  wmat3r <- reactiveVal(wmat3)
  wmat2r <- reactiveVal(wmat2)
  wmat1r <- reactiveVal(wmat1)
  
  bmat4r <- reactiveVal(bmat4)
  bmat3r <- reactiveVal(bmat3)
  bmat2r <- reactiveVal(bmat2)
  bmat1r <- reactiveVal(bmat1)
  
  r_list <- reactiveValues(df = empty_training_set)
  
  shinyjs::disable("aiTurnNoClick")
  
  ######################################################
  
  
  #When a die is rolled
  observeEvent(input$rollDice, {
    rollDiceChoice()
  })
  
  #When a card is picked for current leg of the race
  observeEvent(input$pickCamel, {
    pickCamelChoice(input$camelBet)
  })
  
  #When player selects card on the final winner of the race
  observeEvent(input$winBet, {
    pickWinCamel(input$selectFinal)
  })
  
  #When player selects card on the final loser of the race
  observeEvent(input$loseBet, {
    pickLoseCamel(input$selectFinal)
  })
  
  observeEvent(input$resetGame, {
    colors$colors_left <- reset_camels(colors$colors_left)
    main_matrix$df <- sboard()
    order_list <- find_camel_order(main_matrix$df)
    player1$p1df = player1_df
    player2$p2df = player2_df
    player_score$ps_df['player1score'] <- 0
    player_score$ps_df['player2score'] <- 0
    camel_tiled$ct_df <- start_camel_tile_df
    
    #resets overall winner/loser info
    p1_wl_hand$p1w <- ""
    p1_wl_hand$p1l <- ""
    p1_wl_hand$p1ws <- 0
    p1_wl_hand$p1ls <- 0
    p2_wl_hand$p2w <- ""
    p2_wl_hand$p2l <- ""
    p2_wl_hand$p2ws <- 0
    p2_wl_hand$p2ls <- 0
    
    #resets hands
    p1_hand_view$p1handv = 0
    p2_hand_view$p2handv = 0
    player1$p1df = player1_df
    player2$p2df = player2_df
    camel_tiled$ct_df <- start_camel_tile_df
    
    camel_server(stable_camel)
    
    hnd_tst <- hand_plot()
    p_hand_view$phandv <- hnd_tst
    winName(" ")
    game_over(FALSE)
  })
  
  #removes the ability to pick camel cards if none are left
  observe({
    x <- input$pickCamel
    
    updateRadioButtons(session, "camelBet",
                       label = NULL,
                       choices = camel_server(),
                       selected = camel_server()[1])
    
  })
  
  #toggles pick camel button if no cards are left
  observe({
    if(length(camel_server()) == 0){
      shinyjs::disable("pickCamel")
    }
    else{
      shinyjs::enable("pickCamel")
    }
  })
  
  # observe({
  #   x <- playerTurn()
  #   
  #   if (playerTurn() == "player1"){
  #     shinyjs::hide("resetGame")
  #   }
  #   else if (playerTurn() == "player2"){
  #     shinyjs::show("resetGame")
  #   }
  # })
  
  #removes ability to bet on final winner/loser if already done
  observe({
    if (playerTurn() == "player1"){
      shinyjs::enable("camelBet")
      shinyjs::enable("pickCamel")
      shinyjs::enable("rollDice")
      shinyjs::enable("selectFinal")
      shinyjs::enable("winBet")
      shinyjs::enable("loseBet")
      shinyjs::show("aiTurnNoClick")
      shinyjs::hide("aiTurnMove")
      if (p1_wl_hand$p1w != ""){
        shinyjs::hide("winBet")
      }
    }
    else if(playerTurn() == "player2"){
      shinyjs::disable("camelBet")
      shinyjs::disable("pickCamel")
      shinyjs::disable("rollDice")
      shinyjs::disable("selectFinal")
      shinyjs::disable("winBet")
      shinyjs::disable("loseBet")
      if(!game_over()){
        shinyjs::hide("aiTurnNoClick")
        shinyjs::show("aiTurnMove")
      }
      if (p2_wl_hand$p2w != ""){
        
      }
    }
  })
  
  #lose button toggle
  # observe({
  #   if (playerTurn() == "player1" & 
  #       p1_wl_hand$p1l != "") {
  #     shinyjs::hide("loseBet")
  #   }
  #   else if(playerTurn() == "player2" & 
  #           p2_wl_hand$p2l != ""){
  #     shinyjs::hide("loseBet")
  #   }
  #   else{
  #     shinyjs::show("loseBet")
  #   }
  # })
  
  
  pick_reactive <- eventReactive(input$pickCamel, {
    input$camelBet
  })
  
  output$camelPick <- renderText({
    pick_reactive()
  })
  
  output$camelCards <- renderTable({
    camel_tiled$ct_df
  }, digits = 0)
  
  output$camelCardsPlot <- renderPlot({
    p_hand_view$phandv
  })
  
  output$player1Hand <- renderTable({
    player1$p1df
  }, digits = 0)
  
  output$player2Hand <- renderTable({
    player2$p2df
  }, digits = 0)
  
  output$p1CardsPlot <- renderPlot({
    p1_hand_view$p1handv
  })
  
  output$p2CardsPlot <- renderPlot({
    p2_hand_view$p2handv
  })
  
  output$playerScores <- renderTable({
    s_table <- data.frame("Player 1" = player_score$ps_df[['player1score']],
                          "Player 2" = player_score$ps_df[['player2score']],
                          check.names=FALSE)
    s_table
  }, digits = 0)
  
  observe({
    playerTurn()
    if(playerTurn() == "player1"){
      turnShow(" Player 1")
    }
    else(
      turnShow(" Player 2")
    )
    showAINeuron()
  })
  
  output$pTurn <- renderText({
    paste("<b>Turn:</b><font size=2>",
          turnShow(),"</font>")
    #turnShow()
  })
  
  output$boardTable <- renderTable({
    main_matrix$df
  })
  
  output$boardPlot <- renderPlot({
    board_df <- boardChangeDF(main_matrix$df)
    board_df["image"] <- camel_image_df["image"]
    ggplot(board_df,aes(x_axis, y_axis, color = c_c,image = image)) + 
      geom_image(size = .05) + 
      theme(axis.line.x = element_line(colour = "black"),
            axis.text.x = element_text(face="bold", colour = "black", size = 14),
            axis.title.x = element_blank(),
            axis.line.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position="none") +
      scale_x_discrete(limits= board_x_ticks) +
      scale_y_discrete(limits= board_y_ticks, expand=c(.1,.1)) +
      scale_color_manual(values = c("blue" = "blue", "green" = "green", "orange" = "orange", "red" = "red", "yellow" = "yellow"))
  })
  
  output$dicePlot <- renderPlot({
    dice_df <- remainPlotDF(colors$colors_left)
    #d_c <- dice_df[[unrolled]]
    ggplot(dice_df,aes(x = unrolled, color = unrolled)) + 
      geom_point(size = 8, shape = 15, stat = "count") + 
      #ggtitle("Unrolled Dice") +
      theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"),
            axis.line.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.line.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position="none") +
      scale_color_manual(values = c("blue" = "blue", "green" = "green", "orange" = "orange", "red" = "red", "yellow" = "yellow"))
  })
  
  output$colorsLeft <- renderText({
    colors$colors_left
  })
  
  output$camelPlaces <- renderText({
    camel_place <- unlist(find_camel_order(main_matrix$df), use.names=FALSE)
    camel_place
  })
  
  output$rankCamel <- renderTable({
    camel_ranked <- rankedList(unlist(find_camel_order(main_matrix$df), use.names=FALSE))
    camel_ranked
  })
  
  output$camelListTitle <- renderText({
    paste("<font size=3><b>","Camels","</b></font>")
  })
  
  output$availableTitle <- renderText({
    paste("<font size=3><b>","Available Cards<b>","</b></font>")
  })
  
  output$p1HandTitle <- renderText({
    paste("<font size=3><b>","<b>Player 1 Hand</b>","</b></font>")
  })
  
  output$p2HandTitle <- renderText({
    paste("<font size=3><b>","<b>Player 2 Hand<b>","</b></font>")
  })
  
  output$p1WinTitle <- renderText({
    paste("<font size=2><b>","Winning Camel:", p1_wl_hand$p1w, "<br/>Losing Camel:", p1_wl_hand$p1l,"</b></font>")
  })
  
  output$p2WinTitle <- renderText({
    paste("<font size=2><b>","Winning Camel:", p2_wl_hand$p2w, "<br/>Losing Camel:", p2_wl_hand$p2l,"</b></font>")
  })
  
  output$spaceBreak2 <- renderText({
    HTML(paste(" ", " ", " ", sep="<br/>"))
  })
  
  output$spaceBreak <- renderText({
    HTML(paste(" ", " ", " ", sep="<br/>"))
  })
  
  output$scoreBoard <- renderText({
    paste("<b><u>Score</u></b><br/><div><font size=2>Player 1:",
          player_score$ps_df[['player1score']],
          "</font></div><font size=2></font>",
          "<div><font size=2>Player 1:", 
          player_score$ps_df[['player2score']],"</font></div>")
  })
  
  output$unrolledTitle <- renderText({
    paste("<font size=3><b>","<b>Unrolled Dice</b>","</b></font>")
  })
  
  output$lastRollTitle <- renderText({
    paste("<font size=3><b>","<b>Last Roll</b>","</b></font>")
  })
  
  output$currentRoll <- renderText({
    "None"
  })
  
  output$camelPlacesTitle <- renderText({
    #"Camel Places"
    paste("<b>Camel Places</b>")
  })
  
  output$colorsLeftTitle <- renderText({
    "Camel Left to Roll"
  })
  
  output$winMsg <- renderText({
    paste("<b>",winName(),"</b>")
  })
  
  output$howRules <- renderText({
    paste("<br/><font size=3><b>The Movement of the Camels:</b></font> <br/>
          The Camels move accross the Race track. During the game, the players move the Camels by rolling the dice.
          When a player chooses Roll Dice, 1 colored Die is chosen. The camel corresponding to the color die moves
          the amount rolled.<br/>
          
          <br/>
          
          <font size=3><b>Camel Stack:</b></font> <br/>
          
          Camels on the same space always form a Camel stack. If a Camel is moved that is part of a Camel stack, 
          it carries along all Camels that sit on top of it. Any Camels beneath it are left where they are.<br/>
          
          <br/>
          
          <font size=3><b>Camel Unit:</b></font> <br/>
          
          Since sometimes single Camels and sometimes Camel stacks are moved, we use the term Camel unit. 
          A Camel unit consists either of a single Camel or a stack of Camels.<br/>
          
          If a Camel unit ends its movement on a space where there is another Camel unit, it jumps on top of that other unit.<br/>
          Important: For all purposes of ranking, a Camel that sits on top of another Camel is always considered 
          ahead of the Camel it sits on.<br/>
          
          <br/>
          
          <font size=3><b>The Legs:</b></font> <br/>
          The race comprises several Legs. A Leg ends when the last of the 5 Dice has been rolled
          and the respective Camel has moved. Right at the end of each Leg, a Leg scoring round occurs, 
          in which players receive or lose money according to the tiles they have acquired during the Leg.<br/>
          
          <br/>
          
          <font size=3><b>Game Play:</b></font> <br/>
          The player with the Leg Starting Player marker begins the first Leg (and thus the game) by performing exactly 1 action.
          Then the next player performs 1 action, then the next player and so on. There are 3 possible actions.<br/>
          When it is your turn, you must choose and perform exactly 1 of them:<br/>
          1. Take the top Leg Betting tile of any stack (and thus back the Camel of that color to win the current Leg).<br/>
          2. Roll Dice.<br/>
          3. Bet on the overall winner OR overall loser by placing 1 of your Race Betting cards face down 
          onto the appropriate Betting space.<br/>
          The game ends as soon as the first Camel crosses the finish line. Then a final Leg scoring round occurs, 
          as well as an End scoring round for the overall winning Camel and the overall losing Camel.<br/>
          
          <b>I. Take 1 Leg Betting Tile:</b> <br/>
          Take the top Leg Betting tile from any stack on the Game board and place it in front of you. 
          By doing this, you back the Camel of that color (which means that you hope it will 
          be in the lead at the end of the Leg).<br/>
          There is no limit to the number of Leg Betting tiles you may collect throughout the Leg. 
          You may even have several of the same color lying in front of you.<br/>
          
          <b>II. Roll Dice:</b> <br/>
          <b>III. Bet on the Overall Winner or Overall Loser:</b> <br/>
          As your action, you can bet on the overall winner. This means, you secretly back the Camel 
          that you believe will be in the lead at the end of the game. To do this, secretly choose 1 of your 
          Race betting cards (in the color of the Camel you think will win) and put it face down onto the Betting 
          space for the overall winner.<br/>
          You may instead bet on the overall loser (backing the Camel that you believe will be last on the Race 
          track at the end of the game) by secretly choosing 1 of your Race Betting cards and putting it face 
          down onto the Betting space for the overall loser.
          If there are already any cards on your chosen Betting space, you simply put yours on top of those cards.
          Once placed, a card must stay where it is, even if you later realize you backed the wrong Camel. However, 
          as long as you have cards in hand, you may always choose as your action to place 1 of them onto either Betting space.<br/>
          
          <br/>
          
          <font size=3><b>End of a Leg</b></font> <br/>
          When a player takes the last Pyramid tile from the Game board, he first reveals the last \
          Die from the Pyramid and moves the respective Camel as usual. Then, before the next player takes his turn, 
          a Leg scoring round occurs for all players:<br/>
          Start by giving the Leg Starting Player marker to the player that sits to the left of the player 
          who just took the last Pyramid tile (so you will remember who will start the next Leg).<br/>
          
          Then see which Camel is in the lead (on the space farthest along the Race track). If there is a 
          stack of Camels in the lead, the leading Camel is the one on top of the stack.<br/>
          
          Now, each player gains or loses points according to the Leg Betting tiles.<br/>
          
          For each Leg Betting tile of the leading Camel, the player gains the number of points
          on the tile: 5, 3, or 2.
          
          For each Leg Betting tile of the Camel in second place, the player gains 1 point.
          
          For each Leg Betting tile of any other Camel, the player loses 1 point.<br/>
          
          <br/>
          
          <font size=3><b>End of the Game</b></font> <br/>
          As soon as the first Camel unit crosses the finish line, the race ends immediately. 
          Now, carry out the Leg scoring round one last time. 
          After this final Leg scoring round, the End scoring round for the overall winner and overall loser occurs. 
          For this, first attend to the face-down card deck on the Betting space for the overall winner. 
          Remember: the card on the bottom was the first card placed, the card on top the last. 
          Turn the entire deck face up, so that now the card placed first is face up on top while the card placed last 
          is on the bottom. Now players gain or lose money according to the cards they have placed. Start at the top 
          and go through the face-up deck card by card.
          
          Only cards that show the actual winning Camel grant money:
          The player (if any) that picked the actual winner of the race receives 8 points.
          The player (if any) who placed the second card showing the winner receives 5 points, 
          the third player 3 points, the fourth player 2 EP, all others 1 points.
          Remember: this only applies to cards showing the actual winner. For each card in the deck 
          that shows any Camel other than the winner, the owner of that card must pay 1 point to the bank.
          After going through the deck for the overall winner, go through the deck of cards for the overall 
          loser in the same way. The overall loser, of course, is the least advanced Camel (in the case of a stack, 
          it is the Camel on the bottom of the stack).<br/>
          <br/>")
  })
  
  output$aboutPage <- renderText({
    paste("<br/>The goal of this project was to implement a machine learning program that does not use 
          a machine learning package. <br/>
          
          <br/>
          
          I used all custom functions to create the ai to compete against. It uses back propagation and gradient 
          descent to learn and minimize its loss function.
          
          <br/>
          
          <br/>")
  })
  
  url <- a("My Github link", href="https://www.google.com/")
  
  output$tab <- renderUI({
    tagList(url)
    })
  
  output$slider1 <- renderPrint({
    input$slider1
    })
  
  output$slider2 <- renderPrint({
    input$slider2
  })
  
  output$slider3 <- renderPrint({
    input$slider3
  })
  
  output$aiPlot <- renderPlot({
    ones <- replicate(16, 1)
    x_increase <- flatchoices_x_ticks
    x_flatchoices <- rev(flatchoices)
    
    ai_data <- data.frame(ai_x = x_increase,
                          ai_y = ones,
                          ai_x_label = x_flatchoices)
    
    ggplot(ai_data, aes(y = ai_x, x = ai_y)) +
      geom_point(size = 16, shape = 16, color="green", alpha = ai_start_label()) +
      geom_text(aes(label=ai_start_label())) +
      theme(
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5,vjust=-10, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none") +
      scale_y_continuous(breaks=seq(1,16,1),labels = x_flatchoices)
  })
  
  
  ######
  #Hand Cards
  ######
  
  #For loop could be implemented here to make it more efficient and cleaner
  hand_plot <- function(){
    
    b_c_hand <- handColumnClean(camel_tiled$ct_df['blue'])
    value_hand <- valueHand(camel_tiled$ct_df['blue'])
    x_append <- xHand(b_c_hand)
    y_append <- yHand(b_c_hand)
    color_append <- appendHand(camel_tiled$ct_df['blue'],"blue")
    
    value_hand <- valueHand(camel_tiled$ct_df['green'], value_hand)
    g_c_hand <- handColumnClean(camel_tiled$ct_df['green'])
    color_append <- appendHand(camel_tiled$ct_df['green'],"green",color_append)
    x_append <- xHand(g_c_hand,x_append)
    y_append <- yHand(g_c_hand, y_append)
    
    value_hand <- valueHand(camel_tiled$ct_df['orange'], value_hand)
    o_c_hand <- handColumnClean(camel_tiled$ct_df['orange'])
    color_append <- appendHand(camel_tiled$ct_df['orange'],"orange",color_append)
    x_append <- xHand(o_c_hand,x_append)
    y_append <- yHand(o_c_hand, y_append)
    
    value_hand <- valueHand(camel_tiled$ct_df['red'], value_hand)
    r_c_hand <- handColumnClean(camel_tiled$ct_df['red'])
    color_append <- appendHand(camel_tiled$ct_df['red'],"red",color_append)
    x_append <- xHand(r_c_hand,x_append)
    y_append <- yHand(r_c_hand, y_append)
    
    value_hand <- valueHand(camel_tiled$ct_df['yellow'], value_hand)
    y_c_hand <- handColumnClean(camel_tiled$ct_df['yellow'])
    color_append <- appendHand(camel_tiled$ct_df['yellow'],"yellow",color_append)
    x_append <- xHand(y_c_hand,x_append)
    y_append <- yHand(y_c_hand, y_append)
    
    if(length(y_append)==0){
      
      handggplot <- ggplot() +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5,vjust=-10, face="bold"))
      return(handggplot)
    }
    else
    {
      handggplot <- ggplot() +
        geom_point(aes(x_append, y_append),
                   color = color_append,
                   shape = 15,
                   size = 8.5) +
      geom_text(aes(x_append, y_append, label=value_hand), 
                color = "black") +
        theme(
              axis.line.x = element_blank(),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line.y = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.title = element_text(hjust = 0.5,vjust=-10, face="bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              legend.position="none") +
        scale_x_discrete() +
        scale_y_discrete()
      return(handggplot)
    }
  }
  
  
  p_hand_plot <- function(p_hand_df, g_title){
    
    if (dim(p_hand_df)[1] == 0){
      handggplot <- ggplot() +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5,vjust=-10, face="bold"))
    }else{
    
    handggplot <- ggplot(data = p_hand_df, aes(x = x_vis_label,y = y_vis_label)) +
      geom_point(
                 color = p_hand_df[['color_label']],
                 shape = 15,
                 size = 8.5) +
      geom_text(aes(label=text_label), 
                color = "black") +
      theme(
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5,vjust=-10, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none") +
      scale_x_discrete(expand = c(0, 1.9)) +
      scale_y_discrete(expand = c(0, 1.9))
    }
    
    return(handggplot)
  }
  
  ################################################
  
  #Run simulations to find best answer
  observeEvent(input$runSim, {
    #empty_matrix <- matrix(0, 32, 1)
    #df <- data.frame(matrix(ncol = 32, nrow = 0))
    #blue_tile_df <- data.frame(matrix(ncol = 32, nrow = 1))
    #green_tile_df <- data.frame(matrix(ncol = 32, nrow = 1))
    #orange_tile_df <- data.frame(matrix(ncol = 32, nrow = 1))
    #red_tile_df <- data.frame(matrix(ncol = 32, nrow = 1))
    #yellow_tile_df <- data.frame(matrix(ncol = 32, nrow = 1))
    #training_set <- data.frame(matrix(ncol = 33, nrow = 0))
    #training_set$ts <- data.frame(matrix(ncol = 33, nrow = 0))
    
    save_dfs <- c("blue_tile_df", "green_tile_df", "orange_tile_df", "red_tile_df", "yellow_tile_df")
    
    run_iter <- 50
    
    sim_matrix <- sboard()
    sim_hand_matrix <- start_camel_tile_df
    
    p1_wl <- p1_place_df
    p2_wl <- p2_place_df
    
    remain_camels <- camel_list
    
    sim_turn = "p2"
    
    for(k in 1:100000){
      
      if (k%%1000 == 0) {
        print(k)
      }
      
      if(sim_turn=="p2"){
        sim_turn <- "p1"
      }
      else{
        sim_turn <- "p2"
      }
      
      if(sim_turn=="p1"){
        sim_place_df <- p1_wl
        sim_wl <- c(p1_wl[1,1],p1_wl[1,2])
      }
      else{
        sim_place_df <- p2_wl
        sim_wl <- c(p2_wl[1,1],p2_wl[1,2])
      }
      
      order_list <- find_camel_order(main_matrix$df)
      initial_inputs <- aiInputs(remain_camels, sim_hand_matrix, sim_matrix, sim_place_df,order_list)
      
      outputLayer <- neuronChoice(initial_inputs)
      #learn(initial_inputs)
      
      test_output <- rightChoice(sim_matrix, sim_hand_matrix, remain_camels, sim_wl, run_iter, FALSE)
      
      if(test_output[1] == "roll"){
        sim_roll <- dice_roll(remain_camels)
        remain_camels <- remove_die(sim_roll[2], remain_camels)
        sim_matrix <- board_change(sim_roll, sim_matrix)
        
        if(length(remain_camels) != 0){
          
        }
        else{
          remain_camels <- camel_list
          sim_hand_matrix <- start_camel_tile_df
        }
        if(win_check(sim_matrix)){
          sim_matrix <- sboard()
          sim_hand_matrix <- start_camel_tile_df
          remain_camels <- camel_list
          p1_wl <- p1_place_df
          p2_wl <- p2_place_df
        }
      }
      else if(test_output[2]=="wtile"){
        if(sim_turn=="p1"){
          p1_wl["winner"] <- test_output[1]
        }
        else{
          p2_wl["winner"] <- test_output[1]
        }
      }
      else if(test_output[2]=="ltile"){
        if(sim_turn=="p1"){
          p1_wl["loser"] <- test_output[1]
        }
        else{
          p2_wl["loser"] <- test_output[1]
        }
      }
      else{
        for(row in 1:nrow(sim_hand_matrix)){
          if(sim_hand_matrix[test_output[1]][row,]!=0){
            sim_hand_matrix[test_output[1]][row,] <- 0
            break
          }
        }
      }
      save_input_ans <- append(initial_inputs, test_output[1])
      training_set$ts[nrow(training_set$ts) + 1,] = save_input_ans
    }
    
    #print(training_set$ts)
    
    print("set rows")
    print(nrow(training_set$ts))
    save_set <- training_set$ts
    wd_name <- getwd()
    write_name <- paste(wd_name,"//data//simulation_results.csv",sep = "")
    print(write_name)
    write.csv(save_set,write_name, row.names = FALSE)
  })
  
  observeEvent(input$learnSim, {
    for(i in 1:200){
      learn2(20,1)
    }
    for(i in 1:100){
      learn2(75,.2)
    }
  })
  
  observeEvent(input$debugSim, {
    print(bmat4r())
  })
  
  
  
  learn2 <- function(trials_num, step_size){
    #These are empty matrices used for the adjustments needed 
    for(i in 1:layers){
      rcw <- paste("change_w", i, sep = "")
      assign(rcw, matrix(0, neuron_num()[i+1], neuron_num()[i]))
      rcb <- paste("change_b", i, sep = "")
      assign(rcb, matrix(0, neuron_num()[i+1], 1))
    }
    
    sum_matrix_cost <- 0
    
    #how many iterations it will do before taking the average and adjusting the neurons
    #run_trials <- 100
    run_trials <- trials_num
    first_neuron = neuron_num()[1]
    
    for(r in 1:run_trials){
      
      #train_matrix <- neuronChoice(input_vector)
      
      rand_input <- sample(1:16, 1)
      #rand_input <- 2
      randchoices <- c("roll","blue","green","orange","red","yellow",
                       "wtileblue","wtilegreen","wtileorange","wtilered","wtileyellow",
                       "ltileblue","ltilegreen","ltileorange","ltilered","ltileyellow")
      
      if(rand_input < 7){
        sim_results <- trial_results_df %>%
          filter(trial_results_df[,(first_neuron+1)] == randchoices[rand_input])
      }
      else if(rand_input < 12){
        sim_results <- trial_results_df %>%
          #filter(stringr::str_detect(trial_results_df[,33], "wtile"))
          filter(trial_results_df[,(first_neuron+1)] == randchoices[rand_input])
      }
      else{
        sim_results <- trial_results_df %>%
          #filter(stringr::str_detect(trial_results_df[,33], "ltile"))
          filter(trial_results_df[,(first_neuron+1)] == randchoices[rand_input])
      }
      rand_row_index <- sample(nrow(sim_results), 1)
      sim_row <- sim_results[rand_row_index, ]
      sim_answer <- sim_row[[1,(first_neuron+1)]]
      
      sim_index <- match(sim_answer,randchoices)
      sim_row_input <- sim_row[1:first_neuron]
      sim_row_m <- t(as.matrix(sim_row_input))
      
      activation1 = newActivation(wmat1r(),bmat1r(),sim_row_m)
      activation2 = newActivation(wmat2r(),bmat2r(),activation1)
      activation3 = newActivation(wmat3r(),bmat3r(),activation2)
      activation4 = newActivation(wmat4r(),bmat4r(),activation3)
      
      #train_max <- which(activation4 == max(activation4), arr.ind=T)
      #train_index <- train_max[[1,1]]
      #train_answer <- flatchoices[sim_index]
      
      #Creates zero matrix with the right answer as one
      answer_neuron_matrix <- matrix(.01, 16, 1)
      
      #choseninput is the right answer
      answer_neuron_matrix[sim_index,] <- .99
      
      #Takes answers and subtracts 1 or 0 to find the cost derivative
      newitem = activation4 - answer_neuron_matrix
      cmatrix = t(newitem)
      matrixcost = cmatrix %*% newitem
      
      #Calcs the derivative of the sigmoid
      dsigmoid4 = activation4 * (1 - activation4)
      #Multiply the derivative of the sigmoid function by the cost by the previous layer neuron
      cdicttemp = dsigmoid4 * (newitem*2)
      cbias4 = cdicttemp
      cwmat4 = cdicttemp %*% t(activation3)
      
      #Calculates the changeactivation for previous layer
      cact3 = t(as.matrix(wmat4r())) %*% cdicttemp
      dsigmoid3 = activation3 * (1-activation3)
      cdicttemp = dsigmoid3 * cact3
      cbias3 = cdicttemp
      cwmat3 = cdicttemp %*% t(activation2)
      cact2 = t(as.matrix(wmat3r())) %*% cdicttemp
      
      dsigmoid2 = activation2 * (1-activation2)
      cdicttemp = dsigmoid2 * cact2
      cbias2 = cdicttemp
      cwmat2 = cdicttemp %*% t(activation1)
      cact1 = t(as.matrix(wmat2r())) %*% cdicttemp
      
      dsigmoid1 = activation1 * (1-activation1)
      cdicttemp = dsigmoid1 * cact1
      cbias1 = cdicttemp
      cwmat1 = cdicttemp %*% t(sim_row_m)
      
      #summatrixcost = matrixcost + summatrixcost
      sum_matrix_cost = sum(matrixcost) + sum_matrix_cost
      
      change_w4 <- change_w4 + cwmat4
      change_w3 <- change_w3 + cwmat3
      change_w2 <- change_w2 + cwmat2
      change_w1 <- change_w1 + cwmat1
      
      change_b4 <- change_b4 + cbias4
      change_b3 <- change_b3 + cbias3
      change_b2 <- change_b2 + cbias2
      change_b1 <- change_b1 + cbias1
    }
    #stepsize=.5
    stepsize = step_size
    
    wmat4s <- wmat4r() - (change_w4/run_trials * stepsize)
    wmat3s <- wmat3r() - (change_w3/run_trials * stepsize)
    wmat2s <- wmat2r() - (change_w2/run_trials * stepsize)
    wmat1s <- wmat1r() - (change_w1/run_trials * stepsize)
    
    bmat4s <- bmat4r() - (change_b4/run_trials * stepsize)
    bmat3s <- bmat3r() - (change_b3/run_trials * stepsize)
    bmat2s <- bmat2r() - (change_b2/run_trials * stepsize)
    bmat1s <- bmat1r() - (change_b1/run_trials * stepsize)
    
    
    wmat4r(wmat4s)
    wmat3r(wmat3s)
    wmat2r(wmat2s)
    wmat1r(wmat1s)
    
    bmat4r(bmat4s)
    bmat3r(bmat3s)
    bmat2r(bmat2s)
    bmat1r(bmat1s)
    
    
    #this print statement helps track how far off the learn matrix is
    if(sample(1:10, 1) > 9){
      print(sum_matrix_cost/run_trials)
      #print(activation4*10)
      #print(newitem)
      #print((change_b4/run_trials * stepsize))
    }
  }
  
  modWBs <- function(weight_bias, adj_w_b, trial_num, step_amount){
    new_wb <- weight_bias - (adj_w_b/trial_num * step_amount)
    return(new_wb)
  }
  
  observeEvent(input$saveNeuron, {
    wmat_list <- list(wmat1r(),wmat2r(),wmat3r(),wmat4r())
    bmat_list <- list(bmat1r(),bmat2r(),bmat3r(),bmat4r())
    
    for(i in 1:layers){
      wvar <- paste("wmatSave", i, sep = "")
      new_string <- c("data\\",wvar,".csv")
      w_sav_string <- paste(new_string,collapse = "")
      write.csv(wmat_list[i], w_sav_string, row.names = FALSE)
      
      bvar <- paste("bmatSave", i, sep = "")
      newb_string <- c("data\\",bvar,".csv")
      b_sav_string <- paste(newb_string,collapse = "")
      write.csv(bmat_list[i], b_sav_string, row.names = FALSE)
    }
    print("saved wb")
  })
  
  observeEvent(input$printChoice, {
    
    ai_wl <- data.frame(winner = p1_wl_hand$p1w,
                        loser = p1_wl_hand$p1l
                        )
    
    order_list <- find_camel_order(main_matrix$df)
    initial_inputs <- aiInputs(colors$colors_left, camel_tiled$ct_df, main_matrix$df, ai_wl,order_list)
    print(initial_inputs)
    outputLayer <- neuronChoice(initial_inputs)
    print(round(outputLayer,6))
    ai_index_choice <- which.max(outputLayer)
    #rightChoice(sim_matrix, sim_hand_matrix, remain_camels, sim_wl, run_iter)
    p1_wl <- p1_place_df
    sim_wlrc <- c(p1_wl[1,1],p1_wl[1,2])
    #r_choice_num <- 
    print(rightChoice(main_matrix$df, camel_tiled$ct_df, colors$colors_left, sim_wlrc,50, TRUE))
    ai_start_label(rev(round(outputLayer,3)))
    print(flatchoices[ai_index_choice])
  })
  
  
  observeEvent(input$aiTurnMove, {
    #aiChoiceMove
    
    if(playerTurn() == "player1") {
      ai_wl <- data.frame(winner = p1_wl_hand$p1w,
                          loser = p1_wl_hand$p1l
      )
    }
    else{
      ai_wl <- data.frame(winner = p2_wl_hand$p2w,
                          loser = p2_wl_hand$p2l
      )
    }
    
    order_list <- find_camel_order(main_matrix$df)
    initial_inputs <- aiInputs(colors$colors_left, camel_tiled$ct_df, main_matrix$df, ai_wl,order_list)
    non_choice = initial_inputs[11:15]
    non_choice_wl = initial_inputs[36:37]
    
    #removes choices that are not avvailable
    outputLayer <- neuronChoice(initial_inputs)
    vec_start = 2
    for(x in non_choice){
      outputLayer[vec_start] <- outputLayer[vec_start] * x
      vec_start = vec_start + 1
    }
    
    for(x in 7:11){
      outputLayer[x] <- outputLayer[x] * ((initial_inputs[36] * -1) + 1)
    }
    
    for(x in 12:16){
      outputLayer[x] <- outputLayer[x] * ((initial_inputs[37] * -1) + 1)
    }
    
    ai_index_choice <- which.max(outputLayer)
    p1_wl <- p1_place_df
    sim_wlrc <- c(p1_wl[1,1],p1_wl[1,2])
    ai_start_label(rev(round(outputLayer,3)))
    
    aiRunChoice(ai_index_choice)
  })
  
  
  #When a die is rolled
  rollDiceChoice <- function(){
    dice_options <- c(1,2,3)
    dice_number_return <- sample(dice_options, 1)
    
    if (playerTurn() == "player1") {
      player_score$ps_df['player1score'] <- player_score$ps_df['player1score'] + 1
    }
    else {
      player_score$ps_df['player2score'] <- player_score$ps_df['player2score'] + 1
    }
    
    result_roll <- dice_roll(colors$colors_left)
    
    colors$colors_left <- remove_die(result_roll[2], colors$colors_left)
    
    main_matrix$df <- board_change(result_roll, main_matrix$df)
    
    camel_place <- unlist(find_camel_order(main_matrix$df), use.names=FALSE)
    
    output$boardTable <- renderTable({main_matrix$df})
    output$colorsLeft <- renderText({colors$colors_left})
    output$currentRoll <- renderText({result_roll})
    output$camelPlaces <- renderText({camel_place})
    
    
    if(length(colors$colors_left) != 0) {
      
    }
    else {
      order_list <- find_camel_order(main_matrix$df)
      p1handscore <- calc_score(player1$p1df, order_list[1], order_list[2])
      p2handscore <- calc_score(player2$p2df, order_list[1], order_list[2])
      player_score$ps_df['player1score'] <- p1handscore + player_score$ps_df['player1score']
      player_score$ps_df['player2score'] <- p2handscore + player_score$ps_df['player2score']
      colors$colors_left <- reset_camels(colors$colors_left)
      
      #resets hands
      p1_hand_view$p1handv = 0
      p2_hand_view$p2handv = 0
      player1$p1df = player1_df
      player2$p2df = player2_df
      camel_tiled$ct_df <- start_camel_tile_df
      
      camel_server(stable_camel)
    }
    
    if (playerTurn() == "player1") {
      playerTurn("player2")
    }
    else {
      playerTurn("player1")
    }
    
    output$boardPlot <- renderPlot({
      board_df <- boardChangeDF(main_matrix$df)
      board_df["image"] <- camel_image_df["image"]
      ggplot(board_df,aes(x_axis, y_axis, color = c_c,image = image)) + 
        geom_image(size = .05) + 
        theme(axis.line.x = element_line(colour = "black"),
              axis.text.x = element_text(face="bold", colour = "black", size = 14),
              axis.title.x = element_blank(),
              axis.line.y = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              legend.position="none") +
        scale_x_discrete(limits= board_x_ticks) +
        scale_y_discrete(limits= board_y_ticks, expand=c(.1,.1)) +
        scale_color_manual(values = c("blue" = "blue", "green" = "green", "orange" = "orange", "red" = "red", "yellow" = "yellow"))
    })
    
    output$dicePlot <- renderPlot({
      dice_df <- remainPlotDF(colors$colors_left)
      #d_c <- dice_df[[unrolled]]
      ggplot(dice_df,aes(x = unrolled, color = unrolled)) + 
        geom_point(size = 8, shape = 15, stat = "count") + 
        #ggtitle("Unrolled Dice") +
        theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"),
              axis.line.x = element_blank(),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line.y = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              legend.position="none") +
        scale_color_manual(values = c("blue" = "blue", "green" = "green", "orange" = "orange", "red" = "red", "yellow" = "yellow"))
    })
    
    win_status <- win_check(main_matrix$df)
    if(win_status) {
      print("winner")
      game_over(TRUE)
      
      p1_adj = finalScore(rankedList(unlist(find_camel_order(main_matrix$df), use.names=FALSE)), p1_wl_hand$p1w, p1_wl_hand$p1ws, p1_wl_hand$p1l, p1_wl_hand$p1ls)
      p2_adj = finalScore(rankedList(unlist(find_camel_order(main_matrix$df), use.names=FALSE)), p2_wl_hand$p2w, p2_wl_hand$p2ws, p2_wl_hand$p2l, p2_wl_hand$p2ls)
      
      player_score$ps_df['player1score'] <- player_score$ps_df['player1score'] + p1_adj
      player_score$ps_df['player2score'] <- player_score$ps_df['player2score'] + p2_adj
      
      if(player_score$ps_df[['player1score']] > player_score$ps_df[['player2score']]){
        winName("PLAYER 1 WINS!")
      }
      else{
        winName("PLAYER 2 WINS!")
      }
      print(player_score$ps_df['player1score'])
      print(player_score$ps_df['player2score'])
      
      shinyjs::disable("camelBet")
      shinyjs::disable("pickCamel")
      shinyjs::disable("rollDice")
      shinyjs::disable("selectFinal")
      shinyjs::disable("winBet")
      shinyjs::disable("loseBet")
      shinyjs::show("aiTurnNoClick")
      shinyjs::hide("aiTurnMove")
      print("hiding")
      
      # colors$colors_left <- reset_camels(colors$colors_left)
      # main_matrix$df <- sboard()
      # order_list <- find_camel_order(main_matrix$df)
      # player1$p1df = player1_df
      # player2$p2df = player2_df
      # player_score$ps_df['player1score'] <- 0
      # player_score$ps_df['player2score'] <- 0
      # camel_tiled$ct_df <- start_camel_tile_df
    }
    
    hnd_tst <- hand_plot()
    p_hand_view$phandv <- hnd_tst
  }
  
  newWeightsBias <- function(){
    set_neurons <- c(37, input$slider1, input$slider2, input$slider3, 16)
    neuron_num(set_neurons)
    print(neuron_num())
    for(i in 1:layers){
      wnam <- paste("n_wmat", i, sep = "")
      assign(wnam, matrix(rnorm(neuron_num()[i]*neuron_num()[i+1], mean=0, sd=1), neuron_num()[i+1], neuron_num()[i]) * sqrt(2/neuron_num()[i]))
      bnam <- paste("n_bmat", i, sep = "")
      assign(bnam, matrix(rnorm(neuron_num()[i+1], mean=0, sd=1), neuron_num()[i+1], 1) * sqrt(2/neuron_num()[i]))
    }
    
    wmat4r(n_wmat4)
    wmat3r(n_wmat3)
    wmat2r(n_wmat2)
    wmat1r(n_wmat1)
    
    bmat4r(n_bmat4)
    bmat3r(n_bmat3)
    bmat2r(n_bmat2)
    bmat1r(n_bmat1)
    
    ai_wl <- data.frame(winner = p1_wl_hand$p1w,
                        loser = p1_wl_hand$p1l
    )
    
    order_list <- find_camel_order(main_matrix$df)
    initial_inputs <- aiInputs(colors$colors_left, camel_tiled$ct_df, main_matrix$df, ai_wl, order_list)
    print(initial_inputs)
    outputLayer <- neuronChoice(initial_inputs)
    print("new weight")
  }
  
  observeEvent(input$changeNeurons, {
    newWeightsBias()
  })
  
  #When a card is picked for current leg of the race
  pickCamelChoice <- function(camel_pick){
    search_value <- camel_tiled$ct_df[camel_pick] %>%
      filter(camel_tiled$ct_df[camel_pick] == max(camel_tiled$ct_df[camel_pick]))
    
    rc_find <- which(camel_tiled$ct_df[camel_pick] == search_value[,], arr.ind = T)
    
    card_value <- camel_tiled$ct_df[camel_pick][rc_find,1]
    
    camel_tiled$ct_df[camel_pick][rc_find] <- 0
    
    if (search_value[,] == 2){
      camel_server_temp <- camel_server()[! camel_server() %in% camel_pick]
      camel_server(camel_server_temp)
    }
    
    
    if (playerTurn() == "player1") {
      player1$p1df[camel_pick][rc_find] = search_value[,]
      playerTurn("player2")
    }
    else {
      player2$p2df[camel_pick][rc_find] = search_value[,]
      playerTurn("player1")
    }
    
    hnd_graph <- hand_plot()
    p_hand_view$phandv <- hnd_graph
    
    p1_hand_plot_df <- playerHandVisual(player1$p1df)
    gg_p1_hand <- p_hand_plot(p1_hand_plot_df,"Player 1 Hand")
    p1_hand_view$p1handv <- gg_p1_hand
    
    p2_hand_plot_df <- playerHandVisual(player2$p2df)
    gg_p2_hand <- p_hand_plot(p2_hand_plot_df, "Player 2 Hand")
    p2_hand_view$p2handv <- gg_p2_hand
  }
  
  #When player selects card on the final winner of the race
  pickWinCamel <- function(win_camel_pick){
    if (playerTurn() == "player1") {
      p1_wl_hand$p1w <- win_camel_pick
      p1_wl_hand$p1ws <- wlScore(p1_wl_hand$p1w, p2_wl_hand$p2w)
      playerTurn("player2")
    }
    else {
      p2_wl_hand$p2w <- win_camel_pick
      p2_wl_hand$p2ws <- wlScore(p2_wl_hand$p2w, p1_wl_hand$p1w)
      playerTurn("player1")
    }
  }
  
  #When player selects card on the final loser of the race
  pickLoseCamel <- function(lose_camel_pick){
    if (playerTurn() == "player1") {
      p1_wl_hand$p1l <- lose_camel_pick
      p1_wl_hand$p1ls <- wlScore(p1_wl_hand$p1l, p2_wl_hand$p2l)
      playerTurn("player2")
    }
    else {
      p2_wl_hand$p2l <- lose_camel_pick
      p1_wl_hand$p2ls <- wlScore(p2_wl_hand$p2l, p1_wl_hand$p1l)
      playerTurn("player1")
    }
  }
  
  
  aiRunChoice <- function(ai_action){
    ai_func_input <- flatchoices[ai_action]
    if(ai_action == 1){
      rollDiceChoice()
    }
    else if(ai_action < 7){
      pickCamelChoice(ai_func_input)
    }
    else if(ai_action < 12){
      choice_num <- nchar(flatchoices[ai_action])
      ai_func_input <- substr(ai_func_input, 6, choice_num)
      pickWinCamel(ai_func_input)
    }
    else{
      choice_num <- nchar(flatchoices[ai_action])
      ai_func_input <- substr(ai_func_input, 6, choice_num)
      pickLoseCamel(ai_func_input)
    }
  }
  
  plotWeights <- function(){
    #DFtranspose=""
    col_count <- 0
    x_start_vec <- rep(1, 90)
    x_end_vec <- rep(2, 90)
    y_end_vec <- 1:90
    for(i in 1:ncol(wmat1r())) {
      col <- wmat1r()[,i]
      y_start_vec <- rep(1+col_count, 90)*3
      neuron_start_df <- data.frame (x_beg = x_start_vec,
                                     y_beg = y_start_vec,
                                     x_end = x_end_vec,
                                     y_end = y_end_vec,
                                     wmat_weight = col
      )
      if(col_count == 0){
        neuron_df <- neuron_start_df
        #print(neuron_df)
      }
      else{
        neuron_df <- rbind(neuron_df, neuron_start_df)
        #rownames(DFtranspose) <- neuron_df[1, ]
      }
      col_count <- col_count + 1
      #print(col_count)
    }
    print(neuron_df)
    return(neuron_df)
    #ggplot() +
      #geom_segment(aes(x=neuron_df["x_beg"],y=neuron_df["y_beg"],xend=neuron_df["x_end"],yend=neuron_df["y_end"]))
  }
  
  neuronChoice <- function(inputs){
    activation1 = newActivation(wmat1r(),bmat1r(),inputs)
    activation2 = newActivation(wmat2r(),bmat2r(),activation1)
    activation3 = newActivation(wmat3r(),bmat3r(),activation2)
    activation4 = newActivation(wmat4r(),bmat4r(),activation3)
    
    input_act(inputs)
    act1(activation1)
    act2(activation2)
    act3(activation3)
    act4(activation4)
    
    return(activation4)
  }
  
  plotNeurons <- function(){
    #neuron_length_vec <- c(32,90,90,90,16)
    #neuron_length_vec <- c(16,90,90,90,32)
    neuron_length_vec <- rev(neuron_num())
    neuron_max <- max(neuron_length_vec)
    #activations <- list(input_act(),act1(),act2(),act3(),act4())
    activations <- list(act4(),act3(),act2(),act1(),input_act())
    shape_size <- c(18,3,3,3,8)
    
    #space_mod <- c(3,1,1,1,6)
    x_count <- 0
    x_start_vec <- rep(1, 90)
    for(c in neuron_length_vec){
      
      if(c == neuron_max){
        repeat_value = 1
        x_adjust = 0
      }
      else{
        repeat_value = (neuron_max - (neuron_max/20)) / c
        x_adjust = neuron_max/40
      }
      
      y_start_vec <- rep(x_count+1, c)
      x_start_vec <- (1:c)*repeat_value + x_adjust
      
      #y_start_vec <- rep(1+col_count, 90)*3
      if(x_count == 4){
        neuron_col <- activations[[x_count+1]]
      }
      else{
        neuron_col <- activations[[x_count+1]][,1]
      }
      
      if(x_count == 0){
        neuron_l <- round(activations[[x_count+1]][,1],3)
      }
      else{
        neuron_l <- rep("", c)
      }
      
      #shape_ref <- shape_size[[x_count+1]]
      shape_ref <- 9 / (c/30)
      shape_col <- rep(shape_ref, c)
      
      neuron_start_df <- data.frame (x_beg = x_start_vec,
                                     y_beg = y_start_vec,
                                     neuron_act = neuron_col,
                                     shape_list = shape_col,
                                     neuron_label = neuron_l
                                     )
      
      if(x_count == 0){
        neuron_df <- neuron_start_df
        #print(neuron_df)
        }
      else{
        neuron_df <- rbind(neuron_df, neuron_start_df)
        #rownames(DFtranspose) <- neuron_df[1, ]
        }
      x_count <- x_count + 1
      #print(act4())
      #print(col_count)
    }
    #print(activations[[x_count+1]])
    
    return(neuron_df)
  }
  
  plotNueronLabels <- function(){
    #neuron_length_vec <- c(32,90,90,90,16)
    #neuron_length_vec <- c(16,90,90,90,32)
    neuron_length_vec <- rev(neuron_num())
    neuron_max <- max(neuron_length_vec)
    #activations <- list(input_act(),act1(),act2(),act3(),act4())
    activations <- list(act4(),act3(),act2(),act1(),input_act())
    shape_size <- c(16,3,3,3,8)
    space_mod <- c(3,1,1,1,6)
    x_count <- 0
    
    y_start_vec <- rep(1,16)
    repeat_value = (neuron_max - (neuron_max/20)) / 16
    x_adjust = neuron_max/40
    x_start_vec <- (1:16)*repeat_value + x_adjust
    #y_start_vec <- rep(1+col_count, 90)*3
    neuron_col <- activations[[1]][,1]
    label_ref <- shape_size[[x_count+1]]
    neuron_label_df <- data.frame (x_l_beg = x_start_vec,
                                   y_l_beg = y_start_vec,
                                   neuron_label = label_ref
                                   )
    
    return(neuron_label_df)
  }
  
  showAINeuron <- function(){
    ai_wl <- data.frame(winner = p1_wl_hand$p1w,
                        loser = p1_wl_hand$p1l
    )
    
    order_list <- find_camel_order(main_matrix$df)
    initial_inputs <- aiInputs(colors$colors_left, camel_tiled$ct_df, main_matrix$df, ai_wl,order_list)
    outputLayer <- neuronChoice(initial_inputs)
    ai_index_choice <- which.max(outputLayer)
    #rightChoice(sim_matrix, sim_hand_matrix, remain_camels, sim_wl, run_iter)
    p1_wl <- p1_place_df
    sim_wlrc <- c(p1_wl[1,1],p1_wl[1,2])
    ai_start_label(rev(round(outputLayer,3)))
    #plotNeurons()
  }
  
  output$Text <- renderText({ "This now works" })
  
  # observe({
  #   x <- input$pickCamel
  #   
  #   updateRadioButtons(session, "camelBet",
  #                      label = NULL,
  #                      choices = camel_server(),
  #                      selected = camel_server()[1])
  # })
  
  observeEvent(input$testgraph, {
    plotWeights()
  })
  
  output$testSegment2 <- renderPlot({
    neuron_df <- plotWeights()
    ggplot() +
      geom_segment(aes(x = neuron_df[["x_beg"]],
                       y = neuron_df[["y_beg"]],
                       xend = neuron_df[["x_end"]],
                       yend = neuron_df[["y_end"]],
                       alpha = neuron_df[["wmat_weight"]])
                   )
  })
  
  output$plotLayers <- renderPlot({
    neuron_df <- plotNeurons()
    #neuron_l_df <- plotNueronLabels()
    
    neuron_length_vec <- rev(neuron_num())
    neuron_max <- max(neuron_length_vec)
    repeat_value = (neuron_max - (neuron_max/20)) / neuron_length_vec[1]
    x_adjust = neuron_max/40
    
    x_start_vec <- (1:neuron_length_vec[1])*repeat_value + x_adjust
    x_start_value <- x_start_vec[1]
    x_end_value <- rev(x_start_vec)[1]
    
    ggplot(neuron_df, aes(x = x_beg, y = y_beg)) +
      geom_point(size = neuron_df[["shape_list"]], shape = 21, color="grey", fill=alpha("green",neuron_df[["neuron_act"]])) +
      geom_text(aes(label=neuron_df[["neuron_label"]])) +
      ggtitle("Neuron Choice") +
      theme(
        axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_text(size=10, face="bold"),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size=22,hjust = .5,vjust=0, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none") +
      scale_x_continuous(breaks=seq(x_start_value,x_end_value,repeat_value),labels = flat_word_choices) +
      scale_y_continuous(breaks=seq(1,5,1),labels = rev(y_axis_labels))
  })
  
  output$activationNeurons <- renderPlot({
    neuron_df <- plotNeurons()
    
    ggplot(neuron_df, aes(x = x_beg, y = y_beg)) +
      geom_point(size = 16, shape = 16, color="green", alpha = neuron_df[["neuron_act"]]) +
      theme(
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5,vjust=-10, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none")
  })
  
})

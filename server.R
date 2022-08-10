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
  
  
  p_hand_view <- reactiveValues(phandv = 0)
  p1_hand_view <- reactiveValues(p1handv = 0)
  p2_hand_view <- reactiveValues(p2handv = 0)
  
  
  player1 <- reactiveValues(p1df = player1_df)
  player2 <- reactiveValues(p2df = player2_df)
  
  playerTurn <- reactiveVal({
    "player1"
  })
  
  camel_server <- reactiveVal({c("blue","green","orange","red","yellow")})
  
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
  
  ######################################################
  
  
  #When a die is rolled
  observeEvent(input$rollDice, {
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
      player1$p1df = player1_df
      player2$p2df = player2_df
      camel_tiled$ct_df <- start_camel_tile_df
    }
    
    if (playerTurn() == "player1") {
      playerTurn("player2")
    }
    else {
      playerTurn("player1")
    }
    
    output$boardPlot <- renderPlot({
      board_df <- boardChangeDF(main_matrix$df)
      ggplot(board_df,aes(x_axis, y_axis, color = c_c)) + 
        geom_point(size = 6, shape = 17) + 
        theme(axis.line.x = element_line(colour = "black"),
              axis.text.x = element_text(size=12, face="bold", colour = "black"),
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
      colors$colors_left <- reset_camels(colors$colors_left)
      main_matrix$df <- sboard()
      order_list <- find_camel_order(main_matrix$df)
      player1$p1df = player1_df
      player2$p2df = player2_df
      player_score$ps_df['player1score'] <- 0
      player_score$ps_df['player2score'] <- 0
      camel_tiled$ct_df <- start_camel_tile_df
    }
    
    hnd_tst <- hand_plot()
    p_hand_view$phandv <- hnd_tst
  })
  
  
  observeEvent(input$pickCamel, {
    search_value <- camel_tiled$ct_df[input$camelBet] %>%
      filter(camel_tiled$ct_df[input$camelBet] == max(camel_tiled$ct_df[input$camelBet]))
    
    rc_find <- which(camel_tiled$ct_df[input$camelBet] == search_value[,], arr.ind = T)
    
    card_value <- camel_tiled$ct_df[input$camelBet][rc_find,1]
    
    camel_tiled$ct_df[input$camelBet][rc_find] <- 0
    
    if (search_value[,] == 2){
      camel_server_temp <- camel_server()[! camel_server() %in% input$camelBet]
      camel_server(camel_server_temp)
    }
    
    
    if (playerTurn() == "player1") {
      player1$p1df[input$camelBet][rc_find] = search_value[,]
      playerTurn("player2")
    }
    else {
      player2$p2df[input$camelBet][rc_find] = search_value[,]
      playerTurn("player1")
    }
    
    hnd_tst <- hand_plot()
    p_hand_view$phandv <- hnd_tst
    
    p1_hand_plot_df <- playerHandVisual(player1$p1df)
    gg_p1_hand <- p_hand_plot(p1_hand_plot_df,"Player 1 Hand")
    p1_hand_view$p1handv <- gg_p1_hand
    
    p2_hand_plot_df <- playerHandVisual(player2$p2df)
    gg_p2_hand <- p_hand_plot(p2_hand_plot_df, "Player 2 Hand")
    p2_hand_view$p2handv <- gg_p2_hand
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
  })
  
  
  observe({
    x <- input$pickCamel
    
    updateRadioButtons(session, "camelBet",
                       label = "Camel",
                       choices = camel_server(),
                       selected = "blue")
  })
  
  
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
  
  output$pTurn <- renderText({
    playerTurn()
  })
  
  output$boardTable <- renderTable({
    main_matrix$df
  })
  
  output$boardPlot <- renderPlot({
    board_df <- boardChangeDF(main_matrix$df)
    ggplot(board_df,aes(x_axis, y_axis, color = c_c)) + 
      geom_point(size = 6, shape = 17) + 
      theme(axis.line.x = element_line(colour = "black"),
            axis.text.x = element_text(size=12, face="bold", colour = "black"),
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
  
  output$p1HandTitle <- renderText({
    "Player 1 Hand"
  })
  
  output$p2HandTitle <- renderText({
    "Player 2 Hand"
  })
  
  output$unrolledTitle <- renderText({
    #"Last Roll"
    paste("<b>Unrolled Dice</b>")
  })
  
  output$lastRollTitle <- renderText({
    #"Last Roll"
    paste("<b>Last Roll</b>")
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
      ggtitle("Available Cards") +
      scale_x_discrete(expand = c(0, 1.9)) +
      scale_y_discrete(expand = c(0, 1.9))
    return(handggplot)
  }
  
  
  p_hand_plot <- function(p_hand_df, g_title){
    
    if (dim(p_hand_df)[1] == 0){
      handggplot <- ggplot() +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5,vjust=-10, face="bold")) +
        ggtitle(g_title)
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
      ggtitle(g_title) +
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
    
    run_iter <- 5
    
    sim_matrix <- sboard()
    sim_hand_matrix <- start_camel_tile_df
    
    p1_wl <- p1_place_df
    p2_wl <- p2_place_df
    
    remain_camels <- camel_list
    
    sim_turn = "p2"
    
    for(k in 1:50000){
      
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
      
      initial_inputs <- aiInputs(remain_camels, sim_hand_matrix, sim_matrix, sim_place_df)
      
      outputLayer <- neuronChoice(initial_inputs)
      #learn(initial_inputs)
      
      test_output <- rightChoice(sim_matrix, sim_hand_matrix, remain_camels, sim_wl, run_iter)
      
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
    write.csv(save_set,"data\\simulation_results.csv", row.names = FALSE)
  })
  
  observeEvent(input$learnSim, {
    for(i in 1:1000){
      learn2()
    }
    print("done")
  })
  
  observeEvent(input$debugSim, {
    print(bmat4r())
  })
  
  
  
  learn2 <- function(){
    #These are empty matrices used for the adjustments needed 
    for(i in 1:layers){
      rcw <- paste("change_w", i, sep = "")
      assign(rcw, matrix(0, neurons[i+1], neurons[i]))
      rcb <- paste("change_b", i, sep = "")
      assign(rcb, matrix(0, neurons[i+1], 1))
    }
    
    sum_matrix_cost <- 0
    
    #how many iterations it will do before taking the average and adjusting the neurons
    run_trials <- 50
    
    for(r in 1:run_trials){
      
      #train_matrix <- neuronChoice(input_vector)
      
      rand_input <- sample(1:15, 1)
      randchoices <- c("blue","green","orange","red","yellow",
                       "wtileblue","wtilegreen","wtileorange","wtilered","wtileyellow",
                       "ltileblue","ltilegreen","ltileorange","ltilered","ltileyellow")
      
      if(rand_input < 6){
        sim_results <- trial_results_df %>%
          filter(trial_results_df[,33] == randchoices[rand_input])
      }
      else if(rand_input < 11){
        sim_results <- trial_results_df %>%
          filter(stringr::str_detect(trial_results_df[,33], "wtile"))
      }
      else{
        sim_results <- trial_results_df %>%
          filter(stringr::str_detect(trial_results_df[,33], "ltile"))
      }
      sim_row <- sim_results[sample(nrow(sim_results), 1), ]
      sim_answer <- sim_row[[1,33]]
      
      sim_index <- match(sim_answer,randchoices)
      sim_row_input <- sim_row[1:32]
      sim_row_m <- t(as.matrix(sim_row_input))
      
      activation1 = newActivation(wmat1r(),bmat1,sim_row_m)
      activation2 = newActivation(wmat2r(),bmat2,activation1)
      activation3 = newActivation(wmat3r(),bmat3,activation2)
      activation4 = newActivation(wmat4r(),bmat4,activation3)
      
      train_max <- which(activation4 == max(activation4), arr.ind=T)
      train_index <- train_max[[1,1]]
      train_answer <- flatchoices[sim_index]
      
      #Creates zero matrix with the right answer as one
      answer_neuron_matrix <- matrix(.01, 16, 1)
      
      #choseninput is the right answer
      answer_neuron_matrix[train_index,] <- .99
      
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
      cact3 = t(as.matrix(wmat4)) %*% cdicttemp
      dsigmoid3 = activation3 * (1-activation3)
      cdicttemp = dsigmoid3 * cact3
      cbias3 = cdicttemp
      cwmat3 = cdicttemp %*% t(activation2)
      cact2 = t(as.matrix(wmat3)) %*% cdicttemp
      
      dsigmoid2 = activation2 * (1-activation2)
      cdicttemp = dsigmoid2 * cact2
      cbias2 = cdicttemp
      cwmat2 = cdicttemp %*% t(activation1)
      cact1 = t(as.matrix(wmat2)) %*% cdicttemp
      
      dsigmoid1 = activation1 * (1-activation1)
      cdicttemp = dsigmoid1 * cact1
      cbias1 = cdicttemp
      cwmat1 = cdicttemp %*% t(sim_row_m)
      
      #summatrixcost = matrixcost + summatrixcost
      sum_matrix_cost = sum(matrixcost) + sum_matrix_cost
      
      #print("cb")
      #print(cbias4)
      #print("c_b")
      #print(change_b4)
      #print(change_w4)
      #print(cwmat4)
      change_w4 <- change_w4 + cwmat4
      change_w3 <- change_w3 + cwmat3
      change_w2 <- change_w2 + cwmat2
      change_w1 <- change_w1 + cwmat1
      
      change_b4 <- change_b4 + cbias4
      change_b3 <- change_b3 + cbias3
      change_b2 <- change_b2 + cbias2
      change_b1 <- change_b1 + cbias1
    }
    stepsize=.01
    #print(change_b4)
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
    #print(bmat4r())
    if(sample(1:10, 1) > 8){
      print(sum_matrix_cost/run_trials)
    }
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
  
  
})

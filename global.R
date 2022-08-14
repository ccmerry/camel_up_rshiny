library(dplyr)
library(ggplot2)
library(fontawesome)
library(shinyjs)

camel_list = c("blue","green","orange","red","yellow")

stable_camel = c("blue","green","orange","red","yellow")

start_camel_tile_df <- data.frame(blue = c(5,3,2),
                                  green = c(5,3,2),
                                  orange = c(5,3,2),
                                  red = c(5,3,2),
                                  yellow = c(5,3,2)
)

player1_df <- data.frame(blue = c(0,0,0),
                         green = c(0,0,0),
                         orange = c(0,0,0),
                         red = c(0,0,0),
                         yellow = c(0,0,0)
)


p1_place_df <- data.frame(winner = c(""),
                          loser = c("")
)


player2_df <- data.frame(blue = c(0,0,0),
                         green = c(0,0,0),
                         orange = c(0,0,0),
                         red = c(0,0,0),
                         yellow = c(0,0,0)
)


p2_place_df <- data.frame(winner = c(""),
                          loser = c("")
)


win_cards <- data.frame(blue = c(8,6),
                        green = c(8,6),
                        orange = c(8,6),
                        red = c(8,6),
                        yellow = c(8,6)
)


lose_cards <- data.frame(blue = c(8,6),
                        green = c(8,6),
                        orange = c(8,6),
                        red = c(8,6),
                        yellow = c(8,6)
)

rank_list_num <- c("1.","2.","3.","4.","5.")

empty_training_set <- data.frame(matrix(ncol = 33, nrow = 0))

flatchoices = c('roll','blue', 'green', 'yellow', 'orange',
                'wtilewhite', 'wtileblue', 'wtilegreen', 'wtileyellow','wtileorange', 'wtilewhite',
                'ltileblue', 'ltilegreen','ltileyellow','ltileorange','ltilewhite')

board_x_ticks <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17")
board_y_ticks <- c("1","2","3","4","5")

#country_codes_df <- read.csv("data/country_codes.csv")
if(file.exists("data/simulation_results.csv")){
  trial_results_df <- read.csv("data/simulation_results.csv")
}

if(file.exists("data\\wmatSave1.csv")){
  trial_results_df <- read.csv("data/simulation_results.csv")
}

gtest <- 1

remove_die <- function(rolled_color, camel_colors) {
  camel_colors3 <- camel_colors[camel_colors != rolled_color]
  return(camel_colors3)
}

board_change <- function(roll_result, matrix_of_board) {
  
  y <- 1
  row_count <- 1
  roll_location <- which(matrix_of_board == roll_result[2], arr.ind = T)
  
  if(dim(roll_location)[1] != 0) {
    
    new_column <- strtoi(roll_location[1,2]) + strtoi(roll_result[1])
    
    if(new_column >= 20) {
      new_column <- 20
    }
  } else {
    new_column <- strtoi(roll_result[1])
    
    if(new_column >= 20) {
      new_column <- 20
    }
  }
  for (row in matrix_of_board[1:7,new_column]) {
    
    if(row == "0") {
      
      destination_row <- row_count
      new_row_count <- nrow(matrix_of_board) - (destination_row )
      
      if(dim(roll_location)[1] != 0) {
        
        curr_row_count <- nrow(matrix_of_board) - (roll_location[[1,1]])
        curr_new <- c(new_row_count,curr_row_count)
        move_row_count <- min(curr_new)
        final_r <- roll_location[1,1] + move_row_count
        move_matrix <- matrix_of_board[roll_location[1,1]:final_r, roll_location[1,2]]
        
        #number of rows that need to be moved
        rows_total <- length(move_matrix) + (row_count - 1)
        
        #moves camel and camels above it to the new column
        matrix_of_board[row_count:rows_total,new_column] <- move_matrix
      } else {
        matrix_of_board[row_count,new_column] <- roll_result[2]
      }
      break
    }
    row_count <- row_count + 1
  }
  if(dim(roll_location)[1] != 0) {
    
    #sets old column to zero
    matrix_of_board[roll_location[1,1]:nrow(matrix_of_board), roll_location[1,2]] <- 0
  }
  return(matrix_of_board)
}


sboard <- function() {
  new_matrix <- matrix(0, 7, 20)
  
  start_camels = c("blue","green","orange","red","yellow")
  dice_options <- c(1,2,3)
  
  for (x in 1:length(start_camels)) {
    rand_camel <- sample(start_camels, 1)
    die_result <- sample(dice_options, 1)
    
    row_count <- 1
    
    for (row in new_matrix[1:7,die_result]) {
      if (new_matrix[row_count, die_result] == 0) {
        new_matrix[row_count, die_result] <- rand_camel
        break
      }
      else {
        row_count <- row_count + 1
      }
    }
    start_camels <- start_camels[start_camels != rand_camel]
    
  }
  return(new_matrix)
}

#checks if a camel has passed the finish line
win_check <- function(win_board) {
  
  has_won <- FALSE
  win_c <- c("blue","green","orange","red","yellow")
  for (i in win_c) {
    roll_location <- which(win_board == i, arr.ind = T)
    if (dim(roll_location)[1] != 0){
      if (roll_location[2] >= 17){
        has_won <- TRUE
        break
      }
    }
  }
  return(has_won)
}


dice_roll <- function(dice_remaining) {
  
  dice_color_return <- sample(dice_remaining, 1)
  
  dice_options <- c(1,2,3)
  dice_number_return <- sample(dice_options, 1)
  
  dice_roll_result <- c(dice_number_return, dice_color_return)
  
  return(dice_roll_result)
}


reset_camels <- function(empty_camels) {
  empty_camels <- c("blue","green","orange","red","yellow")
  return(empty_camels)
}


find_camel_order <- function(current_matrix) {
  camel_order <- c()
  
  m_rows <- nrow(current_matrix)
  m_cols <- ncol(current_matrix)
  
  for (c in 1: m_cols) {
    
    #col_check <- (c * -1) - 1
    #col_check <- m_cols - c + 1
    col_check <- c
    
    for (r in 1: m_rows){
      if(!is.null(current_matrix[r,col_check])){
        if (current_matrix[r,col_check] != "0"){
          camel_order <- append(camel_order, current_matrix[r,col_check])
        }
      }
    }
  }
  return(rev(camel_order))
}

calc_score <- function(player_hand, first_camel, second_camel) {
  first_score <- as.vector(colSums(player_hand[first_camel]))
  second_score <- as.vector(colSums(player_hand[second_camel]))
  first_na <- as.vector(colSums(player_hand[first_camel]!=0))
  second_na <- as.vector(colSums(player_hand[second_camel]!=0))
  num_of_cards <- sum(as.vector(colSums(player_hand!=0)))
  hand_score <- first_score + second_score - (2*(num_of_cards - first_na - second_na))
  print(hand_score)
  return(hand_score)
}


boardChangeDF <- function(boardm){
  
  b_df <- data.frame(x_axis = c(0,0,0,0,0),
                     y_axis = c(0,0,0,0,0),
                     c_c = c("blue","green","orange","red","yellow")
  )
  
  for(c in camel_list){
    c_loc <- which(boardm == c, arr.ind = T)
    b_df$x_axis[b_df$c_c == c] <- c_loc[[1,2]]
    b_df$y_axis[b_df$c_c == c] <- c_loc[[1,1]]
  }
  return(b_df)
}

remainPlotDF <- function(dice_tbr){
  num_camels <- length(dice_tbr)
  if(num_camels != 0){
    camel_roll_v <- vector(mode="character", length=num_camels)
    tracker <- 1
    for(c in dice_tbr){
      if(c %in% camel_list){
        camel_roll_v[tracker] <- c
        tracker <- tracker + 1
      }
    }
    tbr_df <- data.frame(unrolled = camel_roll_v)
    return(tbr_df)
  }
  else{
    tbr_df <- data.frame(unrolled = c(""))
    return(tbr_df)
  }
}


rankedList <- function(curr_rnk){
  
  rnk_df <- data.frame(Place = rank_list_num,
                       Camel = curr_rnk
  )
  
  return(rnk_df)
}


#Handles Available Cards Visual

columnPlayerHand <- function(hand_data, square_color, idvar, text_color){
  gplot <- list(
    geom_point(aes(x,y),
               data  = hand_data,
               color = square_color,
               shape = 15,
               size = 8.5),
    geom_text(data = hand_data,
              aes(label=idvar), 
              color = text_color))
  return(gplot)
}

handColumnClean <- function(column_data){
  filter_hand <- column_data %>% 
    filter(rowSums(across(where(is.numeric)) > 0) > 0)
  
  return(filter_hand)
}


valueHand <- function(rep_df, curr_list = c()){
  filter_hand <- rep_df %>% 
    filter(rowSums(across(where(is.numeric)) > 0) > 0)
  
  vector_hand <- filter_hand[,1]
  
  append_list <- append(curr_list, vector_hand)
  return(append_list)
}

appendHand <- function(rep_df, rep_name, curr_list = c()){
  no_df <- rep_df  %>% 
    filter(rowSums(across(where(is.numeric)) > 0) > 0)
  
  rep_num <- nrow(no_df)
  rep_vector <- rep(rep_name, rep_num)
  appended_list <- append(curr_list, rep_vector)
  return(appended_list)
}

xHand <- function(rep_df, curr_list = c()){
  if (length(curr_list) == 0){
    last_item <- 1
  }else{
    last_item <- tail(curr_list, n=1)
    last_item <- last_item + 1
  }
  rep_num <- nrow(rep_df)
  rep_vector <- rep(last_item, rep_num)
  appended_list <- append(curr_list, rep_vector)
  return(appended_list)
}

yHand <- function(rep_df, curr_list = c()){
  
  ylist <- c(3,2,1)
  y_vector <- c()
  rep_num <- nrow(rep_df)
  
  if (rep_num == 0){
    
  }else{
    start_vector <- 4 - rep_num
    y_vector <- ylist[start_vector:3]
  }
  
  appended_list <- append(curr_list, y_vector)
  return(appended_list)
}

#Handles Player hand Visul

playerHandVisual <- function(hand_df){
  color_list = c()
  text_list = c()
  x_list = c()
  x_count = 1
  y_list = c()
  y_count = 1
  
  for (c in stable_camel){
    for (r in hand_df[[c]]){
      if (r != 0){
        color_list = append(color_list,c)
        text_list = append(text_list,r)
        x_list = append(x_list,x_count)
        x_count = x_count + 1
        y_list = append(y_list,y_count)
      }
    }
  }
  
  hand_vis_df <- data.frame(color_label = color_list,
                            text_label = text_list,
                            x_vis_label = x_list,
                            y_vis_label = y_list)
  return(hand_vis_df)
}

global_hand_plot <- function(){
  
  start_camel_tile_df
  
  b_c_hand <- handColumnClean(start_camel_tile_df['blue'])
  value_hand <- valueHand(start_camel_tile_df['blue'])
  x_append <- xHand(b_c_hand)
  y_append <- yHand(b_c_hand)
  color_append <- appendHand(start_camel_tile_df['blue'],"blue")
  
  value_hand <- valueHand(start_camel_tile_df['green'], value_hand)
  g_c_hand <- handColumnClean(start_camel_tile_df['green'])
  color_append <- appendHand(start_camel_tile_df['green'],"green",color_append)
  x_append <- xHand(g_c_hand,x_append)
  y_append <- yHand(g_c_hand, y_append)
  
  value_hand <- valueHand(start_camel_tile_df['orange'], value_hand)
  o_c_hand <- handColumnClean(start_camel_tile_df['orange'])
  color_append <- appendHand(start_camel_tile_df['orange'],"orange",color_append)
  x_append <- xHand(o_c_hand,x_append)
  y_append <- yHand(o_c_hand, y_append)
  
  value_hand <- valueHand(start_camel_tile_df['red'], value_hand)
  r_c_hand <- handColumnClean(start_camel_tile_df['red'])
  color_append <- appendHand(start_camel_tile_df['red'],"red",color_append)
  x_append <- xHand(r_c_hand,x_append)
  y_append <- yHand(r_c_hand, y_append)
  
  value_hand <- valueHand(start_camel_tile_df['yellow'], value_hand)
  y_c_hand <- handColumnClean(start_camel_tile_df['yellow'])
  color_append <- appendHand(start_camel_tile_df['yellow'],"yellow",color_append)
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
      scale_x_discrete(expand = c(0, 1.9)) +
      scale_y_discrete(expand = c(0, 1.9))
    return(handggplot)
  }
}

wlScore <- function(curr_pick, other_pick){
  ret_score = 8
  if (curr_pick == other_pick){
    ret_score = 6
  }
  return(ret_score)
}

finalScore <- function(c_rnking, pick_w, w_score, pick_l, l_score){
  total_change = 0
  
  if(c_rnking[1] == pick_w ||
     pick_w == ""){
    total_change = w_score
  }
  else{
    total_changes = -1
  }
  
  if(tail(c_rnking, n=1) == pick_l ||
     pick_l == ""){
    total_change = total_change + l_score
  }
  else{
    total_change = total_change - 1
  }
  return(total_change)
}

##### Machine Learning Section #####
##################################################################

#Create initial neuron layers and biases

#neurons = [32,90,90,90,16]
neurons <- c(32,90,90,90,16)

# '''Initialize zero matrices'''
# layers = len(neurons)-1
layers <- length(neurons) - 1
# y = 0

# randdict={}
random_dictionary <- list()


#Checks to see if file exists for already trained data. If not it creates random weights and biases
if(file.exists("data\\wmatSave1.csv")){
  for(i in 1:layers){
    
    wvar <- paste("wmatSave", i, sep = "")
    new_string <- c("data\\",wvar,".csv")
    w_read_string <- paste(new_string,collapse = "")
    
    wnam <- paste("wmat", i, sep = "")
    assign(wnam, read.csv(w_read_string))
    
    bvar <- paste("bmatSave", i, sep = "")
    new_string <- c("data\\",bvar,".csv")
    b_read_string <- paste(new_string,collapse = "")
    
    bnam <- paste("bmat", i, sep = "")
    assign(bnam, read.csv(b_read_string))
  }
}else{
  # Creates starting matrices for 
  for(i in 1:layers){
    wnam <- paste("wmat", i, sep = "")
    assign(wnam, matrix(rnorm(neurons[i]*neurons[i+1], mean=0, sd=1), neurons[i+1], neurons[i]))
    bnam <- paste("bmat", i, sep = "")
    assign(bnam, matrix(rnorm(neurons[i+1], mean=0, sd=1), neurons[i+1], 1))
  }
}

  

# for x in range(layers):
#   randdict["wmat{0}".format(y+1)] = np.zeros(shape=(neurons[y+1],neurons[y]))
#   randdict["bmat{0}".format(y+1)] = np.zeros(shape=(neurons[y],1))
#   y = y + 1
# 
# for s in range(len(neurons)-1):
#   currentlayer = neurons[s+1]
#   layerbefore = neurons[s]
#   randdict["wmat{0}".format(s+1)] = np.random.randn(neurons[s+1],neurons[s]) * np.sqrt(2/neurons[s])
#   randdict["bmat{}".format(s+1)] = np.random.randn(neurons[s+1],1) * np.sqrt(2/neurons[s])




sigmoid <- function(x){
  return(1/(1+exp(-x)))
}


learn <- function(){
  
  #These are empty matrices used for the adjustments needed 
  for(i in 1:layers){
    rcw <- paste("change_w", i, sep = "")
    assign(rcw, matrix(0, neurons[i+1], neurons[i]))
    rcb <- paste("change_b", i, sep = "")
    assign(rcb, matrix(0, neurons[i+1], 1))
  }
  #rcw4 = np.zeros(shape=(16,hlayer3))
  #rcw3 = np.zeros(shape=(hlayer3,hlayer2))
  #rcw2 = np.zeros(shape=(hlayer2,hlayer1))
  #rcw1 = np.zeros(shape=(hlayer1,32))
  #rcb4 = np.zeros(shape=(16,1))
  #rcb3 = np.zeros(shape=(hlayer3,1))
  #rcb2 = np.zeros(shape=(hlayer2,1))
  #rcb1 = np.zeros(shape=(hlayer1,1))
  
  
  
  
  #summatrixcost = 0
  sum_matrix_cost <- 0
  
  
  #'flatchoices = ['roll','btile', 'gtile', 'ytile', 'otile', 'wtile',\
  #'             'wblue', 'wgreen','wyellow','worange','wwhite',\
  #'             lblue', 'lgreen','lyellow','lorange','lwhite']
  #'     '''learniter is the batch size for training'''
  #'     learniter = 20
  #'     for i in range(learniter):
  #'         choseninput = random.randint(0,15)
  #'         if choseninput == 0:
  #'             randinfo = choosescenario(rollranswer,90000,'roll')
  #'         elif choseninput == 1:
  #'             randinfo = choosescenario(btileranswer,15500,'btile')
  #'         elif choseninput == 2:
  #'             randinfo = choosescenario(gtileranswer,15500,'gtile')
  #'         elif choseninput == 3:
  #'             randinfo = choosescenario(ytileranswer,15500,'ytile')
  #'         elif choseninput == 4:
  #'             randinfo = choosescenario(otileranswer,15500,'otile')
  #'         elif choseninput == 5:
  #'             randinfo = choosescenario(wtileranswer,15500,'wtile')
  #'         elif choseninput < 11:
  #'             randinfo = choosescenario(winnerranswer,350,flatchoices[choseninput])
  #'         else:
  #'             randinfo = choosescenario(loserranswer,780,flatchoices[choseninput])
  #'         
  #'         inputm = np.matrix(randinfo)
  #'         inputmatrix = np.transpose(inputm)
  #'         
  #'         activation1 = newactivation(wandb["wmat1"],wandb["bmat1"],inputmatrix)
  #'         
  #'         activation2 = newactivation(wandb["wmat2"],wandb["bmat2"],activation1)
  #'         
  #'         activation3 = newactivation(wandb["wmat3"],wandb["bmat3"],activation2)
  #'         
  #'         activation4 = newactivation(wandb["wmat4"],wandb["bmat4"],activation3)
  
  #how many iterations it will do before taking the average and adjusting the neurons
  run_trials <- 20
  
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
    
    #train_matrix <- neuronChoice(input_vector)
    
    activation1 = newActivation(wmat1,bmat1,sim_row_m)
    activation2 = newActivation(wmat2,bmat2,activation1)
    activation3 = newActivation(wmat3,bmat3,activation2)
    activation4 = newActivation(wmat4,bmat4,activation3)
    
    train_max <- which(activation4 == max(activation4), arr.ind=T)
    train_index <- train_max[[1,1]]
    train_answer <- flatchoices[sim_index]
  
    #Creates zero matrix with the right answer as one
    #choicematrixadj = np.zeros(shape=(16,1))
    answer_neuron_matrix <- matrix(.01, 16, 1)
    #choicematrix = choicematrixadj + .01
    
    #choseninput is the right answer
    #choicematrix[choseninput] = [.99]
    answer_neuron_matrix[train_index,] <- .99
    
    #Takes answers and subtracts 1 or 0 to find the cost derivative
    #newitem = activation4 - choicematrix
    newitem = activation4 - answer_neuron_matrix
    #cmatrix = np.transpose(newitem)
    cmatrix = t(newitem)
    #matrixcost = cmatrix * newitem
    matrixcost = cmatrix %*% newitem
    
    #Calcs the derivative of the sigmoid
    #dsigmoid4 = np.multiply(activation4, 1 - activation4)
    dsigmoid4 = activation4 * (1 - activation4)
    #Multiply the derivative of the sigmoid function by the cost by the previous layer neuron
    #cdicttemp = np.multiply(dsigmoid4,newitem*2)
    cdicttemp = dsigmoid4 * (newitem*2)
    #cbias4 = cdicttemp
    cbias4 = cdicttemp
    #cwmat4 = cdicttemp * np.transpose(activation3)
    cwmat4 = cdicttemp %*% t(activation3)
    #Calculates the changeactivation for previous layer
    #cact3 = np.transpose(wandb["wmat4"]) * cdicttemp
    cact3 = t(as.matrix(wmat4)) %*% cdicttemp
    #dsigmoid3 = np.multiply(activation3,1-activation3)
    dsigmoid3 = activation3 * (1-activation3)
    #cdicttemp = np.multiply(dsigmoid3,cact3)
    cdicttemp = dsigmoid3 * cact3
    #cbias3 = cdicttemp
    cbias3 = cdicttemp
    #cwmat3 = cdicttemp * np.transpose(activation2)
    cwmat3 = cdicttemp %*% t(activation2)
    #cact2 = np.transpose(wandb["wmat3"]) * cdicttemp
    cact2 = t(as.matrix(wmat3)) %*% cdicttemp
    
    #dsigmoid2 = np.multiply(activation2,1-activation2)
    dsigmoid2 = activation2 * (1-activation2)
    #cdicttemp = np.multiply(dsigmoid2,cact2)
    cdicttemp = dsigmoid2 * cact2
    #cbias2 = cdicttemp
    cbias2 = cdicttemp
    #cwmat2 = cdicttemp * np.transpose(activation1)
    cwmat2 = cdicttemp %*% t(activation1)
    #cact1 = np.transpose(wandb["wmat2"]) * cdicttemp
    cact1 = t(as.matrix(wmat2)) %*% cdicttemp
    
    #dsigmoid1 = np.multiply(activation1,1-activation1)
    dsigmoid1 = activation1 * (1-activation1)
    #cdicttemp = np.multiply(dsigmoid1,cact1)
    cdicttemp = dsigmoid1 * cact1
    #cbias1 = cdicttemp
    cbias1 = cdicttemp
    #cwmat1 = cdicttemp * inputm
    #inputm <- as.matrix(input_vector)
    cwmat1 = cdicttemp %*% t(sim_row_m)
    
    #summatrixcost = matrixcost + summatrixcost
    sum_matrix_cost = sum(matrixcost) + sum_matrix_cost
    
    #rcw4 = rcw4 + cwmat4
    #rcw3 = rcw3 + cwmat3
    #rcw2 = rcw2 + cwmat2
    #rcw1 = rcw1 + cwmat1
    
    #rcb4 = rcb4 + cbias4
    #rcb3 = rcb3 + cbias3
    #rcb2 = rcb2 + cbias2
    #rcb1 = rcb1 + cbias1
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
  stepsize=1
  
  wmat4 <- wmat4 - (change_w4/(run_trials * stepsize))
  wmat3 <- wmat3 - (change_w3/(run_trials * stepsize))
  wmat2 <- wmat2 - (change_w2/(run_trials * stepsize))
  wmat1 <- wmat1 - (change_w1/(run_trials * stepsize))
  
  bmat4 <- bmat4 - (change_b4/(run_trials * stepsize))
  bmat3 <- bmat3 - (change_b3/(run_trials * stepsize))
  bmat2 <- bmat2 - (change_b2/(run_trials * stepsize))
  bmat1 <- bmat1 - (change_b1/(run_trials * stepsize))
  
  #this print statement helps track how far off the learn matrix is
  print(sum_matrix_cost/run_trials)
  gtest <- gtest + 1
  
  #adjustwb("wmat4",rcw4,learniter)
  #adjustwb("wmat3",rcw3,learniter)
  #adjustwb("wmat2",rcw2,learniter)
  #adjustwb("wmat1",rcw1,learniter)
  #adjustwb("bmat4",rcb4,learniter)
  #adjustwb("bmat3",rcb3,learniter)
  #adjustwb("bmat2",rcb2,learniter)
  #adjustwb("bmat1",rcb1,learniter)
  #learnitercost = summatrixcost/learniter
  #return learnitercost
}

#' def adjustwb(weightorbias,adjustment,adjrunningcount):
#'     global stepsize
#'     global momentum
#'     wandb[weightorbias] = wandb[weightorbias] - (adjustment/adjrunningcount*stepsize)

adjustWB <- function(weightorbias,adjustment,adjrunningcount){
  adjusted_neuron = wandb[weightorbias] - (adjustment/adjrunningcount*stepsize)
}

#'def getranddictionary():
#'     global run
#'     rcw4 = np.zeros(shape=(16,hlayer3))
#'     rcw3 = np.zeros(shape=(hlayer3,hlayer2))
#'     rcw2 = np.zeros(shape=(hlayer2,hlayer1))
#'     rcw1 = np.zeros(shape=(hlayer1,32))
#'     rcb4 = np.zeros(shape=(16,1))
#'     rcb3 = np.zeros(shape=(hlayer3,1))
#'     rcb2 = np.zeros(shape=(hlayer2,1))
#'     rcb1 = np.zeros(shape=(hlayer1,1))
#'     summatrixcost = 0
#'     flatchoices = ['roll','btile', 'gtile', 'ytile', 'otile', 'wtile',\
#'                   'wblue', 'wgreen','wyellow','worange','wwhite',\
#'                  'lblue', 'lgreen','lyellow','lorange','lwhite']
#'     '''learniter is the batch size for training'''
#'     learniter = 20
#'     for i in range(learniter):
#'         choseninput = random.randint(0,15)
#'         if choseninput == 0:
#'             randinfo = choosescenario(rollranswer,90000,'roll')
#'         elif choseninput == 1:
#'             randinfo = choosescenario(btileranswer,15500,'btile')
#'         elif choseninput == 2:
#'             randinfo = choosescenario(gtileranswer,15500,'gtile')
#'         elif choseninput == 3:
#'             randinfo = choosescenario(ytileranswer,15500,'ytile')
#'         elif choseninput == 4:
#'             randinfo = choosescenario(otileranswer,15500,'otile')
#'         elif choseninput == 5:
#'             randinfo = choosescenario(wtileranswer,15500,'wtile')
#'         elif choseninput < 11:
#'             randinfo = choosescenario(winnerranswer,350,flatchoices[choseninput])
#'         else:
#'             randinfo = choosescenario(loserranswer,780,flatchoices[choseninput])
#'         
#'         inputm = np.matrix(randinfo)
#'         inputmatrix = np.transpose(inputm)
#'         
#'         activation1 = newactivation(wandb["wmat1"],wandb["bmat1"],inputmatrix)
#'         
#'         activation2 = newactivation(wandb["wmat2"],wandb["bmat2"],activation1)
#'         
#'         activation3 = newactivation(wandb["wmat3"],wandb["bmat3"],activation2)
#'         
#'         activation4 = newactivation(wandb["wmat4"],wandb["bmat4"],activation3)
#'         
#'         '''Takes the Activations and backpropagates'''
#'         
#'         '''Creates zero matrix with the right answer as one'''
#'         choicematrixadj = np.zeros(shape=(16,1))
#'         choicematrix = choicematrixadj + .01
#'         choicematrix[choseninput] = [.99]
#'         '''Takes answers and subtracts 1 or 0 to find the cost derivative'''
#'         newitem = activation4 - choicematrix
#'         cmatrix = np.transpose(newitem)
#'         matrixcost = cmatrix * newitem
#'         '''Calcs the derivative of the sigmoid'''
#'         dsigmoid4 = np.multiply(activation4, 1 - activation4)
#'         '''Multiply the derivative of the sigmoid function by the cost by the previous layer neuron'''
#'         cdicttemp = np.multiply(dsigmoid4,newitem*2)
#'         cbias4 = cdicttemp
#'         cwmat4 = cdicttemp * np.transpose(activation3)
#'         '''Calculates the changeactivation for previous layer'''
#'         cact3 = np.transpose(wandb["wmat4"]) * cdicttemp
#'         
#'         dsigmoid3 = np.multiply(activation3,1-activation3)
#'         cdicttemp = np.multiply(dsigmoid3,cact3)
#'         cbias3 = cdicttemp
#'         cwmat3 = cdicttemp * np.transpose(activation2)
#'         cact2 = np.transpose(wandb["wmat3"]) * cdicttemp
#'         
#'         dsigmoid2 = np.multiply(activation2,1-activation2)
#'         cdicttemp = np.multiply(dsigmoid2,cact2)
#'         cbias2 = cdicttemp
#'         cwmat2 = cdicttemp * np.transpose(activation1)
#'         cact1 = np.transpose(wandb["wmat2"]) * cdicttemp
#'         
#'         dsigmoid1 = np.multiply(activation1,1-activation1)
#'         cdicttemp = np.multiply(dsigmoid1,cact1)
#'         cbias1 = cdicttemp
#'         cwmat1 = cdicttemp * inputm
#'         
#'         summatrixcost = matrixcost + summatrixcost
#'     
#'         rcw4 = rcw4 + cwmat4
#'         rcw3 = rcw3 + cwmat3
#'         rcw2 = rcw2 + cwmat2
#'         rcw1 = rcw1 + cwmat1
#'         
#'         rcb4 = rcb4 + cbias4
#'         rcb3 = rcb3 + cbias3
#'         rcb2 = rcb2 + cbias2
#'         rcb1 = rcb1 + cbias1
#'     
#'     adjustwb("wmat4",rcw4,learniter)
#'     adjustwb("wmat3",rcw3,learniter)
#'     adjustwb("wmat2",rcw2,learniter)
#'     adjustwb("wmat1",rcw1,learniter)
#'     adjustwb("bmat4",rcb4,learniter)
#'     adjustwb("bmat3",rcb3,learniter)
#'     adjustwb("bmat2",rcb2,learniter)
#'     adjustwb("bmat1",rcb1,learniter)
#'     learnitercost = summatrixcost/learniter
#'     return learnitercost

#' def adjustwb(weightorbias,adjustment,adjrunningcount):
#'     global stepsize
#'     global momentum
#'     wandb[weightorbias] = wandb[weightorbias] - (adjustment/adjrunningcount*stepsize)


#' def newneuronmatrix(inputs, paction, pdec, available):
#'     inputm = np.matrix(inputs)
#'     inputmatrix = np.transpose(inputm)
#'     
#'     activation1 = newactivation(wandb["wmat1"],wandb["bmat1"],inputmatrix)
#'     activation2 = newactivation(wandb["wmat2"],wandb["bmat2"],activation1)
#'     activation3 = newactivation(wandb["wmat3"],wandb["bmat3"],activation2)
#'     activation4 = newactivation(wandb["wmat4"],wandb["bmat4"],activation3)
#'     
#'     finalactivation = activation4
#'     choicemax = np.minimum(finalactivation,available)
#'     outputlist = choicemax.tolist()
#'     aichoice = outputlist.index(max(outputlist))
#'     
#'     flatchoices = ['roll','btile', 'gtile', 'ytile', 'otile', 'wtile', 'wblue', 'wgreen','wyellow','worange','wwhite',\
#'                  'lblue', 'lgreen','lyellow','lorange','lwhite']
#'     
#'     aiplayerselection = flatchoices[aichoice]
#'     formatchoice = activation4[aichoice].item()
#'     aiformatchoice = round(formatchoice*100,3)
#'     aiplayermsg = ("AI is {}% positive of this answer.".format(aiformatchoice))
#'     aimessagechoice(aiplayermsg,aiplayerselection)

neuronChoice <- function(inputs){
  activation1 = newActivation(wmat1,bmat1,inputs)
  activation2 = newActivation(wmat2,bmat2,activation1)
  activation3 = newActivation(wmat3,bmat3,activation2)
  activation4 = newActivation(wmat4,bmat4,activation3)
  return(activation4)
}

newActivation <- function(dweight,dbias,prevact){
  dweight_m <- as.matrix(dweight)
  prevact_m <- as.matrix(prevact)
  dbias_m <- as.matrix(dbias)
  newactw = dweight_m %*% prevact_m
  newactb = dbias_m + newactw
  newactivation = sigmoid(newactb)
  #print(newactivation)
  #print(which(newactivation == max(newactivation), arr.ind=T))
  return(newactivation)
}

     
#'def newactivation(dweight,dbias,prevact):
#'     newactw = dweight * prevact
#'     newactb = dbias + newactw
#'     newactivation = sigmoid(newactb)
#'     return newactivation


#' '''Handles the random inputs and handles making the AI player 2 choices'''
#' def balanceinputs(action):
#'     global camellist
#'     global flatchoicescount
#'     global runtype
#'     global saverandomscheck
#'     global endrun
#'     aiblue5 = -1
#'     aiblue3 = -1
#'     aiblue2 = -1
#'     aigreen5 = -1
#'     aigreen3 = -1
#'     aigreen2 = -1
#'     aiyellow5 = -1
#'     aiyellow3 = -1
#'     aiyellow2 = -1
#'     aiorange5 = -1
#'     aiorange3 = -1
#'     aiorange2 = -1
#'     aiwhite5 = -1
#'     aiwhite3 = -1
#'     aiwhite2 = -1
#'         
#'     if blue.rollstatus == 'rolled':
#'         aiblue = 1
#'     else:
#'         aiblue = -1
#'     if green.rollstatus == 'rolled':
#'         aigreen = 1
#'     else:
#'         aigreen = -1
#'     if yellow.rollstatus == 'rolled':
#'         aiyellow = 1
#'     else:
#'         aiyellow = -1
#'     if orange.rollstatus == 'rolled':
#'         aiorange = 1
#'     else:
#'         aiorange = -1
#'     if white.rollstatus == 'rolled':
#'         aiwhite = 1
#'     else:
#'         aiwhite = -1
#'     
#'     if 'blue' in fivers.betcolors:
#'         aiblue5 = 1
#'     if 'blue' in threes.betcolors:
#'         aiblue3 = 1
#'     if 'blue' in twos.betcolors:
#'         aiblue2 = 1
#'     if 'green' in fivers.betcolors:
#'         aigreen5 = 1
#'     if 'green' in threes.betcolors:
#'         aigreen3 = 1
#'     if 'green' in twos.betcolors:
#'         aigreen2 = 1
#'     if 'yellow' in fivers.betcolors:
#'         aiyellow5 = 1
#'     if 'yellow' in threes.betcolors:
#'         aiyellow3 = 1
#'     if 'yellow' in twos.betcolors:
#'         aiyellow2 = 1
#'     if 'orange' in fivers.betcolors:
#'         aiorange5 = 1
#'     if 'orange' in threes.betcolors:
#'         aiorange3 = 1
#'     if 'orange' in twos.betcolors:
#'         aiorange2 = 1
#'     if 'white' in fivers.betcolors:
#'         aiwhite5 = 1
#'     if 'white' in threes.betcolors:
#'         aiwhite3 = 1
#'     if 'white' in twos.betcolors:
#'         aiwhite2 = 1
#'         
#'     if pturn.get() == "Player 1":
#'         if p1winner != '':
#'             winnertile = 1
#'         else:
#'             winnertile = -1
#'     elif p2winner != '':
#'         winnertile = 1
#'     else:
#'         winnertile = -1
#'     
#'     if pturn.get() == "Player 1":
#'         if p1loser != '':
#'             losertile = 1
#'         else:
#'             losertile = -1
#'     elif p2loser != '':
#'         losertile = 1
#'     else:
#'         losertile = -1
#'     
#'     '''aiinfo are the inputs normalized'''
#'     aiinfo = [(aiblue+1)/2, (aiblue5+1)/2, (aiblue3+1)/2, (aiblue2+1)/2, (blue.space-1)/17,\
#'               (len(blue.cabove))/4,\
#'               (aigreen+1)/2, (aigreen5+1)/2, (aigreen3+1)/2, (aigreen2+1)/2, (green.space-1)/17,\
#'               (len(green.cabove))/4,\
#'               (aiyellow+1)/2, (aiyellow5+1)/2, (aiyellow3+1)/2, (aiyellow2+1)/2, (yellow.space-1)/17,\
#'               (len(yellow.cabove))/4,\
#'               (aiorange+1)/2, (aiorange5+1)/2, (aiorange3+1)/2, (aiorange2+1)/2, (orange.space-1)/17,\
#'               (len(orange.cabove))/4,\
#'               (aiwhite+1)/2, (aiwhite5+1)/2, (aiwhite3+1)/2, (aiwhite2+1)/2, (white.space-1)/17,\
#'               (len(white.cabove))/4,\
#'               (winnertile+1)/2, (losertile+1)/2]
#'     
#'     flatchoices = ['roll','btile', 'gtile', 'ytile', 'otile', 'wtile', 'wblue', 'wgreen','wyellow','worange','wwhite',\
#'                  'lblue', 'lgreen','lyellow','lorange','lwhite']
#'     
#'     decision = flatchoices.index(action)
#'     '''Takes the input during "findinputs" and saves it off. The minsavepoint is the minimum number of examples for the least occuring example.'''
#'     minsavepoint = 351
#'     if runtype != 0:
#'         if min(flatchoicescount) < minsavepoint:
#'             if decision < 6:
#'                 if action == 'roll' and flatchoicescount[0] < 150000:
#'                     saveinputs(flatchoices,flatchoicescount,action,aiinfo,rolldictionary)
#'                 elif action == 'btile':
#'                     saveinputs(flatchoices,flatchoicescount,action,aiinfo,btiledictionary)
#'                 elif action == 'gtile':
#'                     saveinputs(flatchoices,flatchoicescount,action,aiinfo,gtiledictionary)
#'                 elif action == 'ytile':
#'                     saveinputs(flatchoices,flatchoicescount,action,aiinfo,ytiledictionary)
#'                 elif action == 'otile':
#'                     saveinputs(flatchoices,flatchoicescount,action,aiinfo,otiledictionary)
#'                 else:
#'                     saveinputs(flatchoices,flatchoicescount,action,aiinfo,wtiledictionary)
#'             elif decision < 11:
#'                 saveinputs(flatchoices,flatchoicescount,action,aiinfo, winnerdictionary)
#'             else:
#'                 saveinputs(flatchoices,flatchoicescount,action,aiinfo, loserdictionary)
#'         elif saverandomscheck == 0:
#'             saveinputstofile(rolldictionary, 'roll')
#'             saveinputstofile(btiledictionary, 'btile')
#'             saveinputstofile(gtiledictionary, 'gtile')
#'             saveinputstofile(ytiledictionary, 'ytile')
#'             saveinputstofile(otiledictionary, 'otile')
#'             saveinputstofile(wtiledictionary, 'wtile')
#'             saveinputstofile(winnerdictionary, 'winner')
#'             saveinputstofile(loserdictionary, 'loser')
#'             saverandomscheck = 1
#'             print("PRINTING FLATCHOICESCOUNT")
#'             print(flatchoicescount)
#'             print("")
#'             print("All Done")
#'             '''Makes it so the program stops running once files are saved'''
#'             run = endrun + 1
#'         
#'     '''Removes unavailable choices for the AI'''
#'     if runtype == 0 and pturn.get() == "Player 2":
#'         aichoices = np.ones((16,1))
#'         if aiinfo[3] == 0:
#'             aichoices[1] = 0
#'         if aiinfo[9] == 0:
#'             aichoices[2] = 0
#'         if aiinfo[15] == 0:
#'             aichoices[3] = 0
#'         if aiinfo[21] == 0:
#'             aichoices[4] = 0
#'         if aiinfo[27] == 0:
#'             aichoices[5] = 0
#'         '''Checks if it is Player1's turn'''
#' if winnertile == 1:
#'   aichoices[6] = 0
#' aichoices[7] = 0
#' aichoices[8] = 0
#' aichoices[9] = 0
#' aichoices[10] = 0
#' if losertile == 1:
#'   aichoices[11] = 0
#' aichoices[12] = 0
#' aichoices[13] = 0
#' aichoices[14] = 0
#' aichoices[15] = 0
#' newneuronmatrix(aiinfo, action, decision, aichoices)


# There are 32 initial inputs 
# They are stored in a list called ai_inputs
# They record if the die has been rolled, which cards are left, 
# where the camel is on the board, how many camels are on the camel,
# and if the winner or loser tile has been chosen
aiInputs <- function(roll_status, card_status, board_status, p2_final_status){
  
  #empty vectors need to be initialized with the correct size if you want to append elements
  ai_info <- numeric(0)
  num_cam_df <- nrow(start_camel_tile_df)*ncol(start_camel_tile_df)
  ai_info <- c(ai_info, 1:32)
  co <- 0
  
  # Loops over matrix for cards left
  # Then divides the amount by the starting matrix. This results in the value being 0 or 1.
  
  for(row in 1:nrow(card_status)) {
    for(col in 1:ncol(card_status)) {
      co <- co + 1
      ai_info[co] <- card_status[row, col]/start_camel_tile_df[row, col]
    }
  }
  
  all_colors <- c("blue","green","orange","red","yellow")
  
  for(c in all_colors){
    co <- co + 1
    if(c %in% roll_status){
      ai_info[co] <- 1
    }
    else{
      ai_info[co] <- 0
    }
  }
  #need to get position for each color and # of camels above
  for(c in all_colors){
    co <- co + 1
    roll_cord <- which(board_status == c, arr.ind = T)
    ai_info[co] <- roll_cord[1,2]/17
    co <- co + 1
    ai_info[co] <- roll_cord[1,1]/4
  }
  
  co <- co + 1
  
  if(p2_final_status["winner"]==""){
    ai_info[co] <- 0
  } else{
    ai_info[co] <- 1
  }
  co <- co + 1
  
  if(p2_final_status["loser"]==""){
    ai_info[co] <- 0
  } else{
    ai_info[co] <- 1
  }
  
  #print("ai info")
  #print(ai_info)
  return(ai_info)
}


rightChoice <- function(repeat_matrix, hand_matrix, camels_left, wl_tile, run_num){
  
  run_x <- run_num - 1
  first_list <- c("",1:run_x)
  second_list <- c("",1:run_x)
  
  c_range <- length(camels_left)
  
  camel_ordering <- find_camel_order(repeat_matrix)
  
  win_c <- camel_ordering[1]
  second_c <- camel_ordering[2]
  
  lead_space <- which(repeat_matrix == win_c, arr.ind = T)
  second_space <- which(repeat_matrix == second_c, arr.ind = T)
  
  lead_amount <- lead_space[[1,2]] - second_space[[1,2]]
  
  if(wl_tile[1]==""){
    if(lead_amount > 3){
      if(lead_space[[1,2]] > 8){
        p_string <- c("wtile",win_c)
        wintile_color <- paste(p_string,collapse = "")
        return(c(wintile_color,"wtile"))
      }
    }
  }
  
  last_tile <- camel_ordering[5]
  almost_last <- camel_ordering[4]
  
  last_space <- which(repeat_matrix == last_tile, arr.ind = T)
  almost_l_space <- which(repeat_matrix == almost_last, arr.ind = T)
  
  losing_amount <- almost_l_space[[1,2]] - last_space[[1,2]]
  
  if(wl_tile[2]==""){
    if(losing_amount > 3){
      if(lead_space[[1,2]] > 8){
        p_string <- c("ltile",last_tile)
        losetile_color <- paste(p_string,collapse = "")
        return(c(losetile_color,"ltile"))
      }
    }
  }
  
  for(x in 1:run_num){
    
    sim_matrix <- repeat_matrix
    c_left <- camels_left
    for(c in 1:c_range){
      sim_roll <- dice_roll(c_left)
      c_left <- remove_die(sim_roll[2],c_left)
      sim_matrix <- board_change(sim_roll, sim_matrix)
    }
    place_list <- unlist(find_camel_order(sim_matrix), use.names=FALSE)
    first_list[x] <- place_list[1]
    second_list[x] <- place_list[2]
    
  }
  
  s_df <- data.frame(first = c(first_list),
                     second = c(second_list))
  
  score_prob <- data.frame(blue = c(0,0,0),
                           green = c(0,0,0),
                           orange = c(0,0,0),
                           red = c(0,0,0),
                           yellow = c(0,0,0)
  )
  
  all_colors <- camel_list
  
  for(c in all_colors){
    for(row in 1:nrow(hand_matrix)){
      prob_lead <- sum(s_df[,1]==c)/run_num
      score_prob[c][row,1] <- hand_matrix[c][row,1] * prob_lead - (1 - prob_lead)
    }
  }
  best_value <- max(score_prob)
  #pick value which you want ai to select a card (anything above 1pt would be a roll (roll is worth 1pt))
  if(best_value > 1.1){
    best_choice <- which(score_prob == max(score_prob), arr.ind = TRUE)
    col_index <- best_choice[1,2]
    row_index <- best_choice[[1,1]]
    col_choice <- colnames(score_prob[col_index])
    return(c(col_choice,row_index))
  }
  else{
    return(c("roll",""))
  }
}



#'''Takes the input during "findinputs" and saves it off. The minsavepoint is the minimum number of examples for the least occuring example.'''
#'     minsavepoint = 351
#'     if runtype != 0:
#'         if min(flatchoicescount) < minsavepoint:
#'             if decision < 6:
#'                 if action == 'roll' and flatchoicescount[0] < 150000:
#'                     saveinputs(flatchoices,flatchoicescount,action,aiinfo,rolldictionary)
#'                 elif action == 'btile':
#'                     saveinputs(flatchoices,flatchoicescount,action,aiinfo,btiledictionary)
#'                 elif action == 'gtile':
#'                     saveinputs(flatchoices,flatchoicescount,action,aiinfo,gtiledictionary)
#'                 elif action == 'ytile':
#'                     saveinputs(flatchoices,flatchoicescount,action,aiinfo,ytiledictionary)
#'                 elif action == 'otile':
#'                     saveinputs(flatchoices,flatchoicescount,action,aiinfo,otiledictionary)
#'                 else:
#'                     saveinputs(flatchoices,flatchoicescount,action,aiinfo,wtiledictionary)
#'             elif decision < 11:
#'                 saveinputs(flatchoices,flatchoicescount,action,aiinfo, winnerdictionary)
#'             else:
#'                 saveinputs(flatchoices,flatchoicescount,action,aiinfo, loserdictionary)
#'         elif saverandomscheck == 0:
#'             saveinputstofile(rolldictionary, 'roll')
#'             saveinputstofile(btiledictionary, 'btile')
#'             saveinputstofile(gtiledictionary, 'gtile')
#'             saveinputstofile(ytiledictionary, 'ytile')
#'             saveinputstofile(otiledictionary, 'otile')
#'             saveinputstofile(wtiledictionary, 'wtile')
#'             saveinputstofile(winnerdictionary, 'winner')
#'             saveinputstofile(loserdictionary, 'loser')
#'             saverandomscheck = 1
#'             print("PRINTING FLATCHOICESCOUNT")
#'             print(flatchoicescount)
#'             print("")
#'             print("All Done")
#'             '''Makes it so the program stops running once files are saved'''
#'             run = endrun + 1


library(dplyr)
board_matrix <- matrix(0, 7, 20)
board_matrix[1,10] <- 5
print(board_matrix)
print(board_matrix[1,10])
camel_tile_df <- data.frame(blue = c(5,3,2),
                            green = c(0,3,2),
                            orange = c(5,3,2),
                            red = c(5,3,2),
                            yellow = c(5,3,2)
)
camel_tile_df[1,2]
c("blue","red")
colSums(camel_tile_df['blue'])

camel_tile_df['green']
match_num <- camel_tile_df['green'] %>%
  #group_by(blue) %>%
  filter(camel_tile_df['green'] == max(camel_tile_df['green']))
test1 <- match_num[1,]
test1
search_value <- match_num[,]

my_list <- list(a = c("blue","green","orange","red","yellow"),                 
                b = "geeksforgeeks",
                c = 2)
my_list

# Remove elements
my_list[names(my_list) %in% "a" == FALSE]


camel_list = c("blue","green","orange","red","yellow")
camel_list[! camel_list %in% "blue"]
camel_list
dimnames(camel_list)
camel_list[names(camel_list)]
camel_list
my_list[names(my_list) %in% "c" == FALSE]


sfind <- which(camel_tile_df['green'] == search_value, arr.ind = T)[1]
camel_tile_df['green'][sfind,1]



player_score <- data.frame(player1score = c(0),
                           player2score = c(0))

player_score



find_camel_order <- function(current_matrix) {
  camel_order <- list()
  
  m_rows <- nrow(current_matrix)
  m_cols <- ncol(current_matrix)
  
  for (c in 1: m_cols) {
    
    #col_check <- (c * -1) - 1
    col_check <- m_cols - c - 1
    
    for (r in 1: m_rows){
      if(!is.null(current_matrix[r-1,col_check])){
        if (current_matrix[r-1,col_check] != "0"){
          
          camel_order <- append(camel_order, current_matrix[r-1,col_check])
        }
      }
    }
  }
  return(camel_order)
}



board_change <- function(roll_result, matrix_of_board) {
  
  y <- 1
  row_count <- 1
  
  roll_location <- which(board_matrix == roll_result[2], arr.ind = T)
  
  if(dim(roll_location)[1] != 0) {
    
    new_column <- strtoi(roll_location[1,2]) + strtoi(roll_result[1])
  } else {
    new_column <- strtoi(roll_result[1])
  }
  for (row in matrix_of_board[1:7,new_column]) {
    
    if(row == "0") {
      
      destination_row <- row_count
      
      if(dim(roll_location)[1] != 0) {
        
        move_matrix <- matrix_of_board[roll_location[1,1]:nrow(matrix_of_board), roll_location[1,2]]
        
        #number of rows that need to be moved
        rows_total <- length(move_matrix)
        
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



dice_roll <- function(dice_remaining) {
  
  dice_color_return <- sample(dice_remaining, 1)
  
  dice_options <- c(1,2,3)
  dice_number_return <- sample(dice_options, 1)
  
  dice_roll_result <- c(dice_number_return, dice_color_return)
  
  return(dice_roll_result)
}



sboard <- function() {
  board_matrix <- matrix(0, 7, 20)
  
  start_camels = c("blue","green","orange","red","yellow")
  dice_options <- c(1,2,3)
  
  for (x in 1:length(start_camels)) {
    print(x)
    print(length(start_camels))
    rand_camel <- sample(start_camels, 1)
    die_result <- sample(dice_options, 1)
    print(rand_camel)
    
    row_count <- 1
    
    for (row in board_matrix[1:7,die_result]) {
      if (board_matrix[row_count, die_result] == 0) {
        board_matrix[row_count, die_result] <- rand_camel
        break
      }
      else {
        row_count <- row_count + 1
      }
    }
    start_camels <- start_camels[start_camels != rand_camel]
    
  }
  return(board_matrix)
}



new_board <- sboard()

new_board

start_camels = c("blue","green","orange","red","yellow")
length(start_camels)



remove_die <- function(rolled_color, camel_colors) {
  camel_colors3 <- camel_colors[camel_colors != rolled_color]
  return(camel_colors3)
}



d <- 5
for(i in 1:10) { 
  nam <- paste("A", i, sep = "")
  assign(nam, rnorm(3)+d)
}



neurons <- c(32,90,90,90,16)


layers <- length(neurons) - 1



for(i in 1:layers){
  wnam <- paste("wmat", i, sep = "")
  assign(wnam, matrix(rnorm(neurons[i]*neurons[i+1], mean=0, sd=1), neurons[i], neurons[i+1]))
  bnam <- paste("bmat", i, sep = "")
  assign(bnam, matrix(rnorm(neurons[i], mean=0, sd=1), neurons[i], 1))
}

bmat1
wmat2

bmat1 = matrix(rnorm(32, mean=0, sd=1),32,1)
bmat5


start_camel_tile_df <- data.frame(blue = c(5,3,2),
                                  green = c(5,3,2),
                                  orange = c(5,3,2),
                                  red = c(5,3,2),
                                  yellow = c(5,3,2)
)
sum(start_camel_tile_df[,1]==5) * 2
start_camel_tile_df['blue'][2,1]
new_v <- numeric(0)
new_v <- c(new_v, 1:(nrow(start_camel_tile_df)*ncol(start_camel_tile_df)))
co <- 0
# Loop over my_matrix
for(row in 1:nrow(start_camel_tile_df)) {
  for(col in 1:ncol(start_camel_tile_df)) {
    co <- co + 1
    new_v[co] <- start_camel_tile_df[row, col]
    print(start_camel_tile_df[row, col])
  }
}

new_v


two_matrix <- matrix(2, 3, 5)
three_matrix <- matrix(3, 5, 3)

test_m <- list(two_matrix, three_matrix)
#test_m[2]
write.csv(test_m,"data\\test_m_results.csv", row.names = FALSE)



1 - three_matrix

new_m <- two_matrix %*% three_matrix

sample(1:3, 1)

randchoices <- c("blue","green","orange","red","yellow",
                 "wtileblue","wtilegreen","wtileorange","wtilered","wtileyellow",
                 "ltileblue","ltilegreen","ltileorange","ltilered","ltileyellow")

match("green",randchoices)

wmat_list <- list(wmat1r(),wmat2r(),wmat3r(),wmat4r())
wmat_list <- list(bmat1r(),bmat2r(),bmat3r(),bmat4r())
for(i in 1:layers){
  wvar <- paste("wmatSave", i, sep = "")
  new_string <- c("data\\",wvar,".csv")
  w_sav_string <- paste(new_string,collapse = "")
  write.csv(wmat_list[i],"data\\test_m_results.csv", row.names = FALSE)
  bvar <- paste("bmatSave", i, sep = "")
  new_string <- c("data\\",bvar,".csv")
  write.csv(wmat_list[i],"data\\test_m_results.csv", row.names = FALSE)
  print("saved wb")
}

dice_df2 <- data.frame(tba <- c("ant","bear","dog"))
ggplot(dice_df2,aes(x=tba)) + 
  geom_point(stat = "count")


camel_card_graph <- function(camels_cards) {
  ggplot(camels_cards)
  
}

colorlist = c("red","blue","black")
  
xlist = c(1,1,1)
ylist = c(3,2,1)
idlist = c(5,3,2)
dd <- data.frame(x=xlist,y=ylist,id=idlist)
ggplot(dd,aes(x,y)) + 
  geom_point(data = dd,
             color = "red",
    shape = 15,
    size = 8.5) +
  geom_text(aes(label=id), color = "black")

length(xlist)


ggfunction <- function(info,somelist,idvar){
  gpoint <- ggplot() + 
    geom_point(aes(x,y),
               data = somelist,
               color = 'red',
               shape = 15,
               size = 8.5) +
    geom_text(
              data = somelist,
              aes(x,y,label=idvar), 
              color = "white")
  return(gpoint)
}

ggsingle <- function(info,somelist,idvar){
  gpoint <- list(
    geom_point(data  = somelist,
               color = 'red',
               shape = 15,
               size = 8.5),
    geom_text(data = somelist,
              aes(label=idvar), 
              color = "white"))
  return(gpoint)
}

xlist = c(1,1,1)
x2list = c(2,2,2)
ylist = c(3,2,1)
idlist = c(5,3,2)
dd <- data.frame(x=xlist,y=ylist,id=idlist)
ee <- data.frame(x=x2list,y=ylist,id=idlist)
#ggpointfun <- ggfunction(dd)

safetry <- ggplot(dd,aes(x,y)) +
  ggsingle(dd,dd,idlist) +
  ggsingle(ee,ee,idlist)

safetry

ggfunction(dd,dd,idlist)
test2 <- ggfunction(ee,ee,idlist)

ggplot(dd,aes(x,y)) + 
  ggfunction(dd,id)

xtest = c(1,1,1)
ytest = c(0,3,2)
dataframe_test <- data.frame(y=ytest)

dataframe_test %>% 
  filter(rowSums(across(where(is.numeric)) > 0) > 0)

n <- 3
vartest <- rep(1:5)
rep("red",3)


m_list <- c(4,6,1,3)
which.max(m_list)

mydata<- read.csv('WebAttacks.csv', header = FALSE, sep = ',')
summary(mydata)
# spliting my data into two main seeds
set.seed(2)
mydata<- mydata[sample(nrow(mydata)),]
train.df <- mydata[1:as.integer(0.7*30),]
test.df <- mydata[as.integer(0.7*30 +1):30,]

## This algorithm uses the nearest samples by calculating euclidian distance 

euclideanDist <- function(a, b){
  d = 0
  for(i in c(1:(length(a)-1) ))
  {
    d = d + (a[[i]]-b[[i]])^2
  }
  d = sqrt(d)
  return(d)
}

## Prediction function

knn_predict <- function(test_data, train_data, k_value){
  pred <- c()  
  #First look 
  for(i in c(1:nrow(test_data))){   
         # initialization of eucledien distance part
    eu_dist =c()          
    eu_char = c()
    good = 0             
    bad = 0
    
    #second loop designed for training dataset 
    for(j in c(1:nrow(train_data))){
      eu_dist <- c(eu_dist, euclideanDist(test_data[i,], train_data[j,]))
      eu_char <- c(eu_char, as.character(train_data[j,][[6]]))
    }
    
    eu <- data.frame(eu_char, eu_dist) 

    eu <- eu[order(eu$eu_dist),]       
    eu <- eu[1:k_value,]              

    #Third look for counting neighbours
    for(k in c(1:nrow(eu))){
      if(as.character(eu[k,"eu_char"]) == "g"){
        good = good + 1
      }
      else
        bad = bad + 1
    }
	# b and g are being used to describe bad majority and good majority
    if(good > bad){          

      pred <- c(pred, "g")
    }
    else if(good < bad){
                   
      pred <- c(pred, "b")
    }
    
  }
  return(pred) # In a form of a vector
} 
# performance metric which is Accuracy to make sure the predicted values are correct
accuracy <- function(test_data){
  correct = 0
  for(i in c(1:nrow(test_data))){
    if(test_data[i,6] == test_data[i,7]){ 
      correct = correct+1
    }
  }
  accu = correct/nrow(test_data) * 100  
  return(accu)
}
K = 5
predictions <- knn_predict(test.df, train.df, K) #calling knn_predict()

test.df[,20] <- predictions 
print(accuracy(test.df))
79.222223


 

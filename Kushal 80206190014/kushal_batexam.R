
data <- read.csv("batexam.csv")
print(data)


#printing max number from column

#Q1: Compute the MEAN, STDEV of the numeric columns.
calculate_mean = function ("batexam.csv"){
  data_int_datatype = data[,sapply(data,is.integer)]        
  columns = colnames(data_int_datatype)
  for (col in columns) {                                    
    total_sum = sum(data_int_datatype[col])
    cnt = nrow(data_int_datatype[col])
    mean = total_sum/cnt
    print(paste('Mean values of numeric columns:',col,'is-',mean))
  }
  
}

###################calculating SD###################
standard_dev = function("batexam.csv") {
  data_int_datatype = data[,sapply(data,is.integer)]
  columns = colnames(data_int_datatype)
  for (col in columns) {                                   
    total_sum = sum(data_int_datatype[col])
    cnt = nrow(data_int_datatype[col])
    std_dev = sqrt(sum((data_int_datatype[col]-(total_sum/cnt))^2)/cnt)
    print(paste('Standard deviation for ',col,'is-',std_dev))
  }
  
}

#####Q2.###Calculating zscore######
cal_z_score = function("batexam.csv"){
  data_int_datatype = data[,sapply(data,is.integer)]
  columns = colnames(data_int_datatype)
  for (col in columns){                             
    Total_sum = sum(data_int_datatype[col])
    cnt = nrow(data_int_datatype[col])
    z_score_val = (data_int_datatype[col]-(Total_sum/cnt))/(sqrt(sum((data_int_datatype[col]-(Total_sum/cnt))^2)/cnt)) #applying z score formula
    print(paste('z_score for ',col,'is-',z_score_val))
  }
}

#######Q3Implement one hot encoding using your own function to encode Gender######
one_hot_encod = function("batexam.csv"){
  
  data['Gender_dum'] = as.integer(data['Gender'] =='F')
  print(data)
}

one_hot_encod("batexam.csv")

#####################Q4splitting data set#######################
split_test_train = function("batexam.csv"){
  df_male = subset(data, Gender=='M')
#  print(data)
  df_female = subset(data,Gender == 'F')
 # print(data)
  df_male_top_70 = head(df_male[order(df_male['Gender'],decreasing=T),],.70*nrow(df_male))
  df_female_top_70 = head(df_female[order(df_female['Gender'],decreasing=T),],.70*nrow(df_female))
  Train_data = rbind(df_male_top_70,df_female_top_70)
  print(Train_data)
  Test_data = data[ !(data$Name %in% Train_data$Name), ]
    print(Test_data)
  
  
  }
############################Q5Implement a function that executes gradient descent algorithm to estimate the coefficient and bias in Y = wX+b. Also print the MSE at the end of this estimation. Restrict the algorithm to try 100 iterations.######################
library(grid)
library(dplyr)
library(scales)
library(ggplot2)



# original formula 
Formula <- function(x) 1.2 * (x-2)^2 + 3.2

# visualize the function, and the optimal solution
ggplot( data.frame( x = c(0, 4) ), aes(x) ) + 
  stat_function(fun = Formula) + 
  geom_point( data = data.frame( x = 2, y = Formula(2) ), aes(x, y), 
              color = "blue", size = 3 ) + 
  ggtitle( expression( 1.2 * (x-2)^2 + 3.2 ) )
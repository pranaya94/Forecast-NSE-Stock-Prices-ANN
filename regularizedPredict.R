library(gdata)
library(dplyr)
library(DataCombine)
library(ggplot2)

setwd("C:/Users/prtomar/Documents/Data Hackathon")

con <- file("stock.log")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

rawdata1 = read.csv("HackathonRound1.csv")
rawdata2 = read.csv("updatedData.csv")
rawdata = rbind(rawdata1,rawdata2)

nse_stock_neural_net <- function(data_vars,theta1,theta2,minOpen,maxOpen,actualOpen,init_w = 0, learn = 0, alpha = 0.01)
{
  h_values = rep(0,nrow(data_vars))
  
  if(init_w == 1)
  {
    theta1 = matrix(runif(55,-1,1),5,11)          
    theta2 = matrix(runif(6,-1,1),1,6)                    
  }   
  
  t1_DELTA = matrix(0,nrow(theta1),ncol(theta1))
  t2_DELTA = matrix(0,nrow(theta2),ncol(theta2))
  
  m = 0
  J = 0.0
  
  tan_sigmoid <- function(x)
  {
    result = (exp(x) - exp(-x)) / (exp(x) + exp(-x))
    return(result)
  }
  
  for(i in 1:nrow(data_vars))
  {
    a1 = c(1,data_vars[i,1:10]) # 1X11          
    z2 = theta1 %*% a1 #5X11 /*/ 1*11          
    a2 = c(1,tan_sigmoid(z2)) #6X1          
    z3 = theta2 %*% a2 #1X6 * 6X1 = 1X1          
    h = tanh(z3)          
    J = J + (data_vars[i,11] - h)^2          
    m = m + 1
    
    if(learn == 1)  
    {            
      delta3 = h - data_vars[i,11]         
      delta2 = ((t(theta2) %*% delta3) * (a2 * (1 - a2)))[2:6,]             
      
      t1_DELTA = t1_DELTA + (matrix(delta2,5,1) %*% matrix(a1,1,11)) 
      t2_DELTA = t2_DELTA + (delta3 %*% a2) #accumulating errors over each record in batch           
    }
    else {
      print(c("Hypothesis for ",i," is ",abs(h*(maxOpen-minOpen)+minOpen)," actual is ", actualOpen[i,1]))
    }
    
  } # end for
  
  lambda = 1
  
  # average cost for batch    
  J = J/m

  theta1_square = cbind(theta1[,1],theta1[,2:ncol(theta1)]^2)
  theta2_square = cbind(theta2[,1],theta2[,2:ncol(theta2)]^2)
  
  J = J + (lambda/(2*m)) * (sum(theta1_square) + sum(theta2_square))
  
  t1_DELTA = (1/m)*t1_DELTA
  t2_DELTA = (1/m)*t2_DELTA
  t1_DELTA[,2:ncol(t1_DELTA)] = t1_DELTA[,2:ncol(t1_DELTA)] + (lambda/m)*t1_DELTA[,2:ncol(t1_DELTA)]
  t2_DELTA[,2:ncol(t2_DELTA)] = t2_DELTA[,2:ncol(t2_DELTA)] + (lambda/m)*t2_DELTA[,2:ncol(t2_DELTA)]
  
  
  if(learn == 1)
  {
    theta1 = theta1 - (alpha * t1_DELTA)
    theta2 = theta2 - (alpha * t2_DELTA)
    print(c("J: ",J))
  }
  else{
    print(c("J: ",J))
  }
  
  theta1_new = theta1;
  theta2_new = theta2;
  
  theta_list <- list(theta1_new,theta2_new,abs(h*(maxOpen-minOpen)+minOpen),J)
  return(theta_list)        
} # end nse_stock_neural_net()

predict_stock_open_value <- function(ShareName = "Share1",lag=3)
{        
  share_data_frame = rawdata %>% 
    select(Share.Names,Date,Prev.Close,High.Price,Low.Price,Last.Price,Close.Price,Average.Price,
           Total.Traded.Quantity,Turnover.in.Lacs,Deliverable.Qty,X..Dly.Qt.to.Traded.Qty,Open.Price) %>% 
    filter(Share.Names == ShareName)
  
  share_data_frame_numeric = share_data_frame %>% 
    select(-Share.Names,-Date)
  
  maxOpen = share_data_frame_numeric %>% 
    select(Open.Price) %>% 
    apply(2,max)
  
  minOpen = share_data_frame_numeric %>% 
    select(Open.Price) %>% 
    apply(2,min)
  
  actualOpen = as.vector(share_data_frame_numeric %>% 
                           select(Open.Price))
  
  share_data_scaled = share_data_frame_numeric %>%       
    mutate(
      x1 = (Prev.Close - min(Prev.Close)) / (max(Prev.Close) - min(Prev.Close)),
      x2 = (High.Price - min(High.Price)) / (max(High.Price) - min(High.Price)),
      x3 = (Low.Price - min(Low.Price)) / (max(Low.Price) - min(Low.Price)),
      x4 = (Last.Price - min(Last.Price)) / (max(Last.Price) - min(Last.Price)),
      x5 = (Close.Price - min(Close.Price)) / (max(Close.Price) - min(Close.Price)),
      x6 = (Average.Price - min(Average.Price)) / (max(Average.Price) - min(Average.Price)),
      x7 = (Total.Traded.Quantity - min(Total.Traded.Quantity)) / (max(Total.Traded.Quantity) - min(Total.Traded.Quantity)),
      x8 = (Turnover.in.Lacs - min(Turnover.in.Lacs)) / (max(Turnover.in.Lacs) - min(Turnover.in.Lacs)),
      x9 = (Deliverable.Qty - min(Deliverable.Qty)) / (max(Deliverable.Qty) - min(Deliverable.Qty)),
      x10 = (X..Dly.Qt.to.Traded.Qty - min(X..Dly.Qt.to.Traded.Qty)) / (max(X..Dly.Qt.to.Traded.Qty) - min(X..Dly.Qt.to.Traded.Qty)),
      x11 = (Open.Price - min(Open.Price)) / (max(Open.Price) - min(Open.Price))
    ) %>% 
    select(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11)
  
  if(lag == 3){              
    lagged_data <- slide(share_data_scaled, Var = "x11", slideBy = lag) %>% 
      mutate(x11 = x113) %>% 
      select(-x113) %>% 
      filter(!is.na(x11))
  }        
  
  else if(lag == 4) {        
    lagged_data <- slide(share_data_scaled, Var = "x11", slideBy = lag) %>% 
      mutate(x11 = x114) %>% 
      select(-x114) %>% 
      filter(!is.na(x11))
  }        
  
  lagged_data_matrix = data.matrix(lagged_data)
  
  #initialize weights for NN
  theta_list <- nse_stock_neural_net(lagged_data_matrix,theta1,theta2,minOpen,maxOpen,actualOpen,1,1,0.01)
  
  cost_to_plot = rep(0,10)
  iteration_to_plot = rep(0,10)
  for (n in 1:1000)
  {
    theta_list <- nse_stock_neural_net(lagged_data_matrix,theta_list[[1]],theta_list[[2]],minOpen,maxOpen,actualOpen,0,1,0.01)
    cost_to_plot[n] = theta_list[[4]]
    iteration_to_plot[n] = n
    
    # if (n%%10 == 0){
    #   print("Neural Network Output----------------------------------------------------------------------------------")
      print(c("Iteration : ",n))
    #   theta_list <- nse_stock_neural_net(lagged_data_matrix,theta_list[[1]],theta_list[[2]],minOpen,maxOpen,actualOpen)       
    # }
    n = n + 1
  }
  
  plot(x=iteration_to_plot,y=cost_to_plot,type="l")
  
  
  #last row is values on 15 May
  prediction_data = share_data_scaled[nrow(share_data_scaled),] 
  # 
  prediction_data_matrix = data.matrix(prediction_data)
  # 
  theta_list <- nse_stock_neural_net(prediction_data_matrix,theta_list[[1]],theta_list[[2]],minOpen,maxOpen,actualOpen)
  # 
  return(theta_list[[3]])
} #end predict_stock_open_value

# lag = +3 for 18 May and = +4 for 19 May

open_value_18_19 = rep(0,100)
share_name = rep("X",100)
for(x in 0:49)
{
  share_name[2*x+1] = paste("Share",x+1,sep="")
  share_name[2*x+2] = paste("Share",x+1,sep="")
  
  if(x == 35)
  {
    open_value_18_19[2*x+1] = 0
    open_value_18_19[2*x+2] = 0
  }
  else{
    print("-------")
    print(paste("Share",x+1))
    open_value_18_19[2*x+1] = predict_stock_open_value(paste("Share",x+1,sep=""),3)
    open_value_18_19[2*x+2] = predict_stock_open_value(paste("Share",x+1,sep=""),4)
  }
  x = x + 1
}

open_value_output = data.frame(Share = share_name,Open = open_value_18_19)
write.csv(open_value_output,file = "open_values.csv")

sink()
sink(type="message")
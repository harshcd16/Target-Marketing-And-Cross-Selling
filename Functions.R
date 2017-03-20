calculateNumOfDaysUntilNextCall <- function(Number.Of.Days.Until.Next.Call,unique_ids){
  
  i <- 1
  for(i in seq_along(unique_ids)){
    
    sub_df <-   df_OrderedByCustID[df_OrderedByCustID[,2] == unique_ids[i] ,]  
    
    call_date <- sub_df$Call.Date
    previous_date <- c(sub_df$Call.Date[1],head(sub_df$Call.Date,length(sub_df$Call.Date)-1))
    
    date_diff <- as.Date(call_date) - as.Date(previous_date) 
    
    Number.Of.Days.Until.Next.Call <- append(Number.Of.Days.Until.Next.Call,date_diff)
  }
  
  return(Number.Of.Days.Until.Next.Call)
  
}


createNewColumnForAverageReturnDays <- function(mean_days,customerID,Customer.ID,Mean.Days){
  
  i <- 1
  j <- 1
  for(j in seq_along(customerID)){
    
    for(i in seq_along(Customer.ID)){
      
      if(customerID[j] == as.character(Customer.ID[i])){
        
        Mean.Days[i] <- mean_days[customerID[j]]
        
      }
    }
  }
  
  return(Mean.Days)
}

getTestRows <- function(df){
  
  customer_frequency <- as.data.frame(table(df$Customer.ID))
  
  row_names_test <- c()
  i <- 1
  for(i in seq_along(customer_frequency$Var1)){
    
    sub_df <-   df[df[,"Customer.ID"] == as.character(customer_frequency$Var1[i]) ,]
    
    row_names_test <- append(row_names_test, max(as.numeric(rownames(sub_df)))) 
    
  }
  
  return(as.character(row_names_test))
  
}
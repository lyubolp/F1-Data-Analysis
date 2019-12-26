

plot_wins <- function(period)
{
  #Period: 1998, 2006, 2010, 2014, 2017
  if(period == 2017)
  {
    winnings_constructor <- results_df[(results_df$positionText == "1" | results_df$positionText == "2" | results_df$positionText == "3")
                                       & results_df$raceId >= 969 & results_df$raceId <= 1030,]$constructorId
    races_entered_constructors <-results_df[results_df$raceId >= 969 & results_df$raceId <= 1030,]$constructorId
  }
  else if(period == 2014)
  {
    winnings_constructor <- results_df[(results_df$positionText == "1" | results_df$positionText == "2" | results_df$positionText == "3")
                                       & results_df$raceId >= 900 & results_df$raceId <= 968,]$constructorId
    races_entered_constructors <- results_df[results_df$raceId >= 900 & results_df$raceId <= 968,]$constructorId
  }
  else if(period == 2010)
  {
    winnings_constructor <- results_df[(results_df$positionText == "1" | results_df$positionText == "2" | results_df$positionText == "3")
                                       & ((results_df$raceId >= 841 & results_df$raceId <= 899)
                                          | (results_df$raceId >= 337 & results_df$raceId <= 355))
                                       ,]$constructorId
    
    races_entered_constructors <- results_df[(results_df$raceId >= 841 & results_df$raceId <= 899)
                                             | (results_df$raceId >= 337 & results_df$raceId <= 355),]$constructorId
  }
  else if(period == 2006)
  {
    winnings_constructor <- results_df[(results_df$positionText == "1" | results_df$positionText == "2" | results_df$positionText == "3")
                                       & results_df$raceId >= 18 & results_df$raceId <= 70,]$constructorId
    
    races_entered_constructors <- results_df[results_df$raceId >= 18 & results_df$raceId <= 70,]$constructorId
  }
  else if(period == 1998)
  {
    winnings_constructor <- results_df[(results_df$positionText == "1" | results_df$positionText == "2" | results_df$positionText == "3")
                                       & results_df$raceId >= 71 & results_df$raceId <= 206,]$constructorId
    
    races_entered_constructors <- results_df[results_df$raceId >= 71 & results_df$raceId <= 206,]$constructorId
  }
  else
  {
    q(1)
  }
  
  winnings_constructor_label = c()
  i <- 1
  for(constructor_id in winnings_constructor)
  {
    winnings_constructor_label[i] <-(constructors_labels$name[constructors_labels$constructorId == constructor_id])  
    i <- i + 1
  }
  
  table_constructors_wins <- sort(table(winnings_constructor_label), decreasing = T)
  
  teams_wins<-table_constructors_wins["Ferrari"]
  teams_wins<-c(teams_wins, table_constructors_wins["McLaren"])
  teams_wins<-c(teams_wins, table_constructors_wins["Williams"])
  teams_wins<-c(teams_wins, table_constructors_wins["Mercedes"])
  teams_wins<-c(teams_wins, table_constructors_wins["Red Bull"])
  teams_wins<-c(teams_wins, table_constructors_wins["Renault"])
  teams_wins<-c(teams_wins, table_constructors_wins["BMW Sauber"])
  teams_wins<-c(teams_wins, table_constructors_wins["Jordan"])
  
  teams_wins[is.na(teams_wins)] <- 0
  teams_wins
}

table_wins_period <- matrix(plot_wins(1998),nrow=1,byrow=FALSE)
colnames(table_wins_period) <- teams

table_wins_period <- rbind(table_wins_period, plot_wins(2006))
table_wins_period <- rbind(table_wins_period, plot_wins(2010))
table_wins_period <- rbind(table_wins_period, plot_wins(2014))
table_wins_period <- rbind(table_wins_period, plot_wins(2017))


barplot(table_wins_period, col=rainbow(5), 
        legend.text = c("1998-2005", "2006-2008","2010-2013", "2014-2016", "2017-2019"), 
        main = 'Колко подиума има даден отбор през даден период',
        xpd = FALSE, ylim = c(0, 140))
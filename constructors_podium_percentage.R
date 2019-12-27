results_list<-read.csv('./data/results.csv',sep=',',stringsAsFactors=F)
constructors_labels<-read.csv('./data/constructors.csv', sep=',', stringsAsFactors = F)

results_df <- as.data.frame(results_list)
color <- c("#DC0000", "#FF8700", "#FFFFFF", "#00D2BE", "#1E41FF", "#FFF500", "#9B0000", "#F596C8") 
teams <- c("Ferrari", "McLaren", "Williams", "Mercedes", "Red Bull", "Renault", "Sauber", "Force India")
#Ferrari, McLaren, Williams, Mercedes, Red Bull, Renault, Sauber, Force India

plot_podium_percentage <- function(period)
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
  else if(period == 2009)
  {
    winnings_constructor <- results_df[(results_df$positionText == "1" | results_df$positionText == "2" | results_df$positionText == "3")
                                       & ((results_df$raceId >= 1 & results_df$raceId <= 17)
                                          |(results_df$raceId >= 841 & results_df$raceId <= 899)
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
  
  #races_entered_constructors <- results_df$constructorId
  races_entered_constructors_labels = c()
  i <- 1
  for(constructor_id in races_entered_constructors)
  {
    races_entered_constructors_labels[i] <-(constructors_labels$name[constructors_labels$constructorId == constructor_id])
    i <- i + 1
  }
  
  table_races_entered_constructors <- sort(table(races_entered_constructors_labels), decreasing = T) / 2
  teams_wins_percentage_vector = c()
  teams_wins_names = c()
  i <- 1
  for(name in names(teams_wins))
  {
    teams_wins_percentage_vector[i] <- (table_constructors_wins[name] / table_races_entered_constructors[name] * 100)
    teams_wins_names[i] <- (name)
    i <- i + 1
  }
  
  teams_wins_percentage_vector[is.na(teams_wins_percentage_vector)] <- 0
  
  
  teams_wins_percentage_vector
  
}

table_percentage_podium_period <- matrix(plot_win_percentage(1998),nrow=1,byrow=FALSE)
colnames(table_percentage_podium_period) <- teams

table_percentage_podium_period <- rbind(table_percentage_podium_period, plot_podium_percentage(2006))
table_percentage_podium_period <- rbind(table_percentage_podium_period, plot_podium_percentage(2009))
table_percentage_podium_period <- rbind(table_percentage_podium_period, plot_podium_percentage(2014))
table_percentage_podium_period <- rbind(table_percentage_podium_period, plot_podium_percentage(2017))

table_percentage_podium_period_scaled <- scale(table_percentage_podium_period, center=FALSE, scale=colSums(table_percentage_podium_period))

barplot(table_percentage_podium_period_scaled, col=rainbow(5), 
        legend.text = c("1998-2005", "2006-2008","2009-2013", "2014-2016", "2017-2019"), 
        main = 'Колко % от подиумите на даден отбор са през даден период',
        xpd = FALSE, ylim = c(0, 1))




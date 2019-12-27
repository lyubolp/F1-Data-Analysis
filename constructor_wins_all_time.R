results_list<-read.csv('./data/results.csv',sep=',',stringsAsFactors=F)
constructors_labels<-read.csv('./data/constructors.csv', sep=',', stringsAsFactors = F)

results_df <- as.data.frame(results_list)


winnings_constructor <- results_df[results_df$positionText == "1",]$constructorId

winnings_constructor_label = c()
i <- 1
for(constructor_id in winnings_constructor)
{
  winnings_constructor_label[i] <-(constructors_labels$name[constructors_labels$constructorId == constructor_id])  
  i <- i + 1
}

table_constructors_wins <- sort(table(winnings_constructor_label), decreasing = T)
top_5_constructors<-table_constructors_wins[1:5]

color <- c("#DC0000", "#FF8700", "#FFFFFF", "#00D2BE", "#1E41FF")
barplot(top_5_constructors, main='Топ 5 конструктури (победи)', col=color, xpd = FALSE, ylim = c(0, 250))

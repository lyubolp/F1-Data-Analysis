results_list<-read.csv('./data/results.csv',sep=',',stringsAsFactors=F)
constructors_labels<-read.csv('./data/constructors.csv', sep=',', stringsAsFactors = F)

results_df <- as.data.frame(results_list)


# period: 2011-2019 -> raceids: [841;1030]
# period: 1999-2005 -> raceids: [71; 190]
# period: 1991-1998 -> raceids: [191; 320]
qualy_race <- table(as.numeric(results_df$grid[results_df$raceId >= 191 & results_df$raceId <= 320 & results_df$grid >= 1]), 
      as.numeric(results_df$position[results_df$raceId >= 191 & results_df$raceId <= 320 & results_df$grid >= 1]))

print(qualy_race)
chisq.test(qualy_race)
qualy_race <- scale(qualy_race, center=FALSE, scale=colSums(qualy_race))

par(xpd=TRUE)
par(mar=c(5, 5, 5, 5))
barplot(qualy_race, col=rainbow(30))
legend("topright", inset=c(-0.13, -0.07), legend=1:25, title="Group", fill = rainbow(30))
        


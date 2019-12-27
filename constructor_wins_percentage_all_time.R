results_list <-
  read.csv('./data/results.csv',
           sep = ',',
           stringsAsFactors = F)
constructors_labels <-
  read.csv('./data/constructors.csv',
           sep = ',',
           stringsAsFactors = F)

results_df <- as.data.frame(results_list)
color <-
  c(
    "#DC0000",
    "#FF8700",
    "#FFFFFF",
    "#00D2BE",
    "#1E41FF",
    "#FFF500",
    "#9B0000",
    "#F596C8"
  )
teams <-
  c(
    "Ferrari",
    "McLaren",
    "Williams",
    "Mercedes",
    "Red Bull"
  )
#Ferrari, McLaren, Williams, Mercedes, Red Bull, Renault, Sauber, Force India


winnings_constructor <-
  results_df[results_df$positionText == "1",]$constructorId
races_entered_constructors <- results_df$constructorId


winnings_constructor_label = c()
i <- 1
for (constructor_id in winnings_constructor)
{
  winnings_constructor_label[i] <-
    (constructors_labels$name[constructors_labels$constructorId == constructor_id])
  i <- i + 1
}

table_constructors_wins <-
  sort(table(winnings_constructor_label), decreasing = T)

teams_wins <- table_constructors_wins["Ferrari"]
teams_wins <- c(teams_wins, table_constructors_wins["McLaren"])
teams_wins <- c(teams_wins, table_constructors_wins["Williams"])
teams_wins <- c(teams_wins, table_constructors_wins["Mercedes"])
teams_wins <- c(teams_wins, table_constructors_wins["Red Bull"])

#races_entered_constructors <- results_df$constructorId
races_entered_constructors_labels = c()
i <- 1
for (constructor_id in races_entered_constructors)
{
  races_entered_constructors_labels[i] <-
    (constructors_labels$name[constructors_labels$constructorId == constructor_id])
  i <- i + 1
}

table_races_entered_constructors <-
  sort(table(races_entered_constructors_labels), decreasing = T) / 2
teams_wins_percentage_vector = c()
teams_wins_names = c()
i <- 1
for (name in names(teams_wins))
{
  teams_wins_percentage_vector[i] <-
    (table_constructors_wins[name] / table_races_entered_constructors[name] * 100)
  teams_wins_names[i] <- (name)
  i <- i + 1
}

teams_wins_percentage_vector[is.na(teams_wins_percentage_vector)] <-
  0


teams_wins_percentage_vector


barplot(teams_wins_percentage_vector, main='Топ 5 конструктури (% победи)', col=color, xpd = FALSE, ylim = c(0, 100), names.arg = teams)

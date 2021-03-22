setwd("~/node-test/Warzone_GitHub")
library(tidyverse)
library(ggplot2)
library(reshape2)
library(rvest)
library(jsonlite)

# br_87 = Solos
# br_25 = Trios
# br_89 = Quads
# br_71 = BR Stimulus Solos
# br_dmz_104 = Blood Money 

#Import json file from JS API
data <- fromJSON('JSON_files/wzstats.json')
data_all <- fromJSON('JSON_files/wzstats_all.json')
data_combat <- fromJSON('JSON_files/wzfullcombat.json')

matches <- data_combat$response

#convert api data into same format as historic data
api_data <- data.frame(data$response$br)
api_data$player <- gsub('#','',gsub('[0-9]+', '', data$player))
api_data$win_pc <- round(100*api_data$wins/api_data$gamesPlayed,2)
api_data$top10_pc <- round(100*api_data$topTen/api_data$gamesPlayed,2)
api_data$top5_pc <- round(100*api_data$topFive/api_data$gamesPlayed,2)
api_data$avg_kills <- round(api_data$kills/api_data$gamesPlayed,2)
api_data$minutes <- round(api_data$timePlayed/60,0)
api_data$kill_ratio <- round(api_data$kdRatio,2)
api_data$top5conversion <- round(100*api_data$wins/api_data$topFive,2)
api_data$avg_score <- round(api_data$score/api_data$gamesPlayed,0)

#renaming api_data
api_data <- rename(api_data, top10=topTen)
api_data <- rename(api_data, played=gamesPlayed)

api_data <- filter(api_data, player!="TrumpTrash")
str(api_data)

write_csv(api_data,'api_data.csv')
?formatC
#Set up weekly data

data_all <- data_all$response$weekly$mode
warzoneweek <- data_all$br_all$properties
warzoneweek$player <- gsub('#','',gsub('[0-9]+', '', data$player))
warzoneweek$headshotPercentage <- round(100*warzoneweek$headshotPercentage,1)
warzoneweek$damageRatio <- round(warzoneweek$damageDone/warzoneweek$damageTaken,1)
warzoneweek$teamWipedPerGame <- round(warzoneweek$objectiveTeamWiped/warzoneweek$matchesPlayed,2)
warzoneweek$trufflesPerGame <- round(warzoneweek$objectiveBrCacheOpen/warzoneweek$matchesPlayed,2)
warzoneweek$revivesPerGame <- round(warzoneweek$objectiveReviver/warzoneweek$matchesPlayed,2)
warzoneweek$distancePerGame <- round((warzoneweek$distanceTraveled/warzoneweek$matchesPlayed)/1000,2)
warzoneweek$kdRatio <- round(warzoneweek$kdRatio,2)

warzoneweek$gulag_win_pc <- round(100*warzoneweek$gulagKills/(warzoneweek$gulagDeaths+warzoneweek$gulagKills),1)
warzoneweek$gulag_kd <- round(warzoneweek$gulagKills/warzoneweek$gulagDeaths,2)

write_csv(warzoneweek,'last_week.csv')


warzone$top_10_conversions <- warzone$wins/warzone$top10
wins_hour <- round(warzone$wins/warzone$minutes*60,2)
minutes_win <- round(warzone$minutes/warzone$wins,0)



warzone <- api_data
np <- length(warzone$player)

#setting colours for visualisations
colfunc <- colorRampPalette(c("green","orange","blue"))
cust_col <- c("#00FF00", "#7FD200", "#BFBB00","#3FE800", "#FFA500", "#FF7B00", "#FF5200", "#FF2900", "#FF0000")

#games played
p <- ggplot(warzone, aes(x = reorder(player, -played),played))
p <- p + geom_col(fill=colfunc(np)) + theme_bw() + geom_text(aes(label=played), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player")
p <- p+ ggtitle("Games played")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("played.jpg", quality = 100)
print(p)
dev.off()

# Minutes played
p <- ggplot(warzone, aes(x = reorder(player, -minutes),minutes))
p <- p + geom_col(fill=colfunc(np)) + theme_bw() + geom_text(aes(label=minutes), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player")
p <- p+ ggtitle("Total minutes played")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("minutes.jpg", quality = 100)
print(p)
dev.off()

# Average score
p <- ggplot(warzone, aes(x = reorder(player, -avg_score),avg_score))
p <- p + geom_col(fill=colfunc(np)) + theme_bw() + geom_text(aes(label=avg_score), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player")
p <- p+ ggtitle("Average score")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("avg_score.jpg", quality = 100)
print(p)
dev.off()

#Kill ratio
p <- ggplot(warzone, aes(x = reorder(player, -kill_ratio),kill_ratio))
p <- p + geom_col(fill=colfunc(np)) + theme_bw() + geom_text(aes(label=kill_ratio), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player") + geom_hline(yintercept = mean(warzone$kill_ratio),linetype="dashed", color = "red")
p <- p+ ggtitle("Kill death ratio")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("kill_ratio.jpg", quality = 100)
print(p)
dev.off()

# Average kills
p <- ggplot(warzone, aes(x = reorder(player, -avg_kills),avg_kills))
p <- p + geom_col(fill=colfunc(np)) + theme_bw() + geom_text(aes(label=avg_kills), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player")
p <- p+ ggtitle("Average kills per game")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("avg_kills.jpg", quality = 100)
print(p)
dev.off()

mean(warzone$win_pc)

# Wins
p <- ggplot(warzone, aes(x = reorder(player, -wins),wins))
p <- p + geom_col(fill=colfunc(np)) + theme_bw() + geom_text(aes(label=wins), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player")
p <- p+ ggtitle("Wins")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("wins.jpg", quality = 100)
print(p)
dev.off()

#Win %
p <- ggplot(warzone, aes(x = reorder(player, -win_pc),win_pc))
p <- p + geom_col(fill=colfunc(np)) + theme_bw() + geom_text(aes(label=win_pc), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player") + geom_hline(yintercept = mean(warzone$win_pc), linetype="dashed", color = "red")
p <- p+ ggtitle("Win %")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("win_pc.jpg", quality = 100)
print(p)
dev.off()

#Top10 
p <- ggplot(warzone, aes(x = reorder(player, -top10_pc),top10_pc))
p <- p + geom_col(fill=colfunc(np)) + theme_bw() + geom_text(aes(label=top10_pc), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player") + geom_hline(yintercept = mean(warzone$top10_pc), linetype="dashed", color = "red")
p <- p+ ggtitle("% of top 10 finishes")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("top10.jpg", quality = 100)
print(p)
dev.off()

#Top5 
p <- ggplot(warzone, aes(x = reorder(player, -top5_pc),top5_pc))
p <- p + geom_col(fill=colfunc(np)) + theme_bw() + geom_text(aes(label=top5_pc), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player") + geom_hline(yintercept = mean(warzone$top5_pc), linetype="dashed", color = "red")
p <- p+ ggtitle("% of top 5 finishes")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("top5.jpg", quality = 100)
print(p)
dev.off()

#Top5 conversions
p <- ggplot(warzone, aes(x = reorder(player, -top5conversion),top5conversion))
p <- p + geom_col(fill=colfunc(np)) + theme_bw() + geom_text(aes(label=top5conversion), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player") + geom_hline(yintercept = mean(warzone$top5conversion), linetype="dashed", color = "red")
p <- p+ ggtitle("% of top 5 conversions")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("top5conv.jpg", quality = 100)
print(p)
dev.off()

#Kills per hour
p <- ggplot(warzone, aes(x = reorder(player, -kills_hour),kills_hour))
p <- p + geom_col(fill=colfunc(np)) + theme_bw() + geom_text(aes(label=kills_hour), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player") + geom_hline(yintercept = mean(warzone$kills_hour), linetype="dashed", color = "red")
p <- p+ ggtitle("Kills per hour")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("kills_hour.jpg", quality = 100)
print(p)
dev.off()

#Hours for a win
p <- ggplot(warzone, aes(x = reorder(player, -wins_hour),wins_hour))
p <- p + geom_col(fill=colfunc(np)) + theme_bw() + geom_text(aes(label=wins_hour), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player") + geom_hline(yintercept = mean(warzone$wins_hour), linetype="dashed", color = "red")
p <- p+ ggtitle("Average hours played per win")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("hours_win.jpg", quality = 100)
print(p)
dev.off()

#minutes per win
p <- ggplot(warzone, aes(x = reorder(player, -minutes_win),minutes_win))
p <- p + geom_col(fill=colfunc(np)) + theme_bw() + geom_text(aes(label=minutes_win), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player") + geom_hline(yintercept = mean(warzone$minutes_win), linetype="dashed", color = "red")
p <- p+ ggtitle("Average minutes per win")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("minutes_win.jpg", quality = 100)
print(p)
dev.off()

p <- ggplot(warzone, aes(x=win_pc,y=kill_ratio)) + geom_point() + geom_text(aes(label=player),hjust=0, vjust=0)
p <- p+ ggtitle("Win % vs Kill ratio")
print(p)



############ Form analysis - last week data only ######################

#do stats for latest data only
latest_period <- 'Last week only'

#games played
p <- ggplot(warzoneweek, aes(x = reorder(player, -matchesPlayed),matchesPlayed))
p <- p + geom_col(fill=colfunc(np-1)) + theme_bw() + geom_text(aes(label=matchesPlayed), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player")
p <- p + ggtitle("Games played", subtitle = latest_period)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("week_games.jpg", quality = 100)
print(p)
dev.off()

#Kill ratio
p <- ggplot(warzoneweek, aes(x = reorder(player, -kdRatio),kdRatio))
p <- p + geom_col(fill=colfunc(np-1)) + theme_bw() + geom_text(aes(label=kdRatio), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player") + geom_hline(yintercept = mean(warzoneweek$kdRatio),linetype="dashed", color = "red")
p <- p+ ggtitle("Kill death ratio", subtitle = latest_period)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("week_kd.jpg", quality = 100)
print(p)
dev.off()

#Damage ratio
p <- ggplot(warzoneweek, aes(x = reorder(player, -damageRatio),damageRatio))
p <- p + geom_col(fill=colfunc(np-1)) + theme_bw() + geom_text(aes(label=damageRatio), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player") + geom_hline(yintercept = mean(warzoneweek$damageRatio),linetype="dashed", color = "red")
p <- p+ ggtitle("Damage ratio", subtitle = latest_period)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("week_dmg.jpg", quality = 100)
print(p)
dev.off()

#Gulag win pc
p <- ggplot(warzoneweek, aes(x = reorder(player, -gulag_win_pc),gulag_win_pc))
p <- p + geom_col(fill=colfunc(np-1)) + theme_bw() + geom_text(aes(label=gulag_win_pc), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player") + geom_hline(yintercept = mean(warzoneweek$gulag_win_pc),linetype="dashed", color = "red")
p <- p+ ggtitle("Gulag win %", subtitle = latest_period)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("week_gulag.jpg", quality = 100)
print(p)
dev.off()

#Truffles per game
p <- ggplot(warzoneweek, aes(x = reorder(player, -trufflesPerGame),trufflesPerGame))
p <- p + geom_col(fill=colfunc(np-1)) + theme_bw() + geom_text(aes(label=trufflesPerGame), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player") + geom_hline(yintercept = mean(warzoneweek$trufflesPerGame),linetype="dashed", color = "red")
p <- p+ ggtitle("Truffles per game (avg number of boxes)", subtitle = latest_period)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("week_truffles.jpg", quality = 100)
print(p)
dev.off()

#Revives per game
p <- ggplot(warzoneweek, aes(x = reorder(player, -revivesPerGame),revivesPerGame))
p <- p + geom_col(fill=colfunc(np-1)) + theme_bw() + geom_text(aes(label=revivesPerGame), position=position_dodge(width=0.9), vjust=-0.25)
p <- p + xlab("Player") + geom_hline(yintercept = mean(warzoneweek$revivesPerGame),linetype="dashed", color = "red")
p <- p+ ggtitle("Revives per game", subtitle = latest_period)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
print(p)
jpeg("week_revives.jpg", quality = 100)
print(p)
dev.off()

#No win data in latest week

# # Wins
# p <- ggplot(warzoneweek, aes(x = reorder(player, -wins),wins))
# p <- p + geom_col(fill=colfunc(np)) + theme_bw() + geom_text(aes(label=wins), position=position_dodge(width=0.9), vjust=-0.25)
# p <- p + xlab("Player")
# p <- p+ ggtitle("Wins",subtitle = latest_period)
# p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
# print(p)
# 
# #Win %
# p <- ggplot(warzoneweek, aes(x = reorder(player, -win_pc),win_pc))
# p <- p + geom_col(fill=colfunc(np)) + theme_bw() + geom_text(aes(label=win_pc), position=position_dodge(width=0.9), vjust=-0.25)
# p <- p + xlab("Player") + geom_hline(yintercept = mean(warzone$win_pc), linetype="dashed", color = "red")
# p <- p+ ggtitle("Win %", subtitle = latest_period)
# p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
# print(p)

top_kd <- filter(api_data,kdRatio==max(api_data$kdRatio))
top_win <- filter(api_data,win_pc==max(api_data$win_pc))
top_5conversion <- filter(api_data,top5conversion==max(api_data$top5conversion))

install.packages('rsconnect')

api_data2 <- read_csv("api_data.csv")

#### Gauge chart experimentation

library(plotly)

selected <- filter(api_data,player=="Doctor OKO")
#set title
if(1>0){title <- list(text = '★', font = list(outline = 'black', color = 'orange'))} else{title <- ""}

fig <- plot_ly(
  domain = list(x = c(0, 1), y = c(0, 1)),
  value = (selected$revives/selected$played),
  type = "indicator",
  mode = "gauge+number+delta",
  delta = list(reference = mean(api_data$revives/api_data$played)),
  title = title,
  gauge = list(
    axis =list(range = list(NULL, 2)),
    threshold = list(
      line = list(color = "red", width = 4),
      thickness = 0.75,
      value = mean(api_data$revives/api_data$played)))) 
fig <- fig %>%
  layout(margin = list(l=20,r=30))

fig

print(icon("user", lib='glyphicon'))

?plot_ly


sort(api_data$player)


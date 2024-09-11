DAVIESdata.2024.04.02 <- read.csv("C:/Users/George Ferridge/Downloads/DAVIESdata-2017-2024-raw.csv")
df <- DAVIESdata.2024.04.02

library(tidyverse)
library(ggplot2)
library(gcookbook)
library(ggrepel)
library(ggsci)
library(ggthemes)
library(NLP)
library(ggrepel)
library(worldfootballR)
library(jtools)
library(pwr)
library(ggalt)

summary(df)
df_group <- df %>% group_by(League, Team, Season, Gen..Role) %>% 
    summarise(DAVIES_TOTAL = sum(DAVIES), 
              DAVIES_weighted = sum(DAVIES*Min)/sum(Min))

df_2 <- df_group %>% group_by(League, Season, Gen..Role) %>%
    summarise(League_DAVIES = mean(DAVIES_weighted),
              Raw_League_DAVIES = mean(DAVIES_TOTAL),
              SD_League = sd(DAVIES_weighted),
              Raw_SD_League = sd(DAVIES_TOTAL))

df_together <- left_join(df_group,df_2, join_by(League, Season, Gen..Role))


df_together$Z <- (df_together$DAVIES_weighted - df_together$League_DAVIES)/df_together$SD_League



## At this point I see that the model is working as intended, 
## with teams coming out that make a lot of sense. 
## Manchester United's weakness in central defense, 
## Barcelona's strength in creativity


## Now I want to add wages to the player data, which I'll do from scraping FBRef. I'm hoping this works.


nations <- c("GER", "ENG", "FRA", "ESP", "ITA")

league_urls <- c()

for (year in 2018:2024){
  for (nation in nations){
    new_urls <- fb_league_urls(country=nation, gender = "M", year, tier = "1st")
    league_urls <- c(league_urls, new_urls)
  }
}

team_urls <- c()
for (league in league_urls){
  new_urls <- fb_teams_urls(league, time_pause=3)
  team_urls <- c(team_urls, new_urls)
}


team_urls[team_urls != "https://fbref.com/en/squads/60b5e41f/2017-2018/Hannover-96-Stats"]

wage_data = data.frame()

get_wages <- function(urls, df){
  tryCatch(
    {
      for (url in urls){
        stringurl <- as.String(url)
        new_df <- fb_squad_wages(team_urls = url, time_pause = 5)
        df <- rbind(df, new_df)
      }
      error = function(e) {
        print(stringurl + " failed")
        print(e)
      }
      return(df)
    }
  )
}
wage_data <- get_wages(team_urls, wage_data)

print(team_urls)



wage_data = data.frame()

get_wages <- function(urls, df){
  results_list <- list()  # Use a list to store data frames
  for (url in urls) {
    tryCatch({
      new_df <- fb_squad_wages(team_urls = url, time_pause = 5)
      results_list[[url]] <- new_df
    }, error = function(e) {
      print(paste(url, "failed"))
      print(e$message)  # Print error message
    })
  }
  # Combine all data frames into one
  df <- do.call(rbind, results_list)
  return(df)
}

# Assuming team_urls is defined somewhere in your script
wage_data <- get_wages(team_urls, wage_data)

wage_data$WeeklyWageEUR <- as.String(wage_data$WeeklyWageEUR)

wage_data$WeeklyWageEUR <- sapply(wage_data$WeeklyWageEUR, function(x) { x[[1]] })
wage_data$WeeklyWageGBP <- sapply(wage_data$WeeklyWageGBP, function(x) { x[[1]] })
wage_data$WeeklyWageUSD <- sapply(wage_data$WeeklyWageUSD, function(x) { x[[1]] })
wage_data$AnnualWageEUR <- sapply(wage_data$AnnualWageEUR, function(x) { x[[1]] })
wage_data$AnnualWageGBP <- sapply(wage_data$AnnualWageGBP, function(x) { x[[1]] })
wage_data$AnnualWageUSD <- sapply(wage_data$AnnualWageUSD, function(x) { x[[1]] })

write.csv(wage_data, "wage_data.csv", row.names=FALSE)
wage_data <- read.csv("wage_data.csv")


wage_data[] <- lapply(wage_data, function(x) if (is.list(x)) sapply(x, function(y) y[[1]], simplify = "vector") else x)

sapply(wage_data, is.list)

summary(wage_data)

wage_data$WeeklyWageEUR <- sapply(wage_data$WeeklyWageEUR, function(x) {
  if (length(x) == 0 || is.null(x[[1]])) NA else as.numeric(x[[1]])
})

wage_data$WeeklyWageEUR <- vapply(wage_data$WeeklyWageEUR, function(x) {
  if (length(x) > 0 && !is.null(x[[1]])) as.numeric(x[[1]]) else NA
}, numeric(1))

wage_data$WeeklyWageGBP<- sapply(wage_data$WeeklyWageGBP, function(x) {
  if (length(x) == 0 || is.null(x[[1]])) NA else as.numeric(x[[1]])
})

wage_data$WeeklyWageGBP <- vapply(wage_data$WeeklyWageGBP, function(x) {
  if (length(x) > 0 && !is.null(x[[1]])) as.numeric(x[[1]]) else NA
}, numeric(1))

wage_data$WeeklyWageUSD <- sapply(wage_data$WeeklyWageUSD, function(x) {
  if (length(x) == 0 || is.null(x[[1]])) NA else as.numeric(x[[1]])
})

wage_data$WeeklyWageUSD <- vapply(wage_data$WeeklyWageUSD, function(x) {
  if (length(x) > 0 && !is.null(x[[1]])) as.numeric(x[[1]]) else NA
}, numeric(1))

wage_data$AnnualWageEUR <- sapply(wage_data$AnnualWageEUR, function(x) {
  if (length(x) == 0 || is.null(x[[1]])) NA else as.numeric(x[[1]])
})

wage_data$AnnualWageEUR <- vapply(wage_data$AnnualWageEUR, function(x) {
  if (length(x) > 0 && !is.null(x[[1]])) as.numeric(x[[1]]) else NA
}, numeric(1))

wage_data$AnnualWageGBP <- sapply(wage_data$AnnualWageGBP, function(x) {
  if (length(x) == 0 || is.null(x[[1]])) NA else as.numeric(x[[1]])
})

wage_data$AnnualWageGBP <- vapply(wage_data$AnnualWageGBP, function(x) {
  if (length(x) > 0 && !is.null(x[[1]])) as.numeric(x[[1]]) else NA
}, numeric(1))

wage_data$AnnualWageUSD <- sapply(wage_data$AnnualWageUSD, function(x) {
  if (length(x) == 0 || is.null(x[[1]])) NA else as.numeric(x[[1]])
})

wage_data$AnnualWageUSD <- vapply(wage_data$AnnualWageUSD, function(x) {
  if (length(x) > 0 && !is.null(x[[1]])) as.numeric(x[[1]]) else NA
}, numeric(1))

wage_data$WeeklyWageEUR <- wage_data$WeeklyWageEUR_num


df_wage <- left_join(df, wage_data, by=c("Player", "Season"))

lm1 <- lm(DAVIES ~ WeeklyWageEUR+League+Gen..Role, df_wage)

summary(lm1)

lags <- df_wage

lags <- lags %>%
  arrange(Player, Season) %>%  # Sort by player and season
  group_by(Player) %>%         # Group by player, but not reducing data
  mutate(
    stats_last_season = lag(DAVIES, 1),     # Stats from the previous season
    stats_2_seasons_ago = lag(DAVIES, 2),   # Stats from two seasons ago
    stats_3_seasons_ago = lag(DAVIES, 3),   # Stats from three seasons ago
    stats_4_seasons_ago = lag(DAVIES, 4),   # Stats from four seasons ago
    stats_5_seasons_ago = lag(DAVIES, 5),   # Stats from five seasons ago
    stats_6_seasons_ago = lag(DAVIES, 6)    # Stats from six seasons ago
  ) %>%
  ungroup()  # Optionally remove the grouping structure

lm2 <- lm(DAVIES ~ WeeklyWageEUR+Gen..Role+League+Min, lags)

summary(lm2)


lags2 <- lags

interested_leagues <- c("Premier League", "La Liga", "Serie A", "Bundesliga", "Ligue 1")

# Filter the dataframe
filtered_lags2 <- lags2 %>%
  filter(League %in% interested_leagues)

# Display the filtered dataframe
df_predict <- filtered_lags2 %>%
  mutate(across(starts_with("stats"), ~replace_na(., 0)))

# Predict using the model
df_predict$predicted_stats <- predict(lm2, newdata = filtered_lags2, type = "response")

# View results
head(df_predict)


 
## Aggregating predictive values

grouped_predictions <- df_predict %>% group_by(League, Team.x, Season, Gen..Role) %>% 
  summarise(predicted_stats = sum(predicted_stats, na.rm=TRUE), 
            weighted_predictions = sum(predicted_stats*Min, na.rm=TRUE)/sum(Min, na.rm=TRUE))

league_predictions <- grouped_predictions %>% group_by(League, Season, Gen..Role) %>%
  summarise(League_predictions = mean(weighted_predictions),
            Raw_League_predictions = mean(predicted_stats),
            SD_predictions = sd(weighted_predictions),
            Raw_SD_predictions = sd(predicted_stats))


predictions_together <- left_join(grouped_predictions, league_predictions, join_by(League, Season, Gen..Role))


predictions_together$Z_predicted <- ifelse(predictions_together$SD_predictions > 0, (predictions_together$weighted_predictions - predictions_together$League_predictions)/predictions_together$SD_predictions, 0)

predictions_together$Team <- predictions_together$Team.x

data_plus_predictions <- left_join(df_together, predictions_together, join_by(Team, Season, Gen..Role))




total_curiousity <- data_plus_predictions[data_plus_predictions$Team == "Newcastle Utd",]

total_curiousity$plotting_z <- total_curiousity$Z.x - total_curiousity$Z_predicted

ggplot(total_curiousity, aes(x=Season, label=Gen..Role, group=Gen..Role))+
  geom_point(aes(y=plotting_z, col=Gen..Role), size=5)+
  scale_color_brewer(palette="Paired")+
  geom_hline(yintercept=0)+
  ylab("Difference in DAVIES Z score from predicted DAVIES Z Score")+
  xlab("Season")+
  labs(title="Klopp at Liverpool", subtitle="Modest overperforamnce from almost the whole squad")+
  theme_classic()+
  theme(text = element_text(family="Roboto"),axis.text.x=element_text(angle=45, hjust=1), axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"), 
        plot.title = element_text(size=24, face="bold"),
        plot.background=element_rect(fill="#F0EDEB"),
        panel.background=element_rect(fill="#F0EDEB"),
        legend.title = element_text(face="bold"),
        legend.background = element_rect(fill="#F0EDEB", color= "#F0EDEB"))

getwd()
ggsave(filename="KloppNew.png", width=11, height=7, dpi=1200)

Managers_DAVIES <- read.csv("Managers_DAVIES.csv")
data_plus_predictions$League <- data_plus_predictions$League.x
managers <- left_join(Managers_DAVIES, data_plus_predictions, join_by(League, Gen..Role, Team, Season))

total_curiousity <- managers %>%
  filter(!is.na(Manager), Manager == "Mauricio Pochettino")

lm5 <- lm(Z.x ~ Manager+Z_predicted, data=managers)
lm6 <- lm(DAVIES_weighted.y ~ Manager + weighted_predictions, managers)

summary(lm5)

lm6 <- lm(DAVIES_weighted.y ~ weighted_predictions, managers)

plot_summs(lm6, coefs = c( "ManagerPep Guardiola", "ManagerJurgen Klopp", "ManagerMichel","ManagerRoberto De Zerbi",
                           "ManagerMauricio Pochettino", "ManagerXavi", "ManagerXabi Alonso", "ManagerSteve Bruce",
                           "ManagerScott Parker", "ManagerRoy Hodgson", "ManagerMikel Arteta", "ManagerJose Mourinho",
                           "ManagerErik ten Hag", "ManagerChris Wilder", "ManagerChris Hughton", "ManagerGraham Potter"))
summary(lm6)

ggsave(filename="SelectedManagers.png", width=11, height=7, dpi=1200)


data <- data[data <25]
data

lm6$coefficients
hist(lm6$coefficients, breaks=20)

abline(v=mean(lm6$coefficients), col="red", lwd=2)
lines(density(lm6$coefficients), col="blue", lwd=2)

outliers(lm6$coefficients)

print(lm6)



## Wages to download for additional nations


nations <- c("NED", "BRA", "ARG", "USA", "POR")

league_urls <- c()

for (year in 2018:2024){
  for (nation in nations){
    new_urls <- fb_league_urls(country=nation, gender = "M", year, tier = "1st")
    league_urls <- c(league_urls, new_urls)
  }
}

new_team_urls <- c()
for (league in league_urls){
  new_urls <- fb_teams_urls(league, time_pause=3)
  new_team_urls <- c(new_team_urls, new_urls)
}


get_wages <- function(urls, df){
  results_list <- list()  # Use a list to store data frames
  for (url in urls) {
    tryCatch({
      new_df <- fb_squad_wages(team_urls = url, time_pause = 5)
      results_list[[url]] <- new_df
    }, error = function(e) {
      print(paste(url, "failed"))
      print(e$message)  # Print error message
    })
  }
  # Combine all data frames into one
  df <- do.call(rbind, results_list)
  return(df)
}

expanded_wage_data = data.frame()

expanded_wage_data <- get_wages(new_team_urls, expanded_wage_data)


## Making a Klopp chart for visualization purposes, from 2018-2019 season

klopp <- managers[managers$Team=="Liverpool" & managers$Season=="2018-2019",]


ggplot(klopp, aes(x=DAVIES_weighted.x, xend=weighted_predictions, y=reorder(Gen..Role, DAVIES_weighted.x)))+
  geom_dumbbell(color="gray75", size_x = 3, size_xend = 3, colour_x = "firebrick", colour_xend = "black")+
  theme_classic()

klopp_aggregate <- managers[managers$Manager == "Jurgen Klopp",]
klopp_aggregate <- klopp_aggregate[!is.na(klopp_aggregate$Manager),]

agg_klopp <- klopp_aggregate %>% group_by(Gen..Role) %>% 
  summarise(DAVIES_TOTAL = mean(DAVIES_weighted.x, na.rm=TRUE), 
            predictions = mean(weighted_predictions, na.rm=TRUE))


ggplot(agg_klopp, aes(x=DAVIES_TOTAL, xend=predictions ,y=reorder(Gen..Role, predictions)))+
  geom_dumbbell(color="gray75", size_x = 4, size_xend = 3, colour_x = "firebrick", colour_xend = "black")+
  theme_classic()+
  xlab("DAVIES")+
  ylab("")+
  labs(caption="Data from FBRef and ASA")+
  ggtitle("Jurgen Klopp's management quality by position group", subtitle = "Red points represent actual performance, black represent a weighted prediction based on player quality")+
  theme(text = element_text(family="Roboto"),axis.text.x=element_text(angle=45, hjust=1), axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"), 
        plot.title = element_text(size=24, face="bold"),
        plot.background=element_rect(fill="#F0EDEB"),
        panel.background=element_rect(fill="#F0EDEB"))

ggsave(filename="Klopp_aggregated.png", width=11, height=7, dpi=1200)

### Dyche


dyche_aggregate <- managers[managers$Manager == "Sean Dyche",]
dyche_aggregate <- dyche_aggregate[!is.na(dyche_aggregate$Manager),]

agg_dyche <- dyche_aggregate %>% group_by(Gen..Role) %>% 
  summarise(DAVIES_TOTAL = mean(DAVIES_weighted.x, na.rm=TRUE), 
            predictions = mean(weighted_predictions, na.rm=TRUE))


ggplot(agg_dyche, aes(x=DAVIES_TOTAL, xend=predictions ,y=reorder(Gen..Role, predictions)))+
  geom_dumbbell(color="gray75", size_x = 4, size_xend = 3, colour_x = "firebrick", colour_xend = "black")+
  theme_classic()+
  xlab("DAVIES")+
  ylab("")+
  labs(caption="Data from FBRef and ASA")+
  ggtitle("Jurgen Klopp's management quality by position group", subtitle = "Red points represent actual performance, black represent a weighted prediction based on player quality")+
  theme(text = element_text(family="Roboto"),axis.text.x=element_text(angle=45, hjust=1), axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"), 
        plot.title = element_text(size=24, face="bold"),
        plot.background=element_rect(fill="#F0EDEB"),
        panel.background=element_rect(fill="#F0EDEB"))

ggsave(filename="Dyche_aggregated.png", width=11, height=7, dpi=1200)

min(managers$DAVIES_weighted.x)
min(managers$weighted_predictions, na.rm=TRUE)

hist(managers$weighted_predictions)
hist(managers$DAVIES_weighted.x)

ggplot(managers)+geom_density(aes(x=weighted_predictions), col="blue", alpha=0.2)+
  geom_density(aes(x=DAVIES_weighted.x), col="red", alpha=0.2)+
  geom_histogram(aes(x=weighted_predictions), col="red", alpha=0.2)+
  geom_histogram(aes(x=DAVIES_weighted.x), col="blue", alpha=0.2)

lm7 <- lm(Z.x ~ Z_predicted, managers)

summary(lm7)
### Existing ideas to work with: 
## a) subset entire dataset, aggregate for managers by position group and create their weighted davies average, the weighted prediction average, and the difference average
## b) Rank coefficient values from regression
## c) Look at difference as a percentage of prediction to gain a measure of effectiveness, so diff/predict. -1 means the manager is halving the efficiency, 1 means they're doubling it. Or just look at actual/predicted. 1 would be expected, below 1 ineffective, above 1 effective

## Let's make a new dataset, normalize values, and then look at percentage changes

# Big one right now is managers, going to make that man2

man2 <- managers

range(man2$DAVIES_weighted.x)
range(man2$weighted_predictions, na.rm=TRUE)

man2$DW_norm <- (man2$DAVIES_weighted.x+12)/35
man2$Weight_norm <- (man2$weighted_predictions+12)/35

range(man2$DW_norm)
range(man2$Weight_norm, na.rm=TRUE)

man2$mandex <- (man2$DW_norm-man2$Weight_norm)/man2$Weight_norm

range(man2$mandex, na.rm=TRUE)

man2 <- man2[order(-man2$mandex),]


klopp_aggregate <- man2[man2$Manager == "Jurgen Klopp",]
klopp_aggregate <- klopp_aggregate[!is.na(klopp_aggregate$Manager),]

agg_klopp <- klopp_aggregate %>% group_by(Gen..Role) %>% 
  summarise(Performance = mean(mandex, na.rm=TRUE))

ggplot(agg_klopp, aes(x=Performance, y=reorder(Gen..Role, Performance)))+geom_col()


klopp_pep <- man2[man2$Manager == "Jurgen Klopp" | man2$Manager == "Pep Guardiola",]
klopp_pep <- klopp_pep[!is.na(klopp_pep$Manager),]

comparison <- klopp_pep %>% group_by(Manager, Gen..Role) %>% 
  summarise(Performance = mean(mandex, na.rm=TRUE))

ggplot(comparison, aes(x=Performance, y=fct_relevel(Gen..Role, "Finisher", "Dribbler", "Creator", "Midfielder", "Deep Midfielder", "Wide Def", "Central Def")))+
  geom_col(aes(group=Manager, fill=Manager), position=position_dodge(0.5), width=0.5, alpha=0.9)+
  scale_fill_manual(values=c("#D10022", "#84BBFF"))+theme_classic()+xlab("Performance (controlling for player quality)")+
  ylab("")+
  labs(caption="Data from Opta and American Soccer Analysis")+
  ggtitle("Klopp vs Pep", subtitle = "Performances on the index")+
  xlim(-0.75,0.75)+
  geom_vline(xintercept=0)+
  theme(text = element_text(family="Roboto"),axis.text.x=element_text(angle=45, hjust=1), axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"), 
        plot.title = element_text(size=24, face="bold"),
        plot.background=element_rect(fill="#F0EDEB"),
        panel.background=element_rect(fill="#F0EDEB"))

ggsave(filename="Klopp_Pep_final.png", width=11, height=7, dpi=1200)


man3 <- man2

man3 <- man3 %>% group_by(Manager, Gen..Role) %>% 
  summarise(Performance = mean(mandex, na.rm=TRUE), manager_count=n())

man3$max_seasons <- 7

man3$corrected_index <- man3$Performance/(man3$max_seasons-man3$manager_count +1)

man3$readable_index <- man3$corrected_index*100

man_CB <- man3[man3$Gen..Role=="Central Def",]
man_WB <- man3[man3$Gen..Role=="Wide Def",]
man_ST <- man3[man3$Gen..Role == "Finisher",]

man_ST <- man_ST[order(-man_ST$Performance),] 



#### Okay now I need to take the initial code that I had and aggregate them both not by position groups but by teams and years
## This will generate my manager index values
## The dataframes I need are the player predictions one and the player actual one, ungrouped


## Player predictions are df_predict
## Overall dataset is df


### df needs to have aggregate values calculated for league and individuals

df_all <- df
df_newpred <- df_predict

df_group2 <- df_all %>% group_by(League, Team, Season) %>% 
  summarise(DAVIES_TOTAL = sum(DAVIES), 
            DAVIES_weighted = sum(DAVIES*Min)/sum(Min))

df_3 <- df_group2 %>% group_by(League, Season) %>%
  summarise(League_DAVIES = mean(DAVIES_weighted),
            Raw_League_DAVIES = mean(DAVIES_TOTAL),
            SD_League = sd(DAVIES_weighted),
            Raw_SD_League = sd(DAVIES_TOTAL))

df_together2 <- left_join(df_group2,df_3, join_by(League, Season))
df_together2$Z <- (df_together2$DAVIES_weighted - df_together2$League_DAVIES)/df_together2$SD_League




### Now doing the same for predictive values and combining

## Aggregating predictive values

grouped2 <- df_newpred %>% group_by(League, Team.x, Season) %>% 
  summarise(predictions = sum(predicted_stats, na.rm=TRUE), 
            weighted_predictions = sum(predicted_stats*Min, na.rm=TRUE)/sum(Min, na.rm=TRUE))

leagues2 <- grouped2 %>% group_by(League, Season) %>%
  summarise(League_predictions = mean(weighted_predictions),
            Raw_League_predictions = mean(predictions),
            SD_predictions = sd(weighted_predictions),
            Raw_SD_predictions = sd(predictions))


pred_tog <- left_join(grouped2, leagues2, join_by(League, Season))


pred_tog$Z_predicted <- ifelse(pred_tog$SD_predictions > 0, (pred_tog$weighted_predictions - pred_tog$League_predictions)/pred_tog$SD_predictions, 0)

pred_tog$Team <- pred_tog$Team.x

dpp2 <- left_join(df_together2, pred_tog, join_by(Team, Season))

dpp2$League <- dpp2$League.x



## Managers_DAVIES needs some adjustment before adding them in, otherwise things go a bit awry

managers2 <- Managers_DAVIES

managers2 <- managers2 %>% group_by(Team, League, Season, Manager) %>%
  summarise(bogie= mean(DAVIES_TOTAL))
man_total <- left_join(dpp2, managers2, join_by(League, Team, Season))

lm5 <- lm(Z ~ Manager+Z_predicted, data=man_total)
lm6 <- lm(DAVIES_weighted ~ Manager + weighted_predictions, man_total)

summary(lm5)
summary(lm6)

range(man_total$DAVIES_weighted, na.rm=TRUE)
range(man_total$weighted_predictions, na.rm=TRUE)

man_total$DW_norm <- (man_total$DAVIES_weighted+2)/7
man_total$Weight_norm <- (man_total$weighted_predictions+2)/7


range(man_total$DW_norm, na.rm=TRUE)
range(man_total$Weight_norm, na.rm=TRUE)

man_total$mandex <- (man_total$DW_norm-man_total$Weight_norm)/man_total$Weight_norm


man_total <- man_total[order(-man_total$mandex),]



man_total_agg <- man_total %>% group_by(Manager) %>%
  summarise(index = mean(mandex, na.rm=TRUE), count=n())

man_total_agg <- man_total_agg[order(-man_total_agg$index),]

man_total_agg$max_seasons <- 7

man_total_agg$corrected_index <- man_total_agg$index/(man_total_agg$max_seasons-man_total_agg$count +1)

man_total_agg$readable_index <- man_total_agg$corrected_index*100
man_total_agg <- man_total_agg[order(-man_total_agg$readable_index),]



## Playing around


curiousity <- df_together[order(df_together$Z),]

barca <- curiousity[curiousity$Team == "Barcelona",]
barca_creators <- barca[barca$Gen..Role == "Creator",]

ggplot(barca_creators, aes(x=Season, y=DAVIES_weighted, group=DAVIES_weighted)) + 
  geom_col(fill="#004C99",alpha=0.1) + 
  geom_line(col="#A60042",group=1, size=1.5)+ 
  theme_classic()+
  ylab("Weighted DAVIES values of Barcelona's Creators")+
  xlab("Season")+
  labs(title="The Messi Effect", subtitle="Barca's creators dropped down massively after Messi's departure")+
  theme_classic()+
  geom_hline(yintercept=0)+
  theme(text = element_text(family="Roboto"),axis.text.x=element_text(angle=45, hjust=1), axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"), 
        plot.title = element_text(size=24, face="bold"),
        plot.background=element_rect(fill="#F0EDEB"),
        panel.background=element_rect(fill="#F0EDEB"),
        legend.title = element_text(face="bold"),
        legend.background = element_rect(fill="#F0EDEB", color= "#F0EDEB"))

ggsave(filename="Messi_DAVIES.png", width=11, height=7, dpi=1200)


manutd_units <- curiousity[curiousity$Team == "Manchester Utd",]
write.csv(df_together, file="DAVIES_Z_Scores.csv")


ggplot(manutd_units, aes(x=Season, y=Z, label=Gen..Role, group=Gen..Role))+
  geom_point(aes(col=Gen..Role), size=2)+
  geom_line(aes(col=Gen..Role), size=1.5)+
  geom_hline(yintercept=0, linetype="dashed")+
  theme_classic()



arsenal_units <- curiousity[curiousity$Team== "Arsenal",]

install.packages("ggsci")

ggplot(arsenal_units, aes(x=Season, y=Z, label=Gen..Role, group=Gen..Role))+
  geom_point(aes(col=Gen..Role), size=2)+
  scale_color_brewer(palette="Paired")+
  geom_line(aes(col=Gen..Role), size=1)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=3.4, linetype="longdash")+
  geom_text(label = "Arteta appointed", y=2.5, x=3.4, hjust=-0.1, col="black", size=3)+
  ylab("DAVIES performance relative to mean (Z score)")+
  xlab("Season")+
  labs(title="Arteta's Arsenal", subtitle="All positional units have improved under Arteta, except for Finishers")+
  theme_classic()+
  theme(text = element_text(family="Roboto"),axis.text.x=element_text(angle=45, hjust=1), axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"), 
        plot.title = element_text(size=24, face="bold"),
        plot.background=element_rect(fill="#F0EDEB"),
        panel.background=element_rect(fill="#F0EDEB"),
        legend.title = element_text(face="bold"),
        legend.background = element_rect(fill="#F0EDEB", color= "#F0EDEB"))

ggsave(filename="Arteta.png", width=11, height=7, dpi=1200)


burnley_units <- curiousity[curiousity$Team == "Burnley",]
everton_units <- curiousity[curiousity$Team == "Everton",]


ggplot(everton_units, aes(x=Season, y=Z, label=Gen..Role, group=Gen..Role))+
  geom_point(aes(col=Gen..Role), size=2)+
  scale_color_brewer(palette="Paired")+
  geom_line(aes(col=Gen..Role), size=1)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=6, linetype="longdash")+
  geom_text(label = "Arteta appointed", y=2.5, x=3.4, hjust=-0.1, col="black", size=3)+
  ylab("DAVIES performance relative to mean (Z score)")+
  xlab("Season")+
  labs(title="Dyche's Impact at Everton", subtitle="Surprise surprise, dribbling is down...")+
  theme_classic()+
  theme(text = element_text(family="Roboto"),axis.text.x=element_text(angle=45, hjust=1), axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"), 
        plot.title = element_text(size=24, face="bold"),
        plot.background=element_rect(fill="#F0EDEB"),
        panel.background=element_rect(fill="#F0EDEB"),
        legend.title = element_text(face="bold"),
        legend.background = element_rect(fill="#F0EDEB", color= "#F0EDEB"))


ggsave(filename="Dyche.png", width=11, height=7, dpi=1200)


ggplot(burnley_units, aes(x=Season, y=Z, label=Gen..Role, group=Gen..Role))+
  geom_point(aes(col=Gen..Role), size=2)+
  scale_color_brewer(palette="Paired")+
  geom_line(aes(col=Gen..Role), size=1)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=5, linetype="longdash")+
  geom_text(label = "Arteta appointed", y=2.5, x=3.4, hjust=-0.1, col="black", size=3)+
  ylab("DAVIES performance relative to mean (Z score)")+
  xlab("Season")+
  labs(title="Dyche's Impact at Burnley", subtitle="His only overperforming unit? Center Backs")+
  theme_classic()+
  theme(text = element_text(family="Roboto"),axis.text.x=element_text(angle=45, hjust=1), axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"), 
        plot.title = element_text(size=24, face="bold"),
        plot.background=element_rect(fill="#F0EDEB"),
        panel.background=element_rect(fill="#F0EDEB"),
        legend.title = element_text(face="bold"),
        legend.background = element_rect(fill="#F0EDEB", color= "#F0EDEB"))

ggsave(filename="Dyche2.png", width=11, height=7, dpi=1200)


bayer_units <- curiousity[curiousity$Team == "Leverkusen",]


ggplot(bayer_units, aes(x=Season, y=Z, label=Gen..Role, group=Gen..Role))+
  geom_point(aes(col=Gen..Role), size=2)+
  scale_color_brewer(palette="Paired")+
  geom_line(aes(col=Gen..Role), size=1)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=6, linetype="longdash")+
  geom_text(label = "Alonso \nappointed", y=2.5, x=6, hjust=-0.1, col="black", size=3)+
  ylab("DAVIES performance relative to mean (Z score)")+
  xlab("Season")+
  labs(title="Alonso got Bayer Firing Fast", subtitle="Group DAVIES scores for Bayer Leverkusen")+
  theme_classic()+
  theme(text = element_text(family="Roboto"),axis.text.x=element_text(angle=45, hjust=1), axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"), 
        plot.title = element_text(size=24, face="bold"),
        plot.background=element_rect(fill="#F0EDEB"),
        panel.background=element_rect(fill="#F0EDEB"),
        legend.title = element_text(face="bold"),
        legend.background = element_rect(fill="#F0EDEB", color= "#F0EDEB"))

brighton_units <- curiousity[curiousity$Team == "Brighton",]

install.packages("gcookbook")


ggplot(brighton_units, aes(x=Season, y=Z, label=Gen..Role, group=Gen..Role))+
  geom_point(aes(col=Gen..Role), size=2)+
  scale_color_brewer(palette="Paired")+
  geom_line(aes(col=Gen..Role), size=1)+
  ylim(-3,3)+
  geom_hline(yintercept=0)+
  annotate("rect", xmin=1, xmax=1.9, ymin=-3, ymax=3, alpha=0.1, fill="#009900")+
  annotate("rect", xmin=1.9, xmax=5.5, ymin=-3, ymax=3, alpha=0.1, fill="#0054A6")+
  annotate("rect", xmin=5.5, xmax=7.1, ymin=-3, ymax=3, alpha=0.1, fill="#FF0000")+
  geom_text(label = "Hughton", y=2.8, x=1.5, col="black", size=3)+
  geom_text(label = "Potter", y=2.8, x=3.8, col="black", size=3)+
  geom_text(label = "De Zerbi", y=2.8, x=6.3, col="black", size=3)+
  ylab("DAVIES performance relative to mean (Z score)")+
  xlab("Season")+
  labs(title="The Changes at Brighton", subtitle="Potter and De Zerbi seem to focus on different things")+
  theme_classic()+
  theme(text = element_text(family="Roboto"),axis.text.x=element_text(angle=45, hjust=1), axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"), 
        plot.title = element_text(size=24, face="bold"),
        plot.background=element_rect(fill="#F0EDEB"),
        panel.background=element_rect(fill="#F0EDEB"),
        legend.title = element_text(face="bold"),
        legend.background = element_rect(fill="#F0EDEB", color= "#F0EDEB"))

ggsave(filename="Brighton.png", width=11, height=7, dpi=1200)






##### Practicing Radar charts


### let's pivot

man4 <- man3

man5 <- man4 %>%
  pivot_wider(
    id_cols = Manager,             # Specify the ID column
    names_from = Gen..Role,       # Specify the column to get the new column names from
    values_from = readable_index       # Specify the column to get the values from
  )

max_min <- data.frame(
  Manager = c("Max", "Min"),
  'Central Def' = c(max(man5$`Central Def`,na.rm=TRUE), min(man5$`Central Def`,na.rm=TRUE)), Creator = c(max(man5$Creator,na.rm=TRUE), min(man5$Creator,na.rm=TRUE)), 'Deep Midfielder' = c(max(man5$`Deep Midfielder`,na.rm=TRUE), min(man5$`Deep Midfielder`,na.rm=TRUE)),
   'Dribbler'= c(max(man5$Dribbler,na.rm=TRUE), min(man5$Dribbler,na.rm=TRUE)), Finisher = c(max(man5$Finisher,na.rm=TRUE), min(man5$Finisher,na.rm=TRUE)), Midfielder = c(max(man5$Midfielder,na.rm=TRUE), min(man5$Midfielder,na.rm=TRUE)),
  'Wide Def' = c(max(man5$`Wide Def`,na.rm=TRUE), min(man5$`Wide Def`,na.rm=TRUE))
)

names(max_min)[names(max_min) == "Central.Def"] <- "Central Def"
names(max_min)[names(max_min) == "Wide.Def"] <- "Wide Def"
names(max_min)[names(max_min) == "Deep.Midfielder"] <- "Deep Midfielder"


man_radar <- rbind(max_min, man5)

library(fmsb)
Klopp_data <- man_radar[man_radar$Manager == "Max" | man_radar$Manager == "Min" | man_radar$Manager == "Jurgen Klopp", ]
man_radar$`Central Def` <- as.numeric(man_radar$`Central Def`)
man_radar$`Wide Def` <- as.numeric(man_radar$`Wide Def`)
man_radar$Creator <- as.numeric(man_radar$Creator)
man_radar$Dribbler <- as.numeric(man_radar$Dribbler)
man_radar$Midfielder <- as.numeric(man_radar$Midfielder)
man_radar$`Deep Midfielder` <- as.numeric(man_radar$`Deep Midfielder`)
man_radar$Finisher <- as.numeric(man_radar$Finisher)

Klopp_data <- na.omit(Klopp_data)

row.names(Klopp_data) <- c("Max", "Min", "Jurgen Klopp")
Klopp_data <- Klopp_data[,-1]
radarchart(Klopp_data, pcol="#D10022", plwd=4)




#### Working out some percentiles for the different data as that may lead to nicer radar plots

man_total_agg$percentile <- ecdf(man_total_agg$readable_index)(man_total_agg$readable_index)*100


### So that all works for the main statistic, let's calculate it for the rest and then work on a radar or two

man_total_agg$percentile <- ecdf(man_total_agg$readable_index)(man_total_agg$readable_index)*100

man_groups_percentiles <- man3


###### Can calculate percentiles for positional statistics using man_radar

man_radar_pos <- man_radar

man_radar_pos$CB_p <- ecdf(man_radar_pos$`Central Def`)(man_radar_pos$`Central Def`)*100
man_radar_pos$CRE_p <- ecdf(man_radar_pos$Creator)(man_radar_pos$Creator)*100
man_radar_pos$DM_p <- ecdf(man_radar_pos$`Deep Midfielder`)(man_radar_pos$`Deep Midfielder`)*100
man_radar_pos$DR_p <- ecdf(man_radar_pos$Dribbler)(man_radar_pos$Dribbler)*100
man_radar_pos$FI_p <- ecdf(man_radar_pos$Finisher)(man_radar_pos$Finisher)*100
man_radar_pos$MI_p <- ecdf(man_radar_pos$Midfielder)(man_radar_pos$Midfielder)*100
man_radar_pos$WB_p <- ecdf(man_radar_pos$`Wide Def`)(man_radar_pos$`Wide Def`)*100

man_radar_pos2 <- na.omit(man_radar_pos)
man_radar_pos2 <-man_radar_pos2 %>%
  add_row(Manager="Median", CB_p=50, CRE_p=50, DM_p=50, DR_p=50, FI_p=50, MI_p=50, WB_p=50)
row.names(man_radar_pos2) <- man_radar_pos2$Manager


poch <- man_radar_pos2[man_radar_pos2$Manager == "Mauricio Pochettino"|man_radar_pos2$Manager == "Jurgen Klopp"| man_radar_pos2$Manager == "Max" | man_radar_pos2$Manager == "Min" | man_radar_pos2$Manager == "Median",]
pepklopp <- man_radar_pos2[man_radar_pos2$Manager == "Jurgen Klopp" | man_radar_pos2$Manager == "Pep Guardiola" | man_radar_pos2$Manager == "Max" | man_radar_pos2$Manager == "Min",]

pepklopp <- pepklopp %>%
  select(CB_p, CRE_p, DM_p, DR_p, FI_p, MI_p, WB_p)

poch <- poch %>%
  select(CB_p, CRE_p, DM_p, DR_p, FI_p, MI_p, WB_p) %>%
  rename("Center Backs" = CB_p,
         "Creators" = CRE_p,
         "CDMs" = DM_p,
         "Dribblers" = DR_p,
         "Finishers" = FI_p,
         "Midfielders" = MI_p,
         "Wing Backs" = WB_p)


radarchart(poch, pcol=c("firebrick3", "dodgerblue4", "gray"), plty = c(1,1,3), plwd=4)

dychey <- man_radar_pos2[man_radar_pos2$Manager == "Sean Dyche"| man_radar_pos2$Manager == "Max" | man_radar_pos2$Manager == "Min" | man_radar_pos2$Manager == "Median",]

dychey <- dychey %>%
  select(CB_p, CRE_p, DM_p, DR_p, FI_p, MI_p, WB_p) %>%
  rename("Center Backs" = CB_p,
         "Creators" = CRE_p,
         "CDMs" = DM_p,
         "Dribblers" = DR_p,
         "Finishers" = FI_p,
         "Midfielders" = MI_p,
         "Wing Backs" = WB_p)


radarchart(dychey, pcol=c("#0052CC", "dimgrey"), plty = c(1,3), plwd=4)


managerELO <- read.csv("coaches_ranking.csv")


managerELO$Manager <- managerELO$Name

install.packages("stringi")

# Load the stringi package
library(stringi)

managerELO$Manager <- stri_trans_general(managerELO$Manager, "Latin-ASCII")
ELOvsINDEX <- left_join(man_total_agg, managerELO, by = "Manager")

lm8 <- lm(Coach.Rating~corrected_index, ELOvsINDEX)

summary(lm8)


ggplot(data=ELOvsINDEX, aes(x=corrected_index, y=Coach.Rating))+geom_point()+
  theme_classic()+
  geom_smooth(method='lm', col="firebrick")+
  xlab("Index Performance")+
  ylab("Manager ELO")+
  labs(caption="Where the Data is From ")+
  ggtitle("Title", subtitle = "Subtitle")+
  theme(text = element_text(family="Roboto"),axis.text.x=element_text(angle=45, hjust=1), axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"), 
        plot.title = element_text(size=24, face="bold"),
        plot.background=element_rect(fill="#F0EDEB"),
        panel.background=element_rect(fill="#F0EDEB"))


cor.test(ELOvsINDEX$corrected_index, ELOvsINDEX$Coach.Rating)



ELOvsINDEX_outliersout <- ELOvsINDEX[!(ELOvsINDEX$Manager %in% c("Jurgen Klopp", "Pep Guardiola")), ]

lm9 <- lm(Coach.Rating~corrected_index, ELOvsINDEX_outliersout)

summary(lm9)


cor.test(ELOvsINDEX_outliersout$corrected_index, ELOvsINDEX_outliersout$Coach.Rating)



ggplot(data=ELOvsINDEX_outliersout, aes(x=corrected_index, y=Coach.Rating))+geom_point()+
  theme_classic()+
  geom_smooth(method='lm', col="firebrick")+
  xlab("Index Performance")+
  ylab("Manager ELO")+
  labs(caption="Where the Data is From ")+
  ggtitle("Title", subtitle = "Subtitle")+
  theme(text = element_text(family="Roboto"),axis.text.x=element_text(angle=45, hjust=1), axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"), 
        plot.title = element_text(size=24, face="bold"),
        plot.background=element_rect(fill="#F0EDEB"),
        panel.background=element_rect(fill="#F0EDEB"))

install.packages("pwr")
library(pwr)
pwr.t.test(d=0.5, sig.level=0.05, power=0.6, type = "two.sample")
pwr.anova.test(k=3, f=0.25, sig.level=0.05, power=0.6)

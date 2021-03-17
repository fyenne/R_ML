# stage2 W
regresults <- read.csv("./ncaaw-march-mania-2021_2/WRegularSeasonDetailedResults.csv")
results <- read.csv("./ncaaw-march-mania-2021_2/WNCAATourneyDetailedResults.csv")
sub <- read.csv("./ncaaw-march-mania-2021_2/WSampleSubmissionStage2.csv")
seeds <- read.csv("./ncaaw-march-mania-2021_2/WNCAATourneySeeds.csv")

seeds$Seed = as.numeric(substring(seeds$Seed,2,3))


### Collect regular season results - double the data by swapping team positions

r1 = regresults[, c("Season", "DayNum", "WTeamID", "WScore", "LTeamID", "LScore", "NumOT", "WFGA", "WAst", "WBlk", "LFGA", "LAst", "LBlk")]
r2 = regresults[, c("Season", "DayNum", "LTeamID", "LScore", "WTeamID", "WScore", "NumOT", "LFGA", "LAst", "LBlk", "WFGA", "WAst", "WBlk")]
names(r1) = c("Season", "DayNum", "T1", "T1_Points", "T2", "T2_Points", "NumOT", "T1_fga", "T1_ast", "T1_blk", "T2_fga", "T2_ast", "T2_blk")
names(r2) = c("Season", "DayNum", "T1", "T1_Points", "T2", "T2_Points", "NumOT", "T1_fga", "T1_ast", "T1_blk", "T2_fga", "T2_ast", "T2_blk")
regular_season = rbind(r1, r2)


### Collect tourney results - double the data by swapping team positions

t1 = results[, c("Season", "DayNum", "WTeamID", "LTeamID", "WScore", "LScore")] %>% mutate(ResultDiff = WScore - LScore)
t2 = results[, c("Season", "DayNum", "LTeamID", "WTeamID", "LScore", "WScore")] %>% mutate(ResultDiff = LScore - WScore)
names(t1) = c("Season", "DayNum", "T1", "T2", "T1_Points", "T2_Points", "ResultDiff")
names(t2) = c("Season", "DayNum", "T1", "T2", "T1_Points", "T2_Points", "ResultDiff")
tourney = rbind(t1, t2)


### Fit GLMM on regular season data (selected march madness teams only) - extract random effects for each team

march_teams = select(seeds, Season, Team = TeamID)
X =  regular_season %>% 
  inner_join(march_teams, by = c("Season" = "Season", "T1" = "Team")) %>% 
  inner_join(march_teams, by = c("Season" = "Season", "T2" = "Team")) %>% 
  select(Season, T1, T2, T1_Points, T2_Points, NumOT) %>% distinct()
X$T1 = as.factor(X$T1)
X$T2 = as.factor(X$T2)

quality = list()
for (season in unique(X$Season)) {
  glmm = 
    glmer(I(T1_Points > T2_Points) ~  (1 | T1) + (1 | T2), 
          data = X[X$Season == season & X$NumOT == 0, ], 
          family = binomial) 
  
  random_effects = ranef(glmm)$T1
  quality[[season]] = 
    data.frame(Season = season, 
               Team_Id = as.numeric(row.names(random_effects)), 
               quality = exp(random_effects[,"(Intercept)"]))
}
quality = do.call(rbind, quality)


### Regular season statistics

season_summary = 
  regular_season %>%
  mutate(win14days = ifelse(DayNum > 118 & T1_Points > T2_Points, 1, 0),
         last14days = ifelse(DayNum > 118, 1, 0)) %>% 
  group_by(Season, T1) %>%
  summarize(
    WinRatio14d = sum(win14days) / sum(last14days),
    PointsMean = mean(T1_Points),
    PointsMedian = median(T1_Points),
    PointsDiffMean = mean(T1_Points - T2_Points),
    FgaMean = mean(T1_fga), 
    FgaMedian = median(T1_fga),
    FgaMin = min(T1_fga), 
    FgaMax = max(T1_fga), 
    AstMean = mean(T1_ast), 
    BlkMean = mean(T1_blk), 
    OppFgaMean = mean(T2_fga), 
    OppFgaMin = min(T2_fga)  
  )

season_summary_X1 = season_summary
season_summary_X2 = season_summary
names(season_summary_X1) = c("Season", "T1", paste0("X1_",names(season_summary_X1)[-c(1,2)]))
names(season_summary_X2) = c("Season", "T2", paste0("X2_",names(season_summary_X2)[-c(1,2)]))


### Combine all features into a data frame

data_matrix =
  tourney %>% 
  left_join(season_summary_X1, by = c("Season", "T1")) %>% 
  left_join(season_summary_X2, by = c("Season", "T2")) %>%
  left_join(select(seeds, Season, T1 = TeamID, Seed1 = Seed), by = c("Season", "T1")) %>% 
  left_join(select(seeds, Season, T2 = TeamID, Seed2 = Seed), by = c("Season", "T2")) %>% 
  mutate(SeedDiff = Seed1 - Seed2) %>%
  left_join(select(quality, Season, T1 = Team_Id, quality_march_T1 = quality), by = c("Season", "T1")) %>%
  left_join(select(quality, Season, T2 = Team_Id, quality_march_T2 = quality), by = c("Season", "T2"))


### Prepare xgboost 
features = setdiff(names(data_matrix), c("Season", "DayNum", "T1", "T2", "T1_Points", "T2_Points", "ResultDiff"))

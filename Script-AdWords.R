library(dplyr)
## Ad Profile data from Fran
AdProfile <- read.table("150706 - adwordsscoreusers", sep ="|", skip=1, 
                        col.names = c("NA", "userToken", "NA", "NA", "NA", "oKeyword", "NA", "Score", "SignDate", "NA"), 
                        stringsAsFactors = FALSE)[,c(2, 6, 8, 9)]
## Clean the profile data removing the trailing spaces in keyword
AdProfile_clean <- mutate(AdProfile, Keyword = as.character(gsub('\\[|\\]|\\"|\\+', "", trimws(oKeyword))))


## Expected revenue data
ExpRev <- read.csv("1year_expected_revenue.csv", stringsAsFactors = FALSE)
ExpRev_clean <- mutate(ExpRev, min_score = as.numeric(substr(credit_band, 2,4)), max_score = as.numeric(substr(credit_band, 6, 8)))
ExpRev_clean[1,4] <- 850

## Mkg Cost data
MkgCost2 <- read.csv("150706 - All export - Keyword - ID - Cost.csv", skip=1, stringsAsFactors = FALSE)
## Clean the keywords of ad metadata removing the [ and "
## "Keyword"     "Impressions" "Clicks"      "Cost"        "Avg..CPC"
## 2 - 8 - 9 - 10 - 12
MkgCost_clean2 <- filter(MkgCost2, !(Keyword %in% " --"))
MkgCost_clean2 <- cbind.data.frame(as.character(gsub('\\[|\\]|\\"|\\+', "", MkgCost2$Keyword)),
                                  select(MkgCost2, c(Keyword, Impressions, Clicks, Cost)), stringsAsFactors = FALSE)
names(MkgCost_clean2) <- c("Keyword", "oKeyword", "Impressions", "Clicks", "Cost")
MkgCost_agg2 <- summarise(group_by(MkgCost_clean2, Keyword), sum_Impressions = sum(Impressions), sum_Clicks = sum(Clicks), sum_Cost = sum(as.numeric(sub(",","",Cost))))


## =============================================
## Join the profile data and MkgCost data via the Keyword to map users to cost of campaigns
AdProfileMkgCost <- inner_join(x = AdProfile_clean, y = MkgCost_agg2, by = "Keyword")

test <- inner_join(x = AdProfile_clean, y = MkgCost_agg, by = "Keyword")

## map the ExpRev to the corresponding users via the score
aFinal <- cbind(AdProfileMkgCost, ExpRev = rep(0, nrow(AdProfileMkgCost)))
for (i in 1:nrow(AdProfileMkgCost)) {
  j = 0
  for (j in 1:5) {
    if (aFinal$Score[i]>= ExpRev_clean$min_score[j] & aFinal$Score[i] <= ExpRev_clean$max_score[j]) {
      aFinal$ExpRev[i] <- ExpRev_clean$expected_revenue[j]
      }
  }
}

## Format the final output with summaries
aFinal_agg <- left_join(
  summarise(group_by(filter(aFinal, Score>=100), Keyword), "Total Impressions" = max(sum_Impressions), "Total Clicks" = max(sum_Clicks), 
            "Total Cost" = max(sum_Cost), CPC = max(sum_Cost)/max(sum_Clicks), 
            "Number Registered" = n(), "Registration to Click Rate" = n()/max(sum_Clicks), "Total ExpRev" = sum(ExpRev), "Avg ExpRev" = mean(ExpRev), ROI = sum(ExpRev)/max(sum_Cost),
            "Min Score" = min(Score), "Max Score" = max(Score), "Avg Score" = mean(Score), "Median Score" = median(Score)),
  summarise(group_by(filter(aFinal, Score < 100), Keyword), "Num Incomplete Registrations" = n()),
  by = "Keyword"
)
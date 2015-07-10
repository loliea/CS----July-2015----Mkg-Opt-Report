library(dplyr)

## -----1) Profile data with AdWords
## 1.1) Download the Ad Profile data from Fran
AdProfile <- read.csv("150706 - adwordsscoreusers.csv", header = TRUE, stringsAsFactors = FALSE,
                      col.names = c("userToken", NA, NA, "Match.type", "Keyword", NA, "Score", "SignDate"))[,c(1,4,5,7,8)]
AdProfile_clean <- AdProfile
## ---- With older version of the file
# AdProfile <- read.table("AdProfile.csv", sep ="|", skip=1, 
#                        col.names = c("NA", "userToken", "NA", "NA", "NA", "oKeyword", "NA", "Score", "SignDate", "NA"), 
#                        stringsAsFactors = FALSE)[,c(2, 6, 8, 9)]
## Clean the profile data removing the trailing spaces in keyword
# AdProfile_clean <- mutate(AdProfile, Keyword = as.character(gsub('\\[|\\]|\\"|\\+', "", trimws(oKeyword))))

## 1.2) Change the description of Match.type to be identical to what is in AdWords
# e -> Exact, p -> Phrase, b >- Broad
AdProfile_temp_e <- filter(AdProfile_clean, Match.type %in% "e")
AdProfile_temp_e$Match.type <- "Exact"
AdProfile_temp_p <- filter(AdProfile_clean, Match.type %in% "p")
AdProfile_temp_p$Match.type <- "Phrase"
AdProfile_temp_b <- filter(AdProfile_clean, Match.type %in% "b")
AdProfile_temp_b$Match.type <- "Broad"
AdProfile_clean <- bind_rows(AdProfile_temp_e, AdProfile_temp_p, AdProfile_temp_b)


## -----2) Unpload Warren file about expected revenue data
ExpRev <- read.csv("1year_expected_revenue.csv", stringsAsFactors = FALSE)
ExpRev_clean <- mutate(ExpRev, min_score = as.numeric(substr(credit_band, 2,4)), max_score = as.numeric(substr(credit_band, 6, 8)))
ExpRev_clean[1,4] <- 850


## -----3) Data coming from AdWords with cost of campaigns
## 3.1) Upload the dataset from AdWords (report named "All export - Keyword - ID - Cost") "150706 - All export - Keyword - ID - Cost.csv"
MkgCost2 <- read.csv("150706 - All export - Keyword - ID - Cost - no filter.csv", skip=1, stringsAsFactors = FALSE)
## Remove the total lines
MkgCost_clean2 <- filter(MkgCost2, !(Keyword %in% " --"))
## Just keep these columns
## "Keyword"     "Impressions" "Clicks"      "Cost"
## 2 - 8 - 9 - 10 - 12
MkgCost_clean2 <- select(MkgCost_clean2, c(Keyword, Match.type, Impressions, Clicks, Cost))
## Clean the keyword column removing the [, ] and " (this is to match how the keyword is formatted in AProfile)
MkgCost_clean2 <- cbind.data.frame(as.character(gsub('\\[|\\]|\\"', "", MkgCost_clean2$Keyword)),
                                   select(MkgCost_clean2, c(Keyword, Match.type, Impressions, Clicks, Cost)), stringsAsFactors = FALSE)
# Rename the columns
names(MkgCost_clean2) <- c("Keyword", "oKeyword", "Match.type", "Impressions", "Clicks", "Cost")
# Aggregate by Keyword and Match.type
MkgCost_agg2 <- summarise(group_by(MkgCost_clean2, Keyword, Match.type), sum_Impressions = sum(Impressions), sum_Clicks = sum(Clicks), 
                          sum_Cost = sum(as.numeric(sub(",","",Cost))))


## =============================================
## TEST
#table(AdProfile_clean$Keyword, AdProfile_clean$Match.type)
#table(MkgCost_agg2$Keyword, MkgCost_agg2$Match.type)
## END TEST

# TEST to see if we keep all Keyword/Match.type combinations
#test_profile <- unique(select(AdProfile_clean, Keyword, Match.type))
#test_mkg <- unique(select(MkgCost_agg2, Keyword, Match.type))
#test <- inner_join(x=test_profile, y=test_mkg, by = c("Keyword", "Match.type"))
# End TEST

## Join the profile data and MkgCost data via the Keyword to map users to cost of campaigns
AdProfileMkgCost <- inner_join(x = AdProfile_clean, y = MkgCost_agg2, by = c("Keyword", "Match.type"))


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
## Note that the AdWords metrics (impression, clicks, cost) are not sumable because we are directly getting from Google the total impression, click and cost
aFinal_agg <- full_join(
  summarise(group_by(filter(aFinal, Score>=100), Keyword, Match.type), "Total Impressions" = max(sum_Impressions), "Total Clicks" = max(sum_Clicks), 
            "Total Cost" = max(sum_Cost), CPC = max(sum_Cost)/max(sum_Clicks), "Number Registered" = n(), "Click to Registration Rate" = n()/max(sum_Clicks), 
            "Total ExpRev" = sum(ExpRev), "Avg ExpRev" = mean(ExpRev), ROI = sum(ExpRev)/max(sum_Cost),
            "Min Score" = min(Score), "Max Score" = max(Score), "Avg Score" = mean(Score),"Median Score" = median(as.numeric(Score))),
  summarise(group_by(filter(aFinal, Score < 100), Keyword, Match.type), "Num Incomplete Registrations" = n()),
  by = c("Keyword", "Match.type")
)

aFinal_final <- select(aFinal_agg,c(1,2,3,4,5,6,7,8,9,10,11,16,12,13,14,15))
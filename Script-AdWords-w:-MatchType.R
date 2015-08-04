library(dplyr)

## -----1) Profile data with AdWords
## 1.1) Download the Ad Profile data from Fran == 150727 - marketing-attribution-adwords-2015-07-27
View(read.csv("150727 - marketing-attribution-adwords-2015-07-27.csv", header=TRUE, stringsAsFactors = FALSE))
AdProfile <- read.csv("150727 - marketing-attribution-adwords-2015-07-27.csv", header = TRUE, stringsAsFactors = FALSE,
                      col.names = c("userToken", NA, "campaignid", "adGroupID", "Match.type", "Keyword", NA, "Score", "SignDate"))[,c(1,3,4,5,6,8,9)]
# col.names = c("userToken", "reportingDate", "campaignid", "adGroupID", "Match.type", "Keyword", NA, "Score", NA))[,c(1,2,3,4,5,6,8)]
#col.names = c("userToken", NA, NA, "Match.type", "Keyword", NA, "Score", "SignDate"))[,c(1,4,5,7,8)]
AdProfile2 <- distinct(AdProfile)  ## Removes the duplicate rows

# TEST
length(unique(AdProfile2$userToken))  #Unique userToken
length(unique(paste(AdProfile2$userToken, AdProfile2$Score))) #Unique combination userToken and score -- if different than above then a user can have two scores
test <- summarise(group_by(AdProfile2, userToken, Score), nb = n()) # cnt userToken & score
nrow(AdProfile2[test[,"nb"]>1, ])
nrow(AdProfile2[test[,"Score"] == 0, ])
AdProfile2[AdProfile2$userToken== '0006ba82-60c5-4db3-ab91-adc86c0fd802',]
# END TEST
# if a user has two scores then use the avg of the socre !!! to the one that have a score of 0
AdProfile_clean <- summarise(group_by(AdProfile2, userToken, SignDate, Match.type, Keyword), Score = mean(Score)) ## avg the score of users that show twice

## test cnt users for some keywords
uToken_keyword <- filter(AdProfile_clean, Keyword %in% "free credit score")
table(uToken_keyword$Keyword, uToken_keyword$Match.type)

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
MkgCost2 <- read.csv("150727 - All export - Keyword - ID - Cost - No filter.csv", skip=1, stringsAsFactors = FALSE)
## Remove the total lines
MkgCost2.1 <- filter(MkgCost2, !(Keyword %in% " --"))
## Just keep these columns
## "Keyword"     "Max..CPC"  "Impressions" "Clicks"      "Cost"    "Avg.CPC"
## 2 - 8 - 9 - 10 - 12
MkgCost2.2 <- select(MkgCost2.1, c(Keyword, Match.type, Max..CPC, Avg..CPC, Impressions, Clicks, Cost))
## Clean the keyword column removing the [, ] and " (this is to match how the keyword is formatted in AProfile)
MkgCost2.3 <- cbind.data.frame(tolower(as.character(gsub('\\[|\\]|\\"', "", MkgCost2.2$Keyword))),
                                   select(MkgCost2.2, c(Keyword, Match.type, Max..CPC, Avg..CPC, Impressions, Clicks, Cost)), stringsAsFactors = FALSE)
##remove from Max..CPC any numeric entries and convert to number
MkgCost2.3 <- transform(MkgCost2.3, Max..CPC = as.numeric(gsub("(^[a-z]*: )?", "", Max..CPC)))

# If Keyword.max.CPC is missing then replace it with Avg CPC
MkgCost2.3[is.na(MkgCost2.3$Max..CPC),"Max..CPC"] <- MkgCost2.3[is.na(MkgCost2.3$Max..CPC),"Avg..CPC"]

# Rename the columns
names(MkgCost2.3) <- c("Keyword", "oKeyword", "Match.type", "Max.CPC", "Avg.CPC", "Impressions", "Clicks", "Cost")
# Aggregate by Keyword and Match.type
MkgCost_agg2 <- summarise(group_by(MkgCost2.3, Keyword, Match.type), Max_CPC = as.numeric(max(Max.CPC, na.rm = TRUE)), Avg_CPC = as.numeric(max(Avg.CPC, na.rm = TRUE)), sum_Impressions = sum(Impressions, na.rm = TRUE),
                          sum_Clicks = sum(Clicks, na.rm = TRUE), sum_Cost = sum(as.numeric(sub(",","",Cost)), na.rm = TRUE))

## test why NA in final results
filter(MkgCost_agg2, Keyword %in% "credit karma")

## =============================================
## TEST
#table(AdProfile_clean$Keyword, AdProfile_clean$Match.type)
#table(MkgCost_agg2$Keyword, MkgCost_agg2$Match.type)
## END TEST

# TEST to see if we keep all Keyword/Match.type combinations
test_profile <- unique(select(AdProfile_clean, Keyword, Match.type))
test_mkg <- unique(select(MkgCost_agg2, Keyword, Match.type))
test <- inner_join(x=test_profile, y=test_mkg, by = c("Keyword", "Match.type"))
nrow(test)
nrow(test_profile) ## Should match the nrow above
#if doesn't match then get in test2 the one missing after inner join
test <- cbind(test, testcol = (rep(1, nrow(test))))
test2 <- full_join(x=test, y=test_profile, by = c("Keyword", "Match.type"))
View(filter(test2, is.na(testcol)))
#-------------------
View(MkgCost2.3[grepl(pattern = "credit karma", x = MkgCost2.3),])
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

## Remove the + from the keyword
aFinal <- transform(aFinal, Keyword = as.character(gsub("\\+", "", Keyword)))

## Format the final output with summaries
## Note that the AdWords metrics (impression, clicks, cost) are not sumable because we are directly getting from Google the total impression, click and cost
aFinal_all <- summarise(group_by(aFinal, Keyword, Match.type), "Total.Impressions" = max(sum_Impressions), "Total.Clicks" = max(sum_Clicks), "Total.Cost" = max(sum_Cost), "Max.CPC" = max(Max_CPC), "Avg.CPC" = max(Avg_CPC))
aFinal_reg <- summarise(group_by(filter(aFinal, Score>=100), Keyword, Match.type), "Number.Registered" = n(), "Total.ExpRev" = sum(ExpRev), "Avg.ExpRev" = mean(ExpRev))
aFinal_nonReg <- summarise(group_by(filter(aFinal, Score < 100), Keyword, Match.type), "Num.Incomplete.Registrations" = n())

aFinal_1 <- full_join(aFinal_all, aFinal_reg, by = c("Keyword", "Match.type"))
aFinal_2 <- full_join(aFinal_1, aFinal_nonReg, by = c("Keyword", "Match.type"))
aFinal_3 <- mutate(aFinal_2, "Click.to.Registration Rate" = (Num.Incomplete.Registrations)/(Total.Clicks), ROI = (Total.ExpRev)/(Total.Cost),
                  "Cost.per.Registered" = Total.Cost/Number.Registered, "ExpRev.per.Registered" = Total.ExpRev/Number.Registered,
                  "Difference.Cost.to.ExpRev" = ((Total.ExpRev - Total.Cost)/Total.Cost), "Suggested.Max.CPC" = Max.CPC*(1+Difference.Cost.to.ExpRev))


write.table(aFinal_3, pipe("pbcopy"), , sep="\t", row.names=FALSE, col.names=TRUE)

aFinal_final <- select(aFinal_agg,c(1,2,3,4,5,6,7,8,9,10,11,16,12,13,14,15))


##
View(summarise(group_by(aFinal, Keyword, Match.type), "Max.CPC" = max(Max_CPC), "Total.Impressions" = max(sum_Impressions), "Total.Clicks" = max(sum_Clicks), 
          "Total.Cost" = max(sum_Cost), "Number.Registered" = n(), "Click.to.Registration Rate" = n()/max(sum_Clicks), 
          "Total.ExpRev" = sum(ExpRev), "Avg.ExpRev" = mean(ExpRev), ROI = sum(ExpRev)/max(sum_Cost),
          "Cost.per.Registered" = Total.Cost/Number.Registered, "ExpRev.per.Registered" = Total.ExpRev/Number.Registered,
          "Difference.Cost.to.ExpRev" = ((Total.ExpRev - Total.Cost)/Total.Cost)
          #, "Suggested.Max.CPC" = Max.CPC*(1+Difference.Cost.to.ExpRev)
          #            ,"Min Score" = min(Score), "Max Score" = max(Score), "Avg Score" = mean(Score),"Median Score" = median(as.numeric(Score))
))


##Look for users with specific keyword and match.type
filter(MkgCost_agg2, Keyword %in% "free credit score" & Match.type %in% "Broad")
filter(AdProfile_clean, Keyword %in% "free credit score" & Match.type %in% "Broad")$userToken

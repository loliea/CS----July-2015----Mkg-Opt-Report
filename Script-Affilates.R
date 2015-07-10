## -- READ ME --##
## This script creates the Affiliate version of the Marketing Optimization Dashboard
## 1 a) Sources: csv file from Warren about expected revenue
## 1 b) File with affiliate data to date that Sean automaticaly upload to PBWork
## 1 c) List of affiliate with tagging from Matt

##LIBRARIES
library(dplyr)
library(lubridate)
library(data.table)

## -- 1) Load data from files
## a) Load the data from Warren files about exp revenue
## Expected revenue data without breackdown incentive vs. nonincentive
ExpRev <- read.csv("1year_expected_revenue.csv", stringsAsFactors = FALSE)
ExpRev_clean <- mutate(ExpRev, min_score = as.numeric(substr(credit_band, 2,4)), 
                       max_score = as.numeric(substr(credit_band, 6, 8)))
ExpRev_clean[1,4] <- 850
## Expected revenue data with breackdown incentive vs. nonincentive
ExpRev2 <- read.csv("mktincent.csv", stringsAsFactors = FALSE)
ExpRev2_clean <- mutate(ExpRev2, min_score = as.numeric(substr(cs_band, 2,4)), 
                       max_score = as.numeric(substr(cs_band, 6, 9)))
ExpRev2_clean[1,5] <- 850
ExpRev2_clean[5,4] <- 100
ExpRev2_clean[5,5] <- 499



## b) Load the affiliate data -- https://creditsesame-affiliates.pbworks.com/w/browse/#view=ViewFolderNewGui&param=2015-06
## Can ether load full week with two month across i]
## Or full month to date ii]
# ---- i] Loading end of June and beginning July (week starting 29th to 6th)
#affiliate_raw1 <- read.csv("150630 - affiliate2.06302015.csv", stringsAsFactors = FALSE)
#affiliate_raw1 <- select(affiliate_raw1, c(2, 4, 5, 8, 12, 15, 20))
#affiliate_raw1 <- transform(affiliate_raw1, Date = mdy_hms(Date))
#affiliate_raw1 <- filter(affiliate_raw1, Date > mdy_hms("06/28/15 23:59:59 UTC"))  #Select the last two days of June
#affiliate_raw2 <- read.csv("150706 - affiliate2.07062015.csv", stringsAsFactors = FALSE)
#affiliate_raw2 <- select(affiliate_raw2, c(2, 4, 5, 8, 12, 15, 20))
#affiliate_raw2 <- transform(affiliate_raw2, Date = mdy_hms(Date))
#affiliate <- rbind(affiliate_raw1, affiliate_raw2)
# ---- ii] Loading month to date
affiliate_raw <- read.csv("150710 - affiliate2.07092015.csv", stringsAsFactors = FALSE)
#select only "Date", "Partner.Name", "Partner.SubID", "Click", "General.Sale..credit.report", "Avg.CreditScoreCustomer.ID"
affiliate <- select(affiliate_raw, c(2, 4, 5, 8, 12, 15, 20))
## Transform date field in date time datatype
affiliate <- transform(affiliate, Date = mdy_hms(Date))
## Set score to 0 when it is NA
affiliate$Avg.CreditScore[is.na(affiliate$Avg.CreditScore)] <- 0

## c) Add the type acquisition (incent nonincent), importance, Partner.Name_clean
## List the list of affiliate -- from Matt with classification incentive vs. non incentive
listAffliliates_pruned <- read.csv("listAffiliates_pruned.csv", stringsAsFactors = FALSE)[1:4]

## Test to see if there are new affilaite that haven't been classified in Matt's list
affiliate_list_test <- full_join(unique(select(affiliate, Partner.Name)), listAffliliates_pruned, by = "Partner.Name")
View(affiliate_list_test$Partner.Name[is.na(affiliate_list_test$Partner.Name_clean)])

#Join the affiliate data with the list of affiliate from Matt
affiliate_list <- inner_join(affiliate, listAffliliates_pruned, by = "Partner.Name")


## ==================
## --- 2) map the ExpRev to the corresponding users via the score
affiliate_Score <- bind_cols(affiliate_list, data.frame(ExpRev = rep(0, nrow(affiliate_list))), 
                             data.frame(ExpRev2 = rep(0, nrow(affiliate_list))))
## Avg.CreditScore (6)
## IncentType (9)
#scoreRanges <- c(100, 499, 500, 574, 575, 674, 675, 749, 750, 850)
#incent <- c("incent", "nonincent")

affiliate_Score_5i <- filter(affiliate_Score, Avg.CreditScore >= 100 & Avg.CreditScore <=499 & IncentType %in% "incent")
affiliate_Score_5n <- filter(affiliate_Score, Avg.CreditScore >= 100 & Avg.CreditScore <=499 & IncentType %in% "nonincent")
affiliate_Score_4i <- filter(affiliate_Score, Avg.CreditScore >= 500 & Avg.CreditScore <=574 & IncentType %in% "incent")
affiliate_Score_4n <- filter(affiliate_Score, Avg.CreditScore >= 500 & Avg.CreditScore <=574 & IncentType %in% "nonincent")
affiliate_Score_3i <- filter(affiliate_Score, Avg.CreditScore >= 575 & Avg.CreditScore <=674 & IncentType %in% "incent")
affiliate_Score_3n <- filter(affiliate_Score, Avg.CreditScore >= 575 & Avg.CreditScore <=674 & IncentType %in% "nonincent")
affiliate_Score_2i <- filter(affiliate_Score, Avg.CreditScore >= 675 & Avg.CreditScore <=749 & IncentType %in% "incent")
affiliate_Score_2n <- filter(affiliate_Score, Avg.CreditScore >= 675 & Avg.CreditScore <=749 & IncentType %in% "nonincent")
affiliate_Score_1i <- filter(affiliate_Score, Avg.CreditScore >= 750 & Avg.CreditScore <=850 & IncentType %in% "incent")
affiliate_Score_1n <- filter(affiliate_Score, Avg.CreditScore >= 750 & Avg.CreditScore <=850 & IncentType %in% "nonincent")
#affiliate_Score_0i <- filter(affiliate_Score, Avg.CreditScore <100 & IncentType %in% "incent")
#affiliate_Score_0n <- filter(affiliate_Score, Avg.CreditScore <100 & IncentType %in% "nonincent")
affiliate_Score_0 <- filter(affiliate_Score, Avg.CreditScore <100 | !(IncentType %in% c("incent", "nonincent")))

affiliate_Score_1i$ExpRev <- ExpRev_clean$expected_revenue[1]
affiliate_Score_1i$ExpRev2 <- ExpRev2_clean$incent[1]
affiliate_Score_1n$ExpRev <- ExpRev_clean$expected_revenue[1]
affiliate_Score_1n$ExpRev2 <- ExpRev2_clean$nonincent[1]

affiliate_Score_2i$ExpRev <- ExpRev_clean$expected_revenue[2]
affiliate_Score_2i$ExpRev2 <- ExpRev2_clean$incent[2]
affiliate_Score_2n$ExpRev <- ExpRev_clean$expected_revenue[2]
affiliate_Score_2n$ExpRev2 <- ExpRev2_clean$nonincent[2]

affiliate_Score_3i$ExpRev <- ExpRev_clean$expected_revenue[3]
affiliate_Score_3i$ExpRev2 <- ExpRev2_clean$incent[3]
affiliate_Score_3n$ExpRev <- ExpRev_clean$expected_revenue[3]
affiliate_Score_3n$ExpRev2 <- ExpRev2_clean$nonincent[3]

affiliate_Score_4i$ExpRev <- ExpRev_clean$expected_revenue[4]
affiliate_Score_4i$ExpRev2 <- ExpRev2_clean$incent[4]
affiliate_Score_4n$ExpRev <- ExpRev_clean$expected_revenue[4]
affiliate_Score_4n$ExpRev2 <- ExpRev2_clean$nonincent[4]

affiliate_Score_5i$ExpRev <- ExpRev_clean$expected_revenue[5]
affiliate_Score_5i$ExpRev2 <- ExpRev2_clean$incent[5]
affiliate_Score_5n$ExpRev <- ExpRev_clean$expected_revenue[5]
affiliate_Score_5n$ExpRev2 <- ExpRev2_clean$nonincent[5]

affiliate_Score2 <- bind_rows(affiliate_Score_1i, affiliate_Score_2i, affiliate_Score_3i, affiliate_Score_4i, affiliate_Score_5i,
                             affiliate_Score_1n, affiliate_Score_2n, affiliate_Score_3n, affiliate_Score_4n, affiliate_Score_5n,
                             affiliate_Score_0)
rm(list = ls(pattern ="affiliate_Score_[0-9]*")) #Clean the temporary datasets created above

##--- TEST
table(affiliate_list$IncentType) # cnt breakdown of incent type
table(affiliate_Score2$IncentType) # should match number above
table(affiliate_Score2$ExpRev, affiliate_Score$IncentType)
table(affiliate_Score2$ExpRev2, affiliate_Score$IncentType)
## END TEST

## Not needed anymore using the butcher approach above
#for (i in 1:nrow(affiliate_Score)) {
#  j = 0
#  for (j in 1:5) {
#    if (affiliate_Score$Avg.CreditScore[i]>= ExpRev_clean$min_score[j] 
#        & affiliate_Score$Avg.CreditScore[i] <= ExpRev_clean$max_score[j]) {
#      affiliate_Score$ExpRev[i] <- ExpRev_clean$expected_revenue[j]
#      }
#  }
#}

## TEST -----------
#names(affiliate_Score2)
#[1] "Date"                         "Partner.Name"                 "Partner.SubID"               
#[4] "Click"                        "General.Sale..credit.report." "Avg.CreditScore"             
#[7] "Customer.ID"                  "Partner.Name_clean"           "IncentType"                  
#[10] "Importance"                   "ExpRev"                       "ExpRev2"
length(affiliate_Score2$Avg.CreditScore[affiliate_Score2$Avg.CreditScore >=100 & affiliate_Score2$IncentType %in% c("incent", "nonincent")])  ## number of rows that have score >= 100
length(affiliate_Score2$ExpRev[affiliate_Score2$ExpRev > 0]) ## number of input where ExpRev has been changed (>0) -- Should match the number above
table(affiliate_Score2$ExpRev) ## First number should be for ExpRev 0
nrow(affiliate_Score2[affiliate_Score2$Avg.CreditScore <100 | !(affiliate_Score2$IncentType %in% c("incent", "nonincent")), 6]) ## Should match the number above in table with 0
## Get some stats about the output by partner
ftable(table(affiliate_Score2$Partner.Name, affiliate_Score$ExpRev))
## END TEST


## ==================
## 3) Associate the cost of different types of campaigns
## Before 6/12/15 incent and nonincent = $7
## After incent = 2 and nonincent = $8
affiliate_Score3 <- cbind(affiliate_Score2, Cost = rep(0, nrow(affiliate_Score2)))

affiliate_Score3_0 <- filter(affiliate_Score3, (General.Sale..credit.report. == 0) & IncentType %in% c("incent", "nonincent"))
affiliate_Score3_1_b4 <- filter(affiliate_Score3, General.Sale..credit.report. == 1 & Date <= mdy("06/11/2015")
                              & IncentType %in% c("incent", "nonincent"))
affiliate_Score3_1_now_i <- filter(affiliate_Score3, General.Sale..credit.report. == 1 & Date > mdy("06/11/2015")
                                  & IncentType == "incent")
affiliate_Score3_1_now_n <- filter(affiliate_Score3, General.Sale..credit.report. == 1 & Date > mdy("06/11/2015")
                                  & IncentType == "nonincent")
affiliate_Score3_00 <- filter(affiliate_Score3, !(IncentType %in% c("incent", "nonincent")))

affiliate_Score3_1_b4$Cost <- 7
affiliate_Score3_1_now_i$Cost <- 2
affiliate_Score3_1_now_n$Cost <- 8
affiliate_Score3_0$Cost <- 0
affiliate_Score3_00$Cost <- 0

## TEST
nrow(affiliate_Score2)
nrow(affiliate_Score3)
nrow(affiliate_Score3_1_b4) + nrow(affiliate_Score3_1_now_n) + nrow(affiliate_Score3_1_now_i) + nrow(affiliate_Score3_0) + nrow(affiliate_Score3_00)

## Bind the individual datasets including the one that does not contain incent and nonincent
affiliateScore4 <- bind_rows(affiliate_Score3_1_b4, affiliate_Score3_1_now_i, affiliate_Score3_1_now_n, affiliate_Score3_0, affiliate_Score3_00)
rm(list = ls(pattern ="affiliate_Score3_[0-9]*"))

## ==================
## 4) Final wrangling getting all the data together
## ---------------------All affiliates
## ALL By Parnter.Name Total Clicks = sum(Click), sum(Cost)
aFinal_all <- summarise(group_by(affiliateScore4, Partner.Name_clean, IncentType), "Total Clicks" = sum(Click, na.rm = TRUE), "Total Cost" = sum(Cost, na.rm = TRUE))
## ONLY W/ SCORE    Number_Registered =  n(), sum(ExpRev), sum(ExpRev2)
aFinal_reg <- summarise(group_by(filter(affiliateScore4, General.Sale..credit.report. == 1), Partner.Name_clean, IncentType), 
                        "Number Registered" = n(), "Total ExpRev" = sum(ExpRev, na.rm = TRUE), "Total ExpRev2" = sum(ExpRev2, na.rm = TRUE))
## ONLY NO SCORE    Number_Incomplete_Registration = n()
aFinal_nonReg <- summarise(group_by(filter(affiliateScore4, General.Sale..credit.report. == 0 ), Partner.Name_clean, IncentType), 
                           "Number Incomplete Registration" = n())


## Stats about the score
aFinal_reg_score <- summarise(group_by(filter(affiliateScore4, General.Sale..credit.report. == 1 & Avg.CreditScore >=100), Partner.Name_clean, IncentType),
                              "Min Score" = min(Avg.CreditScore), "Max Score" = max(Avg.CreditScore),	"Avg Score" = mean(Avg.CreditScore), "Median Score" = median(Avg.CreditScore))

## Putting it all together
aFinal_all_reg <- left_join(aFinal_all, aFinal_reg, by = c("Partner.Name_clean", "IncentType"))
aFinal_all_reg_nonReg <- left_join(aFinal_all_reg, aFinal_nonReg, by = c("Partner.Name_clean", "IncentType"))
aFinal_final_all <- left_join(aFinal_all_reg_nonReg, aFinal_reg_score, by = c("Partner.Name_clean", "IncentType"))

## add more metrics and reorganize the columns
aFinal_final_all <- mutate(aFinal_final_all, "Avg CPA" = `Total Cost`/`Number Registered`,
                          "Click to Registration Rate" = `Total Clicks`/`Number Registered`,
                          "Avg ExpRev by Registered" = `Total ExpRev`/`Number Registered`,
                          "Avg ExpRev by Registered2" = `Total ExpRev2`/`Number Registered`,
                          "Exp ROI" = `Total ExpRev`/`Total Cost`,
                          "Exp ROI2" = `Total ExpRev2`/`Total Cost`,
                          "Ratio Incomplete Registration vs. Complete Registration (smaller is better)" = `Number Incomplete Registration`/`Number Registered`)
zFinal <- aFinal_final_all[,c(1,2,3,4,13,5,14,6,15,16,7,18,19,8,17,9,10,11,12)]

View(arrange(as.data.frame(filter(zFinal, IncentType %in% c("incent", "nonincent"))), IncentType, desc(`Number Registered`)))


## ==================
## 5) Doing the output by subID for maxbounty and nameoffers
## --------------------- by subID
## Details by SubID for evoleads, maxb and namoffers
aFinal_subID <- filter(affiliateScore4, Partner.Name_clean %in% c("maxb", "namoffers"))

##---table(filter(aFinal_subID, Partner.Name_clean == "evoleads")$Partner.SubID, filter(aFinal_subID, Partner.Name_clean == "evoleads")$Click)

## ALL By Parnter.Name SubID Total Clicks = sum(Click), sum(Cost)
aFinal_subID_all <- summarise(group_by(aFinal_subID, Partner.Name_clean, Partner.SubID, IncentType), "Total.Clicks" = sum(Click, na.rm = TRUE), 
                              "Total.Cost" = sum(Cost, na.rm = TRUE))
## ONLY W/ SCORE    Number_Registered =  n(), sum(ExpRev)
aFinal_subID_reg <- summarise(group_by(filter(aFinal_subID, General.Sale..credit.report. == 1), Partner.Name_clean, Partner.SubID, IncentType), 
                              "Number.Registered" = n(), "Total.ExpRev" = sum(ExpRev, na.rm = TRUE))
## ONLY NO SCORE    Number_Incomplete_Registration = n()
aFinal_subID_nonReg <- summarise(group_by(filter(aFinal_subID, General.Sale..credit.report. == 0), Partner.Name_clean, Partner.SubID, IncentType), 
                                 "Number.Incomplete.Registration" = n())
## to get score
##aFinal_subID_reg_score <- summarise(group_by(filter(aFinal_subID, General.Sale..credit.report. == 1 & Avg.CreditScore >=100), Partner.Name_clean, Partner.SubID, IncentType),
##                          "Min Score" = min(Avg.CreditScore), "Max Score" = max(Avg.CreditScore),	"Avg Score" = mean(Avg.CreditScore), "Median Score" = median(Avg.CreditScore))
                        
## Putting it all together
aFinal_subID_all_reg <- inner_join(aFinal_subID_all, aFinal_subID_reg, by = c("Partner.Name_clean", "Partner.SubID", "IncentType"))
aFinal_subID_all_reg <- left_join(aFinal_subID_all, aFinal_subID_reg, by = c("Partner.Name_clean", "Partner.SubID", "IncentType"))
aFinal_subID_all_reg_nonReg <- left_join(aFinal_subID_all_reg, aFinal_subID_nonReg, by = c("Partner.Name_clean", "Partner.SubID", "IncentType"))
##aFinal_subID_final_all <- left_join(aFinal_subID_all_reg_nonReg, aFinal_subID_reg_score, by = c("Partner.Name_clean", "Partner.SubID", "IncentType"))

## Rename the SubID of evoleads that have just one click to subID.agg (that way they will be summarize into one subID later on) -- nrow(aFinal_subID_all_reg_nonReg)
aFinal_subID_all_reg_nonReg$Partner.SubID <- ifelse((aFinal_subID_all_reg_nonReg$Total.Clicks < 3) & (aFinal_subID_all_reg_nonReg$Partner.Name_clean %in% "evoleads"), 
                                                    "subID.agg", aFinal_subID_all_reg_nonReg$Partner.SubID)

## ==================
## ----6) Doing the output by subID for evoleads
## Extract the data of evolead
aFinal_evoleads <- filter(affiliateScore4, Partner.Name_clean %in% c("evoleads"))
## Isolate the correct SubID in a new columns named SubIDs
aFinal_evoleads <- transform(aFinal_evoleads, Partner.SubID = substr(aFinal_evoleads$Partner.SubID, regexpr("-", aFinal_evoleads$Partner.SubID)+1, nchar(aFinal_evoleads$Partner.SubID)))
##
aFinal_subID <- aFinal_evoleads
## ALL By Parnter.Name SubID Total Clicks = sum(Click), sum(Cost)
aFinal_subID_all <- summarise(group_by(aFinal_subID, Partner.Name_clean, Partner.SubID, IncentType), "Total.Clicks" = sum(Click, na.rm = TRUE), 
                              "Total.Cost" = sum(Cost, na.rm = TRUE))
## ONLY W/ SCORE    Number_Registered =  n(), sum(ExpRev)
aFinal_subID_reg <- summarise(group_by(filter(aFinal_subID, General.Sale..credit.report. == 1), Partner.Name_clean, Partner.SubID, IncentType), 
                              "Number.Registered" = n(), "Total.ExpRev" = sum(ExpRev, na.rm = TRUE))
## ONLY NO SCORE    Number_Incomplete_Registration = n()
aFinal_subID_nonReg <- summarise(group_by(filter(aFinal_subID, General.Sale..credit.report. == 0), Partner.Name_clean, Partner.SubID, IncentType), 
                                 "Number.Incomplete.Registration" = n())

## Putting it all together
aFinal_subID_all_reg <- left_join(aFinal_subID_all, aFinal_subID_reg, by = c("Partner.Name_clean", "Partner.SubID", "IncentType"))
aFinal_subID_all_reg_nonReg <- left_join(aFinal_subID_all_reg, aFinal_subID_nonReg, by = c("Partner.Name_clean", "Partner.SubID", "IncentType"))

View(aFinal_subID_all_reg_nonReg)
write.table(aFinal_subID_all_reg_nonReg, pipe("pbcopy"), , sep="\t", row.names=FALSE, col.names=TRUE)

## ===================== Study on #click
aFinal_2 <- aFinal_all_reg_nonReg
names(aFinal_2) = make.names(names(aFinal_all_reg_nonReg))
aFinal_2 <- mutate(aFinal_2, Inc.vs.Comp.Reg = Number.Incomplete.Registration/Number.Registered)
boxplot(Inc.vs.Comp.Reg ~ IncentType, data = aFinal_2) ## Big overlap between incent adn non incent - bottom end of nonincent could in fact be incent

maxB <- filter(affiliateScore3, Partner.Name_clean %in% c("maxb", "maxb2"))
## Convert date time to date
maxB <- mutate(maxB, theDate = as.Date(unname(maxB$Date), "%Y-%m-%d"))
# Date, IncentType, General.Sale..credit.report  sum(Click)
maxB_reg <- summarise(group_by(filter(maxB, General.Sale..credit.report. == 1), theDate, IncentType), Cnt_reg = n(), Clicks = sum(Click))
maxB_nonreg <- summarise(group_by(filter(maxB, General.Sale..credit.report. == 0), theDate, IncentType), Cnt_nonreg = n(), Clicks = sum(Click))

maxB_sum <- full_join(maxB_reg, maxB_nonreg, by=c("theDate", "IncentType"))
maxB_sum <- mutate(maxB_sum, nonreg.vs.reg = Cnt_nonreg/Cnt_reg)
maxB_sum_ok <- filter(maxB_sum, !is.na(nonreg.vs.reg))

maxB_sum_ok_inc <- maxB_sum_ok[maxB_sum_ok$IncentType == "incent",c(1,3:7)]
maxB_sum_ok_noninc <- maxB_sum_ok[maxB_sum_ok$IncentType == "nonincent",c(1,3:7)]

plot(x = maxB_sum_ok_noninc$theDate, y = maxB_sum_ok_noninc$nonreg.vs.reg, type = "l")
plot(x = maxB_sum_ok_inc$theDate, y = maxB_sum_ok_inc$nonreg.vs.reg, type = "l")
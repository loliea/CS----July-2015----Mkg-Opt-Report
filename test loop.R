## Test for the intersection between affiliate and score to get the ExpRev

sourceDF <- data.frame(min = c(1,10,20,30,40), max = c(9, 19, 29, 39, 50), rev = 1:5)
testDF <- data.frame(Score = c(245, 552, 633, 655, 733, 844), number = 1:6, ExpRev = rep(0,6))
apply(testDF, 1, function(e) ExpRev_clean[findInterval(e, c(ExpRev_clean[,3], ExpRev_clean[,4])),2])
apply(testDF, 1, function(e) ExpRev_clean[findInterval(e, c(575, 674)),2])



for (i in 1:6) {
  ExpRev_clean[testDF$Score[i] >= ExpRev_clean$min_score & testDF$Score[i] <= ExpRev_clean$max_score, 2]
}


---
  3:39

for (j in 1:5) {
  if (affiliateScore$Avg.CreditScore[1]>= ExpRev_clean$min_score[j] 
      & affiliateScore$Avg.CreditScore[1] <= ExpRev_clean$max_score[j]) {
    ExpRev_clean$expected_revenue[j]
  }
}

for (i in 1:2) {     #nrow(affiliateScore3)
  if (affiliateScore3$Date[i] <= mdy("06/11/2015")) {
    affiliateScore3$Cost[i] = 7
  } else 
    if (affiliateScore3$Date[i] > mdy("06/11/2015") & affiliateScore3$IncentType[i] == "incent") {
      affiliateScore3$Cost[i] = 2
    } else 
      if (affiliateScore3$Date[i] > mdy("06/11/2015") & affiliateScore3$General.Sale..credit.report. == 1 & affiliateScore3$IncentType[i] == "nonincent") {
        affiliateScore3$Cost[i] = 8
      } else affiliateScore3$Cost[i] = 0
}
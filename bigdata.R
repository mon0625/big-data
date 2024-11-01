library(dplyr)
library(kknn)
user = read.csv('user_data.csv')
dp1_exam = read.csv('dp001_exam.csv')
dp1_prac = read.csv('dp001_prac.csv')
dp1_review = read.csv('dp001_review.csv')
dp1_review_p = read.csv('dp001_review_plus.csv')
dp2_exam = read.csv('dp002_exam.csv')
dp3_math = read.csv('dp003_math.csv')
dp3_word = read.csv('dp003_word.csv')

dp1_prac_ch = dp1_prac[grep('國語文', dp1_prac$subject_name), ] # dp1測驗，只有國語資料
dp1_prac_en = dp1_prac[grep('英語', dp1_prac$subject_name), ]   # dp1測驗，只有英語資料
dp1_prac_ma = dp1_prac[grep('數學', dp1_prac$subject_name), ]   # dp1測驗，只有數學資料

## 測驗成績(X1)、大考成績(Y), prac
# dp1 測驗 [國語, 英語, 數學]=[41, 8, 58]
dp1_rate_ch = c() # dp1，國語測驗成績
dp1_rate_en = c() # dp1，英語測驗成績
dp1_rate_ma = c() # dp1，數學測驗成績

dp1_count_ch = c() # dp1，國語測驗次數
dp1_count_en = c() # dp1，英語測驗次數
dp1_count_ma = c() # dp1，數學測驗次數
i = 1
for(p in user$user_sn){
  dp1_rate_ch[i] = mean(dp1_prac_ch[dp1_prac_ch$user_sn==p, 'score_rate'])
  dp1_rate_en[i] = mean(dp1_prac_en[dp1_prac_en$user_sn==p, 'score_rate'])
  dp1_rate_ma[i] = mean(dp1_prac_ma[dp1_prac_ma$user_sn==p, 'score_rate'])
  dp1_count_ch[i] = length(dp1_prac_ch[dp1_prac_ch$user_sn==p, 'score_rate'])
  dp1_count_en[i] = length(dp1_prac_en[dp1_prac_en$user_sn==p, 'score_rate'])
  dp1_count_ma[i] = length(dp1_prac_ma[dp1_prac_ma$user_sn==p, 'score_rate'])
  i = i+1
}

prac1_ch = data.frame(user_sn=user$user_sn, testscore1=dp1_rate_ch, testcount1=dp1_count_ch) %>% na.omit()
prac1_en = data.frame(user_sn=user$user_sn, testscore1=dp1_rate_en, testcount1=dp1_count_en) %>% na.omit()
prac1_ma = data.frame(user_sn=user$user_sn, testscore1=dp1_rate_ma, testcount1=dp1_count_ma) %>% na.omit()

# dp2 測驗 [國語, 英語]=[42, 15]
dp2_rate_ch = c() # dp2，國語測驗成績
dp2_rate_en = c() # dp2，英語測驗成績
i = 1
for(p in user$user_sn){
  tem = as.numeric(as.logical(dp2_exam[dp2_exam$user_sn==p & dp2_exam$object_type=='中文閱讀', 'result_success']))
  dp2_rate_ch[i] = (sum(tem)/length(tem))*100
  tem = as.numeric(as.logical(dp2_exam[dp2_exam$user_sn==p & dp2_exam$object_type=='英文閱讀', 'result_success']))
  dp2_rate_en[i] = (sum(tem)/length(tem))*100
  i = i+1
}

prac2_ch = data.frame(user_sn=user$user_sn, testscore2=dp2_rate_ch) %>% na.omit()
prac2_en = data.frame(user_sn=user$user_sn, testscore2=dp2_rate_en) %>% na.omit()

# dp3 測驗 [英語, 數學]=[12, 24]
dp3_rate_en = c() # dp3，英語測驗成績
dp3_rate_ma = c() # dp3，數學測驗成績
dp3_count_en = c() # dp3，英語測驗次數
dp3_count_ma = c() # dp3，數學測驗次數
i = 1
for(p in user$user_sn){
  tem = as.numeric(as.logical(dp3_word[dp3_word$user_sn==p, 'is_correct']))
  dp3_rate_en[i] = (sum(tem)/length(tem))*100
  dp3_count_en[i] = length(tem)
  tem = as.numeric(as.logical(dp3_math[dp3_math$user_sn==p, 'is_correct']))
  dp3_rate_ma[i] = (sum(tem)/length(tem))*100
  dp3_count_ma[i] = length(tem)
  i = i+1
}

prac3_en = data.frame(user_sn=user$user_sn, testscore3=dp3_rate_en, testcount3=dp3_count_en) %>% na.omit()
prac3_ma = data.frame(user_sn=user$user_sn, testscore3=dp3_rate_ma, testcount3=dp3_count_ma) %>% na.omit()

# 國語 dp1, dp2 在大考的成績
prac1_ch$finalscore = user[user$user_sn %in% prac1_ch$user_sn, 'chinese_score'] ## dp1，有國語測驗的[測驗、大考]成績
prac2_ch$finalscore = user[user$user_sn %in% prac2_ch$user_sn, 'chinese_score'] ## dp2，有國語測驗的[測驗、大考]成績

# 英語 dp1, dp2, dp3 在大考的成績
prac1_en$finalscore = user[user$user_sn %in% prac1_en$user_sn, 'english_score'] ## dp1，有英語測驗的[測驗、大考]成績
prac2_en$finalscore = user[user$user_sn %in% prac2_en$user_sn, 'english_score'] ## dp2，有英語測驗的[測驗、大考]成績
prac3_en$finalscore = user[user$user_sn %in% prac3_en$user_sn, 'english_score'] ## dp3，有英語測驗的[測驗、大考]成績

# 數學 dp1, dp3 在大考的成績
prac1_ma$finalscore = user[user$user_sn %in% prac1_ma$user_sn, 'math_score']    ## dp1，有數學測驗的[測驗、大考]成績
prac3_ma$finalscore = user[user$user_sn %in% prac3_ma$user_sn, 'math_score']    ## dp3，有數學測驗的[測驗、大考]成績

# ============================↓ picture1 (各平台測驗) ↓============================
# dp1 測驗成績箱型圖
png("pic/dp1_prac.png", width = 800, height = 600)
par(mfrow=c(1, 3))
boxplot(prac1_ch$testscore, ylim=c(0, 100), main='平台1國語測驗成績', cex.main=3, cex.axis=2)
boxplot(prac1_en$testscore, ylim=c(0, 100), main='平台1英語測驗成績', cex.main=3, cex.axis=2)
boxplot(prac1_ma$testscore, ylim=c(0, 100), main='平台1數學測驗成績', cex.main=3, cex.axis=2)
dev.off()

# dp2 測驗成績箱型圖
png("pic/dp2_prac.png", width = 800, height = 600)
par(mfrow=c(1, 2))
boxplot(prac2_ch$testscore, ylim=c(0, 100), main='平台2國語測驗成績', cex.main=2.5, cex.axis=2)
boxplot(prac2_en$testscore, ylim=c(0, 100), main='平台2英語測驗成績', cex.main=2.5, cex.axis=2)
dev.off()

# dp3 測驗成績箱型圖
png("pic/dp3_prac.png", width = 800, height = 600)
par(mfrow=c(1, 2))
boxplot(prac3_en$testscore, ylim=c(0, 100), main='平台3英語測驗成績', cex.main=2.5, cex.axis=2)
boxplot(prac3_ma$testscore, ylim=c(0, 100), main='平台3數學測驗成績', cex.main=2.5, cex.axis=2)
dev.off()
# ============================↑ picture1 (各平台測驗) ↑============================

# ============================↓ picture2 (測驗者的大考成績) ↓============================
png("pic/chinese.png", width = 800, height = 600)
par(mfrow=c(1, 3))
boxplot(user$chinese_score, ylim=c(0, 100), main='國語大考', cex.main=3, cex.axis=2)
boxplot(prac1_ch$finalscore, ylim=c(0, 100), main='平台1國語大考', cex.main=3, cex.axis=2)
boxplot(prac2_ch$finalscore, ylim=c(0, 100), main='平台2國語大考', cex.main=3, cex.axis=2)
dev.off()

png("pic/english.png", width = 800, height = 600)
par(mfrow=c(1, 4))
boxplot(user$english_score, ylim=c(0, 100), main='英語大考', cex.main=2.5, cex.axis=2)
boxplot(prac1_en$finalscore, ylim=c(0, 100), main='平台1英語大考', cex.main=2.5, cex.axis=2)
boxplot(prac2_en$finalscore, ylim=c(0, 100), main='平台2英語大考', cex.main=2.5, cex.axis=2)
boxplot(prac3_en$finalscore, ylim=c(0, 100), main='平台3英語大考', cex.main=2.5, cex.axis=2)
dev.off()

png("pic/math.png", width = 800, height = 600)
par(mfrow=c(1, 3))
boxplot(user$math_score, ylim=c(0, 100), main='數學大考', cex.main=3, cex.axis=2)
boxplot(prac1_ma$finalscore, ylim=c(0, 100), main='平台1數學大考', cex.main=3, cex.axis=2)
boxplot(prac3_ma$finalscore, ylim=c(0, 100), main='平台3數學大考', cex.main=3, cex.axis=2)
dev.off()
# ============================↑ picture2 (測驗者的大考成績) ↑============================

## dp1 影片時長(X2)、觀看次數, video
meanlen = function(df){
  result = df %>% group_by(stime) %>% summarise(onedaylen=sum(learningtime))
  return(sum(result$onedaylen)/nrow(result))
}

# 國語 [16]
review_ch = dp1_review[grep('國語文', dp1_review$subject_name), ] # dp1影片，只有國語資料
video = data.frame(table(review_ch$user_sn))                      
colnames(video) = c('user_sn', 'videocounts')
review_ch$learningtime = as.POSIXct(review_ch$end_time)-as.POSIXct(review_ch$start_time)

review_ch$stime = format(as.POSIXlt(review_ch$start_time), "%Y%m%d")
review_ch$etime = format(as.POSIXlt(review_ch$end_time), "%Y%m%d")
review_ch$sameday = apply(review_ch, 1, function(row) row['stime']==row['etime'])
which(!review_ch$sameday)

len = c()
i = 1
for(p in video$user_sn){
  tem = review_ch[review_ch$user_sn==p, ]
  len[i] = meanlen(tem)
  i = i+1
}
video$learningmean = len
df = user[user$user_sn %in% video$user_sn, c('user_sn', 'chinese_score')]
video_ch = merge(video, df, by='user_sn') ## dp1影片，觀看國語影片次數、時長、大考成績

# 英語 [7]
review_en = dp1_review[grep('英語', dp1_review$subject_name), ]
video = data.frame(table(review_en$user_sn))
colnames(video) = c('user_sn', 'videocounts')
review_en$learningtime = as.POSIXct(review_en$end_time)-as.POSIXct(review_en$start_time)

review_en$stime = format(as.POSIXlt(review_en$start_time), "%Y%m%d")
review_en$etime = format(as.POSIXlt(review_en$end_time), "%Y%m%d")
review_en$sameday = apply(review_en, 1, function(row) row['stime']==row['etime'])
which(!review_en$sameday)

len = c()
i = 1
for(p in video$user_sn){
  tem = review_en[review_en$user_sn==p, ]
  len[i] = meanlen(tem)
  i = i+1
}
video$learningmean = len
df = user[user$user_sn %in% video$user_sn, c('user_sn', 'english_score')]
video_en = merge(video, df, by='user_sn') ## dp1影片，觀看英語影片次數、時長、大考成績

# 數學 [57]
review_ma = dp1_review[grep('數學', dp1_review$subject_name), ]
video = data.frame(table(review_ma$user_sn))
colnames(video) = c('user_sn', 'videocounts')
review_ma$learningtime = as.POSIXct(review_ma$end_time)-as.POSIXct(review_ma$start_time)

review_ma$stime = format(as.POSIXlt(review_ma$start_time), "%Y%m%d")
review_ma$etime = format(as.POSIXlt(review_ma$end_time), "%Y%m%d")
review_ma$sameday = apply(review_ma, 1, function(row) row['stime']==row['etime'])
which(!review_ma$sameday)

len = c()
i = 1
for(p in video$user_sn){
  tem = review_ma[review_ma$user_sn==p, ]
  len[i] = meanlen(tem)
  i = i+1
}
video$learningmean = len
df = user[user$user_sn %in% video$user_sn, c('user_sn', 'math_score')]
video_ma = merge(video, df, by='user_sn') ## dp1影片，觀看數學影片次數、時長、大考成績

# ============================↓ picture3 (成績-時長、成績-次數圖) ↓============================
png("pic/chinese_learningTimeCount.png", width = 800, height = 600)
par(mfrow=c(1, 2), mar=c(5.1, 6, 4.1, 2.1))
plot(video_ch$learningmean, video_ch$chinese_score, 
     main='成績-觀看時間', xlab='觀看時間(s)', ylab='國語大考成績',
     cex.main=2.5, cex.lab=2, cex.axis=1.5)
lines(lowess(video_ch$learningmean, video_ch$chinese_score), col='red', lwd=2, lty=1)

plot(video_ch$counts, video_ch$chinese_score,
     main='成績-觀看次數', xlab='觀看次數', ylab='國語大考成績',
     cex.main=2.5, cex.lab=2, cex.axis=1.5)
lines(lowess(video_ch$counts, video_ch$chinese_score), col='red', lwd=2, lty=1)
dev.off()

png("pic/english_learningTimeCount.png", width = 800, height = 600)
par(mfrow=c(1, 2), mar=c(5.1, 6, 4.1, 2.1))
plot(video_en$learningmean, video_en$english_score,
     main='成績-觀看時間', xlab='觀看時間(s)', ylab='英語大考成績',
     cex.main=2.5, cex.lab=2, cex.axis=1.5)
lines(lowess(video_en$learningmean, video_en$english_score), col='red', lwd=2, lty=1)

plot(video_en$counts, video_en$english_score,
     main='成績-觀看次數', xlab='觀看次數', ylab='英語大考成績',
     cex.main=2.5, cex.lab=2, cex.axis=1.5)
lines(lowess(video_en$counts, video_en$english_score, f=.8), col='red', lwd=2, lty=1)
dev.off()

png("pic/math_learningTimeCount.png", width = 800, height = 600)
par(mfrow=c(1, 2), mar=c(5.1, 6, 4.1, 2.1))
plot(video_ma$learningmean, video_ma$math_score,
     main='成績-觀看時間', xlab='觀看時間(s)', ylab='數學大考成績',
     cex.main=2.5, cex.lab=2, cex.axis=1.3)
lines(lowess(video_ma$learningmean, video_ma$math_score), col='red', lwd=2, lty=1)

plot(video_ma$counts, video_ma$math_score,
     main='成績-觀看次數', xlab='觀看次數', ylab='數學大考成績',
     cex.main=2.5, cex.lab=2, cex.axis=1.3)
lines(lowess(video_ma$counts, video_ma$math_score), col='red', lwd=2, lty=1)
dev.off()
# ============================↑ picture3 (成績-時長、成績-次數圖) ↑============================

### ↓ 交集處理 ↓ ### 
## 數學 [dp1_prac, dp1_review, dp3]=[58, 57, 24] ↓============================
#(dp1) 數學 [測驗]且[影片] [57]
df1_ma_PV = merge(prac1_ma, video_ma, by='user_sn', all=FALSE)
df1_ma_PV = df1_ma_PV[ , -c(grep('math_score', colnames(df1_ma_PV)))]
nrow(df1_ma_PV)

# (dp1, dp3) 數學 [dp1測驗且影片]且[dp3] [11]
df1_ma_PV3 = merge(df1_ma_PV, prac3_ma, by='user_sn', all.x=TRUE)
df1_ma_PV3$indp3 = ifelse(df1_ma_PV3$user_sn %in% prac3_ma$user_sn, 1, 0)
sum(df1_ma_PV3$indp3)

# (dp1, dp3) 數學 [dp3]/[dp1測驗且影片] [13]
df3_ma_no1PV = prac3_ma[!prac3_ma$user_sn %in% df1_ma_PV$user_sn, ]
nrow(df3_ma_no1PV)

# (dp1, dp3) 數學 [dp1測驗]且[dp3] [12]
df1_ma_P3 = prac1_ma[prac1_ma$user_sn %in% prac3_ma$user_sn, ]
nrow(df1_ma_P3)

# (dp1, dp3) 數學 [dp1影片]且[dp3] [11]
df1_ma_V3 = video_ma[video_ma$user_sn %in% prac3_ma$user_sn, ]
nrow(df1_ma_V3)

## 國語 [dp1_prac, dp1_review, dp2]=[41, 16, 42] ↓============================
# (dp1) 國語 [測驗且影片] [10]
df1_ch_PV = merge(prac1_ch, video_ch, by='user_sn', all=FALSE)
df1_ch_PV = df1_ch_PV[ , -c(grep('chinese_score', colnames(df1_ch_PV)))]
nrow(df1_ch_PV)

# (dp1, dp2) 國語 [dp1測驗且影片]且[dp2] [1]
df1_ch_PV2 = merge(df1_ch_PV, prac2_ch, by='user_sn', all.x=TRUE)
df1_ch_PV2$indp2 = ifelse(df1_ch_PV$user_sn %in% prac2_ch$user_sn, 1, 0)
sum(df1_ch_PV2$indp2)

# (dp1, dp2) 國語 [dp2]/[dp1測驗且影片] [12]
df2_ch_no1PV = prac2_ch[!prac2_ch$user_sn %in% df1_ch_PV$user_sn, ]
nrow(df2_ch_no1PV)

# (dp1, dp2) 國語 [dp1測驗]且[dp2] [28]
df1_ch_P2 = prac1_ch[prac1_ch$user_sn %in% prac2_ch$user_sn, ]
nrow(df1_ch_P2)

# (dp1, dp2) 國語 [dp1影片]且[dp2] [3]
df1_ch_V2 = video_ch[video_ch$user_sn %in% prac2_ch$user_sn, ]
nrow(df1_ch_V2)

## 英語 [dp1_prac, dp1_review, dp2, dp3]=[8, 7, 15, 12] ↓============================
# (dp1) 英語 [測驗且影片] [5]
df1_en_PV = merge(prac1_en, video_en, by='user_sn', all=FALSE)
df1_en_PV = df1_en_PV[ , -c(grep('english_score', colnames(df1_en_PV)))]
nrow(df1_en_PV)

# ---------------------------↓ dp1, dp2 ↓---------------------------
# (dp1, dp2) 英語 [dp1測驗且影片]且[dp2] [0]
df1_en_PV2 = merge(df1_en_PV, prac2_en, by='user_sn', all.x=TRUE)
df1_en_PV2$indp2 = ifelse(df1_en_PV$user_sn %in% prac2_en$user_sn, 1, 0)
sum(df1_en_PV2$indp2)

# (dp1, dp2) 英語 [dp2]/[dp1測驗且影片] [15]
df2_en_no1PV = prac2_en[!prac2_en$user_sn %in% df1_en_PV$user_sn, ]
nrow(df2_en_no1PV)

# (dp1, dp2) 英語 [dp1測驗]且[dp2] [1]
prac1_en$indp2 = ifelse(prac1_en$user_sn %in% prac2_en$user_sn, 1, 0)
sum(prac1_en$indp2)

# (dp1, dp2) 英語 [dp1影片]且[dp2] [1]
video_en$indp2 = ifelse(video_en$user_sn %in% prac2_en$user_sn, 1, 0)
sum(video_en$indp2)
# ---------------------------↑ dp1, dp2 ↑---------------------------

# ---------------------------↓ dp1, dp3 ↓---------------------------
# (dp1, dp3) 英語 [dp1測驗且影片]且[dp3] [3]
df1_en_PV3 = merge(df1_en_PV, prac3_en, by='user_sn', all.x=TRUE)
df1_en_PV3$indp3 = ifelse(df1_en_PV$user_sn %in% prac3_en$user_sn, 1, 0)
sum(df1_en_PV3$indp3)

# (dp1, dp3) 英語 [dp3]/[dp1測驗且影片] [9]
df3_en_no1PV = prac3_en[!prac3_en$user_sn %in% df1_en_PV$user_sn, ]
nrow(df3_en_no1PV)

# (dp1, dp3) 英語 [dp1測驗]且[dp3] [4]
prac1_en$indp3 = ifelse(prac1_en$user_sn %in% prac3_en$user_sn, 1, 0)
sum(prac1_en$indp3)

# (dp1, dp3) 英語 [dp1影片]且[dp3] [3]
video_en$indp3 = ifelse(video_en$user_sn %in% prac3_en$user_sn, 1, 0)
sum(video_en$indp3)
# ---------------------------↑ dp1, dp3 ↑---------------------------

# ---------------------------↓ dp2, dp3 ↓---------------------------
# (dp2, dp3) 英語 [dp2]且[dp3] [1]
df2_en_3 = prac2_en[prac2_en$user_sn %in% prac3_en$user_sn, ]
nrow(df2_en_3)
# ---------------------------↑ dp2, dp3 ↑---------------------------

# ---------------------------↓ dp1, dp2, dp3 ↓---------------------------
# (dp1, dp2, dp3) 英語 [dp1測驗且影片]且[dp2]且[dp3] [0]
df1_en_PV$indp23 = apply(df1_en_PV, 1, function(row) row['indp2']==1 & row['indp3']==1)
sum(df1_en_PV$indp23)

# (dp1, dp2, dp3) 英語 [dp1測驗]且[dp2]且[dp3] [0]
prac1_en$indp23 = apply(prac1_en, 1, function(row) row['indp2']==1 & row['indp3']==1)
sum(prac1_en$indp23)

# (dp1, dp2, dp3) 英語 [dp1影片]且[dp2]且[dp3] [0]
video_en$indp23 = apply(video_en, 1, function(row) row['indp2']==1 & row['indp3']==1)
sum(video_en$indp23)
# ---------------------------↑ dp1, dp2, dp3 ↑---------------------------
### ↑ 交集處理 ↑ ###

# 建模
# df1_ma_PV3[is.na(df1_ma_PV3)]=0
set.seed(1)
df1_ma1 = df1_ma_PV3[df1_ma_PV3$indp3==1, ] # dp1, dp3交集
s = sample(1:nrow(df1_ma1), 6)
df1_ma_train_1 = df1_ma1[s, ] # 有交集train
df1_ma_test_1 = df1_ma1[-s, ] # 有交集test

df1_ma0 = df1_ma_PV3[df1_ma_PV3$indp3==0, ] # dp1, dp3沒交集
s = sample(1:nrow(df1_ma0), 23)
df1_ma_train_0 = df1_ma0[s, ] # 沒交集train
df1_ma_test_0 = df1_ma0[-s, ] # 沒交集test
Train = rbind(df1_ma_train_1, df1_ma_train_0) # dp1和dp3有、沒有交集的訓練集之組合
Test1 = rbind(df1_ma_test_1, df1_ma_test_0)   # dp1和dp3有、沒有交集的測試集之組合
# Test2 = Test1+只有dp3，但是有資料缺失項

### model:
model.lm = lm(finalscore.x ~ testscore3+testcount3+learningmean+indp3, data=Train)

kk = 1
model.knn = kknn(finalscore.x ~ testscore1+learningmean+indp3, train=Train, test=Test1, k=kk)
model.k1re = kknn(finalscore.x ~ testscore1+learningmean+indp3, train=Train, test=Test1, k=kk,
                  distance=1, kernel='rectangular')
model.k1tr = kknn(finalscore.x ~ testscore1+learningmean+indp3, train=Train, test=Test1, k=kk,
                  distance=1, kernel='triangular')
model.k1ep = kknn(finalscore.x ~ testscore1+learningmean+indp3, train=Train, test=Test1, k=kk,
                  distance=1, kernel='epanechnikov')
model.k1ga = kknn(finalscore.x ~ testscore1+learningmean+indp3, train=Train, test=Test1, k=kk,
                  distance=1, kernel='gaussian')
model.k1op = kknn(finalscore.x ~ testscore1+learningmean+indp3, train=Train, test=Test1, k=kk,
                  distance=1, kernel='optimal')
model.k2re = kknn(finalscore.x ~ testscore1+learningmean+indp3, train=Train, test=Test1, k=kk,
                  distance=2, kernel='rectangular')
model.k2tr = kknn(finalscore.x ~ testscore1+learningmean+indp3, train=Train, test=Test1, k=kk,
                  distance=2, kernel='triangular')
model.k2ep = kknn(finalscore.x ~ testscore1+learningmean+indp3, train=Train, test=Test1, k=kk,
                  distance=2, kernel='epanechnikov')
model.k2ga = kknn(finalscore.x ~ testscore1+learningmean+indp3, train=Train, test=Test1, k=kk,
                  distance=2, kernel='gaussian')
model.k2op = kknn(finalscore.x ~ testscore1+learningmean+indp3, train=Train, test=Test1, k=kk,
                  distance=2, kernel='optimal')

Test1$pre.lm = predict(model.lm, newdata=Test1)
Test1$pre.knn = fitted(model.knn)
Test1$pre.k1re = fitted(model.k1re)
Test1$pre.k1tr = fitted(model.k1tr)
Test1$pre.k1ep = fitted(model.k1ep)
Test1$pre.k1ga = fitted(model.k1ga)
Test1$pre.k1op = fitted(model.k1op)
Test1$pre.k2re = fitted(model.k2re)
Test1$pre.k2tr = fitted(model.k2tr)
Test1$pre.k2ep = fitted(model.k2ep)
Test1$pre.k2ga = fitted(model.k2ga)
Test1$pre.k2op = fitted(model.k2op)

cat('MSE.ml : ', mean((Test1$pre.lm-Test1$finalscore)^2), '\n')
cat('MAE.ml : ', mean(abs(Test1$pre.lm-Test1$finalscore)), '\n')

cat('MSE.knn : ', mean((Test1$pre.knn-Test1$finalscore)^2), '\n')
cat('MAE.knn : ', mean(abs(Test1$pre.knn-Test1$finalscore)), '\n')

cat('MSE.k1re : ', mean((Test1$pre.k1re-Test1$finalscore)^2), '\n')
cat('MAE.k1re : ', mean(abs(Test1$pre.k1re-Test1$finalscore)), '\n')

cat('MSE.k1tr : ', mean((Test1$pre.k1tr-Test1$finalscore)^2), '\n')
cat('MAE.k1tr : ', mean(abs(Test1$pre.k1tr-Test1$finalscore)), '\n')

cat('MSE.k1ep : ', mean((Test1$pre.k1ep-Test1$finalscore)^2), '\n')
cat('MAE.k1ep : ', mean(abs(Test1$pre.k1ep-Test1$finalscore)), '\n')

cat('MSE.k1ga : ', mean((Test1$pre.k1ga-Test1$finalscore)^2), '\n')
cat('MAE.k1ga : ', mean(abs(Test1$pre.k1ga-Test1$finalscore)), '\n')

cat('MSE.k1op : ', mean((Test1$pre.k1op-Test1$finalscore)^2), '\n')
cat('MAE.k1op : ', mean(abs(Test1$pre.k1op-Test1$finalscore)), '\n')

cat('MSE.k2re : ', mean((Test1$pre.k2re-Test1$finalscore)^2), '\n')
cat('MAE.k2re : ', mean(abs(Test1$pre.k2re-Test1$finalscore)), '\n')

cat('MSE.k2tr : ', mean((Test1$pre.k2tr-Test1$finalscore)^2), '\n')
cat('MAE.k2tr : ', mean(abs(Test1$pre.k2tr-Test1$finalscore)), '\n')

cat('MSE.k2ep : ', mean((Test1$pre.k2ep-Test1$finalscore)^2), '\n')
cat('MAE.k2ep : ', mean(abs(Test1$pre.k2ep-Test1$finalscore)), '\n')

cat('MSE.k2ga : ', mean((Test1$pre.k2ga-Test1$finalscore)^2), '\n')
cat('MAE.k2ga : ', mean(abs(Test1$pre.k2ga-Test1$finalscore)), '\n')

cat('MSE.k2op : ', mean((Test1$pre.k2op-Test1$finalscore)^2), '\n')
cat('MAE.k2op : ', mean(abs(Test1$pre.k2op-Test1$finalscore)), '\n')




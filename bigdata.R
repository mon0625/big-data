user = read.csv('user_data.csv')
dp1_exam = read.csv('dp001_exam.csv')
dp1_prac = read.csv('dp001_prac.csv')
dp1_review = read.csv('dp001_review.csv')
dp1_review_p = read.csv('dp001_review_plus.csv')
dp2_exam = read.csv('dp002_exam.csv')
dp3_math = read.csv('dp003_math.csv')
dp3_word = read.csv('dp003_word.csv')

# dp1
dp1_prac_ch = dp1_prac[grep('國語文', dp1_prac$subject_name), ]
dp1_prac_en = dp1_prac[grep('英語', dp1_prac$subject_name), ]
dp1_prac_ma = dp1_prac[grep('數學', dp1_prac$subject_name), ]

dp1_mean_rate_ch = c()
dp1_mean_rate_en = c()
dp1_mean_rate_ma = c()
i = 1

for(p in user$user_sn){
  dp1_mean_rate_ch[i] = mean(dp1_prac_ch[dp1_prac_ch$user_sn==p, 'score_rate'])
  dp1_mean_rate_en[i] = mean(dp1_prac_en[dp1_prac_en$user_sn==p, 'score_rate'])
  dp1_mean_rate_ma[i] = mean(dp1_prac_ma[dp1_prac_ma$user_sn==p, 'score_rate'])
  i = i+1
}

# png("dp1_mean_rate_ch.png", width = 800, height = 600)
# boxplot(dp1_mean_rate_ch, ylim=c(0, 100), main='平台1國語測驗成績')
# dev.off()

# png("dp1_mean_rate_en.png", width = 800, height = 600)
# boxplot(dp1_mean_rate_en, ylim=c(0, 100), main='平台1英語測驗成績')
# dev.off()

# png("dp1_mean_rate_ma.png", width = 800, height = 600)
# boxplot(dp1_mean_rate_ma, ylim=c(0, 100), main='平台1數學測驗成績')
# dev.off()

# dp2
dp2_rate_ch = c()
dp2_rate_en = c()
i = 1

for(p in user$user_sn){
  tem_ch = as.numeric(as.logical(dp2_exam[dp2_exam$user_sn==p & dp2_exam$object_type=='中文閱讀', 'result_success']))
  dp2_rate_ch[i] = (sum(tem_ch)/length(tem_ch))*100
  tem_en = as.numeric(as.logical(dp2_exam[dp2_exam$user_sn==p & dp2_exam$object_type=='英文閱讀', 'result_success']))
  dp2_rate_en[i] = (sum(tem_en)/length(tem_en))*100
  i = i+1
}

# png("dp2_rate_ch.png", width = 800, height = 600)
# boxplot(dp2_rate_ch, ylim=c(0, 100), main='平台2國語測驗成績')
# dev.off()

# png("dp2_rate_en.png", width = 800, height = 600)
# boxplot(dp2_rate_en, ylim=c(0, 100), main='平台2英語測驗成績')
# dev.off()

# dp3
dp3_rate_en = c()
dp3_rate_ma = c()
i = 1

for(p in user$user_sn){
  tem_en = as.numeric(as.logical(dp3_word[dp3_word$user_sn==p, 'is_correct']))
  dp3_rate_en[i] = (sum(tem_en)/length(tem_en))*100
  tem_ma = as.numeric(as.logical(dp3_math[dp3_math$user_sn==p, 'is_correct']))
  dp3_rate_ma[i] = (sum(tem_ma)/length(tem_ma))*100
  i = i+1
}

# png("dp3_rate_en.png", width = 800, height = 600)
# boxplot(dp3_rate_en, ylim=c(0, 100), main='平台3英語測驗成績')
# dev.off()

# png("dp3_rate_ma.png", width = 800, height = 600)
# boxplot(dp3_rate_ma, ylim=c(0, 100), main='平台3數學測驗成績')
# dev.off()

### picture
# chinese
dp1_user_ch = user[user$user_sn %in% dp1_prac_ch$user_sn, 'chinese_score']
dp2_exam_ch = unique(dp2_exam[grep('中文閱讀', dp2_exam$object_type), ]$user_sn)
dp2_user_ch = user[user$user_sn %in% dp2_exam_ch, 'chinese_score']

png("chinese.png", width = 800, height = 600)
par(mfrow=c(1, 3))
boxplot(user$chinese_score , ylim=c(0, 100), main='國語大考')
boxplot(dp1_user_ch, ylim=c(0, 100), main='平台1國語大考')
boxplot(dp2_user_ch, ylim=c(0, 100), main='平台2國語大考')
dev.off()

# english
dp1_user_en = user[user$user_sn %in% dp1_prac_en$user_sn, 'english_score']
dp2_exam_en = unique(dp2_exam[grep('英文閱讀', dp2_exam$object_type), ]$user_sn)
dp2_user_en = user[user$user_sn %in% dp2_exam_en, 'english_score']
dp3_sn_en = unique(dp3_word$user_sn)
dp3_user_en = user[user$user_sn %in% dp3_sn_en, 'english_score']

png("english.png", width = 800, height = 600)
par(mfrow=c(1, 4))
boxplot(user$english_score, ylim=c(0, 100), main='英語大考')
boxplot(dp1_user_en, ylim=c(0, 100), main='平台1英語大考')
boxplot(dp2_user_en, ylim=c(0, 100), main='平台2英語大考')
boxplot(dp3_user_en, ylim=c(0, 100), main='平台3英語大考')
dev.off()

# math
dp1_user_ma = user[user$user_sn %in% dp1_prac_ma$user_sn, 'math_score']
dp3_sn_ma = unique(dp3_math$user_sn)
dp3_user_ma = user[user$user_sn %in% dp3_sn_ma, 'math_score']

png("math.png", width = 800, height = 600)
par(mfrow=c(1, 3))
boxplot(user$math_score, ylim=c(0, 100), main='數學大考')
boxplot(dp1_user_ma, ylim=c(0, 100), main='平台1數學大考')
boxplot(dp3_user_ma, ylim=c(0, 100), main='平台3數學大考')
dev.off()

# time video
library(dplyr)
meanlen = function(df){
  result = df %>% group_by(stime) %>% summarise(onedaylen=sum(learningtime))
  return(sum(result$onedaylen)/nrow(result))
}

# chinese
dp1_review_ch = dp1_review[grep('國語文', dp1_review$subject_name), ]
video_ch = data.frame(table(dp1_review_ch$user_sn))
colnames(video_ch) = c('user_sn', 'counts')
dp1_review_ch$learningtime = as.POSIXct(dp1_review_ch$end_time)-as.POSIXct(dp1_review_ch$start_time)

dp1_review_ch$stime = format(as.POSIXlt(dp1_review_ch$start_time), "%Y%m%d")
dp1_review_ch$etime = format(as.POSIXlt(dp1_review_ch$end_time), "%Y%m%d")
dp1_review_ch$sameday = apply(dp1_review_ch, 1, function(row) row['stime']==row['etime'])
which(!dp1_review_ch$sameday)

len = c()
i = 1
for(p in video_ch$user_sn){
  tem = dp1_review_ch[dp1_review_ch$user_sn==p, ]
  len[i] = meanlen(tem)
  i = i+1
}
video_ch$learningmean = len
a = user$user_sn %in% dp1_prac_ch$user_sn
dfa = data.frame(user_sn=user$user_sn[a], grade=dp1_user_ch)
df1_ch = merge(video_ch, dfa, by='user_sn')

png("chinese_learningTimeCount.png", width = 800, height = 600)
par(mfrow=c(1, 2))
plot(df1_ch$learningmean, df1_ch$grade)
plot(df1_ch$counts, df1_ch$grade)
dev.off()

# fit <- lm(df1_ch$grade ~ df1_ch$learningmean)
# plot(df1_ch$counts, df1_ch$grade)
# lines(df1_ch$learningmean, predict(fit), col = "blue", lwd = 2)

# math
dp1_review_ma = dp1_review[grep('數學', dp1_review$subject_name), ]
video_ma = data.frame(table(dp1_review_ma$user_sn))
colnames(video_ma) = c('user_sn', 'counts')
dp1_review_ma$learningtime = as.POSIXct(dp1_review_ma$end_time)-as.POSIXct(dp1_review_ma$start_time)

dp1_review_ma$stime = format(as.POSIXlt(dp1_review_ma$start_time), "%Y%m%d")
dp1_review_ma$etime = format(as.POSIXlt(dp1_review_ma$end_time), "%Y%m%d")
dp1_review_ma$sameday = apply(dp1_review_ma, 1, function(row) row['stime']==row['etime'])
which(!dp1_review_ma$sameday)

len = c()
i = 1
for(p in video_ma$user_sn){
  tem = dp1_review_ma[dp1_review_ma$user_sn==p, ]
  len[i] = meanlen(tem)
  i = i+1
}
video_ma$learningmean = len
a = user$user_sn %in% dp1_prac_ma$user_sn
dfa = data.frame(user_sn=user$user_sn[a], grade=dp1_user_ma)
df1_ma = merge(video_ma, dfa, by='user_sn')
df1_ma$testgrade = dp1

png("math_learningTimeCount.png", width = 800, height = 600)
par(mfrow=c(1, 2))
plot(df1_ma$learningmean, df1_ma$grade)
plot(df1_ma$counts, df1_ma$grade)
dev.off()

# english
dp1_review_en = dp1_review[grep('英語', dp1_review$subject_name), ]
video_en = data.frame(table(dp1_review_en$user_sn))
colnames(video_en) = c('user_sn', 'counts')
dp1_review_en$learningtime = as.POSIXct(dp1_review_en$end_time)-as.POSIXct(dp1_review_en$start_time)

dp1_review_en$stime = format(as.POSIXlt(dp1_review_en$start_time), "%Y%m%d")
dp1_review_en$etime = format(as.POSIXlt(dp1_review_en$end_time), "%Y%m%d")
dp1_review_en$sameday = apply(dp1_review_en, 1, function(row) row['stime']==row['etime'])
which(!dp1_review_en$sameday)

len = c()
i = 1
for(p in video_en$user_sn){
  tem = dp1_review_en[dp1_review_en$user_sn==p, ]
  len[i] = meanlen(tem)
  i = i+1
}
video_en$learningmean = len
a = user$user_sn %in% dp1_prac_en$user_sn
dfa = data.frame(user_sn=user$user_sn[a], grade=dp1_user_en)
df1_en = merge(video_en, dfa, by='user_sn')

png("english_learningTimeCount.png", width = 800, height = 600)
par(mfrow=c(1, 2))
plot(df1_en$learningmean, df1_en$grade)
line(df1_en$learningmean, df1_en$grade)
plot(df1_en$counts, df1_en$grade)
dev.off()


# dp1測驗成績且有看影片
df1_ma_TandV = data.frame(user_sn=user$user_sn, marate=dp1_mean_rate_ma) #


# dp3

# 建模
df1_ma1 = df1_ma[df1_ma$d==1, ] # dp1, dp3交集
df1_ma0 = df1_ma[df1_ma$d==0, ] # dp1, dp3沒交集
s = sample(1:nrow(df1_ma1), 6)
df1_ma_train_1 = df1_ma1[s, ] # 有交集train
df1_ma_test_1 = df1_ma1[-s, ] # 有交集test

s = sample(1:nrow(df1_ma0), 23)
df1_ma_train_0 = df1_ma0[s, ] # 沒交集train
df1_ma_test_0 = df1_ma0[-s, ] # 沒交集test
Train = rbind(df1_ma_train_1, df1_ma_train_0)
Test1 = rbind(df1_ma_test_1, df1_ma_test_0) # 

df3_ma0 = dp3_sn_ma[!dp3_sn_ma %in% df1_ma1$user_sn] # 只在dp3，沒在dp1
Test2 = rbind(dp3_math) #test1+只有dp3
model1 = lm(Train$grade ~ Train$)



sum(df1_ma$d)

#Train 跟test都要測驗分數,dfma改
#test2 需dp3的測驗成績



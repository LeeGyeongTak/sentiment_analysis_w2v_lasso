



category<-"mobile_phone_review"
category<-"food_review"

setwd(paste0("D:\\연대\\논문\\word2vec_lasso_감성분석\\영문데이터\\",category))
source("prep.R")


library(reader)

require(data.table)



# cm<-fread("Amazon_Unlocked_Mobile.csv",header=T)
# cm<-fread("Reviews.csv",header=T)



save(cm,file="Reviews.RData")
save(score,file="score.RData")


review<-cm$Text
score<-(cm$Score)

review<-as.character(review)
comments<-gsub("<.*?>","",review)
comments<-gsub("[][!#$%*,:;<=>@_`|‘~{}&★☆ㅋㅎ《》◈△▲▽▼○●◎◇◆□◁◀▷▶♤♠♡♥♧♣◉◈▣◐◑♨☏☎☜☞↖↘♭♩♪♬㈜]", " ",comments)
comments<-gsub("rdquo|gt|lt|nbsp|amp|quot|apos","",comments)
comments<-gsub("  "," ",comments)
comments<-gsub("\\^"," ",comments)
comments<-gsub("ㅠ|ㅜ|ㅡ"," ",comments)
comments<-gsub("\\(|\\)"," ",comments)
comments<-gsub("\"|\n|+","",comments)
comments<-gsub("\\+","",comments)
comments<-gsub("\\]|\\[|\\(|\\)|:|-|\\,|\\."," ",comments)
comments<-gsub("/|!|\\*|\\+|\\@"," ",comments)
comments<-gsub("'","",comments)
comments<-gsub("\"","",comments)
comments<-gsub("\"","",comments)
comments<-gsub("=","",comments)
comments<-gsub("~|;|<|>","",comments)
comments<-gsub("\\?","",comments)
comments<-gsub("\\(.*?\\)","",comments)
comments<-gsub("\\!","",comments)
comments<-gsub("\"\"","",comments)

review<-gsub("-","",comments)
review<-tolower(review)

save(review,file="review.RData")

sam_r<-sample(1:length(review),length(review))

write.csv(review[sam_r],"review.csv")
head(sam_r)
library(wordVectors)
prep_word2vec("review.csv","review.txt",lowercase=T)   
model = train_word2vec("review.txt",output="review.bin",threads = 3,vectors =50,window=7,min_count =30, cbow = 0,iter = 3,force=T)


model
nearest_to(model,model[["wonderful"]],10)
nearest_to(model,model[["unfortunately"]],10)
nearest_to(model,model[["fantasy"]],10)
nearest_to(model,model[["fascinating"]],10)
nearest_to(model,model[["sweet"]],10)
nearest_to(model,model[["true"]],10)
nearest_to(model,model[["pretty"]],10)
nearest_to(model,model[["emotional"]],10)
nearest_to(model,model[["action"]],10)
nearest_to(model,model[["big"]],10)
nearest_to(model,model[["performances"]],10)
nearest_to(model,model[["good"]],50)
nearest_to(model,model[["great"]],10)
nearest_to(model,model[["charming"]],10)





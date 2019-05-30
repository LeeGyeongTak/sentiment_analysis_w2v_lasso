
setwd("D:\\연대\\논문\\word2vec_lasso_감성분석\\영문데이터\\sentence_movie_review_data\\rt-polaritydata")

list.files()


getwd()

library(wordVectors)

library(stringr)



category<-"mobile_phone_review"
category<-"food_review"
num_sample<-1000


setwd(paste0("D:\\연대\\논문\\word2vec_lasso_감성분석\\영문데이터\\",category,"\\실험",num_sample))
# sum(1:15)
getwd()

load("review.RData")
load("score.RData")

polar_review<-review
library(wordVectors)
length(polar_review)
la<-1
se<-10
q<-1

zz<-0

getwd()






la_result<-NULL
la_result2<-NULL
la_result3<-NULL
la_result4<-NULL

la_result_fm<-NULL
la_result2_fm<-NULL
la_result3_fm<-NULL
la_result4_fm<-NULL


for(q in 1:10){
  
  # la<-0
  
  final_result<-list()
  seqq<-c(1,10,30,50,100,300,500,1000)
  # seqq<-c(1,10,30,50)
  
  comments<-      (  as.character(polar_review))
  sam<-sample(1:length(comments),num_sample*1)
  # length(polar_review)
  library(stringr)
  
  save(sam,file=paste0(q,"sam.RData"))
  # load("3sam.RData")
  cos<-comments[sam]

  po<-str_split(cos," ")
  
  toq<-table(unlist(str_split(cos," ")))
  toq2<-(toq[toq>1])
  length(toq)
  # la<-1
  erw<-c("") 
  
  
  

  library(lexicon)
  data(hash_sentiment_sentiword)
  
  hash_sentiment_sentiword[hash_sentiment_sentiword$x %in% "like",]
  hash_sentiment_sentiword[hash_sentiment_sentiword$x %in% "love",]
  hash_sentiment_sentiword[hash_sentiment_sentiword$x %in% "best",]
  hash_sentiment_sentiword[hash_sentiment_sentiword$x %in% "good",]

  
  
  head(cos)
  for(la in 1:1){
    se 
    # la<-0
    ##basemodel
    poc_list<-list()
    neg_list<-list()
    int_list<-list()
    lasso_result2<-NULL
    for(x in 1:50){
      cat("\n",x,"회중 시행 중")
      
      
      
      ss<-(sample(1:length(cos),length(cos),replace=T))
      
      
      # 
      #       bbi<-ifelse(score[sam]>3,1,0)
      #       bbi2<-1-table(bbi)/length(bbi)
      #       bbi3<-bbi2[match(bbi,names(bbi2))]
      #       bbi4<-bbi3/sum(bbi3)
      # 
      #         ss<-(sample(1:length(cos),length(cos),prob=bbi4,replace=T))
      
      cos2<-cos[ss]
      
      
      load("score.RData")
      score2<-score[sam]
      
      score3<-score2[ss]
      table(score3)
      ims<-which(score3==3)
      
      # score3<-score3[-ims]
      # sp_cos_sh<-str_split(cos2[-ims]," ")
      
      sp_cos_sh<-str_split(cos2," ")
      mean(sapply(sp_cos_sh,length))
      tas<-table(unlist(sp_cos_sh))
      head(tas)
      length(tas)
      
      tas2<-tas[tas >4]
      length(tas2)
      
      length(names(tas2))
      length(names(toq))
      names(tas2)[(!names(tas2) %in% names(toq2))]
      
      
      # tas2<-tas2[names(tas2) %in% name_to]
      
      tas2<-tas2[!sapply(str_split(names(tas2),""),length)  <= 1]
      
      tas2<-tas2[!str_detect(names(tas2),"\n|\\)")]
      
      tas2<-tas2[!str_detect(names(tas2),"\\d")]
      tas2<- tas2[!names(tas2) %in% c(erw,gun)]
      
      
      tas3<-tas2[!names(tas2)== ""]
      nat<-names(tas3)
      
      dtm<-matrix(c(0),ncol=length(tas3),nrow=(length(ss)))
      colnames(dtm)<-nat
      length(nat)
      dim(dtm)
      i<-1
      i
      sp_cos_sh[[1]]
      for(i in 1:length(sp_cos_sh)){
        
        matin<-match(sp_cos_sh[[i]],nat)
        tt<-table(matin[!is.na(matin)])
        
        dtm[i,  as.numeric(names(tt))]<-tt
        # dtm[i,  as.numeric(names(tt))]<-1
        # dtm[i,nat %in% sp_cos[[i]] ]<-1
        
        # cat("\n",i)
        
      }
      
      
      # dim(dtm)
      
      # score<-polar_review[,2]
      
      # score<-c(rep(1,1000),rep(0,1000))
      
      
      whs <- apply(dtm,1,sum) == 0
      real_dtm2<-dtm[!whs,]
      sp_cos3<-cos2[!whs]
      sum(whs)
      # scorescore3[score3!=3]
      score_one_word<-      ifelse(score3>3,1,0)[!whs]
      
      # table(score_one_word)
      
      
      # good_col<-colnames(dtm)
      
      
      library(glmnet)
      
      import_vari<-sample(1:ncol(dtm),ncol(dtm)*1)
      
      
      
      # la<-1
      
      
      #################################Rasso or Lidge
      
      
      fit2=glmnet(as.matrix(real_dtm2[,import_vari]),as.numeric(score_one_word),family="binomial",alpha=la)
      coef(fit2,s=0.001) # 1이면 lasso
      pre<-predict(fit2,newx=as.matrix(real_dtm2[,import_vari]),s=0.0011) # make predictions
      
      
      pre2<-ifelse(pre > 0,1,0)
      
      sum(pre2 == score_one_word)/length(score_one_word)
      
      table(pre2)
      length(pre2)
      table(score_one_word)
      
      
      test_data<-cos[which(!1:num_sample %in% unique(ss))]
      
      dtm2<-matrix(c(0),ncol=length(tas3),nrow=(length(test_data)))
      colnames(dtm2)<-nat
      spc<-str_split(test_data," ")
      j<-1
      for(j in 1:length(test_data)){
        matin<-match(spc[[j]],nat)
        tt<-table(matin[!is.na(matin)])
        dtm2[j,  as.numeric(names(tt))]<-tt
        # dtm2[j,  as.numeric(names(tt))]<-1
      }
      dtm[1,]
      
      
      la
      
      # pre<-predict(fit2,newx=as.matrix(dtm2[,colnames(dtm2)  %in% good_col]),s=0.011111) # make predictions
      pre<-predict(fit2,newx=as.matrix(dtm2[,import_vari]),s=0.011111) # make predictions
      
      pred<-as.vector(ifelse(pre>0,1,0))
      dim(as.matrix(dtm2[,import_vari]))
      
      table(score2)
      gek<-score2[which(!1:num_sample %in% unique(ss))]
      gek<-ifelse(gek>3.5,1,0)
      table(gek)
      lasso_result<-sum(pred ==gek)/length(pred);lasso_result
      
      
      coes<-coef(fit2,s=0.01)
      dim(coes)
      cod<-as.matrix(coes)
      hist(cod)
      
      dim(cod)
      cod2<-cod[cod[,1] !=0,][-1]
      int<-cod[1,]
      int_list[[x]]<-int
      
      poc<-(cod2[cod2>0])
      bumwi3<-max(poc)-min(poc)
      dan3<-bumwi3
      poc2<-poc/dan3
      poc3<-poc2-max(poc2)+1
      length(poc3)
      # # final_result
      #
      hist(poc3)
      
      neg<-(cod2[cod2<0])
      bumwi4<-max(neg)-min(neg)
      dan4<-bumwi4
      neg2<-neg/dan4
      neg3<-neg2-max(neg2)
      # hist(neg3)
      # summary(poc);summary(neg)
      # length(poc3);length(neg3)
      # #
      # poc3<-poc
      # neg3<-neg
      # # hist(poc3)
      sort(poc,decreasing = T)[1:10]
      sort(neg,decreasing = F)[1:10]
      summary(poc)
      summary(neg)
      length(poc);length(neg)
      
      
      poc_list[[x]]<-poc3
      neg_list[[x]]<-neg3
      
      cat("\n",lasso_result)
      lasso_result2<-c(lasso_result2,lasso_result)
      
      ####
      # 
      # # he
      # uu<-1
      # baba<-NULL
      # for(uu in 1:length(sp_cos_sh)){
      # 
      #   matin<-match(sp_cos_sh[[uu]], names(allc))
      #   tt<-table(matin[!is.na(matin)])
      #   allc[as.numeric(names(tt))]
      #   baba[uu]<- sum(allc[as.numeric(names(tt))])
      # }
      # sum(ifelse(baba>0,1,0) == score_one_word)/(num_sample)
      
      ########
      
      
      ns<- sort(neg,decreasing = F)[1:10];ns
      ns<- sort(poc,decreasing = T)[1:10];ns
      table(score_one_word[str_detect(cos,names(ns[1]))])
      
      
    }
    # 0.80285413
    mean(lasso_result2)
    # 0.764
    # warnings()
    
    # for(qq in 1:length(poc_list)){
    #   poc_list[[qq]]<-poc_list[[qq]]*lasso_result2[qq]
    #   neg_list[[qq]]<-neg_list[[qq]]*lasso_result2[qq]
    # 
    #   int_list[[qq]]<-int_list[[qq]]*lasso_result2[qq]
    # }
    
    
    upc<-unlist(poc_list)
    une<-unlist(neg_list)
    int2<-mean(unlist(int_list))
    
    unun<-c(upc,une)
    
    nau<-table(names(unun))
    all_w<- tapply((unun),names(unun),sum)
    
    
    
    # save(all_w,file="all_word_polar_word2vec.RData")
    
    head(all_w)
    sort(all_w,decreasing = T)[1:10]
    # chargers      brand  excellent everything relatively    awesome    perfect    exactly  excelente       love 
    # 12.322056   9.409657   9.277923   8.965769   8.737494   8.548497   8.446212   8.373338   8.345910   7.988426 
    sort(all_w,decreasing = F)[1:10]
    length(all_w)
    save(all_w,file="all_w.RData")
    poc3<-all_w[all_w > 0]
    neg3<-all_w[all_w < 0]
    
    
    poc3<-poc3[!names(poc3) %in% erw]
    neg3<- neg3[!names(neg3) %in% erw]
    
    length(poc3);length(neg3)
    
    # mpn<- min(length(poc3),length(neg3))
    
    # poc3<-sqrt(poc3)
    # neg3<- -sqrt(abs(neg3))
    
    # save(poc3,file=paste0(la,"-",se,"-",q,"positive_word.RData"))
    # save(neg3,file=paste0(la,"-",se,"-",q,"negative_word.RData"))
    # load("positive_word.RData")
    # load("negative_word.RData")
    
    
    
    
    
    won_result<-NULL
    won_result2<-NULL
    won_result3<-NULL
    won_result4<-NULL
    won_result_fm<-NULL
    won_result2_fm<-NULL
    won_result3_fm<-NULL
    won_result4_fm<-NULL
    sen_result_fm<-NULL
    sen_result<-NULL
    lenw<-c()
    for(se in seqq){
      
      
      for(ww in 1:1){
        
        # se<-1
        getwd()
        
        if(ww ==1){
          wm<-"orig_w2v"
          # model<-read.vectors("review3.bin")
          # load("googlewv.RData")
          # save(model,file="model.RData")
          
          load("model.RData")
          model<-(model)[!str_detect(rownames(model),"\\d$"),]
          rownames(model)
          # erw2
          # model<-read.vectors("sen_comments_9000_3.bin")
          # model<-read.vectors("sen_comments_9000_3.bin")
          # sort(poc3,decreasing = T)[1:10]
          allc<-c(poc3,neg3)
          
          
          sksk<-ifelse(score2>3,1,0)
          sksk2<-table(sksk)[1]/table(sksk)[2]
          sksk2
          
          al<-length(neg3)/(length(poc3))
          sksk2<-al
          if(al <1){
            sksk2<-length(poc3)/(length(neg3))             
          }
          
          ap<-abs(mean(sort(poc3,decreasing = T)[1:10]))
          ap2<-abs(mean(sort(neg3,decreasing = F)[1:10]))
          al<-ap/ap2 
          sksk2<-al 
          if(al >1){
            sksk2<-   ap2/ap
            
          }
          
          pos_wrod<-list()
          
          # for(i in poi){
          i<-10
          length(allc)
          # se<-500
          poc4<- sort(poc3,decreasing = T)
          # se<-1000
          # poc4<-  poc4[1:mpn]
          
          li<-se/length(poc4)
          li2<-length(poc4):1 *li
          # li2<-1:length(poc4) *li
          se2<-se- round(li2)+1
          head(poc4)
          
          se2<-ceiling(1/se2 *se)+1

          se
          if(se ==1){
            se2<-rep(1,length(poc4))
          }
          
          # se2<-rep(se,length(poc4))
          for(i in 1:length(poc4)){
            
            nap<-nearest_to(model,model[[names(poc4[i])]],se2[i])
            
            
            nap<-  nap[!is.na(nap)]
            nap2<-nap
            # nap<-nap[nap<0.3]
            
            # nap<-nap[!names(nap) %in% names(allc)]
            
            
            if(length(nap)==0){
              
              pos_wrod[[i]]<-poc4[i]
              next;
            }
            pos_wrod[[i]]<-c(poc4[i] * (1-nap) )
            # cat("\t",i)
            
          }
          
          
          pix<-sapply(pos_wrod,function(x){ sum(names(x) %in% "better")}) == 1
          pos_wrod[pix]
          
          
          pos_wrod2<-do.call("c",pos_wrod)
          
          top<-table(names(do.call("c",pos_wrod)))
          sort(top,decreasing = T)[1:30]
          
          
          pos_word2<-tapply((pos_wrod2),names(pos_wrod2),sum)
          # pos_word2<-tapply((pos_wrod2),names(pos_wrod2),max)
          
          
          neg_wrod<-list()
          neg4<- sort(neg3,decreasing = F)
          # neg4<-neg4[1:mpn]
          
          # neg_se<-round(se*sksk2,0)
          neg_se<-se
          li<-neg_se/length(neg4)
          li2<-length(neg4):1 *li
          # li2<-1:length(poc4) *li
          se2<-neg_se- round(li2)+1
          head(neg4)
          se
          se2<-ceiling(1/se2 *neg_se)+1
          head(se2)
          sum(se2)*sksk2
          se2<-round(se2*sksk2,0)
          se
          if(se ==1){
            se2<-rep(1,length(neg4))
          }
          
          # se2<-rep(se,length(neg4))
          for(i in 1:length(neg4)){
            # for(i in (nei)){
            nap<-nearest_to(model,model[[names(neg4[i])]],se2[i])
            nap<-  nap[!is.na(nap)]
            # nap2<-nap - 0.2
            # nap2[nap2<=0]<-0
            nap2<-nap
            # nap<-nap[nap<0.3]
            # nap<-nap[!names(nap) %in% names(allc)]
            
            if(length(nap)==0){
              neg_wrod[[i]]<-neg4[i]
              next;
            }
            neg_wrod[[i]]<-c(neg4[i] * (1-nap))
            # cat("\t",i)
            # print(neg3[i] * abs(1-nap))
            
          }
          
          # neg_wrod<-c(neg_wrod,neg3)
          neg_wrod2<-do.call("c",neg_wrod)
          neg_word2<-tapply((neg_wrod2),names(neg_wrod2),sum)
          # neg_word2<-tapply((neg_wrod2),names(neg_wrod2),min)
          
          pix<-sapply(neg_wrod,function(x){ sum(names(x) %in% "horrible")}) == 1
          neg_wrod[pix]
          
          
          sort(neg_word2,decreasing = F)[1:10]
          sort(pos_word2,decreasing = T)[1:10]
          
          length(poc3);length(neg3)
          length(pos_word2)
          length(neg_word2)
          
          
          # warnings()  
          
          
          # # 
          pos_word2<-  pos_word2[!names(pos_word2) %in% names(neg3)]
          neg_word2<-  neg_word2[!names(neg_word2) %in% names(poc3)]
          pos_word2<- (sqrt(pos_word2))
          neg_word2<- -(sqrt(abs(neg_word2)))
          # 
          mpmp<-min(length(neg_word2),length(pos_word2))
          neg_word3<-sort(neg_word2,decreasing = T)[1:mpmp]
          pos_word3<-sort(pos_word2,decreasing = F)[1:mpmp]
          
          
          all_word<-c(pos_word2,neg_word2)
          
          all_word2<-tapply((all_word),names(all_word),sum)
          lenw<-c(lenw,length(all_word2))
          
          # all_word2<-all_word2[abs(all_word2)  > quantile(abs(all_word2),0.1)]
          all_word2<-all_word2[!names(all_word2) %in% names(ll2)[1:10]]
          # all_word2<-all_word2[!names(all_word2) %in% names(pvr)]
          sort(all_word2,decreasing = T)[1:30]
          sort(all_word2,decreasing = F)[1:30]
          length(neg_word2);length(pos_word2)
          # 

          #########
          
          # num_sample
          # se2
          
          # 
          # gc_score<-score[-sam]
          # length(comments)
          # sp_gc_sam<-str_split(tolower(comments[-sam])," ")
          # test_sm<-sample(1:length(gc_score),100000)
          # 
          # gc_score<-gc_score[test_sm]
          # sp_gc_sam<-(sp_gc_sam[test_sm])
          # length(gc_score)
          # length(sp_gc_sam)
          # 
          # save(gc_score,file="gc_score.RData")
          # save(sp_gc_sam,file="sp_gc_sam.RData")
          load("gc_score.RData")
          load("sp_gc_sam.RData")
          table(gc_score)
          gc_score<-ifelse(gc_score>3,1,0)
          # sp_gc_sam[[i]]
          
          uss<-unique(unlist(sp_gc_sam))
          
          all_word2<-all_word2[names(all_word2) %in% uss]
          
          won<-c()
          final<-c()
          
          allc<-c(poc3,neg3)
          allc<-allc[!names(allc) %in% names(ll2)[1:10]]
    
          
          
          
          senti<-c()
          for(i in 1:length(sp_gc_sam)){
            matin<-match(sp_gc_sam[[i]], names(all_word2))
            tt<-table(matin[!is.na(matin)])
            won[i]<- sum(all_word2[as.numeric(names(tt))])

            matin<-match(sp_gc_sam[[i]], names(allc))
            tt<-table(matin[!is.na(matin)])
            he[i]<- sum(allc[as.numeric(names(tt))])

            matin<-match(sp_gc_sam[[i]], hash_sentiment_sentiword$x)
            tt<-table(matin[!is.na(matin)])

            if(nrow(hash_sentiment_sentiword[as.numeric(names(tt))])==0){
              senti[i]<- 0
            }else{
            senti[i]<- sum(hash_sentiment_sentiword[as.numeric(names(tt))][,2])
            }
          }
            sum(he==0)
          he

          tet<-ifelse(he >0,1,0)
          table(tet)
          sum(he ==0)
          gc_score3<-gc_score
          se

          head(he,30)
          head(won,30)
          se
          sigm<-function (x)           {
            1/(1 + exp(-x))         }
          tet<-sigm(he)
          tet<-ifelse(tet >0.5,1,0);table(tet)


          base<-sum(tet==gc_score3)/length(tet);base
          precision<-sum(tet==1 & gc_score3==1)/sum(tet==1) ##precision
          recall<-sum(tet==1 & gc_score3==1)/sum(gc_score3==1) ##recall

          base_fm<-2*((precision*recall)/(precision+recall));base_fm
          roc_obj <- pROC::roc(gc_score3,he)
          plot.roc(roc_obj)
          base_fm<-pROC::auc(roc_obj)
          base_fm
          won_result<-c(won_result,base)
          won_result_fm<-c(won_result_fm,base_fm)

          # sum(won == 0)
          he[5615]
          # prediction2[5615]
          # won2<-won[won> 0]
          sigm<-function (x)           {
            1/(1 + exp(-x))         }
          prediction<-sigm(won)
          prediction2<-ifelse(prediction >0.5,1,0);table(prediction2)



          # prediction2<-ifelse(won >0,1,0);table(prediction2)
          real_score2<-gc_score
          # anmat<-(prediction2 != prediction)
          # sum(anmat)

          whiwhi<-which(final != 0)
          table(real_score2)
          se
          wonwon<-sum(prediction2 == real_score2[1:length(prediction2)])/length(prediction2);wonwon
          precision<-sum(prediction2==1 & real_score2==1)/sum(prediction2==1) ##precision
          recall<-sum(prediction2==1 & real_score2==1)/sum(real_score2==1) ##recall
          # 0.892088
          won_fm<-2*((precision*recall)/(precision+recall));won_fm
          la
          library(pROC)
          roc_obj <- pROC::roc( real_score2,prediction)
          won_fm<-pROC::auc(roc_obj)
          won_fm
          plot.roc(roc_obj)
          #acc2<-c(wonwon,won_fm)

          won_result2<-c(won_result2,wonwon)
          won_result2_fm<-c(won_result2_fm,won_fm)

          print(won_result)
          print(won_result2)
          print(won_result_fm)
          print(won_result2_fm)
          #######+
          prediction<-sigm(senti)
          prediction2<-ifelse(prediction >0.5,1,0);table(prediction2)

          # prediction2<-ifelse(senti>0,1,0);table(prediction2)
          real_score2<-gc_score
          # anmat<-(prediction2 != prediction)
          # sum(anmat)

          whiwhi<-which(final != 0)
          table(real_score2)
          se
          wonwon<-sum(prediction2 == real_score2[1:length(prediction2)])/length(prediction2);wonwon
          precision<-sum(prediction2==1 & real_score2==1)/sum(prediction2==1) ##precision
          recall<-sum(prediction2==1 & real_score2==1)/sum(real_score2==1) ##recall

          s_fm<-2*((precision*recall)/(precision+recall));s_fm
          roc_obj <- pROC::roc(real_score2,prediction)
          s_fm<-pROC::auc(roc_obj)
          s_fm
          acc3<-c(wonwon,s_fm)

          sen_result<-c(sen_result,wonwon)
          sen_result_fm<-c(sen_result_fm,s_fm)
        
          
          cat("\n",la,"-",se,"-",ww,"-",q)
          if(la == 0){
            la2<-"ridge"
          }else{
            la2<-"lasso"
          }
          write.csv(won_result2_fm,paste0(la2,"-nearest",se,"-",wm,"-",q,"회 실험-all-",round(mean(lasso_result2),4),"-",round(base,4),"-",round(base_fm,4),"-acc",round(wonwon,4),"-f_m",round(won_fm,4),".csv"),row.names=F)
          save(all_word2,file=paste0("png_word_list-",la2,"-",wm,"-nearest",se,"-data",num_sample,"-",q,"회 실험-all-",round(wonwon,4),"-f_m",round(won_fm,4),".RData"))
          
 
          
          
          
          
        }
        
        
        
        
      }
      
      
     
      
      
      
    }
    
    
   

      la2<-"lasso"

      sen_la_result<-rbind(sen_la_result,sen_result)
      sen_la_result_fm<-rbind(sen_la_result_fm,sen_result_fm)
      la_result<-rbind(la_result,won_result)
      la_result2<-rbind(la_result2,won_result2) 
      la_result3<-rbind(la_result3,won_result3) 
      la_result4<-rbind(la_result4,won_result4) 
      la_result_fm<-rbind(la_result_fm,won_result_fm) 
      la_result2_fm<-rbind(la_result2_fm,won_result2_fm) 
      la_result3_fm<-rbind(la_result3_fm,won_result3_fm) 
      la_result4_fm<-rbind(la_result4_fm,won_result4_fm) 
      

    
    
  }
  
  ############supervised learning
  score2<-score[sam]

  # score3<-score2[ss]
  # table(score3)
  ims<-which(score2==3)

  score3<-score2[-ims]
  sp_cos_sh<-str_split(cos2[-ims]," ")

  # sp_cos_sh<-str_split(cos2," ")
  mean(sapply(sp_cos_sh,length))
  tas<-table(unlist(sp_cos_sh))
  head(tas)
  length(tas)

  tas2<-tas[tas >2]
  length(tas2)

  length(names(tas2))
  length(names(toq))
  names(tas2)[(!names(tas2) %in% names(toq2))]


  # tas2<-tas2[names(tas2) %in% name_to]

  tas2<-tas2[!sapply(str_split(names(tas2),""),length)  <= 1]

  tas2<-tas2[!str_detect(names(tas2),"\n|\\)")]

  tas2<-tas2[!str_detect(names(tas2),"\\d")]
  tas2<- tas2[!names(tas2) %in% c(erw,gun)]


  tas3<-tas2[!names(tas2)== ""]
  nat<-names(tas3)

  dtm<-matrix(c(0),ncol=length(tas3),nrow=(length(ss)))
  colnames(dtm)<-nat
  length(nat)

  load("gc_score.RData")
  load("sp_gc_sam.RData")
  table(gc_score)


  gc_score<-ifelse(gc_score>3,1,0)
  sp_gc_sam[[i]]


  validtm<-matrix(c(0),ncol=length(tas3),nrow=(length(sp_gc_sam)))
  colnames(validtm)<-nat

  for(i in 1:length(sp_cos_sh)){

    matin<-match(sp_cos_sh[[i]],nat)
    tt<-table(matin[!is.na(matin)])

    dtm[i,  as.numeric(names(tt))]<-tt


  }
  for(i in 1:length(sp_gc_sam)){
    matin<-match(sp_gc_sam[[i]],nat)
    tt<-table(matin[!is.na(matin)])
    validtm[i,  as.numeric(names(tt))]<-tt
  }

  whs <- apply(dtm,1,sum) == 0
  real_dtm2<-dtm[!whs,]
  sp_cos3<-cos2[!whs]
  sum(whs)
  # scorescore3[score3!=3]
  score_one_word<-      ifelse(score3>3,1,0)[!whs]

  pred<-pre2
  valid<-gc_score
  fm<-function(pred,valid){
    precision<-sum(pred==1 & valid==1)/sum(pred==1) ##precision
    recall<-sum(pred==1 & valid==1)/sum(valid==1) ##recall
    won_fm<-2*((precision*recall)/(precision+recall));won_fm
    won_fm
  }
  # install.packages("pROC")

  #############lasso
  fit2=glmnet(as.matrix(real_dtm2),as.numeric(score_one_word),family="binomial",alpha=1,nlambda=1000)
  # coef(fit2,s=0.001) # 1이면 lasso
  ss<-fit2$lambda[length(fit2$lambda)]
  pre<-predict(fit2,newx=as.matrix(validtm),s=ss) # make predictions
  pre2<-ifelse(pre > 0,1,0)
  table(pre2)
  lasso1<-sum(pre2 == gc_score)/length(pre2);lasso1
  lasso_fm<-fm(pre2,gc_score);lasso_fm

  roc_obj <- pROC::roc(gc_score,as.numeric(pre2))
  lasso_fm<-pROC::auc(roc_obj);lasso_fm
  ############ridge
  fit2=glmnet(as.matrix(real_dtm2),as.numeric(score_one_word),family="binomial",alpha=0,nlambda=1000)
  # coef(fit2,s=0.001) # 1이면 lasso
  ss<-fit2$lambda[length(fit2$lambda)]
  pre<-predict(fit2,newx=as.matrix(validtm),s=ss) # make predictions
  pre2<-ifelse(pre > 0,1,0)
  ridge1<-sum(pre2 == gc_score)/length(pre2);ridge1
  # ridge_fm<-fm(pre2,gc_score);ridge_fm
  roc_obj <-pROC::roc(gc_score,as.numeric(pre2) )
  ridge_fm<-pROC::auc(roc_obj);ridge_fm


  ## SVM
  library(kernlab)

  #polydot
  # rbfdot
  svmdtm2<-data.frame(cbind(real_dtm2,score_one_word))
  colnames(svmdtm2)<-c(paste0("V",1:(ncol(svmdtm2)-1)),"target")
  colnames(validtm)<-colnames(svmdtm2)[-ncol(svmdtm2)]
  ### linear
  irismodel <- ksvm(target~.,data=svmdtm2,type="C-bsvc",
                    kernel="vanilladot",C=0.001,prob.model=TRUE)
  irismodel
  head(validtm)
  pr<-predict(irismodel, as.matrix(validtm), type="prob")
  pre2<-ifelse(pr[,2] > 0.5,1,0)
  svm<-sum(pre2 == gc_score)/length(pre2);svm


  roc_obj <-pROC::roc(gc_score,as.numeric(pr[,2]) )
  svm_fm<-pROC::auc(roc_obj);svm_fm

  #######
  irismodel <- ksvm(target~.,data=svmdtm2,type="C-bsvc",
                    kernel="polydot",C=0.001,prob.model=TRUE)
  irismodel
  pr<-predict(irismodel, as.matrix(validtm),type="prob")
  pre2<-ifelse(pr[,2] > 0.5,1,0)
  svm2<-sum(pre2 == gc_score)/length(pre2);svm2


  roc_obj <-pROC::roc(gc_score,as.numeric(pr[,2]) )
  svm_fm2<-pROC::auc(roc_obj);svm_fm2
  # pr
  irismodel <- ksvm(target~.,data=svmdtm2,type="C-bsvc",
                    kernel="rbfdot",C=0.001,prob.model=TRUE)
  irismodel
  pr<-predict(irismodel, as.matrix(validtm), type="prob")
  pre2<-ifelse(pr[,2] > 0.5,1,0)
  svm3<-sum(pre2 == gc_score)/length(pre2);svm3

  roc_obj <-pROC::roc(gc_score,as.numeric(pr[,2]) )
  svm_fm3<-pROC::auc(roc_obj);svm_fm3





  library(randomForest)



  iris.rf <- randomForest(target ~ ., data=svmdtm2, importance=TRUE,do.trace=T,ntree=300,
                          proximity=TRUE)
  pr<-predict(iris.rf, (validtm))

  summary(pr)
  pre2<-ifelse(pr > 0.5,1,0)
  rf<-sum(pre2 == gc_score)/length(pre2);rf

  library(AUC)

  roc_obj <-pROC::roc(gc_score,pr )
  rffm<-pROC::auc(roc_obj);rffm

  sv_result<-rbind(sv_result,c(lasso1,ridge1,svm,svm2,svm3,rf))
  sv_result_fm<-rbind(sv_result_fm,c(lasso_fm,ridge_fm,svm_fm,svm_fm2,svm_fm3,rffm))

  # sen_la_result<-rbind(sen_la_result,c(sen_result,sen_result_fm))
  
}

lasso_result<-cbind(cbind(la_result,"",la_result_fm),"",
                    cbind(la_result2,"",la_result2_fm),"",
                    cbind(sen_la_result,"",sen_la_result_fm))

sv_result<-cbind(sv_result,"",sv_result_fm)




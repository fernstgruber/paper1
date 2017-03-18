require(e1071)
#source("/home/fabs/Data/ST_landformclasses/ST_landforms_funcsandlegends/relief_legends.R")
require(nnet)
setwd("/home/fabs/Data/paper1_lenny/neu_unzugeordnet/FWCV/")
load("makroreddata_andpredlists.RData")
data=makroreddata
data$neigunggrad <- atan(data$Neig/100)*180/pi
data$neidiff <- abs(data$neigunggrad - data$slope_ws5)
summary(data$neidiff)
data <- data[data$neidiff <= 15,]
load("//home/fabs/Data/paper1_lenny/datawithpreds.RData")
dependent="Def_red_mak"
#newdata <- datawithpreds[c("AufID",dependent)]
#newdata[[dependent]] <- as.factor(newdata[[dependent]])
#newdata <- newdata[newdata[[dependent]] %in% c("0","6"),]
#newdata <- droplevels(newdata)
#regroup="Mittelhang"
#data <- merge(newdata,data,by="AufID",all.x=T)
load("allterraincols.RData")
allterrain %in% names(data)
##########################

preds=c(goodtophat,tpi_10m,tpi_50m,as.character(allterrain))

withna <- vector()
for (i in preds){
  if(sum(is.na(data[[i]])) != 0) {
    withna <- c(withna,as.character(i))
  }
}

preds <- preds[!(preds %in% withna)]
fullmodelcols <- c(dependent,preds)
mymodeldata <- na.omit(data[fullmodelcols])

#########################################################################################
paramsetnames_plus <- c("terraintpitophat")
paramsetnames = paramsetnames_plus
paramsets = list(preds)
n=1
folds = sample(rep(1:10, length = nrow(mymodeldata)))
p=paramsets[1]
for (p in paramsets[1]){
predset_name <- paramsetnames[1]
preds <- unlist(p)
predset=preds
tt=1:6 #number of best parameters in combination
mydir=paste("fwcv_6p_slopeadj_",dependent,"_",predset_name,sep="")
dir.create(mydir)
#############################################################################################################################
#############################################################################################################################


for(k in 1:10){
  kmodeldata=mymodeldata[folds != k,]
  ktestdata =  mymodeldata[folds == k,]
  keepers <- vector()
  pred_df_orig <- data.frame(preds = as.character(predset))
  pred_df_orig$index <- 1:nrow(pred_df_orig)
  pred_df <- pred_df_orig
  result_df <- data.frame(tt)
  predictions_metrics <- data.frame(index=as.character(unique(pred_df$preds)))
  predset_new <- predset
  t=1
  for(t in tt){
    predictions_metrics <- predictions_metrics
    predset_new <-predset_new[!(predset_new %in% keepers)]
    seed=sample(1:1000,1)
    g=predset_new[1]
    for(g in predset_new){
      starttime <- proc.time()
      set.seed(seed)
      modelcols <- c(dependent,g,keepers)
      modeldata <- kmodeldata[names(kmodeldata) %in% modelcols]
      f <- paste(dependent,"~.")
      fit <- do.call("svm",list(as.formula(f),modeldata,cross=10))
      cverror = 1-(fit$tot.accuracy)/100
      predictions_metrics[predictions_metrics$index== eval(g),"cverror"] <- cverror
      endtime <- proc.time()
      time <- endtime - starttime
      print(paste(g, " with cverror error.rate of ",cverror, "and time =",time[3]))
      #######################################################################
    }
    predictions_metrics <- predictions_metrics[order(predictions_metrics$cverror),]
    
    minindex <- predictions_metrics[predictions_metrics$cverror == min(predictions_metrics$cverror),"index"][1]
    print(paste("###########################################################################################################################################################remove the metric ",minindex,"##############################################################################################################################################################",sep=""))
    result_df[t,"metric"] <- minindex
    result_df[t,"cverror"] <- predictions_metrics[predictions_metrics$index == minindex,"cverror"]
    keepers[t] <-as.character(minindex)
    modelcols <- c(dependent,keepers)
    modeldata <- modeldata <- kmodeldata[names(kmodeldata) %in% modelcols]
    modeldata <- na.omit(modeldata)
    set.seed(seed)
    fit <- do.call("svm",list(as.formula(f),modeldata,cross=10))
    preds=predict(fit,newdata=ktestdata)
    prederror=mean(ktestdata[[dependent]] != preds)
    result_df[t,"geheimerprederror"] <- prederror
    print(paste("geheimerprederror = ",prederror,sep=""))
    testdatatable <- table(ktestdata[[dependent]])
    traindatatable<- table(kmodeldata[[dependent]])
    save(predictions_metrics,result_df,fit,keepers,traindatatable,testdatatable,file=paste(mydir,"/k",k,"_round_",t,".RData",sep=""))
    predictions_metrics <- predictions_metrics[predictions_metrics$index != as.character(minindex),]
    
  }
}
n=n+1
}






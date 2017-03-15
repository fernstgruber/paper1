
setwd("/home/fabs/Data/paper1_lenny/Rplots")
load("/home/fabs/Data/paper1_lenny/relieflegends.RData")
load("/home/fabs/Data/paper1_lenny/neu_unzugeordnet/FWCV/allmodeldataforSVM.RData")
load("/home/fabs/Data/paper1_lenny/neu_unzugeordnet/FWCV/makroreddata_andpredlists.RData")
load("/home/fabs/Data/paper1_lenny/neu_unzugeordnet/FWCV/mesoreddata_andpredlists.RData")
load("/home/fabs/Data/paper1_lenny/neu_unzugeordnet/FWCV/makrodata_andpredlists.RData")
load("/home/fabs/Data/paper1_lenny/neu_unzugeordnet/FWCV/mesodata_andpredlists.RData")
load("/home/fabs/Data/paper1_lenny/datawithpreds.RData")
load("/home/fabs/Data/paper1_lenny/modeldata_SuedundNordtirol.RData")

MGL1 <- datawithpreds[!(is.na(datawithpreds$c_MGL1)),]
MGL2 <- datawithpreds[!(is.na(datawithpreds$c_MGL2)),]
mGL1 <- datawithpreds[!(is.na(datawithpreds$c_mGL1)),]
mGL2 <- datawithpreds[!(is.na(datawithpreds$c_mGL2)),]
preds_MGL1_tn <- c("Topographic_Wetness_Index","profc_DTM_50m_avg_ws7","minic_DTM_50m_avg_ws5","slope_ws15")
MGL1_tn <- merge(x=MGL1,y=makroreddata[c("AufID","hillheight_vr1500_hr1000_t125","Texture","crosc_DTM_50m_avg_ws5","TPI_i0m_o400m_10m","Normalized_Height","TPI_i0m_o250m_10m","Topographic_Wetness_Index","profc_DTM_50m_avg_ws7","minic_DTM_50m_avg_ws5","slope_ws15")],by="AufID",all.x=T)
preds_mGL1_tn <-c("crosc_ws5","minic_ws15","slope_DTM_50m_avg_ws3","maxic_ws5")
mGL1_tn <- merge(x=mGL1,y=mesoreddata[c("AufID","slope_ws7","General_Curvature","TPI_i0m_o120m_10m","TPI_i0m_o50m_10m","crosc_ws5","minic_ws15","slope_DTM_50m_avg_ws3","maxic_ws5")],by="AufID",all.x=T)
MGL2_tn <- merge(x=MGL2,y=makrodata[c("AufID","TPI_i0m_o900m","minic_ws15","crosc_ws11","hillheight_vr1500_hr1500_t200")],by="AufID",all.x=T)
mGL2_tn <- merge(x=mGL2,y=mesodata[c("AufID","Topographic_Wetness_Index")],by="AufID",all.x=T)
#opar <- par()

##############################################################################################################################################################
##############################################################################################################################################################
##############################################################################################################################################################

#> levels(KZmakdata$KZ_mak)
#[1] "GE"  "GR"  "HVF" "HVS" "KU"  "MH"  "OH"  "PL"  "RU"  "SF"  "SK"  "TA"  "TE"  "UH" 
#pdf("Relmak_histograms.pdf")
par(pty="s")
par(mar=c(2,3,2,1))
par(mfcol=c(1,3))
plot(KZmakdata$KZ_mak,las=2,names=c("SV","CH","SF","SS","SU","BS","SH","PL","RI","DF","DC","VF","TE","FS"))
plot(makrodata_red$Def_red_mak,las=2,names=c("FL","BS","SH","RI","DA","LO","FS"))
plot(EZclassdata$EZ_class,las=2,names=c("FS","CH","BS","SH"))     
#dev.off()
par(opar)

par(mfcol=c(1,1))
#> levels(KZmesdata$KZ_mes)
#[1] "GE"  "GR"  "HF"  "HVF" "HVS" "KU"  "MH"  "MU"  "OH"  "RKG" "RU"  "SF"  "SK"  "TA"  "UH" 
#pdf("Relmes_histograms.pdf")
par(pty="s")
par(mar=c(2,3,2,1))
par(mfcol=c(1,3))
plot(KZmesdata$KZ_mes,las=2,names=c("SV","CH","TS","SF","SS","SU","BS","DE","SH","GR","RI","DF","DC","VF","FS"))
plot(mesodata_red$Def_red_mes,las=2,names=c("FL","SF","SS","BS","SH","RI","DA","LO","FS"))
plot(EZclassmesdata$EZ_class,las=2,names=c("FS","BS","SH"))     
#dev.off()


#pdf("Distribution_topopositions.pdf")
par(pty="s")
par(mar=c(2,3,2,1))
par(mfrow=c(2,3))
plot(KZmakdata$KZ_mak,las=2,names=c("SV","CH","SF","SS","SU","BS","SH","PL","RI","DF","DC","VF","TE","FS"))
plot(makrodata_red$Def_red_mak,las=2,names=c("FL","BS","SH","RI","DA","LO","FS"))
plot(EZclassdata$EZ_class,las=2,names=c("FS","CH","BS","SH")) 
plot(KZmesdata$KZ_mes,las=2,names=c("SV","CH","TS","SF","SS","SU","BS","DE","SH","GR","RI","DF","DC","VF","FS"))
plot(mesodata_red$Def_red_mes,las=2,names=c("FL","SF","SS","BS","SH","RI","DA","LO","FS"))
plot(EZclassmesdata$EZ_class,las=2,names=c("FS","BS","SH"))     
#dev.off()


#pdf("Distribution_topopositions_ohnetext.pdf")
par(pty="s")
par(mar=c(2,3,2,1))
par(mfrow=c(2,3))
plot(KZmakdata$KZ_mak,las=2,names=F)
plot(makrodata_red$Def_red_mak,las=2,names=c("FL","BS","SH","RI","DA","LO","FS"))
plot(EZclassdata$EZ_class,las=2,names=c("FS","CH","BS","SH")) 
plot(KZmesdata$KZ_mes,las=2,names=c("SV","CH","TS","SF","SS","SU","BS","DE","SH","GR","RI","DF","DC","VF","FS"))
plot(mesodata_red$Def_red_mes,las=2,names=c("FL","SF","SS","BS","SH","RI","DA","LO","FS"))
plot(EZclassmesdata$EZ_class,las=2,names=c("FS","BS","SH"))     
#dev.off()


####SVG
#svg(file="Distribution_topopositions.svg",onefile = T)
par(pty="s")

par(mfrow=c(2,3))
plot(KZmakdata$KZ_mak,las=2,names=c("SV","CH","SF","SS","SU","BS","SH","PL","RI","DF","DC","VF","TE","FS"))
plot(makrodata_red$Def_red_mak,las=2,names=c("FL","BS","SH","RI","DA","LO","FS"))
plot(EZclassdata$EZ_class,las=2,names=c("FS","CH","BS","SH")) 
plot(KZmesdata$KZ_mes,las=2,names=c("SV","CH","TS","SF","SS","SU","BS","DE","SH","GR","RI","DF","DC","VF","FS"))
plot(mesodata_red$Def_red_mes,las=2,names=c("FL","SF","SS","BS","SH","RI","DA","LO","FS"))
plot(EZclassmesdata$EZ_class,las=2,names=c("FS","BS","SH"))     
#dev.off()

#####Sued und nortirol
#svg("Def_red_mak_SUEDUNDNORD.svg")
par(mar=c(2,3,2,1))
par(mfrow=c(1,2))
plot(makrodata_red$Def_red_mak,las=2,names=c("FL","BS","SH","RI","DA","LO","FS"),ylim=c(0,600))
plot(NT_makrored$Def_red_mak,las=2,names=c("FL","BS","SH","RI","DA","LO","FS"),ylim=c(0,600))

#svg("barplot_correctclassification.svg",onefile = T,height=4,width=10)
par(mfrow=c(1,4))
barplot(table(factor(MGL1$c_MGL1,levels=0:6)),ylim=c(0,500))
barplot(table(factor(MGL2$c_MGL2,levels=0:6)),ylim=c(0,500))
barplot(table(factor(mGL1$c_mGL1,levels=0:6)),ylim=c(0,500))
barplot(table(factor(mGL2$c_mGL2,levels=0:6)),ylim=c(0,500))
#dev.off()

defredmakstack <- table(MGL1[MGL1$c_MGL1 == 0,"Def_red_mak"])
for (i in 1:6){
  defredmakstack <- rbind(defredmakstack,table(MGL1[MGL1$c_MGL1 == i,"Def_red_mak"]))
}
defredmesstack <- table(mGL1[mGL1$c_mGL1 == 0,"Def_red_mes"])
for (i in 1:6){
  defredmesstack <- rbind(defredmesstack,table(mGL1[mGL1$c_mGL1 == i,"Def_red_mes"]))
}
ezclassmesstack <- table(mGL2[mGL2$c_mGL2 == 0,"EZ_class_mes"])
for (i in 1:6){
  ezclassmesstack <- rbind(ezclassmesstack,table(mGL2[mGL2$c_mGL2 == i,"EZ_class_mes"]))
}
ezclassstack <- table(MGL2[MGL2$c_MGL2 == 0,"EZ_class"])
for (i in 1:6){
  ezclassstack <- rbind(ezclassstack,table(MGL2[MGL2$c_MGL2 == i,"EZ_class"]))
}
#svg("nrcorrectbarplot_Defredmak.svg")
barplot(defredmakstack,col=c("#d7191c","#ec6e43","#fdb96e","#fee7a4","#e7f5b7","#b7e1a7","#2b83ba"),las=2,border=F,ylim=c(0,600))
legend("topright",pch=15,legend=as.character(0:6),col=c("#d7191c","#ec6e43","#fdb96e","#fee7a4","#e7f5b7","#b7e1a7","#2b83ba"))
#dev.off()


#svg("nrcorrectbarplot_Defredmes.svg")
barplot(defredmesstack,col=c("#d7191c","#ec6e43","#fdb96e","#fee7a4","#e7f5b7","#b7e1a7","#2b83ba"),las=2,border=F,ylim=c(0,600))
#dev.off()


#svg("nrcorrectbarplot_ezclassmes.svg")
barplot(ezclassmesstack,col=c("#d7191c","#ec6e43","#fdb96e","#fee7a4","#e7f5b7","#b7e1a7","#2b83ba"),las=2,border=F,ylim=c(0,600))
#dev.off()


#svg("nrcorrectbarplot_ezclass.svg")
barplot(ezclassstack,col=c("#d7191c","#ec6e43","#fdb96e","#fee7a4","#e7f5b7","#b7e1a7","#2b83ba"),las=2,border=F,ylim=c(0,600))
#dev.off()


#svg("nrcorrectbarplot_colored.svg",height=4,width=10)
par(mfrow=c(1,4))
barplot(defredmakstack,col=c("#d7191c","#ec6e43","#fdb96e","#fee7a4","#e7f5b7","#b7e1a7","#2b83ba"),las=2,border=F,ylim=c(0,600))
barplot(ezclassstack,col=c("#d7191c","#ec6e43","#fdb96e","#fee7a4","#e7f5b7","#b7e1a7","#2b83ba"),las=2,border=F,ylim=c(0,600))
barplot(defredmesstack,col=c("#d7191c","#ec6e43","#fdb96e","#fee7a4","#e7f5b7","#b7e1a7","#2b83ba"),las=2,border=F,ylim=c(0,600))
barplot(ezclassmesstack,col=c("#d7191c","#ec6e43","#fdb96e","#fee7a4","#e7f5b7","#b7e1a7","#2b83ba"),las=2,border=F,ylim=c(0,600))
#dev.off()
######################################################
######################################################


par(opar)
n0 = nrow(MGL1_tn[MGL1_tn$c_MGL1== 0,])  #454
n6 = nrow(MGL1_tn[MGL1_tn$c_MGL1== 6,])  #398
#########################  MACRO GL1  #############
#svg("hillheight_vr1500_hr1000_t125_MGL1.svg")
par(mfrow=c(1,1))
boxplot(data=MGL1_tn,hillheight_vr1500_hr1000_t125~ c_MGL1,ylim=c(0,500),notch=T)
#dev.off()
#svg("Texture_MGL1.svg")
par(mfrow=c(1,1))
boxplot(data=MGL1_tn,Texture~ c_MGL1,ylim=c(0,1),notch=T)
#dev.off()
#svg("crosc_DTM_50m_avg_ws5_MGL1.svg")
par(mfrow=c(1,1))
boxplot(data=MGL1_tn,crosc_DTM_50m_avg_ws5~ c_MGL1,ylim=c(0,0.01),notch=T)
#dev.off()
#svg("TPI_i0m_o400m_10m_MGL1.svg")
par(mfrow=c(1,1))
boxplot(data=MGL1_tn,TPI_i0m_o400m_10m~ c_MGL1,ylim=c(-150,150),notch=T)
#dev.off()
#########################  MACRO GL2  #############
#svg("TPI_i0m_o900m_MGL2.svg")
par(mfrow=c(1,1))
boxplot(data=MGL2_tn,TPI_i0m_o900m~c_MGL2,ylim=c(-300,300),notch=T)
#dev.off()
#svg("minic_ws15_MGL2.svg")
par(mfrow=c(1,1))
boxplot(data=MGL2_tn,minic_ws15~c_MGL2,ylim=c(-0.03,0.02),notch=T)
#dev.off()
#svg("crosc_ws11_MGL2.svg")
par(mfrow=c(1,1))
boxplot(data=MGL2_tn,crosc_ws11~c_MGL2,ylim=c(-0.0400,0.0300),notch=T)
#dev.off()
#svg("hillheight_vr1500_hr1500_t200_MGL2.svg")
par(mfrow=c(1,1))
boxplot(data=MGL2_tn,hillheight_vr1500_hr1500_t200~c_MGL2,notch=T)
#dev.off()
#########################  MESO GL1  #############
#svg("slope_ws7_mGL1.svg")
par(mfrow=c(1,1))
boxplot(data=mGL1_tn,slope_ws7~c_mGL1,ylim=c(0,70),notch=T)
#dev.off()
#svg("General_Curvature_mGL1.svg")
par(mfrow=c(1,1))
boxplot(data=mGL1_tn,General_Curvature~c_mGL1,ylim=c(-0.1,0.1),notch=T)
#dev.off()
#svg("TPI_i0m_o120m_10m_mGL1.svg")
par(mfrow=c(1,1))
boxplot(data=mGL1_tn,TPI_i0m_o120m_10m~c_mGL1,ylim=c(0,70),notch=T)
#dev.off()
#########################  MESO GL2  #############
#svg("Topographic_Wetness_Index_mGL2.svg")
par(mfrow=c(1,1))
boxplot(data=mGL2_tn,Topographic_Wetness_Index~ c_mGL2,ylim=c(0,20),notch=T)
#dev.off()
######################              MGL1_correct_vs_wrong
#svg("correct vs. wrong and Texture vs. hillheight.svg")
par(mar=c(6,6,3,3))
plotdata <- MGL1_tn[MGL1_tn$c_MGL1 %in% c(0,6),]
plotdata[plotdata$c_MGL1 == 0,"col"] <- "red"
plotdata[plotdata$c_MGL1 == 6,"col"] <- "blue"
plot(y=plotdata$hillheight_vr1500_hr1000_t125,x=plotdata$Texture,col=plotdata$col,pch=3,ylab="hillheight",xlab="Texture")
legend("topleft",legend=c("always correct","always wrong"),pch=3,col=c("blue","red"),cex=0.5)
#dev.off()

#svg("correct vs. wrong and crosssectional curvature vs. normalized height.svg")
par(mar=c(6,6,3,3))
plotdata <- MGL1_tn[MGL1_tn$c_MGL1 %in% c(0,6),]
plotdata[plotdata$c_MGL1 == 0,"col"] <- "red"
plotdata[plotdata$c_MGL1 == 6,"col"] <- "blue"
plot(y=plotdata$Normalized_Height,x=plotdata$crosc_DTM_50m_avg_ws5,col=plotdata$col,pch=3,ylab="norm. Height",xlab="crosc")
legend("topleft",legend=c("always correct","always wrong"),pch=3,col=c("blue","red"),cex=0.5)
#dev.off()

#svg("correct vs. wrong and crosssectional curvature vs. Texture.svg")
par(mar=c(6,6,3,3))
plotdata <- MGL1_tn[MGL1_tn$c_MGL1 %in% c(0,6),]
plotdata[plotdata$c_MGL1 == 0,"col"] <- "red"
plotdata[plotdata$c_MGL1 == 6,"col"] <- "blue"
plot(y=plotdata$Texture,x=plotdata$crosc_DTM_50m_avg_ws5,col=plotdata$col,pch=3,ylab="Texture",xlab="crosc")
legend("topleft",legend=c("always correct","always wrong"),pch=3,col=c("blue","red"),cex=0.5)
#dev.off()

svg("correct vs. wrong and crosssectional hillheight vs. TPI_400m.svg")
par(mar=c(6,6,3,3))
plotdata <- MGL1_tn[MGL1_tn$c_MGL1 %in% c(0,6),]
plotdata[plotdata$c_MGL1 == 0,"col"] <- "red"
plotdata[plotdata$c_MGL1 == 6,"col"] <- "blue"
plot(y=plotdata$hillheight_vr1500_hr1000_t150,x=plotdata$TPI_i0m_o400m_10m,col=plotdata$col,pch=3,ylab="hillheight",xlab="TPI")
legend("topleft",legend=c("always correct","always wrong"),pch=3,col=c("blue","red"),cex=0.5)
#dev.off()


#svg("MGL1 correct vs. wrong and tpi vs. minic.svg")
par(mar=c(6,6,3,3))
plotdata <- MGL2_tn[MGL2_tn$c_MGL2 %in% c(0,6),]
plotdata[plotdata$c_MGL2 == 0,"col"] <- "red"
plotdata[plotdata$c_MGL2 == 6,"col"] <- "blue"
str(plotdata$c_MGL2)
plot(plotdata$TPI_i0m_o900m,plotdata$minic_ws15,col=plotdata$col,pch=3)
legend("bottomright",legend=c("always correct","always wrong"),pch=3,col=c("blue","red"))
#dev.off()
######################              mGL1_correct_vs_wrong
#svg("MESO correct vs. wrong and Texture vs. hillheight.svg")
par(mar=c(6,6,3,3))
plotdata <- mGL1_tn[mGL1_tn$c_mGL1 %in% c(0,6),]
plotdata[plotdata$c_mGL1 == 0,"col"] <- "red"
plotdata[plotdata$c_mGL1 == 6,"col"] <- "blue"
plot(y=plotdata$slope_ws7,x=plotdata$TPI_i0m_o120m_10m,col=plotdata$col,pch=3,ylab="slope",xlab="TPI")
legend("topleft",legend=c("always correct","always wrong"),pch=3,col=c("blue","red"),cex=0.5)
#dev.off()

#################################################################testing 3d plots
require(scatterplot3d)
plotdata$pcol[plotdata$col == "0"] <-"red"
plotdata$pcol[plotdata$col == "6"] <-"blue"

with(plotdata, {
  scatterplot3d(hillheight_vr1500_hr1000_t125,Texture,  TPI_i0m_o400m_10m,        # x y and z axis
                color=pcol, pch=19, # filled blue circles
                type="h",lty.hplot=2,# lines to the horizontal plane
                scale.y=1.0,
                main="Macro scale ",
                xlab="Texture",
                ylab="Hillheight",
                zlab="TPI")
})


plotdata <- MGL1_tn[MGL1_tn$c_MGL1 %in% c(0,6),]
plotdata$col <- as.factor(plotdata$c_MGL1)
predictors=c("Texture","hillheight_vr1500_hr1000_t125", "Normalized_Height")
plot(plotdat)
#######################################################################################
##plot for stepwise forward selection; ALSO TEST IF ONE_SE_RULE IS IMPLEMENTED RIGHTLY
#svg("stepwiseselection.svg")
mypath="/home/fabs/Data/paper1_lenny/neu_unzugeordnet/results_selection/svm_fw_10fold_10p_Def_red_mak_allterrain/";kk = 1:10;endround = 10;yrange = c(0.4,0.6);error="cverror";geheim="geheimerprederror"
xrange <- c(1,endround)
yrange=yrange
plot(xrange,yrange,type="n",xlab="steps",ylab="prediction-error")
colors=rainbow(max(kk))
vcpreds <- data.frame(1:endround)
for (k in kk){ #kk is cv times, tt=number of parameters
  load(file=paste(mypath,"/k",k,"_round_",endround,".RData",sep=""))
  lines(result_df$tt,result_df[[error]],type="b",col=colors[k],lty=2) # stellen den cv-error der 90% dar
  vcpreds[[k]] <- result_df[[geheim]] #das ergibt eine spalte mit dem geheimen prederror
}
vcpreds["meanprederror"] <- apply(vcpreds[,1:10],1,mean,na.rm=TRUE)
vcpreds["sum"] <- apply(vcpreds[,1:10],1,sum)
vcpreds["standard.error"] <- apply(vcpreds[,1:10],1,function(x) { sd(x)/sqrt(length(kk))})
vcpreds["se_lower"] <- vcpreds$meanprederror - 2*vcpreds$standard.error
vcpreds["se_upper"] <- vcpreds$meanprederror + 2*vcpreds$standard.error
cn <- ncol(vcpreds)
lines(1:endround,vcpreds$meanprederror,type="b",col="black",lwd=3)
#lines(1:endround,vcpreds$se_lower,type="l",col="black",lty=1)
#lines(1:endround,vcpreds$se_upper,type="l",col="black",lty=1)
one.se.rule <- vcpreds[vcpreds$meanprederror==min(vcpreds$meanprederror),"standard.error"] + min(vcpreds$meanprederror)
lines(x=1:endround,y=rep(one.se.rule,times=endround),lwd=1)
#dev.off()
####################################################################SLOPE VS SLOPE
load("/home/fabs/Data/paper1_lenny/neu_unzugeordnet/FWCV_NORDTIROL/relieflegends.RData")
load("/home/fabs/Data/paper1_lenny/neu_unzugeordnet/FWCV_NORDTIROL/mesomakrolegends.RData")
load("/home/fabs/Data/paper1_lenny/modeldata_SuedundNordtirol.RData")
ST_makrored$neigunggrad <- atan(ST_makrored$Neig/100)*180/pi
NT_makrored$neigunggrad <- atan(NT_makrored$Neig/100)*180/pi

fit <- lm(data=ST_makrored,neigunggrad~slope_ws3)
print(summary(fit)$r.squared)
fit <- lm(data=ST_makrored,neigunggrad~slope_ws5)
print(summary(fit)$r.squared)
fit <- lm(data=ST_makrored,neigunggrad~slope_ws7)
print(summary(fit)$r.squared)
fit <- lm(data=ST_makrored,neigunggrad~slope_ws11)
print(summary(fit)$r.squared)
fit <- lm(data=ST_makrored,neigunggrad~slope_ws15)
print(summary(fit)$r.squared)
###best result is with ws5 = 50m
fit <- lm(data=ST_makrored,neigunggrad~slope_ws5)
print(summary(fit)$r.squared)
#svg("slope_vs_slope.svg")
plot(ST_makrored$slope_ws5,ST_makrored$neigunggrad,pch=3,cex=0.5,xlim=c(0,65),ylim=c(0,65))
lines(x=fit$model$slope_ws5,y=fit$fitted.values)
lines(x=c(0,65),y=c(0,65),col="red")
#dev.off()
########################################################################################TERRAIN PARAMETER STATS FOR TERRAIN PREDICTIONS OF TEST SET
##########################MGL1
require(e1071)
makromodeldata <- ST_makrored[c("Def_red_mak",preds_MGL1_tn)]
svm_terrain_MGL1 <- svm(data=makromodeldata,Def_red_mak~. , cross=10) 
print(summary(svm_terrain_MGL1)$tot.accuracy )
makromodeldata$preds  <-  predict(svm_terrain_MGL1,newdata=makromodeldata)
makromodeldata <- merge(makromodeldata,makrolegend_gen1)
summary(makromodeldata$preds)

svg("TWI_datapoints_MGL1_2.svg")
boxplot(Topographic_Wetness_Index~Def_red_mak,data=makromodeldata,las=2,aspect=0.1)
dev.off()
svg("TWI_datapoints_MGL1_predicted_2.svg"r)
boxplot(Topographic_Wetness_Index~preds,data=makromodeldata,las=2,aspect=0.1)
dev.off()
##
svg("PROFC_datapoints_MGL1.svg")
boxplot(profc_DTM_50m_avg_ws7~Def_red_mak,data=makromodeldata,las=2)
dev.off()
svg("PROFC_datapoints_MGL1_predicted.svg")
boxplot(profc_DTM_50m_avg_ws7~preds,data=makromodeldata,las=2)
dev.off()
##
svg("MINIC_datapoints_MGL1.svg")
boxplot(minic_DTM_50m_avg_ws5~Def_red_mak,data=makromodeldata,las=2)
dev.off()
svg("MINIC_datapoints_MGL1_predicted.svg")
boxplot(minic_DTM_50m_avg_ws5~preds,data=makromodeldata,las=2)
dev.off()
##
svg("SLOPE_datapoints_MGL1.svg")
boxplot(slope_ws15~Def_red_mak,data=makromodeldata,las=2)
dev.off()
svg("SLOPE_datapoints_MGL1_predicted.svg")
boxplot(slope_ws15~preds,data=makromodeldata,las=2)
dev.off()
##################mGL1
require(e1071)
mesomodeldata <- ST_mesored[c("Def_red_mes",preds_mGL1_tn)]
svm_terrain_mGL1 <- svm(data=mesomodeldata,Def_red_mes~. , cross=10) 
print(summary(svm_terrain_mGL1)$tot.accuracy )
mesomodeldata$preds  <-  predict(svm_terrain_mGL1,newdata=mesomodeldata)
summary(mesomodeldata$preds)

svg("CROSC_datapoints_mGL1.svg")
boxplot(crosc_ws5~Def_red_mes,data=mesomodeldata,las=2)
dev.off()
svg("CROSC_datapoints_mGL1_predicted.svg")
boxplot(crosc_ws5~preds,data=mesomodeldata,las=2)
dev.off()
##
svg("MINIC_datapoints_mGL1.svg")
boxplot(minic_ws15~Def_red_mes,data=mesomodeldata,las=2)
dev.off()
svg("MINIC_datapoints_mGL1_predicted.svg")
boxplot(minic_ws15~preds,data=mesomodeldata,las=2)
dev.off()
##
svg("MAXIC_datapoints_mGL1.svg")
boxplot(maxic_ws5~Def_red_mes,data=mesomodeldata,las=2)
dev.off()
svg("MAXIC_datapoints_mGL1_predicted.svg")
boxplot(maxic_ws5~preds,data=mesomodeldata,las=2)
dev.off()
##
svg("SLOPE_datapoints_mGL1.svg")
boxplot(slope_DTM_50m_avg_ws3~Def_red_mes,data=mesomodeldata,las=2)
dev.off()
svg("SLOPE_datapoints_mGL1_predicted.svg")
boxplot(slope_DTM_50m_avg_ws3~preds,data=mesomodeldata,las=2)
dev.off()
##
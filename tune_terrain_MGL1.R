
require(e1071)
load("/media/fabs/Volume/01_PAPERZEUG/paper1_lenny/modeldata_SuedundNordtirol.RData")
preds_MGL1_tn <- c("Topographic_Wetness_Index","profc_DTM_50m_avg_ws7","minic_DTM_50m_avg_ws5","slope_ws15")
preds_mGL1_tn <-c("crosc_ws5","minic_ws15","slope_DTM_50m_avg_ws3","maxic_ws5")
makromodeldata <- ST_makrored[c("Def_red_mak",preds_MGL1_tn)]
mesomodeldata <- ST_mesored[c("Def_red_mes",preds_mGL1_tn)]

costs=c(-5,-3,-1,0,1,3,5,7,9,11,13)
gammas=c(-15,-13,-11,-9,-7,-5,-3,-1,0,1,2,3)
resultmatrix_MGL1 <- data.frame(matrix(ncol=length(costs),nrow=length(gammas)))
names(resultmatrix_MGL1) <- costs
row.names(resultmatrix_MGL1) <- gammas
nc=1
cost=2^costs[1]
for(cost in 2^costs){
  ng=1
  g=2^gammas[1]
  for(g in 2^gammas){
    fit <-  svm(data=makromodeldata,Def_red_mak~. , cross=10,gamma=g,cost=cost) 
    resultmatrix_MGL1[ng,nc] <- summary(fit)$tot.accuracy
    print(paste("cv_accuracy with cost=",cost," and gamma=",g," is: ",summary(fit)$tot.accuracy),sep="")
    ng=ng+1
  }
  nc=nc+1
}

costs=c(-5,-3,-1,0,1,3,5,7,9,11,13)
gammas=c(-15,-13,-11,-9,-7,-5,-3,-1,0,1,2,3)
resultmatrix_mGL1 <- data.frame(matrix(ncol=length(costs),nrow=length(gammas)))
names(resultmatrix_mGL1) <- costs
row.names(resultmatrix_mGL1) <- gammas
nc=1
cost=2^costs[1]
for(cost in 2^costs){
  ng=1
  g=2^gammas[1]
  for(g in 2^gammas){
    fit <-  svm(data=mesomodeldata,Def_red_mes~. , cross=10,gamma=g,cost=cost) 
    resultmatrix_mGL1[ng,nc] <- summary(fit)$tot.accuracy
    print(paste("cv_accuracy with cost=",cost," and gamma=",g," is: ",summary(fit)$tot.accuracy),sep="")
    ng=ng+1
  }
  nc=nc+1
}
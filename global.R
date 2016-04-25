
#Packages
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("zoo")
usePackage("FactoMineR") #PCA
usePackage("missMDA")#imputepca
usePackage("subselect")
usePackage("ggplot2")#Graphs
usePackage("stats")
usePackage("e1071")#svm
usePackage("pROC")#roccurve
usePackage("devtools")
usePackage("readxl")
usePackage("shiny")
usePackage("plyr")
# if (!is.element("factoextra", installed.packages()[,1]))
#   install_github("kassambara/factoextra")
usePackage("factoextra")#PCA graphs
usePackage("reshape2")#melt function
usePackage("xlsx")#import fichier xls#Fonctions
usePackage("randomForest")
usePackage("missForest")
usePackage("Hmisc")
usePackage("corrplot")

##########################
importfile<-function (datapath,extension,NAstring="NA",sheet=1,skiplines=0,dec=".",sep=","){
  # datapath: path of the file
  #extention: extention of the file : csv, xls, ou xlsx
  if(extension=="csv"){
    toto <- read.csv2(datapath,header = F,sep =sep,dec=dec,na.strings = NAstring,stringsAsFactors = F)
  }
  if(extension=="xlsx"){
    options(warn=-1)
      file.rename(datapath,paste(datapath, ".xlsx", sep=""))
    options(warn=0)
    toto <-read_excel(paste(datapath, ".xlsx", sep=""),na=NAstring,col_names = F,skip = skiplines,sheet = 1)
    
  }
  toto<-as.data.frame(toto)
  return(toto)
}
downloaddataset<-function(x,file){
  ext<-strsplit(x = file,split = "[.]")[[1]][2]
  if(ext=="csv"){
    write.csv2(x,file)
  }
  if(ext=="xlsx"){
    write.xlsx(x,file)
  }
  
}

downloadplot<-function(file){
  ext<-strsplit(x = file,split = "[.]")[[1]][2]
  
  if(ext=="png"){
    png(file)
  }
  if(ext=="jpg"){
    jpeg(file)
  }  
  if(ext=="pdf"){
    pdf(file) 
  }     
}
renamvar<-function(names){
  #rename the duplicate name by adding ".1, .2 ....
  #toto is a vector of the col names of the tab
  names[is.na(names)]<-"NA"
  for(i in 1:length(names)){
    ind <- which(names%in%names[i])
    if(length(ind)>1){
      nb<-c(1:length(ind))
      newnames<-paste(names[ind],".",nb,sep="")
      
      names[ind]<-newnames
    }
  }
  return(names)
}

transformdata<-function(toto,nrownames=1,ncolnames=1,transpose,zeroegalNA,log){
  if(length(which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)))!=0){
    toto<-toto[-which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)),]}
  #remove empty rows
  if(length(which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto)))!=0){
    toto<-toto[,-which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto))]}
  #remove empty columns
  
  if(ncolnames!=0){
      colnames(toto)<-renamvar(toto[ncolnames,])
      toto<-toto[-ncolnames,]
  }
  if(nrownames!=0){
      rownames(toto)<-renamvar(toto[,nrownames])
      toto<-toto[,-nrownames]
  }
  if(transpose){toto<-t(toto)}
  
  if(zeroegalNA){toto[which(toto==0,arr.ind = T)]<-NA}
  
  toto<-as.data.frame(toto)
 
}
confirmdata<-function(toto){
  toto<-as.data.frame(toto)
toto[,1]<-as.factor(as.character(toto[,1]))
for (i in 2:ncol(toto)){
  toto[,i]<-as.numeric(as.character(toto[,i]))
}
return(toto)
}

replaceproptestNA<-function(toto,threshold=0.05,rempNA,maxvaluesgroupmin=100,minvaluesgroupmax=0,replacezero=T){
  class<-toto[,1]
  resproptest<-proptestNA(toto=toto)
  vecond<-c(resproptest$pval<=threshold & resproptest$prctless<=(maxvaluesgroupmin/100) & resproptest$prctmore>=(minvaluesgroupmax/100))
  if(sum(vecond)>0){
    resp<-resproptest[vecond,]
    totopropselect<-data.frame(toto[,vecond])
    colnames(totopropselect)<-resp$names
    #print(heatmapNA(totopropselect[, order(resp[,2])],names = paste(class,1:length(class)))            )
    #rempNA des 0
    if(replacezero){
    for (i in 1:ncol(totopropselect)){
      totopropselect[which(is.na(totopropselect[,i])&class==as.character(resp[i,"lessgroup"])),i]<-0
    }
    totopropselect<-as.data.frame(replaceNA(toto =cbind(class,totopropselect),rempNA = rempNA )[,-1])
  }
  }
  else{return(NULL)}
  totopropselect<-as.data.frame(totopropselect[, order(resp[,2])])
  colnames(totopropselect)<-resp$names[order(resp[,2])]
  return(totopropselect)
}

replaceNA<-function(toto,rempNA="z",pos=F,NAstructure=F,thresholdstruct=0.05,maxvaluesgroupmin=100,minvaluesgroupmax=0){ 
  #rempNA: remplace Non ATtributes values by zero("z"), the mean of the colum (moy), 
  # the mean in each group define by the factor of the first column(moygr), itarative pca (pca), or keep th NA
  if(NAstructure){
    totoNAstruct<-replaceproptestNA(toto = toto,threshold = thresholdstruct ,rempNA =rempNA,maxvaluesgroupmin,minvaluesgroupmax)
    toto[,colnames(totoNAstruct)]<-totoNAstruct
  }
  
  if (rempNA == "none" | sum(is.na(toto))==0 ) {return(toto)}
  cnames<-colnames(toto)
  class<-(toto[,1])
  cat<-levels(class)
  toto<-as.data.frame(toto[,-1],optional = T)
  #toto<-apply(toto,MARGIN = 2,function(x)as.numeric(x))
  n<-ncol(toto) 
  #par default je remplace les NA par 0
  if (rempNA == "z") {
    toto[which(is.na(toto),arr.ind = T)]<-0
  }
  if (rempNA== "moy") {
    toto<-na.aggregate(toto)}
  if(rempNA=="moygr"){
    
    for (i in 1:length(cat)){
      tab<-toto[which(class==cat[i]),]
      tab<-na.aggregate(tab)
      toto[which(class==cat[i]),]<-tab
    }
    toto[which(is.na(toto) ,arr.ind = T )]<-0
  }
  if (rempNA == "pca"){
    
    #prise en compte des liaisons entre variable et de la ressemblance entre individus    
    #nb<-estim_ncpPCA(toto[,(nbqualisup+1):n],ncp.min = 0,ncp.max = 10,method.cv = "Kfold")    #take a lot time
    nindiv<-nrow(toto)
    prctnacol<-apply(X = toto,MARGIN = 2,FUN=function(x){ if(sum(is.na(x))/nindiv==1){x<-rep(0,length=nindiv)}
      else{x}})
    toto<-imputePCA(toto,ncp = min(n-1,5),method.cv="Kfold")$completeObs
    if(pos){toto[which(toto<0,arr.ind = T)]<-0}
    toto<-as.data.frame(toto)
    
  }
  if(rempNA=="missforest"){
    toto<-missForest(toto,maxiter = 5)$ximp
    if(pos){toto[which(toto<0,arr.ind = T)]<-0}
  }
  
  toto<-cbind(class,toto)
  toto[which(is.na(toto),arr.ind = T)]<-0
  
  colnames(toto)<-cnames
  
  return(toto)
}

AppPCA<-function(toto,nbaxe=5,rempNA="z"){
  
  n<-ncol(toto)
  toto<-replaceNA(toto = toto,rempNA = rempNA) #PCA needs too file the NA
  
  if(nbaxe==0){nbaxe<-estim_ncpPCA(toto[,2:n],ncp.min = 1,ncp.max = 5,method.cv ="Kfold",nbsim=10 )[[1]]}
  #estime the best number of axes
  resPCA<-PCA(X = toto,ncp=nbaxe, quali.sup = 1,graph = F)
  
  return(resPCA)
}

selectprctNA<-function(toto,prctNA=100,group=F,restrictif=F){ 
  n<-ncol(toto)
  if (!group){
    NAvec<-vector(length =max(n,0) )
    for(i in 1:n){
      NAvec[i]<-  (sum(is.na(toto[,i]))/nrow(toto)  ) 
    }
    vec<-(NAvec<=(prctNA/100))
    
  } 
  
  if(group){
    nbcat<-length(levels(toto[,1]))
    tabgroup<-matrix(nrow = nbcat, ncol=n )
    for(i in 1:nbcat){
      tab<-toto[which(toto[,1]==levels(toto[,1])[i]),]
      for(j in 1:(n) ){
        tabgroup[i,j]<-(sum(is.na(tab[,j]))/nrow(tab))  
      }  
    }
    if(!restrictif){
    vec<-apply(X = tabgroup,MARGIN = 2,FUN = function(x){as.logical(max (x <= (prctNA/100))) }) 
    }
    if(restrictif){
      vec<-apply(X = tabgroup,MARGIN = 2,FUN = function(x){as.logical(min (x <= (prctNA/100))) }) 
    }
  }
  toto<-toto[,as.logical(vec)]
}

selectvar<-function(resPCA,toto){
  #select variables which are correlate to the axes correlate to the cotegorial variable of the first column
  restri<-dimdesc(resPCA,axes = c(1:(min(ncol(toto),10)-1)) )
  varquali<-vector()
  score<-0
  #restri is a dimdesc data
  for (i in 1:length(restri)){
    if ( !is.null(restri[[i]]$quali ) ) {
      score<-score+restri[[i]]$quali[[1]]
      varquali<-c(varquali,row.names(restri[[i]]$quanti))
    }
  }
  #score<-1- ( ( (1+score)*(nrow(toto)-1) )/(nrow(toto)-ncol(toto)-1) )
  return(list("varquali"=varquali,"score"=score))
}

selectind<-function(resPCA,toto){
  #select particular individual, by clustering with the coordinates in pca
  #if an individual is alone in a group, he is definied as particular
  hcpc<-HCPC(resPCA,nb.clust = 0,consol=T,iter.max=10,graph = F,max=4)
  partind<-row.names(toto[which(hcpc$data.clust$clust %in% which( table(hcpc$data.clust$clust)==1)),])
  return(partind)
}


selectdata<-function(toto){
  #remove variable  with less than 2 value and replace 0 by NA
  n<-ncol(toto)
  toto[which(toto==0 ,arr.ind = T )]<-NA
  vec<-rep(T,length=n)
  for(i in 2:n){
    vec[i]<-( (length(unique(toto[,i]))>2) )
  }
  #rm var with less than 3 values (0 or NA , and 2 other (important for the rempNA PCA))
  toto<-toto[,as.logical(vec)]
  return(toto)
}


testObject <- function(object){
  #test if the object is in the global environnement
  exists(as.character(substitute(object)))
}

distributionNA<-function(toto,prctNAselect,nvar,maintitle="Number of variables according to\nthe % of NA's selected",graph=T,ggplot=T){
  #drow the NA distribution by variables
  
  #
  percentageNA<-seq(0,1,by = 0.01)
  prctall<-apply(X = toto,MARGIN = 2,FUN = function(x){sum(is.na(x))})/nrow(toto)
  NAwhithoutgroup<-sapply(X = percentageNA,FUN = function(x,prct=prctall){sum(x>=prct)})
  prctlev1<-apply(X = toto[which(toto[,1]==levels(toto[,1])[1]),],MARGIN = 2,FUN = function(x){sum(is.na(x))})/nrow(toto[which(toto[,1]==levels(toto[,1])[1]),])
  prctlev2<-apply(X = toto[which(toto[,1]==levels(toto[,1])[2]),],MARGIN = 2,FUN = function(x){sum(is.na(x))})/nrow(toto[which(toto[,1]==levels(toto[,1])[2]),])
  
  NApergroupmin<-sapply(X = percentageNA,FUN = function(x,prct1=prctlev1,prct2=prctlev2){sum(x>=apply(rbind(prct1,prct2),2,min))})  
  NApergroupmax<-sapply(X = percentageNA,FUN = function(x,prct1=prctlev1,prct2=prctlev2){sum(x>=apply(rbind(prct1,prct2),2,max))})  

  distribNA<<-data.frame("percentageNA"=percentageNA,"whithout groups"=NAwhithoutgroup,"both groups"= NApergroupmax,"at least one group"=NApergroupmin)
  if(!graph)(return(distribNA))
  col<-gg_color_hue(ncol(distribNA)-1)
  if(!ggplot){
    matplot(x=distribNA$percentageNA,distribNA[,-1],type=c("l","l"),lty = c(1,1,1),
            col=c("red","green","blue"), xlab="percentage of NA selected",ylab="Number of variables",main=maintitle)
    legend("bottomright",colnames(distribNA[,-1]),col=c("red","green","blue"),lty=1)
    abline(v = prctNAselect,lty=3,col="grey")
    abline(h = nvar,lty=3,col="grey")
  }
  if (ggplot){
  distribNAlong<- melt(distribNA,id.vars = "percentageNA",variable.name = "select_method",value.name = "number_of_variables")  # convert to long format
  p<-ggplot(data=distribNAlong,
            aes(x=percentageNA, y=number_of_variables, colour=select_method)) +geom_line()+
    ggtitle(maintitle)
  p+theme(plot.title=element_text( size=15),legend.text=element_text(size=10),legend.title=element_text(color = 0),legend.position=c(0.75,0.15))+
  geom_vline(xintercept=prctNAselect,linetype=3)+
  geom_hline(yintercept=nvar,linetype=3)
  }
}
boxplotNAgroup<-function(toto,maintitle="percentage of NA in variable\nseparate by group",ggplot=T,graph=T){
  prctlev1<-apply(X = toto[which(toto[,1]==levels(toto[,1])[1]),],MARGIN = 2,FUN = function(x){sum(is.na(x))})/nrow(toto[which(toto[,1]==levels(toto[,1])[1]),])
  prctlev2<-apply(X = toto[which(toto[,1]==levels(toto[,1])[2]),],MARGIN = 2,FUN = function(x){sum(is.na(x))})/nrow(toto[which(toto[,1]==levels(toto[,1])[2]),])
  if(!graph)(return(rbind(prctlev1,prctlev2)))
  if(!ggplot){boxplot(prctlev1,prctlev2,col=c("red","blue"),names=c(levels(toto[,1])[1],levels(toto[,1])[2]),main=maintitle)}
  if(ggplot){
    data<-data.frame("group"=rep(c(levels(toto[,1])[2],levels(toto[,1])[2]),each=length(prctlev1)),"prctNA"=c(prctlev1,prctlev2))
    p<-ggplot(data, aes(x=group, y=prctNA, fill=group)) +
      geom_boxplot()+
      ggtitle(maintitle)+theme(plot.title=element_text( size=15))
    p    
    }
}
heatmapNA<-function(toto,maintitle="Distribution of NA",names=NULL,graph=T){
  if(!graph){ return(toto)}
  if(graph){
  if(is.null(toto)){errorplot(text = " No input Data")}
  else{
  tab<-toto
  tab[which(!is.na(tab) ,arr.ind = T )]<-"Value"
  tab[which(is.na(tab) ,arr.ind = T )]<-"NA"
  tab<-cbind(paste(toto[,1],1:length(toto[,1])),tab)
  tab<-apply(tab,2,as.factor)
  rownames(tab)<-names
  tabm <- melt(tab)
  tabm<-tabm[-c(1:nrow(toto)),]
  colnames(tabm)<-c("individual","variables","value")
  if(ncol(toto)>60){
    ggplot(tabm, aes(variables, individual)) + geom_tile(aes(fill = value)) + scale_fill_manual(values=c("lightgrey","steelblue"))+ 
      ggtitle("Distribution of NA") + theme(plot.title = element_text(size=15),axis.text.x=element_blank())
  }
  else{
    ggplot(tabm, aes(variables, individual)) + geom_tile(aes(fill = value), colour = "white") + scale_fill_manual(values=c("lightgrey","steelblue"))+ 
      ggtitle(maintitle) + theme(plot.title = element_text(size=15),axis.text.x=element_blank())
  }
  }
  }
}
heatmapNAstructure<-function(toto,threshold){
    resproptest<-proptestNA(toto=to)
    if(sum(resproptest$pval<=threshold)==0){errorplot(text = " No NA's structure")}
    else{
        totopropselect<<-as.data.frame(tabdecouv[,which(resproptest$pval<=threshold)])
        resproptest2<<-resproptest[which(resproptest$pval<=threshold),]
        #resproptest2$lessgroup<<-as.factor(as.character(resproptest2$lessgroup))
        heatmapNA(toto = as.data.frame(totopropselect[, order(resproptest2[,2])]),names=paste(tabdecouv[,1],1:length(tabdecouv[,1])))
    }
}

heatmapplot<-function(toto,nbclass=0,ggplot=T,maintitle="Heatmap of the transform data ",scale=F,graph=T){
  
  row.names(toto)<-paste(toto[,1],1:length(toto[,1]))
  toto<-as.matrix(toto[,-1])
  colnames(toto)<-seq(1:ncol(toto))
  if(scale)toto<-scale(toto, center = F, scale = TRUE)
  if(nbclass>0){
    quant<-quantile(toto,probs=seq(0,1,length=nbclass+1))
  }
  if(!graph){return(toto)}
  if(!ggplot){
    if (nbclass==0){
      heatmap.2(toto,Rowv = NA,Colv=F,trace="none",dendrogram = "none",key=T,margins=c(2,4),keysize=1.30,main=maintitle)
    }
    else{
      colr<-heat.colors(nbclass)
      heatmap.2(toto,Rowv = NA,Colv=F,trace="none",dendrogram = "none",key=F,keysize = 0.65,main=maintitle,cexRow=0.75, cexCol=0.75,labCol =NA,
                col=colr,breaks=quantile(toto,probs=seq(0,1,length=nbclass+1)))
      legend.col(col =colr, lev = toto)
    }
  }
  if(ggplot){
    titi<-melt(toto,value.name = "Intensity")
    colnames(titi)<-c("Individuals","Variables","Intensity")
    if(nbclass>0)titi$Intensity<-as.numeric(as.character(cut(titi$Intensity,breaks =  unique(quant), include.lowest = TRUE,labels = 1:(length(unique(quant))-1 ))))
    
    ggplot(titi, aes( Variables, Individuals,fill = Intensity),colour=NA) + geom_raster()+ggtitle(maintitle)+theme(plot.title=element_text( size=15))
  }
}
mdsplot<-function(toto,ggplot=T,maintitle="MDS representation of the individuals",graph=T){
  class<-toto[,1]
  toto<-toto[-1]
  d <- dist(toto) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  x <- fit$points[,1]
  y <- fit$points[,2] 
  coord<-(data.frame("class"=class,x,y))
  if(!graph){return(coord)}
  if(!ggplot){
    colr<-c("red","blue")
    
    plot(x, y, xlab="", ylab="",pch=20,main=maintitle, type="p",col=c(rep(colr[1],times=15),rep(colr[2],times=34) ))
    text(x, y, labels = row.names(toto), cex=.7,col=c(rep(colr[1],times=15),rep(colr[2],times=34) ))
    legend("topleft",legend=levels(class),text.col = colr)
  }
  #MDS ggplot
  if(ggplot){
    p <- ggplot(coord, aes(x, y,label=rownames(toto)))
    p + geom_text(aes(colour = class))+ggtitle(maintitle)+theme(plot.title=element_text( size=15))
  }
}
diffexptest<-function(toto,test="Wtest",adjustpval=F){ 
#fonction test if the variables (in column) of toto (dataframe) are differently 
#expressed according to the first variable (first column) (two groups : OP Tem)
  #test= Ttes: porsuit a sTudent test for each column (parmetric test), the sample have to be normal and with the same variance
        #Wtest : willcoxon test (nonparametric), assume that dispersion a on the same scale
  x<-toto[,1]
  toto<-toto[,-1]
  pval<-vector()
  mlev1<-vector()
  mlev2<-vector()
  difNA<-vector()#idea see the diff of %NA between the two groups
  FC<-vector()

  for (i in 1:max(1,ncol(toto)) ){
    lev1<-toto[which(x==levels(x)[1]),i]
    lev2<-toto[which(x==levels(x)[2]),i]
    mlev1[i]<-mean(lev1,na.rm = T)+0.0001
    mlev2[i]<-mean(lev2,na.rm = T)+0.0001
    
    FC[i]<-mlev1[i]/mlev2[i]
    if( test=="Ttest"){pval[i]<-t.test(x = lev1,y = lev2)$p.value}
    else if( test=="Wtest"){pval[i]<-wilcox.test(lev1 ,lev2,exact = F)$p.value } 
  } 
  if (adjustpval==T){ 
    pval<-p.adjust(pval, method = "BH")}
  logFC<-log2(abs(FC))
  pval[which(is.na(pval))]<-1 
  listgen<-data.frame(colnames(toto),pval,FC,logFC,mlev1,mlev2) 
  colnames(listgen)<-c("nom","pval","FC","logFC",levels(x)[1],levels(x)[2]) 
  return(listgen)
} 
conditiontest<-function(toto,shaptest=T,Ftest=T,threshold=0.05){
  x<-toto[,1]
  toto<-toto[,-1]
  pvalF<-vector()
  pvalnormlev1<-vector()
  pvalnormlev2<-vector()
  vlev1<-vector()
  vlev2<-vector()
  samplenorm<-vector()
  varequal<-vector()
  conditiontest<-data.frame("name"=colnames(toto))
  for (i in 1:ncol(toto) ){
    lev1<-toto[which(x==levels(x)[1]),i]
    lev2<-toto[which(x==levels(x)[2]),i]
    if(shaptest){
      #pvalnormTem[i]<-shapiro.test(Tem)$p.value
      
      out<- tryCatch(shapiro.test(lev1)$p.value, error = function(e) e)
      if(any(class(out)=="error"))pvalnormlev1[i]<-1
      else{pvalnormlev1[i]<-out}
      
      out<- tryCatch(shapiro.test(lev2)$p.value, error = function(e) e)
      if(any(class(out)=="error"))pvalnormlev2[i]<-1
      else{pvalnormlev2[i]<-out}
      
      if((pvalnormlev2[i]>=threshold) & (pvalnormlev1[i]>=threshold)){samplenorm[i]<-"norm"}
      else{samplenorm[i]<-"notnorm"}
    }
    if(Ftest){
      #to perform a fisher test the value have to be normal
      pvalF[i]<-var.test(lev1,lev2)$p.value
      if(is.na(pvalF[i]))pvalF[i]<-1
      vlev1[i]<-var(lev1)
      vlev2[i]<-var(lev2)
      if(pvalF[i]>=threshold){varequal[i]<-"varequal"}
      else{varequal[i]<-"varnotequal"}
    }
  }
  if(shaptest){conditiontest<-data.frame(conditiontest,"pvalnormlev1"=pvalnormlev1,"pvalnormlev2"=pvalnormlev2,"samplenorm"=samplenorm)}
  if(Ftest){conditiontest<-data.frame(conditiontest,"pvalF"=pvalF,"variancelev1"=vlev1,"variancelev2"=vlev2,"varequal"=varequal)}
  return(conditiontest) 
} 


volcanoplot<-function(toto,thresholdFC=1,thresholdpv=0.05,graph=T,maintitle="Volcano plot"){
  ##Highlight genes that have an absolute fold change > 2 and a p-value < Bonferroni cut-off

  threshold <- (as.numeric(abs(toto$logFC) > thresholdFC & toto$pval< thresholdpv ) +1)*2
  listgen<-data.frame(toto,"threshold"=threshold)
  if(!graph){return(listgen)}
  ##Construct the plot object
  g = ggplot(data=listgen, aes(x=logFC, y=-log10(pval))) +
    geom_point(alpha=0.4, size=1.75, colour=threshold) +
    theme(legend.position = "none") +
    xlim(c(-(max(listgen$logFC)+0.2), max(listgen$logFC)+0.2)) + ylim(c(0, max(-log10(listgen$pval))+0.2)) +
    xlab("log2 fold change") + ylab("-log10 p-value")+
    ggtitle(maintitle)+theme(plot.title=element_text( size=15)) 
  g
} 

barplottest<-function(restest,thresholdpv=0.05,thresholdFC=1,graph=T,maintitle="Mean by group for differentially expressed metabolit"){
  meta<-rep(restest[,1],each=2)
  lev1<-colnames(restest)[5]
  lev2<-colnames(restest)[6]
  categ<-rep(c(lev1,lev2),times=(nrow(restest)))
  pval<-rep((restest$pval< thresholdpv),each=2)
  logFC<-rep((abs(restest$logFC)> thresholdFC),each=2) 
  moy<-vector() 
  moy[seq(from=1,to=length(meta),by = 2)]<-restest[,5]
  moy[seq(from=2,to=length(meta),by = 2)]<-restest[,6]
  data<-data.frame(meta,categ,pval,logFC,moy)
  data<-data[order(data$pval),]
  if(!graph){return(data[(which(data$pval==TRUE)& (data$logFC==TRUE)),])}
  else{
      ggplot(data[which( ( data$pval) & (data$logFC) ),], aes(meta, moy,fill=categ))+geom_bar(stat="identity", position="dodge")+ 
      ggtitle(maintitle)+theme(plot.title=element_text( size=15))}
}

barplot2<-function(toto){
  meta<-rep(colnames(toto[,-1]),each=2)
  categ<-rep(levels(toto[,1]),times=(ncol(toto)-1))
  moy<-vector()
  moy[seq(from=1,to=length(meta),by = 2)]<-colMeans(toto[which(toto[,1]==categ[1]),-1],na.rm = T)
  moy[seq(from=2,to=length(meta),by = 2)]<-colMeans(toto[which(toto[,1]==categ[2]),-1],na.rm = T)
  data<-data.frame(meta,categ,moy)
  
  ggplot(data, aes(meta, moy,fill=categ))+geom_bar(stat="identity", position="dodge")+ 
    ggtitle("Intensity Mean for differentially expressed metabolit") + theme(plot.title = element_text(lineheight=.8, face="bold"))
}
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

plot_pred_type_distribution <- function(class,pred,names, threshold,maintitle="Score representation",graph=T) {
  df<-data.frame(names,class,pred)
  colnames(df)<-c("names","class","pred")
  v <-rep(NA, nrow(df))
  v <- ifelse(df$pred >= threshold & df$class == levels(class)[1], "TruePositiv", v)
  v <- ifelse(df$pred >= threshold & df$class == levels(class)[2], "FalsePositiv", v)
  v <- ifelse(df$pred < threshold & df$class ==  levels(class)[1], "FalseNegativ", v)
  v <- ifelse(df$pred < threshold & df$class == levels(class)[2], "TrueNegativ", v)
  
  df$predtype <-factor(v,levels = c("FalseNegativ","FalsePositiv","TrueNegativ","TruePositiv"),ordered = T)
  if(!graph){return(df)}
  set.seed(20011203)
  ggplot(data=df, aes(x=class, y=pred)) + 
    #geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
    geom_jitter(aes(color=predtype), alpha=0.6) +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_manual(values=palet(df$predtype),name="") +
    ggtitle(maintitle)+theme(plot.title=element_text( size=15))
}


boxplotggplot<-function(class,score,names,maintitle="svm score's Boxplot ",graph=T){
  data<-data.frame("names"=names,"class"= class,"score"=as.vector(score))
  if(!graph){return(data)}
  p<-ggplot(data, aes(x=class, y=score, fill=class)) +
    geom_boxplot()+
    ggtitle(maintitle)+theme(plot.title=element_text( size=15))

  p
}

scoremodelplot<-function(class,score,names,threshold,type,graph){
    if(type=="boxplot"){
          boxplotggplot(class =class,score =score,names=names,
                        graph = graph)
    }
    else if(type=="points"){
          plot_pred_type_distribution(class = class, pred = score,names=names,threshold=threshold,graph=graph )
    } 
}

ROCcurve<-function(validation,decisionvalues,maintitle="Roc curve",graph=T,ggplot=T){
  validation<-factor(validation,levels = rev(levels(validation)),ordered = TRUE)
  
  #argument : validation, vector of appartenance,
  #            decisionvalues, vector of scores
  #fulldata<-rocdata(grp = validation, pred = as.vector(decisionvalues))
  data<-roc(validation,decisionvalues)
  if(!graph){return(data.frame("sensitivity"=data$sensitivities,"specificity"=data$specificities,"thresholds"=data$thresholds))}
  if(!ggplot){plot(data)}
  if(ggplot){
    y<-rev(data$sensitivities)
    x<-rev(data$specificities)
    roc<-data.frame(x,y)
    auc<-as.numeric(auc(data))
    
    col<-gg_color_hue(3)
    roccol<-col[1]
    bin = 0.01
    diag = data.frame(x = seq(0, 1, by = bin), y = rev(seq(0, 1, by = bin)))
    p <- ggplot(data = roc, aes(x = x, y = y)) + 
      geom_point(color = roccol) +
      geom_line(color = roccol) + 
      geom_line(data = diag, aes(x = x, y = y), color =col[3])
    sp = 19
    f <- p + geom_point(data = diag, aes(x = x, y = y), color = "lightgrey",
                        shape = sp) + theme(axis.text = element_text(size = 16),
                                            title = element_text(size = 15)) + labs(x = "Sensitivity",
                                                                                    y = "Specificity", title = maintitle) +
      annotate("text",x=0.2,y=0.1,label=paste("AUC = ",as.character(round(auc,digits = 3))),size=7,colour= roccol)+
      scale_x_reverse()
    
    f
  }
}
barplottestcond<-function(toto){
  #toto: dataframe res from conditiontest function
  rescond<-vector()
  for (i in (1:nrow(toto))){
    if(toto$samplenorm[i]=="norm" & toto$varequal[i]!="varequal"){rescond[i]<-"norm"}
    else if(toto$samplenorm[i]=="norm" & toto$varequal[i]=="varequal"){rescond[i]<-"both"}
    else if( toto$samplenorm[i]!="norm" &toto$varequal[i]=="varequal"){rescond[i]<-"varequal"}
    else{rescond[i]<-"none"}
    
  }
  data<-as.factor(rescond)
  p<-qplot(factor(data), geom="bar", fill=factor(data))
  p+ggtitle("Repartition of the variables according to the test results")+
    theme(plot.title=element_text(size=15))
}

showsingledata<-function(toto,namevar,title=" "){
  data<-toto[,c(1,1,  which(colnames(toto)==namevar))]
  set.seed(1)
  data[,2]<-as.numeric(data[,2])+rnorm(n = nrow(data),mean = 0,sd = 0.1)
  colnames(data)<-c("class","num","intensity")
  col<-gg_color_hue(2)
  data<-data[complete.cases(data),]
  ggplot(data, aes(x=num, y=intensity, group=class)) +
    geom_point(aes( color=class) ,size=5)+
    annotate("text",x=1.5,y=max(data[,3]),label=namevar,size=6)+
    ggtitle(title)
    
  
}
proptestNA<-function(toto){
  group<-toto[,1]
  toto[,1]<-as.character(toto[,1])
  toto[which(!is.na(toto),arr.ind=T)]<-"value"
  toto[which(is.na(toto),arr.ind=T)]<-"NA"
  pval<-vector("numeric",length = ncol(toto))
  lessgroup<-vector("character",length = ncol(toto))
  prctmore<-vector("numeric",length = ncol(toto))
  prctless<-vector("numeric",length = ncol(toto))
  for (i in 1:ncol(toto)){
    conting<-table(group,factor(toto[,i],levels=c("value","NA")))
    options(warn=-1)
    res<-prop.test(conting)
    options(warn=0)
    pval[i]<-res$p.value
    prctmore[i]<-max(res$estimate)
    prctless[i]<-min(res$estimate)
    if(res$estimate[1]==res$estimate[2]){ lessgroup[i]<-"NA"}
    else{lessgroup[i]<-rownames(conting)[which(res$estimate==min(res$estimate))]}
  }
  pval[is.na(pval)]<-1
  return(data.frame("pval"=pval,"lessgroup"=lessgroup,"prctless"=prctless,"prctmore"=prctmore,"names"=colnames(toto)))
}



errorplot<-function(text=paste("error /n","text error")){
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, text,cex = 1.6, col = "black")}

bestmodel<-function(tabdecouv,tabval,parameters){
  tabselect<-selectprctNA(toto = tabdecouv,prctNA = parameters$prctNA,group=as.logical(parameters$NAgroup),restrictif =as.logical(parameters$restrict))
  nbselect<<-ncol(tabselect)-1
  if(parameters$NAstructure==TRUE){
    tabNAstructure<<-as.data.frame(replaceproptestNA(toto = tabdecouv,threshold = parameters$thresholdNAstructure ,rempNA ="moygr",
                                     maxvaluesgroupmin=parameters$maxvaluesgroupmin,minvaluesgroupmax=parameters$minvaluesgroupmax,replacezero=T))
    if(!is.null(tabNAstructure)){##
    tabselect1<-cbind(tabselect,tabNAstructure[,!colnames(tabNAstructure)%in%colnames(tabselect)])
    if(sum(!colnames(tabNAstructure)%in%colnames(tabselect))!=0){
      colnames(tabselect1)[(ncol(tabselect)+1):ncol(tabselect1)]<-colnames(tabNAstructure)[!colnames(tabNAstructure)%in%colnames(tabselect)]}
    tabselect<-tabselect1}
  }

  if(parameters$log==TRUE) {
    tabselect[,-1]<-log(x = tabselect[,-1]+1,base = 2)}
  if(parameters$scaled==TRUE){
    tabselect[,-1]<-scale(tabselect[,-1], center = F, scale = TRUE)
  }
  tabselectssNA<-replaceNA(toto=tabselect,rempNA=parameters$rempNA,pos=F,NAstructure = as.logical(parameters$NAstructure),
                           threshold=parameters$thresholdNAstructure,maxvaluesgroupmin=parameters$maxvaluesgroupmin,
                           minvaluesgroupmax=parameters$minvaluesgroupmax)
  tabdiff<-testdiff(tabselectssNA = tabselectssNA,test = parameters$test,adjustpval = as.logical(parameters$adjustpval),
                    thresholdpv = parameters$thresholdpv,thresholdFC = parameters$thresholdFC)
  nbdiff<-as.integer(ncol(tabdiff)-1)
  if(nbdiff<=0){auc<-c(0,0,0)
  nbdiff<-0}
  else{ 
    if(parameters$NAstructure==TRUE){varstructure<-colnames(tabNAstructure)}
    else{varstructure<-NULL}
    auc<-modelisation(tabdiff = tabdiff,tabval = tabval,model = as.character(parameters$model),
            rempNA = as.character(parameters$rempNA),log=parameters$log,scaled=parameters$scaled,varstructure=varstructure)
    }
  res<-c(nbselect,nbdiff,auc)
  return(res)
}


testdiff<-function(tabselectssNA,test,adjustpval,thresholdpv,thresholdFC){
  if(test=="notest"){tabdiff<-tabselectssNA}
  else{
    datatest<-diffexptest(toto = tabselectssNA,test = test ,adjustpval=adjustpval)
    #differential expressed          
    datatestdiff<-datatest[which( (datatest$pval<thresholdpv)&(abs(datatest$logFC)>thresholdFC )),]
    if(dim(datatestdiff)[1]==0){
      print("no differentially expressed variables")
      tabdiff<-data.frame()
    }
    else{
      indvar<-(colnames(tabselectssNA)%in%datatestdiff$nom)
      indvar[1]<-T #keep the categorial variable
      tabdiff<-tabselectssNA[,indvar]
    }
  }
  return(tabdiff)
}

modelisation<-function(tabdiff,tabval,model,rempNA,log,scaled,varstructure=NULL){

  if (model=="randomforest"){
    set.seed(20011203)
    tab<-as.data.frame(tabdiff[,-1])
    colnames(tab)<-colnames(tabdiff)[-1]
    resmodel <- randomForest(tab,tabdiff[,1],ntree=500,
                             importance=T,keep.forest=T)
  }   
  if(model=="svm"){
    resmodel<- best.tune(svm,class ~ ., data = tabdiff )
    #resmodel <- svm(class ~ ., data = tabdiff )
  }
  #Validation
  
  tabvaldiff<-tabval[,which(colnames(tabval)%in%colnames(tabdiff))]
  if(rempNA=="moygr"){rempNA<-"moy" }
  if(log==TRUE) {tabvaldiff[,-1]<-log(x = tabvaldiff[,-1]+1,base = 2)}
  if(scaled==TRUE){
    tabvaldiff[,-1]<-scale(tabvaldiff[,-1], center = F, scale = TRUE)
  }
  if(!is.null(varstructure)){
    tabvaldiff[which(is.na(tabvaldiff),arr.ind = T)[which(which(is.na(tabvaldiff),arr.ind = T)[,2]%in%which(colnames(tabvaldiff)%in%varstructure)),]]<-0
    }
  alldata<-rbind(tabvaldiff,tabdiff)
  
  tabvaldiffssNA<<-replaceNA(toto = alldata,rempNA =rempNA,pos =T ,NAstructure = F)
  #prediction a partir du model
  validation<-tabvaldiffssNA[1:nrow(tabvaldiff),-1]
  colnames(validation)<-colnames(tabvaldiff )[-1]
  if(model=="randomforest"){
    scoreval<<-predict(object=resmodel,type="prob",newdata = validation)[,2]
    predval<<-predict(object=resmodel,type="response",newdata = validation)
  }

  if(model=="svm"){
    predval<-predict(resmodel,newdata =validation,decision.values=F)
    scoreval <-attr(predict(resmodel,newdata =validation,decision.values=T),"decision.values")
  }
  auc<-auc(roc(tabvaldiff[,1], as.numeric(scoreval)))
  if(is.na(auc))auc<-0
  data<-table(predval, tabvaldiff[,1])
  sensibilite<-round(data[1,1]/(data[1,1]+data[2,1]),digits = 3)
  specificite<-round(data[2,2]/(data[1,2]+data[2,2]),digits=3)
  res<-c(auc,sensibilite,specificite)
  return(res)
}
constructparameters<-function(listparameters){
  resparameters<-data.frame(listparameters[[1]])
  namescol<-names(listparameters)
  
  for(i in 2:length(listparameters)){
    tt<-rep(listparameters[[i]],each=nrow(resparameters))
    res<-resparameters
    if(length(listparameters[[i]])>1){
      for (j in 1:(length(listparameters[[i]])-1)){
        res<-rbind(res,resparameters)
      }
    }
    resparameters<-cbind(res,tt)
  }
  colnames(resparameters)<-namescol
  return(resparameters)
}

classparameters<-function(resparameters){
  resparameters<-as.data.frame(resparameters)
  resparameters$prctNA<-as.numeric( resparameters$prctNA)
  resparameters$NAgroup<-as.logical(resparameters$NAgroup)
  resparameters$restrict<-as.logical(resparameters$restrict)
  resparameters$log<-as.logical(resparameters$log)
  resparameters$scaled<-as.logical(resparameters$scaled)
  resparameters$rempNA<-as.factor(resparameters$rempNA)
  resparameters$NAstructure<-as.logical(resparameters$NAstructure)
  resparameters$test<-as.factor(resparameters$test)
  resparameters$adjustpval<-as.logical(resparameters$adjustpval)
  resparameters$thresholdpv<-as.numeric(resparameters$thresholdpv)
  resparameters$thresholdFC<-as.numeric(resparameters$thresholdFC)
  resparameters$model<-as.factor(resparameters$model)
  return(resparameters)
  
}

correlogrammapp<-function(toto,maintitle="Correlogramm",graph=T){
  
  data<-calculcorr(toto)
  if (!graph){return(data)}
  rownames(data$correlation)<-substring(rownames(data$correlation),1,4)
  colnames(data$correlation)<-substring( colnames(data$correlation),1,4)
  rownames(data$pvalue)<-substring(rownames(data$pvalue),1,4)
  colnames(data$pvalue)<-substring( colnames(data$pvalue),1,4)
  correlation<-corrplot(data$correlation,order = "hclust" ,hclust.method="ward",
                        p.mat = data$pvalue, sig.level = 0.01, insig = "blank",title =maintitle ,mar=c(1,0,2,0))
}
calculcorr<-function(toto){
  data<-rcorr(as.matrix(toto),type = "spearman") 
  correlation<-data$r
  correlation[which(is.na((correlation)),arr.ind = T)]<-0
  #Test de significaticité de la corrélation
  testcorrelation<-data$P 
  testcorrelation[which(is.na((testcorrelation)),arr.ind = T)]<-1
  diag(testcorrelation)<-NA
  return(list("correlation"=correlation,"pvalue"=testcorrelation))
}
palet<-function(pred){
  col<-sort(unique(as.character(pred)))
  col[which(col=="FalseNegativ")]<-"#C77CFF"
  col[which(col=="FalsePositiv")]<-"#00BA38"
  col[which(col=="TrueNegativ")]<-"#00BFC4"
  col[which(col=="TruePositiv")]<-"#F8766D"
  return(col)
}

densityscore<-function(score,scorepredict,maintitle="Density learning's score and prediction score",threshold,graph=T){
  x<-density(score)$x
  y<-density(score)$y
  xddf <- data.frame(x=x,y=y)
  x<-scorepredict
  y<-rep(x = 0.1,length=length(scorepredict) )
  coordpredict<- data.frame(x=x,y=y)
  if(!graph){rawdata<-data.frame(xcurve=xddf$x,ycurve=xdff$y,xpoints=coordpredict$x,ypoints=coordpredict$y)}
  qplot(x,y,data=xddf,geom="line",xlab = "score",ylab="density of learning's score")+
    geom_ribbon(data=subset(xddf ,x>min(density(score)$x) & x<threshold),aes(x=x,ymin=0,ymax=y,fill="blue"),
                colour=NA,alpha=0.5)+
    geom_ribbon(data=subset(xddf ,x>threshold & x<max(density(score)$x)),aes(x=x,ymin=0,ymax=y,fill="red"),
                colour=NA,alpha=0.5)+
    geom_point(data = coordpredict, colour = "black",size=rep(4,length(x)))+
    ggtitle(maintitle)+theme(plot.title=element_text( size=15))+theme(legend.position = "bottom") +
    scale_fill_manual(name='density',
                      values=c(alpha("blue",alpha = 0.1),alpha("red",alpha = 0.5)),
                      labels=c("negativ","positiv"))+guides(colour = guide_legend(override.aes = list(alpha = 0.5)))
}


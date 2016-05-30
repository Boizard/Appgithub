
######################################################
options(shiny.maxRequestSize=60*1024^2) 
source("global.R")
tablearn<<-data.frame()    
tabval<<-NULL
lev<<-vector()
#tabdiff<<-data.frame()
testdiffdata<<-data.frame()
shinyServer(function(input, output,session) {    

  #boot output
  #####
#   observe({
#     if(!is.null(input$filestate)    ){
#       return(!is.null(input$learningfile))
#     }
#   })
  output$modelUploaded <- reactive({
    return(!is.null(input$modelfile))
  })
  outputOptions(output, 'modelUploaded', suspendWhenHidden=FALSE)
  
  output$fileUploaded <- reactive({
    return(!is.null(input$learningfile))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  

  output$image1<-renderImage({return (list(src="pictures/Logo I2MC.jpg", 
                                           contentType="image/jpeg",
                                           alt="I2MC logo"))},deleteFile = F)
  output$namefilelearn<-renderText({
    namelearn<-input$learningfile$name
  })
  output$fileUploadedval <- reactive({
    return( !is.null(DATA()$VALIDATION))
  })
  outputOptions(output, 'fileUploadedval', suspendWhenHidden=FALSE)
  
  output$modelUploadedval <- reactive({
    return(!is.null(DATA()$VALIDATION))
  })
  outputOptions(output, 'modelUploadedval', suspendWhenHidden=FALSE)
  
  observe({
    if(input$confirmdatabutton!=0 & !is.null(input$modelfile)){
      updateNumericInput(session, "prctNA", value = previous$parameters$prctNA)
      updateCheckboxInput(session ,"NAgroup",value=previous$parameters$NAgroup)
      updateRadioButtons(session,"restrict",selected =  previous$parameters$restrict)
      updateCheckboxInput(session ,"NAstructure",value=previous$parameters$NAstructure)
      updateNumericInput(session, "thresholdNAstructure", value = previous$parameters$thresholdNAstructure)
      updateRadioButtons(session,"structdata",selected=previous$parameters$structdata)
      updateNumericInput(session, "maxvaluesgroupmin", value = previous$parameters$maxvaluesgroupmin)
      updateNumericInput(session, "minvaluesgroupmax", value = previous$parameters$minvaluesgroupmax)
      updateRadioButtons(session,"rempNA",selected=previous$parameters$rempNA)
      updateCheckboxInput(session ,"log",value=previous$parameters$log)
      updateCheckboxInput(session ,"scaled",value=previous$parameters$scaled)
      updateRadioButtons(session,"test",selected=previous$parameters$test)
      updateNumericInput(session, "thresholdFC", value = previous$parameters$thresholdFC)
      updateNumericInput(session, "thresholdpv", value = previous$parameters$thresholdpv)
      updateCheckboxInput(session ,"adjustpval",value=previous$parameters$adjustpval)
      updateRadioButtons(session,"model",selected=previous$parameters$model)
      updateNumericInput(session, "thresholdmodel", value = previous$parameters$thresholdmodel)
      updateCheckboxInput(session ,"fs",value=previous$parameters$fs)
      updateCheckboxInput(session ,"adjustval",value=previous$parameters$adjustval)
      
    }
  }) 
  #####
  #Slidebar2output
  ######
  output$dim1learn<-renderText({
    di1<-dim(x = DATA()$LEARNING)[1]  
  })
  output$dim2learn<-renderText({
    di2<-dim(x = DATA()$LEARNING)[2]  
  })
  output$namefileval<-renderText({
    nameval<-input$validationfile$name
  })  
  output$dim1val<-renderText({
    di1<-dim(x = DATA()$VALIDATION)[1]  
  })
  output$dim2val<-renderText({
    di2<-dim(x = DATA()$VALIDATION)[2]  
  })
  
  DATA<-reactive({ 
    print(paste("first",is.null(input$learningfile)&is.null(input$modelfile)))
    if(is.null(input$learningfile)&is.null(input$modelfile)){return()}#Pas de fichier
    
    if(!is.null(input$modelfile) ){
      load(file = input$modelfile$datapath)
      previous<<-state
      tablearn<-previous$data$LEARNING
      tabval<-previous$data$VALIDATION
      lev<-previous$data$LEVELS
    }
    
    if(!is.null(input$learningfile)  ){
      if(input$confirmdatabutton==0){
        datapath<- input$learningfile$datapath
        out<<-tryCatch(
          importfile(datapath = datapath,extension = input$filetype,
                            NAstring=input$NAstring,sheet=input$sheetn,skiplines=input$skipn,dec=input$dec,sep=input$sep)
          ,error=function(e) e )
        print(class(out))
        if(any(class(out)=="error")){tablearn<-data.frame()}
        else{tablearn<<-out}
        print(is.null(tablearn))
        validate(need(ncol(tablearn)>1 & nrow(tablearn)>1,"problem import"))
        
#           tablearn<<-importfile(datapath = datapath,extension = input$filetype,
#                          NAstring=input$NAstring,sheet=input$sheetn,skiplines=input$skipn,dec=input$dec,sep=input$sep)

        if(input$changedata){
          tablearn<<-transformdata(toto = tablearn,nrownames=input$nrownames,ncolnames=input$ncolnames,
                              transpose=input$transpose,zeroegalNA=input$zeroegalNA)
        }

      }
      if(input$confirmdatabutton!=0){
        tablearn<<-confirmdata(toto = tablearn)
        lev<-levels(x = tablearn[,1])
        names(lev)<-c("positif","negatif")
        lev<<-lev
      }
    }
    if(!is.null(input$validationfile)  ){
      
      if(input$confirmdatabutton==0){
        datapathV<- input$validationfile$datapath
        out<<-tryCatch(importfile(datapath = datapathV,extension = input$filetype,
                            NAstring=input$NAstring,sheet=input$sheetn,skiplines=input$skipn,dec=input$dec,sep=input$sep),error=function(e) e)
        if(any(class(out)=="error")){tabval<-NULL}
        else{tabval<<-out}
        #print(class(out)=="error")
        #validate(need(ncol(tabval)>1 & nrow(tabval)>1,"problem import"))
        
        if(input$changedata){
          tabval<<-transformdata(toto = tabval,nrownames=input$nrownames,ncolnames=input$ncolnames,
                                transpose=input$transpose,zeroegalNA=input$zeroegalNA)
        }
        
      }
      if(input$confirmdatabutton!=0){
        tabval<<-confirmdata(toto = tabval)
      }
    }
    #else{tabval<-NULL}
    print(is.null(tablearn))
    print("######################")
     list(LEARNING=tablearn, 
          VALIDATION=tabval,
          LEVELS=lev)
    })
  state <- reactiveValues()
  observe({
    parameters<-data.frame("prctNA"=input$prctNA,"NAgroup"=input$NAgroup,"restrict"=input$restrict,"NAstructure"=input$NAstructure,
                           "thresholdNAstructure"=input$thresholdNAstructure,"structdata"=input$structdata,"maxvaluesgroupmin"=input$maxvaluesgroupmin,"minvaluesgroupmax"=input$minvaluesgroupmax,
                           "rempNA"=input$rempNA,"log"=input$log,"scaled"=input$scaled,
                           "test"=input$test,"thresholdFC"=input$thresholdFC,"thresholdpv"=input$thresholdpv,"adjustpval"=input$adjustpv,
                           "model"=input$model,"thresholdmodel"=input$thresholdmodel,"fs"=input$fs,"adjustval"=input$adjustval)
    data<-DATA()
    model<-MODEL()
    if(input$NAstructure){varstructure<-colnames(NASTRUCT()$NAstructure)}
    else{varstructure<-NULL}
    isolate(state<<-list("parameters"=parameters,"data"=data,"model"=model,"varstructure"=varstructure))
  })
  
  output$savestate <- downloadHandler(
    filename <- function(){
      paste("model.RData")
    },
    content = function(file) { 
      save(state, file = file)
    }
  )
  #####
  #dataTable output
  #####
  output$JDDlearn=renderDataTable({
    learning<<-DATA()$LEARNING
    print(paste("plot",!is.null(learning)))
    validate(need(!is.null(learning),"problem import"))
    colmin<-min(ncol(learning),100)
    rowmin<-min(nrow(learning),100)
    cbind(Names=rownames(learning[1:rowmin,1:colmin]),learning[1:rowmin,1:colmin])},
    options = list(    "orderClasses" = F,
                       "responsive" = F,
                       "pageLength" = 10))
  
  output$downloaddataJDDlearn <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   DATA()$LEARNING, file) })
  
  
  output$JDDval=renderDataTable({
    vali<-DATA()$VALIDATION
    validate(need(!is.null(vali),"problem import"))
    colmin<-min(ncol(vali),100)
    rowmin<-min(nrow(vali),100)
    cbind(Names=rownames(vali[1:rowmin,1:colmin]),vali[1:rowmin,1:colmin])},
    options = list(    "orderClasses" = F,
                       "responsive" = F,
                       "pageLength" = 10)) 
  
  output$downloaddataJDDval <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   DATA()$VALIDATION, file) })
  
#####  
  SELECTDATA<-reactive({
  validate(need(input$prctNA>=0 &input$prctNA<=100,"%  NA has to be between 0 and 100"))
  tabdecouv<-DATA()$LEARNING
  restrict<-as.logical(input$restrict)
  tabselect<-selectprctNA(tabdecouv,input$prctNA,group=input$NAgroup,restrictif =restrict)

  })
#####
#Selection Output
#####
  output$heatmapNA<-renderPlot({
    tabselect<-SELECTDATA()
    heatmapNA(toto =tabselect[,-1], names =paste(tabselect[,1],1:length(tabselect[,1])))
    #plot(x = (tabselect[,2:3]))
  })
  output$downloadplotheatmapNA = downloadHandler(
    filename = function() { 
      paste('graph','.',input$paramdownplot, sep='') 
    },
    content = function(file) {
      ggsave(file, plot =    heatmapNA(toto =tabselect[,-1], names =paste(tabselect[,1],1:length(tabselect[,1]))), 
             device = input$paramdownplot)

    },
    contentType=NA)
  
  output$downloaddataheatmapNA <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(SELECTDATA(), file)
    }
  )

  output$plotNA<-renderPlot({
    tabselect<-SELECTDATA()
    tablearn<-DATA()$LEARNING
    distributionNA(toto = tablearn,prctNAselect = input$prctNA/100,nvar = ncol(tabselect) ,ggplot =  T)  
    })
  
  output$nvarselect<-renderText({
    di1<-dim(x = SELECTDATA())[2]-1  
  })
  
  output$downloadplotNA = downloadHandler(
    filename = function() { 
      paste('graph','.',input$paramdownplot, sep='') 
    },
    content = function(file) {
      ggsave(file, plot =        distributionNA(toto = DATA()$LEARNING,prctNAselect = input$prctNA/100,nvar = ncol(SELECTDATA()) ,ggplot =  T), 
             device = input$paramdownplot)
    },
    contentType=NA)
  
  output$downloaddataplotNA <- downloadHandler( 
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(distributionNA(toto = DATA()$LEARNING,prctNAselect = input$prctNA/100,nvar = ncol(SELECTDATA()),graph=F)  , file)
    }
  )
  
  
  NASTRUCT<-reactive({
    validate(need(input$minvaluesgroupmax>=0 &input$minvaluesgroupmax<=100 & input$maxvaluesgroupmin>=0 &input$maxvaluesgroupmin<=100,"% threshold has to be between 0 and 100"),
             need(input$thresholdNAstructure>0,input$thresholdNAstructure<1,"threshold of the pvalue has to be between 0 and 1"))
    if(input$structdata=="alldata"){tabdecouv<-DATA()$LEARNING}
    if(input$structdata=="selecteddata"){tabdecouv<-SELECTDATA()}

    NAstructuressNA<-replaceproptestNA(toto = tabdecouv,threshold = input$thresholdNAstructure ,rempNA ="moygr",
                              maxvaluesgroupmin=input$maxvaluesgroupmin,minvaluesgroupmax=input$minvaluesgroupmax,replacezero = T)
    NAstructure<-replaceproptestNA(toto = tabdecouv,threshold = input$thresholdNAstructure ,rempNA ="moygr",
                              maxvaluesgroupmin=input$maxvaluesgroupmin,minvaluesgroupmax=input$minvaluesgroupmax,replacezero=F)
    list(NAstructuressNA=NAstructuressNA,NAstructure=NAstructure)
  })
    
  
  output$heatmapNAstructure<-renderPlot({
    class<-DATA()$LEARNING[,1]
    NAstructure<-NASTRUCT()$NAstructure
    heatmapNA(toto=NAstructure,names = paste(class,1:length(class)))            
    #else{errorplot(text = " No NA's structure")}
  })
  
  output$downloadstructur = downloadHandler(
    filename = function() { 
      paste('graph','.',input$paramdownplot, sep='') 
    },
    content = function(file) {
      ggsave(file, plot = heatmapNA(NASTRUCT()$NAstructwithoutzero,names = paste(DATA()$LEARNING[,1],1:length(DATA()$LEARNING[,1]))), 
             device = input$paramdownplot)
    },
    contentType=NA)
  
  output$downloaddatastructur <- downloadHandler( 
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(heatmapNA(NASTRUCT()$NAstructwithoutzero,names = paste(DATA()$LEARNING[,1],1:length(DATA()$LEARNING[,1])),graph=F), file)
    }
  )
#####  
  TRANSFORMDATA<-reactive({
    tabselect<<-SELECTDATA()
    numcol<-ncol(tabselect)
    validate(need(numcol>0,"No select dataset"))
    if(input$rempNA=="pca"){
    validate(need(min(apply(X = tabselect,MARGIN = 2,FUN = function(x){sum(!is.na(x))}))>1,"not enough data for pca estimation"))
    }
    if(input$NAstructure){
      NAstructure<<-NASTRUCT()$NAstructuressNA
      if(!is.null(NAstructure)){
        if (!colnames(NAstructure)%in%colnames(tabselect)!=0){
      tabselect1<-cbind(tabselect,NAstructure[,!colnames(NAstructure)%in%colnames(tabselect)])
      colnames(tabselect1)[(numcol+1):ncol(tabselect1)]<-colnames(NAstructure)[!colnames(NAstructure)%in%colnames(tabselect)]
      tabselect<-tabselect1
        }
      }
    } 
    if(input$log) { 
    tabselect[,-1]<-log(x = tabselect[,-1]+1,base = 2)}
    if(input$scaled){
      tabselect[,-1]<-scale(tabselect[,-1], center = F, scale = TRUE)
    }

    tabselectssNA<-replaceNA(toto=tabselect,rempNA=input$rempNA,pos=F,NAstructure = F)

  })  
  
  output$plotheatmaptransformdata<-renderPlot({
    selectdata<-TRANSFORMDATA()

  heatmapplot(toto =selectdata,nbclass=0,ggplot = T,scale=F)
  })
  output$downloadplotheatmap = downloadHandler(
    filename = function() { 
      paste('graph','.',input$paramdownplot, sep='') 
    },
    content = function(file) {
      ggsave(file, plot =    heatmapplot(toto =TRANSFORMDATA(),nbclass=input$nbclassvalues,ggplot = T,scale=F), 
             device = input$paramdownplot)
      
    },
    contentType=NA)
  
  output$downloaddataheatmap <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(heatmapplot(toto =TRANSFORMDATA(),nbclass=0,ggplot = T,scale=F,graph=F), file)
    }
  )
  
  output$plotmds<-renderPlot({
    selectdata<-TRANSFORMDATA()
    mdsplot(toto = selectdata,ggplot=T)
  })
  output$downloadplotmds = downloadHandler(
    filename = function() { 
      paste('graph','.',input$paramdownplot, sep='') 
    },
    content = function(file) {
      ggsave(file, plot =        mdsplot(toto = TRANSFORMDATA(),ggplot=T),  device = input$paramdownplot)
    },
    contentType=NA)
  
  output$downloaddatamds <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(    mdsplot(toto = TRANSFORMDATA(),ggplot=T,graph=F), file)
    }
  )
#####
  TEST<-reactive({
    tabselectssNA<<-TRANSFORMDATA()
    #condition tests
    if (input$SFtest){
      datatesthypothesis<-conditiontest(tabselectssNA,shaptest=T,Ftest=T,threshold=0.05)
    }
    else{datatesthypothesis<-data.frame()}
    #diff test
    if(input$test=="notest"){tabdiff<-tabselectssNA}
    else{
        datatest<-diffexptest(toto = tabselectssNA,test = input$test ,adjustpval=input$adjustpv)
    #differential expressed          
        datatestdiff<-datatest[which( (datatest$pval<input$thresholdpv)&(abs(datatest$logFC)>input$thresholdFC )),]
        if(dim(datatestdiff)[1]==0){
            print("no differentially expressed variables")
            tabdiff<<-data.frame()
      }
        else{
            indvar<-(colnames(tabselectssNA)%in%datatestdiff$nom)
            indvar[1]<-T #keep the categorial variable
            tabdiff<<-tabselectssNA[,indvar]
        }
    }
    list("tabdiff"=tabdiff,"testdiffdata"=datatest,"hypothesistest"=datatesthypothesis)
  })
#####
#Output test
#####
  output$plottest1 <- renderPlot({
    testdiffdata<-TEST()$testdiffdata
    volcanoplot(toto = testdiffdata ,thresholdFC = input$thresholdFC,thresholdpv = (input$thresholdpv ) )
  })
  output$downloadplottest1 = downloadHandler(
    filename = function() {paste('graph','.',input$paramdownplot, sep='')},
    content = function(file) {
      ggsave(file, plot = volcanoplot(toto = TEST()$testdiffdata ,
            thresholdFC = input$thresholdFC,thresholdpv = (input$thresholdpv ) ),  device = input$paramdownplot)},
    contentType=NA)
  output$nvarselect2<-renderText({
    di1<-dim(x = SELECTDATA())[2]-1  
  })  
  output$nbdiff<-renderText({
    nbdiff = ncol(TEST()$tabdiff)-1
  })
    
  output$downloaddatatest1 <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   volcanoplot(toto = TEST()$testdiffdata ,thresholdFC = input$thresholdFC,
                                    thresholdpv = (input$thresholdpv ),graph=F ), file) })
  
  output$plottest2 <- renderPlot({
    tabdiff<-TEST()$tabdiff
    testdiffdata<-TEST()$testdiffdata
    if(nrow(tabdiff)!=0){barplottest(restest = testdiffdata,thresholdpv=input$thresholdpv,thresholdFC=input$thresholdFC)}
    else{errorplot(text = " No differently expressed ")}
    
  })
  output$downloadplottest2 = downloadHandler(
    filename = function() {paste('graph','.',input$paramdownplot, sep='')},
    content = function(file) {
      ggsave(file, plot = barplottest(restest = TEST()$testdiffdata,thresholdpv=input$thresholdpv,thresholdFC=input$thresholdFC),  device = input$paramdownplot)},
    contentType=NA)
  output$downloaddatatest2 <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   barplottest(restest = TEST()$testdiffdata,
                              thresholdpv=input$thresholdpv,thresholdFC=input$thresholdFC,graph=F ), file) })
  
  output$dataconditiontest=renderDataTable({
    hypothesistest<-TEST()$hypothesistest},options = list("orderClasses" = F,
                                                                "responsive" = F,
                                                                "pageLength" = 10))
  output$plottestcondition=renderPlot({
    hypothesistest<-TEST()$hypothesistest   
    barplottestcond(hypothesistest)
  })
  
#####
  MODEL<-reactive({
    #Modelisation 
    #Learning
    if(input$model!="nomodel"){
        if(input$test=="notest"){learning<-TRANSFORMDATA()}
        else{learning<-TEST()$tabdiff}
      validate(need(ncol(learning)!=0,"No select dataset"))
        if(input$invers){
            learning[,1]<-factor(learning[,1],levels = rev(levels(learning[,1])),ordered = TRUE)
        }
      
      lev<-levels(x = learning[,1])
      names(lev)<-c("positif","negatif")

    #Build model
        if (input$model=="randomforest"){
            set.seed(20011203)
            model <<- randomForest(learning[,-1],learning[,1],ntree=500,
                            importance=T,keep.forest=T)
            if(input$fs){
              featureselect<-selectedfeature(model=model,modeltype = "randomforest",tab=tabdiff,
                                             criterionimportance = "fscore",criterionmodel = "BER")
              model<-featureselect$model
              learning<-featureselect$dataset
            }
            scoredecouv =data.frame(model$votes[,lev["positif"]])
            colnames(scoredecouv)<-paste(lev[1],"/",lev[2],sep="")
            predictclassdecouv<-factor(levels = lev) 
            predictclassdecouv[which(scoredecouv>=input$thresholdmodel)]<-lev["positif"]
            predictclassdecouv[which(scoredecouv<input$thresholdmodel)]<-lev["negatif"]
            predictclassdecouv<-as.factor(predictclassdecouv)
            #predictclassdecouv==model$predicted
            
    }   

        if(input$model=="svm"){
            colnames(learning)[1]<-"class"
            
         model <- best.tune(svm,class ~ ., data = learning,cross=10 )   
            
            if(input$fs){
#               x<-as.matrix(learning[,-1])
#               y<-((as.numeric(learning[,1])-1)*2)-1
#               scad<- svm.fs(x, y=y, fs.method="scad", bounds=NULL,
#                             cross.outer= 0, grid.search = "interval",  maxIter = 100,
#                             inner.val.method = "cv", cross.inner= 5, maxevals=500,
#                             seed=123, parms.coding = "log2", show="none", verbose=FALSE )
#               length(scad$model$xind)
#               learning<-learning[,c(1,scad$model$xind+1)]
              featureselect<-selectedfeature(model=model,modeltype = "svm",tab=tabdiff,criterionimportance = "fscore",criterionmodel = "BER")
              model<-featureselect$model
              learning<-featureselect$dataset
            }
            scoredecouv <-model$decision.values
            if(sum(lev==(strsplit(colnames(scoredecouv),split = "/")[[1]]))==0){
              scoredecouv<-scoredecouv*(-1)
              colnames(scoredecouv)<-paste(lev[1],"/",lev[2],sep="")
              }
                
            colnames(scoredecouv)
            predictclassdecouv<-factor(levels = lev) 
            predictclassdecouv[which(scoredecouv>=input$thresholdmodel)]<-lev["positif"]
            predictclassdecouv[which(scoredecouv<input$thresholdmodel)]<-lev["negatif"]
            predictclassdecouv<-as.factor(predictclassdecouv)

           
            }
      #levels(predictclassval)<-paste("test",levels(predictclassdecouv),sep="")
    levels(predictclassdecouv)<-paste("test",lev,sep="")
    classdecouv<-learning[,1]
    resmodeldecouv<-data.frame(classdecouv,scoredecouv,predictclassdecouv)
    colnames(resmodeldecouv) <-c("classdecouv","scoredecouv","predictclassdecouv") 
    decouverte<-list("decouvdiff"=learning,"resmodeldecouv"=resmodeldecouv)

    if (input$adjustval){
    #Validation
        tabval<-DATA()$VALIDATION
        tabvaldiff<-tabval[,which(colnames(tabval)%in%colnames(learning))]
        if(input$log) { 
          tabvaldiff[,-1]<-log(x = tabvaldiff[,-1]+1,base = 2)}
        if(input$scaled){
          tabvaldiff[,-1]<-scale(tabvaldiff[,-1], center = F, scale = TRUE)
        }
        #NAstructure if NA ->0
        if(input$NAstructure){
        varstructure<-colnames(NASTRUCT()$NAstructure)
        tabvaldiff[which(is.na(tabvaldiff),arr.ind = T)[which(which(is.na(tabvaldiff),arr.ind = T)[,2]%in%which(colnames(tabvaldiff)%in%varstructure)),]]<-0
        }
        #merge learning tabvaldiff
        alldata<-rbind(tabvaldiff,learning)
        if(input$rempNA=="moygr"){ 
            print("impossible de remplacer les NA par la moy par group pour la validation")
          tabvaldiffssNA<-replaceNA(toto = alldata,rempNA ="moy")        }
        
        else{tabvaldiffssNA<-replaceNA(toto = alldata,rempNA =input$rempNA)}

    #prediction a partir du model
    validation<-tabvaldiffssNA[1:nrow(tabvaldiff),-1]
    if(input$model=="randomforest"){
      scoreval <-predict(object=model,type="prob",newdata = validation)[,lev["positif"]]
      predictclassval<-vector(length = length(scoreval) ) 
      predictclassval[which(scoreval>=input$thresholdmodel)]<-lev["positif"]
      predictclassval[which(scoreval<input$thresholdmodel)]<-lev["negatif"]
      predictclassval<-as.factor(predictclassval)

    }

    if(input$model=="svm"){
      scoreval =attr(predict(model,newdata =  validation,decision.values=T),"decision.values")
      if(sum(lev==(strsplit(colnames(scoreval),split = "/")[[1]]))==0){scoreval<-scoreval*(-1)}
      
      colnames(scoreval)
      predictclassval<-vector(length = length(scoreval) ) 
      predictclassval[which(scoreval>=input$thresholdmodel)]<-lev["positif"]
      predictclassval[which(scoreval<input$thresholdmodel)]<-lev["negatif"]
      predictclassval<-as.factor(predictclassval)
 
    }

    if(sum(lev==(levels(predictclassval)))==0){
      predictclassval<-factor(predictclassval,levels = rev(levels(predictclassval)),ordered = TRUE)
    }
    classval<- tabvaldiff[,1]
    if(sum(lev==(levels(classval)))==0){
      classval<-factor(classval,levels = rev(levels(classval)),ordered = TRUE)
    }
    #levels(predictclassval)<-paste("test",levels(predictclassval),sep="")
    levels(predictclassval)<-paste("test",lev,sep="")
    resmodelvalidation<-data.frame(classval,scoreval,predictclassval)
    colnames(resmodelvalidation) <-c("classval","scoreval","predictclassval") 
    auc<-auc(roc(as.vector(classval), as.vector(scoreval)))
    validation<-list("selectdata"=tabvaldiff,"validationdiff"=validation,"resmodelvalidation"=resmodelvalidation,"auc"=auc)

    }
    else{
      validation<-list()
    }
    listparameters<-data.frame("prctNA"=input$prctNA,"NAgroup"=input$NAgroup,"restrict"=input$restrict,"NAstructure"=input$NAstructure,
                               "thresholdNAstructure"=input$thresholdNAstructure,"structdata"=input$structdata,"maxvaluesgroupmin"=input$maxvaluesgroupmin,"minvaluesgroupmax"=input$minvaluesgroupmax,
                               "rempNA"=input$rempNA,"log"=input$log,"scaled"=input$scaled,
                          "test"=input$test,"thresholdFC"=input$thresholdFC,"thresholdpv"=input$thresholdpv,"adjustpval"=input$adjustpv,
                          "model"=input$model,"thresholdmodel"=input$thresholdmodel,"adjustval"=input$adjustval)
    res<<-list("decouverte"=decouverte,"model"=model,"validation"=validation,"groups"=lev,"parameters"=listparameters)
    }
  })

  observe({
    if (input$model=="svm") { updateNumericInput(session, "thresholdmodel", value = 0)} 
    else if (input$model=="randomforest"){  updateNumericInput(session, "thresholdmodel", value = 0.5)}
  })


  output$downloaddatalearning <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   MODEL()$decouverte$decouvdiff, file) })
  
  
  output$plotmodeldecouvroc <- renderPlot({
    res<-MODEL()
    ROCcurve(validation = res$decouverte$resmodeldecouv$classdecouv,decisionvalues = res$decouverte$resmodeldecouv$scoredecouv)
  })
  
  output$downloadplotdecouvroc = downloadHandler(
    filename = function() {paste('graph','.',input$paramdownplot, sep='')},
    content = function(file) {
      ggsave(file, plot = ROCcurve(validation = MODEL()$decouverte$resmodeldecouv$classdecouv,decisionvalues = MODEL()$decouverte$resmodeldecouv$scoredecouv),  device = input$paramdownplot)},
    contentType=NA)
  
  output$downloaddatadecouvroc <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   ROCcurve(validation = MODEL()$decouverte$resmodeldecouv$classdecouv,decisionvalues = MODEL()$decouverte$resmodeldecouv$scoredecouv,graph=F ), file) })
  
  output$plotmodeldecouvbp <- renderPlot({
    res<-MODEL()
    scoremodelplot(class =res$decouverte$resmodeldecouv$classdecouv ,score =res$decouverte$resmodeldecouv$scoredecouv,names=rownames(res$decouverte$resmodeldecouv),
                   threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = T)
})
  output$downloadplotmodeldecouvbp = downloadHandler(
    filename = function() {paste('graph','.',input$paramdownplot, sep='')},
    content = function(file) {
      ggsave(file, plot =scoremodelplot(class =res$decouverte$resmodeldecouv$classdecouv ,score =res$decouverte$resmodeldecouv$scoredecouv,names=rownames(res$decouverte$resmodeldecouv),
                                        threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = T),  device = input$paramdownplot)},
    contentType=NA)
  
  output$downloaddatamodeldecouvbp <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   scoremodelplot(class =res$decouverte$resmodeldecouv$classdecouv ,score =res$decouverte$resmodeldecouv$scoredecouv,names=rownames(res$decouverte$resmodeldecouv),
                                        threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = F), file) })
  output$nbselectmodel<-renderText({
    ncol(MODEL()$decouverte$decouvdiff)-1
  })
  
  output$tabmodeldecouv<-renderTable({
    res<-MODEL()
    as.data.frame.matrix(table(res$decouverte$resmodeldecouv$predictclassdecouv,res$decouverte$resmodeldecouv$classdecouv ))
  })
  output$sensibilitydecouv<-renderText({
    res<-MODEL()
    data<-table(res$decouverte$resmodeldecouv$predictclassdecouv,res$decouverte$resmodeldecouv$classdecouv )
    round(data[1,1]/(data[1,1]+data[2,1]),digits = 3)
  })
  output$positif<-renderText({
    res<-MODEL()$groups[1]
  })
  output$negatif<-renderText({
    lev<-MODEL()$groups[2]
  })
  output$specificitydecouv<-renderText({
    res<-MODEL()
    data<-table(res$decouverte$resmodeldecouv$predictclassdecouv,res$decouverte$resmodeldecouv$classdecouv )
    round(data[2,2]/(data[1,2]+data[2,2]),digit=3)
  })
  
  
  output$downloaddatavalidation <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   MODEL()$validation$validationdiff, file) })
    
    
  output$plotmodelvalroc <- renderPlot({
    res<-MODEL()
    ROCcurve(validation =  res$validation$resmodelvalidation$classval,decisionvalues =  res$validation$resmodelvalidation$scoreval)
  })
  output$downloadplotvalroc = downloadHandler(
    filename = function() {paste('graph','.',input$paramdownplot, sep='')},
    content = function(file) {
    ggsave(file, plot =ROCcurve(validation =  res$validation$resmodelvalidation$classval,decisionvalues =  res$validation$resmodelvalidation$scoreval),  device = input$paramdownplot)},
    contentType=NA)
  
  output$downloaddatavalroc <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   ROCcurve(validation =res$validation$resmodelvalidation$classval,decisionvalues =  res$validation$resmodelvalidation$scoreval,graph=F ), file) })
  
    output$plotmodelvalbp <- renderPlot({
      res<-MODEL()
      scoremodelplot(class =res$validation$resmodelvalidation$classval ,score =res$validation$resmodelvalidation$scoreval,names=rownames(res$validation$resmodelvalidation),
                     threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = T)

    })
    output$downloadplotmodelvalbp = downloadHandler(
      filename = function() {paste('graph','.',input$paramdownplot, sep='')},
      content = function(file) {
        ggsave(file, plot =scoremodelplot(class =MODEL()$validation$resmodelvalidation$classval ,score =MODEL()$validation$resmodelvalidation$scoreval,names=rownames(res$validation$resmodelvalidation),
                                          threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = T),  device = input$paramdownplot)},
      contentType=NA)
    
    output$downloaddatamodelvalbp <- downloadHandler(
      filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
      content = function(file) {
        downloaddataset(   scoremodelplot(class =MODEL()$validation$resmodelvalidation$classval ,score =MODEL()$validation$resmodelvalidation$scoreval,names=rownames(res$validation$resmodelvalidation),
                                          threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = F), file) })
      
    
    output$tabmodelval<-renderTable({ 
        res<-MODEL()
        as.data.frame.matrix(table(res$validation$resmodelvalidation$predictclassval, res$validation$resmodelvalidation$classval))
      })
    output$sensibilityval<-renderText({
      res<-MODEL()
      data<-table(res$validation$resmodelvalidation$predictclassval, res$validation$resmodelvalidation$classval)
      round(data[1,1]/(data[1,1]+data[2,1]),digits = 3)
    })
    output$specificityval<-renderText({
      res<-MODEL()
      data<-table(res$validation$resmodelvalidation$predictclassval, res$validation$resmodelvalidation$classval)
      round(data[2,2]/(data[1,2]+data[2,2]),digits=3)
    })
######
output$summarymodel<-renderPrint({
  model<-print(MODEL()$model)
})
output$plotimportance<-renderPlot({
  if(input$model=="randomforest"){
    mod<-MODEL()$model
    var_importance<- data.frame(variable=rownames(mod$importance),
                                importance=as.vector(mod$importance[,4]))
    var_importance <- arrange(var_importance, desc(importance))
    var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)
    p <- ggplot(var_importance, aes(x=variable, weight=importance,fill=variable))
    p + geom_bar()+coord_flip()+ylab("Variable Importance (Mean Decrease in Gini Index)")+
      theme(legend.position="none",plot.title=element_text( size=15))+ggtitle("Importance of the variable in the model")+scale_fill_grey()
  }
})
output$plotcorrelation<-renderPlot({
  if(input$test!="iotest"){
  tabdiff<-MODEL()$decouvert$decouvdiff
  correlogrammapp(toto=as.matrix(tabdiff[,-1]))
  }
})
output$downloadplotcorrelation = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =correlogrammapp(toto=as.matrix(MODEL()$decouvert$decouvdiff[,-1])),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddatacorrelation<- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(   correlogrammapp(toto=as.matrix(MODEL()$decouvert$decouvdiff[,-1]),graph = F), file) })
#####
tabparameters <- eventReactive(input$tunetest, { 
  prctNA<-seq(input$prctNAtest[1],input$prctNAtest[2],by = 10)
  listparameters<-list("prctNA"=prctNA,"NAgroup"=input$NAgrouptest,"restrict"=input$restricttest,"log"=input$logtest,"scaled"=input$scaledtest,
                       "rempNA"=input$rempNAtest,"NAstructure"=input$NAstructuretest,"thresholdNAstructure"=input$thresholdNAstructuretest,"maxvaluesgroupmin"=input$maxvaluesgroupmintest,
                       "minvaluesgroupmax"=input$minvaluesgroupmaxtest,"test"=input$testtest,"adjustpval"=input$adjustpvaltest,
                       "thresholdpv"=input$thresholdpvtest,"thresholdFC"=input$thresholdFCtest,"fs"=input$fstest,"model"=input$modeltest)
  if( sum(do.call(rbind, lapply(listparameters, FUN=function(x){as.numeric(is.null(x))})))>0){resparameters="One of the parameters is empty"}
  else{
  resparameters<-constructparameters(listparameters)
  #resparameters<-resparameters[-which((resparameters$NAgroup==F)&(resparameters$restrict==T)),]
  #resparameters<-resparameters[-which((resparameters$rempNA=="z")&(resparameters$mixt==T)),]
  }
  resparameters<<-classparameters(resparameters)

  learning<<-DATA()$LEARNING
  validation<<-DATA()$VALIDATION
    resmodel<-matrix(ncol=6,nrow=nrow(resparameters))
  colnames(resmodel)=c("nbselect","nbdiff","nbselectmodel","auc","sensibilite","specificite")

  print(nrow(resparameters))
  for (i in 1:nrow(resparameters)){
    print(paste("para",i))
    resmodel[i,]<-bestmodel(tabdecouv = learning ,tabval = validation ,parameters=resparameters[i,] )
  }

  resparameters<-cbind(resmodel,resparameters)
  
  })
output$testparameters<-renderDataTable({
  resparameters<-tabparameters()
  cbind(Names=rownames(resparameters),resparameters)},
  options = list(    "orderClasses" = F,
                     "responsive" = F,
                     "pageLength" = 100))


output$dataparam<-renderTable({
  resparameters<-tabparameters()
  
})
output$downloaddatatestparameters <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(   tabparameters(), file ) })




#######
# PREDICT<-reactive({
#   res<-MODEL()
#   if(is.null(input$predictionfile)){return()
#   }#Pas de fichier
#   
#   if(!is.null(input$predictionfile)  ){
#     
#     if(input$confirmdatabuttonpred==0){
#       datapath<- input$predictionfile$datapath
#       tabprediction<<-importfile(datapath = datapath,extension = input$filetypepred,
#                             NAstring=input$NAstringpred,sheet=input$sheetnpred,skiplines=input$skipnpred,dec=input$decpred,sep=input$seppred)
#       if(input$changedata){
#         tabprediction<<-transformdata(toto = tabprediction,nrownames=input$nrownamespred,ncolnames=input$ncolnamespred,
#                                  transpose=input$transposepred,zeroegalNA=input$zeroegalNApred)
#       }
#       resprediction<-data.frame()
#       
#     }
#     if(input$confirmdatabuttonpred!=0){
#       for (i in 1:ncol(tabprediction)){
#        tabprediction[,i]<-as.numeric(as.character(tabprediction[,i]))
#       }
# 
#        learning<-res$decouverte$decouvdiff
#        select<-which(colnames(tabprediction)%in%colnames(learning))
#        tabprediction<-tabprediction[,select]
# 
# 
#        if(input$NAstructure){
#          varstructure<-colnames(NASTRUCT()$NAstructure)
#          for (i in 1:length(varstructure)){
#            tabprediction[is.na(tabprediction[,varstructure[i]]),varstructure[i]]<-0 }
#        }
# 
#        if(input$log) { 
#          tabprediction<-log(x = tabprediction+1,base = 2)}
#        if(input$scaled){
#          tabprediction<-scale(tabprediction, center = F, scale = TRUE)
#        }
# 
#        #if ( !"class"%in%colnames(tabprediction)){
#          class<-rep(NA,times=nrow(tabprediction))
#        tabprediction<-cbind(class,tabprediction)
# 
#        alldata<-rbind(tabprediction,learning)
#      if(input$rempNA=="moygr"){ 
#          print("impossible de remplacer les NA par la moy par group pour la validation")
#          tabpredictionssNA<-replaceNA(toto = tabprediction,rempNA ="moy")        }
#        else{tabpredictionssNA<-replaceNA(toto = tabprediction,rempNA =input$rempNA)}
#     tabprediction<-tabpredictionssNA[,-1]
# 
#     ######prediction
#     lev<-res$groups
#     model<-res$model
#     if(input$model=="randomforest"){
#       score <-predict(object=model,type="prob",newdata = tabprediction)[,lev["positif"]]
#       predictclass<-vector(length = length(score) ) 
#       predictclass[which(score>=input$thresholdmodel)]<-lev["positif"]
#       predictclass[which(score<input$thresholdmodel)]<-lev["negatif"]
#       predictclass<-as.factor(predictclass)
# 
#     }
# 
#     if(input$model=="svm"){
#       score =attr(predict(model,newdata =  tabprediction,decision.values=T),"decision.values")
#       if(sum(lev==(strsplit(colnames(score),split = "/")[[1]]))==0){score<-score*(-1)}
#       
#       colnames(score)
#       predictclass<-vector(length = length(score) ) 
#       predictclass[which(score>=input$thresholdmodel)]<-lev["positif"]
#       predictclass[which(score<input$thresholdmodel)]<-lev["negatif"]
#       predictclass<-as.factor(predictclass)
# 
#     }
#     if(sum(lev==(levels(predictclass)))==0){
#       predictclass<-factor(predictclass,levels = rev(levels(predictclass)),ordered = TRUE)
#     }
# 
#     resprediction<<-data.frame("score"=score,"predictclass"=predictclass)
# 
#     colnames(resprediction)<-c("score","predictclass")
#     }
#   }
#   parameters<<-res$parameters
#   list("tab"=tabprediction,"parameters"=parameters,"resprediction"=resprediction)
#   
# }) 
#  
# #####
# output$JDDpredict=renderDataTable({
#   respredict<-PREDICT()
#   predict<-PREDICT()$tab
#   colmin<-min(ncol(predict),100)
#   rowmin<-min(nrow(predict),100)
#   cbind(Names=rownames(predict[1:rowmin,1:colmin]),predict[1:rowmin,1:colmin])},
#   options = list(    "orderClasses" = F,
#                      "responsive" = F,
#                      "pageLength" = 10))
# 
# 
# output$parameters=renderTable({
#   parameters<-PREDICT()$parameters
#   cbind(Names=rownames(parameters),parameters)})
# 
# output$resprediction=renderTable({
#   resprediction<-PREDICT()$resprediction
#   resprediction}) 
# 
# 
# 
# output$plotscorepred <- renderPlot({
#   scorepredict<-PREDICT()$resprediction$score
#   score<-MODEL()$decouverte$resmodeldecouv$scoredecouv
#   densityscore(score,scorepredict,maintitle="Density learning's score and prediction score",threshold=input$thresholdmodel)
#   })

}) 

library(shiny)

shinyUI(fluidPage(
  
  # Application title
    titlePanel("APP"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
    sidebarLayout(
        sidebarPanel(
            wellPanel( 
              
              conditionalPanel(condition ="input.confirmdatabutton==0" ,
                      radioButtons("analysis","",c("new analysis","previous analysis"),inline=T),
                        conditionalPanel( condition="input.analysis=='previous analysis' ",     
                        fileInput("modelfile",label=h4("previous analysis"),accept=".RData")), 
              conditionalPanel( condition="input.analysis=='new analysis' ",
                                fluidRow(column(12,br(),radioButtons("filetype", "Extention of the file",
                                                                     c("csv" = "csv", "xlsx ou xls" = "xlsx")))),
                        fluidRow( column(12,conditionalPanel(condition ="input.help",
                                                             helpText("Learning file is obligatory to continue")),
                                         fileInput("learningfile", label = h4("learning File"),
                                                     accept =  c("text/csv","application/vnd.ms-excel",
                                                                 "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xls",".xlsx")) ),
                            column(12,fileInput("validationfile", label = h4("validation File "),
                                  accept =  c("text/csv","application/vnd.ms-excel",
                                              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xls",".xlsx")) )
                            
                        ),
                        
                        fluidRow(
                          column(6,textInput('dec', 'character for decimal point',value = "." )),
                          column(6,textInput("NAstring", label = "characters for missing values",value = "NA"))),   
                        fluidRow(
                          conditionalPanel(condition ="input.filetype=='csv' ",
                                           
                                helpText("For csv extension"),
                                radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t') )),
                          conditionalPanel(condition ="input.filetype=='xlsx' ",
                                   helpText("For excel extension"),
                                   numericInput("skipn",label = "number of lines to skip",value = 0),
                                   numericInput("sheetn",label = "sheet",value = 1))),
                        
                        checkboxInput("changedata",h3("Transformation data"),value = FALSE),
                        #conditionalPanel(condition ="input.fileselect==true",
                           fluidRow(column(6,numericInput("nrownames",label = "rownames",value = 1)),
                                    column(6,numericInput("ncolnames",label = "colnames",value = 1))),
                            checkboxInput("transpose","Transpose the table",FALSE),
                            checkboxInput("zeroegalNA","consider 0 as NA",FALSE))
                        ,
                        actionButton("confirmdatabutton","Confirm data"),
                        conditionalPanel(condition ="input.help",
                                         helpText("Data have to be confirm to continue"))
       ),
       
       conditionalPanel(condition ="input.confirmdatabutton!=0",
          h4("Learning data"),
          textOutput("namefilelearn",inline=T), tags$head(tags$style("#namefilelearn{color: grey;font-size: 15px;font-style: italic;}")),
          br(),
          textOutput("dim1learn",inline=T), "lines (individuals)",br(),
          textOutput("dim2learn",inline=T), "columns (variables)",
          br()),
      conditionalPanel(condition ="input.confirmdatabutton!=0 & output.fileUploadedval",
          h4("Validation data"),
          textOutput("namefileval",inline=T), tags$head(tags$style("#namefileval{color: grey;font-size: 15px;font-style: italic;}")),
          br(),
          textOutput("dim1val",inline=T), "lines (individuals)",br(),
          textOutput("dim2val",inline=T), "columns (variables)",
          br()), 
       conditionalPanel(condition ="input.confirmdatabutton!=0",
          hr(),
          radioButtons("paramdownplot","Download images as : ",
                       choices=list("png"="png","jpg"="jpg","pdf"="pdf"),selected="png"),
          radioButtons("paramdowntable","Download datasets as : ",
                       choices=list("csv"="csv","xlsx"="xlsx"),selected="csv"),
          hr(),
          downloadButton("savestate","Save state",class = "dlButton")
      ),
      hr(),
      checkboxInput("help","show help",FALSE)
      ),width=3      
      ) ,

        mainPanel(
            conditionalPanel(condition ="!output.fileUploaded & !output.modelUploaded",
                      imageOutput("image1", height = 300)),           

            conditionalPanel(condition ="output.fileUploaded || output.modelUploaded",
              tabsetPanel(id = "data",              
                tabPanel("Learning Data",
                    br(),
                    conditionalPanel(condition ="input.help",
                                     helpText("To verify if the import parameters are correct : the first column have to be the group,
                                              the Non attributes values have to appears empty, ")),
                    dataTableOutput("JDDlearn"),
                    p(downloadButton("downloaddataJDDlearn","Download dataset"),align="center")
                    
                    ),
                                 
                tabPanel("Validation Data",
                         conditionalPanel(condition ="output.fileUploadedval",
                                          
                         br(),
                         dataTableOutput("JDDval"),
                         p(downloadButton("downloaddataJDDval","Download dataset"),align="center")
                )),
                
            

                  tabPanel("Select Data", 
                           conditionalPanel(condition ="input.help",
                                            helpText(" Select variables to keep from the learning dataset according to the number ot the structure of Non-Attribute values (missing values)")),  
                    fluidRow(
                        column(7, numericInput("prctNA","% NA accepted" , 100, min = 0, max = 100, step = 5),
                               conditionalPanel(condition ="input.help",
                                                helpText("If by group is unchecked, select variables with less than % of missing values" ))
                              ),
                        column(5, checkboxInput("NAgroup", "Select NA by group " , value = FALSE),
                               conditionalPanel(condition ="input.help",
                                                helpText("Calculate the % of missing values by group")),
                               conditionalPanel(condition ="input.NAgroup",
                               radioButtons("restrict"," ",c("each group has less than x% of NA "=TRUE,"at least one group has less than x% of NA"=FALSE))))
                         ),
                    hr(),
                    fluidRow(
                      column(7,textOutput("nvarselect",inline=T), "selected variables",
                             plotOutput("heatmapNA",width = "100%",height = 500),
                             p(downloadButton("downloadplotheatmapNA","Download plot"),
                               downloadButton('downloaddataheatmapNA', 'Download raw data'),align="center")),
                    column(5,br(),
                           conditionalPanel(condition ="input.help",
                                                   helpText("The 3 curves present the number of variables selected for the three possible options and the % of Na's selected")),
                           plotOutput("plotNA",width = "100%",height = 500),
                           p(downloadButton("downloadplotNA","Download plot"),
                             downloadButton('downloaddataplotNA', 'Download raw data'),align="center")
                    )),
                    hr(),
                    fluidRow(
                      column(6,  checkboxInput("NAstructure", "Select metabolites with a NA's structure " , value = FALSE),
                             
                              conditionalPanel(condition ="input.help",
                                      helpText("proceed a proportion test of the Non Attributes values between the 2 groups."),
                                      helpText("Consider the NA in the group with less values as real 0 (replace by 0) the NA in the group with more values are raplace by the solution chosen before")),
                             conditionalPanel(condition ="input.NAstructure==true",
                                      numericInput("thresholdNAstructure","pvalue for the structure test" , 0.05, min = 0, max = 1, step = 0.005))),
                      conditionalPanel(condition ="input.NAstructure==true",
                                       column(6,
                             radioButtons("structdata", "search structure in : ",
                                            c("all dataset" = "alldata","selected dataset" = "selecteddata"))))
                        ), 
                hr(),
                fluidRow(
                  conditionalPanel(condition ="input.NAstructure==true", 
                                   column(9,plotOutput("heatmapNAstructure" ,width = "95%",height = 600),
                                          p(downloadButton("downloadstructur","Download plot"),
                                            downloadButton('downloaddatastructur', 'Download raw data'),align="center")),
                                   column(3,br(),br(),
                                          numericInput("maxvaluesgroupmin","The group with the minimum of values has at most x% of values",value = 25,min = 0,max = 100,step = 5),
                                          numericInput("minvaluesgroupmax","The group with the maximum of values has at least y% of values",value = 75,min = 0,max = 100,step = 5)))
                )),
                tabPanel("Transform Data", 
                         fluidRow(
                           column(6,radioButtons("rempNA", "Replacing NA by:",
                                                 c("zero" = "z","mean of the metobolite" = "moy",
                                                   "mean by group"="moygr","PCA estimation" = "pca","Random forest estimation"="missforest"))),
                           column(6,checkboxInput("log","transform data in log",FALSE),
                           checkboxInput("scaled","scaled dataset",FALSE))),
                                hr(),
                         
                         fluidRow(
                           column(10,plotOutput("plotheatmaptransformdata" ,width = "100%",height = 600),
                                  p(downloadButton("downloadplotheatmap","Download plot"),
                                  downloadButton('downloaddataheatmap', 'Download raw data'),align="center"))
                         ),  
                         hr(),
                         fluidRow(conditionalPanel(condition ="input.help",
                                                   helpText("The mds (MultiDimensionnal Scaling) calcul the distances between the individuals (rows) and represented it on a plan as well as possible"),
                                                   helpText("The aim of this graphic is to vizualized if the selection and transform parameters separate well the 2 groups")),
                            plotOutput("plotmds", width = "100%",height = 500)),
                            p(downloadButton("downloadplotmds","Download plot"),
                            downloadButton('downloaddatamds', 'Download raw data'),align="center")
                ),  
              
               
                tabPanel("Tests",
                         fluidRow(
                           column(6,
                         radioButtons("test", "Tests",
                                      c( "No test"="notest",
                                         "Wilcoxon Test" = "Wtest",
                                         "Student Test" = "Ttest")),
                         conditionalPanel(condition ="input.help",helpText("The test will select the differently expressed variables. The willcoxon test (Mann-Whitney-Willcoxon test) is a non parametric test. The student test is parametrics (the group has to be normally distribute and  the variance equal (or effectve superior to 30))")),
                         checkboxInput("SFtest","Shapiro and Fisher Tests",F)),
                         conditionalPanel(condition ="input.help",
                                          helpText("The Fisher test test the equality of variance of each column, shapiro test the normality")),
                         column(6,br(),
                                numericInput("thresholdFC","choise of the Foldchange threshold %" , 1, min =0, max = 5, step = 0.5),
                                conditionalPanel(condition ="input.help",
                                                 helpText("mesure of the difference between the means of the 2 groups")),
                                numericInput("thresholdpv","choise of the p-value threshold %" , 0.05, min =0, max = 1, step = 0.01),
                                checkboxInput("adjustpv", "adjust p-value " , value = FALSE),
                                conditionalPanel(condition ="input.help", helpText("Benjamini & Hochberg correction")))),
                         hr(),
                         conditionalPanel(
                           condition= "input.test== 'Wtest' || input.test== 'Ttest'",
                           fluidRow(
                               column(6,
                                      textOutput("nvarselect2",inline=T), "selected variables",
                                      plotOutput("plottest1" ,width = 500,height = 500),
                                      p(downloadButton("downloadplottest1","Download plot"),
                                        downloadButton('downloaddatatest1', 'Download raw data'),align="center")),
                               column(6,textOutput("nbdiff",inline=T), "differently expressed",
                                      plotOutput("plottest2" ,width = 400,height = 500),   
                                      p(downloadButton("downloadplottest2","Download plot"),
                                        downloadButton('downloaddatatest2', 'Download raw data'),align="center")
                               ))),
                           conditionalPanel(condition ="input.SFtest==true  ",
                                  column(6,conditionalPanel(condition ="input.help",
                                                            helpText("Barplot presents the Results of shapiro and Fisher test")),
                                          plotOutput("plottestcondition"))
                                  )
                ),
                tabPanel("Model",
                        fluidRow(
                            column(6,
                         radioButtons("model", "Type of model to adjust :",
                                      c("No model" = "nomodel",
                                        "Random Forest"="randomforest",
                                        "Support Vector Machine" = "svm"))
                         ),
                            column(6,
                         numericInput("thresholdmodel","threshold model" ,0, min = -1, max = 1, step = 0.05),
                         conditionalPanel(condition ="input.help",
                                          helpText("The threshold of the score is used for the validation")),
                         checkboxInput("fs","features selection by cross validation",F)
                        )),
                         
                        
                        conditionalPanel(condition ="output.fileUploadedval & input.model!='nomodel'  ",
                                checkboxInput("adjustval","Adjust model on validation data",F)),
                        hr(),
                        conditionalPanel(condition ="input.model!='nomodel'  ",
                         fluidRow(div(
                           column(6,textOutput('nbselectmodel',inline=T),'selected variables',
                                  h3("model learning")),
                           column(6,br(),downloadButton('downloaddatalearning', 'Download learning data')))),
                         fluidRow(
                           column(6,plotOutput("plotmodeldecouvroc"),
                                  p(downloadButton("downloadplotdecouvroc","Download plot"),
                                    downloadButton('downloaddatadecouvroc', 'Download raw data'),align="center")),
                           column(4,plotOutput("plotmodeldecouvbp"),
                                  p(downloadButton("downloadplotmodeldecouvbp","Download plot"),
                                    downloadButton('downloaddatamodeldecouvbp', 'Download raw data'),align="center")),
                           column(2,radioButtons("plotscoremodel", "",c( "boxplot"="boxplot","points" = "points")),
                                  textOutput("positif",inline=T), " = positif",br(),
                                  textOutput("negatif",inline=T), " = negatif"),br(),
                           tableOutput("tabmodeldecouv"),
                                  "Sensibility = ",textOutput("sensibilitydecouv",inline=T), br(),
                                  "Specificity = ",textOutput("specificitydecouv",inline=T)
                           )
                         ),
                         hr(),
                         conditionalPanel(condition ="input.adjustval==true  ",
                        fluidRow(div(
                          column(6,h3("model validation")), 
                          column(6,br(),downloadButton('downloaddatavalidation', 'Download validation data')))), 
                          
                        fluidRow(
                            column(6,plotOutput("plotmodelvalroc"),
                                   p(downloadButton("downloadplotvalroc","Download plot"),
                                     downloadButton('downloaddatavalroc', 'Download raw data'),align="center")),
                            column(4,plotOutput("plotmodelvalbp"),
                                   p(downloadButton("downloadplotmodelvalbp","Download plot"),
                                     downloadButton('downloaddatamodelvalbp', 'Download raw data'),align="center")),
                            column(2,tableOutput("tabmodelval"),
                                   "Sensibility = ",textOutput("sensibilityval",inline=T), br(),
                                   "Specificity = ",textOutput("specificityval",inline=T))
                            ),
                         checkboxInput("invers", "inverse positive/negative modalities " , value = FALSE)
                )),
                tabPanel("Analys of the model",
                         h3("Summary of the model"),
                         verbatimTextOutput("summarymodel"),
                         plotOutput("plotimportance"),hr(),
                         plotOutput("plotcorrelation",width = 500,height = 500),
                         p(downloadButton("downloadplotcorrelation","Download plot"),
                           downloadButton('downloaddatacorrelation', 'Download raw data'),align="center")
                ),
                tabPanel("Best parameters",
                    h3("Parameters selection for testing"),
                    conditionalPanel(condition ="input.help",
                                     helpText("Th application will calculated all the different models with all the parameters selected. "), 
                                     helpText("The criterion we use is the AUC ( area under the roc curve) of the validation"),
                                     helpText("This procedure can be long (15 min) if the parameters are all selected")),
                    conditionalPanel(condition ="input.help",
                                     helpText("compute all paramaters to find the best according to the AUC of the validation")),
                    fluidRow(
                      column(4,h3("Select Data"),
                    sliderInput("prctNAtest", h4("Percent of NA accepted"),min = 0, max = 100, value = c(10,50),width="60%"),
                    checkboxGroupInput("NAgrouptest", label = h4("Select NA by group"), 
                                       choices = list("TRUE" = TRUE, "FALSE" = FALSE),
                                       selected = 1),
                    checkboxGroupInput("restricttest", label = h3(""), 
                                       choices = list("each group" = TRUE, "at least one group" = FALSE),
                                       selected = 1),
                    h4("has less than x% of NA"),hr(),
                    h3("Transform Data"),
                    checkboxGroupInput("logtest", label = h4("transform data in log"), 
                                       choices = list("TRUE" = TRUE, "FALSE" = FALSE),
                                       selected = 2),
                    checkboxGroupInput("scaledtest", label = h4("scaled dataset"), 
                                       choices = list("TRUE" = TRUE, "FALSE" = FALSE),
                                       selected = 2)),
                    column(4,checkboxGroupInput("rempNAtest", label = h4("Replace NA with"), 
                                       choices = list("zero" = "z", "mean" ="moy", "mean by group" = "moygr",
                                                      "pca"="pca","randomforest"="randomforest"),
                                       selected = 1),
                    checkboxGroupInput("NAstructuretest", label = h4("Select NA structure"), 
                                       choices = list("TRUE" = TRUE, "FALSE" = FALSE),
                                       selected = 1),
                    numericInput("thresholdNAstructuretest","choise of the pvalue NA structure test" ,value =  0.05, min =0, max = 1, step = 0.05),
                    
                    numericInput("maxvaluesgroupmintest","choise of the max % values of group with the minimum values" ,value =  100, min =0, max = 100, step = 5),
                    numericInput("minvaluesgroupmaxtest","choise of the min % values of group with the maximum values" ,value =  0, min =0, max = 100, step = 5)),
                    
                  
                    column(4,
                    checkboxGroupInput("testtest", label = h3("Test"), 
                                       choices = list("no test"="notest","Wilcoxon test" = "Wtest", "Fisher test" = "Ftest"),
                                       selected = 2),
                    checkboxGroupInput("adjustpvaltest", label = h4("use adjust p-value"), 
                                       choices = list("TRUE" = TRUE, "FALSE" = FALSE),
                                       selected = 1),
                    numericInput("thresholdFCtest","choise of the Foldchange threshold %" , 1, min =0, max = 5, step = 0.5),
                    numericInput("thresholdpvtest","choise of the p-value threshold %" , 0.05, min =0, max = 1, step = 0.01),  hr(),
                    checkboxGroupInput("modeltest", label = h3("Model"), 
                                       choices = list("svm"="svm","randomforest" = "randomforest"),
                                       selected = 1),
                    checkboxGroupInput("fstest","features selection by cross validation",choices = list("TRUE" = TRUE, "FALSE" = FALSE),
                                       selected = 1)
                    )),
                    actionButton("tunetest","Test all models"),
                    dataTableOutput("testparameters"),
                    p(downloadButton('downloaddatatestparameters', 'Download raw data'),align="center")                    
                )
             
#                 ,
#                 tabPanel("Prediction",
#                          wellPanel(
#                             fluidRow(
#                               column(4,h3("Download new data"),
#                                   fileInput("predictionfile", label = h4("prediction File "),accept =  c("text/csv","application/vnd.ms-excel",
#                                                          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xls",".xlsx")) ),
#                               column(4,br(),br(),br(),radioButtons("filetypepred", "Extention of the file",c("csv" = "csv", "xlsx ou xls" = "xlsx"))),
#                               column(4,textInput('decpred', 'character for decimal point',value = "." ),
#                                      textInput("NAstringpred", label = "characters for missing values",value = "NA"))),
#                             hr(),
#                          fluidRow( column(4,
#                                   conditionalPanel(condition ="input.filetypepred=='csv' ",
#                                                    
#                                   helpText("For csv extension"),
#                                   radioButtons('seppred', 'Separator',c(Comma=',',Semicolon=';',Tab='\t') )),
#                          
#                                   conditionalPanel(condition ="input.filetypepred=='xlsx' ",
#                                   helpText("For excel extension"),
#                                   numericInput("skipnpred",label = "number of lines to skip",value = 0),
#                                   numericInput("sheetnpred",label = "sheet",value = 1))),
#                                   column(4,checkboxInput("changedatapred",h4("Transformation data"),TRUE),
#                                          checkboxInput("transposepred","Transpose the table",FALSE),
#                                          checkboxInput("zeroegalNApred","consider 0 as NA",FALSE)
#                                          ),
#                                   column(4,numericInput("ncolnamespred",label = "colnames",value = 1),
#                                          numericInput("nrownamespred",label = "rownames",value = 1))),
#                                  
#                          hr(),
#                          actionButton("confirmdatabuttonpred","Confirm data")),
#                         hr(),
#                         h3("predict Data"),
#                         dataTableOutput("JDDpredict"),
#                         hr(),
#                         h3("Model parameters"),
#                         tableOutput("parameters"),
#                         hr(),
#                         fluidRow(
#                         column(5,
#                         h3("Prediction, score"),
#                         tableOutput("resprediction")
#                         ),
#                         column(7,br(), plotOutput("plotscorepred",width = "100%",height = 500))
#                          )
#                         )
                         
                         
           ) )  
        )         
        
    )))

 



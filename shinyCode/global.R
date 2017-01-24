## Initialisation of information for shiny demo
# 
# Built for presentation to Citizens Data Meetup group, Melbourne, Australia, 24 Jan 2017
#
# See also:
#   - ui.R and server.R for the shiny app
#   - ../baseCode for cdmFunctions.R which contains the technical parts of the code
#   - ../Documentation for rmarkdown (Rmd) files used in the documentation

# Packages ----
library( shiny )
library( ggplot2 )
library( rmarkdown )
library( neuralnet )
library( shinythemes )

# Data ----
library( MASS )
myData <- Boston
yName <- 'medv'
xNames <- setdiff( names( Boston ), yName )

# External functions ----
source( '../baseCode/cdmFunctions.R' )

# Initial Values ----
SPLIT_SEED = 100
TRAIN_PERCENT = 75
NN_SEED = 500
INC_INT = FALSE
HIDDEN_VEC = c( 5, 3 )
N_CV = 10

# Initialise reactiveValues ----
# Data
splitData <- makeSplitData( myData, trainPercent = TRAIN_PERCENT, splitSeed = SPLIT_SEED )

dataRV <- reactiveValues( splitData=splitData
                          , trainData=splitData$trainData
                          , testData=splitData$testData
                          , scaleTrain=scaleDF( splitData$trainData )
                          , scaleTest=scaleDF( splitData$testData, splitData$trainData )
)

# Models
nnFormula <- buildFormula( yName=yName, xNames=xNames )
lmFormula = buildFormula( yName = yName, xNames = xNames, includeInteractions = INC_INT )
modelRV <- reactiveValues( nnFormula = nnFormula
                           , nnHiddenVec=HIDDEN_VEC
                           , myNN= isolate( trainNN( trainData = dataRV$scaleTrain, nnFormula = nnFormula, hiddenVec = HIDDEN_VEC, nnSeed = NN_SEED ) )
                           , lmFormula = lmFormula
                           , myLM = isolate( trainLM( trainData = dataRV$scaleTrain, lmFormula = lmFormula ) )
)

# Results
resultsRV <- reactiveValues( trainResults=isolate( assessModels( scaledData=dataRV$scaleTrain
                                                        , xNames=xNames
                                                        , yName=yName
                                                        , myNN=modelRV$myNN
                                                        , myLM=modelRV$myLM
                                                        , unCentre=min( dataRV$trainData[,yName] )
                                                        , unScale=max( dataRV$trainData[,yName] ) - min( dataRV$trainData[,yName] ) ) )
                             , testResults= NULL
                             , cvResults=NULL
)

# Plots
plotsRV <- reactiveValues( trainLine=isolate( makeLineGraph( resultsRV$trainResults ) )
                           , testLine=NULL
                           , cvBox=NULL )

# Documentation
docRV <- reactiveValues( makeSlides=FALSE, updateIn=FALSE, updateNumber=0, fileName='../Documentation/initialWriteUp.html', reasons=NULL )
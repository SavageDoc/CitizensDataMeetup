## Comments & Credits ----
# Function definition for CitizensDataMeetup presentation
# Based on the blog and gist of Michy Alice:
#   Blog: https://datascienceplus.com/fitting-neural-network-in-r/
#   Gist: https://gist.github.com/mick001/49fad7f4c6112d954aff

## Package loading ---- 
library( MASS ) # Boston data set
library( neuralnet ) # neural network auxillary functions
library( ggplot2 ) # Graphing

## Function definition ----
# makeSplitData takes a data frame, a training percentage, and an optional random seed
makeSplitData <- function( fullData=NULL
                       , trainPercent=75
                       , splitSeed=NULL ){

  # Check that data are input...
  if( is.null( fullData ) )
    return( NULL )
  
  N <- nrow( fullData )
  
  # Check if a seed is specified
  if( !is.null( splitSeed ) )
    set.seed( seed=splitSeed )
  
  # Get indices of training data -- note use of sample with an integer input
  trainIndex <- sample( N, round( N*trainPercent/100 ), replace=FALSE )
  
  trainData <- fullData[trainIndex,]
  testData <- fullData[-trainIndex,]
  
  splitData <- list( trainData=trainData, testData=testData )
  # Return the data split
  return( splitData )
}

# buildFormula takes the dependent & independent variables, and returns a formula. Also takes an optional argument for interactions. 
buildFormula <- function( yName, xNames, includeInteractions=FALSE ){
  if( !includeInteractions ){
    # Formula will be of the form: yName ~ xNames[1] + xNames[2] + ... + xNames[n]
    myFormula <- paste( yName, '~', paste( xNames, collapse='+' ), sep=' ' )
  } else {
    # Formula will be of the form: yName ~ (xNames[1] + xNames[2] + ... + xNames[n])^2
    myFormula <- paste( yName, '~ (', paste( xNames, collapse='+' ), ')^2', sep=' ' )
  }
  return( myFormula )
}
# scaleDF takes a data frame to scale, and an optional reference data frame to scale to. 
scaleDF <- function( scaleMe, scaleRef=NULL ){
  # If there's only one argument, scale according to itself
  if( is.null( scaleRef ) )
    scaleRef <- scaleMe
  
  # For each column...
  maxs <- apply( scaleRef, 2, max )
  mins <- apply( scaleRef, 2, min )
  
  # Scale the data: If self-scaled, all columns then \in [0,1]
  # (But not if the reference data is off...)
  scaledData <- scale( scaleMe, center=mins, scale=maxs-mins )
  
  # scaledData contains attributes that we won't want in other parts of the program...
  return( as.data.frame( scaledData ) )
}
# trainNN trains a neural network via neuralnet::neuralnet with the following parameters:
# trainData: See splitData to generate training data.
# formula: See buildFormula function.
# hiddenVec: Vector of hidden nodes. I've only tested it for scalars and length-2 vectors....
# ...: If you'd like to include more parameters into neuralnet, feel free!
trainNN <- function( trainData=NULL
                     , nnFormula=NULL
                     , hiddenVec=NULL
                     , nnSeed=NULL
                     , ... ){
  nnPack <- require( neuralnet )
  # Error checking...
  stopifnot( nnPack & !is.null( trainData ) & !is.null( nnFormula ) & !is.null( hiddenVec ) )
  
  if( !is.null( nnSeed ) )
    set.seed( nnSeed )
  
  
  myNN <- neuralnet( formula=nnFormula
                     , hidden = hiddenVec
                     , data = trainData
                     , ... # User-supplied inputs to neuralnet
                     )
  
  return( myNN )
}
# trainLM "trains" a linear regression in an analogous manner to trainNN.
# trainLM takes the same inputs as trainNN, without the hiddenVec or initialisation seed.
trainLM <- function( trainData=NULL
                     , lmFormula=NULL
                     , ... # Additional inputs to lm
                     ){
  # Error checking...
  stopifnot( !is.null( trainData ) & !is.null( lmFormula ) )
  
  myLM <- lm( formula=lmFormula
                     , data = trainData
                     , ... # User-supplied inputs to lm
  )
  
  return( myLM )
}
# assessModels looks at predicted vs observed data for a LM and NN model.
assessModels <- function( scaledData, xNames, yName, myNN, myLM, unCentre=0, unScale=1 ){
  
  predScaleNN <- compute( myNN, scaledData[,xNames] )$net.result
  predScaleLM <- predict( myLM, scaledData[,xNames] )
  
  predNN <- unscalePredictions( predScaleNN
                                , unCentre=unCentre
                                , unScale =unScale )
  predLM <- unscalePredictions( predScaleLM
                                , unCentre=unCentre
                                , unScale =unScale )
  obsData <- unscalePredictions( scaledData[,yName]
                                 , unCentre=unCentre
                                 , unScale =unScale )
  assessment <- data.frame( Observed=obsData, predNN=predNN, predLM=predLM )
  return( assessment )
}
# unscalePredictions converts outputs \in [0,1] to applicable values based on centre and scale parameters.
unscalePredictions <- function( scaledPred
                                , unCentre=0 # Default to no additive/translational scaling
                                , unScale=1  # Default to no multiplicative/range scaling
                                ){
  # From scaled=(unscaled-centre)/scale -> unscaled=scaled*scale+centre
  unscalePred <- scaledPred*unScale + unCentre
  return( unscalePred )
}
# makeLineGraph creates a ggplot object for Observed/NN/LM comparisons.
# Takes a data frame input with columns:
#   Observed: Observations
#   predNN: Predictions from Neural Network
#   predLM: Predictions from Linear Model
makeLineGraph <- function( predDF = NULL ){
  if( is.null( predDF ) )
    return( NULL )
  
  # Packages
  ggPack <- require( ggplot2 )
  rcbPack <- require( RColorBrewer )
  
  # Error checking
  stopifnot( ggPack & rcbPack & all( c('Observed', 'predNN', 'predLM') %in% names( predDF ) ) )
  
  nnMSE <- round( calcMSE( predDF$Observed, predDF$predNN ), 2 )
  lmMSE <- round( calcMSE( predDF$Observed, predDF$predLM ), 2 )
  # Make the graph...
  lineGraph <- ggplot( data=predDF, aes( x=Observed ) ) + # Observed on x-axis
    geom_point( aes( y=predNN, colour='NN' ) ) +  # Add points for NN predictions
    geom_point( aes( y=predLM, colour='LM' ) ) +  # Add points for LM predictions
    geom_abline( slope=1, intercept=0 ) + # Add comparison line
    labs( x='Observed', y='Predicted', title=paste( 'Comparison of Neural Network (MSE :', nnMSE,') and Linear Models (MSE:', lmMSE, ')' ) ) + # Labels
    guides( colour=guide_legend( title='Algorithm' ) ) + # Change name of colour legend
    theme( legend.position='bottom' ) + # Useful for some graph options (like shiny, IMHO)
    scale_colour_brewer( type='qual' ) # Qualitative colours
  
  return( lineGraph )
}
# makeBoxGraph creates a boxplot with LM and NN results from cross-validation.
# Input is a data frame with assumed columns:
#   mseNN: Mean squared error from NN predictions
#   mseLM: Mean squared error from LM predictions
makeBoxGraph <- function( cvResults=NULL ){
  
  if( is.null( cvResults ) )
    return( NULL )
  
  ggPack <- require( ggplot2 )
  
  # Error checking
  stopifnot( ggPack & all( c('mseLM', 'mseNN') %in% names( cvResults ) ) )
  
  # Make the graph....
  boxGraph <- ggplot( data=cvResults ) +
    geom_boxplot( aes( x='NN', y=mseNN, colour='NN' ) ) +
    geom_boxplot( aes( x='LM', y=mseLM, colour='LM' ) ) +
    labs( x='Algorithm', y='MSE', title='Comparison of multiple runs of NN and LM' ) +
    xlim( 'NN', 'LM' ) +
    guides( colour=guide_legend( title='Algorithm' ) ) +
    theme( legend.position='bottom' ) +
    scale_colour_brewer( type='qual' )
    
  return( boxGraph )
}
# calcMSE calculates the mean squared error between observations and predictions
calcMSE <- function( obs, pred ){
  mse <- mean( (obs-pred)^2 )
  return( mse )
}

doCrossValidation <- function( fullData # Full data set
                               , xNames # Independent variable names 
                               , yName # Dependent variable name
                               , N # Number it iterations
                               , trainPercent, splitSeed=NULL # Arguments to split into training/test data
                               , hiddenVec, nnSeed=NULL # Arguments to train neural network
                               , includeInteractions # Arguments to calculate LM
){

  utilPack <- require( utils )
  stopifnot( utilPack & all( c(xNames, yName) %in% names( fullData ) ) )
  
  modelData <- fullData[,c(xNames, yName)]
  
  nnFormula <- buildFormula( yName, xNames )
  lmFormula <- buildFormula( yName, xNames, includeInteractions = includeInteractions )

  cvResults <- data.frame( mseNN=rep( 0, N ), mseLM=rep(0,N) )
  wpb <- winProgressBar( title='Cross Validation Progress', label=paste( '0 of', N, 'completed...', sep=' ' ), min=0, max=N )
  
  for( k in 1:N ){
    if( k == 1 ){
      splitData <- makeSplitData( modelData, trainPercent, splitSeed )
      scaleTrain <- scaleDF( splitData$trainData )
      myNN <- trainNN( trainData = scaleTrain
                       , nnFormula = nnFormula 
                       , hiddenVec = hiddenVec
                       , nnSeed = nnSeed)
      FIRST_ITER <<- list( myNN=myNN, splitData=splitData )
    } else {
      splitData <- makeSplitData( modelData, trainPercent )
      scaleTrain <- scaleDF( splitData$trainData )
      myNN <- trainNN( trainData = scaleTrain
                       , nnFormula = nnFormula 
                       , hiddenVec = hiddenVec
                       )
    }
    myLM <- trainLM( trainData = scaleTrain
                     , lmFormula = lmFormula )
    
    scaleTest <- scaleDF( splitData$testData, splitData$trainData )
    
    testResults <- assessModels( scaleTest, xNames, yName, myNN, myLM
                                 , unCentre=min( splitData$trainData[,yName] )
                                 , unScale=max(splitData$trainData[,yName] )-min(splitData$trainData[,yName] ) )
    
    if( k == 1 )
      TEST_RESULTS <<- testResults
    
    cvResults[k, 'mseNN'] <- calcMSE( testResults$Observed, testResults$predNN )
    cvResults[k, 'mseLM'] <- calcMSE( testResults$Observed, testResults$predLM )
    setWinProgressBar( wpb, value=k, label=paste( k, 'of', N, 'completed...', sep=' ' ) )
  }
  
  close( wpb )
  return( cvResults )
}
## Server (back-end) information for demonstration to Citizens Data Meetup group in Melbourne, Australia, on 24 Jan, 2017
#
# See also:
#   - ui.R and global.R within this directory
#   - ../baseCode/cdmFunctions.R for most of the guts of the code
#   - ../Documentation for rmarkdown (Rmd) files used in producing the documentation.

library(shiny)

shinyServer(function(input, output, session) {
  
  # Updates to data (Note reactive dependencies!) ----
  observeEvent( c( input$trainPercent, input$splitSeed ), {
    dataRV$splitData <- makeSplitData( myData, trainPercent = input$trainPercent, splitSeed = input$splitSeed )
    dataRV$trainData <- dataRV$splitData$trainData
    dataRV$testData <- dataRV$splitData$testData
    dataRV$scaleTrain <- scaleDF( dataRV$splitData$trainData )
    dataRV$scaleTest <- scaleDF( dataRV$splitData$testData, dataRV$splitData$trainData )
  })
  
  # Updates to models (Note reactive dependencies!) ----
  observeEvent( input$xNames, modelRV$nnFormula <- buildFormula( yName=yName, xNames=input$xNames ) )
  observeEvent( eventExpr = c( input$nnNodes1, input$nnNodes2 ), 
                {if( input$nnNodes2 == 0 )
                  modelRV$nnHiddenvec <- input$nnNodes1
                else
                  modelRV$nnHiddenVec <- c( input$nnNodes1, input$nnNodes2 )
                })
  observeEvent( list( scaletrain=dataRV$scaleTrain, nnFormula=modelRV$nnFormula, hiddenVec=modelRV$nnHiddenVec, nnSeed=input$nnSeed )
                ,   modelRV$myNN <- trainNN( trainData = dataRV$scaleTrain
                                             , nnFormula = modelRV$nnFormula
                                             , hiddenVec = modelRV$nnHiddenVec
                                             , nnSeed = input$nnSeed )
  )
  
  observeEvent( c( input$xNames, input$incInt ),  
                modelRV$lmFormula <- buildFormula( yName = yName, xNames = input$xNames, includeInteractions = input$incInt ) )
  
  observeEvent( list( trainData=dataRV$scaleTrain, lmFormula=modelRV$lmFormula ), 
                modelRV$myLM <- trainLM( trainData = dataRV$scaleTrain, lmFormula = modelRV$lmFormula ) )
  
  # Updates to results (Note reactive dependencies!) ----
  observeEvent( list( data=dataRV$splitData, modelNN=modelRV$myNN, modelLM=modelRV$myLM ), {
    resultsRV$trainResults <- assessModels( scaledData=dataRV$scaleTrain
                                            , xNames=input$xNames
                                            , yName=yName
                                            , myNN=modelRV$myNN
                                            , myLM=modelRV$myLM
                                            , unCentre=min( dataRV$trainData[,yName] )
                                            , unScale=max( dataRV$trainData[,yName] ) - min( dataRV$trainData[,yName] ) 
                                            
    )
    resultsRV$testResults <- assessModels( scaledData=dataRV$scaleTest
                                           , xNames=input$xNames
                                           , yName=yName
                                           , myNN=modelRV$myNN
                                           , myLM=modelRV$myLM
                                           , unCentre=min( dataRV$trainData[,yName] )
                                           , unScale=max( dataRV$trainData[,yName] ) - min( dataRV$trainData[,yName] ) )
  })
  
  
  observeEvent( input$goCV, {
    resultsRV$cvResults <- doCrossValidation( fullData=myData
                                              , xNames=input$xNames
                                              , yName=yName
                                              , N = input$nCV
                                              , trainPercent=input$trainPercent
                                              , splitSeed=input$splitSeed
                                              , hiddenVec=c( input$nnNodes1, input$nnNodes2 )
                                              , nnSeed = input$nnSeed
                                              , includeInteractions = input$incInt ) 
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent( input$decision,{
    docRV$makeSlides <- FALSE
    docRV$reasons <- NULL
  })
  
  # Updates to Plots ----
  observeEvent( resultsRV$trainResults, plotsRV$trainLine <- makeLineGraph( resultsRV$trainResults ) )
  observeEvent( resultsRV$testResults, plotsRV$testLine <- makeLineGraph( resultsRV$testResults ) )
  observeEvent( resultsRV$cvResults, {
    plotsRV$cvBox <- makeBoxGraph( resultsRV$cvResults ) 
    # Sneaky update to documentation...
    unlink('../Documentation/shinyWriteUp_cache', recursive = TRUE)
    render( '../Documentation/shinyWriteUp.Rmd' )
    docRV$updateIn <- TRUE
  }) 
  observeEvent( input$goReason, {
    docRV$reasons <- c( docRV$reasons, input$decReasons ) 
    updateTextInput( session, inputId='decReasons', value='' )
  })
  
  observeEvent( input$goSlides,{
    
    render( input$decision, output_file = '../Documentation/slidesOut.html' )
    
    docRV$makeSlides <- TRUE
  })
  
  # Output definitions ----
  output$inDoc <- renderUI({
    if( docRV$updateIn )
      includeHTML( '../Documentation/shinyWriteUp.html' )
    else
      includeHTML( '../Documentation/initialWriteUp.html' )
  })
  
  output$trainSummary <- renderPrint( summary( dataRV$trainData[,input$xNames], digits=3 ) )
  output$lineTrain <- renderPlot( plotsRV$trainLine )
  output$lineTest <- renderPlot({ 
    if( is.null( plotsRV$testLine ) )
      return( NULL )
    
    plotsRV$testLine })
  output$testSummary <- renderPrint( summary( dataRV$testData[,input$xNames], digits=3 ) )
  output$boxCV <- renderPlot({
    if( is.null( plotsRV$cvBox ) )
      return( NULL )
    
    plotsRV$cvBox })
  
  output$mseCV <- renderTable({
    
    if( is.null( resultsRV$cvResults ) )
      return( NULL )
    
    cvResults <- resultsRV$cvResults
    names( cvResults ) <- c( 'NN MSE', 'LM MSE')
    rownames( cvResults ) <- seq( from=1,  to=nrow( cvResults ), by=1 )
    cvResults
  }, rownames=TRUE, digits=4, bordered=TRUE, striped=TRUE )
  
  output$listReasons <- renderPrint({
    if( is.null( docRV$reasons ) ) 'None' else docRV$reasons
  })
  
  output$outDoc <- renderUI({

    if( !docRV$makeSlides ){
      h3('Slides not yet generated.' )
    }
    else{
      h3( 'Slides created!' )
    }

  } )
  
})

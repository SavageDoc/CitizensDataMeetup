## Server (back-end) information for demonstration to Citizens Data Meetup group in Melbourne, Australia, on 24 Jan, 2017
#
# See also:
#   - ui.R and global.R within this directory
#   - ../baseCode/cdmFunctions.R for most of the guts of the code
#   - ../Documentation for rmarkdown (Rmd) files used in producing the documentation.

library(shiny)

shinyServer(function(input, output, session) {
  
  # Updates to data (Note reactive dependencies!) ----
  # Update training/test data split. 
  # Reactive to: Training %, Random seed.
  observeEvent( c( input$trainPercent, input$splitSeed ), {
    # makeSplitData is in cdmFunctions.R (sourced in global.R)
    dataRV$splitData <- makeSplitData( myData, trainPercent = input$trainPercent, splitSeed = input$splitSeed )
    
    # Abbreviate...
    dataRV$trainData <- dataRV$splitData$trainData
    dataRV$testData <- dataRV$splitData$testData
    
    # Scale train and test data according to training data
    dataRV$scaleTrain <- scaleDF( dataRV$splitData$trainData )
    dataRV$scaleTest <- scaleDF( dataRV$splitData$testData, dataRV$splitData$trainData )
  })
  
  # Updates to models (Note reactive dependencies!) ----
  #
  # Update the formula if the input names change -- keep the value in the reactiveValues for later use (i.e. documentation)
  observeEvent( input$xNames, modelRV$nnFormula <- buildFormula( yName=yName, xNames=input$xNames ) )
  # Change the hidden nodes - again, update reactiveValues for use in documentation.
  observeEvent( eventExpr = c( input$nnNodes1, input$nnNodes2 ), 
                {if( input$nnNodes2 == 0 )
                  modelRV$nnHiddenvec <- input$nnNodes1
                else
                  modelRV$nnHiddenVec <- c( input$nnNodes1, input$nnNodes2 )
                })
  # NN Update. This is clunky, but illustrates that you can make observers dependent on many things. A reactive() might be easier...
  observeEvent( list( scaletrain=dataRV$scaleTrain, nnFormula=modelRV$nnFormula, hiddenVec=modelRV$nnHiddenVec, nnSeed=input$nnSeed )
                ,   modelRV$myNN <- trainNN( trainData = dataRV$scaleTrain
                                             , nnFormula = modelRV$nnFormula
                                             , hiddenVec = modelRV$nnHiddenVec
                                             , nnSeed = input$nnSeed )
  )
  # LM formula update. Depends on variable selection and inclusion of interactions.
  observeEvent( c( input$xNames, input$incInt ),  
                modelRV$lmFormula <- buildFormula( yName = yName, xNames = input$xNames, includeInteractions = input$incInt ) )
  
  # The LM has fewer inputs than the nn...
  observeEvent( list( trainData=dataRV$scaleTrain, lmFormula=modelRV$lmFormula ), 
                modelRV$myLM <- trainLM( trainData = dataRV$scaleTrain, lmFormula = modelRV$lmFormula ) )
  
  # Updates to results (Note reactive dependencies!) ----
  # If the data or models change, update the train/test results.
  observeEvent( list( data=dataRV$splitData, modelNN=modelRV$myNN, modelLM=modelRV$myLM ), {
    # assessModels is in cdmFunctions.R, sourced in global.R
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
  
  
  # Cross validation only occurs on actionButton
  observeEvent( input$goCV, {
    # doCrossValidation is in cdmFunctions.R, sourced in global.R
    resultsRV$cvResults <- doCrossValidation( fullData=myData
                                              , xNames=input$xNames
                                              , yName=yName
                                              , N = input$nCV
                                              , trainPercent=input$trainPercent
                                              , splitSeed=input$splitSeed
                                              , hiddenVec=c( input$nnNodes1, input$nnNodes2 )
                                              , nnSeed = input$nnSeed
                                              , includeInteractions = input$incInt ) 
  }, ignoreNULL = TRUE )
  
  # If the decision changes, reset slide flag and reasons.
  observeEvent( input$decision,{
    docRV$makeSlides <- FALSE
    docRV$reasons <- NULL
  })
  
  # Updates to Plots ----
  # If results change, change the plots. Note the functions are in cdmFunctions.R
  observeEvent( resultsRV$trainResults, plotsRV$trainLine <- makeLineGraph( resultsRV$trainResults ) )
  observeEvent( resultsRV$testResults, plotsRV$testLine <- makeLineGraph( resultsRV$testResults ) )
  observeEvent( resultsRV$cvResults, {
    plotsRV$cvBox <- makeBoxGraph( resultsRV$cvResults ) 
    # Sneaky update to documentation...
    # Clear the documentation cache
    unlink('../Documentation/shinyWriteUp_cache', recursive = TRUE)
    # Generate a new file name
    docRV$updateNumber <- docRV$updateNumber + 1
    docRV$fileName <- paste0( '../Documentation/shinyWriteUp', docRV$updateNumber, '.html'  )
    render( '../Documentation/shinyWriteUp.Rmd', output_file = docRV$fileName )
    docRV$updateIn <- TRUE
  }) 
  
  # Updates to slides ---------
  observeEvent( input$goReason, {
    docRV$reasons <- c( docRV$reasons, input$decReasons ) 
    updateTextInput( session, inputId='decReasons', value='' )
  })
  
  # Wait to render slides for actionButton.
  observeEvent( input$goSlides,{
    
    # Note input$decision is a file name - not the display text!
    render( input$decision, output_file = '../Documentation/slidesOut.html' )
    
    docRV$makeSlides <- TRUE
  })
  
  # Output definitions ----
  # Note most of the outputs are somewhat trivial, as the plots/results have been generated already.
  output$inDoc <- renderUI({
    if( docRV$updateIn )
      includeHTML( docRV$fileName )
    else
      includeHTML( '../Documentation/initialWriteUp.html' )
  })
  
  output$trainSummary <- renderPrint( summary( dataRV$trainData[,input$xNames], digits=3 ) )
  output$lineTrain <- renderPlot( plotsRV$trainLine )
  output$lineTest <- renderPlot({ 
    if( is.null( plotsRV$testLine ) )
      return( NULL )
    
    # Return results if not null
    plotsRV$testLine 
    })
  
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

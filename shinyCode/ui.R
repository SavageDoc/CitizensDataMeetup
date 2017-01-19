## UI Front-end for NN/LM Modelling comparison for presentation to the Citizens Data Meetup group
#
# This is part of a larger project, including:
#    - Functions in baseCode directory
#    - Document templates in Documentation directory
#    - Presentation information in Presentation directory
#

library(shiny)
library( shinythemes )

shinyUI(
  fluidPage(
    
    # Shiny themes
    themeSelector()
    # Application title
    , titlePanel("Comparison of NN and LM Models: Citizens Data Meetup"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        ## Data splitting conditional ----
        conditionalPanel( condition='input.tabs == "Data"' 
                          ,wellPanel( h2('Data Splitting:' )
                                      , numericInput( inputId='splitSeed', label='Split seed:', value=SPLIT_SEED, min=0, max=1000, step=10 )
                                      , sliderInput( inputId='trainPercent', label='Select train percentage:', min=0, max=100, value=TRAIN_PERCENT )
                                      , checkboxGroupInput( inputId='xNames', label='Select Variables:', choices=c(
                                        'Crime rate' = 'crim'
                                        , 'Large lots'='zn'
                                        , 'Non-retail %'='indus'
                                        , 'Charles river' = 'chas'
                                        , 'NO_2 Conc' = 'nox'
                                        , 'Rooms' = 'rm'
                                        , 'Old homes'='age'
                                        , 'Distance to Emp Centres'='dis'
                                        , 'Access to radial highways' = 'rad'
                                        , 'Property tax rate' = 'tax'
                                        , 'Pupil-teacher ratio' = 'ptratio'
                                        , 'Proportion African-American' = 'black'
                                        , 'Lower status (%)' = 'lstat'
                                      )
                                      , selected=xNames ) 
                          )
        )
        ## Model configuration conditional ----
        , conditionalPanel( condition='input.tabs == "Train" | input.tabs == "Test"'
                            , wellPanel( h2( 'Model Configuration:' )
                                         , numericInput( inputId='nnSeed', label='NN Seed:', value=NN_SEED, min=0, max=1000, step=10 )
                                         , numericInput( inputId='nnNodes1', label='Nodes in First Layer:', min=1, max=15, value=HIDDEN_VEC[1] )
                                         , numericInput( inputId='nnNodes2', label='Nodes in Second Layer (0 for none):', min=0, max=8, value=HIDDEN_VEC[2] ) 
                                         , checkboxInput( inputId='incInt', label='Include Interactions', value=INC_INT ) 
                            )
        )
        ## Cross-validation conditional ----
        , conditionalPanel( condition='input.tabs=="CV"'
                            , wellPanel( h2('Cross-Validation')
                                         , numericInput( inputId='nCV', label='Number of iterations:', value=N_CV, min=5, max=50, step=1 )
                                         , actionButton( inputId='goCV', label='Cross-Validate' ) )
        )
        ## Slides conditional ----
        , conditionalPanel( condition='input.tabs=="Slides"'
                            , radioButtons( inputId='decision', label='Decision:', choices=c(
                              'Implement NN'='shinyNN.Rmd'
                              , 'Implement LM' = 'shinyLM.Rmd'
                              , 'Implement Both (A/B Test)'='shinyBoth.Rmd' 
                              , 'Implement Neither' = 'shinyNeither.Rmd' )
                            )
                            , textInput( inputId='decReasons', label='Reasons:' )
                            , verbatimTextOutput( outputId='listReasons' )
                            , actionButton( inputId='goReason', label='Submit Reason' )
                            , actionButton( inputId='goSlides', label='Build Slides' )
        )
        , width=3),
      
      # Main Panel ----
      mainPanel(
        tabsetPanel( id='tabs'
                     , tabPanel( title='Overview'
                                 , htmlOutput( outputId='inDoc' ) )
                     , tabPanel( title='Data'
                                 , verbatimTextOutput( outputId='trainSummary' ) )
                     , tabPanel( title='Train'
                                 , plotOutput( outputId='lineTrain' ) )
                     , tabPanel( title='Test'
                                 , plotOutput( outputId='lineTest' )
                                 , verbatimTextOutput( outputId='testSummary' ) )
                     , tabPanel( title='CV'
                                 , plotOutput( outputId='boxCV' )
                                 , tableOutput( outputId='mseCV' ) )
                     , tabPanel( title='Slides'
                                 , htmlOutput( outputId='outDoc', inline=TRUE ) )
        )
        , width=9 )
    ) # End sidebarLayout
  ) # End fluidPage
) # End shinyUI
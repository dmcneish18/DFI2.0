###packages

#make sure latest version of dynamic in installed (last updated 5/8/24)
devtools::install_github("melissagwolf/dynamic")

#needed for spinner if not installed
devtools::install_github("daattali/shinycssloaders")

library(shiny)
library(lavaan)
library(dynamic)
library(dplyr)
library(semPlot)
library(shinycssloaders)
library(shinyjs)
library(rmarkdown)
library(knitr)

######################## Shiny Code

## Code for Interface
ui <- fluidPage(

  shinyjs::useShinyjs(),
  
    # Application title
    titlePanel(h1(HTML("Dynamic Fit Index Cutoffs for Factor Analysis"), align="center", style="background-color: #1C4E80; color: white; padding-top:10px;padding-bottom:10px;"),
               windowTitle = "DFI for Factor Analysis"
               ),

    # sidebar for data upload and option selection
      sidebarLayout(
        
        sidebarPanel(
          
          style="height:90vh; overflow-y:auto;",
          #make labels font color white
          tags$style('label {color:white;}'),
        
          #make help text font color white;
          tags$style('.help-block {color:white;}'),
          
          #change progress bar to black
          tags$head(tags$style(".progress-bar{background-color:#000000;}")),
          
          #create object that is horizontal line
          tags$head(tags$style(HTML("hr {border-top: 1px solid color=white;}"))), 
          
            #instructions heading
            h3(HTML("<b>Instructions</b>"), align="center", style="color: white; margin-top:0px;"),
            
            #instructions;
            helpText("1. Upload a dataset in .csv format"),
            helpText("2. The first 5 rows will appear to verify the data loaded correctly."),
            helpText("3. Select the number of factors"), 
            helpText("4. Select the items on each factor"),
            helpText("5. With 2 or more factors, specify if the factors correlate or are orthogonal"),
            helpText("6. Specify if there are any residual covariances (keep at 0 if you're unsure)"),
            helpText("7. Choose the reponse scale of the items"),
            helpText("8. Choose the estimator (choices adapt based on the response scale)"),
            helpText("9. Choose the cutoff precision and click Submit"),
            helpText("10. Results will appear in a few minutes after calculations are complete"),
            
            #add horizontal line to separate instructions from options
            hr(),
          
            #label for file upload
            h5(HTML("<b>File Upload</b>"), style="color: white; margin-top:25px; margin-bottom:10px;"),
          
            #box to upload data
            fileInput("upload", NULL, accept = c(".csv")),
          
            #reduce space between upload box and ratio buttons
            div(style = "margin-top:-15px"),
          
            #box to input missing data indicator
            textInput("missing", "Missing Data Indicator", "NA"),
        
            ##Number of Factors
            numericInput("Factors", "Select the Number of Factors",
                        min=1, max=8, value=" "),
                        
            #Box(es) for selecting items; Number of boxes based on number of factors
            uiOutput("FactorItems"),
          
            #Box for correlated factors if number of factors > 1
            uiOutput("FacCor"),
          
            #Box for hierarchical factor if factors >2
            uiOutput("GenFac"),
          
            #box for no OP method if model is hierarchical
            uiOutput("HierWarn1"),
            
          ##Number of Residual Covariances
           numericInput("ResCov", "Select Number of Residual Covariances",
                       min=0, max=20, value=0),
          
          #boxes for residual Covariances
           uiOutput("RC"),
            
          #Response Scale
          radioButtons("Scale", label="Select Response Scale", 
                       choiceNames=c("Continuous (Normal)",
                                     "Continuous (Non-Normal)",
                                     "Likert/Ordinal as Continuous",
                                     "Binary or Categorical"), 
                       choiceValues=c("N","NN", "L", "C"), selected=character(0)),
           
           #information about limited options for Likert/Ordinal responses
           uiOutput("LikertWarn"),
           #Information about limited options for categorical responses
           uiOutput("CatWarn"),
           #Hierarchical Warning about no likert option
           uiOutput("HierWarn2"),
          
            #Estimator; options vary depending on estimator (e.g., no ML for categorical)
            radioButtons("est", label="Estimator", 
                         choiceNames=c("Maximum Likelihood",
                                       "Robust Maximum Likelihood (MLR)",
                                       "Robust Diagonally Weighted Least Squares (WLSMV)",
                                       "Robust Unweighted Least Squares (ULSMV)"), 
                         choiceValues=c("ML","MLR", "WLSMV", "ULSMV"), selected=character(0)),
            
          #Precision/Reps option
          radioButtons("Reps", label="Select Level of Cutoff Precision", 
                       choiceNames=c("Rough",
                                     "2-Decimal Point",
                                     "3-Decimal Point",
                                     "(App Developer Test Only)"),
                       choiceValues=c(50,100,250,10), selected=character(0)),
          # Helptext underneath box to select items
          helpText(HTML("<i>Higher precision takes more computational time</i>")),
          
          #Misspecification Method
            radioButtons("Miss", label="Select Misspecification Method", 
                         choiceNames=c("Direct Discrepancy Matrix",
                                       "Omitted Paths"),
                         choiceValues=c("DD","OP"), selected=character(0)),
          
            # Helptext underneath box to select items
            helpText(HTML("<i>Omitted Paths mirrors Hu and Bentler (1999), but is less standardized across models</i>")),
            helpText(HTML("<i>Direct Discrepancy is more standardized and easier to compare across models</i>")),
           
            #automatically download results
            div(style="display:inline-block; width:100%; text-align: center;", checkboxInput("dl", label = "Download results automatically", value = FALSE)),
          
            #activation button to begin calculations
            #center "submit" within column
            div(style="display:inline-block; width:100%; text-align: center;", actionButton("go", "Submit")),
            
            #make background blue
            style="background-color: #1C4E80;"
        ),
  
   #print first 5 rows of data after it is uploaded
   mainPanel(tableOutput("head"),
   
     #create panels for output
     tabsetPanel(id="Tabs",
       
       #Welcome/Overview
       tabPanel( title = "Overview",
                 
                 h4("Welcome to Dynamic Fit Index Cutoffs, a tool to customize validation of measurement scales!"),
                 
                 p(HTML("<br/>")),
                 
                 h4(HTML("<li>Comparing  observed iter-item correlations to  
                   predicted iter-item correlations implied by a factor model is a common source of validity evidence."),style = "margin-bottom: 30px;"),
                 h4(HTML("<li>But how close do the observed and predicted values need to be to conclude that the hypothesized factor structure is reasonable?"),style = "margin-bottom: 30px;"),
                 
                 h4(HTML("<li>Researchers commonly rely on guidelines from Hu and Bentler (1999) or Browne & Cudeck (1993)")),
                 h4(HTML("<ul><li>These sources suggest metrics like RMSEA < .06 and CFI > .95 or RMSEA < .05 and CFI > 0.90."),style = "margin-bottom: 30px;"), 
                   
                 h4(HTML("<li>However, statistical simulations show that these criteria do not generalize well")),
                 h4(HTML("<ul><li>Common criteria only work for a subset of factor structures and data types")),
                 h4(HTML("<ul><li>These criteria can arbitrarily reward or punish certain types of models or data."), style = "margin-bottom: 30px;"),
  
                 h4(HTML("<li><b>This software simulates RMSEA and CFI fit index cutoff values that are optimized for your specific model and data. </b>"), style = "margin-bottom: 30px;"),
                 
                 h4(HTML("<li>The idea is similar to a power analysis.")),
                 h4(HTML("<ul><li> One study may only need N = 50 to have sufficient power and another may need N = 400")), 
                 h4(HTML("<ul><li> Similarly, different RMSEA or CFI values are needed to assess the closeness of the observed and predicted correlations."))

                 ),
       
       #DFI Cutoff table
       tabPanel(title = "DFI Table",
                 h4("Model Fit Indices:"),
                 tableOutput("Fit"),
                 h4("Dynamic Fit Index Cutoffs for the Model and Data:"),
                 tableOutput("DFI"),
                 textOutput("Ref"),
                 downloadButton("download","Download DFI Report"),
                ),
       
       #DFI plots
       tabPanel(title = "DFI Plot",
                  
                  #updates based on number of plots in specified function
                  uiOutput("plots")
                ),
       
       tabPanel(title = "Correlation Residual Matrix",
                p(HTML("<br/>")),
                h4("Purpose"),
                p(HTML("<li>This is the difference between the correlations observed in the data and the correlations predicted by the model")),
                p(HTML("<li>Global fit indices like SRMR, RMSEA, and CFI are essentially trying to summarize this matrix with a single number")),
                p(HTML("<li>Inspecting individual elements of the matrix can be useful for identifying areas of local strain or local misfit")),
                p(HTML("<li>Local fit assessment is a commonly recommended complement to global fit indices")),
                p(HTML("<br/>")),
                h4("Correlation Residual Matrix"),
                tableOutput("SRM"),
                p(HTML("<br/>")),
                h4(HTML("Interpretation")),
                p(HTML("<li>Entries with absolute value below .10 are generally considered to be acceptable")),
                p(HTML("<li>Entries with absolute value above .10 may indicate a misspecification that is causing the predicted correlations to be inaccurate"))
       ),
       
       #Model estimates
       tabPanel(title = "Standardized Model Estimates",
                tableOutput("Table")
                ),
       
       #Path diagram
       tabPanel(title = "Path Diagram",
                 plotOutput("PD",width="7in", height="5in")
                ),
       
       #lavaan model
        tabPanel(title = "lavaan Model Statement",
                 verbatimTextOutput("Model"),
                 p("This syntax can be copied and pasted in R to fit the same model in the lavaan package")
                 ),
        
       #references
        tabPanel(title = "References",
                 h3("Methods used to create dynamic fit index cutoffs:"),
                 p(HTML("<li>McNeish, D. & Wolf, M.G. (in press). Direct discrepancy dynamic fit index cutoffs for arbitrary covariance structure models. <i>Structural Equation Modeling</i>.")),
                 p(HTML("<li>McNeish, D. & Wolf, M.G. (2023). Dynamic fit index cutoffs for confirmatory factor analysis models. <i>Psychological Methods, 28</i> (1), 61-88.")),
                 p(HTML("<li>McNeish, D. (2023). Dynamic fit index cutoffs for categorical factor analysis with Likert-type, ordinal, or binary responses. <i>American Psychologist, 79</i> (9), 1061-1075.")),
                 p(HTML("<li>McNeish, D. & Wolf, M.G. (2023). Dynamic fit index cutoffs for one-factor models. <i>Behavior Research Methods, 55</i> (3), 1157-1174.")),
                 p(HTML("<li>McNeish, D. (2023). Generalizability of dynamic fit index, equivalence testing, and Hu & Bentler cutoffs for evaluating fit in factor analysis. <i>Multivariate Behavioral Research, 58</i> (1), 195-219.")),
                 h3("This Application:"),
                 p(HTML("<li>Wolf, M. G. & McNeish, D. (2020). Dynamic Model Fit. R Shiny application version 2.1.0.")),
                 h3("The R package underlying this Application:"),
                 p(HTML("<li>Wolf, M.G. & McNeish, D. (2023). dynamic: An R package for deriving dynamic fit index cutoffs for factor analysis. ,<i>Multivariate Behavioral Research, 58</i> (1), 189-194.")),
                 h3("Computationally, this application relies on:"),
                 p("XXX"),
                 h3("Aesthetically, this application relies on:"),
                 p("XXX")
                 
                 
                  )
          )
        )
     )
  )

#R code to execute in the background
server <- function(input, output,session) {
  
  #create object for data once it is uploaded  
  data <- reactive({
        req(input$upload)
        
        ext <- tools::file_ext(input$upload$name)
        switch(ext,
               csv = vroom::vroom(input$upload$datapath, delim = ","),
               validate("Invalid file; Please upload a .csv file")
        )
    })
  
  shinyjs::disable("download")
  # Identify which response scale was selected
  N<-reactive({req(input$Scale=="N")})
  NN<-reactive({req(input$Scale=="NN")})
  L<-reactive({req(input$Scale=="L")})
  C<-reactive({req(input$Scale=="C")})
  G<-reactive(req(input$Factors>2))
  H1<-reactive({req(input$GenFac==1)})
  H0<-reactive({req(input$GenFac==0)})
  D<-reactive({req(input$dl==TRUE)})
  
  #If normal, don't allow MLR
  observeEvent(N(), {updateRadioButtons(session,"est", 
                                                     choiceNames=c("Maximum Likelihood",
                                                                   "Robust Diagonally Weighted Least Squares (WLSMV)",
                                                                   "Robust Unweighted Least Squares (ULSMV)"), 
                                                     choiceValues=c("ML", "WLSMV", "ULSMV"))
                    updateRadioButtons(session,"Miss", 
                                       choiceNames=c("Direct Discrepancy Matrix",
                                                     "Omitted Paths"),
                                       choiceValues=c("DD","OP"), selected=character(0))
                    })
  
  observeEvent(c(N(),G()), {           
                 if(input$GenFac==0){
                    updateRadioButtons(session,"Miss", 
                                       choiceNames=c("Direct Discrepancy Matrix",
                                                     "Omitted Paths"),
                                      choiceValues=c("DD","OP"), selected=character(0))
                    }
    
                   if(input$GenFac==1){
                    updateRadioButtons(session,"Miss", 
                                       choiceNames=c("Direct Discrepancy Matrix"),
                                       choiceValues=c("DD"))
                   }
                }
               )
  
  #If non-normal, don't allow ML
  observeEvent(NN(), {updateRadioButtons(session,"est", 
                                          choiceNames=c("Robust Maximum Likelihood (MLR)",
                                                        "Robust Diagonally Weighted Least Squares (WLSMV)",
                                                        "Robust Unweighted Least Squares (ULSMV)"), 
                                          choiceValues=c("MLR", "WLSMV", "ULSMV"))
                      updateRadioButtons(session,"Miss", 
                                         choiceNames=c("Direct Discrepancy Matrix",
                                                       "Omitted Paths"),
                                         choiceValues=c("DD","OP"), selected=character(0))
  })
  
  observeEvent(c(NN(),G()), {            
                    if(input$GenFac==0){
                      updateRadioButtons(session,"Miss", 
                                         choiceNames=c("Direct Discrepancy Matrix",
                                                       "Omitted Paths"),
                                         choiceValues=c("DD","OP"), selected=character(0))
                    }
                    
                    if(input$GenFac==1){
                      updateRadioButtons(session,"Miss", 
                                         choiceNames=c("Direct Discrepancy Matrix"),
                                         choiceValues=c("DD"))
                    }
               })
  
  #If Likert, no restrictions
  observeEvent(L(), {updateRadioButtons(session,"est", 
                                        choiceNames=c("Maximum Likelihood",
                                                      "Robust Maximum Likelihood (MLR)",
                                                      "Robust Diagonally Weighted Least Squares (WLSMV)",
                                                      "Robust Unweighted Least Squares (ULSMV)"), 
                                        choiceValues=c("ML", "MLR", "WLSMV", "ULSMV"), selected="Robust Maximum Likelihood (MLR)")
                  
                     updateRadioButtons(session,"Miss", 
                                  choiceNames=c("Omitted Paths"),
                                  choiceValues=c("OP"))})
  
  #If categorical, no ML or MLR
  observeEvent(C(), { updateRadioButtons(session,"est", 
                                                     choiceNames=c( "Robust Diagonally Weighted Least Squares (WLSMV)",
                                                                    "Robust Unweighted Least Squares (ULSMV)"), 
                                                     choiceValues=c("WLSMV", "ULSMV"))
                      updateRadioButtons(session,"Miss", 
                                         choiceNames=c("Direct Discrepancy Matrix",
                                                       "Omitted Paths"),
                                         choiceValues=c("DD","OP"), selected=character(0))
                    })
  
  observeEvent(c(C(),G()), {
                    if(input$GenFac==0){
                      updateRadioButtons(session,"Miss", 
                                         choiceNames=c("Direct Discrepancy Matrix",
                                                       "Omitted Paths"),
                                         choiceValues=c("DD","OP"), selected=character(0))
                    }
                    
                    if(input$GenFac==1){
                      updateRadioButtons(session,"Miss", 
                                         choiceNames=c("Direct Discrepancy Matrix"),
                                         choiceValues=c("DD"))
                    }
               })
  
  ##If model is hierarchical, remove Likert & Omitted path options
  observeEvent(H1(), {
    
    updateRadioButtons(session,"Scale", 
                 choiceNames=c("Continuous (Normal)",
                               "Continuous (Non-Normal)",
                               "Binary or Categorical"), 
                 choiceValues=c("N","NN", "C"), selected=character(0))
    
    updateRadioButtons(session,"Miss", 
                       choiceNames=c("Direct Discrepancy Matrix"),
                       choiceValues=c("DD"))
    
  })
  
  ### If model is non-hierarchical, reset the options to include Likert & Omitted Paths
  observeEvent(H0(), {
    
    updateRadioButtons(session,"Scale", 
                       choiceNames=c("Continuous (Normal)",
                                     "Continuous (Non-Normal)",
                                     "Likert/Ordinal as Continuous",
                                     "Binary or Categorical"), 
                       choiceValues=c("N","NN", "L", "C"), selected=character(0))
    
    updateRadioButtons(session,"Miss", 
                       choiceNames=c("Direct Discrepancy Matrix",
                                     "Omitted Paths"),
                       choiceValues=c("DD","OP"), selected=character(0))
  })
  
  
  
    #adaptively update the number of boxes based number of specified factors
    output$FactorItems<-renderUI({
      req(input$Factors)
      lapply(1:(input$Factors), function(i) {
        selectizeInput(inputId=paste0("Factor",i), 
                    label=paste0("Select Items on Factor ", i, ":"),
                    choices=names(data()), multiple=TRUE)
      })
    })
    
    #if Factors >1, add option for correlated factors
    output$FacCor<-renderUI({
      req(input$Factors>1)
      radioButtons("FacCor", label="Are Factors Correlated?", 
                   choices=c("Yes",
                             "No"))
    })
    
    #if Factors >2, add option for general hierarchical factor
    output$GenFac<-renderUI({
      req(input$Factors>2)
      radioButtons("GenFac", label="Is there a General Hierarchical Factor loading on all factors?", 
                   choiceNames=c("Yes","No"), 
                   choiceValues=c(1,0),
                   selected=0)
    })
    
 
    ###
    #Update to only include choices that were selected in Factors?
    ###
    
    #If user indicates residual covariances, then create boxes for each pair
    output$RC<-renderUI({
      req(input$ResCov >0)
      lapply(1:(input$ResCov), function(i) {
        selectizeInput(inputId=paste0("RC",i), 
                       label=paste0("Residual Covariance Pair #", i, ":"),
                       choices=names(data()), multiple=TRUE, options=list(maxItems=2))
      })
    })
    
    #Limitations for Hierarchical models, DD method
    output$HierWarn1<-renderUI({
      req(input$GenFac==1)
      helpText(HTML("<i>Hierarhical factor models are only supported with the Direct Discrepancy Matrix method</i>"))
    })
    
    #Limitations for Hierarchical models, no Likert option
    output$HierWarn2<-renderUI({
      req(input$GenFac==1)
      helpText(HTML("<i>Hierarhical factor models cannot currently support treating Likert response as continuous. Non-Normal is a close approximation.</i>"))
    })
    
    #Limitations for Likert/ordinal responses
    output$LikertWarn<-renderUI({
      req(input$Scale=="L")
      helpText(HTML("<i>Direct Discrepancy Matrix method does not yet support treating Likert items as continuous</i>"))
    })
    
    #Limitations for Categorical responses
    output$CatWarn<-renderUI({
      req(input$Scale=="C")
      helpText(HTML("<i> Expect categorical models to take several minutes to simulate cutoffs </i>"))
    })
 
  #print the first five rows of the uploaded data
    output$head <- renderTable({
      req(input$upload)
      head(data(), 5)
    })
    
###########################################
    
    #once the submit button is clicked (go =1), run the conditional reliabliity function using the selected options
    observeEvent(input$go,{
      
    #create list of items on each factor
    #8 are listed because its the maximum currently allowed
    #easier to list out all 8 that to adaptively change the suffix after a dollar sign  
      l<-list(input$Factor1,input$Factor2,
              input$Factor3,input$Factor4,
              input$Factor5,input$Factor6,
              input$Factor7,input$Factor8)
      
       #left side
      lhs<-list()
      #right side
      rhs<-list()
      #combined
      line<-list()
      
      if (input$Factors <3){
        #loop over factors
        for(m in 1: input$Factors){
          
          #left side is factor name 
          lhs[[m]]<-paste0("f",m,"=~")
          #begin right side with first item
          rhs[[m]]<-l[[m]][1]
          
          #loop over number of items
          for(i in 1:(length(l[[m]])-1)){
            #plus sign between item names
            rhs[[m]]<-paste(rhs[[m]],"+", l[[m]][i+1])
          }
          
          #combine left and right hand side
          line[[m]]<-paste(lhs[[m]],rhs[[m]])
        }
        
        ###add residual covariances here###
        if(input$ResCov >0){
          for(r in 1:input$ResCov){
            line[[input$Factors+r]]<-paste(eval(parse(text=paste0("input$RC",r,"[1]"))), "~~", eval(parse(text=paste0("input$RC",r,"[2]"))))
          }
        }
      }
      
      if (input$Factors>2) {
        #loop over factors
        for(m in 1: input$Factors){
          
          #left side is factor name 
          lhs[[m]]<-paste0("f",m,"=~")
          #begin right side with first item
          rhs[[m]]<-l[[m]][1]
          
          #loop over number of items
          for(i in 1:(length(l[[m]])-1)){
            #plus sign between item names
            rhs[[m]]<-paste(rhs[[m]],"+", l[[m]][i+1])
          }
          
          #combine left and right hand side
          line[[m]]<-paste(lhs[[m]],rhs[[m]])
        }
        
        # if hierarchical factor is present, add line to statement has load general factor 'g' on all factors
        if(as.numeric(input$GenFac==1)){
          line[[input$Factors+1]]<-paste0("g","=~","f1")
          for(m in 2: input$Factors){
            line[[input$Factors+1]]<-paste0(line[[input$Factors+1]], " + ", "f",m)
          }
        }
        
        ###add residual covariances here###
        if(input$ResCov >0){
          for(r in 1:input$ResCov){
            line[[input$Factors+as.numeric(input$GenFac)+r]]<-paste(eval(parse(text=paste0("input$RC",r,"[1]"))), "~~", eval(parse(text=paste0("input$RC",r,"[2]"))))
          }
        }
      }
      
        #unlist to put into one model statement
        model<-unlist(line)
       
    
    # T/F indicator for correlated factors in lavaan 
    COR<-ifelse(input$FacCor=="Yes",TRUE,FALSE)
    
    #T/F indicator for categorical data in lavaan
    CAT<-ifelse(input$Scale=="C",TRUE,FALSE)
    
    #Fit model in lavaan
    a<-lavaan::cfa(data=data(), model=model, estimator=input$est, auto.cov.lv.x=COR, ordered=CAT)
    #standardized residual matrix
    b<-lavResiduals(a, type="cor")
    output$SRM<-renderTable({as.data.frame(round(b$cov,2))}, rownames=T)

    
##################
# DDDFI
##################
    
    #### If Direct Discrepancy method
    if(input$Miss=="DD"){
     
       if(input$Scale=="N"){
        S="normal"
      }  
      if(input$Scale=="NN"){
        S="nonnormal"
      }
      if(input$Scale=="C"){
        S="categorical"
      }
    
    ############################################  
    #reps will eventually be based on input$Reps
    ############################################
      
    #show spinner while calculating  
    shinycssloaders::showPageSpinner(caption = "Simulating cutoffs, please wait")
    
    ## DDDFI   
    DFI<-dynamic::DDDFI(a, data=data(),plot.dfi =T,reps=as.numeric(input$Reps), estimator=input$est, scale=S)
    
    #hide spinner when calculations are complete
    shinycssloaders::hidePageSpinner()
    
    #switch to results tabs after completion
    updateTabsetPanel(session, inputId = "Tabs", selected = 'DFI Table')
    
    #DDDFI always have consistent output table dimension
    #turn results into data frame for easier display in Shiny
    x<-data.frame(matrix(DFI$cutoffs, nrow=11, ncol=5))
    n<-data.frame(matrix(c("Exact", "Specificity", " ",
                           "Close", "Sensitivity", " ",
                           "Fair", "Sensitivity", " ",
                           "Mediocre", "Sensitivity"), nrow=11, ncol=1))
    
    x1<-data.frame(cbind<-c(n,x))
    names(x1)<-c(" ", "MAD", "sim. MAD", "CFI", "RMSEA", "RMSEA 90% CI")
    
    #save the standardized solution
    output$Table<-renderTable({lavaan::standardizedSolution(a)})
    #save the model statement
    LMS<-base::paste(model, sep="", collapse="\n") #format model statement
    output$Model<-renderText(LMS)
    #print the DFI table
    output$DFI<-renderTable(x1)
    #print the fit indices
    output$Fit<-renderTable(DFI$fit)
    #create list of DFI plots
    #always 3 in DDDFI
    output$plots <- renderUI({
      
      lapply(1:3, function(i) {
        # creates a unique ID for each plotOutput
        id <- paste0("plot_", i)
        plotOutput(outputId = id,width="6in", height="4in")
        
        # render each plot
        output[[id]] <- renderPlot(DFI$plot.dfi[[i]], width=600, height=400)
      })
      
    })
    #print the path diagram
    PD<-semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE)
    output$PD<-renderPlot(semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE))
    
    #output$Ref<-renderPrint(p("For more information or to cite the method producing these cutoffs:"),
    #                       p(HTML("McNeish, D. & Wolf, M.G. (in press). Direct discrepancy dynamic fit index cutoffs for arbitrary covariance structure models. <i>Structural Equation Modeling</i>.")),
    #                       p(HTML("McNeish, D. & Wolf, M.G. (2023). Dynamic fit index cutoffs for confirmatory factor analysis models. <i>Psychological Methods, 28</i> (1), 61-88.")),
    #                       p(HTML("Wolf, M.G. & McNeish, D. (2023). dynamic: An R package for deriving dynamic fit index cutoffs for factor analysis. ,i>Multivariate Behavioral Research, 58</i> (1), 189-194."))
    #                       )
    
    #########################
    # Add interpretation text
    #########################
    
    }
 
######################
# one-factor functions
######################
    
    #### If One-Factor, Normal, Omitted Paths
    if(input$Miss=="OP" & input$Factors==1 & input$Scale=="N"){
      
      #reps will eventually be based on input$Reps
      shinycssloaders::showPageSpinner(caption = "Simulating cutoffs, please wait")
      
      DFI<-dynamic::cfaOne(a,plot =T,reps=as.numeric(input$Reps), estimator=input$est)
      
      shinycssloaders::hidePageSpinner()
      
      #switch to results tabs after completion
      updateTabsetPanel(session, inputId = "Tabs", selected = 'DFI Table')
      
      x<-data.frame(matrix(DFI$cutoffs, nrow=nrow(DFI$cutoffs), ncol=ncol(DFI$cutoffs)))
      n<-data.frame(matrix(c("Level-0", "Specificity", " ",
                                                    "Level-1", "Sensitivity", " ",
                                                    "Level-2", "Sensitivity", " ",
                                                    "Level-3", "Sensitivity"), nrow=11, ncol=1))
      
      n1<-n[1:nrow(x),]
      x1<-data.frame(cbind(n1,x))
      names(x1)<-c(" ", "SRMR", "RMSEA", "CFI")
      
      #Standardized Solution Table
      output$Table<-renderTable({lavaan::standardizedsolution(a)})
      #save the model statement
      LMS<-base::paste(model, sep="", collapse="\n") #format model statement
      output$Model<-renderText(LMS)
      output$DFI<-renderTable(x1)
      output$Fit<-renderTable(DFI$fit)
      
      #print the path diagram
      PD<-semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE)
      output$PD<-renderPlot(semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE))
      
      q<-nrow(x)-(((nrow(x)*2)/3)+(2/3))
      output$plots <- renderUI({
        lapply(1:q, function(i) {
          # creates a unique ID for each plotOutput
          id <- paste0("plot_", i)
          plotOutput(outputId = id,width="6in", height="4in")
          
          # render each plot
          output[[id]] <- renderPlot(DFI$plots[[i]], width=600, height=400)
        })
        
      })
    }
    
    
    #### If One-Factor, Likert, Omitted Paths
    if(input$Miss=="OP" & input$Factors==1 & input$Scale=="L"){
      
      #reps will eventually be based on input$Reps
      shinycssloaders::showPageSpinner(caption = "Simulating cutoffs, please wait")
      
      DFI<-dynamic::likertOne(a,data=data(),plot =T,reps=as.numeric(input$Reps), estimator=input$est)
      
      shinycssloaders::hidePageSpinner()
      
      #switch to results tabs after completion
      updateTabsetPanel(session, inputId = "Tabs", selected = 'DFI Table')
      
      x<-data.frame(matrix(DFI$cutoffs, nrow=nrow(DFI$cutoffs), ncol=ncol(DFI$cutoffs)))
      n<-data.frame(matrix(c("Level-0", "Specificity", " ",
                             "Level-1", "Sensitivity", " ",
                             "Level-2", "Sensitivity", " ",
                             "Level-3", "Sensitivity"), nrow=11, ncol=1))
      
      n1<-n[1:nrow(x),]
      x1<-data.frame(cbind(n1,x))
      names(x1)<-c(" ", "SRMR", "RMSEA", "CFI")
      
      #Standardized Solution Table
      output$Table<-renderTable({lavaan::standardizedsolution(a)})
      #save the model statement
      LMS<-base::paste(model, sep="", collapse="\n") #format model statement
      output$Model<-renderText(LMS)      
      output$DFI<-renderTable(x1)
      output$Fit<-renderTable(DFI$fit)
      
      #print the path diagram
      PD<-semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE)
      output$PD<-renderPlot(semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE))
      
      q<-nrow(x)-(((nrow(x)*2)/3)+(2/3))
      output$plots <- renderUI({
        lapply(1:q, function(i) {
          # creates a unique ID for each plotOutput
          id <- paste0("plot_", i)
          plotOutput(outputId = id,width="6in", height="4in")
          
          # render each plot
          output[[id]] <- renderPlot(DFI$plots[[i]], width=600, height=400)
        })
        
      })
    }
    
  #### If One-Factor Non-Normal, Omitted-Paths
    if(input$Miss=="OP" & input$Factors==1 & input$Scale=="NN"){
      
      #reps will eventually be based on input$Reps
      shinycssloaders::showPageSpinner(caption = "Simulating cutoffs, please wait")
      
      DFI<-dynamic::nnorOne(a,data=data(),plot =T,reps=as.numeric(input$Reps), estimator=input$est)
      
      shinycssloaders::hidePageSpinner()
      
      #switch to results tabs after completion
      updateTabsetPanel(session, inputId = "Tabs", selected = 'DFI Table')
      
      x<-data.frame(matrix(DFI$cutoffs, nrow=nrow(DFI$cutoffs), ncol=ncol(DFI$cutoffs)))
      n<-data.frame(matrix(c("Level-0", "Specificity", " ",
                             "Level-1", "Sensitivity", " ",
                             "Level-2", "Sensitivity", " ",
                             "Level-3", "Sensitivity"), nrow=11, ncol=1))
      
      n1<-n[1:nrow(x),]
      x1<-data.frame(cbind(n1,x))
      names(x1)<-c(" ", "SRMR", "RMSEA", "CFI")
      
      #Standardized Solution Table
      output$Table<-renderTable({lavaan::standardizedsolution(a)})
      #save the model statement
      LMS<-base::paste(model, sep="", collapse="\n") #format model statement
      output$Model<-renderText(LMS)      
      output$DFI<-renderTable(x1)
      output$Fit<-renderTable(DFI$fit)
      
      #print the path diagram
      PD<-semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE)
      output$PD<-renderPlot(semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE))      
      
      q<-nrow(x)-(((nrow(x)*2)/3)+(2/3))
      output$plots <- renderUI({
        lapply(1:q, function(i) {
          # creates a unique ID for each plotOutput
          id <- paste0("plot_", i)
          plotOutput(outputId = id,width="6in", height="4in")
          
          # render each plot
          output[[id]] <- renderPlot(DFI$plots[[i]], width=600, height=400)
        })
        
      })
    }
    
    #### If One-Factor Categorical, Omitted-Paths
    if(input$Miss=="OP" & input$Factors==1 & input$Scale=="C"){
      
      #reps will eventually be based on input$Reps
      shinycssloaders::showPageSpinner(caption = "Simulating cutoffs, please wait")
      
      DFI<-dynamic::catOne(a,plot =T,reps=as.numeric(input$Reps), estimator=input$est)
      
      shinycssloaders::hidePageSpinner()
      
      #switch to results tabs after completion
      updateTabsetPanel(session, inputId = "Tabs", selected = 'DFI Table')
      
      x<-data.frame(matrix(DFI$cutoffs, nrow=nrow(DFI$cutoffs), ncol=ncol(DFI$cutoffs)))
      n<-data.frame(matrix(c("Level-0", "Specificity", " ",
                             "Level-1", "Sensitivity", " ",
                             "Level-2", "Sensitivity", " ",
                             "Level-3", "Sensitivity"), nrow=11, ncol=1))
      
      n1<-n[1:nrow(x),]
      x1<-data.frame(cbind(n1,x))
      names(x1)<-c(" ", "SRMR", "RMSEA", "CFI")
      
      #Standardized Solution Table
      output$Table<-renderTable({lavaan::standardizedsolution(a)})
      
      #save the model statement
      LMS<-base::paste(model, sep="", collapse="\n") #format model statement
      output$Model<-renderText(LMS)      
      output$DFI<-renderTable(x1)
      output$Fit<-renderTable(DFI$fit)
      
      #print the path diagram
      PD<-semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE)
      output$PD<-renderPlot(semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE))
     
      q<-nrow(x)-(((nrow(x)*2)/3)+(2/3))
      output$plots <- renderUI({
        lapply(1:q, function(i) {
          # creates a unique ID for each plotOutput
          id <- paste0("plot_", i)
          plotOutput(outputId = id,width="6in", height="4in")
          
          # render each plot
          output[[id]] <- renderPlot(DFI$plots[[i]], width=600, height=400)
        })
        
      })
    }
    
######################
#Multifactor functions
######################    
    
    #### If Multi-Factor, Normal, Omitted-Paths
    if(input$Miss=="OP" & input$Factors>1 & input$Scale=="N"){
      
      #reps will eventually be based on input$Reps
      shinycssloaders::showPageSpinner(caption = "Simulating cutoffs, please wait")
      
      DFI<-dynamic::cfaHB(a,plot =T,reps=as.numeric(input$Reps), estimator=input$est)
      
      shinycssloaders::hidePageSpinner()
      
      #switch to results tabs after completion
      updateTabsetPanel(session, inputId = "Tabs", selected = 'DFI Table')
      
      #x<-data.frame(DFI$cutoffs[,1:3])
      #n<-data.frame(matrix(c("Level-1 95/5", "Level-1 90/10", 
      #                       "Level-2 95/5", "Level-2 90/10",
      #                       "Level-3 95/5", "Level-3 90/10",
      #                       "Level-4 95/5", "Level-4 90/10",
      #                       "Level-5 95/5", "Level-5 90/10",
      #                       "Level-6 95/5", "Level-6 90/10",
      #                       "Level-7 95/5", "Level-7 90/10"), nrow=14, ncol=1))
     
      #n1<-n[1:(input$Factors-1),]
      #x1<-data.frame(cbind<-c(n1,x))
      # names(x1)<-c(" ", "SRMR", "RMSEA", "CFI")
      
      xx<-DFI$cutoffs[1:((3*input$Factors)-1),1:3]
      
      x<-data.frame(matrix(xx, nrow=((3*input$Factors)-1), ncol=3))
      n<-data.frame(matrix(c("Level-0", "Specificity", " ",
                             "Level-1", "Sensitivity", " ",
                             "Level-2", "Sensitivity", " ",
                             "Level-3", "Sensitivity", " ",
                             "Level-4", "Sensitivity", " ",
                             "Level-5", "Sensitivity", " ",
                             "Level-6", "Sensitivity", " ",
                             "Level-7", "Sensitivity"),
                           nrow=23, ncol=1))
      
      n1<-n[1:(3*(input$Factors)-1),]
      x1<-data.frame(cbind(n1,x))
      names(x1)<-c(" ", "SRMR", "RMSEA", "CFI")
      
      #save the conditional reliability table
      output$Table<-renderTable({lavaan::standardizedSolution(a)})
      #save the model statement
      LMS<-base::paste(model, sep="", collapse="\n") #format model statement
      output$Model<-renderText(LMS)      
      output$DFI<-renderTable(x1)
      output$Fit<-renderTable(DFI$fit)
      
      #print the path diagram
      PD<-semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE)
      output$PD<-renderPlot(semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE))
      
      output$plots <- renderUI({
        lapply(1:(input$Factors-1), function(i) {
          # creates a unique ID for each plotOutput
          id <- paste0("plot_", i)
          plotOutput(outputId = id,width="6in", height="4in")
          
          # render each plot
          output[[id]] <- renderPlot(DFI$plots[[i]], width=600, height=400)
        })
      })
    }
    
    #### If Multi-Factor, Likert, Omitted-Paths
    if(input$Miss=="OP" & input$Factors>1 & input$Scale=="L"){
      
      #reps will eventually be based on input$Reps
      shinycssloaders::showPageSpinner(caption = "Simulating cutoffs, please wait")
      
      DFI<-dynamic::likertHB(a,data=data(),plot =T,reps=as.numeric(input$Reps), estimator=input$est)
      
      shinycssloaders::hidePageSpinner()
      
      #switch to results tabs after completion
      updateTabsetPanel(session, inputId = "Tabs", selected = 'DFI Table')
      
      xx<-DFI$cutoffs[1:((3*input$Factors)-1),1:3]
      
      x<-data.frame(matrix(xx, nrow=((3*input$Factors)-1), ncol=3))
      n<-data.frame(matrix(c("Level-0", "Specificity", " ",
                             "Level-1", "Sensitivity", " ",
                             "Level-2", "Sensitivity", " ",
                             "Level-3", "Sensitivity", " ",
                             "Level-4", "Sensitivity", " ",
                             "Level-5", "Sensitivity", " ",
                             "Level-6", "Sensitivity", " ",
                             "Level-7", "Sensitivity"),
                           nrow=23, ncol=1))
      
      n1<-n[1:(3*(input$Factors)-1),]
      x1<-data.frame(cbind(n1,x))
      names(x1)<-c(" ", "SRMR", "RMSEA", "CFI")
      
      #save the conditional reliability table
      output$Table<-renderTable({lavaan::standardizedSolution(a)})
      #save the model statement
      LMS<-base::paste(model, sep="", collapse="\n") #format model statement
      output$Model<-renderText(LMS)      
      output$DFI<-renderTable(x1)
      output$Fit<-renderTable(DFI$fit)
      
      #print the path diagram
      PD<-semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE)
      output$PD<-renderPlot(semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE))      
      
      output$plots <- renderUI({
        lapply(1:(input$Factors-1), function(i) {
          # creates a unique ID for each plotOutput
          id <- paste0("plot_", i)
          plotOutput(outputId = id,width="6in", height="4in")
          
          # render each plot
          output[[id]] <- renderPlot(DFI$plots[[i]], width=600, height=400)
        })
      })
    }
    
    
    #### If Multi-Factor, Non-Normal, Omitted-Paths
    if(input$Miss=="OP" & input$Factors>1 & input$Scale=="NN"){
      
      #reps will eventually be based on input$Reps
      shinycssloaders::showPageSpinner(caption = "Simulating cutoffs, please wait")
      
      DFI<-dynamic::nnorHB(a,data=data(),plot =T,reps=as.numeric(input$Reps), estimator=input$est)
      
      shinycssloaders::hidePageSpinner()
      
      #switch to results tabs after completion
      updateTabsetPanel(session, inputId = "Tabs", selected = 'DFI Table')
      
      xx<-DFI$cutoffs[1:((3*input$Factors)-1),1:3]
      
      x<-data.frame(matrix(xx, nrow=((3*input$Factors)-1), ncol=3))
      n<-data.frame(matrix(c("Level-0", "Specificity", " ",
                             "Level-1", "Sensitivity", " ",
                             "Level-2", "Sensitivity", " ",
                             "Level-3", "Sensitivity", " ",
                             "Level-4", "Sensitivity", " ",
                             "Level-5", "Sensitivity", " ",
                             "Level-6", "Sensitivity", " ",
                             "Level-7", "Sensitivity"),
                           nrow=23, ncol=1))
      
      n1<-n[1:(3*(input$Factors)-1),]
      x1<-data.frame(cbind(n1,x))
      names(x1)<-c(" ", "SRMR", "RMSEA", "CFI")
      
      #save the conditional reliability table
      output$Table<-renderTable({lavaan::standardizedSolution(a)})
      #save the model statement
      LMS<-base::paste(model, sep="", collapse="\n") #format model statement
      output$Model<-renderText(LMS)      
      output$DFI<-renderTable(x1)
      output$Fit<-renderTable(DFI$fit)
      
      #print the path diagram
      PD<-semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE)
      output$PD<-renderPlot(semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE))     
      
      output$plots <- renderUI({
        lapply(1:(input$Factors-1), function(i) {
          # creates a unique ID for each plotOutput
          id <- paste0("plot_", i)
          plotOutput(outputId = id,width="6in", height="4in")
          
          # render each plot
          output[[id]] <- renderPlot(DFI$plots[[i]], width=600, height=400)
        })
      })
    }
    
    if(input$Miss=="OP" & input$Factors>1 & input$Scale=="C"){
      
      #reps will eventually be based on input$Reps
      shinycssloaders::showPageSpinner(caption = "Simulating cutoffs, please wait")
      
      DFI<-dynamic::catHB(a,plot =T,reps=as.numeric(input$Reps), estimator=input$est)
      
      shinycssloaders::hidePageSpinner()
      
      #switch to results tabs after completion
      updateTabsetPanel(session, inputId = "Tabs", selected = 'DFI Table')
      
      xx<-DFI$cutoffs[1:((3*input$Factors)-1),1:3]
      
      x<-data.frame(matrix(xx, nrow=((3*input$Factors)-1), ncol=3))
      n<-data.frame(matrix(c("Level-0", "Specificity", " ",
                             "Level-1", "Sensitivity", " ",
                             "Level-2", "Sensitivity", " ",
                             "Level-3", "Sensitivity", " ",
                             "Level-4", "Sensitivity", " ",
                             "Level-5", "Sensitivity", " ",
                             "Level-6", "Sensitivity", " ",
                             "Level-7", "Sensitivity"),
                           nrow=23, ncol=1))
      
      n1<-n[1:(3*(input$Factors)-1),]
      x1<-data.frame(cbind(n1,x))
      names(x1)<-c(" ", "SRMR", "RMSEA", "CFI")
      
      #save the conditional reliability table
      output$Table<-renderTable({lavaan::standardizedSolution(a)})
      #save the model statement
      LMS<-base::paste(model, sep="", collapse="\n") #format model statement
      output$Model<-renderText(LMS)      
      output$DFI<-renderTable(x1)
      output$Fit<-renderTable(DFI$fit)
      
      #print the path diagram
      PD<-semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE)
      output$PD<-renderPlot(semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE))
      
      output$plots <- renderUI({
        lapply(1:(input$Factors-1), function(i) {
          # creates a unique ID for each plotOutput
          id <- paste0("plot_", i)
          plotOutput(outputId = id,width="6in", height="4in")
          
          # render each plot
          output[[id]] <- renderPlot(DFI$plots[[i]], width=600, height=400)
        })
      })
    }
    
    shinyjs::enable("download")
    #download results
    output$download <- downloadHandler(
      filename = function() {
        # create default file name
        paste0("DFICutoffs - ",Sys.Date(), ".html")
      },
      content = function(f) {
        # Create a new empty environment
        # This allows us to pass in only the relevant variables into the report
        e <- new.env()
        # Add the cutoffs
        e$cutoffs <- x1
        # Add the model image
        e$PD<-  PD
        e$fit<-DFI$fit
        e$LMS<-LMS
        # Render the document using template saved in same folder
        rmarkdown::render("template-FA.Rmd",
                          output_format = rmarkdown::html_document(),
                          output_file=f,
                          envir = e)
      })
    
    if(D()==TRUE) {
      shinyjs::delay(5,shinyjs::click("download"))
    }
    
    

})

    
#

#observeEvent(input$go, {
#  if(D()==TRUE) {
#    
#    output$DL<-renderUI({
#      shinyjs::enable("download")
#    })
#    shinyjs::delay(5, shinyjs::click("download"))  
#  }
#})
    
}

###########################################

# Run the application 
shinyApp(ui = ui, server = server)

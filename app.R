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

#### With names under the title
#  titlePanel(div("Dynamic Fit Index Cutoffs for Factor Analysis", align="center",style="background-color: #1C4E80; color: white; padding-top:10px;padding-bottom:5px;margin-bottom:0px;'",
#      h4(a("Melissa Wolf", href="https://www.melissagwolf.com/", target="_blank", style="color:#B8B8B8;"), "&"
#         ,a("Dan McNeish", href="https://sites.google.com/site/danielmmcneish/background", target="_blank",style="color:#B8B8B8;"),
#      style="margin-bottom:0px;margin-top:3px;")),
#      windowTitle = "DFI for Factor Analysis"),

## Without Names under the title Application title
    titlePanel(h1(HTML("Dynamic Fit Index Cutoffs for Factor Analysis"), align="center", style="background-color: #1C4E80; color: white; padding-top:10px;padding-bottom:10px; margin:bottom=0px;"),
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
                 tags$details(tags$summary("Purpose", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                  p(HTML("These are the fit indices for your model.They are all <i>global</i> metrics that try to summarize
                          the fit of the entire model into a single number."), style="width:60%")),
                 tags$details(tags$summary("Interpretation", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),(
                  p(HTML("The <b>chi-square test</b> evaluates whether the interitem covariance matrix predicted by your model is within
                         sampling variability of the interitem covariance matrix observed in the data. A small p-value indicates that
                         the two matrices are not equal. This is a strict test with varying opinions about the utility of testing for exact fit.<br>
                         <br>
                         <b>SRMR</b> summarizes the typical difference between each observed interitem correlation and each predicted interitem correlation.
                         Values close to 0 indicate better fit because it means that the predicted and observed correlations are close to each other.<br>
                         <br>
                         <b>RMSEA</b> is a parsimonious fit index that evaluates how far your model is from a perfect fitting model, adjusted for the
                         complexity of the model (simpler models are rewarded). Values close to 0 indicate better fit because they indicate that the
                         model is closer to exactly fitting the model, relative to the number of estimated parameters.<br>
                         <br>
                         <b>CFI</b> is an incremental fit index which evaluates how much better your model fits than a baseline model that
                         assumes that the items have 0 covariance with each other. Values close to 1 indicate better fit because they
                         mean that your model is a greater improvement over the baseline model.
                         "),style="width:60%"))),

                  h4("Dynamic Fit Index Cutoffs for the Model and Data:"),
                  tableOutput("DFI"),
                  downloadButton("download","Download DFI Report"),
                  p(HTML(" "),style="margin-top:12px;"),
                  tags$details(tags$summary("Purpose", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),(
                   p(HTML("Fit indices try to quantify the misfit magnitude. A pervasive practical question with their use is
                          'how much misfit is too much'? This is similar to other effect-size measures like Cohen's <i>d</i> where it is not intrinsically clear which
                          value signals a practically meaningful effect.<br>
                          <br>
                          Traditionally, fixed cutoffs like RMSEA < .05 or CFI > .95 have been suggested benchmarks for 'too much misfit'. However,
                          fit indices are unstandardized, so their interpretation changes depending on aspects like the number of items, the response scale,
                          the number of factors, etc. Therefore, fixed benchmarks may arbitrarily reward or punish certain types
                          of models and data. <br>
                          <br>
                          DFI performs a custom simulation to identify what fit index values are reasonable benchmarks for 'too much misfit'
                          with <i>your</i> specific data and with <i>your</i> specific model. The idea is to essentially perform a fit index power analysis.<br>
                          <br> Just as power analysis tailors the sample size to a specific research design, DFI tailors fit index cutoffs to specific characteristics
                          of your scale."),
                          style="width:60%"))),
                 tags$details(tags$summary("Interpretation", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),(
                 p(HTML("This table shows fit index cutoffs that are specifically tailored to your model, data, response scale, estimator,
                 and missing data pattern. There are multiple rows to facilitate interpreting fit indices as continuous effect sizes rather
                 than dichotomously as ‘good’ or ‘bad’.<br>
                 <br>
                 For RMSEA and SRMR (if reported), closeness of fit is determined by the row where the value from your model is <i>equal to
                 or lower</i> than the cutoff.<br>
                 <br>
                 For CFI, closeness of fit is determined by the row where the value from your model is <i>equal to or higher</i> than the cutoff.<br>
                 <br>
                 The ‘Sensitivity’ row indicates the power that the cutoff has to detect a misspecification magnitude in the specified row.
                 If Sensitivity is below 50%, no cutoff is displayed. This is more likely to occur with small samples or lower reliability and indicates
                 that fit indices cannot consistently differentiate correct and incorrect models with your model and data.<br>
                 <br>
                 With the Direct Discrepancy method, the misspecification levels are standardized (based on the <i>mean absolute discrepancy</i> or <i>MAD</i>)
                 and are the same across all models. This allows cutoffs to be directly compared across different models. The MAD is the average
                 difference between observed and predicted correlations in the simulation.<br>
                 <br>
                 With the Omitted Paths method, the misspecification levels are model specific. The ‘Levels’ in the output are useful for describing the fit of
                 one model in isolation, but ‘Level-1’ in one model does not necessarily compare to ‘Level-1’ in a different model. Therefore, Levels are not
                 necessarily comparable across models.<br>"), style="width:60%"))),
                #tags$details(tags$summary("References", style = "display: list-item; font-size:14px;; margin-bottom:8px; font-weight:bold;"),
                #             p(HTML("test"))),
                #uiOutput("refs"),
                tags$details(tags$summary("References", style = "display: list-item; font-size:14px;; margin-bottom:8px; font-weight:bold;"),
                #             p(HTML(textOutput(cat(unlist("ref"), sep="\n\n"))))),
                                #htmlOutput("refs",inline=TRUE,container=tags$details(tags$summary("References", style = "display: list-item; font-size:14px;; margin-bottom:8px; font-weight:bold;"))),
                  #p("Ref")),
                 uiOutput("ref1")),
                p(HTML("<br/>")),

                ),

       #DFI plots
       tabPanel(title = "DFI Plot",
                p(HTML(" "),style="margin-top:15px;"),
                tags$details(tags$summary("Purpose", style = "display: list-item;font-size:14px; font-weight:bold; margin-bottom:8px"),(
                  p(HTML("These distributions are used to derive fit index cutoffs by identifying the fit index value that optimally separates
                  and classifies correct and incorrect models.<br>
                  <br>
                  An optimal cutoff will not reject more than 5% of the blue distribution corresponding to correct models
                  while rejecting at least 90% of the red distribution corresponding to incorrect models"), style="width:60%"))),
                tags$details(tags$summary("Interpretation", style = "display: list-item;font-size:14px; font-weight:bold;; margin-bottom:8px"),(
                  p(HTML("This app simulates fit index distributions assuming your model is correct (in blue) and assuming your
                  model is incorrect (in red) to different degrees. This tab shows plots with these simulated distributions.
                  Each row represents a different degree of incorrectness of hypothetical misspecification. <br>
                  <br>
                  Each plot has a line representing the optimal DFI cutoff for your specific model and data, and a line representing the traditional
                  cutoff from Hu and Bentler (1999)"),style="width:60%"))),
                  p(HTML("<br/>")),

                #updates based on number of plots in specified function
                  uiOutput("plots")
                ),

       tabPanel(title = "Correlation Residual Matrix",
                h4(HTML("Correlation Residual Matrix"),style="margin-top:15px;"),
                tableOutput("SRM"),

                tags$details(tags$summary("Purpose", style = "display: list-item;font-size:14px; ; margin-bottom:8px; font-weight:bold;"),
                p(HTML("<li>This is the difference between the interitem correlations observed in the data and the interitem correlations predicted by the model")),
                p(HTML("<li>Global fit indices like SRMR, RMSEA, and CFI are essentially trying to summarize this matrix with a single number")),
                p(HTML("<li>Inspecting individual elements of the matrix can be useful for identifying areas of local strain or local misfit")),
                p(HTML("<li>Local fit assessment is a commonly recommended complement to global fit indices"))),

                tags$details(tags$summary("Interpretation", style = "display: list-item; font-size:14px; font-weight:bold;; margin-bottom:8px;"),
                p(HTML("<li>Entries with absolute value below .10 are generally considered to be acceptable")),
                p(HTML("<li>Entries with absolute value above .10 may indicate a misspecification that is causing the predicted interitem correlations to be inaccurate"))),

                tags$details(tags$summary("References", style = "display: list-item; font-size:14px;; margin-bottom:8px; font-weight:bold;"),
                p(HTML("<br>Appelbaum, M., Cooper, H., Kline, R. B., Mayo-Wilson, E., Nezu, A. M., & Rao, S. M. (2018). Journal article
                reporting standards for quantitative research in psychology: The APA Publications and Communications Board task force report.
                American Psychologist, 73(1), 3-25.<br>
                <br>
                McDonald, R. P., & Ho, M. H. R. (2002). Principles and practice in reporting structural equation
                analyses. Psychological Methods, 7(1), 64-82.<br>
                <br>
                Thoemmes, F., Rosseel, Y., & Textor, J. (2018). Local fit evaluation of structural equation models
                using graphical criteria. Psychological Methods, 23(1), 27-41.<br>
                <br>
                West, S.G., Wu, W., McNeish, D., & Savord, A. (2023). Model fit in structural equation modeling.
                In R.H. Hoyle (Ed.), Handbook of Structural Equation Modeling (2nd Ed.) New York: Guilford Press, pp. 184-205."),style="width:60%"))
                ),

       #Model estimates
       tabPanel(title = "Standardized Model Estimates",
                h4("Loadings:"),
                tableOutput("Load"),

                h4("Correlations:"),
                tableOutput("Corr"),

                h4("Variances:"),
                tableOutput("Var")
                ),

       #Path diagram
       tabPanel(title = "Path Diagram",
                p(HTML(" "),style="margin-top:15px;"),
                tags$details(tags$summary("Interpretation", style = "display: list-item;font-size:14px; ; margin-bottom:8px; font-weight:bold;"),
                             p(HTML("This is a visual depiction of the specified model. Circles are factors,
                             rectangles are observed item responses, single-headed arrows are regression paths,
                             and double-headed arrows are covariances."), style="width:60%;")),
                tags$details(tags$summary("Reference", style = "display: list-item;font-size:14px; ; margin-bottom:8px; font-weight:bold;"),
                            p(HTML("Epskamp, S. (2015). semPlot: Unified visualizations of structural equation models.
                              Structural Equation Modeling, 22(3), 474-483."), style="width:60%;")),
                plotOutput("PD",width="7in", height="5in")
                ),

       #lavaan model
        tabPanel(title = "lavaan Model Statement",
                 verbatimTextOutput("Model"),
                 p(HTML("This syntax can be copied and pasted in R to fit the same model in the lavaan package"), style="margin-top:8px;")
                 ),

       #FAQS
       tabPanel(title="FAQs",
                p(HTML(" "),style="margin-top:12px;"),
                p(HTML("<i>Have a question that is not answered here? Submit it <a href='mailto:dmcneish@asu.edu,missgord@gmail.com?subject=DFI FAQ Suggestion'>here.</a></i>")),
                p(HTML("<b><ol><li>Does the application save or cache data that are uploaded?</b>")),
                tags$details(tags$summary("Answer", style = "display: list-item;"),
                p(HTML("<br>No! The data are stored in a temporary folder that is deleted once the session is terminated."), style="width:60%")),

                p(HTML("<br>")),

                p(HTML("<b><li>How long does it take to compute DFI cutoffs? </b>")),
                tags$details(tags$summary("Answer", style = "display: list-item;"),
                p(HTML("<br>Computational times are a function of model size, response scale, and precision.
                Some models can produce cutoffs in under a minute, other models may take 30 minutes or more.<br>
                <br>
                Bigger models generally require more time. Higher precision generally requires more time. The computational
                time by response scale  is Normal < Non-Normal/Ordinal << Categorical. Normality is the fastest because it
                simulates directly from a multivariate normal distribution and does not try to match the distributions in the data.
                Computational time with other response scales is slower because they have intermediate steps to generate data
                that match the distributions/categories in the data.<br>
                <br>
                If you are worried about computational time or have many models, start with the 'Rough' precision option.
                This uses fewer simulation replications and provides a quicker (but less precise) idea of the suitable cutoffs. Reserve higher
                precision for final models.<br>
                <br>
                Additionally, you can use the ‘Download Results Automatically’ option directly above 'Submit' to save a report of the DFI output
                automatically when computation is complete so that you do not have to wait for the app or worry about being disconnected from the server
                and losing your results if you walk away from the computer during longer computational times."), style="width:60%")),

                p(HTML("<br>")),

                p(HTML("<b><li>What is the difference between the Omitted Paths and Direct Discrepancy misspecification methods?</b>")),
                tags$details(tags$summary("Answer", style = "display: list-item;"),
                p(HTML("<br>
                <u>Omitted Paths Approach</u>
                <br>
                The Omitted Path approach tries to identify hypothetical paths that could be added to the original model that would make
                it approximately as misspecified as the  models stuided in Hu and Bentler (1999). The DFI cutoffs are then based on fit index values that
                would allow you to detect that the selected paths had been omitted from the fitted model.<br>
                <br>
                As an analogy, in traditional power analysis, you pick a relevant effect size and the power analysis tells you the sample size needed to detect
                the effect. With the Omitted Paths approach, an algorithm identifies relevant omitted paths (because such paths can be hard for researchers to articulate) and the DFI cutoffs
                tell you the fit index values that would be able to detect that the paths were omitted.<br>
                <br>
                Once suitable paths are identified,the same paths are used in all DFI replications. This method most closely adheres to the design of Hu and Bentler (1999),
                but the results are not always comparable across models because the approach is not standardized (i.e., different models select different paths).<br>
                <br>
                <u>Direct Disrepancy Approach</u>
                <br>
                The Direct Discrepancy method deviates from the approach in Hu and Bentler (1999) by using a <i>matrix</i> based
                definition of misspecification. Misfit is not defined by specific paths but instead is defined by the average difference
                between observed and predicted correlations. This method random samples a discrepancy matrix and adds it directly to your
                model’s predicted correlation matrix. Simulated data will then have a different correlation matrix than predicted by the model.<br>
                <br>
                As an analogy, in traditional power analysis, you pick a relevant effect size and the power analysis tells you the sample size needed to detect
                the effect. With the Direct Discrepancy approach, the effect size is defined by a difference in observed and predicted inter-item correlations.
                The DFI cutoffs then tell you the fit index values that are able to detect that the observed and predicted inter-item correlations differ.<br>
                <br>
                Each replication randomly samples a new discrepancy matrix, so the cutoffs average many possible misspecifications that produce the same difference
                between the observed and predicted inter-item correlations. Because this method is more direct,
                it is easier to generalize to more models and results are comparable across different models. Some types of models can only be fit with this method.<br>
                <br>
                <u>Key Differences</u>
                <ul>
                <li>Direct Discrepancy considers many types of misspecification, Omitted Paths considers one type.
                <li>Direct Discrepancy cutoffs are standardized, Omitted Paths are not.
                <li>Direct Discrepancy can be applied to more models, Omitted Paths is limited to CFA with correlated or orthogonal factors.
                <li>Omitted Paths more closely resembles Hu and Bentler (1999)
                <li>Omitted Paths misspecifications are less correlated fit indices, Direct Discrepancy misspecifciation can be a little circular
                </ul>
                "), style="width:60%")),

                p(HTML("<br>")),

                p(HTML("<b><li>Can I use this for bifactor models?</b>")),
                tags$details(tags$summary("Answer", style = "display: list-item;"),
                p(HTML("<br>Yes! A general factor within a bifactor model can be treated just like any other factor. For instance,
                if a bifactor model has 3 specific factors and 1 general factor, select 4 factors. The first 3 factors would
                represent the specific factors and the 4th factor would load on all items to represent the general factor.<br>
                <br>
                Be sure to indicate the factors do not correlate with a bifactor model, otherwise the model may not be identified.<br>
                <br>
                The Direct Discrepancy method is best suited for bifactor models."), style="width:60%")),

                p(HTML("<br>")),

                p(HTML("<b><li>Can I use this for hierarchical models?</b>")),
                tags$details(tags$summary("Answer", style = "display: list-item;"),
                p(HTML("<br>Yes! After data are uploaded, if you select 3 or more factors, an option for a hierarchical model will
                       appear (at least three factors are needed for a hierarchical model to be identified).<br>
                       <br>
                       If selecting a hierarchical model, a general factor that loads on all specified substantive factors will be included.
                       Only models with a single higher-order factor that loads on all lower-order factors are supported in this application.
                       More complicated hierarchical models are possible in the dynamic R package with the DDDFI function.<br>
                       <br>
                       When selecting the number of factors with a hierarchical model, do <b>not</b> count the general factor."), style="width:60%")),

                p(HTML("<br>")),

                p(HTML("<b><li>Can I use this for measurement invariance?</b>")),
                tags$details(tags$summary("Answer", style = "display: list-item;"),
                             p(HTML("<br>Not yet, unfortunately. Measurement invariance models have additional
                nuances like an overidentified mean structure and constraints which require a different
                approach than the method used in the application.<br>
                <br>
                However, an aim of the grant supporting this work is to develop an approach to support measurement invariance, so
                support for these models may be available in the near future."), style="width:60%")),

                p(HTML("<br>")),

                p(HTML("<b><li>Can I use this for multilevel models?</b>")),
                tags$details(tags$summary("Answer", style = "display: list-item;"),
                p(HTML("<br>Not yet. Multilevel models are complicated by the fact that they have predicted and observed inter-item
                correlation matrices at multiple levels. DFI has not yet been extended to accommodate nuances of level-specific fit.
               "), style="width:60%")),

                p(HTML("<br>")),

                p(HTML("<b><li>Why is the MLR estimator not available with continuous, normal outcomes?</b>")),
                tags$details(tags$summary("Answer", style = "display: list-item;"),
                p(HTML("<br>The MLR and ML estimators are identical when data are truly continuous. When choosing
                the continuous (normal) option for response scale, data are simulated from a
                multivariate normal distribution. In this situation, the MLR estimator is unnecessary because the data
                are known to be normal and the correction term drops out, making ML and MLR equivalent.<br>
                <br>
                If the MLR estimator is desired because non-normality is expected, choosing one of the other response scale options
                is a better idea and will produce more accurate cutoffs."), style="width:60%")),

                p(HTML("<br>")),

                p(HTML("<b><li>Why does treating Likert/Ordinal data as continuous not allow the Direct Discrepancy method?</b>")),
                tags$details(tags$summary("Answer", style = "display: list-item;"),
                p(HTML("<br>When ordinal data are treated as continuous, the model is based on attenuated Pearson correlations. This makes simulating data more
                challenging because there are extra steps necessary to simulate ordinal data that have a specific Pearson correlation matrix
                (it requires an <i>intermediate</i> matrix).<br>
                <br>
                The Direct Discrepancy approach resamples misspecifications for every replication, which means that hundreds or thousands of intermediate
                matrices need to be calculated. This is possible, but it can take a long time (over an hour of computation for each model). <br>
                <br>
                Because the Omitted Paths approach does not resample misspecification, the computational time for intermediate matrices is negligible because it only
                needs to be calculated once.<br>
                <br>
                Therefore, we only offer the Omitted Paths approach for models that treat ordinal data as continuous to keep computational times reasonable.<br><ol>"), style="width:60%")),
                p(HTML("<br>")),
                ),

       #references
        tabPanel(style="width:70%;",title = "References",
                 h4("This Application:"),
                 p(HTML("Wolf, M. G. & McNeish, D. (2020). Dynamic Model Fit. R Shiny application version 2.0.0.")),
                 p(HTML("<br>")),
                 h4("The R package underlying this Application:"),
                 p(HTML("Wolf, M.G. & McNeish, D. (2023). dynamic: An R package for deriving dynamic fit index cutoffs for factor analysis.<i>Multivariate Behavioral Research, 58</i> (1), 189-194.")),
                 p(HTML("<br>")),
                 h4("Methods used to create dynamic fit index cutoffs:"),
                 p(HTML("McNeish, D. & Wolf, M.G. (2023). Dynamic fit index cutoffs for confirmatory factor analysis models. <i>Psychological Methods, 28</i> (1), 61-88.")),
                 p(HTML("McNeish, D. & Wolf, M.G. (in press). Direct discrepancy dynamic fit index cutoffs for arbitrary covariance structure models. <i>Structural Equation Modeling</i>.")),
                 p(HTML("McNeish, D. (2023). Dynamic fit index cutoffs for categorical factor analysis with Likert-type, ordinal, or binary responses. <i>American Psychologist, 79</i> (9), 1061-1075.")),
                 p(HTML("McNeish, D. (in press). Dynamic fit index cutoffs for treating Likert items as continuous. Psychological Methods.")),
                 p(HTML("McNeish, D. & Wolf, M.G. (2023). Dynamic fit index cutoffs for one-factor models. <i>Behavior Research Methods, 55</i> (3), 1157-1174.")),
                 p(HTML("McNeish, D. (2023). Generalizability of dynamic fit index, equivalence testing, and Hu & Bentler cutoffs for evaluating fit in factor analysis. <i>Multivariate Behavioral Research, 58</i> (1), 195-219.")),
                 p(HTML("<br>")),
                 h4("Computationally, this application relies on:"),
                 p(HTML("Epskamp, S. (2015). semPlot: Unified visualizations of structural equation models. Structural Equation Modeling, 22(3), 474-483.")),
                 p(HTML("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36.")),
                 p(HTML("Schneider, W. J. (2019). simstandard: Generate Standardized Data. R package version 0.3.0.")),
                 p(HTML("<br>")),
                 h4("Aesthetically, this application relies on:"),
                 p(HTML("Chang, W., Cheng, J., Allaire, J., Xie, Y., & McPherson, J. (2020). shiny: Web Application Framework for R. R package version 1.4.0.2.")),
                 p(HTML("Attali, D. (2021). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. R package version 2.1.0.")),
                 p(HTML("Attali, D & Sali, A. (2023). shinycssloaders: Add loading animations to a ‘shiny’ output while it’s recalculating.")),
                 p(HTML("Pendersen, T. L. (2020). patchwork: The composer of plots. R package version 1.0.1."))
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
  shinyjs::disable("go")

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
                       #choices=names(data()), multiple=TRUE, options=list(maxItems=2))
                       choices=c(input$Factor1,input$Factor2,
                                 input$Factor3,input$Factor4,
                                 input$Factor5,input$Factor6,
                                 input$Factor7,input$Factor8)
                                 , multiple=TRUE, options=list(maxItems=2))
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

    output$ref1<-renderUI({

      req(input$Scale)
      req(input$Factors)
      req(input$Miss)

      ref1<-"McNeish, D. & Wolf, M.G. (2023). Dynamic fit index cutoffs for confirmatory factor analysis models. Psychological Methods, 28 (1), 61-88."
      ref2<-NULL
      ref3<-NULL
      ref4<-NULL
      ref5<-NULL

      if (input$Miss == "DD"){
        ref2<-"McNeish, D. & Wolf, M.G. (in press). Direct discrepancy dynamic fit index cutoffs for arbitrary covariance structure models.  Structural Equation Modeling."
      }

      if (input$Scale =="C"){
        ref3<-"McNeish, D. (2023). Dynamic fit index cutoffs for factor analysis with Likert-type, ordinal, or binary responses. American Psychologist, 79 (9), 1061-1075."
      }

      if (input$Scale =="L"){
        ref4<-"McNeish, D. (in press). Dynamic fit index cutoffs for treating Likert items as continuous. Psychological Methods."
      }

      if (input$Factors==1){
        ref5<-"McNeish, D. & Wolf, M.G. (2023). Dynamic fit cutoffs for one-factor models. Behavior Research Methods, 55 (3), 1157-1174."
      }

      if (input$Scale =="N"|input$Scale =="NN"){
        ref3<-NULL
        ref4<-NULL
      }

      ref<-list(ref1,ref2,ref3,ref4,ref5)
      ref<-ref[!unlist(lapply(ref, is.null))]
      c<- capture.output(cat(unlist(ref), sep="<br><br>"))


      p(HTML(paste(c)),style="width:60%;")

      })

    observeEvent(c(req(input$Factors),
                   req(eval(parse(text=paste0("input$Factor",input$Factors)))),
                   req(input$Scale),
                   req(input$est),
                   req(input$Reps),
                   req(input$Miss)),
                   {shinyjs::enable("go")})

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
        if (input$Factors==1) {
          COR<-TRUE}
        else{
          COR<-ifelse(input$FacCor=="Yes",TRUE,FALSE)
        }

    #T/F indicator for categorical data in lavaan
    CAT<-ifelse(input$Scale=="C",TRUE,FALSE)

    #Fit model in lavaan
    a<-lavaan::cfa(data=data(), model=model, estimator=input$est, auto.cov.lv.x=COR, ordered=CAT)
    #standardized residual matrix
    b<-lavResiduals(a, type="cor")
    output$SRM<-renderTable({as.data.frame(round(b$cov,2))}, rownames=T)
    q<-lavaan::standardizedsolution(a)

    names(q)[names(q) == 'est.std'] <- 'Std. Estimate'
    names(q)[names(q) == 'se'] <- 'SE'
    names(q)[names(q) == 'z'] <- 'Z'
    names(q)[names(q) == 'pvalue'] <- 'p'
    names(q)[names(q) == 'ci.lower'] <- '95% CI Lower Limit'
    names(q)[names(q) == 'ci.upper'] <- '95% CI Upper Limit'

    qload<-q %>%
                  filter(op=="=~")
                  qload<-qload[,-c(2)]
                  names(qload)[names(qload) == 'lhs'] <- 'Factor'
                  names(qload)[names(qload) == 'rhs'] <- 'Item'

    qvar<-q %>%
                filter(lhs==rhs & op=="~~")
                qvar<-qvar[,-c(1,2)]
                names(qvar)[names(qvar) == 'rhs'] <- 'Variable'

    qcov<-q %>%
                filter(lhs!=rhs & op=="~~")
                qcov<-qcov[,-c(2)]
                names(qcov)[names(qcov) == 'lhs'] <- 'Variable 1'
                names(qcov)[names(qcov) == 'rhs'] <- 'Variable 2'

    output$Load<-renderTable({qload})
    output$Var<-renderTable({qvar})
    output$Corr<-renderTable({qcov})

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
    x1<-x1[,c(1:2,4:6)]
    names(x1)<-c(" ", "MAD", "CFI", "RMSEA", "RMSEA 90% CI")

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

    #output$Ref<-renderText(p(HTML("McNeish, D. & Wolf, M.G. (in press). Direct discrepancy dynamic fit index cutoffs for arbitrary covariance structure models. <i>Structural Equation Modeling</i>.")),
    #                       p(HTML("<br>")),
    #                       p(HTML("McNeish, D. & Wolf, M.G. (2023). Dynamic fit index cutoffs for confirmatory factor analysis models. <i>Psychological Methods, 28</i> (1), 61-88.")),
    #                       p(HTML("<br>")),
    #                       p(HTML("Wolf, M.G. & McNeish, D. (2023). dynamic: An R package for deriving dynamic fit index cutoffs for factor analysis. ,i>Multivariate Behavioral Research, 58</i> (1), 189-194."))
    #                       )

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
        # Render the document using template saved in same folder
        rmarkdown::render("template-FA1.Rmd",
                          output_format = rmarkdown::html_document(),
                          output_file=f,
                          params=list(
                            data = input$upload,
                            name = input$upload$name,
                            Scale = input$Scale,
                            Reps = input$Reps,
                            Factors = input$Factors,
                            est = input$est,
                            Miss = input$Miss,
                            missing= input$missing,
                            PD = PD,
                            fit = DFI$fit,
                            cutoffs=x1
                          ))
      })

    if(D()==TRUE) {
      shinyjs::delay(100,shinyjs::click("download"))
    }

    #output$test<-renderText(unlist(ref))
    #output$refs<-renderUI({tags$details(tags$summary("References", style = "display: list-item; font-size:14px;; margin-bottom:8px; font-weight:bold;"), p(HTML(" ")))
      #p(HTML(paste(cat(unlist(ref), sep="\n\n")))))
    #})

})

}

###########################################

# Run the application
shinyApp(ui = ui, server = server)

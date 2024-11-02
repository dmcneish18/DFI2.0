###packages

#needed for spinner if not installed
devtools::install_github("daattali/shinycssloaders")

library(shiny)
library(lavaan)
library(dplyr)
library(shinycssloaders)
library(MASS)
library(tidyr)
library(purrr)
library(semPlot)
library(shinyjs)
library(rmarkdown)
library(knitr)


######################## Shiny Code

## Code for Interface
ui <- fluidPage(

  shinyjs::useShinyjs(),

## Without Names under the title Application title
    titlePanel(h1(HTML("Dynamic Measurement Invariance Cutoffs"), align="center", style="background-color: #127032; color: white; padding-top:10px;padding-bottom:10px; margin:bottom=0px;"),
               windowTitle = "Dynamic Measurement Invariance Cutoffs"
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
            helpText("8. Choose the estimator (some combinataions of options only have one choice)"),
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

            uiOutput("Group"),
            uiOutput("GroupWarn"),

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
                                     "Likert/Ordinal as Continuous"),
                       choiceValues=c("N","L"), selected=character(0)),

           #information about limited options for Likert/Ordinal responses
           uiOutput("LikertWarn"),
           #Information about limited options for categorical responses
           uiOutput("CatWarn"),
           #Hierarchical Warning about no likert option
           uiOutput("HierWarn2"),


          #Invariance options
          radioButtons("Inv", label="Constraints of Interests/Invariance Type",
                       choiceNames=c("Loadings Only (Metric)",
                                     "Loadings & Intercepts (Metric & Scalar)",
                                     "Loadings, Intercepts, & Residual Variances (Metric, Scalar, & Strong)"),
                       choiceValues=c(1,2,3), selected=character(0)),
          helpText(HTML("<i>More constraints requires testing more models and more computational time</i>")),


            #Estimator; options vary depending on estimator (e.g., no ML for categorical)
            radioButtons("est", label="Estimator",
                         choiceNames=c("Maximum Likelihood",
                                       "Robust Maximum Likelihood (MLR)"),
                         choiceValues=c("ML","MLR"), selected=character(0)),

          #Precision/Reps option
          radioButtons("Reps", label="Select Level of Precision",
                       choiceNames=c(
                                     "Exploratory (100 replications)",
                                     "Full (1000 Replications, mimicking Cheung & Rensvold)",
                                     "(App Developer Test Only)"),
                       choiceValues=c(100,1000,10), selected=character(0)),
          # Helptext underneath box to select items
          helpText(HTML("<i>Higher precision takes more computational time</i>")),

          #automatically download results
            div(style="display:inline-block; width:100%; text-align: center;", checkboxInput("dl", label = "Download results automatically", value = FALSE)),

            #activation button to begin calculations
            #center "submit" within column
            div(style="display:inline-block; width:100%; text-align: center;", actionButton("go", "Submit")),

            #make background blue
            style="background-color: #127032;"
        ),

   #print first 5 rows of data after it is uploaded
   mainPanel(tableOutput("head"),

     #create panels for output
     tabsetPanel(id="Tabs",

       #Welcome/Overview
       tabPanel( title = "Overview",

                 h4("Welcome to Dynamic Measurement Invariance Cutoffs, a tool to customize fit index difference cuttofs for measurement invariance!"),

                 p(HTML("<br/>")),

                 h4(HTML("<li>Measurement invariance is commonly assessed by comparing fit of models with invarinace constraints."),style = "margin-bottom: 30px;"),
                 h4(HTML("<li>But how close do the constrained and unconstrained models need to be to conclude invarinace is reasonable?"),style = "margin-bottom: 30px;"),

                 h4(HTML("<li>Researchers commonly rely on guidelines from Chueng & Rensvold (2002)")),
                 h4(HTML("<ul><li>General suggestion is that &#x0394RMSEA < .01, &#x0394 CFI > -.01, or &#x0394McDonald's Centrality > -.02 indicate reasonable invariance"),style = "margin-bottom: 30px;"),

                 h4(HTML("<li>However, statistical simulations show that these criteria do not generalize well")),
                 h4(HTML("<ul><li>Common criteria only work for a subset of factor structures and data types")),
                 h4(HTML("<ul><li>These criteria can arbitrarily reward or punish certain types of models or data."), style = "margin-bottom: 30px;"),

                 h4(HTML("<li><b>This software simulates custom &#x0394RMSEA, &#x0394 CFI, &#x0394McDonald's Centrality  cutoff values</b>")),
                  h4(HTML("<ul><li>Cutoffs are optimized for your specific model and data charactersitics")),
                  h4(HTML("<ul><li>Separate cutoffs are provided for metric, scalar, and strong invariance "), style = "margin-bottom: 30px;"),
                 ),
       #DMI Cutoff table
       tabPanel(title = "DMI Table",
                 h4("Your Model Fit Indices:"),
                 tableOutput("Fit"),
                 h4("Your Fit Index Differences:"),
                 tableOutput("Del"),
                 tags$details(tags$summary("Purpose", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                  p(HTML("These are the fit index differences for when applying models with and without invariance constraints to your data with the
                         specified factor structure."), style="width:60%")),
                 tags$details(tags$summary("Interpretation", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),(
                  p(HTML("These values tell you how much worse the model fit after adding constraints across groups. Metric Invariance constrains the loadings; Scalar
                  Invariance constrains the loadings and items means; and Strong Invariance constrains the loadings, item intercepts, and residual variances.
                  <br>
                  <br>
                  When constraints are added across groups, fit indices are expected to be a little worse by chance due to sampling variability. The practical question
                  is demarcating how much of a decrease is acceptable (and suggesting invariance) versus problematic (and suggesting non-invariance).<br>
                  <br>
                  Fit index difference cutoffs are one piece of evidence that can help make this decision.

                         "),style="width:60%"))),

                  h4("Dynamic Measurement Invariance  Cutoffs for the Model and Data:"),
                  tableOutput("DFI"),
                  downloadButton("download","Download DFI Report"),
                  htmlOutput("GroupPrint"),
                  htmlOutput("GroupWarn2"),

                  #downloadButton("download","Download DFI Report"),
                  p(HTML(" "),style="margin-top:12px;"),
                  tags$details(tags$summary("Purpose", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),(
                   p(HTML("Measurement invariance assessement attempts to answer whether a construct is measured the same way in different groups.
                   <br>
                   <br>
                   <b>Metric</b> Invariance constrains loadings to be equal across groups but allows item means and residual variances to be unique.
                   <br>
                   <br>
                   <b>Scalar</b> Invariance constrains loadings and items means to be equal across groups but allows item residuals variances to be unique.
                   <br>
                   <br>
                   <b>Strong</b> Invariance constraints loadings, item means, and item residual variances to be equal across groups.
                   <br>
                   <br>
                   Fit index differences try to quantify how much worse the model fits when parameters are constrained across groups.
                   A practical question is: how much can fit decrease after applying constraints while still considering fit to be acceptable?
                   Simulations studies have tried to answer this questions and provided commonly used rules of thumb like &#x0394RMSEA < .01,
                   &#x0394 CFI > -.01, or &#x0394McDonald's Centrality > -.02.
                   <br>
                   <br>
                   However, behavior of fit indices is known to change as a function of the number of items, strong of loadings, distribution of responses, and missing data.
                   Therefore, the traditional static cutoffs may not be equally effective for different types of model. This software calculates customized indices that are
                   intended to be directly applicable to your specific model of interest so that you can make the most accurate decisions about invariance as possible."),
                          style="width:60%"))),

                 tags$details(tags$summary("Interpretation", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),(
                 p(HTML("Compare the values in the table under 'Your Fit Index Differences' to the Dynamic Measurement Invariance Cutoffs.<br>
                 <br>
                 Low values are better for RMSEA and SRMR but high values are better for CFI and McDonald's Centrality. So Interpretation is different
                 for different indices.<br>
                 <br>
                 If the &#x0394RMSEA and &#x0394SRMR are <b>smaller</b> than the cutoff, that indicates  evidence for the type of variance. For example,
                 if the cutoff is 0.015 and &#x0394RMSEA in your model is 0.012, there is evidence for invariance (invariance constraints  invariance resulted
                 in a small change in fit and suggest invariance). However, if the &#x0394RMSEA in your model is 0.019, then there is not evidence for invariance (invariance constraints
                  resulted in an unacceptably large change fit and do not suggest invariance). <br>
                 <br>
                 If the &#x0394 CFI and &#x0394McDonald's Centrality are <b>larger</b> than the cutoff, that indicates  evidence for the type of variance. For example,
                 if the cutoff is -0.015 and &#x0394 CFI in your model is -0.012, there is evidence for invariance (invariance constraints  invariance resulted
                 in a small change in fit and suggest invariance). Negative signs can be tricky: smaller absolute values with negative numbers are bigger.
                 However, if the &#x0394 CFI in your model is -0.019, then there is not evidence for invariance (invariance constraints
                  resulted in an unacceptably large change fit and do not suggest invariance).<br>
                 <br>
                 Note that there are different cutoffs associated with different magnitudes of invariance. <i>It is possible that your model may satisfy a less restrictive form of invariance (like Metric)
                 but will not satisfy the cutoff for a more restrictive form of invariance (like Strong) </i>. The conclusions are not all-or-nothing. Typically, scalar invariance
                 is desirable because it allows direct comparison of latent variable means across groups. "),
                   style="width:60%"))),
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

       #Model estimates
       tabPanel(title = "Standardized Model Estimates", tabsetPanel(
         tabPanel("Metric Invariance Model",
                  tags$details(tags$summary("Loadings", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                h5("Group 1:"),
                tableOutput("Loadm2g1"),
                h5("Group 2:"),
                tableOutput("Loadm2g2")),

                tags$details(tags$summary("Intercepts", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                             h5("Group 1:"),
                             tableOutput("Intm2g1"),
                             h5("Group 2:"),
                             tableOutput("Intm2g2")
                ),

                tags$details(tags$summary("Correlations", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                h5("Group 1:"),
                tableOutput("Corrm2g1"),
                h5("Group 2:"),
                tableOutput("Corrm2g2")),

                tags$details(tags$summary("Variances", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                h5("Group 1:"),
                tableOutput("Varm2g1"),
                h5("Group 2:"),
                tableOutput("Varm2g2"))
         ),
         tabPanel("Scalar Invariance Model",
                  tags$details(tags$summary("Loadings", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                               h5("Group 1:"),
                               tableOutput("Loadm3g1"),
                               h5("Group 2:"),
                               tableOutput("Loadm3g2")),

                  tags$details(tags$summary("Intercepts", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                               h5("Group 1:"),
                               tableOutput("Intm3g1"),
                               h5("Group 2:"),
                               tableOutput("Intm3g2")
                  ),

                  tags$details(tags$summary("Correlations", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                               h5("Group 1:"),
                               tableOutput("Corrm3g1"),
                               h5("Group 2:"),
                               tableOutput("Corrm3g2")),

                  tags$details(tags$summary("Variances", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                               h5("Group 1:"),
                               tableOutput("Varm3g1"),
                               h5("Group 2:"),
                               tableOutput("Varm3g2"))
         ),

         tabPanel("Strong Invariance Model",
                  tags$details(tags$summary("Loadings", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                               h5("Group 1:"),
                               tableOutput("Loadm4g1"),
                               h5("Group 2:"),
                               tableOutput("Loadm4g2")
                               ),

                  tags$details(tags$summary("Intercepts", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                               h5("Group 1:"),
                               tableOutput("Intm4g1"),
                               h5("Group 2:"),
                               tableOutput("Intm4g2")
                  ),

                  tags$details(tags$summary("Correlations", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                               h5("Group 1:"),
                               tableOutput("Corrm4g1"),
                               h5("Group 2:"),
                               tableOutput("Corrm4g2")
                               ),

                  tags$details(tags$summary("Variances", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                               h5("Group 1:"),
                               tableOutput("Varm4g1"),
                               h5("Group 2:"),
                               tableOutput("Varm4g2")
                               )
         ),
       )
       ),




       #Path diagram
       tabPanel(title = "Path Diagram",
                p(HTML(" "),style="margin-top:15px;"),
                tags$details(tags$summary("Interpretation", style = "display: list-item;font-size:14px; ; margin-bottom:8px; font-weight:bold;"),
                             p(HTML("This is a visual depiction of the <b>configural</b> model. Circles are factors,
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

       # #FAQS
       # tabPanel(title="FAQs",
       #          p(HTML(" "),style="margin-top:12px;"),
       #          p(HTML("<i>Have a question that is not answered here? Submit it <a href='mailto:dmcneish@asu.edu,missgord@gmail.com?subject=DFI FAQ Suggestion'>here.</a></i>")),
       #          p(HTML("<b><ol><li>Does the application save or cache data that are uploaded?</b>")),
       #          tags$details(tags$summary("Answer", style = "display: list-item;"),
       #          p(HTML("<br>No! The data are stored in a temporary folder that is deleted once the session is terminated."), style="width:60%")),
       #
       #          p(HTML("<br>")),
       #
       #          p(HTML("<b><li>How long does it take to compute DFI cutoffs? </b>")),
       #          tags$details(tags$summary("Answer", style = "display: list-item;"),
       #          p(HTML("<br>Computational times are a function of model size, response scale, and precision.
       #          Some models can produce cutoffs in under a minute, other models may take 30 minutes or more.<br>
       #          <br>
       #          Bigger models generally require more time. Higher precision generally requires more time. The computational
       #          time by response scale  is Normal < Non-Normal/Ordinal << Categorical. Normality is the fastest because it
       #          simulates directly from a multivariate normal distribution and does not try to match the distributions in the data.
       #          Computational time with other response scales is slower because they have intermediate steps to generate data
       #          that match the distributions/categories in the data.<br>
       #          <br>
       #          If you are worried about computational time or have many models, start with the 'Rough' precision option.
       #          This uses fewer simulation replications and provides a quicker (but less precise) idea of the suitable cutoffs. Reserve higher
       #          precision for final models.<br>
       #          <br>
       #          Additionally, you can use the ‘Download Results Automatically’ option directly above 'Submit' to save a report of the DFI output
       #          automatically when computation is complete so that you do not have to wait for the app or worry about being disconnected from the server
       #          and losing your results if you walk away from the computer during longer computational times."), style="width:60%")),
       #
       #          p(HTML("<br>")),
       #
       #          p(HTML("<b><li>What is the difference between the Omitted Paths and Direct Discrepancy misspecification methods?</b>")),
       #          tags$details(tags$summary("Answer", style = "display: list-item;"),
       #          p(HTML("<br>
       #          <u>Omitted Paths Approach</u>
       #          <br>
       #          The Omitted Path approach tries to identify hypothetical paths that could be added to the original model that would make
       #          it approximately as misspecified as the  models stuided in Hu and Bentler (1999). The DFI cutoffs are then based on fit index values that
       #          would allow you to detect that the selected paths had been omitted from the fitted model.<br>
       #          <br>
       #          As an analogy, in traditional power analysis, you pick a relevant effect size and the power analysis tells you the sample size needed to detect
       #          the effect. With the Omitted Paths approach, an algorithm identifies relevant omitted paths (because such paths can be hard for researchers to articulate) and the DFI cutoffs
       #          tell you the fit index values that would be able to detect that the paths were omitted.<br>
       #          <br>
       #          Once suitable paths are identified,the same paths are used in all DFI replications. This method most closely adheres to the design of Hu and Bentler (1999),
       #          but the results are not always comparable across models because the approach is not standardized (i.e., different models select different paths).<br>
       #          <br>
       #          <u>Direct Disrepancy Approach</u>
       #          <br>
       #          The Direct Discrepancy method deviates from the approach in Hu and Bentler (1999) by using a <i>matrix</i> based
       #          definition of misspecification. Misfit is not defined by specific paths but instead is defined by the average difference
       #          between observed and predicted correlations. This method random samples a discrepancy matrix and adds it directly to your
       #          model’s predicted correlation matrix. Simulated data will then have a different correlation matrix than predicted by the model.<br>
       #          <br>
       #          As an analogy, in traditional power analysis, you pick a relevant effect size and the power analysis tells you the sample size needed to detect
       #          the effect. With the Direct Discrepancy approach, the effect size is defined by a difference in observed and predicted inter-item correlations.
       #          The DFI cutoffs then tell you the fit index values that are able to detect that the observed and predicted inter-item correlations differ.<br>
       #          <br>
       #          Each replication randomly samples a new discrepancy matrix, so the cutoffs average many possible misspecifications that produce the same difference
       #          between the observed and predicted inter-item correlations. Because this method is more direct,
       #          it is easier to generalize to more models and results are comparable across different models. Some types of models can only be fit with this method.<br>
       #          <br>
       #          <u>Key Differences</u>
       #          <ul>
       #          <li>Direct Discrepancy considers many types of misspecification, Omitted Paths considers one type.
       #          <li>Direct Discrepancy cutoffs are standardized, Omitted Paths are not.
       #          <li>Direct Discrepancy can be applied to more models, Omitted Paths is limited to CFA with correlated or orthogonal factors.
       #          <li>Omitted Paths more closely resembles Hu and Bentler (1999)
       #          <li>Omitted Paths misspecifications are less correlated fit indices, Direct Discrepancy misspecifciation can be a little circular
       #          </ul>
       #          "), style="width:60%")),
       #
       #          p(HTML("<br>")),
       #
       #          p(HTML("<b><li>Can I use this for bifactor models?</b>")),
       #          tags$details(tags$summary("Answer", style = "display: list-item;"),
       #          p(HTML("<br>Yes! A general factor within a bifactor model can be treated just like any other factor. For instance,
       #          if a bifactor model has 3 specific factors and 1 general factor, select 4 factors. The first 3 factors would
       #          represent the specific factors and the 4th factor would load on all items to represent the general factor.<br>
       #          <br>
       #          Be sure to indicate the factors do not correlate with a bifactor model, otherwise the model may not be identified.<br>
       #          <br>
       #          The Direct Discrepancy method is best suited for bifactor models."), style="width:60%")),
       #
       #          p(HTML("<br>")),
       #
       #          p(HTML("<b><li>Can I use this for hierarchical models?</b>")),
       #          tags$details(tags$summary("Answer", style = "display: list-item;"),
       #          p(HTML("<br>Yes! After data are uploaded, if you select 3 or more factors, an option for a hierarchical model will
       #                 appear (at least three factors are needed for a hierarchical model to be identified).<br>
       #                 <br>
       #                 If selecting a hierarchical model, a general factor that loads on all specified substantive factors will be included.
       #                 Only models with a single higher-order factor that loads on all lower-order factors are supported in this application.
       #                 More complicated hierarchical models are possible in the dynamic R package with the DDDFI function.<br>
       #                 <br>
       #                 When selecting the number of factors with a hierarchical model, do <b>not</b> count the general factor."), style="width:60%")),
       #
       #          p(HTML("<br>")),
       #
       #          p(HTML("<b><li>Can I use this for measurement invariance?</b>")),
       #          tags$details(tags$summary("Answer", style = "display: list-item;"),
       #                       p(HTML("<br>Not yet, unfortunately. Measurement invariance models have additional
       #          nuances like an overidentified mean structure and constraints which require a different
       #          approach than the method used in the application.<br>
       #          <br>
       #          However, an aim of the grant supporting this work is to develop an approach to support measurement invariance, so
       #          support for these models may be available in the near future."), style="width:60%")),
       #
       #          p(HTML("<br>")),
       #
       #          p(HTML("<b><li>Can I use this for multilevel models?</b>")),
       #          tags$details(tags$summary("Answer", style = "display: list-item;"),
       #          p(HTML("<br>Not yet. Multilevel models are complicated by the fact that they have predicted and observed inter-item
       #          correlation matrices at multiple levels. DFI has not yet been extended to accommodate nuances of level-specific fit.
       #         "), style="width:60%")),
       #
       #          p(HTML("<br>")),
       #
       #          p(HTML("<b><li>Why is the MLR estimator not available with continuous, normal outcomes?</b>")),
       #          tags$details(tags$summary("Answer", style = "display: list-item;"),
       #          p(HTML("<br>The MLR and ML estimators are identical when data are truly continuous. When choosing
       #          the continuous (normal) option for response scale, data are simulated from a
       #          multivariate normal distribution. In this situation, the MLR estimator is unnecessary because the data
       #          are known to be normal and the correction term drops out, making ML and MLR equivalent.<br>
       #          <br>
       #          If the MLR estimator is desired because non-normality is expected, choosing one of the other response scale options
       #          is a better idea and will produce more accurate cutoffs."), style="width:60%")),
       #
       #          p(HTML("<br>")),
       #
       #          p(HTML("<b><li>Why does treating Likert/Ordinal data as continuous not allow the Direct Discrepancy method?</b>")),
       #          tags$details(tags$summary("Answer", style = "display: list-item;"),
       #          p(HTML("<br>When ordinal data are treated as continuous, the model is based on attenuated Pearson correlations. This makes simulating data more
       #          challenging because there are extra steps necessary to simulate ordinal data that have a specific Pearson correlation matrix
       #          (it requires an <i>intermediate</i> matrix).<br>
       #          <br>
       #          The Direct Discrepancy approach resamples misspecifications for every replication, which means that hundreds or thousands of intermediate
       #          matrices need to be calculated. This is possible, but it can take a long time (over an hour of computation for each model). <br>
       #          <br>
       #          Because the Omitted Paths approach does not resample misspecification, the computational time for intermediate matrices is negligible because it only
       #          needs to be calculated once.<br>
       #          <br>
       #          Therefore, we only offer the Omitted Paths approach for models that treat ordinal data as continuous to keep computational times reasonable.<br><ol>"), style="width:60%")),
       #          p(HTML("<br>")),
       #          ),

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


  output$Group<-renderUI({
    req(input$upload)
      selectizeInput(inputId="Group",
                     label="Select Group Variable",
                     choices=names(data()), multiple=TRUE, options=list(maxItems=1))
    })

  grp<-reactive({
    req(input$Group)
    x<-data()[,input$Group]
    return(x)
  })

  output$GroupWarn<-renderUI({
    req(input$Group)
    req(nrow(unique(grp()))>2)
    helpText(HTML("<i>Grouping variables with more than 2 categories are not currently supported </i>"))
  })

  output$GroupWarn2<-renderUI({
    req(input$Group)
    req(nrow(unique(grp()))>2)
    HTML("<i>Only showing cutoffs for comparing first two groups, Software does not support 3+ groups </i>")
  })

  shinyjs::disable("download")
  shinyjs::disable("go")

  # Identify which response scale was selected
  N<-reactive({req(input$Scale=="N")})
  L<-reactive({req(input$Scale=="L")})
  G<-reactive(req(input$Factors>2))
  H1<-reactive({req(input$GenFac==1)})
  H0<-reactive({req(input$GenFac==0)})
  D<-reactive({req(input$dl==TRUE)})

  #If normal, don't allow MLR
  observeEvent(N(), {updateRadioButtons(session,"est",
                                                     choiceNames=c("Maximum Likelihood"),
                                                     choiceValues=c("ML"))
                    #updateRadioButtons(session,"Miss",
                    #                   choiceNames=c("Direct Discrepancy Matrix",
                     #                                "Omitted Paths"),
                      #                 choiceValues=c("DD","OP"), selected=character(0))
                    })

  #observeEvent(c(N(),G()), {
  #               if(input$GenFac==0){
  #                  updateRadioButtons(session,"Miss",
  #                                     choiceNames=c("Direct Discrepancy Matrix",
  #                                                   "Omitted Paths"),
  #                                    choiceValues=c("DD","OP"), selected=character(0))
   #                 }

    #               if(input$GenFac==1){
     #               updateRadioButtons(session,"Miss",
    #                                   choiceNames=c("Direct Discrepancy Matrix"),
     #                                  choiceValues=c("DD"))
      #             }
       #         }
       #        )


  #If Likert, no restrictions
  observeEvent(L(), {updateRadioButtons(session,"est",
                                        choiceNames=c("Maximum Likelihood",
                                                      "Robust Maximum Likelihood (MLR)"),
                                        choiceValues=c("ML", "MLR"), selected="Robust Maximum Likelihood (MLR)")
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
                   req(input$Reps)),
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
          COR<-ifelse(input$FacCor=="Yes",FALSE,TRUE)
        }

    #Fit model in lavaan
    a<-lavaan::cfa(data=data(), model=model, estimator=input$est, orthogonal=COR, std.lv=TRUE)
    lav0<-lavaan::partable(a)
    df0<-a@test$standard$df
    imp0<-a@implied$cov[[1]]
    n0<-a@Data@nobs[[1]]
    indm1<-t(as.data.frame(lavaan::fitmeasures(a,c("rmsea","srmr","cfi","mfi","chisq","df","pvalue"))))
    colnames(indm1)<-c("RMSEA","SRMR","CFI","McDonald Centrality", "Chi-Square", "df","p-value")
    rownames(indm1)<-("Configural")

    ##########################################################################
    ##ADD ROWS TO TABLE THAT TAKE DIFFERENCE BETWEEN CURRENT AND PREVIOUS ROW#
    ##########################################################################

    if(input$Inv>0){
      fit2<-lavaan::cfa(data=data(), model=model, estimator=input$est, orthogonal=COR, std.lv=TRUE, group=input$Group,group.equal="loadings")

      q<-lavaan::parameterEstimates(fit2)

      names(q)[names(q) == 'est'] <- 'Estimate'
      names(q)[names(q) == 'se'] <- 'SE'
      names(q)[names(q) == 'z'] <- 'Z'
      names(q)[names(q) == 'pvalue'] <- 'p'
      names(q)[names(q) == 'ci.lower'] <- '95% CI Lower Limit'
      names(q)[names(q) == 'ci.upper'] <- '95% CI Upper Limit'

      q1<-q%>%dplyr::filter(group==1)
      q2<-q%>%dplyr::filter(group==2)

      q1load<-q1 %>%
        dplyr::filter(op=="=~")
      q1load<-q1load[,-c(2,4:6)]
      names(q1load)[names(q1load) == 'lhs'] <- 'Factor'
      names(q1load)[names(q1load) == 'rhs'] <- 'Item'

      q1int<-q1 %>%
        dplyr::filter(op=="~1")
      q1int<-q1int[,-c(2:6)]
      names(q1int)[names(q1int) == 'lhs'] <- 'Variable'

      q1var<-q1 %>%
        dplyr::filter(lhs==rhs & op=="~~")
      q1var<-q1var[,-c(1,2,4:6)]
      names(q1var)[names(q1var) == 'rhs'] <- 'Variable'

      q1cov<-q1 %>%
        dplyr::filter(lhs!=rhs & op=="~~")
      q1cov<-q1cov[,-c(2,4:6)]
      names(q1cov)[names(q1cov) == 'lhs'] <- 'Variable 1'
      names(q1cov)[names(q1cov) == 'rhs'] <- 'Variable 2'

      output$Loadm2g1<-renderTable({q1load})
      output$Intm2g1<-renderTable({q1int})
      output$Varm2g1<-renderTable({q1var})
      output$Corrm2g1<-renderTable({q1cov})

      #group 2
      q2load<-q2 %>%
        dplyr::filter(op=="=~")
      q2load<-q2load[,-c(2,4:6)]
      names(q2load)[names(q2load) == 'lhs'] <- 'Factor'
      names(q2load)[names(q2load) == 'rhs'] <- 'Item'

      q2int<-q2 %>%
        dplyr::filter(op=="~1")
      q2int<-q2int[,-c(2:6)]
      names(q2int)[names(q2int) == 'lhs'] <- 'Variable'

      q2var<-q2 %>%
        dplyr::filter(lhs==rhs & op=="~~")
      q2var<-q2var[,-c(1,2,4:6)]
      names(q2var)[names(q2var) == 'rhs'] <- 'Variable'

      q2cov<-q2 %>%
        dplyr::filter(lhs!=rhs & op=="~~")
      q2cov<-q2cov[,-c(2,4:6)]
      names(q2cov)[names(q2cov) == 'lhs'] <- 'Variable 1'
      names(q2cov)[names(q2cov) == 'rhs'] <- 'Variable 2'

      output$Loadm2g2<-renderTable({q2load})
      output$Intm2g2<-renderTable({q2int})
      output$Varm2g2<-renderTable({q2var})
      output$Corrm2g2<-renderTable({q2cov})

      indm2<-t(as.data.frame(lavaan::fitmeasures(fit2,c("rmsea","srmr","cfi","mfi","chisq","df","pvalue"))))
      colnames(indm2)<-c("RMSEA","SRMR","CFI","McDonald Centrality", "Chi-Square", "df","p-value")
      rownames(indm2)<-("Metric")
      Ind<-rbind(indm1,indm2)

      Dif<-t(as.data.frame(indm2[,1:4]-indm1[,1:4]))
      del='\u0394'
      colnames(Dif)<-c(paste0(del,"RMSEA"), paste0(del,"SRMR"), paste0(del,"CFI"), paste0(del,"McDonald Centrality"))
      rownames(Dif)<-c("Metric")

      Dif<-t(Dif)

      output$Fit<-renderTable(Ind,rownames=TRUE, digits=3)
      output$Del<-renderTable(Dif,rownames=TRUE, digits=3)



    }

    if(input$Inv>1){
      fit3<-lavaan::cfa(data=data(), model=model, estimator=input$est, orthogonal=COR, std.lv=TRUE, group=input$Group,group.equal=c("loadings", "intercepts"))

      qq<-lavaan::parameterEstimates(fit3)

      names(qq)[names(qq) == 'est'] <- 'Estimate'
      names(qq)[names(qq) == 'se'] <- 'SE'
      names(qq)[names(qq) == 'z'] <- 'Z'
      names(qq)[names(qq) == 'pvalue'] <- 'p'
      names(qq)[names(qq) == 'ci.lower'] <- '95% CI Lower Limit'
      names(qq)[names(qq) == 'ci.upper'] <- '95% CI Upper Limit'

      qq1<-qq%>%dplyr::filter(group==1)
      qq2<-qq%>%dplyr::filter(group==2)

      qq1load<-qq1 %>%
        dplyr::filter(op=="=~")
      qq1load<-qq1load[,-c(2,4:6)]
      names(qq1load)[names(qq1load) == 'lhs'] <- 'Factor'
      names(qq1load)[names(qq1load) == 'rhs'] <- 'Item'

      qq1int<-qq1 %>%
        dplyr::filter(op=="~1")
      qq1int<-qq1int[,-c(2:6)]
      names(qq1int)[names(qq1int) == 'lhs'] <- 'Variable'

      qq1var<-qq1 %>%
        dplyr::filter(lhs==rhs & op=="~~")
      qq1var<-qq1var[,-c(1,2,4:6)]
      names(qq1var)[names(qq1var) == 'rhs'] <- 'Variable'

      qq1cov<-qq1 %>%
        dplyr::filter(lhs!=rhs & op=="~~")
      qq1cov<-qq1cov[,-c(2,4:6)]
      names(qq1cov)[names(qq1cov) == 'lhs'] <- 'Variable 1'
      names(qq1cov)[names(qq1cov) == 'rhs'] <- 'Variable 2'

      output$Loadm3g1<-renderTable({qq1load})
      output$Intm3g1<-renderTable({qq1int})
      output$Varm3g1<-renderTable({qq1var})
      output$Corrm3g1<-renderTable({qq1cov})

      #group 2
      qq2load<-qq2 %>%
        dplyr::filter(op=="=~")
      qq2load<-qq2load[,-c(2,4:6)]
      names(qq2load)[names(qq2load) == 'lhs'] <- 'Factor'
      names(qq2load)[names(qq2load) == 'rhs'] <- 'Item'

      qq2int<-qq2 %>%
        dplyr::filter(op=="~1")
      qq2int<-qq2int[,-c(2:6)]
      names(qq2int)[names(qq2int) == 'lhs'] <- 'Variable'

      qq2var<-qq2 %>%
        dplyr::filter(lhs==rhs & op=="~~")
      qq2var<-qq2var[,-c(1,2,4:6)]
      names(qq2var)[names(qq2var) == 'rhs'] <- 'Variable'

      qq2cov<-qq2 %>%
        dplyr::filter(lhs!=rhs & op=="~~")
      qq2cov<-qq2cov[,-c(2,4:6)]
      names(qq2cov)[names(qq2cov) == 'lhs'] <- 'Variable 1'
      names(qq2cov)[names(qq2cov) == 'rhs'] <- 'Variable 2'

      output$Loadm3g2<-renderTable({qq2load})
      output$Intm3g2<-renderTable({qq2int})
      output$Varm3g2<-renderTable({qq2var})
      output$Corrm3g2<-renderTable({qq2cov})

      indm3<-t(as.data.frame(lavaan::fitmeasures(fit3,c("rmsea","srmr","cfi","mfi","chisq","df","pvalue"))))
      colnames(indm3)<-c("RMSEA","SRMR","CFI","McDonald Centrality", "Chi-Square", "df","p-value")
      rownames(indm3)<-("Scalar")
      Ind<-rbind(indm1,indm2,indm3)

      Dif1<-t(as.data.frame(indm2[,1:4]-indm1[,1:4]))
      Dif2<-t(as.data.frame(indm3[,1:4]-indm2[,1:4]))
            Dif<-rbind(Dif1,Dif2)
      del='\u0394'
      colnames(Dif)<-c(paste0(del,"RMSEA"), paste0(del,"SRMR"), paste0(del,"CFI"), paste0(del,"McDonald Centrality"))
      rownames(Dif)<-c("Metric", "Scalar")

      Dif<-t(Dif)
      output$Fit<-renderTable(Ind,rownames=TRUE, digits=3)
      output$Del<-renderTable(Dif,rownames=TRUE, digits=3)


    }

    if(input$Inv>2){
      fit4<-lavaan::cfa(data=data(), model=model, estimator=input$est, orthogonal=COR, std.lv=TRUE, group=input$Group,group.equal=c("loadings", "intercepts", "residuals"))

      qqq<-lavaan::parameterEstimates(fit4)

      names(qqq)[names(qqq) == 'est'] <- 'Estimate'
      names(qqq)[names(qqq) == 'se'] <- 'SE'
      names(qqq)[names(qqq) == 'z'] <- 'Z'
      names(qqq)[names(qqq) == 'pvalue'] <- 'p'
      names(qqq)[names(qqq) == 'ci.lower'] <- '95% CI Lower Limit'
      names(qqq)[names(qqq) == 'ci.upper'] <- '95% CI Upper Limit'

      qqq1<-qqq%>%dplyr::filter(group==1)
      qqq2<-qqq%>%dplyr::filter(group==2)

      qqq1load<-qqq1 %>%
        dplyr::filter(op=="=~")
      qqq1load<-qqq1load[,-c(2,4:6)]
      names(qqq1load)[names(qqq1load) == 'lhs'] <- 'Factor'
      names(qqq1load)[names(qqq1load) == 'rhs'] <- 'Item'

      qqq1int<-qqq1 %>%
        dplyr::filter(op=="~1")
      qqq1int<-qqq1int[,-c(2:6)]
      names(qqq1int)[names(qqq1int) == 'lhs'] <- 'Variable'

      qqq1var<-qqq1 %>%
        dplyr::filter(lhs==rhs & op=="~~")
      qqq1var<-qqq1var[,-c(1,2,4:6)]
      names(qqq1var)[names(qqq1var) == 'rhs'] <- 'Variable'

      qqq1cov<-qqq1 %>%
        dplyr::filter(lhs!=rhs & op=="~~")
      qqq1cov<-qqq1cov[,-c(2,4:6)]
      names(qqq1cov)[names(qqq1cov) == 'lhs'] <- 'Variable 1'
      names(qqq1cov)[names(qqq1cov) == 'rhs'] <- 'Variable 2'

      output$Loadm4g1<-renderTable({qqq1load})
      output$Intm4g1<-renderTable({qqq1int})
      output$Varm4g1<-renderTable({qqq1var})
      output$Corrm4g1<-renderTable({qqq1cov})

      #group 2
      qqq2load<-qqq2 %>%
        dplyr::filter(op=="=~")
      qqq2load<-qqq2load[,-c(2,4:6)]
      names(qqq2load)[names(qqq2load) == 'lhs'] <- 'Factor'
      names(qqq2load)[names(qqq2load) == 'rhs'] <- 'Item'

      qqq2int<-qqq2 %>%
        dplyr::filter(op=="~1")
      qqq2int<-qqq2int[,-c(2:6)]
      names(qqq2int)[names(qqq2int) == 'lhs'] <- 'Variable'

      qqq2var<-qqq2 %>%
        dplyr::filter(lhs==rhs & op=="~~")
      qqq2var<-qqq2var[,-c(1,2,4:6)]
      names(qqq2var)[names(qqq2var) == 'rhs'] <- 'Variable'

      qqq2cov<-qqq2 %>%
        dplyr::filter(lhs!=rhs & op=="~~")
      qqq2cov<-qqq2cov[,-c(2,4:6)]
      names(qqq2cov)[names(qqq2cov) == 'lhs'] <- 'Variable 1'
      names(qqq2cov)[names(qqq2cov) == 'rhs'] <- 'Variable 2'

      output$Loadm4g2<-renderTable({qqq2load})
      output$Intm4g2<-renderTable({qqq2int})
      output$Varm4g2<-renderTable({qqq2var})
      output$Corrm4g2<-renderTable({qqq2cov})

      indm4<-t(as.data.frame(lavaan::fitmeasures(fit4,c("rmsea","srmr","cfi","mfi","chisq","df","pvalue"))))
      colnames(indm4)<-c("RMSEA","SRMR","CFI","McDonald Centrality", "Chi-Square", "df","p-value")
      rownames(indm4)<-("Strong")
      Ind<-rbind(indm1,indm2,indm3,indm4)

      Dif1<-t(as.data.frame(indm2[,1:4]-indm1[,1:4]))
      Dif2<-t(as.data.frame(indm3[,1:4]-indm2[,1:4]))
      Dif3<-t(as.data.frame(indm4[,1:4]-indm3[,1:4]))

      Dif<-rbind(Dif1,Dif2,Dif3)
      del='\u0394'
      colnames(Dif)<-c(paste0(del,"RMSEA"), paste0(del,"SRMR"), paste0(del,"CFI"), paste0(del,"McDonald Centrality"))
      rownames(Dif)<-c("Metric", "Scalar","Strong")

      Dif<-t(Dif)
      output$Fit<-renderTable(Ind,rownames=TRUE, digits=3)
      output$Del<-renderTable(Dif,rownames=TRUE, digits=3)

      }

    #show spinner while calculating
    shinycssloaders::showPageSpinner(caption = "Simulating cutoffs, please wait")


    sig.star<-function(lav=NULL, dat=NULL)
    {

      ss_mod_load <- suppressMessages(lav %>%
                                        dplyr::filter(lhs != rhs) %>%
                                        dplyr::group_by(lhs,op) %>%
                                        dplyr::filter(op != "~1") %>%
                                        dplyr::filter(op != "|") %>%
                                        dplyr::select(lhs,op,rhs,est) %>%
                                        #dplyr::mutate(est=round(est,digits=8)) %>%
                                        dplyr::reframe(rhs=paste(est,"*",rhs,collapse=" + ")) %>%
                                        dplyr::arrange(desc(op)) %>%
                                        tidyr::unite("mod",lhs,op,rhs,sep="") %>%
                                        dplyr::pull(mod))

      ss_mod_res <- suppressMessages(lav %>%
                                       dplyr::filter(lhs == rhs) %>%
                                       dplyr::select(lhs,op,rhs,est) %>%
                                       #dplyr::mutate(est=round(est,digits=8)) %>%
                                       dplyr::reframe(rhs=paste(lhs,"~~",est,"*",rhs)) %>%
                                       dplyr::pull())

      #Collapse into one string because my other functions expect that
      mod <- base::paste(c(ss_mod_load,ss_mod_res), sep="", collapse="\n")

      #gdat<-dplyr::filter(dat,dat[,noquote(paste0(input$Group))]==grp)
      b<-lavaan::cfa(data=dat, model=mod)

      return(b@implied$cov[[1]])
    }

    sig.star0<-sig.star(lav0,dat=data())


    ##helper functions for simulation

  duplication.matrix <- function (n = 1)
    {
      if ((n < 1) | (round(n) != n))
        stop("n must be a positive integer")
      d <- matrix(0, n * n, n * (n + 1)/2)
      count = 0
      for (j in 1:n) {
        d[(j - 1) * n + j, count + j] = 1
        if (j < n) {
          for (i in (j + 1):n) {
            d[(j - 1) * n + i, count + i] <- 1
            d[(i - 1) * n + j, count + i] <- 1
          }
        }
        count = count + n - j
      }
      return(d)
    }

    vech<-function (x)
    {
      if (!is.square.matrix(x))
        stop("argument x is not a square numeric matrix")
      return(t(t(x[!upper.tri(x)])))
    }

    is.square.matrix<-function (x)
    {
      if (!is.matrix(x))
        stop("argument x is not a matrix")
      return(nrow(x) == ncol(x))
    }

    matrix.trace<-function (x)
    {
      if (!is.square.matrix(x))
        stop("argument x is not a square matrix")
      return(sum(diag(x)))
    }

    #lav = parameter table, n = sample size in group G, imp= implied covariance matrix in Group G

    datgencov<-function(lav=NULL, n=NULL, imp=NULL, dat=NULL, df=NULL) {

      #preliminary definitions
      Sigma.gamma0<-imp #model-implied from fitted model
      p<-nrow(Sigma.gamma0) #number of manifest variables
      q<-lav_partable_npar(lav) #number of model parameters
      p.star <- p*(p+1)/2 #number of non-duplicated entries

      discrep<-df/n #desired disscrepancy is df / N
      delta<-discrep

      D.mat <- duplication.matrix(p)
      D <- t(D.mat)%*%D.mat
      Sigma.gamma0<-sig.star(lav,dat)
      W <- Sigma.gamma0
      W.inv <- solve(W)

      #matrix manipulation functions

      #setup tracking matrices
      h<- 1e-8
      Sigma.deriv<- array(NA, c(p,p,q))
      B <- matrix(NA, p.star, q)

      #loop through parameter table and perturb each estimate
      for (i in 1:q){
        lav1<-lav
        lav1[i,14]<- lav1[i,14]+h #should this be ordered or does it not matter because the parameters are fixed anyway?
        #[order(-lav$free),]
        Sigma.gamma <-sig.star(lav1,dat)
        Sigma.deriv[,,i] <- 	(Sigma.gamma-Sigma.gamma0)*(1/h)
        B[,i]<- (-1)*D%*%vech(W.inv%*%Sigma.deriv[,,i]%*%W.inv)
      }

      #randomly draw errors
      set.seed(101492)
      y <- matrix(rnorm(p.star), p.star,1)
      B.qr<- qr(B)
      e.tilt <- qr.resid(B.qr, y)

      E1 <- matrix(0, p,p)
      index<-1
      for (i2 in 1:p){
        for(i1 in i2:p){
          E1[i1, i2]<- e.tilt[index,1]
          index<-index+1
        }
      }

      E2 <- matrix(0, p,p)
      index <- 1
      for (i1 in 1:p){
        for (i2 in i1:p){
          E2[i1, i2]<- e.tilt[index, 1]
          index <- index+1
        }
      }

      E.tilt <- E1+E2-diag(diag(E1))

      #rescale errors to have magnitude that will give desired result
      G <- W.inv %*% E.tilt
      get.kappa <- function(kappa, G, I, delta){
        target<-abs(kappa*matrix.trace(G) - log(det(I+kappa*G))-delta)
        return(target)
      }

      kappa0 <- sqrt(2*delta/matrix.trace(G%*%G))
      I <- diag(p)
      res.kappa<- suppressWarnings(nlm(get.kappa, kappa0, G=G, I=I, delta=delta))
      kappa <- res.kappa$estimate
      iter<- res.kappa$iterations

      kappa<-as.numeric(kappa)
      E <- kappa*E.tilt

      #return matrix with perturbations that make chi-square = df
      Sigma.star <-Sigma.gamma0+E

      return(Sigma.star)

    }

  Sigma.star0<-datgencov(lav=lav0, n=n0, imp=imp0, dat=data(), df=df0)

  ##########################################################
  # add feature to get proportion of sample in each group  #
  # Use this to simulate data with correct balance         #
  ##########################################################

  ### Also deal with:
    #Check factor correlation: auto.cov.lv.x VS orthogonal
    #reproduce missing data
    #add option for residual covariance invariance?
    #Big one is to adapt for Likert



    true_fit_MI <- function(model,reps, n0){

      #Can make this faster by only doing it once
      #Would need to change table. Not sure what would happen to plot.
      #Already did this

      #Number of reps
      r<-reps

      #Set Seed
      set.seed(8675)

      datax<-as.data.frame(MASS::mvrnorm(n=n0*r, mu=rep(0,nrow(Sigma.star0)), Sigma.star0))
      colnames(datax)<-c(unlist(a@Data@ov.names[[1]]))

      prop<-table(data()[,input$Group])[1]/nrow(data()[,input$Group])

      g<-rbinom(n0*r,1,prop)
      datax1<-cbind(datax,g)

      rep <- base::rep(1:r,n0)
      data_true<- base::cbind(datax1,rep)

      #Group and list
      true_data <- data_true %>%
        dplyr::group_by(rep) %>%
        tidyr::nest() %>%
        base::as.list()

      ind<-c("srmr", "rmsea", "cfi", "mfi")
      #Run 500 cfa

      start<-Sys.time()

      ##configural Model
      m1 <- purrr::map(true_data$data,function(x) lavaan::cfa(model=model, group="g",estimator=input$est, orthogonal=COR,
                                                              data=x,
                                                              std.lv=TRUE,
                                                              check.gradient=FALSE,
                                                              check.post=FALSE,
                                                              check.vcov=FALSE,
                                                              control=list(rel.tol=.001)))
      m1_fit <- purrr::map_dfr(m1,~lavaan::fitMeasures(., ind)) %>%
        #`colnames<-`(c("SRMR_M1","RMSEA_M1","CFI_M1", "McD_M1","Chisq_M1"))
        `colnames<-`(c("SRMR_M1","RMSEA_M1","CFI_M1", "McD_M1"))

      # mean(m1_fit$Chisq_M1)

      summary(m1[[1]])
      #metric models
      m2 <- purrr::map(true_data$data,function(x) lavaan::cfa(model=model, group="g",group.equal="loadings",estimator=input$est, orthogonal=COR,
                                                              data=x,
                                                              std.lv=TRUE,
                                                              check.gradient=FALSE,
                                                              check.post=FALSE,
                                                              check.vcov=FALSE,
                                                              control=list(rel.tol=.001)))
      m2_fit <- purrr::map_dfr(m2,~lavaan::fitMeasures(., ind)) %>%
        `colnames<-`(c("SRMR_M2","RMSEA_M2","CFI_M2", "McD_M2"))

      if(input$Inv >1){
      #scalar models
      m3 <- purrr::map(true_data$data,function(x) lavaan::cfa(model=model, group="g",group.equal=c("loadings","intercepts"),estimator=input$est, orthogonal=COR,
                                                              data=x,
                                                              std.lv=TRUE,
                                                              check.gradient=FALSE,
                                                              check.post=FALSE,
                                                              check.vcov=FALSE,
                                                              control=list(rel.tol=.001)))
      m3_fit <- purrr::map_dfr(m3,~lavaan::fitMeasures(., ind)) %>%
        `colnames<-`(c("SRMR_M3","RMSEA_M3","CFI_M3", "McD_M3"))
      }

      if(input$Inv ==3) {
      #strong models
      m4 <- purrr::map(true_data$data,function(x) lavaan::cfa(model=model, group="g",group.equal=c("loadings","intercepts","residuals"),estimator=input$est, orthogonal=COR,
                                                              data=x,
                                                              std.lv=TRUE,
                                                              check.gradient=FALSE,
                                                              check.post=FALSE,
                                                              check.vcov=FALSE,
                                                              control=list(rel.tol=.001)))
      m4_fit <- purrr::map_dfr(m4,~lavaan::fitMeasures(., ind)) %>%
        `colnames<-`(c("SRMR_M4","RMSEA_M4","CFI_M4", "McD_M4"))
      }

      if(input$Inv==1){
        ##might have to change comparison
        ##Should it be 2 vs. 4 and not 3 vs 4?
        m<-cbind(m1_fit, m2_fit) %>%
          mutate(RMSEA_metric=RMSEA_M2-RMSEA_M1)%>%
          mutate(SRMR_metric=SRMR_M2-SRMR_M1)%>%
          mutate(CFI_metric=CFI_M2-CFI_M1)%>%
          mutate(McD_metric=McD_M2-McD_M1)

        res<-apply(m[,9:12],2,quantile, probs=c(.01,.99))

        #signs are different, so save group by lower-is-better and higher-is-better
        pos<-res[2,1:2]
        neg<-res[1,3:4]
        all_res<-as.data.frame(c(pos,neg))

        #one-column per fit index
        metric<-all_res[c(1,2,3,4),]

        #table with more intuitive labels
        table<-cbind(metric)
        colnames(table)<-c("Metric")
        del='\u0394'
        rownames(table)<-c(paste0(del,"RMSEA"), paste0(del,"SRMR"), paste0(del,"CFI"), paste0(del,"McDonald Centrality"))
      }

      if(input$Inv==2){
        ##might have to change comparison
        ##Should it be 2 vs. 4 and not 3 vs 4?
        m<-cbind(m1_fit, m2_fit, m3_fit) %>%
          mutate(RMSEA_metric=RMSEA_M2-RMSEA_M1)%>%
          mutate(RMSEA_Scalar=RMSEA_M3-RMSEA_M2)%>%

          mutate(SRMR_metric=SRMR_M2-SRMR_M1)%>%
          mutate(SRMR_Scalar=SRMR_M3-SRMR_M2)%>%

          mutate(CFI_metric=CFI_M2-CFI_M1)%>%
          mutate(CFI_Scalar=CFI_M3-CFI_M2)%>%

          mutate(McD_metric=McD_M2-McD_M1)%>%
          mutate(McD_Scalar=McD_M3-McD_M2)

        res<-apply(m[,13:20],2,quantile, probs=c(.01,.99))

        #signs are different, so save group by lower-is-better and higher-is-better
        pos<-res[2,1:4]
        neg<-res[1,5:8]
        all_res<-as.data.frame(c(pos,neg))

        #one-column per fit index
        metric<-all_res[c(1,3,5,7),]
        scalar<-all_res[c(2,4,6,8),]

        #table with more intuitive labels
        table<-cbind(metric, scalar)
        colnames(table)<-c("Metric", "Scalar")
        del='\u0394'
        rownames(table)<-c(paste0(del,"RMSEA"), paste0(del,"SRMR"), paste0(del,"CFI"), paste0(del,"McDonald Centrality"))
      }

      if(input$Inv==3){
      ##might have to change comparison
      ##Should it be 2 vs. 4 and not 3 vs 4?
      m<-cbind(m1_fit, m2_fit, m3_fit, m4_fit) %>%
        mutate(RMSEA_metric=RMSEA_M2-RMSEA_M1)%>%
        mutate(RMSEA_Scalar=RMSEA_M3-RMSEA_M2)%>%
        mutate(RMSEA_Strong=RMSEA_M4-RMSEA_M3)%>%

        mutate(SRMR_metric=SRMR_M2-SRMR_M1)%>%
        mutate(SRMR_Scalar=SRMR_M3-SRMR_M2)%>%
        mutate(SRMR_Strong=SRMR_M4-SRMR_M3)%>%

        mutate(CFI_metric=CFI_M2-CFI_M1)%>%
        mutate(CFI_Scalar=CFI_M3-CFI_M2)%>%
        mutate(CFI_Strong=CFI_M4-CFI_M3)%>%

        mutate(McD_metric=McD_M2-McD_M1)%>%
        mutate(McD_Scalar=McD_M3-McD_M2)%>%
        mutate(McD_Strong=McD_M4-McD_M3)

          Sys.time()-start

      res<-apply(m[,17:28],2,quantile, probs=c(.01,.99))

      #signs are different, so save group by lower-is-better and higher-is-better
      pos<-res[2,1:6]
      neg<-res[1,7:12]
      all_res<-as.data.frame(c(pos,neg))

      #one-column per fit index
      metric<-all_res[c(1,4,7,10),]
      scalar<-all_res[c(2,5,8,11),]
      strong<-all_res[c(3,6,9,12),]

      #table with more intuitive labels
      table<-cbind(metric, scalar, strong)
      colnames(table)<-c("Metric", "Scalar", "Strong")
      del='\u0394'
      rownames(table)<-c(paste0(del,"RMSEA"), paste0(del,"SRMR"), paste0(del,"CFI"), paste0(del,"McDonald Centrality"))
      }

      #set.seed(NULL)

      return(round(table,4))

    }

    Results<-true_fit_MI(model=model,reps=as.numeric(input$Reps), n0=n0)

    #hide spinner when calculations are complete
    shinycssloaders::hidePageSpinner()

    updateTabsetPanel(session, inputId = "Tabs", selected = 'DMI Table')


    output$DFI<-renderTable(Results, rownames=TRUE, digits=3)

    PD<-semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE)
    output$PD<-renderPlot(semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE))

    if(input$Inv==1){
      MI<-"Metric"
    }

    if(input$Inv==2){
      MI<-"Metric and Scalar"
    }

    if(input$Inv==3){
      MI<-"Metric, Scalar, and Strong"
    }

    shinyjs::enable("download")
    #  #download results
    output$download <- downloadHandler(
      filename = function() {
       # create default file name
       paste0("DMICutoffs - ",Sys.Date(), ".html")
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
                           Group = input$Group,
                           Factors = input$Factors,
                           missing= input$missing,
                           MI =MI,
                           PD = PD,
                           fit = Ind,
                           Del = Dif,
                           cutoffs=Results
                         ))
     })

    if(D()==TRUE) {
      shinyjs::delay(100,shinyjs::click("download"))
    }


})

}

###########################################

# Run the application
shinyApp(ui = ui, server = server)

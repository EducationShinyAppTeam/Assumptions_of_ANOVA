# Load Packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(ggplot2)
library(car)
library(sortable)
library(shinyWidgets)
library(rstatix)
library(lme4)

# Global Constants, Functions, and Data Sets ----
barley1 <- read.csv("blockingValid.csv", header = TRUE)
barley2 <- read.csv("blockingInvalid.csv", header = TRUE)
barley3 <- read.csv("barley3.csv", header = TRUE)

dragChoices <- c(
  "Normality of residuals",
  "Homoscedasticity",
  "Independence of observations",
  "Linear relationship covariate and the response",
  "Equality of the covariate's slope parameter",
  "No statistically significiant potential outliers",
  "Interaction of block and treatment",
  "Random effects"
)

anovaAssumptions <- dragChoices[1:3]
ancovaAssumptions <- dragChoices[1:6]
blockingAssumptions <- c(dragChoices[1:3], dragChoices[7])
randomEffectsAssumptions <- c(dragChoices[1:3], dragChoices[8])
repeatedMeasuresAssumptions <- c(dragChoices[1:3], dragChoices[7:8])

dragGrader <- function(session, inputName, description, userResponse, ansKey){
  correct <- FALSE
  
  if (all(userResponse %in% ansKey) && length(userResponse) == length(ansKey)) {
    grade <- list(
      feedback = "Congratulations! You got it right!",
      mark = "correct"
    )
    correct <- TRUE
  } else if (any(userResponse %in% ansKey) && length(userResponse) > length(ansKey)) {
    grade <- list(
      feedback = "You've included some assumptions not needed in this model.",
      mark = "partial"
    )
  } else if (all(userResponse %in% ansKey) && length(userResponse) < length(ansKey)) {
    grade <- list(
      feedback = "You've not included all of the assumptions for this model.",
      mark = "partial"
    )
  } else {
    grade <- list(
      feedback = "Please try again.",
      mark = "incorrect"
    )
  }
  
  # TODO: Base score on % correct.
  score <- list(raw = 0, scaled = 0)
  
  if(grade$mark == "correct") {
    score <- list(
      raw = 100,
      scaled = 1
    )  
  } else if(grade$mark == "partial") {
    score <- list(
      raw = 50,
      scaled = 0.5
    )  
  }
  
  # TODO: For future versions add correctResponsesPattern
  # https://github.com/rpc5102/rlocker/issues/8
  stmt <- boastUtils::generateStatement(
    session,
    verb = "answered",
    object = inputName,
    description = description,
    interactionType = "matching",
    response = paste(userResponse, collapse = ", "),
    success = correct,
    score = list(
      min = 0,
      max = 100,
      raw = score$raw,
      scaled = score$scaled
    ),
    completion = correct
  )
  
  boastUtils::storeStatement(stmt)
  
  return(grade)
}

choiceGrader <- function(session, inputName, description, userResponse, ans) {
  correct <- userResponse == ans
  
  stmt <- boastUtils::generateStatement(
    session,
    verb = "answered",
    object = inputName,
    description = paste0("Select the plot that shows a ", description, "."),
    interactionType = "choice",
    response = userResponse,
    success = correct,
    completion = correct
  )
  
  print(stmt)
  
  boastUtils::storeStatement(stmt)
}

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "black",
    ## Header ----
    dashboardHeader(
      title = "Assumptions of ANOVA Models",
      tags$li(
        class = "dropdown",
        actionLink("info",
                   icon("info"))),
      tags$li(
        class = "dropdown",
        tags$a(target = "_blank", icon("comments"),
               href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Assumptions_of_ANOVA"
        )
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")))
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("Drag and Drop Game", tabName = "game1", icon = icon("gamepad")),
        menuItem("Multiple Choice Game", tabName = "game2", icon = icon("gamepad")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Assumptions of ANOVA Models"),
          p("This app introduces the assumptions for different ANOVA models and
            how to test those assumptions."),
          p("You can also learn what will happen if assumptions are invalid."),
          h2("Instructions"),
          tags$ol(
            tags$li("Click the Go button to enter the prerequisites page."),
            tags$li("In the Explore page, view and compare graphics illustrating
                    situations that are valid and invalid under different models."),
            tags$li("Test yourself on which assumptions apply to which model in
                    the Drag and Drop Game."),
            tags$li("Test yourself on when plots show a violation of assumptions
                    in the Multiple Choice Game.")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "explore",
              label = "GO!",
              icon = icon("bolt"),
              size = "large"
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p("This version of the app was developed and coded by Gonghao Liu, Neil J.
            Hatfield, and Robert P. Carey, III.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 11/28/2020 by Gonghao Liu.")
          )
        ),
        ### Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          h2("Prerequisites"),
          box(
            title = "What is ANOVA?",
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "In general, ANOVA refers to a family of statistical techniques that
            assess potential differences in a response (i.e., scale-level
            dependent variable) given one or more factors (i.e., nominal-level
            independent variables with 2+ categories). Each specific model in the
            ANOVA family has their own assumptions."
          ),
          box(
            title = "Why is assumption testing important to ANOVA?",
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "ANOVA methods are primarily used for statistical inference. In order
            for the inference to be valid (regardless of whether we reject/fail
            to reject the null hypothesis), the assumptions which underpin that
            inference must be satisfied. If the assumptions aren't, then any
            p-values or confidence intervals we found don't mean what we want
            them to mean."
          ),
          box(
            title = "What if the assumptions aren't met?",
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "By checking the assumptions before doing inference, we give ourselves
            the opportunity to shift gears. We can do this in a number of ways
            including but not limited to: using a different model, transforming
            the response, using a robust method, using randomization methods, and
            using nonparametric rank-based methods."
          )
        ),
        ### Explore Page ----
        tabItem(
          tabName = "explore",
          h2("Explore"),
          fluidPage(
            tabsetPanel(
              #### ANOVA Tab ----
              tabPanel(
                title = "Oneway ANOVA",
                br(),
                h3("Scenario"),
                p("An experiment was conducted to determine the relationship of
                  honey output and the types of flowers,there were 3 types of
                  flowers, 3 beehives were randomly assigned to each type of
                  flowers with 9 beehives in total."),
                selectInput(
                  inputId = "anovaSelect",
                  label = "Select the assumption you want to test",
                  choices = list(
                    "Normality of Residuals" = "normality",
                    "Homoscedasticity" = "homoscedasticity",
                    "Independence of Observation" = "independence"
                    ),
                  width = NULL
                  ),
                h4("Valid Example"),
                textOutput("anovaTextValid"),
                plotOutput("anovaImageValid"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('anovaImageValid').setAttribute('aria-label',
                  `This plot output is depend on the user's choice. For normality,
                  this is a plot of norm quantile with all the points lay in the
                  97% confidence envelope. For homoscedasticity, this is a plot
                  of response versus explanatory, all the points lay in a random
                  position. For independence, this is a plot of response versus
                  index, all the points lay in a random position.`)
                  })"
                  )),
                h4("Invalid Example"),
                textOutput("anovaTextInvalid"),
                plotOutput("anovaImageInvalid"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('anovaImageInvalid').setAttribute('aria-label',
                  `This plot output is depend on the user's choice. For normality,
                  this is a plot of norm quantile with 2 points lay in the 97%
                  confidence envelope. For homoscedasticity, this is a plot of
                  response versus explanatory, all the points lay in a pattern.
                  For independence, this is a plot of response versus index,
                  all the points lay in a pattern.`)
                  })"
                  ))
                ),
              #### ANCOVA Tab ----
              tabPanel(
                title = "ANCOVA",
                br(),
                h3("Scenario"),
                p("We are wanting to understand the impact of the type of
                  keyboard on how many hours of pain a person experiences in
                  their hands, wrists, and forearms. We suspect that the number
                  of hours a person spends keyboarding is related to the number
                  of hours of pain that they feel. We have 12 volunteers who will
                  use a specific keyboard we assign them for 2 weeks. During that
                  time, they will record the number of hours they use the
                  keyboard and the number of hours of repetitive motion pain
                  during the study period."),
                selectInput(
                  inputId = "ancovaSelect",
                  label = "Select the assumption you want to test",
                  choices = list(
                    "Normality of Residuals" = "normality",
                    "Homoscedasticity" = "homoscedasticity",
                    "Independence of Observation" = "independence",
                    "Linear Relationship covariate and the Response" = "linear",
                    "Equality of the covariate's Slope parameter" = "slope",
                    "No Statistically Significant Potential Outliers" = "outlier"
                    ),
                  width = '45%'
                  ),
                h4("Valid Example"),
                textOutput("ancovaTextValid"),
                plotOutput("ancovaImageValid"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('ancovaImageValid').setAttribute('aria-label',
                  `This plot output is depend on the user's choice. For normality,
                  this is a plot of norm quantile with all the points lay in the
                  97% confidence envelope. For homoscedasticity, this is a plot
                  of response versus explanatory, all the points lay in a random
                  position. For independence, this is a plot of response versus
                  index, all the points lay in a random position. For linear, the
                  plot shows that response and explantory have a linear relationship.
                  For slope, points in the different groups have a similar trend.
                  For outlier, the plot shows that the data don't have obvious outliers.`)
                  })"
                  )),
                h4("Invalid Example"),
                textOutput("ancovaTextInvalid"),
                plotOutput("ancovaImageInvalid"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('ancovaImageInvalid').setAttribute('aria-label',
                  `This plot output is depend on the user's choice. For normality,
                  this is a plot of norm quantile with 2 points lay in the 97%
                  confidence envelope. For homoscedasticity, this is a plot of
                  response versus explanatory, all the points lay in a pattern.
                  For independence, this is a plot of response versus index, all
                  the points lay in a pattern. For linear, the plot shows that
                  response and explantory have no linear relationship. For slope,
                  points in the different groups have a different trend. For outlier,
                  the plot shows that the data have obvious outliers.`)
                  })"
                  ))
                ),
              #### Blocking Tab ----
              tabPanel(
                "Blocking",
                br(),
                h3("Scenario"),
                p("A farmer wants to test out four varieties of barley and see
                  if there is any difference in yield. He has four fields in
                  which he can plant the barley. However, the farmer is aware of
                  differences between each field. For example,"),
                tags$ul(
                  tags$li("One field has a higher clay content in the soil than
                          the others"),
                  tags$li("One field has rockier soil than the others"),
                  tags$li("Two fields are in wetter climates; two are in drier
                          climates"),
                  tags$li("One field very loose soil while another field has much
                          more compacted soil"),
                  tags$li("Two fields are relatively flat, one has a hill in the
                          middle, and the last has a valley.")
                ),
                selectInput(
                  inputId = "blockingSelect",
                  label = "Select the assumption you want to test",
                  choices = list(
                    "Normality of Residuals" = "normality",
                    "Homoscedasticity" = "homoscedasticity",
                    "Independence of Observation" = "independence",
                    "Interaction of Block and Treatment" = "interaction"
                    ),
                  width = "35%"
                  ),
                h4("Valid Example"),
                textOutput("blockingTextValid"),
                plotOutput("blockingImageValid"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('blockingImageValid').setAttribute('aria-label',
                  `This plot output is depend on the user's choice. For normality,
                  this is a plot of norm quantile with all the points lay in the
                  97% confidence envelope. For homoscedasticity, this is a plot
                  of response versus explanatory, all the points lay in a random
                  position. For independence, this is a plot of response versus index,
                  all the points lay in a random position. For interaction, points
                  in the different block have a similar trend.`)
                  })"
                  )),
                h4("Invalid Example"),
                textOutput("blockingTextInvalid"),
                plotOutput("blockingImageInvalid"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('blockingImageInvalid').setAttribute('aria-label',
                  `This plot output is depend on the user's choice. For normality,
                  this is a plot of norm quantile with 2 points lay in the 97%
                  confidence envelope. For homoscedasticity, this is a plot of
                  response versus explanatory, all the points lay in a pattern.
                  For independence, this is a plot of response versus index, all
                  the points lay in a pattern. For interaction, points in the
                  different block have a different trend.`)
                  })"
                  ))
                ),
              #### Random Effects Tab ----
              tabPanel(
                title = "Random Effects",
                br(),
                h3("Scenario"),
                p("Apex Enterprises is a company that builds roadside restaurants
                  carrying one of several promoted trade names, leases franchises
                  to individuals to operate the restaurants, and provides management
                  services. This company employs a large number of personnel
                  officers who interview applicants for jobs in the restaurants.
                  At the end of the interview, the personnel officer assigns a
                  rating between 0 to 100 to indicate the applicant's potential
                  value on the job"),
                p("Apex would like to know two things: How great is the variation
                  is in ratings among all personnel officers? What is the mean
                  rating given by all personnel officers?"),
                selectInput(
                  inputId = "randomEffectSelect",
                  label = "Select the assumption you want to test",
                  choices = list(
                    "Normality of Residuals" = "normality",
                    "Homoscedasticity" = "homoscedasticity",
                    "Independence of Observation" = "independence",
                    "Random Effects" = "random"),
                  width = NULL
                  ),
                h4("Valid Example"),
                textOutput("randomEffectTextValid"),
                plotOutput("randomEffectImageValid"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('randomEffectImageValid').setAttribute('aria-label',
                  `This plot output is depend on the user's choice. For normality,
                  this is a plot of norm quantile with all the points lay in the
                  97% confidence envelope. For homoscedasticity, this is a plot
                  of response versus explanatory, all the points lay in a random
                  position. For independence, this is a plot of response versus
                  index, all the points lay in a random position. For random,
                  this is a plot of norm quantile with all the points lay in the
                  80% confidence envelope.`)
                  })"
                  )),
                h4("Invalid Example"),
                textOutput("randomEffectTextInvalid"),
                plotOutput("randomEffectImageInvalid"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('randomEffectImageInvalid').setAttribute('aria-label',
                  `This plot output is depend on the user's choice. For normality,
                  this is a plot of norm quantile with 2 points lay in the 97%
                  confidence envelope. For homoscedasticity, this is a plot of
                  response versus explanatory, all the points lay in a pattern.
                  For independence, this is a plot of response versus index, all
                  the points lay in a pattern. For random, this is a plot of norm
                  quantile with 1 point laid in the 80% confidence envelope.`)
                  })"
                  ))
                ),
              ##### Repeated Measures Tab ----
              tabPanel(
                title = "Repeated Measures",
                br(),
                h3("Scenario"),
                p("Beer is big business; the craft brewing industry contributed
                  $79.1 billion to the US Economy in 2018 and 550,000+ jobs
                  (PA: $6.335 billion)."),
                p("Getting a craft beer scored can be quite the achievement. In
                  a single blind tasting, judges are given a chilled, properly
                  poured beer and told the style category. They then judge the
                  beer on Aroma (24 pts), Appearance (6 pts), Flavor (40 pts),
                  Mouthfeel (10 pts), and Overall Impression (20 pts)."),
                p("We have decided to put several State College beers to the test: "),
                tags$ul(
                  tags$li("Barnstormer (an IPA from Happy Valley Brewing Company)"),
                  tags$li("Craftsman (a Brown Ale from Happy Valley Brewing Company)"),
                  tags$li("Red Mo (a Red Ale from Otto's Pub and Brewery)"),
                  tags$li("King Richard Red (an Amber Ale from Robin Hood Brewing Co.)")
                ),
                selectInput(
                  inputId = "repeatedMeasureSelect",
                  label = "Select the assumption you want to test",
                  choices = list(
                    "Normality of Residuals" = "normality",
                    "Homoscedasticity" = "homoscedasticity",
                    "Independence of Observation" = "independence",
                    "Interaction of Block and Treatment" = "interaction",
                    "Random Effects" = "random"
                  ),
                  width = "35%"
                  ),
                h4("Valid Example"),
                textOutput("repeatedMeasureTextValid"),
                plotOutput("repeatedMeasureImageValid"),
                tags$script(HTML(
                 "$(document).ready(function() {
                 document.getElementById('repeatedMeasureImageValid').setAttribute('aria-label',
                 `This plot output is depend on the user's choice. For normality,
                 this is a plot of norm quantile with all the points lay in the
                 97% confidence envelope. For homoscedasticity, this is a plot
                 of response versus explanatory, all the points lay in a random
                 position.mFor independence, this is a plot of response versus
                 index, all the points lay in a random position.nFor interaction,
                 points in the different block have a similar trend.For random,
                 this is a plot of norm quantile with all the points lay in the
                 80% confidence envelope.`)
                 })"
                 )),
               h4("Invalid Example"),
               textOutput("repeatedMeasureTextInvalid"),
               plotOutput("repeatedMeasureImageInvalid"),
               tags$script(HTML(
                "$(document).ready(function() {
                document.getElementById('repeatedMeasureImageInvalid').setAttribute('aria-label',
                `This plot output is depend on the user's choice.
                For normality, this is a plot of norm quantile
                with 2 points lay in the 97% confidence envelope.
                For homoscedasticity, this is a plot of response
                versus explanatory, all the points lay in a pattern.
                For independence, this is a plot of response versus index, all
                the points lay in a pattern. For interaction, points in the
                different block have a different trend. For random, this is a
                plot of norm quantile with 1 point laid in the 80% confidence
                envelope.`)
                })"
                ))
               )
              )
            )
          ),
        ### Drag and Drop Game Page ----
        tabItem(
          tabName = "game1",
          withMathJax(),
          h2("Practice/Test Yourself With the Drag and Drop Matching Game"),
          br(),
          tabsetPanel(
            #### ANOVA DnD Game Tab ----
            tabPanel(
              title = "Oneway ANOVA",
              bucket_list(
                header = "Pick the assumptions for Oneway ANOVA models.",
                add_rank_list(
                  text = "Drag assumptions from here",
                  input_id = "dragAnova",
                  labels = sample(
                    x = dragChoices,
                    size = length(dragChoices),
                    replace = FALSE
                  )
                ),
                add_rank_list(
                  text = "to here",
                  input_id = "dropAnova"
                )
              ),
              fluidRow(
                column(
                  width = 2,
                  offset = 1,
                  bsButton(
                    inputId = 'submitAnova',
                    label = "Submit",
                    size = "large",
                    style = "default",
                    disabled = FALSE
                  )
                ),
                column(
                  width = 1,
                  uiOutput('iconAnova')
                ),
                column(
                  width = 8,
                  uiOutput('feedbackAnova')
                )
              )
            ),
            #### ANCOVA DnD Game Tab ----
            tabPanel(
              title = "ANCOVA",
              bucket_list(
                header = "Pick the assumptions for ANCOVA models.",
                add_rank_list(
                  text = "Drag assumptions from here",
                  input_id = "dragAncova",
                  labels = sample(
                    x = dragChoices,
                    size = length(dragChoices),
                    replace = FALSE
                  )
                ),
                add_rank_list(
                  text = "to here",
                  input_id = "dropAncova"
                )
              ),
              fluidRow(
                column(
                  width = 2,
                  offset = 1,
                  bsButton(
                    inputId = 'submitAncova',
                    label = "Submit",
                    size = "large",
                    style = "default",
                    disabled = FALSE
                  )
                ),
                column(
                  width = 1,
                  uiOutput('iconAncova')
                ),
                column(
                  width = 8,
                  uiOutput('feedbackAncova')
                )
              )
            ),
            #### Blocking DnD Game Tab ----
            tabPanel(
              title = "Blocking",
              bucket_list(
                header = "Pick the assumptions for (Oneway) ANOVA models
                          with Blocking.",
                add_rank_list(
                  text = "Drag assumptions from here",
                  input_id = "dragBlocking",
                  labels = sample(
                    x = dragChoices,
                    size = length(dragChoices),
                    replace = FALSE
                  )
                ),
                add_rank_list(
                  text = "to here",
                  input_id = "dropBlocking"
                )
              ),
              fluidRow(
                column(
                  width = 2,
                  offset = 1,
                  bsButton(
                    inputId = 'submitBlocking',
                    label = "Submit",
                    size = "large",
                    style = "default",
                    disabled = FALSE
                  )
                ),
                column(
                  width = 1,
                  uiOutput('iconBlocking')
                ),
                column(
                  width = 8,
                  uiOutput('feedbackBlocking')
                )
              )
            ),
            #### Random Effects DnD Game Tab ----
            tabPanel(
              title = "Random Effects",
              bucket_list(
                header = "Pick the assumptions for ANOVA models with
                          Random Effects.",
                add_rank_list(
                  text = "Drag assumptions from here",
                  input_id = "dragRandomEffect",
                  labels = sample(
                    x = dragChoices,
                    size = length(dragChoices),
                    replace = FALSE
                  )
                ),
                add_rank_list(
                  text = "to here",
                  input_id = "dropRandomEffect"
                )
              ),
              fluidRow(
                column(
                  width = 2,
                  offset = 1,
                  bsButton(
                    inputId = 'submitRandomEffect',
                    label = "Submit",
                    size = "large",
                    style = "default",
                    disabled = FALSE
                  )
                ),
                column(
                  width = 1,
                  uiOutput('iconRandomEffect')
                ),
                column(
                  width = 8,
                  uiOutput('feedbackRandomEffect')
                )
              )
            ),
            #### Repeated Measures DnD Game Tab ----
            tabPanel(
              title = "Repeated Measures",
              bucket_list(
                header = "Pick the assumptions for Repeated Measures models.",
                add_rank_list(
                  text = "Drag assumptions from here",
                  input_id = "dragRepeatedMeasure",
                  labels = sample(
                    x = dragChoices,
                    size = length(dragChoices),
                    replace = FALSE
                  )
                ),
                add_rank_list(
                  text = "to here",
                  input_id = "dropRepeatedMeasure"
                )
              ),
              fluidRow(
                column(
                  width = 2,
                  offset = 1,
                  bsButton(
                    inputId = 'submitRepeatedMeasure',
                    label = "Submit",
                    size = "large",
                    style = "default",
                    disabled = FALSE
                  )
                ),
                column(
                  width = 1,
                  uiOutput('iconRepeatedMeasure')
                ),
                column(
                  width = 8,
                  uiOutput('feedbackRepeatedMeasure')
                )
              )
            )
          )
        ),
        ### Multiple Choice Game Page ----
        tabItem(
          tabName = "game2",
          withMathJax(),
          h2("Choose the Plot That Violates the Assumption"),
          tabsetPanel(
            #### Normality MC Game Tab ----
            tabPanel(
              title = "Normality",
              br(),
              p("Select the plot that shows a violation of normality."),
              fluidRow(
                column(
                  width = 4,
                  plotOutput('normalityGamePlot1'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('normalityGamePlot1').setAttribute('aria-label',
                    `This is a plot of normal quantiles. There are 50 points in
                    the 95% confidence envelope`)
                    })"
                  ))
                ),
                column(
                  width = 4,
                  plotOutput('normalityGamePlot3'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('normalityGamePlot3').setAttribute('aria-label',
                    `This is a plot of normal quantiles. There are 50 points,
                    most of the points lied in the 95% confidence envelope,
                    while others not`)
                    })"
                  ))
                ),
                column(
                  width = 4,
                  plotOutput('normalityGamePlot2'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('normalityGamePlot2').setAttribute('aria-label',
                    `This is a plot of normal quantiles. There are 50 points in
                    the 95% confidence envelope`)
                    })"
                  ))
                )),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "normalitySelected",
                    label = 'Your choice',
                    choices = c(
                      "Plot A",
                      "Plot B",
                      "Plot C")
                  )
                ),
                column(
                  width = 2,
                  br(),
                  bsButton(
                    inputId = 'submitNormality',
                    label = "Submit",
                    size = "large",
                    style = "default",
                    disabled = FALSE
                  )
                ),
                column(
                  width = 6,
                  br(),
                  uiOutput('markNormality')
                )
              )
            ),
            #### Homoscedasticity MC Game Tab ----
            tabPanel(
              title = "Homoscedasticity",
              br(),
              p("Select the plot that shows a violation of homoscedasticity."),
              fluidRow(
                column(
                  width = 4,
                  plotOutput('homoGamePlot1'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('homoGamePlot1').setAttribute('aria-label',
                    `This is a plot of response versus explanatory, all the points
                    lay in a random position.`)
                    })"
                  ))
                ),
                column(
                  width = 4,
                  plotOutput('homoGamePlot2'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('homoGamePlot2').setAttribute('aria-label',
                    `This is a plot of response versus explanatory, all the points
                    lay in a random position.`)
                    })"
                  ))
                ),
                column(
                  width = 4,
                  plotOutput('homoGamePlot3'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('homoGamePlot3').setAttribute('aria-label',
                    `This is a plot of response versus explanatory, all the points
                    lay in a pattern.`)
                    })"
                  ))
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "homoSelected",
                    label = 'Your choice',
                    choices = c(
                      "Plot A",
                      "Plot B",
                      "Plot C")
                  )
                ),
                column(
                  width = 2,
                  br(),
                  bsButton(
                    inputId = 'submitHomo',
                    label = "Submit",
                    size = "large",
                    style = "default",
                    disabled = FALSE
                  )
                ),
                column(
                  width = 6,
                  br(),
                  uiOutput('markHomo')
                )
              )
            ),
            #### Independence of Observations MC Game Tab ----
            tabPanel(
              title = "Independence of Observations",
              br(),
              p("Select the plot that shows a lack of independence of observations."),
              fluidRow(
                column(
                  width = 4,
                  plotOutput('indeGamePlot3'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('indeGamePlot3').setAttribute('aria-label',
                    `This is a plot of response versus index, all the points lay
                    in a pattern.`)
                    })"
                  ))
                ),
                column(
                  width = 4,
                  plotOutput('indeGamePlot2'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('indeGamePlot2').setAttribute('aria-label',
                    `This is a plot of response versus index, all the points lay
                    in a random position.`)
                    })"
                  ))
                ),
                column(
                  width = 4,
                  plotOutput('indeGamePlot1'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('indeGamePlot1').setAttribute('aria-label',
                    `This is a plot of response versus index, all the points lay
                    in a random position.`)
                    })"
                  ))
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "indeSelected",
                    label = 'Your choice',
                    choices = list(
                      "Plot A",
                      "Plot B",
                      "Plot C"
                    )
                  )
                ),
                column(
                  width = 2,
                  br(),
                  bsButton(
                    inputId = 'submitInde',
                    label = "Submit",
                    size = "large",
                    style = "default",
                    disabled = FALSE
                  )
                ),
                column(
                  width = 6,
                  br(),
                  uiOutput('markInde')
                )
              )
            ),
            ##### Linear Relationship MC Game Tab ----
            tabPanel(
              title = "Linear Relationship",
              br(),
              p("Select the plot that shows a non-linear relationship between the
                response and the covariate."),
              fluidRow(
                column(
                  width = 4,
                  plotOutput('linearGamePlot3'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('linearGamePlot3').setAttribute('aria-label',
                    `The plot shows that response and explantory have no linear
                    relationship.`)
                    })"
                  ))
                ),
                column(
                  width = 4,
                  plotOutput('linearGamePlot2'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('linearGamePlot2').setAttribute('aria-label',
                    `The plot shows that response and explantory have a linear
                    relationship.`)
                    })"
                  ))
                ),
                column(
                  width = 4,
                  plotOutput('linearGamePlot1'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('linearGamePlot1').setAttribute('aria-label',
                    `The plot shows that response and explantory have a linear
                    relationship.`)
                    })"
                  ))
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "linearSelected",
                    label = 'Your choice',
                    choices = c(
                      "Plot A",
                      "Plot B",
                      "Plot C"
                    )
                  )
                ),
                column(
                  width = 2,
                  br(),
                  bsButton(
                    inputId = 'submitLinear',
                    label = "Submit",
                    size = "large",
                    style = "default",
                    disabled = FALSE
                  )
                ),
                column(
                  width = 6,
                  br(),
                  uiOutput('markLinear')
                )
              )
            ),
            #### Common Slope MC Game Tab ----
            tabPanel(
              title = "Common slope",
              br(),
              p("Select the plot that shows violation of parallel slopes."),
              fluidRow(
                column(
                  width = 4,
                  plotOutput('slopeGamePlot1'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('slopeGamePlot1').setAttribute('aria-label',
                    `In the plot, points in the different groups have a similar
                    trend.`)
                    })"
                  ))
                ),
                column(
                  width = 4,
                  plotOutput('slopeGamePlot3'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('slopeGamePlot3').setAttribute('aria-label',
                    `In the plot, points in the different groups have a different
                    trend`)
                    })"
                  ))
                ),
                column(
                  width = 4,
                  plotOutput('slopeGamePlot2'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('slopeGamePlot2').setAttribute('aria-label',
                    `In the plot, points in the different groups have a similar
                    trend.`)
                    })"
                  ))
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "slopeSelected",
                    label = 'Your choice',
                    choices = c(
                      "Plot A",
                      "Plot B",
                      "Plot C"
                    )
                  )
                ),
                column(
                  width = 2,
                  br(),
                  bsButton(
                    inputId = 'submitSlope',
                    label = "Submit",
                    size = "large",
                    style = "default",
                    disabled = FALSE
                  )
                ),
                column(
                  width = 6,
                  br(),
                  uiOutput('markSlope')
                )
              )
            ),
            #### Potential Outliers MC Game Tab ----
            tabPanel(
              title = "No potential outliers",
              br(),
              p("Select the plot that shows a violation of outliers."),
              fluidRow(
                column(
                  width = 4,
                  plotOutput('outGamePlot3'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('outGamePlot3').setAttribute('aria-label',
                    `The plot shows that the data has obvious outliers`)
                    })"
                  ))
                ),
                column(
                  width = 4,
                  plotOutput('outGamePlot2'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('outGamePlot2').setAttribute('aria-label',
                    `The plot shows that the data do not have obvious outliers`)
                    })"
                  ))
                ),
                column(
                  width = 4,
                  plotOutput('outGamePlot1'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('outGamePlot1').setAttribute('aria-label',
                    `The plot shows that the data do not have obvious outliers`)
                    })"
                  ))
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "outSelected",
                    label = 'Your choice',
                    choices = c(
                      "Plot A",
                      "Plot B",
                      "Plot C"
                    )
                  )
                ),
                column(
                  width = 2,
                  br(),
                  bsButton(
                    inputId = 'submitOut',
                    label = "Submit",
                    size = "large",
                    style = "default",
                    disabled = FALSE
                  )
                ),
                column(
                  width = 6,
                  br(),
                  uiOutput('markOut')
                )
              )
            ),
            #### Interaction of Block MC Game Tab ----
            tabPanel(
              title = "Interaction of Block",
              br(),
              p("Select the plot that shows a violation of block interaction."),
              fluidRow(
                column(
                  width = 4,
                  plotOutput('interGamePlot1'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('interGamePlot1').setAttribute('aria-label',
                    `In the plot, points in the different block have a similar
                    trend.`)
                    })"
                  ))
                ),
                column(
                  width = 4,
                  plotOutput('interGamePlot3'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('interGamePlot3').setAttribute('aria-label',
                    `In the plot, points in the different block have a different
                    trend`)
                    })"
                  ))
                ),
                column(
                  width = 4,
                  plotOutput('interGamePlot2'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('interGamePlot2').setAttribute('aria-label',
                    `In the plot, points in the different block have a similar
                    trend.`)
                    })"
                  ))
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "interSelected",
                    label = 'Your choice',
                    choices = c(
                      "Plot A",
                      "Plot B",
                      "Plot C"
                    )
                  )
                ),
                column(
                  width = 2,
                  br(),
                  bsButton(
                    inputId = 'submitInter',
                    label = "Submit",
                    size = "large",
                    style = "default",
                    disabled = FALSE
                  )
                ),
                column(
                  width = 6,
                  br(),
                  uiOutput('markInter')
                )
              )
            )
          )
        ),
        ### References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
            ),
          p(
            class = "hangingindent",
            "Bates, D., Maechler, M., Bolker, B., and Walker, S. (2015). Fitting
            Linear Mixed-Effects Models Using lme4. Journal of Statistical Software,
            67(1), 1-48. doi:10.18637/jss.v067.i01."
          ),
          p(
            class = "hangingindent",
            "Carey, R. (2019). boastUtils: BOAST Utilities. (v0.1.0).
            [R Package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create
            dashboards with 'Shiny'. (v0.7.1) [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019). shiny: Web application framework for R. (v1.4.0)
            [R Package]. Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Fox, J. and Weisberg, S. (2019). An R Companion to Applied
            Regression, Third Edition. Thousand Oaks CA: Sage. Avaliable from:
            https://socialsciences.mcmaster.ca/jfox/Books/Companion/"
          ),
          p(
            class = "hangingindent",
            "Hatfield, N. J. (2020), Stat 461: ANOVA Course Notes [course notes],
            Spring 2020."
          ),
          p(
            class = "hangingindent",
            "Kassambara, A. (2020). rstatix: Pipe-Friendly Framework for Basic
            Statistical Tests. R package version 0.6.0.
            https://CRAN.R-project.org/package=rstatix"
          ),
          p(
            class = "hangingindent",
            "Kutner, M. H., Nachtsheim, C. J., Neter, J., and Li, W. (2005),
            Applied Linear Statistical Models [apex enterprises data set],
            New York: McGraw-Hill Irwin"
          ),
          p(
            class = "hangingindent",
            "Oehlert, G. W. (2000), A First Course in Design and Analysis of Experiments
            [keyboarding data set], New York: W. H. Freeman."
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2020). shinyWidgets:
            Custom Inputs Widgets for Shiny. R package version 0.5.3. Avaliable from:
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "de Vries, A., Schloerke, B., and Russell, K. (2019).
            sortable: Drag-and-Drop in 'shiny' Apps with 'SortableJS'. R package
            version 0.4.2. Avaliable from: https://CRAN.R-project.org/package=sortable"
          ),
          p(
            class = "hangingindent",
            "Wickham, W. (2016). ggplot2: Elegant graphics for data analysis.
            [R Package]. Springer-Verlag New York. Available from
            https://ggplot2.tidyverse.org"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define the server ----
server <- function(input, output, session) {
  ## Info button ----
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions",
      text = "Learn the assumptions for each model and how to exam them. Also,
      test yourself with the drag and drop and multiple choice games.",
      type = "info"
    )
  })

  ## Set Up Go Button ----
  observeEvent(input$explore, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "explore"
    )
  })

  ## Exploration Code ----
  ### Explore ANOVA Text ----
  output$anovaTextValid <- renderText({
    switch(
      EXPR = input$anovaSelect,
      "normality" = paste("In this plot, the boundary line should envelop almost
                          all the points in the graph."),
      "homoscedasticity" = paste("The points here should have similar
                                  variability for each x value."),
      "independence" = paste("The points in this graph should have no pattern.")
    )
  })

  output$anovaTextInvalid <- renderText({
    switch(
      EXPR = input$anovaSelect,
      "normality" = paste("In this plot, too many points are located outside of
                          the envelop."),
      "homoscedasticity" = paste("The points in this graph tend to have different
                                 amounts of variation for different x values."),
      "independence" = paste("The points in this graph tend to have a pattern.")
    )
  })

  ### Explore ANOVA Plots ----
  output$anovaImageValid <- renderPlot({
    honey <- data.frame(
      Surplus = c(100, 60, 90, 85, 90, 95, 105, 70, 80),
      Varietal = c(rep("Clover", 3), rep("Orange Blossom", 3), rep("Alfalfa", 3))
    )
    if (input$anovaSelect == "normality") {
      par(cex.axis = 1.5, cex.lab = 1.5, mar = c(4, 4.5, 2, 2))
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = honey$Surplus,
        distribution = "norm",
        envelope = 0.97,
        ylab = "Surplus Honey (lbs)"
      )
    } else if (input$anovaSelect == "homoscedasticity") {
      par(mar = c(4, 4.5, 2, 2))
      stripchart(
        pch = 19,
        cex = 1.5,
        Surplus ~ Varietal,
        vertical = TRUE,
        data = honey,
        cex.lab = 1.5,
        cex.axis = 1.5
        )
    } else if (input$anovaSelect == "independence") {
      par(mar = c(4, 4.5, 2, 2))
      plot(
        honey$Surplus,
        type = "b",
        ylab = "Surplus Honey (lbs)",
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
  })

  output$anovaImageInvalid <- renderPlot({
    honey <- data.frame(
      Surplus = c(50, 40, 55, 85, 80, 82, 105, 180, 192),
      Varietal = c(rep("Clover", 3), rep("Orange Blossom", 3), rep("Alfalfa", 3))
    )
    if (input$anovaSelect == "normality") {
      par(cex.axis = 1.5, cex.lab = 1.5, mar = c(4, 4.5, 2, 2))
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = honey$Surplus,
        distribution = "norm",
        envelope = 0.97,
        ylab = "Surplus Honey (lbs)"
      )
    } else if (input$anovaSelect == "homoscedasticity") {
      par(mar = c(4, 4.5, 2, 2))
      stripchart(
        pch = 19,
        cex = 1.5,
        Surplus ~ Varietal,
        vertical = TRUE,
        data = honey,
        cex.lab = 1.5,
        cex.axis = 1.5)
    } else if (input$anovaSelect == "independence") {
      par(mar = c(4, 4.5, 2, 2))
      plot(
        honey$Surplus,
        type = "b",
        ylab = "Surplus Honey (lbs)",
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
  })

  ### Explore ANCOVA Text ----
  output$ancovaTextValid <- renderText({
    switch(
      EXPR = input$ancovaSelect,
      "normality" = paste("In this plot, the boundary line should envelop almost
                          all the points in the graph."),
      "homoscedasticity" = paste("The points here should have similar variability
                                 for each x value."),
      "independence" = paste("The points in this graph should have no pattern."),
      "linear" = paste("By the graph, we expect to see a linear relationship
                       between covariate and response."),
      "slope" = paste("The rate of change of the response with respect to the
                      covariate should be the same when we look at different
                      levels of the factor, represented by different colors."),
      "outlier" = paste("We expected no visualized outliers.")
    )
  })

  output$ancovaTextInvalid <- renderText({
    switch(
      EXPR = input$ancovaSelect,
      "normality" = paste("In this plot, too many points are located outside of
                          the envelop."),
      "homoscedasticity" = paste("The points in this graph tend to have different
                                 amounts of variation for different x values."),
      "independence" = paste("The points in this graph tend to have a pattern."),
      "linear" = paste("There is no linear relationship between covariate and
                       response in the graph."),
      "slope" = paste("The rate of change of the response with respect to the
                      covariate is different when we we look at different
                      levels of the factor, represented by different colors."),
      "outlier" = paste("There are some visual outliers in the plot.")
    )
  })

  ### Explore ANCOVA Plots ----
  output$ancovaImageValid <- renderPlot({
    keyboarding <- data.frame(
      kbd.type = c(rep("1", 4), rep("2", 4), rep("3", 4)),
      hrs.kbd = c(60, 72, 61, 50, 54, 68, 66, 59, 51, 56, 55, 56),
      hrs.pain = c(85, 95, 69, 58, 41, 74, 71, 52, 34, 40, 41, 40)
    )
    if (input$ancovaSelect == "normality") {
      par(cex.axis = 1.5, cex.lab = 1.5, mar = c(4, 4.5, 2, 2))
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = keyboarding$hrs.pain,
        distribution = "norm",
        envelope = 0.97,
        ylab = "Hours of Pain"
      )
    } else if (input$ancovaSelect == "homoscedasticity") {
      par(mar = c(4, 4.5, 2, 2))
      stripchart(
        pch = 19,
        cex = 1.5,
        hrs.pain ~ kbd.type,
        vertical = TRUE,
        data = keyboarding,
        cex.lab = 1.5,
        cex.axis = 1.5)
    } else if (input$ancovaSelect == "independence") {
      par(mar = c(4, 4.5, 2, 2))
      plot(
        keyboarding$hrs.pain,
        type = "b",
        ylab = "Hours of Pain",
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        cex.lab = 1.5,
        cex.axis = 1.5)
    } else if (input$ancovaSelect == "linear") {
      ggplot(
        data = keyboarding,
        mapping = aes(y = hrs.pain,x = hrs.kbd)
      ) +
        geom_point(size = 3) +
        geom_smooth(method = "lm", formula = y ~x, se = FALSE) +
        ggplot2::theme_bw() +
        theme(
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)
        ) +
        xlab("Hours Spent Keyboarding") +
        ylab("Hours of Pain")
    } else if (input$ancovaSelect == "slope") {
      ggplot(
        data = keyboarding,
        mapping = aes(
          y = hrs.pain,
          x = hrs.kbd,
          group = kbd.type,
          color = kbd.type,
          shape = kbd.type
        )) +
        geom_point(size = 3) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
        ggplot2::theme_bw() +
        theme(
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18)
        ) +
        xlab("Hours Spent Keyboarding") +
        ylab("Hours of Pain") +
        labs(color = "Keyboard Type", shape = "Keyboard Type")
    } else if (input$ancovaSelect == "outlier") {
      key2 <- rstatix::mahalanobis_distance(keyboarding)
      key2 <- cbind(key2, factor = keyboarding$kbd.type)
      ggplot(
        data = key2,
        mapping = aes(
          y = hrs.pain,
          x = hrs.kbd,
          shape = is.outlier,
          color = factor
        )) +
        geom_point(size = 3) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18)
        ) +
        xlab("Hours Spent Keyboarding") +
        ylab("Hours of Pain") +
        labs(color = "Keyboard", shape = "Potential Outlier")
    }
  })

  output$ancovaImageInvalid <- renderPlot({
    keyboarding <- data.frame(
      kbd.type = c(rep("1", 4), rep("2", 4), rep("3", 4)),
      hrs.kbd = c(60, 72, 61, 50, 54, 68, 66, 59, 56, 56, 55, 29),
      hrs.pain = c(190, 200, 69, 58, 41, 54, 61, 52, 4, 2, 5, 120)
    )
    if (input$ancovaSelect == "normality") {
      par(cex.axis = 1.5, cex.lab = 1.5, mar = c(4, 4.5, 2, 2))
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = keyboarding$hrs.pain,
        distribution = "norm",
        envelope = 0.97,
        ylab = "Hours of Pain"
      )
    } else if (input$ancovaSelect == "homoscedasticity") {
      par(mar = c(4, 4.5, 2, 2))
      stripchart(
        pch = 19,
        cex = 1.5,
        hrs.pain ~ kbd.type,
        vertical = TRUE,
        data = keyboarding,
        cex.lab = 1.5,
        cex.axis = 1.5)
    } else if (input$ancovaSelect == "independence") {
      par(mar = c(4, 4.5, 2, 2))
      plot(
        keyboarding$hrs.pain,
        type = "b",
        ylab = "Hours of Pain",
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        cex.lab = 1.5,
        cex.axis = 1.5)
    } else if (input$ancovaSelect == "linear") {
      ggplot(
        data = keyboarding,
        mapping = ggplot2::aes(
          y = hrs.pain,
          x = hrs.kbd
        )) +
        geom_point(size = 3) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)
        ) +
        xlab("Hours Spent Keyboarding") +
        ylab("Hours of Pain")
    } else if (input$ancovaSelect == "slope") {
      ggplot(
        data = keyboarding,
        mapping = aes(
          y = hrs.pain,
          x = hrs.kbd,
          group = kbd.type,
          color = kbd.type,
          shape = kbd.type
        )) +
        geom_point(size = 3) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18)
        ) +
        xlab("Hours Spent Keyboarding") +
        ylab("Hours of Pain") +
        labs(color = "Keyboard Type", shape = "Keyboard Type")
    } else if (input$ancovaSelect == "outlier") {
      key2 <- rstatix::mahalanobis_distance(keyboarding)
      key2$is.outlier <- ifelse(
        test = key2$mahal.dist >= qchisq(p = 0.99, df = 1),
        yes = TRUE,
        no = FALSE
      )
      key2 <- cbind(key2, factor = keyboarding$kbd.type)
      ggplot(
        data = key2,
        mapping = aes(
          y = hrs.pain,
          x = hrs.kbd,
          color = factor,
          shape = is.outlier
        )) +
        geom_point(size = 3) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18)
        ) +
        xlab("Hours Spent Keyboarding") +
        ylab("Hours of Pain") +
        labs(color = "Keyboard", shape = "Potential Outlier")
    }
  })

  ### Explore Blocking Text ----
  output$blockingTextValid <- renderText({
    switch(
      EXPR = input$blockingSelect,
      "normality" = paste("In this plot, the boundary line should envelop almost
                          all the points in the graph."),
      "homoscedasticity" = paste("The points here should have similar variability
                                 for each x value."),
      "independence" = paste("The points in this graph should have no pattern."),
      "interaction" = paste("The data in different groups should have a similar
                            pattern.")
    )
  })

  output$blockingTextInvalid <- renderText({
    switch(
      EXPR = input$blockingSelect,
      "normality" = paste("In this plot, too many points are located outside of
                          the envelop."),
      "homoscedasticity" = paste("The points in this graph tend to have different
                                 amounts of variation for different x values."),
      "independence" = paste("The points in this graph tend to have a pattern."),
      "interaction" = paste("The data in different groups here have different
                            patterns.")
    )
  })

  ### Explore Blocking Plots ----
  output$blockingImageValid <- renderPlot({
    barleyModel <- aov(Yield ~ Treatment + Field, data = barley1)
    if (input$blockingSelect == "normality") {
      par(cex.axis = 1.5, cex.lab = 1.5, mar = c(4, 4.5, 2, 2))
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = barleyModel$residuals,
        distribution = "norm",
        envelope = 0.97,
        ylab = "Yield (bushels per arce)"
      )
    } else if (input$blockingSelect == "homoscedasticity") {
      par(mar = c(4, 4.5, 2, 2))
      stripchart(
        pch = 19,
        cex = 1.5,
        Yield ~ Treatment,
        vertical = TRUE,
        data = barley1,
        cex.lab = 1.5,
        cex.axis = 1.5)
    } else if (input$blockingSelect == "independence") {
      par(mar = c(4, 4.5, 2, 2))
      plot(
        barley1$Yield,
        type = "b",
        ylab = "Yield (bushels per acre)",
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        cex.lab = 1.5,
        cex.axis = 1.5)
    } else if (input$blockingSelect == "interaction") {
      ggplot(
        data = barley1,
        mapping = aes(
          x = Treatment,
          y = Yield,
          color = Field,
          group = Field
        )) +
        geom_point(size = 3) +
        geom_line(size = 1) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18)
        ) +
        xlab("Variety") +
        ylab("Yield (bushels per acre)") +
        labs(color = "Field")
    }
  })

  output$blockingImageInvalid <- renderPlot({
    barleyModel <- aov(Yield ~ Treatment + Field, data = barley2)
    if (input$blockingSelect == "normality") {
      par(cex.axis = 1.5, cex.lab = 1.5, mar = c(4, 4.5, 2, 2))
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = barleyModel$residuals,
        distribution = "norm",
        envelope = 0.5,
        ylab = "Yield (bushels per arce)"
      )
    } else if (input$blockingSelect == "homoscedasticity") {
      par(mar = c(4, 4.5, 2, 2))
      stripchart(
        pch = 19,
        cex = 1.5,
        Yield ~ Treatment,
        vertical = TRUE,
        data = barley2,
        cex.lab = 1.5,
        cex.axis = 1.5)
    } else if (input$blockingSelect == "independence") {
      par(mar = c(4, 4.5, 2, 2))
      plot(
        barley2$Yield,
        type = "b",
        ylab = "Yield (bushels per acre)",
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        cex.lab = 1.5,
        cex.axis = 1.5)
    } else if (input$blockingSelect == "interaction") {
      ggplot(
        data = barley3,
        mapping = aes(
          x = Treatment,
          y = Yield,
          color = Field,
          group = Field
        )) +
        geom_point(size = 3) +
        geom_line(size = 1) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18)
        ) +
        xlab("Variety") +
        ylab("Yield (bushels per acre)") +
        labs(color = "Field")
    }
  })

  ### Explore Random Effect Text ----
  output$randomEffectTextValid <- renderText({
    switch(
      EXPR = input$randomEffectSelect,
      "normality" = paste("In this plot, the boundary line should envelop almost
                          all the points in the graph."),
      "homoscedasticity" = paste("The points here should have similar variability
                                 for each x value."),
      "independence" = paste("The points in this graph should have no pattern."),
      "random" = paste("In this plot, the boundary line should envelop almost all
                       the points in the graph.")
    )
  })

  output$randomEffectTextInvalid <- renderText({
    switch(
      EXPR = input$randomEffectSelect,
      "normality" = paste("In this plot, too many points are located outside of
                          the envelop."),
      "homoscedasticity" = paste("The points in this graph tend to have different
                                 amounts of variation for different x values."),
      "independence" = paste("The points in this graph tend to have a pattern."),
      "random" = paste("In this plot, too many points are located outside of the
                       envelop.")
    )
  })

  ### Explore Random Effects Plots ----
  output$randomEffectImageValid <- renderPlot({
    apex <- data.frame(
      officer = sort(c(rep(LETTERS[1:5], 4))),
      score = c(
        76, 65, 85, 74,
        59, 75, 81, 67,
        49, 63, 61, 46,
        74, 71, 85, 89,
        66, 84, 80, 79
      )
    )
    options("contrasts" = c("contr.sum","contr.poly"))
    apexFE <- aov(score ~ officer, data = apex)
    apexRE <- lme4::lmer(
      formula = score ~ (1|officer),
      data = apex,
      REML = TRUE
    )
    if (input$randomEffectSelect == "normality") {
      par(cex.axis = 1.5, cex.lab = 1.5, mar = c(4, 4.5, 2, 2))
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = resid(apexRE),
        distribution = "norm",
        envelope = 0.92,
        ylab = "Score of Applicant",
        main = "Residuals"
      )
    } else if (input$randomEffectSelect == "homoscedasticity") {
      par(mar = c(4, 4.5, 2, 2))
      stripchart(
        pch = 19,
        cex = 1.5,
        score ~ officer,
        vertical = TRUE,
        data = apex,
        cex.lab = 1.5,
        cex.axis = 1.5)
    } else if (input$randomEffectSelect == "independence") {
      par(mar = c(4, 4.5, 2, 2))
      plot(
        apex$score,
        type = "b",
        ylab = "Score of Applicant",
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        cex.lab = 1.5,
        cex.axis = 1.5)
    } else if (input$randomEffectSelect == "random") {
      par(cex.axis = 1.5, cex.lab = 1.5, mar = c(4, 4.5, 2, 2))
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = lme4::ranef(apexRE)$officer[, "(Intercept)"],
        distribution = "norm",
        envelope = 0.92,
        ylab = "Score of Applicant",
        main = "Random Effects"
      )
    }
  })

  output$randomEffectImageInvalid <- renderPlot({
    apex <- data.frame(
      officer = sort(c(rep(LETTERS[1:5], 4))),
      score = c(
        76, 65, 85, 74,
        5, 75, 81, 67,
        4, 63, 61, 46,
        74, 71, 85, 189,
        66, 84, 80, 79
      )
    )
    options("contrasts" = c("contr.sum","contr.poly"))
    apexFE <- aov(score ~ officer, data = apex)
    apexRE <- lme4::lmer(
      formula = score ~ (1|officer),
      data = apex,
      REML = TRUE
    )
    if (input$randomEffectSelect == "normality") {
      par(cex.axis = 1.5, cex.lab = 1.5, mar = c(4, 4.5, 2, 2))
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = resid(apexRE),
        distribution = "norm",
        envelope = 0.92,
        ylab = "Score of Applicant",
        main = "Residuals"
      )
    } else if (input$randomEffectSelect == "homoscedasticity") {
      par(mar = c(4, 4.5, 2, 2))
      stripchart(
        pch = 19,
        cex = 1.5,
        score ~ officer,
        vertical = TRUE,
        data = apex,
        cex.lab = 1.5,
        cex.axis = 1.5)
    } else if (input$randomEffectSelect == "independence") {
      par(mar = c(4, 4.5, 2, 2))
      plot(
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        apex$score,
        type = "b",
        ylab = "Score of Applicant",
        cex.lab = 1.5,
        cex.axis = 1.5)
    } else if (input$randomEffectSelect == "random") {
      par(cex.axis = 1.5, cex.lab = 1.5, mar = c(4, 4.5, 2, 2))
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = lme4::ranef(apexRE)$officer[, "(Intercept)"],
        distribution = "norm",
        envelope = 0.2,
        ylab = "Score of Applicant",
        main = "Random Effects"
      )
    }
  })

  ### Explore Repeated Measures Text ----
  output$repeatedMeasureTextValid <- renderText({
    switch(
      EXPR = input$repeatedMeasureSelect,
      "normality" = paste("In this plot, the boundary line should envelop almost
                          all the points in the graph."),
      "homoscedasticity" = paste("The points here should have similar variability
                                 for each x value."),
      "independence" = paste("The points in this graph should have no pattern."),
      "interaction" = paste("The data in different group should have same pattern."),
      "random" = paste("In this plot, the boundary line should envelop almost all
                       the points in the graph.")
    )
  })

  output$repeatedMeasureTextInvalid <- renderText({
    switch(
      EXPR = input$repeatedMeasureSelect,
      "normality" = paste("In this plot, too many points are located outside of
                          the envelop."),
      "homoscedasticity" = paste("The points in this graph tend to have different
                                 amounts of variation for different x values."),
      "independence" = paste("The points in this graph tend to have a pattern."),
      "random" = paste("In this plot, too many points are located outside of the
                       envelop."),
      "interaction" = paste("The data in different groups here have different
                            patterns.")
    )
  })

  ### Explore Repeated Measures Plots ----
  output$repeatedMeasureImageValid <- renderPlot({
    beer <- data.frame(
      judge = sort(rep(LETTERS[1:6],4)),
      beer = rep(c("Barnstormer", "King Richard Red",
                   "Craftsman", "Red Mo"), 6),
      score = c(50, 60, 70, 70,
                38, 45, 58, 60,
                45, 48, 60, 58,
                65, 65, 75, 75,
                55, 60, 70, 65,
                48, 53, 68, 63)
    )
    beerM1 <- lme4::lmer(
      formula = score ~ beer + (1|judge),
      data = beer
    )
    if (input$repeatedMeasureSelect == "normality") {
      par(cex.axis = 1.5, cex.lab = 1.5, mar = c(4, 4.5, 2, 2))
      car::qqPlot(
        x = residuals(beerM1),
        distribution = "norm",
        envelope = 0.9,
        ylab = "Score",
        pch = 19,
        cex = 1.5,
        id = FALSE
      )
    } else if (input$repeatedMeasureSelect == "homoscedasticity") {
      par(mar = c(4, 4.5, 2, 2))
      stripchart(
        pch = 19,
        cex = 1.5,
        score ~ beer,
        vertical = TRUE,
        data = beer,
        cex.lab = 1.5,
        cex.axis = 1.5)
    } else if (input$repeatedMeasureSelect == "independence") {
      par(mar = c(4, 4.5, 2, 2))
      plot(
        beer$score,
        type = "b",
        ylab = "Score",
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        cex.lab = 1.5,
        cex.axis = 1.5)
    } else if (input$repeatedMeasureSelect == "interaction") {
      ggplot(
        data = beer,
        mapping = aes(
          x = beer,
          y = score,
          color = judge,
          group = judge
        )) +
        geom_point(size = 3) +
        geom_line(size = 1) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18)
        ) +
        xlab("Beer") +
        ylab("Score") +
        labs(color = "Judge")
    } else if (input$repeatedMeasureSelect == "random") {
      par(cex.axis = 1.5, cex.lab = 1.5, mar = c(4, 4.5, 2, 2))
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = lme4::ranef(beerM1)$judge[, "(Intercept)"],
        distribution = "norm",
        envelope = 0.9,
        ylab = "score"
      )
    }
  })

  output$repeatedMeasureImageInvalid <- renderPlot({
    beer <- data.frame(
      judge = sort(rep(LETTERS[1:6],4)),
      beer = rep(c("Barnstormer", "King Richard Red",
                   "Craftsman", "Red Mo"), 6),
      score = c(
        33, 87, 72, 56,
        43, 45, 67, 62,
        41, 86, 70, 79,
        35, 48, 65, 61,
        33, 89, 69, 64,
        36, 48, 67, 54)
    )
    beerM1 <- lme4::lmer(score ~ beer + (1|judge), data = beer)
    if (input$repeatedMeasureSelect == "normality") {
      par(cex.axis = 1.5, cex.lab = 1.5, mar = c(4, 4.5, 2, 2))
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = residuals(beerM1),
        distribution = "norm",
        envelope = 0.9,
        ylab = "Score"
      )
    } else if (input$repeatedMeasureSelect == "homoscedasticity") {
      par(mar = c(4, 4.5, 2, 2))
      stripchart(
        pch = 19,
        cex = 1.5,
        score ~ beer,
        vertical = TRUE,
        data = beer,
        cex.lab = 1.5,
        cex.axis = 1.5)
    } else if (input$repeatedMeasureSelect == "independence") {
      par(mar = c(4, 4.5, 2, 2))
      plot(
        beer$score,
        type = "b",
        ylab = "Score",
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        cex.lab = 1.5,
        cex.axis = 1.5)
    } else if (input$repeatedMeasureSelect == "interaction") {
      ggplot(
        data = beer,
        mapping = aes(
          x = beer,
          y = score,
          color = judge,
          group = judge
        )) +
        geom_point(size = 3) +
        geom_line(size = 1) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18)
        ) +
        xlab("Beer") +
        ylab("Score") +
        labs(color = "Judge")
    } else if (input$repeatedMeasureSelect == "random") {
      par(cex.axis = 1.5, cex.lab = 1.5, mar = c(4, 4.5, 2, 2))
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = lme4::ranef(beerM1)$judge[, "(Intercept)"],
        distribution = "norm",
        envelope = 0.3,
        ylab = "score"
      )
    }
  })

  ## Drag and Drop Code ----
  observeEvent(
    eventExpr = input$submitAnova,
    handlerExpr = {
      anovaGrade <- dragGrader(
        session,
        inputName = "dnd_ANOVA",
        description = "Oneway ANOVA models",
        userResponse = input$dropAnova,
        ansKey = anovaAssumptions
      )
      output$feedbackAnova <- renderUI({
        anovaGrade$feedback
      })
      output$iconAnova <- renderIcon(icon = anovaGrade$mark, width = 48)
  })

  observeEvent(
    eventExpr = c(input$dragAnova, input$dropAnova),
    handlerExpr = {
      output$feedbackAnova <- renderUI({NULL})
      output$iconAnova <- renderIcon()
    }
  )

  observeEvent(
    eventExpr = input$submitAncova,
    handlerExpr = {
      ancovaGrade <- dragGrader(
        session,
        inputName = "dnd_ANCOVA",
        description = "ANCOVA models",
        userResponse = input$dropAncova,
        ansKey = ancovaAssumptions
      )
      output$feedbackAncova <- renderUI({
        ancovaGrade$feedback
      })
      output$iconAncova <- renderIcon(icon = ancovaGrade$mark, width = 48)
    })

  observeEvent(
    eventExpr = c(input$dragAncova, input$dropAncova),
    handlerExpr = {
      output$feedbackAncova <- renderUI({NULL})
      output$iconAncova <- renderIcon()
    }
  )

  observeEvent(
    eventExpr = input$submitBlocking,
    handlerExpr = {
      blockingGrade <- dragGrader(
        session,
        inputName = "dnd_blocking",
        description = "(Oneway) ANOVA models with Blocking",
        userResponse = input$dropBlocking,
        ansKey = blockingAssumptions
      )
      output$feedbackBlocking <- renderUI({
        blockingGrade$feedback
      })
      output$iconBlocking <- renderIcon(icon = blockingGrade$mark, width = 48)
    })

  observeEvent(
    eventExpr = c(input$dragBlocking, input$dropBlocking),
    handlerExpr = {
      output$feedbackBlocking <- renderUI({NULL})
      output$iconBlocking <- renderIcon()
    }
  )

  observeEvent(
    eventExpr = input$submitRandomEffect,
    handlerExpr = {
      randomEffectGrade <- dragGrader(
        session,
        inputName = "drag_randomEffect",
        description = "ANOVA models with Random Effects",
        userResponse = input$dropRandomEffect,
        ansKey = randomEffectsAssumptions
      )
      output$feedbackRandomEffect <- renderUI({
        randomEffectGrade$feedback
      })
      output$iconRandomeEffect <- renderIcon(
        icon = randomEffectGrade$mark,
        width = 48
      )
    })

  observeEvent(
    eventExpr = c(input$dragRandomEffect, input$dropRandomEffect),
    handlerExpr = {
      output$feedbackRandomEffect <- renderUI({NULL})
      output$iconRandomEffect <- renderIcon()
    }
  )

  observeEvent(
    eventExpr = input$submitRepeatedMeasure,
    handlerExpr = {
      repeatedMeasureGrade <- dragGrader(
        session,
        inputName = "dnd_repeatedMeasure",
        description = "Repeated Measures models",
        userResponse = input$dropRepeatedMeasure,
        ansKey = repeatedMeasuresAssumptions
      )
      output$feedbackRepeatedMeasure <- renderUI({
        repeatedMeasureGrade$feedback
      })
      output$iconRepeatedMeasure <- renderIcon(
        icon = repeatedMeasureGrade$mark,
        width = 48
      )
    })

  observeEvent(
    eventExpr = c(input$dragRepeatedMeasure, input$dropRepeatedMeasure),
    handlerExpr = {
      output$feedbackRepeatedMeasure <- renderUI({NULL})
      output$iconRepeatedMeasure <- renderIcon()
    }
  )

  ## Multiple Choice Code ----
  ### MC Normality ----
  normalityData1 <- rnorm(n = 50, mean = 0, sd = 1)
  normalityData2 <- rnorm(n = 50, mean = 0, sd = 1)
  normalityData3 <-  rbeta(50, 0.7, 1.5)

  output$normalityGamePlot1 <- renderPlot({
    par(cex.axis = 1.5, cex.lab = 1.5, mar = c(4, 4.5, 2, 2))
    car::qqPlot(
      pch = 19,
      cex = 1.5,
      id = FALSE,
      x = normalityData1,
      distribution = "norm",
      envelope = 0.95,
      ylab = "data",
      main = "Plot A"
    )
  })

  output$normalityGamePlot2 <- renderPlot({
    par(cex.axis = 1.5, cex.lab = 1.5, mar = c(4, 4.5, 2, 2))
    car::qqPlot(
      pch = 19,
      cex = 1.5,
      id = FALSE,
      x = normalityData2,
      distribution = "norm",
      envelope = 0.95,
      ylab = "data",
      main = "Plot C"
    )
  })

  output$normalityGamePlot3 <- renderPlot({
    par(cex.axis = 1.5, cex.lab = 1.5, mar = c(4, 4.5, 2, 2))
    car::qqPlot(
      pch = 19,
      cex = 1.5,
      id = FALSE,
      x = normalityData3,
      distribution = "norm",
      envelope = 0.8,
      ylab = "data",
      main = "Plot B"
    )
  })

  observeEvent(
    eventExpr = input$submitNormality,
    handlerExpr = {
      output$markNormality <- renderIcon(
        icon = ifelse(
          test = input$normalitySelected == 'Plot B',
          yes = "correct",
          no = "incorrect"
        ),
        width = 48
      )
      
      choiceGrader(
        session,
        inputName = "normalitySelected",
        description = "violation of block interaction",
        userResponse = input$normalitySelected,
        ans = "Plot B"
      )
    })

  observeEvent(
    eventExpr = input$normalitySelected,
    handlerExpr = {
      output$markNormality <- renderIcon()
    }
  )

  ### MC Homoscedasticity ----
  homoData1 <- data.frame(
    homoData1_1 = c(rep('1',10), rep('2',10), rep('3',10), rep('4',10), rep('5',10)),
    homoData1_2 = c(sample(1:100, 10, replace = FALSE),
                    sample(1:100, 10, replace = FALSE),
                    sample(1:80, 10, replace = FALSE),
                    sample(1:100, 10, replace = FALSE),
                    sample(1:100, 10, replace = FALSE)))
  homoData2 <- data.frame(
    homoData2_1 = c(rep('1',10), rep('2',10), rep('3',10), rep('4',10), rep('5',10)),
    homoData2_2 = c(sample(1:100, 10, replace = FALSE),
                    sample(1:100, 10, replace = FALSE),
                    sample(1:100, 10, replace = FALSE),
                    sample(1:100, 10, replace = FALSE),
                    sample(1:100, 10, replace = FALSE)))
  homoData3 <- data.frame(
    homoData3_1 = c(rep('1',10), rep('2',10), rep('3',10), rep('4',10), rep('5',10)),
    homoData3_2 = c(sample(1:100, 10, replace = FALSE),
                    sample(10:90, 10, replace = FALSE),
                    sample(20:80, 10, replace = FALSE),
                    sample(30:70, 10, replace = FALSE),
                    sample(40:60, 10, replace = FALSE)))

  output$homoGamePlot1 <- renderPlot({
    par(mar = c(4, 4.5, 2, 2))
    stripchart(
      main = "Plot A",
      pch = 19,
      cex = 1.5,
      homoData1_2 ~ homoData1_1,
      vertical = TRUE,
      data = homoData1,
      xlab = 'Index',
      ylab = 'Data',
      cex.lab = 1.5,
      cex.axis = 1.5)
  })

  output$homoGamePlot2 <- renderPlot({
    par(mar = c(4, 4.5, 2, 2))
    stripchart(
      main = "Plot B",
      pch = 19,
      cex = 1.5,
      homoData2_2 ~ homoData2_1,
      vertical = TRUE,
      data = homoData2,
      xlab = 'Index',
      ylab = 'Data',
      cex.lab = 1.5,
      cex.axis = 1.5)
  })

  output$homoGamePlot3 <- renderPlot({
    par(mar = c(4, 4.5, 2, 2))
    stripchart(
      main = "Plot C",
      pch = 19,
      cex = 1.5,
      homoData3_2 ~ homoData3_1,
      vertical = TRUE,
      data = homoData3,
      xlab = 'Index',
      ylab = 'Data',
      cex.lab = 1.5,
      cex.axis = 1.5)
  })

  observeEvent(
    eventExpr = input$submitHomo,
    handlerExpr = {
      output$markHomo <- renderIcon(
        icon = ifelse(
          test = input$homoSelected == 'Plot C',
          yes = "correct",
          no = "incorrect"
        ),
        width = 48
      )
      
      choiceGrader(
        session,
        inputName = "homoSelected",
        description = "violation of homoscedasticity",
        userResponse = input$homoSelected,
        ans = "Plot C"
      )
    })

  observeEvent(
    eventExpr = input$homoSelected,
    handlerExpr = {
      output$markHomo <- renderIcon()
    }
  )

  ## MC Independence of Observations ----
  indeData1 <- rnorm(n = 50, mean = 0, sd = 1)
  indeData2 <- rnorm(n = 50, mean = 0, sd = 1)
  indeData3 <- ts(1:10, frequency = 4, start = c(1959, 2))

  output$indeGamePlot1 <- renderPlot({
    par(mar = c(4, 4.5, 2, 2))
    plot(
      indeData1,
      type = "b",
      main = "Plot C",
      pch = 19,
      cex = 1.5,
      ylab = "Data",
      xlab = "Row index",
      cex.lab = 1.5,
      cex.axis = 1.5
    )
  })

  output$indeGamePlot2 <- renderPlot({
    par(mar = c(4, 4.5, 2, 2))
    plot(
      indeData2,
      type = "b",
      main = "Plot B",
      pch = 19,
      cex = 1.5,
      ylab = "Data",
      xlab = "Row index",
      cex.lab = 1.5,
      cex.axis = 1.5
    )
  })

  output$indeGamePlot3 <- renderPlot({
    par(cex.lab = 1.5, cex.axis = 1.5, mar = c(4, 4.5, 2, 2))
    plot(
      indeData3,
      type = "b",
      main = "Plot A",
      pch = 19,
      cex = 1.5,
      ylab = "Data",
      xlab = "Row index"
    )
  })

  observeEvent(
    eventExpr = input$submitInde,
    handlerExpr = {
      output$markInde <- renderIcon(
        icon = ifelse(
          test = input$indeSelected == 'Plot A',
          yes = "correct",
          no = "incorrect"
        ),
        width = 48
      )
      
      choiceGrader(
        session,
        inputName = "indeSelected",
        description = "lack of independence of observations",
        userResponse = input$indeSelected,
        ans = "Plot A"
      )
    })

  observeEvent(
    eventExpr = input$indeSelected,
    handlerExpr = {
      output$markInde <- renderIcon()
    }
  )

  ### MC Linear Relationship ----
  linearData1 <- data.frame(
    linearData1_1 = rep(LETTERS[1:5], 10),
    linearData1_2 = c(sample(1:20, 10, replace = FALSE),
                      sample(10:30, 10, replace = FALSE),
                      sample(15:35, 10, replace = FALSE),
                      sample(20:40, 10, replace = FALSE),
                      sample(30:50, 10, replace = FALSE)),
    linearData1_3 = c(sample(1:20, 10, replace = FALSE),
                      sample(10:30, 10, replace = FALSE),
                      sample(15:35, 10, replace = FALSE),
                      sample(20:40, 10, replace = FALSE),
                      sample(30:50, 10, replace = FALSE))
  )
  linearData2 <- data.frame(
    linearData2_1 = rep(LETTERS[1:5], 10),
    linearData2_2 = c(sample(50:30, 10, replace = FALSE),
                      sample(40:20, 10, replace = FALSE),
                      sample(35:15, 10, replace = FALSE),
                      sample(30:10, 10, replace = FALSE),
                      sample(20:1, 10, replace = FALSE)),
    linearData2_3 = c(sample(1:20, 10, replace = FALSE),
                      sample(10:30, 10, replace = FALSE),
                      sample(15:35, 10, replace = FALSE),
                      sample(20:40, 10, replace = FALSE),
                      sample(30:50, 10, replace = FALSE))
  )
  linearData3 <- data.frame(
    linearData3_1 = c(rep("A", 10), rep("B", 10), rep("C", 10), rep("D", 10), rep("E", 10)),
    linearData3_2 = sample(1:100, 50, replace = FALSE),
    linearData3_3 = sample(1:100, 50, replace = FALSE)
  )

  output$linearGamePlot1 <- renderPlot({
    ggplot(
      data = linearData1,
      mapping = ggplot2::aes(
        y = linearData1_2,
        x = linearData1_3,
        group = linearData1_1,
        color = linearData1_1
      )) +
      geom_point(size = 3) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = "bottom",
        plot.title = element_text(size = 24)
      ) +
      xlab("X") +
      ylab("Y") +
      labs(
        title = "Plot C",
        color = "Type"
      )
  })

  output$linearGamePlot2 <- renderPlot({
    ggplot(
      data = linearData2,
      mapping = ggplot2::aes(
        y = linearData2_2,
        x = linearData2_3,
        group = linearData2_1,
        color = linearData2_1
      )) +
      geom_point(size = 3) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = "bottom",
        plot.title = element_text(size = 24)
      ) +
      xlab("X") +
      ylab("Y") +
      labs(
        title = "Plot B",
        color = "Type"
      )
  })

  output$linearGamePlot3 <- renderPlot({
    ggplot(
      data = linearData3,
      mapping = ggplot2::aes(
        y = linearData3_2,
        x = linearData3_3,
        group = linearData3_1,
        color = linearData3_1
      )) +
      geom_point(size = 3) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = "bottom",
        plot.title = element_text(size = 24)
      ) +
      xlab("X") +
      ylab("Y") +
      labs(
        title = "Plot A",
        color = "Type"
      )
  })

  observeEvent(
    eventExpr = input$submitLinear,
    handlerExpr = {
      output$markLinear <- renderIcon(
        icon = ifelse(
          test = input$linearSelected == 'Plot A',
          yes = "correct",
          no = "incorrect"
        ),
        width = 48
      )
      
      choiceGrader(
        session,
        inputName = "linearSelected",
        description = "non-linear relationship between the response and the covariate",
        userResponse = input$linearSelected,
        ans = "Plot A"
      )
    })

  observeEvent(
    eventExpr = input$normalitySelected,
    handlerExpr = {
      output$markNormality <- renderIcon()
    }
  )

  ### MC Same Slope ----
  slopeData1 <- data.frame(
    slopeData1_1 = rep(LETTERS[1:5], 10),
    slopeData1_2 = c(sample(1:20, 10, replace = FALSE),
                     sample(10:30, 10, replace = FALSE),
                     sample(15:35, 10, replace = FALSE),
                     sample(20:40, 10, replace = FALSE),
                     sample(30:50, 10, replace = FALSE)),
    slopeData1_3 = c(sample(1:20, 10, replace = FALSE),
                     sample(10:30, 10, replace = FALSE),
                     sample(15:35, 10, replace = FALSE),
                     sample(20:40, 10, replace = FALSE),
                     sample(30:50, 10, replace = FALSE))
  )
  slopeData2 <- data.frame(
    slopeData2_1 = rep(LETTERS[1:5], 10),
    slopeData2_2 = c(sample(50:30, 10, replace = FALSE),
                     sample(40:20, 10, replace = FALSE),
                     sample(35:15, 10, replace = FALSE),
                     sample(30:10, 10, replace = FALSE),
                     sample(20:1, 10, replace = FALSE)),
    slopeData2_3 = c(sample(1:20, 10, replace = FALSE),
                     sample(10:30, 10, replace = FALSE),
                     sample(15:35, 10, replace = FALSE),
                     sample(20:40, 10, replace = FALSE),
                     sample(30:50, 10, replace = FALSE))
  )
  slopeData3 <- data.frame(
    slopeData3_1 = c(rep("A", 10), rep("B", 10), rep("C", 10), rep("D", 10), rep("E", 10)),
    slopeData3_2 = sample(1:100, 50, replace = FALSE),
    slopeData3_3 = sample(1:100, 50, replace = FALSE)
  )

  output$slopeGamePlot1 <- renderPlot({
    ggplot(
      data = slopeData1,
      mapping = ggplot2::aes(
        y = slopeData1_2,
        x = slopeData1_3,
        group = slopeData1_1,
        color = slopeData1_1
      )) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = "bottom",
        plot.title = element_text(size = 24)
      ) +
      xlab("X") +
      ylab("Y") +
      labs(
        title = "Plot A",
        color = "Type")
  })

  output$slopeGamePlot2 <- renderPlot({
    ggplot(
      data = slopeData2,
      mapping = ggplot2::aes(
        y = slopeData2_2,
        x = slopeData2_3,
        group = slopeData2_1,
        color = slopeData2_1
      )) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = "bottom",
        plot.title = element_text(size = 24)
      ) +
      xlab("X") +
      ylab("Y") +
      labs(
        title = "Plot C",
        color = "Type"
      )
  })



  output$slopeGamePlot3 <- renderPlot({
    ggplot(
      data = slopeData3,
      mapping = ggplot2::aes(
        y = slopeData3_2,
        x = slopeData3_3,
        group = slopeData3_1,
        color = slopeData3_1
      )) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
      ggplot2::theme_bw() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = "bottom",
        plot.title = element_text(size = 24)
      ) +
      xlab("X") +
      ylab("Y") +
      labs(
        title = "Plot B",
        color = "Type"
      )
  })

  observeEvent(
    eventExpr = input$submitSlope,
    handlerExpr = {
      output$markSlope <- renderIcon(
        icon = ifelse(
          test = input$slopeSelected == 'Plot B',
          yes = "correct",
          no = "incorrect"
        ),
        width = 48
      )
      
      choiceGrader(
        session,
        inputName = "slopeSelected",
        description = "violation of parallel slopes",
        userResponse = input$slopeSelected,
        ans = "Plot B"
      )
    })

  observeEvent(
    eventExpr = input$slopeSelected,
    handlerExpr = {
      output$markSlope <- renderIcon()
    }
  )

  ### MC Outliers ----
  outData1 <- data.frame(
    outData1_1 = rep(LETTERS[1:5], 10),
    outData1_2 = c(sample(1:15, 10, replace = FALSE),
                   sample(10:25, 10, replace = FALSE),
                   sample(20:35, 10, replace = FALSE),
                   sample(30:45, 10, replace = FALSE),
                   sample(40:55, 10, replace = FALSE)),
    outData1_3 = c(sample(1:15, 10, replace = FALSE),
                   sample(10:25, 10, replace = FALSE),
                   sample(20:35, 10, replace = FALSE),
                   sample(30:45, 10, replace = FALSE),
                   sample(40:55, 10, replace = FALSE))
  )
  outData2 <- data.frame(
    outData2_1 = rep(LETTERS[1:5], 10),
    outData2_2 = c(sample(1:15, 10, replace = FALSE),
                   sample(10:25, 10, replace = FALSE),
                   sample(20:35, 10, replace = FALSE),
                   sample(30:45, 10, replace = FALSE),
                   sample(40:55, 10, replace = FALSE)),
    outData2_3 = c(sample(45:60, 10, replace = FALSE),
                   sample(35:50, 10, replace = FALSE),
                   sample(25:40, 10, replace = FALSE),
                   sample(35:50, 10, replace = FALSE),
                   sample(45:60, 10, replace = FALSE))
  )
  outData3 <- data.frame(
    outData3_1 = c(rep("A", 10), rep("B", 10), rep("C", 10), rep("D", 10), rep("E", 10)),
    outData3_2 = c(rnorm(46, mean = 70, sd = 15),
                   sample(130:190, 2, replace = FALSE),
                   sample(0:15, 2, replace = FALSE)),
    outData3_3 = sample(1:100, 50, replace = FALSE)
  )

  output$outGamePlot1 <- renderPlot({
    realOutData1 <- rstatix::mahalanobis_distance(outData1)
    realOutData1$is.outlier <- ifelse(
      test = realOutData1$mahal.dist >= qchisq(p = 0.995, df = 1),
      yes = TRUE,
      no = FALSE
    )
    realOutData1 <- cbind(realOutData1, factor = outData1$outData1_1)
    ggplot2::ggplot(
      data = realOutData1,
      mapping = ggplot2::aes(
        y = outData1_2,
        x = outData1_3,
        color = factor,
        # shape = is.outlier ## Uncomment for debugging
      )) +
      geom_point(size = 3) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(size = 24)
      ) +
      xlab("X") +
      ylab("Y") +
      labs(
        title = "Plot C",
        color = "Type",
        shape = "Potential Outlier"
      )
  })

  output$outGamePlot2 <- renderPlot({
    realOutData2 <- rstatix::mahalanobis_distance(outData2)
    realOutData2$is.outlier <- ifelse(
      test = realOutData2$mahal.dist >= qchisq(p = 0.99, df = 1),
      yes = TRUE,
      no = FALSE
    )
    realOutData2 <- cbind(realOutData2, factor = outData2$outData2_1)
    ggplot2::ggplot(
      data = realOutData2,
      mapping = ggplot2::aes(
        y = outData2_2,
        x = outData2_3,
        color = factor,
        # shape = is.outlier ## Uncomment for debugging
      )) +
      geom_point(size = 3) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(size = 24)
      ) +
      xlab("X") +
      ylab("Y") +
      labs(
        title = "Plot B",
        color = "Type",
        shape = "Potential Outlier"
      )
  })

  output$outGamePlot3 <- renderPlot({
    realOutData3 <- rstatix::mahalanobis_distance(outData3)
    realOutData3$is.outlier <- ifelse(
      test = realOutData3$mahal.dist >= qchisq(p = 0.99, df = 1),
      yes = TRUE,
      no = FALSE
    )
    realOutData3 <- cbind(realOutData3, factor = outData3$outData3_1)
    ggplot(
      data = realOutData3,
      mapping = ggplot2::aes(
        y = outData3_2,
        x = outData3_3,
        color = factor,
        # shape = is.outlier ## Uncomment for debugging
      )) +
      geom_point(size = 3) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(size = 24)
      ) +
      xlab("X") +
      ylab("Y") +
      labs(
        title = "Plot A",
        color = "Type",
        shape = "Potential Outlier"
      )
  })

  observeEvent(
    eventExpr = input$submitOut,
    handlerExpr = {
      output$markOut <- renderIcon(
        icon = ifelse(
          test = input$outSelected == 'Plot A',
          yes = "correct",
          no = "incorrect"
        ),
        width = 48
      )
      
      choiceGrader(
        session,
        inputName = "outSelected",
        description = "violation of outliers",
        userResponse = input$outSelected,
        ans = "Plot A"
      )
    })

  observeEvent(
    eventExpr = input$outSelected,
    handlerExpr = {
      output$markOut <- renderIcon()
    }
  )

  ### MC Interaction ----
  interData1 <- data.frame(
    interData1_1 = rep(LETTERS[1:5], 10),
    interData1_2 = c(sample(1:10, 10, replace = FALSE),
                     sample(10:20, 10, replace = FALSE),
                     sample(20:30, 10, replace = FALSE),
                     sample(30:40, 10, replace = FALSE),
                     sample(40:50, 10, replace = FALSE)),
    interData1_3 = c(sample(1:10, 10, replace = FALSE),
                     sample(10:20, 10, replace = FALSE),
                     sample(20:30, 10, replace = FALSE),
                     sample(30:40, 10, replace = FALSE),
                     sample(40:50, 10, replace = FALSE))
  )
  interData2 <- data.frame(
    interData2_1 = rep(LETTERS[1:5], 10),
    interData2_2 = c(sample(1:10, 10, replace = FALSE),
                     sample(10:20, 10, replace = FALSE),
                     sample(20:30, 10, replace = FALSE),
                     sample(30:40, 10, replace = FALSE),
                     sample(40:50, 10, replace = FALSE)),
    interData2_3 = c(sample(1:10, 10, replace = FALSE),
                     sample(10:20, 10, replace = FALSE),
                     sample(20:30, 10, replace = FALSE),
                     sample(30:40, 10, replace = FALSE),
                     sample(40:50, 10, replace = FALSE))
  )
  interData3 <- data.frame(
    interData3_1 = c(rep("A", 10), rep("B", 10), rep("C", 10), rep("D", 10), rep("E", 10)),
    interData3_2 = sample(1:100, 50, replace = FALSE),
    interData3_3 = sample(1:100, 50, replace = FALSE)
  )

  output$interGamePlot1 <- renderPlot({
    ggplot(
      data = interData1,
      mapping = aes(
        x = interData1_2,
        y = interData1_3,
        color = interData1_1,
        group = interData1_1
      )) +
      geom_point(size = 3) +
      geom_line(size = 1) +
      ggplot2::theme_bw() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(size = 24)
      ) +
      xlab("X") +
      ylab("Y") +
      labs(
        title = "Plot A",
        color = "Group"
      )
  })

  output$interGamePlot2 <- renderPlot({
    ggplot(
      data = interData2,
      mapping = aes(
        x = interData2_2,
        y = interData2_3,
        color = interData2_1,
        group = interData2_1
      )) +
      geom_point(size = 3) +
      geom_line(size = 1) +
      ggplot2::theme_bw() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(size = 24)
      ) +
      xlab("X") +
      ylab("Y") +
      labs(
        title = "Plot C",
        color = "Group"
      )
  })

  output$interGamePlot3 <- renderPlot({
    ggplot(
      data = interData3,
      mapping = aes(
        x = interData3_2,
        y = interData3_3,
        color = interData3_1,
        group = interData3_1
      )) +
      geom_point(size = 3) +
      geom_line(size = 1) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(size = 24)
      ) +
      xlab("X") +
      ylab("Y") +
      labs(
        title = "Plot B",
        color = "Group"
      )
  })

  observeEvent(
    eventExpr = input$submitInter,
    handlerExpr = {
      output$markInter <- renderIcon(
        icon = ifelse(
          test = input$interSelected == 'Plot B',
          yes = "correct",
          no = "incorrect"
        ),
        width = 48
      )
      
      choiceGrader(
        session,
        inputName = "interSelected",
        description = "violation of block interaction",
        userResponse = input$interSelected,
        ans = "Plot B"
      )
    })

  observeEvent(
    eventExpr = input$interSelected,
    handlerExpr = {
      output$markInter <- renderIcon()
    }
  )
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
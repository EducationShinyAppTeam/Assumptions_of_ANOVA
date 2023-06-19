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
ansOptionAnovaAssumption <- list("Normality of Residuals",
                                 "Homoscedasticity",
                                 "Independence of Observation")
ansOptionAncovaAssumption <- list("Normality of Residuals", 
                                  "Homoscedasticity",
                                  "Independence of Observation",
                                  "Linear Relationship covariate and the Response",
                                  "Equality of the covariate's Slope parameter",
                                  "No Statistically Significant Potential Outliers")
ansOptionBlockingAssumption <- list("Normality of Residuals",
                                    "Homoscedasticity" ,
                                    "Independence of Observation",
                                    "Interaction of Block and Treatment")
ansOptionRandomAssumption <- list("Normality of Residuals",
                                  "Homoscedasticity",
                                  "Independence of Observation",
                                  "Random Effects")
ansOptionRepeatAssumption <- list("Normality of Residuals",
                                  "Homoscedasticity",
                                  "Independence of Observation",
                                  "Interaction of Block and Treatment",
                                  "Random Effects")
ansOptionPlot <- list("plot A", "plot B", "plot C")
honey1 <- data.frame(
  Surplus = c(100, 60, 90, 85, 90, 95, 105, 70, 80),
  Varietal = c(rep("Clover", 3), rep("Orange Blossom", 3), rep("Alfalfa", 3))
)
honey2 <- data.frame(
  Surplus = c(50, 40, 55, 85, 80, 82, 105, 180, 192),
  Varietal = c(rep("Clover", 3), rep("Orange Blossom", 3), rep("Alfalfa", 3))
)
keyboarding1 <- data.frame(
  kbd.type = c(rep("1", 4), rep("2", 4), rep("3", 4)),
  hrs.kbd = c(60, 72, 61, 50, 54, 68, 66, 59, 51, 56, 55, 56),
  hrs.pain = c(85, 95, 69, 58, 41, 74, 71, 52, 34, 40, 41, 40),
  hrs.pain1 = c(85, 95, 69, 58, 41, 74, 71, 52, 34, 68, 41, 56)
)
keyboarding2 <- data.frame(
  kbd.type = c(rep("1", 4), rep("2", 4), rep("3", 4)),
  hrs.kbd = c(60, 72, 61, 50, 54, 68, 66, 59, 56, 56, 55, 29),
  hrs.pain = c(190, 200, 69, 58, 41, 54, 61, 52, 4, 2, 5, 120)
)
apex1 <- data.frame(
  officer = sort(c(rep(LETTERS[1:5], 4))),
  score = c(
    76, 65, 85, 74,
    59, 75, 81, 67,
    49, 63, 61, 46,
    74, 71, 85, 89,
    66, 84, 80, 79
  ),
  score1 = c(
    76, 65, 85, 74,
    59, 75, 81, 67,
    49, 63, 72, 46,
    74, 65, 85, 80,
    66, 84, 80, 79
  )
)
apex2 <- data.frame(
  officer = sort(c(rep(LETTERS[1:5], 4))),
  score = c(
    76, 65, 85, 74,
    5, 75, 81, 67,
    4, 63, 61, 46,
    74, 71, 85, 189,
    66, 84, 80, 79
  )
)
beer1 <- data.frame(
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
beer2 <- data.frame(
  judge = sort(rep(LETTERS[1:6],4)),
  beer = rep(c("Barnstormer", "King Richard Red",
               "Craftsman", "Red Mo"), 6),
  score = c(50, 60, 70, 70,
            300, 2, 190, 6,
            45, 48, 60, 58,
            65, 65, 75, 75,
            55, 60, 70, 65,
            48, 53, 68, 63)
)

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "black",
    ### Create the app header ----
    dashboardHeader(
      title = "Assumptions of ANOVA",
      titleWidth = 250,
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
        tags$a(href='https://shinyapps.science.psu.edu/',
               icon("home")))
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("Model Assumptions", tabName = "assumptiongame", icon = icon("gamepad")),
        menuItem("Assumption Checking", tabName = "checkinggame", icon = icon("gamepad")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Assumptions of ANOVA Models"),
          p("This app introduces the assumptions for different ANOVA models and 
            how to test those assumptions."),
          p("You can also learn what will happen if assumptions are invalid."),
          h2("Instructions"),
          tags$ol(
            tags$li("Click the go button to enter the prerequisites page."),
            tags$li("In the explore section, view and compare graphics illustrating 
                    situations that are valid and invalid under different models."),
            tags$li("Test yourself on which assumptions apply to which model in 
                    the drag and drop game."),
            tags$li("Test yourself on when plots show a violation of assumptions 
                    in the multiple choice game.")
          ),
          ##### Go Button ----
          div(style = "text-align: center;",
              bsButton(
                inputId = "explore",
                label = "GO!",
                icon = icon("bolt"),
                size = "large")),
          ##### Create two lines of space ----
          br(),
          br(),
          h2("Acknowledgements"),
          p("This version of the app was developed and coded by Gonghao Liu, Neil J.
            Hatfield, Robert P. Carey, III, and Phichchaya Sutaporn.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 06/19/2023 by LJE.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          h2("Prerequisites"),
          box(
            title = strong("What is ANOVA?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "In general, ANOVA refers to a family of statistical techniques that 
            assess potential differences in a response (i.e., scale-level 
            dependent variable) given one or more factors (i.e., nominal-level 
            independent variables with 2+ categories). Each specific model in the 
            ANOVA family has their own assumptions.",
            br(),
            tags$ol(
              tags$li("Oneway ANOVA: a technique that is used to compare the means 
                      of three or more groups or categories."),
              tags$li("ANCOVA: a technique that combines elements of both ANOVA 
                      and regression analysis."),
              tags$li("Blocking: a technique that is used in experimental design 
                      to reduce the variability caused by nuisance factors or sources 
                      of variation that are not of primary interest. "),
              tags$li("Random Effects: a technique that represents unobserved or 
                      latent factors that are assumed to have a random variation."),
              tags$li("Repeated Measure: a technique that compares means across 
                      one or more variables that are based on repeated observations.")
          )
          ),
          box(
            title = strong("Why is assumption testing important to ANOVA?"),
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
            title = strong("What if the assumptions aren't met?"),
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
        #### Set up an Explore Page ----
        tabItem(
          tabName = "explore",
          h2("Explore"),
          fluidPage(
            tabsetPanel(
              ##### Set up ANOVA page ----
              tabPanel(
                title = "Oneway ANOVA",
                br(),
                h3("Scenario"),
                p("An experiment was conducted to determine the relationship of 
                  honey output and the types of flowers,there were 3 types of 
                  flowers, 3 beehives were randomly assigned to each type of 
                  flowers with 9 beehives in total."),
                selectInput(
                  "anovaSelect",
                  label = "Select the assumption you want to test",
                  choices = ansOptionAnovaAssumption,
                  width = NULL
                ),
                h4("Valid Example"),
                textOutput("anovaTextValid"),
                plotOutput("anovaImageValid"),
                h4("Invalid Example"),
                textOutput("anovaTextInValid"),
                plotOutput("anovaImageInValid")
              ),
              ##### Set up ANCOVA page ----
              tabPanel(
                "ANCOVA",
                br(),
                h3("Scenario"),
                p("We are wanting to understand the impact of the type of keyboard 
                  on how many hours of pain a person experiences in their hands, 
                  wrists, and forearms."),
                p("We suspect that the number of hours a person spends keyboarding 
                  is related to the number of hours of pain that they feel."),
                p("We have 12 volunteers who will use a specific keyboard we 
                  assign them for 2 weeks. During that time, they will record the 
                  number of hours they use the keyboard and the number of hours of 
                  repetitive motion pain during the study period."),
                selectInput(
                  "ancovaSelect",
                  label = "Select the assumption you want to test",
                  choices = ansOptionAncovaAssumption,
                  width = '30%'
                ),
                h4("Valid Example"),
                textOutput("ancovaTextValid"),
                plotOutput("ancovaImageValid"),
                h4("Invalid Example"),
                textOutput("ancovaTextInValid"),
                plotOutput("ancovaImageInValid")
              ),
              ##### Set up blocking page ----
              tabPanel(
                "Blocking",
                br(),
                h3("Scenario"),
                tags$ul(
                  tags$li("A farmer wants to test out four varieties of barley and see 
                  if there is any difference in yield."),
                  tags$li("He has four fields in which he can plant the barley. However, 
                          the farmer is aware of differences between each field. For example,"),
                  tags$li("One field has a higher clay content in the soil than 
                          the others"),
                  tags$li("One field has rockier soil than the others"),
                  tags$li("Two fields are in wetter climates; two are in drier 
                          climates"),
                  tags$li("One field very loose soil while another field has much 
                          more compacted soil"),
                  tags$li("wo fields are relatively flat, one has a hill in the 
                          middle, and the last has a valley.")
                ),
                selectInput(
                  "blockingSelect",
                  label = "Select the assumption you want to test",
                  choices = ansOptionBlockingAssumption,
                  width = NULL
                ),
                h4("Valid Example"),
                textOutput("blockingTextValid"),
                plotOutput("blockingImageValid"),
                h4("Invalid Example"),
                textOutput("blockingTextInvalid"),
                plotOutput("blockingImageInvalid")
              ),
              ##### Set up random effects page ----
              tabPanel(
                "Random Effects",
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
                  "randomEffectSelect",
                  label = "Select the assumption you want to test",
                  choices = ansOptionRandomAssumption,
                  width = NULL
                ),
                h4("Valid Example"),
                textOutput("randomEffectTextValid"),
                plotOutput("randomEffectImageValid"),
                h4("Invalid Example"),
                textOutput("randomEffectTextInvalid"),
                plotOutput("randomEffectImageInvalid")
              ),
              ##### Set up repeated measure page ----
              tabPanel(
                "Repeated Measure",
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
                p(tags$li("Barnstormer (IPA, Happy Valley Brewing Company)")),
                p(tags$li("Craftsman (Brown, Happy Valley Brewing Company)")),
                p(tags$li("Red Mo (Red, Otto's Pub and Brewery)")),
                p(tags$li("King Richard Red (Amber, Robin Hood Brewing Co.)")),
                selectInput(
                  "repeatedMeasureSelect",
                  p("Select the assumption you want to test"),
                  choices = ansOptionRepeatAssumption,
                  width = NULL
                ),
                h4("Valid Example"),
                textOutput("repeatedMeasureTextValid"),
                plotOutput("repeatedMeasureImageValid"),
                h4("Invalid Example"),
                textOutput("repeatedMeasureTextInvalid"),
                plotOutput("repeatedMeasureImageInvalid")
              )
            )
          )
        ),
        
        #### Set up a Model Assumptions game Page ----
        tabItem(
          tabName = "assumptiongame",
          withMathJax(),
          h2("Practice/Test Yourself with the drag and drop matching game"),
          ##### Set up ANOVA game1 ----
          tabsetPanel(
            tabPanel(
              "Oneway ANOVA",
              fluidRow(
                bucket_list(
                  header = "Pick the assumptions for ANOVA",
                  add_rank_list(
                    text = "Drag assumptions from here",
                    input_id = "dragAnova",
                    labels = c("Normality",
                               "Homoscedasticity",
                               "Independence of Observation",
                               "Linear relationship covariate and the response",
                               "Equality of the covariate's slope parameter",
                               "No statistically significiant potential outliers",
                               "Interaction of block and treatment",
                               "Random effects")
                  ),
                  add_rank_list(
                    text = "to here",
                    input_id = "dropAnova"
                  )
                )
              ),
              div(style = "text-align:left;",
                  fluidRow(
                    column(
                      6,
                      bsButton(
                        inputId = 'submitAnova',
                        label = "Submit",
                        size = "large",
                        style = "default",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markAnova'),
                      uiOutput('markAnova2')
                    )
                  )
                )
              ),
            ##### Set up ANCOVA game1 ----
            tabPanel(
              "ANCOVA",
              fluidRow(
                bucket_list(
                  header = "Pick the assumptions for ANCOVA",
                  add_rank_list(
                    text = "Drag assumptions from here",
                    input_id = "dragAncova",
                    labels = c("Normality",
                               "Homoscedasticity",
                               "Independence of Observation",
                               "Linear relationship covariate and the response",
                               "Equality of the covariate's slope parameter",
                               "No statistically significiant potential outliers",
                               "Interaction of block and treatment",
                               "Random effects")
                  ),
                  add_rank_list(
                    text = "to here",
                    input_id = "dropAncova"
                  )
                )
              ),
              div(style = "text-align:left;",
                  fluidRow(
                    column(
                      6,
                      bsButton(
                        inputId = 'submitAncova',
                        label = "Submit",
                        size = "large",
                        style = "default",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markAncova'),
                      uiOutput('markAncova2')
                    )
                  )
              )
            ),
            ##### Set up blocking game1 ----
            tabPanel(
              "Blocking",
              fluidRow(
                bucket_list(
                  header = "Pick the assumptions for blocking",
                  add_rank_list(
                    text = "Drag assumptions from here",
                    input_id = "dragBlocking",
                    labels = c("Normality",
                               "Homoscedasticity",
                               "Independence of Observation",
                               "Linear relationship covariate and the response",
                               "Equality of the covariate's slope parameter",
                               "No statistically significiant potential outliers",
                               "Interaction of block and treatment",
                               "Random effects")
                  ),
                  add_rank_list(
                    text = "to here",
                    input_id = "dropBlocking"
                  )
                )
              ),
              div(style = "text-align:left;",
                  fluidRow(
                    column(
                      6,
                      bsButton(
                        inputId = 'submitBlocking',
                        label = "Submit",
                        size = "large",
                        style = "default",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markBlocking'),
                      uiOutput('markBlocking2')
                    )
                  )
              )
            ),
            ##### Set up random effects game1 ----
            tabPanel(
              "Random Effects",
              fluidRow(
                bucket_list(
                  header = "Pick the assumptions for random effects",
                  add_rank_list(
                    text = "Drag assumptions from here",
                    input_id = "dragRandomEffect",
                    labels = c("Normality",
                               "Homoscedasticity",
                               "Independence of Observation",
                               "Linear relationship covariate and the response",
                               "Equality of the covariate's slope parameter",
                               "No statistically significiant potential outliers",
                               "Interaction of block and treatment",
                               "Random effects")
                  ),
                  add_rank_list(
                    text = "to here",
                    input_id = "dropRandomEffect"
                  )
                )
              ),
              div(style = "text-align:left;",
                  fluidRow(
                    column(
                      6,
                      bsButton(
                        inputId = 'submitRandomEffect',
                        label = "Submit",
                        size = "large",
                        style = "default",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markRandomEffect'),
                      uiOutput('markRandomEffect2')
                    )
                  )
              )
            ),
            ##### Set up repeated measure game1 ----
            tabPanel(
              "Repeated Measure",
              fluidRow(
                bucket_list(
                  header = "Pick the assumptions for repeated measure",
                  add_rank_list(
                    text = "Drag assumptions from here",
                    input_id = "dragRepeatedMeasure",
                    labels = c("Normality",
                               "Homoscedasticity",
                               "Independence of Observation",
                               "Linear relationship covariate and the response",
                               "Equality of the covariate's slope parameter",
                               "No statistically significiant potential outliers",
                               "Interaction of block and treatment",
                               "Random effects")
                  ),
                  add_rank_list(
                    text = "to here",
                    input_id = "dropRepeatedMeasure"
                  )
                )
              ),
              div(style = "text-align:left;",
                  fluidRow(
                    column(
                      6,
                      bsButton(
                        inputId = 'submitRepeatedMeasure',
                        label = "Submit",
                        size = "large",
                        style = "default",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markRepeatedMeasure'),
                      uiOutput('markRepeatedMeasure2')
                    )
                  )
              )
            )
          )
        ),
        #### Set up Game 2 Page ----
        tabItem(
          tabName = "checkinggame",
          withMathJax(),
          h2("Choose the plot which violates the assumption"),
          tabsetPanel(
            ##### Set up page for normality game2 ----
            tabPanel(
              "Normality",
              fluidRow(
                column(
                  4,
                  plotOutput('normalityGamePlot1')
                ),
                column(
                  4,
                  plotOutput('normalityGamePlot3')
                ),
                column(
                  4,
                  plotOutput('normalityGamePlot2')
                )),
              fluidRow(
                column(
                  12,
                  selectInput(
                    "nomalitySelected",
                    label = 'Your choice',
                    choices = ansOptionPlot
                  )
                )
              ),
              fluidRow(
                div(style = "text-align:middle;",
                    column(
                      6,
                      bsButton(
                        inputId = 'submitNormality',
                        label = "Submit",
                        size = "large",
                        style = "default",
                        disabled = FALSE)
                    )
                ),
                column(
                  6,
                  uiOutput('markNormality')
                )
              )
            ),
            ##### Set up page for homoscedasticity game2 ----
            tabPanel(
              "Homoscedasticity",
              fluidRow(
                column(
                  4,
                  plotOutput('homoGamePlot1')
                ),
                column(
                  4,
                  plotOutput('homoGamePlot2')
                ),
                column(
                  4,
                  plotOutput('homoGamePlot3')
                )
              ),
              fluidRow(
                column(
                  4,
                  selectInput(
                    "homoSelected",
                    label = 'Your choice',
                    choices = ansOptionPlot
                  )
                )
              ),
              fluidRow(
                div(style = "text-align:middle;",
                    column(
                      6,
                      bsButton(
                        inputId = 'submitHomo',
                        label = "Submit",
                        size = "large",
                        style = "default",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markHomo')
                    )
                )
              )
            ),
            ##### Set up page for independence game2 ----
            tabPanel(
              "Independence of observation",
              fluidRow(
                column(
                  4,
                  plotOutput('indeGamePlot3')
                ),
                column(
                  4,
                  plotOutput('indeGamePlot2')
                ),
                column(
                  4,
                  plotOutput('indeGamePlot1')
                )
              ),
              fluidRow(
                column(
                  4,
                  selectInput(
                    "indeSelected",
                    label = 'Your choice',
                    choices = ansOptionPlot
                  )
                )
              ),
              fluidRow(
                div(style = "text-align:middle;",
                    column(
                      6,
                      bsButton(
                        inputId = 'submitInde',
                        label = "Submit",
                        size = "large",
                        style = "default",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markInde')
                    )
                )
              )
            ),
            ##### Set up page for Linear game2 ----
            tabPanel(
              "Linear",
              fluidRow(
                column(
                  4,
                  plotOutput('linearGamePlot3')
                ),
                column(
                  4,
                  plotOutput('linearGamePlot2')
                ),
                column(
                  4,
                  plotOutput('linearGamePlot1')
                )
              ),
              fluidRow(
                column(
                  4,
                  selectInput(
                    "linearSelected",
                    label = 'Your choice',
                    choices = ansOptionPlot
                  )
                )
              ),
              fluidRow(
                div(style = "text-align:middle;",
                    column(
                      6,
                      bsButton(
                        inputId = 'submitLinear',
                        label = "Submit",
                        size = "large",
                        style = "default",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markLinear')
                    )
                )
              )
            ),
            ##### Set up page for Common slope game2 ----
            tabPanel(
              "Common slope",
              fluidRow(
                column(
                  4,
                  plotOutput('slopeGamePlot1')
                ),
                column(
                  4,
                  plotOutput('slopeGamePlot3')
                ),
                column(
                  4,
                  plotOutput('slopeGamePlot2')
                )
              ),
              fluidRow(
                column(
                  4,
                  tags$style(HTML(".radio-inline {margin-right: 472px;}")),
                  selectInput(
                    "slopeSelected",
                    label = 'Your choice',
                    choices = ansOptionPlot
                  )
                )
              ),
              fluidRow(
                div(style = "text-align:middle;",
                    column(
                      6,
                      bsButton(
                        inputId = 'submitSlope',
                        label = "Submit",
                        size = "large",
                        style = "default",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markSlope')
                    )
                )
              )
            ),
            ##### Set up page for potential outliers game2 ----
            tabPanel(
              "No potential outliers",
              fluidRow(
                column(
                  4,
                  plotOutput('outGamePlot3')
                ),
                column(
                  4,
                  plotOutput('outGamePlot2')
                ),
                column(
                  4,
                  plotOutput('outGamePlot1')
                )
              ),
              fluidRow(
                column(
                  4,
                  selectInput(
                    "outSelected",
                    label = 'Your choice',
                    choices = ansOptionPlot
                  )
                )
              ),
              fluidRow(
                div(style = "text-align:middle;",
                    column(
                      6,
                      bsButton(
                        inputId = 'submitOut',
                        label = "Submit",
                        size = "large",
                        style = "default",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markOut')
                    )
                )
              )
            ),
            ##### Set up page for interaction of block game2 ----
            tabPanel(
              "Interaction of block",
              fluidRow(
                column(
                  4,
                  plotOutput('interGamePlot1')
                ),
                column(
                  4,
                  plotOutput('interGamePlot3')
                ),
                column(
                  4,
                  plotOutput('interGamePlot2')
                )
              ),
              fluidRow(
                column(
                  4,
                  selectInput(
                    "interSelected",
                    label = 'Your choice',
                    choices = ansOptionPlot
                  )
                )
              ),
              fluidRow(
                div(style = "text-align:middle;",
                    column(
                      6,
                      bsButton(
                        inputId = 'submitInter',
                        label = "Submit",
                        size = "large",
                        style = "default",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markInter')
                    )
                  )
                )
              )
            )
          ),
        #### Set up the References Page-REQUIRED ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"), 
          p(class = "hangingindent",
            "Bates, D., Maechler, M., Bolker, B., Walker, S. (2015). Fitting
            Linear Mixed-Effects Models Using lme4. Journal of Statistical Software,
            67(1), 1-48. doi:10.18637/jss.v067.i01."),
          p(class = "hangingindent",
            "Carey, R. (2019). boastUtils: BOAST Utilities. (v0.1.0).
            [R Package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"),
          p(class = "hangingindent",
            "Chang, W. and Borges, R. B. (2018). shinydashboard: Create
            dashboards with 'Shiny'. (v0.7.1) [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"),
          p(class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019). shiny: Web application framework for R. (v1.4.0)
            [R Package]. Available from https://CRAN.R-project.org/package=shiny"),
          p(class = "hangingindent",
            "de Vries, A., Schloerke, B. and Russell, K. (2019).
            sortable: Drag-and-Drop in 'shiny' Apps with 'SortableJS'. R package
            version 0.4.2. Avaliable from: https://CRAN.R-project.org/package=sortable"),
          p(class = "hangingindent",
            "Fox, J. and Weisberg, S. (2019). An {R} Companion to Applied
            Regression, Third Edition. Thousand Oaks CA: Sage. Avaliable from:
            https://socialsciences.mcmaster.ca/jfox/Books/Companion/"),
          p(class = "hangingindent",
            "Hatfield, N. J. (2020), Stat 461: ANOVA Course Notes [course notes], Spring 2020."),
          p(class = "hangingindent",
            "Kassambara, A. (2020). rstatix: Pipe-Friendly Framework for Basic
            Statistical Tests. R package version 0.6.0.
            https://CRAN.R-project.org/package=rstatix"),
          p(class = "hangingindent",
            "Kutner, M. H., Nachtsheim, C. J., Neter, J., and Li, W. (2005),
            Applied Linear Statistical Models [apex enterprises data set],
            New York: McGraw-Hill Irwin"),
          p(class = "hangingindent",
            "Oehlert, G. W. (2000), A First Course in Design and Analysis of Experiments
            [keyboarding data set], New York: W. H. Freeman."),
          p(class = "hangingindent",
            "Perrier, V., Meyer, F. and Granjon, D. (2020). shinyWidgets:
            Custom Inputs Widgets for Shiny. R package version 0.5.3. Avaliable from:
            https://CRAN.R-project.org/package=shinyWidgets"),
          p(class = "hangingindent",
            "Wickham, W. (2016). ggplot2: Elegant graphics for data analysis.
            [R Package]. Springer-Verlag New York. Available from
            https://ggplot2.tidyverse.org") ,
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
  ## Set Up "Explore" Button
  observeEvent(input$explore, {
    updateTabItems(session, "pages", "explore")
  })

  output$anovaTextValid <- renderText({
    if (input$anovaSelect == "Normality of Residuals"){
      paste("In this plot, the boundary line should envelop almost all the points 
            in the graph.")
    }
    else if (input$anovaSelect == "Homoscedasticity"){
      paste("The points here should have similar variability for each x value.")
    }
    else if (input$anovaSelect == "Independence of Observation"){
      paste("The points in this graph should have no pattern.")
    }
  })
  ### Explore ANOVA ----
  observeEvent(
    eventExpr = input$anovaSelect,
    handlerExpr = {
      output$anovaImageValid <- renderPlot(
        expr = {
    if (input$anovaSelect == "Normality of Residuals"){
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = honey1$Surplus,
        distribution = "norm",
        envelope = 0.97,
        ylab = "Surplus Honey (lbs)"
      )
    }
    else if (input$anovaSelect == "Homoscedasticity"){
      stripchart(
        pch = 19,
        cex = 1.5,
        Surplus ~ Varietal,
        vertical = TRUE,
        data = honey1,
        cex.lab = 1.5,
        cex.axis = 1.5
      )
    }
    else if (input$anovaSelect == "Independence of Observation"){
      plot(
        honey1$Surplus, 
        type = "b", 
        ylab = "Surplus Honey (lbs)",
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
  },
  alt = if (input$anovaSelect == "Normality of Residuals"){
    "For normality, this is a plot of norm quantile with all the points lay in the 
      97% confidence envelope."}
  else if (input$anovaSelect == "Homoscedasticity"){
    "For homoscedasticity, this is a plot of response versus explanatory,all the 
      points lay in a random position."}
  else if (input$anovaSelect == "Independence of Observation"){
    "For independence, this is a plot of response versus index, all the points 
      lay in a random position."})
})

  output$anovaTextInValid <- renderText({
    if (input$anovaSelect == "Normality of Residuals"){
      paste("In this plot, too many points are located outside of the envelop.")
    }
    else if (input$anovaSelect == "Homoscedasticity"){
      paste("The points in this graph tend to have different amounts of variation 
            for different x values.")
    }
    else if (input$anovaSelect == "Independence of Observation"){
      paste("The points in this graph tend to have a pattern.")
    }
  })
  
  observeEvent(
    eventExpr = input$anovaSelect,
    handlerExpr = {
      output$anovaImageInValid <- renderPlot(
        expr = {
    if (input$anovaSelect == "Normality of Residuals"){
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = honey2$Surplus,
        distribution = "norm",
        envelope = 0.97,
        ylab = "Surplus Honey (lbs)")
    }
    else if (input$anovaSelect == "Homoscedasticity"){
      stripchart(
        pch = 19,
        cex = 1.5,
        Surplus ~ Varietal,
        vertical = TRUE,
        data = honey2,
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
    else if (input$anovaSelect == "Independence of Observation"){
      plot(
        honey2$Surplus, 
        type = "b", 
        ylab = "Surplus Honey (lbs)",
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
  },
  alt = if (input$anovaSelect == "Normality of Residuals"){
    "For normality, this is a plot of norm quantile with 2 points lay out side of 
    the 97% confidence envelope."}
  else if (input$anovaSelect == "Homoscedasticity"){
    "For homoscedasticity, this is a plot of response versus explanatory,all the 
      points lay in a pattern."}
  else if (input$anovaSelect == "Independence of Observation"){
    "For independence, this is a plot of response versus index, all the points 
      lay in a pattern."
    })
})
  
  ### Explore ANCOVA ---- 
  output$ancovaTextValid <- renderText({
    if (input$ancovaSelect == "Normality of Residuals"){
      paste("In this plot, the boundary line should envelop almost all the points 
            in the graph.")
    }
    else if (input$ancovaSelect == "Homoscedasticity"){
      paste("The points here should have similar variability for each x value.")
    }
    else if (input$ancovaSelect == "Independence of Observation"){
      paste("The points in this graph should have no pattern.")
    }
    else if (input$ancovaSelect == "Linear Relationship covariate and the Response"){
      paste("By the graph, we expect to see a linear relationship between covariate 
            and response.")
    }
    else if (input$ancovaSelect == "Equality of the covariate's Slope parameter"){
      paste("The different covariate represent by different colors here should have 
            homogeneity slope.")
    }
    else if (input$ancovaSelect == "No Statistically Significant Potential Outliers"){
      paste("We expected no visualized outliers.")
    }
  })
  
  observeEvent(
    eventExpr = input$ancovaSelect,
    handlerExpr = {
      output$ancovaImageValid <- renderPlot(
        expr = {
    if (input$ancovaSelect == "Normality of Residuals"){
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = keyboarding1$hrs.pain,
        distribution = "norm",
        envelope = 0.97,
        ylab = "Hours of Pain"
      )
    }
    else if (input$ancovaSelect == "Homoscedasticity"){
      stripchart(
        pch = 19,
        cex = 1.5,
        hrs.pain1 ~ kbd.type,
        vertical = TRUE,
        data = keyboarding1,
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
    else if (input$ancovaSelect == "Independence of Observation"){
      plot(
        keyboarding1$hrs.pain, 
        type = "b", 
        ylab = "Hours of Pain",
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
    else if (input$ancovaSelect == "Linear Relationship covariate and the Response"){
      ggplot2::ggplot(data = keyboarding1,
                      mapping = ggplot2::aes(
                        y = hrs.pain,
                        x = hrs.kbd
                      )) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_smooth(method = "lm", se = FALSE) +
        ggplot2::theme_bw() +
        theme(axis.title = element_text(size = 18)) +
        xlab("Hours Spent Keyboarding") +
        ylab("Hours of Pain")
    }
    else if (input$ancovaSelect == "Equality of the covariate's Slope parameter"){
      ggplot2::ggplot(data = keyboarding1,
                      mapping = ggplot2::aes(
                        y = hrs.pain,
                        x = hrs.kbd,
                        group = kbd.type,
                        color = kbd.type,
                        shape = kbd.type
                      )) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_smooth(method = "lm", se = FALSE) +
        ggplot2::theme_bw() +
        theme(axis.title = element_text(size = 18)) +
        xlab("Hours Spent Keyboarding") +
        ylab("Hours of Pain") +
        labs(color = "Keyboard Type") 
        
    }
    else if (input$ancovaSelect == "No Statistically Significant Potential Outliers"){
      key2 <- rstatix::mahalanobis_distance(keyboarding1)
      key2 <- cbind(key2, factor = keyboarding1$kbd.type)
      ggplot2::ggplot(data = key2,
                      mapping = ggplot2::aes(
                        y = hrs.pain,
                        x = hrs.kbd,
                        color = factor
                      )) +
        ggplot2::geom_point(size = 3) +
        ggplot2::theme_bw() +
        theme(axis.title = element_text(size = 18)) +
        xlab("Hours Spent Keyboarding") +
        ylab("Hours of Pain") +
        labs(color = "Keyboard", shape = "Potential Outlier") 
    }
  },
  alt = if (input$ancovaSelect == "Normality of Residuals"){
    "For normality, this is a plot of norm quantile with all the points lay in the 
    97% confidence envelope."}
  else if (input$ancovaSelect == "Homoscedasticity"){
    "For homoscedasticity, this is a plot of response versus explanatory, all the 
    points lay in a random position."}
  else if (input$ancovaSelect == "Independence of Observation"){
    "For independence, this is a plot of response versus index, all the points lay 
    in a random position."}
  else if (input$ancovaSelect == "Linear Relationship covariate and the Response"){
    "For linear, the plot shows that response and explantory have a linear relationship."}
  else if (input$ancovaSelect == "Equality of the covariate's Slope parameter"){
    "For slope, points in the different groups have a similar trend."}
  else if (input$ancovaSelect == "No Statistically Significant Potential Outliers"){
    "For outlier, the plot shows that the data don't have obvious outliers."}
    )
  })
  
  output$ancovaTextInValid <- renderText({
    if (input$ancovaSelect == "Normality of Residuals"){
      paste("In this plot, too many points are located outside of the envelop.")
    }
    else if (input$ancovaSelect == "Homoscedasticity"){
      paste("The points in this graph tend to have different amounts of variation 
            for different x values.")
    }
    else if (input$ancovaSelect == "Independence of Observation"){
      paste("The points in this graph tend to have a pattern.")
    }
    else if (input$ancovaSelect == "Linear Relationship covariate and the Response"){
      paste("There is no linear relationship between covariate and response in 
            the graph.")
    }
    else if (input$ancovaSelect == "Equality of the covariate's Slope parameter"){
      paste("The different covariate represent by different colors here have no 
            homogeneity slope.")
    }
    else if (input$ancovaSelect == "No Statistically Significant Potential Outliers"){
      paste("There are some visualized outliers in the plot.")
    }
  })

  observeEvent(
    eventExpr = input$ancovaSelect,
    handlerExpr = {
      output$ancovaImageInValid <- renderPlot(
        expr = {
    if (input$ancovaSelect == "Normality of Residuals"){
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = keyboarding2$hrs.pain,
        distribution = "norm",
        envelope = 0.97,
        ylab = "Hours of Pain"
      )
    }
    else if (input$ancovaSelect == "Homoscedasticity"){
      stripchart(
        pch = 19,
        cex = 1.5,
        hrs.pain ~ kbd.type,
        vertical = TRUE,
        data = keyboarding2,
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
    else if (input$ancovaSelect == "Independence of Observation"){
      plot(
        keyboarding2$hrs.pain, 
        type = "b", 
        ylab = "Hours of Pain",
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
    else if (input$ancovaSelect == "Linear Relationship covariate and the Response"){
      ggplot2::ggplot(data = keyboarding2,
                      mapping = ggplot2::aes(
                        y = hrs.pain,
                        x = hrs.kbd
                      )) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_smooth(method = "lm", se = FALSE) +
        ggplot2::theme_bw() +
        theme(axis.title = element_text(size = 18)) +
        xlab("Hours Spent Keyboarding") +
        ylab("Hours of Pain")
    }
    else if (input$ancovaSelect == "Equality of the covariate's Slope parameter"){
      ggplot2::ggplot(data = keyboarding2,
                      mapping = ggplot2::aes(
                        y = hrs.pain,
                        x = hrs.kbd,
                        group = kbd.type,
                        color = kbd.type,
                        shape = kbd.type
                      )) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_smooth(method = "lm", se = FALSE) +
        ggplot2::theme_bw() +
        theme(axis.title = element_text(size = 18)) +
        xlab("Hours Spent Keyboarding") +
        ylab("Hours of Pain") +
        labs(color = "Keyboard Type")
    }
    else if (input$ancovaSelect == "No Statistically Significant Potential Outliers"){
      key2 <- rstatix::mahalanobis_distance(keyboarding2)
      key2 <- cbind(key2, factor = keyboarding2$kbd.type)
      ggplot2::ggplot(data = key2,
                      mapping = ggplot2::aes(
                        y = hrs.pain,
                        x = hrs.kbd,
                        color = factor
                      )) +
        ggplot2::geom_point(size = 3) +
        ggplot2::theme_bw() +
        theme(axis.title = element_text(size = 18)) +
        xlab("Hours Spent Keyboarding") +
        ylab("Hours of Pain") +
        labs(color = "Keyboard", shape = "Potential Outlier")
    }
  },
  alt = if (input$ancovaSelect == "Normality of Residuals"){
    "For normality, this is a plot of norm quantile with 2 points lay in the 97% 
    confidence envelope."}
  else if (input$ancovaSelect == "Homoscedasticity"){
    "For homoscedasticity, this is a plot of response versus explanatory, all the 
    points lay in a pattern."}
  else if (input$ancovaSelect == "Independence of Observation"){
    "For independence, this is a plot of response versus index, all the points lay 
    in a pattern."}
  else if (input$ancovaSelect == "Linear Relationship covariate and the Response"){
    "For linear, the plot shows that response and explantory have no linear relationship."}
  else if (input$ancovaSelect == "Equality of the covariate's Slope parameter"){
    "For slope, points in the different groups have a different trend."}
  else if (input$ancovaSelect == "No Statistically Significant Potential Outliers"){
    "For outlier, the plot shows that the data have obvious outliers."}
    )
  })
  
  ### Explore Blocking ---- 
  output$blockingTextValid <- renderText({
    if (input$blockingSelect == "Normality of Residuals"){
      paste("In this plot, the boundary line should envelop almost all the points 
            in the graph.")
    }
    else if (input$blockingSelect == "Homoscedasticity"){
      paste("The points here should have similar variability for each x value.")
    }
    else if (input$blockingSelect == "Independence of Observation"){
      paste("The points in this graph should have no pattern.")
    }
    else if (input$blockingSelect == "Interaction of Block and Treatment"){
      paste("The data in different groups should have a similar pattern.")
    }
  })
  
  observeEvent(
    eventExpr = input$blockingSelect,
    handlerExpr = {
      output$blockingImageValid <- renderPlot(
        expr = {
    barleyModel <- aov(Yield ~ Treatment + Field, data = barley1)
    if (input$blockingSelect == "Normality of Residuals"){
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = barleyModel$residuals,
        distribution = "norm",
        envelope = 0.97,
        ylab = "Yield (bushels per arce)"
      )
    }
    else if (input$blockingSelect == "Homoscedasticity"){
      stripchart(
        pch = 19,
        cex = 1.5,
        Yield ~ Treatment,
        vertical = TRUE,
        data = barley2,
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
    else if (input$blockingSelect == "Independence of Observation"){
      plot(
        barley1$Yield, 
        type = "b", 
        ylab = "Yield (bushels per acre)",
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
    else if (input$blockingSelect == "Interaction of Block and Treatment"){
      ggplot2::ggplot(data = barley1,
                      mapping = aes(x = Treatment,
                                    y = Yield,
                                    color = Field,
                                    group = Field)) +
        ggplot2::geom_point(size=2) +
        ggplot2::geom_line(size=1) +
        ggplot2::theme_bw() +
        theme(axis.title = element_text(size = 18)) +
        xlab("Variety") +
        ylab("Yield (bushels per acre)") +
        labs(color = "Field")
    }
    },
    alt = if (input$blockingSelect == "Normality of Residuals"){
      "For normality, this is a plot of norm quantile with all the points lay in 
      the 97% confidence envelope."}
    else if (input$blockingSelect == "Homoscedasticity"){
      "For homoscedasticity, this is a plot of response versus explanatory, all 
      the points lay in a random position."}
    else if (input$blockingSelect == "Independence of Observation"){
      "For independence, this is a plot of response versus index, all the points 
      lay in a random position."}
    else if (input$blockingSelect == "Interaction of Block and Treatment"){
      "For interaction, points in the different block have a similar trend."}
    )
  })
  
  output$blockingTextInvalid <- renderText({
    if (input$blockingSelect == "Normality of Residuals"){
      paste("In this plot, too many points are located outside of the envelop.")
    }
    else if (input$blockingSelect == "Homoscedasticity"){
      paste("The points in this graph tend to have different amounts of variation 
            for different x values.")
    }
    else if (input$blockingSelect == "Independence of Observation"){
      paste("The points in this graph tend to have a pattern.")
    }
    else if (input$blockingSelect == "Interaction of Block and Treatment"){
      paste("The data in different groups here have different patterns.")
    }
  })
  
  observeEvent(
    eventExpr = input$blockingSelect,
    handlerExpr = {
      output$blockingImageInvalid <- renderPlot(
        expr = {
    barleyModel <- aov(Yield ~ Treatment + Field, data = barley2)
    if (input$blockingSelect == "Normality of Residuals"){
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = barleyModel$residuals,
        distribution = "norm",
        envelope = 0.5,
        ylab = "Yield (bushels per arce)"
      )
    }
    else if (input$blockingSelect == "Homoscedasticity"){
      stripchart(
        pch = 19,
        cex = 1.5,
        Yield ~ Treatment,
        vertical = TRUE,
        data = barley1,
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
    else if (input$blockingSelect == "Independence of Observation"){
      plot(
        barley2$Yield, 
        type = "b", 
        ylab = "Yield (bushels per acre)",
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
    else if (input$blockingSelect == "Interaction of Block and Treatment"){
      ggplot2::ggplot(data = barley3,
                      mapping = aes(x = Treatment,
                                    y = Yield,
                                    color = Field,
                                    group = Field)) +
        ggplot2::geom_point(size=2) +
        ggplot2::geom_line(size=1) +
        ggplot2::theme_bw() +
        theme(axis.title = element_text(size = 18)) +
        xlab("Variety") +
        ylab("Yield (bushels per acre)") +
        labs(color = "Field")
    }
  },
  alt = if (input$blockingSelect == "Normality of Residuals"){
    "For normality, this is a plot of norm quantile with 2 points lay in 
      the 97% confidence envelope."}
  else if (input$blockingSelect == "Homoscedasticity"){
    "For homoscedasticity, this is a plot of response versus explanatory, all 
      the points lay in a pattern."}
  else if (input$blockingSelect == "Independence of Observation"){
    "For independence, this is a plot of response versus index, all the points 
      lay in a pattern."}
  else if (input$blockingSelect == "Interaction of Block and Treatment"){
    "For interaction, points in the different block have a different trend."}
    )
  })      
  
  ### Explore Random Effect ---- 
  output$randomEffectTextValid <- renderText({
    if (input$randomEffectSelect == "Normality of Residuals"){
      paste("In this plot, the boundary line should envelop almost all the points 
            in the graph.")
    }
    else if (input$randomEffectSelect == "Homoscedasticity"){
      paste("The points here should have similar variability for each x value.")
    }
    else if (input$randomEffectSelect == "Independence of Observation"){
      paste("The points in this graph should have no pattern.")
    }
    else if (input$randomEffectSelect == "Random Effects"){
      paste("In this plot, the boundary line should envelop almost all the points 
            in the graph.")
    }
  })

  observeEvent(
    eventExpr = input$randomEffectSelect,
    handlerExpr = {
  output$randomEffectImageValid <- renderPlot(
    expr = {
    options("contrasts" = c("contr.sum","contr.poly"))
    apexFE <- aov(score ~ officer, data = apex1)
    apexRE <- lme4::lmer(
      score ~ (1|officer),
      data = apex1,
      REML = TRUE)
    if (input$randomEffectSelect == "Normality of Residuals"){
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
    }
    else if (input$randomEffectSelect == "Homoscedasticity"){
      stripchart(
        pch = 19,
        cex = 1.5,
        score1 ~ officer,
        vertical = TRUE,
        data = apex1,
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
    else if (input$randomEffectSelect == "Independence of Observation"){
      plot(
        apex1$score, 
        type = "b", 
        ylab = "Score of Applicant",
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
    else if (input$randomEffectSelect == "Random Effects"){
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
  },
  alt = if (input$randomEffectSelect == "Normality of Residuals"){
    "For normality, this is a plot of norm quantile with all the points lay in 
      the 97% confidence envelope."}
  else if (input$randomEffectSelect == "Homoscedasticity"){
    "For homoscedasticity, this is a plot of response versus explanatory, all 
      the points lay in a random position."}
  else if (input$randomEffectSelect == "Independence of Observation"){
    "For independence, this is a plot of response versus index, all the points 
      lay in a random position."}
  else if (input$randomEffectSelect == "Random Effects"){
    "For random, this is a plot of norm quantile with all the points lay in the 
    80% confidence envelope."})
})
  
  output$randomEffectTextInvalid <- renderText({
    if (input$randomEffectSelect == "Normality of Residuals"){
      paste("In this plot, too many points are located outside of the envelop.")
    }
    else if (input$randomEffectSelect == "Homoscedasticity"){
      paste("The points in this graph tend to have different amounts of variation 
            for different x values.")
    }
    else if (input$randomEffectSelect == "Independence of Observation"){
      paste("The points in this graph tend to have a pattern.")
    }
    else if (input$randomEffectSelect == "Random Effects"){
      paste("In this plot, too many points are located outside of the envelop.")
    }
  })
  
  observeEvent(
    eventExpr = input$randomEffectSelect,
    handlerExpr = {
      output$randomEffectImageInvalid <- renderPlot(
        expr = {
    options("contrasts" = c("contr.sum","contr.poly"))
    apexFE <- aov(score ~ officer, data = apex2)
    apexRE <- lme4::lmer(
      score ~ (1|officer),
      data = apex2,
      REML = TRUE)
    if (input$randomEffectSelect == "Normality of Residuals"){
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
    }
    else if (input$randomEffectSelect == "Homoscedasticity"){
      stripchart(
        pch = 19,
        cex = 1.5,
        score ~ officer,
        vertical = TRUE,
        data = apex2,
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
    else if (input$randomEffectSelect == "Independence of Observation"){
      plot(
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        apex2$score, 
        type = "b", 
        ylab = "Score of Applicant",
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
    else if (input$randomEffectSelect == "Random Effects"){
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
  },
  alt = if (input$randomEffectSelect == "Normality of Residuals"){
    "For normality, this is a plot of norm quantile with 2 points lay in 
      the 97% confidence envelope."}
  else if (input$randomEffectSelect == "Homoscedasticity"){
    "For homoscedasticity, this is a plot of response versus explanatory, all 
      the points lay in a pattern."}
  else if (input$randomEffectSelect == "Independence of Observation"){
    "For independence, this is a plot of response versus index, all the points 
      lay in a pattern."}
  else if (input$randomEffectSelect == "Random Effects"){
    "For random, this is a plot of norm quantile with 1 point lay in the 
    80% confidence envelope."})
})
       
  ### Explore Repeated Measure ----
  output$repeatedMeasureTextValid <- renderText({
    if (input$repeatedMeasureSelect == "Normality of Residuals"){
      paste("In this plot, the boundary line should envelop almost all the points 
            in the graph.")
    }
    else if (input$repeatedMeasureSelect == "Homoscedasticity"){
      paste("The points here should have similar variability for each x value.")
    }
    else if (input$repeatedMeasureSelect == "Independence of Observation"){
      paste("The points in this graph should have no pattern.")
    }
    else if (input$repeatedMeasureSelect == "Interaction of Block and Treatment"){
      paste("The data in different group should have same pattern.")
    }
    else if (input$randomEffectSelect == "Random Effects"){
      paste("In this plot, the boundary line should envelop almost all the points 
            in the graph.")
    }
  })
  
  observeEvent(
    eventExpr = input$repeatedMeasureSelect,
    handlerExpr = {
      output$repeatedMeasureImageValid <- renderPlot(
        expr = {
    beerM1 <- lme4::lmer(score ~ beer + (1|judge), data = beer1)
    if (input$repeatedMeasureSelect == "Normality of Residuals"){
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = residuals(beerM1),
        distribution = "norm",
        envelope = 0.9,
        ylab = "Score"
      )
    }
    else if (input$repeatedMeasureSelect == "Homoscedasticity"){
      stripchart(
        pch = 19,
        cex = 1.5,
        score ~ beer,
        vertical = TRUE,
        data = beer1,
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
    else if (input$repeatedMeasureSelect == "Independence of Observation"){
      plot(
        beer1$score, 
        type = "b", 
        ylab = "Score",
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
    else if (input$repeatedMeasureSelect == "Interaction of Block and Treatment"){
      ggplot2::ggplot(data = beer1,
                      mapping = aes(x = beer,
                                    y = score,
                                    color = judge,
                                    group = judge)) +
        ggplot2::geom_point(size=2) +
        ggplot2::geom_line(size=1) +
        ggplot2::theme_bw() +
        theme(axis.title = element_text(size = 18)) +
        viridis::scale_color_viridis(discrete = TRUE, option = "viridis") +
        xlab("Beer") +
        ylab("Score") +
        labs(color = "Judge")
    }
    else if (input$repeatedMeasureSelect == "Random Effects"){
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
  },
  alt = if (input$repeatedMeasureSelect == "Normality of Residuals"){
    "For normality, this is a plot of norm quantile with all the points lay in 
      the 97% confidence envelope."}
  else if (input$repeatedMeasureSelect == "Homoscedasticity"){
    "For homoscedasticity, this is a plot of response versus explanatory, all 
      the points lay in a random position"}
  else if (input$repeatedMeasureSelect == "Independence of Observation"){
    "For independence, this is a plot of response versus index, all the points 
      lay in a random position"}
  else if (input$repeatedMeasureSelect == "Interaction of Block and Treatment"){
    "For interaction, points in the different block have a similar trend."}
  else if (input$repeatedMeasureSelect == "Random Effects"){
    "For random, this is a plot of norm quantile with all the points lay in the 
    80% confidence envelope."})
})
  
  output$repeatedMeasureTextInvalid <- renderText({
    if (input$repeatedMeasureSelect == "Normality of Residuals"){
      paste("In this plot, too many points are located outside of the envelop.")
    }
    else if (input$repeatedMeasureSelect == "Homoscedasticity"){
      paste("The points in this graph tend to have different amounts of variation 
            for different x values.")
    }
    else if (input$repeatedMeasureSelect == "Independence of Observation"){
      paste("The points in this graph tend to have a pattern.")
    }
    else if (input$repeatedMeasureSelect == "Random Effects"){
      paste("In this plot, too many points are located outside of the envelop.")
    }
    else if (input$repeatedMeasureSelect == "Interaction of Block and Treatment"){
      paste("The data in different groups here have different patterns.")
    }
  })
  
  
  observeEvent(
    eventExpr = input$repeatedMeasureSelect,
    handlerExpr = {
      output$repeatedMeasureImageInvalid <- renderPlot(
        expr = {
    beerM1 <- lme4::lmer(score ~ beer + (1|judge), data = beer2)
    if (input$repeatedMeasureSelect == "Normality of Residuals"){
      car::qqPlot(
        pch = 19,
        cex = 1.5,
        id = FALSE,
        x = residuals(beerM1),
        distribution = "norm",
        envelope = 0.9,
        ylab = "Score"
      )
    }
    else if (input$repeatedMeasureSelect == "Homoscedasticity"){
      stripchart(
        pch = 19,
        cex = 1.5,
        score ~ beer,
        vertical = TRUE,
        data = beer2,
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
    else if (input$repeatedMeasureSelect == "Independence of Observation"){
      plot(
        beer2$score, 
        type = "b", 
        ylab = "Score",
        pch = 19,
        cex = 1.5,
        xlab = "Row index",
        cex.lab = 1.5,
        cex.axis = 1.5)
    }
    else if (input$repeatedMeasureSelect == "Interaction of Block and Treatment"){
      ggplot2::ggplot(data = beer2,
                      mapping = aes(x = beer,
                                    y = score,
                                    color = judge,
                                    group = judge)) +
        ggplot2::geom_point(size=2) +
        ggplot2::geom_line(size=1) +
        ggplot2::theme_bw() +
        theme(axis.title = element_text(size = 18)) +
        viridis::scale_color_viridis(discrete = TRUE, option = "viridis") +
        xlab("Beer") +
        ylab("Score") +
        labs(color = "Judge")
    }
    else if (input$repeatedMeasureSelect == "Random Effects"){
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
  },
  alt = if (input$repeatedMeasureSelect == "Normality of Residuals"){
    "For normality, this is a plot of norm quantile with 2 points lay in 
      the 97% confidence envelope."}
  else if (input$repeatedMeasureSelect == "Homoscedasticity"){
    "For homoscedasticity, this is a plot of response versus explanatory, all 
      the points lay in a pattern."}
  else if (input$repeatedMeasureSelect == "Independence of Observation"){
    "For independence, this is a plot of response versus index, all the points 
      lay in a pattern."}
  else if (input$repeatedMeasureSelect == "Interaction of Block and Treatment"){
    "For interaction, points in the block have a different trend."}
  else if (input$repeatedMeasureSelect == "Random Effects"){
    "For random, this is a plot of norm quantile with 1 point lay in the 
    80% confidence envelope."})
})
  
  ### Model Assumptions ----
  observeEvent(input$submit, {
    updateButton(session, "submitAnova", disabled = TRUE)
  })
  
  observeEvent(input$submitAnova,{
    output$markAnova <- renderUI({
      if ("Normality" %in% input$dropAnova &&
          "Homoscedasticity" %in% input$dropAnova &&
          "Independence of Observation" %in% input$dropAnova &&
          length(input$dropAnova) == 3){
        paste("Congratulation! You got it right!")
      }
      else if ("Equality of the covariate's slope parameter" %in% input$dropAnova |
               "No statistically significiant potential outliers" %in% input$dropAnova |
               "Linear relationship covariate and the response" %in% input$dropAnova |
               "Random effects" %in% input$dropAnova |
               "Interaction of block and treatment" %in% input$dropAnova){
        if("Normality" %in% input$dropAnova &&
           "Homoscedasticity" %in% input$dropAnova &&
           "Independence of Observation" %in% input$dropAnova){
          paste("You've included some assumptions not needed in this situation")
        }
        else if("Normality" %in% input$dropAnova == FALSE |
                "Homoscedasticity" %in% input$dropAnova == FALSE |
                "Independence of Observation" %in% input$dropAnova == FALSE){
          paste("You've included some assumptions not needed in this situation and 
                You failed to list some assumptions")
        }
      }
      else if ("Normality" %in% input$dropAnova == FALSE |
               "Homoscedasticity" %in% input$dropAnova == FALSE |
               "Independence of Observation" %in% input$dropAnova == FALSE){
        paste("You failed to list some assumptions")
      }
    })
  })
  
  observeEvent(input$submitAnova,{
    output$markAnova2 <- renderUI({
      if("Normality" %in% input$dropAnova &&
         "Homoscedasticity" %in% input$dropAnova &&
         "Independence of Observation" %in% input$dropAnova &&
         length(input$dropAnova) == 3){
        renderIcon("correct")
      }
      else{
        renderIcon("incorrect")
      }
    })
  })
  
  observeEvent(input$submit, {
    updateButton(session, "submitAncova", disabled = TRUE)
  })
  
  observeEvent(input$submitAncova,{
    output$markAncova <- renderUI({
      if ("Normality" %in% input$dropAncova &&
          "Homoscedasticity" %in% input$dropAncova &&
          "Independence of Observation" %in% input$dropAncova &&
          "Linear relationship covariate and the response" %in% input$dropAncova &&
          "Equality of the covariate's slope parameter" %in% input$dropAncova &&
          "No statistically significiant potential outliers" %in% input$dropAncova &&
          length(input$dropAncova) == 6){
        paste("Congratulation! You got it right!")
      }
      else if ("Random effects" %in% input$dropAncova |
               "Interaction of block and treatment" %in% input$dropAncova){
        if("Normality" %in% input$dropAncova &&
           "Homoscedasticity" %in% input$dropAncova &&
           "Independence of Observation" %in% input$dropAncova &&
           "Linear relationship covariate and the response" %in% input$dropAncova &&
           "Equality of the covariate's slope parameter" %in% input$dropAncova &&
           "No statistically significiant potential outliers" %in% input$dropAncova){
          paste("You've included some assumptions not needed in this situation")
        }
        else if("Normality" %in% input$dropAnova == FALSE |
                "Homoscedasticity" %in% input$dropAnova == FALSE |
                "Independence of Observation" %in% input$dropAnova == FALSE |
                "Linear relationship covariate and the response" %in% input$dropAncova == FALSE |
                "Equality of the covariate's slope parameter" %in% input$dropAncova == FALSE |
                "No statistically significiant potential outliers" %in% input$dropAncova == FALSE){
          paste("You've included some assumptions not needed in this situation and 
                You failed to list some assumptions")
        }
      }
      else if ("Normality" %in% input$dropAnova == FALSE |
               "Homoscedasticity" %in% input$dropAnova == FALSE |
               "Independence of Observation" %in% input$dropAnova == FALSE |
               "Linear relationship covariate and the response" %in% input$dropAncova == FALSE |
               "Equality of the covariate's slope parameter" %in% input$dropAncova == FALSE |
               "No statistically significiant potential outliers" %in% input$dropAncova == FALSE){
        paste("You failed to list some assumptions")
      }
    })
  })
  
  observeEvent(input$submitAncova,{
    output$markAncova2 <- renderUI({
      if("Normality" %in% input$dropAncova &&
         "Homoscedasticity" %in% input$dropAncova &&
         "Independence of Observation" %in% input$dropAncova &&
         "Linear relationship covariate and the response" %in% input$dropAncova &&
         "Equality of the covariate's slope parameter" %in% input$dropAncova &&
         "No statistically significiant potential outliers" %in% input$dropAncova &&
         length(input$dropAncova) == 6){
        renderIcon("correct")
      }
      else{
        renderIcon("incorrect")
      }
    })
  })
  
  observeEvent(input$submit, {
    updateButton(session, "submitBlocking", disabled = TRUE)
  })
  
  observeEvent(input$submitBlocking,{
    output$markBlocking <- renderUI({
      if ("Normality" %in% input$dropBlocking &&
          "Homoscedasticity" %in% input$dropBlocking &&
          "Independence of Observation" %in% input$dropBlocking &&
          "Interaction of block and treatment" %in% input$dropBlocking &&
          length(input$dropBlocking) == 4){
        paste("Congratulation! You got it right!")
      }
      else if ("Equality of the covariate's slope parameter" %in% input$dropBlocking |
               "No statistically significiant potential outliers" %in% input$dropBlocking |
               "Linear relationship covariate and the response" %in% input$dropBlocking |
               "Random effects" %in% input$dropBlocking ){
        if("Normality" %in% input$dropBlocking &&
           "Homoscedasticity" %in% input$dropBlocking &&
           "Independence of Observation" %in% input$dropBlocking &&
           "Interaction of block and treatment" %in% input$dropBlocking){
          paste("You've included some assumptions not needed in this situation")
        }
        else if("Normality" %in% input$dropBlocking == FALSE |
                "Homoscedasticity" %in% input$dropBlocking == FALSE |
                "Independence of Observation" %in% input$dropBlocking == FALSE |
                "Interaction of block and treatment" %in% input$dropBlocking == FALSE){
          paste("You've included some assumptions not needed in this situation and 
                You failed to list some assumptions")
        }
      }
      else if ("Normality" %in% input$dropBlocking |
               "Homoscedasticity" %in% input$dropBlocking |
               "Independence of Observation" %in% input$dropBlocking |
               "Interaction of block and treatment" %in% input$dropBlocking){
        paste("You failed to list some assumptions")
      }
    })
  })
  
  observeEvent(input$submitBlocking,{
    output$markBlocking2 <- renderUI({
      if ("Normality" %in% input$dropBlocking &&
          "Homoscedasticity" %in% input$dropBlocking &&
          "Independence of Observation" %in% input$dropBlocking &&
          "Interaction of block and treatment" %in% input$dropBlocking &&
          length(input$dropBlocking) == 4){
        renderIcon("correct")
      }
      else{
        renderIcon("incorrect")
      }
    })
  })
  
  observeEvent(input$submit, {
    updateButton(session, "submitRandomEffect", disabled = TRUE)
  })
  
  observeEvent(input$submitRandomEffect,{
    output$markRandomEffect <- renderUI({
      if ("Normality" %in% input$dropRandomEffect &&
          "Homoscedasticity" %in% input$dropRandomEffect &&
          "Independence of Observation" %in% input$dropRandomEffect &&
          "Random effects" %in% input$dropRandomEffect &&
          length(input$dropRandomEffect) == 4){
        paste("Congratulation! You got it right!")
      }
      else if ("Equality of the covariate's slope parameter" %in% input$dropRandomEffect |
               "No statistically significiant potential outliers" %in% input$dropRandomEffect |
               "Linear relationship covariate and the response" %in% input$dropRandomEffect |
               "Interaction of block and treatment" %in% input$dropRandomEffect){
        if("Normality" %in% input$dropRandomEffect &&
           "Homoscedasticity" %in% input$dropRandomEffect &&
           "Independence of Observation" %in% input$dropRandomEffect &&
           "Random effects" %in% input$dropRandomEffect){
          paste("You've included some assumptions not needed in this situation")
        }
        else if("Normality" %in% input$dropRandomEffect == FALSE |
                "Homoscedasticity" %in% input$dropRandomEffect == FALSE |
                "Independence of Observation" %in% input$dropRandomEffect == FALSE |
                "Random effects" %in% input$dropRandomEffect == FALSE){
          paste("You've included some assumptions not needed in this situation and 
                You failed to list some assumptions")
        }
      }
      else if ("Normality" %in% input$dropAnova == FALSE |
               "Homoscedasticity" %in% input$dropAnova == FALSE |
               "Independence of Observation" %in% input$dropAnova == FALSE |
               "Random effects" %in% input$dropRandomEffect == FALSE){
        paste("You failed to list some assumptions")
      }
    })
  })
  
  observeEvent(input$submitRandomEffect,{
    output$markRandomEffect2 <- renderUI({
      if ("Normality" %in% input$dropRandomEffect &&
          "Homoscedasticity" %in% input$dropRandomEffect &&
          "Independence of Observation" %in% input$dropRandomEffect &&
          "Random effects" %in% input$dropRandomEffect &&
          length(input$dropRandomEffect) == 4){
        renderIcon("correct")
      }
      else{
        renderIcon("incorrect")
      }
    })
  })
  
  observeEvent(input$submit, {
    updateButton(session, "submitRepeatedMeasure", disabled = TRUE)
  })
  
  observeEvent(input$submitRepeatedMeasure,{
    output$markRepeatedMeasure <- renderUI({
      if ("Normality" %in% input$dropRepeatedMeasure &&
          "Homoscedasticity" %in% input$dropRepeatedMeasure &&
          "Independence of Observation" %in% input$dropRepeatedMeasure &&
          "Random effects" %in% input$dropRepeatedMeasure &&
          "Interaction of block and treatment" %in% input$dropRepeatedMeasure &&
          length(input$dropRepeatedMeasure) == 5){
        paste("Congratulation! You got it right!")
      }
      else if ("Equality of the covariate's slope parameter" %in% input$dropRepeatedMeasure |
               "No statistically significiant potential outliers" %in% input$dropRepeatedMeasure |
               "Linear relationship covariate and the response" %in% input$dropRepeatedMeasure){
        if("Normality" %in% input$dropRepeatedMeasure &&
           "Homoscedasticity" %in% input$dropRepeatedMeasure &&
           "Independence of Observation" %in% input$dropRepeatedMeasure &&
           "Random effects" %in% input$dropRepeatedMeasure &&
           "Interaction of block and treatment" %in% input$dropRepeatedMeasure){
          paste("You've included some assumptions not needed in this situation")
        }
        else if("Normality" %in% input$dropRepeatedMeasure == FALSE |
                "Homoscedasticity" %in% input$dropRepeatedMeasure == FALSE |
                "Independence of Observation" %in% input$dropRepeatedMeasure == FALSE |
                "Random effects" %in% input$dropRepeatedMeasure == FALSE |
                "Interaction of block and treatment" %in% input$dropRepeatedMeasure == FALSE){
          paste("You've included some assumptions not needed in this situation and 
                You failed to list some assumptions")
        }
      }
      else if ("Normality" %in% input$dropRepeatedMeasure == FALSE |
               "Homoscedasticity" %in% input$dropRepeatedMeasure == FALSE |
               "Independence of Observation" %in% input$dropRepeatedMeasure == FALSE |
               "Random effects" %in% input$dropRepeatedMeasure == FALSE |
               "Interaction of block and treatment" %in% input$dropRepeatedMeasure == FALSE){
        paste("You failed to list some assumptions")
      }
    })
  })
  
  observeEvent(input$submitRepeatedMeasure,{
    output$markRepeatedMeasure2 <- renderUI({
      if ("Normality" %in% input$dropRepeatedMeasure &&
          "Homoscedasticity" %in% input$dropRepeatedMeasure &&
          "Independence of Observation" %in% input$dropRepeatedMeasure &&
          "Random effects" %in% input$dropRepeatedMeasure &&
          "Interaction of block and treatment" %in% input$dropRepeatedMeasure &&
          length(input$dropRepeatedMeasure) == 5){
        renderIcon("correct")
      }
      else{
        renderIcon("incorrect")
      }
    })
  })
  
  ### Checking  ---- 
    #### Normality ----
  normalityData1 <- rnorm(n = 50, mean = 0, sd = 1)
  normalityData2 <- rnorm(n = 50, mean = 0, sd = 1)
  normalityData3 <-  rbeta(50, 0.7, 1.5)
  
  output$normalityGamePlot1 <- renderPlot(
    expr = {
      car::qqPlot(
      pch = 19,
      cex = 1.5,
      id = FALSE,
      x = normalityData1,
      distribution = "norm",
      envelope = 0.95,
      ylab = "data",
      main = "Plot A"
      )},
    alt = "This is a plot of normal quantiles. There are 50 points in the 95% confidence 
        envelope.")
  
  output$normalityGamePlot2 <- renderPlot(
    expr = {
    car::qqPlot(
      pch = 19,
      cex = 1.5,
      id = FALSE,
      x = normalityData2,
      distribution = "norm",
      envelope = 0.95,
      ylab = "data",
      main = "Plot C"
    )},
    alt = "This is a plot of normal quantiles. There are 50 points, most of the 
    points lied in the 95% confidence envelope, while others not."
  )
  
  output$normalityGamePlot3 <- renderPlot(
    expr = {
    car::qqPlot(
      pch = 19,
      cex = 1.5,
      id = FALSE,
      x = normalityData3,
      distribution = "norm",
      envelope = 0.8,
      ylab = "data",
      main = "Plot B"
    )},
    alt = "This is a plot of normal quantiles. There are 50 points in the 95% confidence 
    envelope.")
  
  observeEvent(input$submitNormality,{
    output$markNormality <- renderUI({
      if (input$nomalitySelected == 'plotC'){
        renderIcon("correct")
      }
      else{
        renderIcon("incorrect")
      }
    })
  })
  
  observeEvent(input$submit, {
    updateButton(session, "submitNormality", disabled = TRUE)
  })
    #### Homo ----
  homoData1 <- data.frame(
    homoData1_1 = c(rep('1',10), rep('2',10), rep('3',10), rep('4',10), rep('5',10)),
    homoData1_2 = c(sample(1:100, 10, replace=FALSE),
                    sample(1:100, 10, replace=FALSE),
                    sample(1:80, 10, replace=FALSE),
                    sample(1:100, 10, replace=FALSE),
                    sample(1:100, 10, replace=FALSE)))
  homoData2 <- data.frame(
    homoData2_1 = c(rep('1',10), rep('2',10), rep('3',10), rep('4',10), rep('5',10)),
    homoData2_2 = c(sample(1:100, 10, replace=FALSE),
                    sample(1:100, 10, replace=FALSE),
                    sample(1:100, 10, replace=FALSE),
                    sample(1:100, 10, replace=FALSE),
                    sample(1:100, 10, replace=FALSE)))
  homoData3 <- data.frame(
    homoData3_1 = c(rep('1',10), rep('2',10), rep('3',10), rep('4',10), rep('5',10)),
    homoData3_2 = c(sample(1:100, 10, replace=FALSE),
                    sample(10:90, 10, replace=FALSE),
                    sample(20:80, 10, replace=FALSE),
                    sample(30:70, 10, replace=FALSE),
                    sample(40:60, 10, replace=FALSE)))
  
  output$homoGamePlot1 <- renderPlot(
    expr = {
    stripchart(
      pch = 19,
      cex = 1.5,
      homoData1_2 ~ homoData1_1,
      vertical = TRUE,
      data = homoData1,
      xlab = 'Index',
      ylab = 'homoData1',
      main = "Plot A",
      cex.lab = 1.5,
      cex.axis = 1.5)
    },
    alt = "This is a plot of response versus explanatory, all the points lay in 
    a random position.")
  
  output$homoGamePlot2 <- renderPlot(
    expr = {
    stripchart(
      pch = 19,
      cex = 1.5,
      homoData2_2 ~ homoData2_1,
      vertical = TRUE,
      data = homoData2,
      xlab = 'Index',
      ylab = 'homoData2',
      main = "Plot B",
      cex.lab = 1.5,
      cex.axis = 1.5)
  },
   alt = "This is a plot of response versus explanatory, all the points lay in a 
    random position.")
  
  output$homoGamePlot3 <- renderPlot(
    expr = {
    stripchart(
      pch = 19,
      cex = 1.5,
      homoData3_2 ~ homoData3_1,
      vertical = TRUE,
      data = homoData3,
      xlab = 'Index',
      ylab = 'homoData3',
      main = "Plot C",
      cex.lab = 1.5,
      cex.axis = 1.5)
    },
    alt = "This is a plot of response versus explanatory, all the points lay in 
    a pattern.")
  
  observeEvent(input$submitHomo,{
    output$markHomo <- renderUI({
      if (input$homoSelected == 'plotC'){
        renderIcon("correct")
      }
      else{
        renderIcon("incorrect")
      }
    })
  })
  
  observeEvent(input$submit, {
    updateButton(session, "submitHomo", disabled = TRUE)
  })
  #### Indep ----  
  indeData1 <- rnorm(n = 50, mean = 0, sd = 1)
  indeData2 <- rnorm(n = 50, mean = 0, sd = 1)
  indeData3 <- ts(1:10, frequency = 4, start = c(1959, 2))
  
  output$indeGamePlot1 <- renderPlot(
    expr = {
    plot(indeData1,
         type = "b",
         main = "Plot C")
    },
    alt = "This is a plot of response versus index, all the points lay in a pattern.")
  
  output$indeGamePlot2 <- renderPlot(
    expr = {
    plot(indeData2,
         type = "b",
         main = "Plot B")
    },
    alt = "This is a plot of response versus index, all the points layin a random 
    position.")
  
  output$indeGamePlot3 <- renderPlot(
    expr = {
    plot(indeData3,
         type = "b",
         main = "Plot A",
         ylab = "Index",
         xlab = 'Index')
    },
    alt = "This is a plot of response versus index, all the points lay in a random 
    position")
  
  observeEvent(input$submitInde,{
    output$markInde <- renderUI({
      if (input$indeSelected == 'plotC'){
        renderIcon("correct")
      }
      else{
        renderIcon("incorrect")
      }
    })
  })
  
  observeEvent(input$submit, {
    updateButton(session, "submitInde", disabled = TRUE)
  })
  #### Linear ----
  linearData1 <- data.frame(
    linearData1_1 = rep(c('1', '2', '3', '4', '5'), 10),
    linearData1_2 = c(sample(1:20, 10, replace=FALSE),
                      sample(10:30, 10, replace=FALSE),
                      sample(15:35, 10, replace=FALSE),
                      sample(20:40, 10, replace=FALSE),
                      sample(30:50, 10, replace=FALSE)),
    linearData1_3 = c(sample(1:20, 10, replace=FALSE),
                      sample(10:30, 10, replace=FALSE),
                      sample(15:35, 10, replace=FALSE),
                      sample(20:40, 10, replace=FALSE),
                      sample(30:50, 10, replace=FALSE))
  )
  linearData2 <- data.frame(
    linearData2_1 = rep(c('1', '2', '3', '4', '5'), 10),
    linearData2_2 = c(sample(50:30, 10, replace=FALSE),
                      sample(40:20, 10, replace=FALSE),
                      sample(35:15, 10, replace=FALSE),
                      sample(30:10, 10, replace=FALSE),
                      sample(20:1, 10, replace=FALSE)),
    linearData2_3 = c(sample(1:20, 10, replace=FALSE),
                      sample(10:30, 10, replace=FALSE),
                      sample(15:35, 10, replace=FALSE),
                      sample(20:40, 10, replace=FALSE),
                      sample(30:50, 10, replace=FALSE))
  )
  linearData3 <- data.frame(
    linearData3_1 = c(rep("1", 10), rep("2", 10), rep("3", 10), rep("4", 10), rep("5", 10)),
    linearData3_2 = sample(1:100, 50, replace=FALSE),
    linearData3_3 = sample(1:100, 50, replace=FALSE)
  )
  
  output$linearGamePlot1 <- renderPlot(
    expr = {
    ggplot2::ggplot(data = linearData1,
                    mapping = ggplot2::aes(
                      y = linearData1_2,
                      x = linearData1_3,
                      group = linearData1_1,
                      color = linearData1_1
                    )) +
      ggplot2::geom_point(size = 3) +
      ggplot2::theme_bw() +
      theme(axis.title = element_text(size = 18)) +
      xlab("X") +
      ylab("Y") +
      labs(color = "Type")+
      ggtitle("Plot C")+ 
      theme(plot.title = element_text(size=15, hjust = 0.5, vjust = 1, face = "bold"))
    },
    alt = "The plot shows that response and explantory have no linear relationship.")
  
  output$linearGamePlot2 <- renderPlot(
    expr = {
    ggplot2::ggplot(data = linearData2,
                    mapping = ggplot2::aes(
                      y = linearData2_2,
                      x = linearData2_3,
                      group = linearData2_1,
                      color = linearData2_1
                    )) +
      ggplot2::geom_point(size = 3) +
      ggplot2::theme_bw() +
      theme(axis.title = element_text(size = 18)) +
      xlab("X") +
      ylab("Y") +
      labs(color = "Type")+
      ggtitle("Plot B")+ 
      theme(plot.title = element_text(size=15, hjust = 0.5, vjust = 1, face = "bold"))
    },
    alt = "The plot shows that response and explantory have a negative linear relationship.")
  
  output$linearGamePlot3 <- renderPlot(
    expr = {
    ggplot2::ggplot(data = linearData3,
                    mapping = ggplot2::aes(
                      y = linearData3_2,
                      x = linearData3_3,
                      group = linearData3_1,
                      color = linearData3_1
                    )) +
      ggplot2::geom_point(size = 3) +
      ggplot2::theme_bw() +
      theme(axis.title = element_text(size = 18)) +
      xlab("X") +
      ylab("Y") +
      labs(color = "Type")+
      ggtitle("Plot A")+ 
      theme(plot.title = element_text(size=15, hjust = 0.5, vjust = 1, face = "bold"))
    },
    alt = "The plot shows that response and explantory have a postitive linear relationship.")
  
  observeEvent(input$submitLinear,{
    output$markLinear <- renderUI({
      if (input$linearSelected == 'plotC'){
        renderIcon("correct")
      }
      else{
        renderIcon("incorrect")
      }
    })
  })
  
  observeEvent(input$submit, {
    updateButton(session, "submitLinear", disabled = TRUE)
  })
  #### Slope ----
  slopeData1 <- data.frame(
    slopeData1_1 = rep(c('1', '2', '3', '4', '5'), 10),
    slopeData1_2 = c(sample(1:20, 10, replace=FALSE),
                     sample(10:30, 10, replace=FALSE),
                     sample(15:35, 10, replace=FALSE),
                     sample(20:40, 10, replace=FALSE),
                     sample(30:50, 10, replace=FALSE)),
    slopeData1_3 = c(sample(1:20, 10, replace=FALSE),
                     sample(10:30, 10, replace=FALSE),
                     sample(15:35, 10, replace=FALSE),
                     sample(20:40, 10, replace=FALSE),
                     sample(30:50, 10, replace=FALSE))
  )
  slopeData2 <- data.frame(
    slopeData2_1 = rep(c('1', '2', '3', '4', '5'), 10),
    slopeData2_2 = c(sample(50:30, 10, replace=FALSE),
                     sample(40:20, 10, replace=FALSE),
                     sample(35:15, 10, replace=FALSE),
                     sample(30:10, 10, replace=FALSE),
                     sample(20:1, 10, replace=FALSE)),
    slopeData2_3 = c(sample(1:20, 10, replace=FALSE),
                     sample(10:30, 10, replace=FALSE),
                     sample(15:35, 10, replace=FALSE),
                     sample(20:40, 10, replace=FALSE),
                     sample(30:50, 10, replace=FALSE))
  )
  slopeData3 <- data.frame(
    slopeData3_1 = c(rep("1", 10), rep("2", 10), rep("3", 10), rep("4", 10), rep("5", 10)),
    slopeData3_2 = sample(1:100, 50, replace=FALSE),
    slopeData3_3 = sample(1:100, 50, replace=FALSE)
  )
  
  output$slopeGamePlot1 <- renderPlot(
    expr = {
    ggplot2::ggplot(data = slopeData1,
                    mapping = ggplot2::aes(
                      y = slopeData1_2,
                      x = slopeData1_3,
                      group = slopeData1_1,
                      color = slopeData1_1
                    )) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_smooth(method = "lm", se = FALSE) +
      ggplot2::theme_bw() +
      theme(axis.title = element_text(size = 18)) +
      xlab("X") +
      ylab("Y") +
      labs(color = "Type")+
      ggtitle("Plot A")+ 
      theme(plot.title = element_text(size=15, hjust = 0.5, vjust = 1, face = "bold"))
    },
    alt = "In the plot, points in the different groups have a similar trend.")
  
  output$slopeGamePlot2 <- renderPlot(
    expr = {
    ggplot2::ggplot(data = slopeData2,
                    mapping = ggplot2::aes(
                      y = slopeData2_2,
                      x = slopeData2_3,
                      group = slopeData2_1,
                      color = slopeData2_1
                    )) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_smooth(method = "lm", se = FALSE) +
      ggplot2::theme_bw() +
      theme(axis.title = element_text(size = 18)) +
      xlab("X") +
      ylab("Y") +
      labs(color = "Type")+
      ggtitle("Plot C")+ 
      theme(plot.title = element_text(size=15, hjust = 0.5, vjust = 1, face = "bold"))
    },
    alt = "In the plot, points in the different groups have a similar trend")
  
  output$slopeGamePlot3 <- renderPlot(
    expr = {
    ggplot2::ggplot(data = slopeData3,
                    mapping = ggplot2::aes(
                      y = slopeData3_2,
                      x = slopeData3_3,
                      group = slopeData3_1,
                      color = slopeData3_1
                    )) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_smooth(method = "lm", se = FALSE) +
      ggplot2::theme_bw() +
      theme(axis.title = element_text(size = 18)) +
      xlab("X") +
      ylab("Y") +
      labs(color = "Type")+
      ggtitle("Plot B")+ 
      theme(plot.title = element_text(size=15, hjust = 0.5, vjust = 1, face = "bold"))
    },
    alt = "In the plot, points in the different groups have a different trend.")
  
  observeEvent(input$submitSlope,{
    output$markSlope <- renderUI({
      if (input$slopeSelected == 'plotC'){
        renderIcon("correct")
      }
      else{
        renderIcon("incorrect")
      }
    })
  })
  
  observeEvent(input$submit, {
    updateButton(session, "submitSlope", disabled = TRUE)
  })
  #### Outliters ----
  outData1 <- data.frame(
    outData1_1 = rep(c('1', '2', '3', '4', '5'), 10),
    outData1_2 = c(sample(1:10, 10, replace=FALSE),
                   sample(10:20, 10, replace=FALSE),
                   sample(20:30, 10, replace=FALSE),
                   sample(30:40, 10, replace=FALSE),
                   sample(40:50, 10, replace=FALSE)),
    outData1_3 = c(sample(1:10, 10, replace=FALSE),
                   sample(10:20, 10, replace=FALSE),
                   sample(20:30, 10, replace=FALSE),
                   sample(30:40, 10, replace=FALSE),
                   sample(40:50, 10, replace=FALSE))
  )
  outData2 <- data.frame(
    outData2_1 = rep(c('1', '2', '3', '4', '5'), 10),
    outData2_2 = c(sample(1:10, 10, replace=FALSE),
                   sample(10:20, 10, replace=FALSE),
                   sample(20:30, 10, replace=FALSE),
                   sample(30:40, 10, replace=FALSE),
                   sample(40:50, 10, replace=FALSE)),
    outData2_3 = c(sample(1:10, 10, replace=FALSE),
                   sample(10:20, 10, replace=FALSE),
                   sample(20:30, 10, replace=FALSE),
                   sample(30:40, 10, replace=FALSE),
                   sample(40:50, 10, replace=FALSE))
  )
  outData3 <- data.frame(
    outData3_1 = c(rep("1", 10), rep("2", 10), rep("3", 10), rep("4", 10), rep("5", 10)),
    outData3_2 = c(sample(1:100, 48, replace=FALSE),
                   sample(200:210, 2, replace=FALSE)),
    outData3_3 = sample(1:100, 50, replace=FALSE)
  )
  
  output$outGamePlot1 <- renderPlot(
    expr = {
    realOutData1 <- rstatix::mahalanobis_distance(outData1)
    realOutData1 <- cbind(realOutData1, factor = outData1$outData1_1)
    ggplot2::ggplot(data = realOutData1,
                    mapping = ggplot2::aes(
                      y = outData1_2,
                      x = outData1_3,
                      color = factor
                    )) +
      ggplot2::geom_point(size = 3) +
      ggplot2::theme_bw() +
      theme(axis.title = element_text(size = 18)) +
      xlab("X") +
      ylab("Y") +
      labs(color = "Type", shape = "Potential Outlier")+
      ggtitle("Plot C")+ 
      theme(plot.title = element_text(size=15, hjust = 0.5, vjust = 1, face = "bold"))
    }, 
    alt = "The plot shows that the data do not have obvious outliers.")
  
  output$outGamePlot2 <- renderPlot(
    expr = {
    realOutData2 <- rstatix::mahalanobis_distance(outData2)
    realOutData2 <- cbind(realOutData2, factor = outData2$outData2_1)
    ggplot2::ggplot(data = realOutData2,
                    mapping = ggplot2::aes(
                      y = outData2_2,
                      x = outData2_3,
                      color = factor
                    )) +
      ggplot2::geom_point(size = 3) +
      ggplot2::theme_bw() +
      theme(axis.title = element_text(size = 18)) +
      xlab("X") +
      ylab("Y") +
      labs(color = "Type", shape = "Potential Outlier")+
      ggtitle("Plot B")+ 
      theme(plot.title = element_text(size=15, hjust = 0.5, vjust = 1, face = "bold"))
    },
    alt = "The plot shows that the data do not have obvious outliers.")
  
  output$outGamePlot3 <- renderPlot(
    expr = {
    realOutData3 <- rstatix::mahalanobis_distance(outData3)
    realOutData3 <- cbind(realOutData3, factor = outData3$outData3_1)
    ggplot2::ggplot(data = realOutData3,
                    mapping = ggplot2::aes(
                      y = outData3_2,
                      x = outData3_3,
                      color = factor
                    )) +
      ggplot2::geom_point(size = 3) +
      ggplot2::theme_bw() +
      theme(axis.title = element_text(size = 18)) +
      xlab("X") +
      ylab("Y") +
      labs(color = "Type", shape = "Potential Outlier")+
      ggtitle("Plot A")+ 
      theme(plot.title = element_text(size=15, hjust = 0.5, vjust = 1, face = "bold"))
    }, 
    alt = "The plot shows that the data has obvious outliers.")
  
  observeEvent(input$submitOut,{
    output$markOut <- renderUI({
      if (input$outSelected == 'plotC'){
        renderIcon("correct")
      }
      else{
        renderIcon("incorrect")
      }
    })
  })
  
  observeEvent(input$submit, {
    updateButton(session, "submitOut", disabled = TRUE)
  })
  #### Interaction ----
  interData1 <- data.frame(
    interData1_1 = rep(c('1', '2', '3', '4', '5'), 10),
    interData1_2 = c(sample(1:10, 10, replace=FALSE),
                     sample(10:20, 10, replace=FALSE),
                     sample(20:30, 10, replace=FALSE),
                     sample(30:40, 10, replace=FALSE),
                     sample(40:50, 10, replace=FALSE)),
    interData1_3 = c(sample(1:10, 10, replace=FALSE),
                     sample(10:20, 10, replace=FALSE),
                     sample(20:30, 10, replace=FALSE),
                     sample(30:40, 10, replace=FALSE),
                     sample(40:50, 10, replace=FALSE))
  )
  interData2 <- data.frame(
    interData2_1 = rep(c('1', '2', '3', '4', '5'), 10),
    interData2_2 = c(sample(1:10, 10, replace=FALSE),
                     sample(10:20, 10, replace=FALSE),
                     sample(20:30, 10, replace=FALSE),
                     sample(30:40, 10, replace=FALSE),
                     sample(40:50, 10, replace=FALSE)),
    interData2_3 = c(sample(1:10, 10, replace=FALSE),
                     sample(10:20, 10, replace=FALSE),
                     sample(20:30, 10, replace=FALSE),
                     sample(30:40, 10, replace=FALSE),
                     sample(40:50, 10, replace=FALSE))
  )
  interData3 <- data.frame(
    interData3_1 = c(rep("1", 10), rep("2", 10), rep("3", 10), rep("4", 10), rep("5", 10)),
    interData3_2 = sample(1:100, 50, replace=FALSE),
    interData3_3 = sample(1:100, 50, replace=FALSE)
  )
  
  output$interGamePlot1 <- renderPlot(
    expr = {
    ggplot2::ggplot(data = interData1,
                    mapping = aes(x = interData1_2,
                                  y = interData1_3,
                                  color = interData1_1,
                                  group = interData1_1)) +
      ggplot2::geom_point(size=2) +
      ggplot2::geom_line(size=1) +
      ggplot2::theme_bw() +
      theme(axis.title = element_text(size = 18)) +
      xlab("X") +
      ylab("Y") +
      labs(color = "Group")+
      ggtitle("Plot A")+ 
      theme(plot.title = element_text(size=15, hjust = 0.5, vjust = 1, face = "bold"))
    }, 
    alt = "In the plot, points in the different block have a similar trend.")
  
  output$interGamePlot2 <- renderPlot(
    expr ={
    ggplot2::ggplot(data = interData2,
                    mapping = aes(x = interData2_2,
                                  y = interData2_3,
                                  color = interData2_1,
                                  group = interData2_1)) +
      ggplot2::geom_point(size=2) +
      ggplot2::geom_line(size=1) +
      ggplot2::theme_bw() +
      theme(axis.title = element_text(size = 18)) +
      xlab("X") +
      ylab("Y") +
      labs(color = "Group")+
      ggtitle("Plot C")+ 
      theme(plot.title = element_text(size=15, hjust = 0.5, vjust = 1, face = "bold"))
    }, 
    alt = "In the plot, points in the different block have a similar trend.")
  
  output$interGamePlot3 <- renderPlot(
    expr = {
    ggplot2::ggplot(data = interData3,
                    mapping = aes(x = interData3_2,
                                  y = interData3_3,
                                  color = interData3_1,
                                  group = interData3_1)) +
      ggplot2::geom_point(size=2) +
      ggplot2::geom_line(size=1) +
      ggplot2::theme_bw() +
      theme(axis.title = element_text(size = 18)) +
      xlab("X") +
      ylab("Y") +
      labs(color = "Group")+
      ggtitle("Plot B")+ 
      theme(plot.title = element_text(size=15, hjust = 0.5, vjust = 1, face = "bold"))
    }, 
    alt = "In the plot, points in the different block have a different trend.")
  
  observeEvent(input$submitInter,{
    output$markInter <- renderUI({
      if (input$interSelected == 'plotC'){
        renderIcon("correct")
      }
      else{
        renderIcon("incorrect")
      }
    })
  })
  
  observeEvent(input$submit, {
    updateButton(session, "submitInter", disabled = TRUE)
  })
  
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Learn the assumptions for each model and how to exam them. Also, 
      test yourself with the drag and drop and multiple choice games.",
      type = "info"
    )
  })
  }

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
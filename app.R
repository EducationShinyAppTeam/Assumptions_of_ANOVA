# Load Packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(ggplot2)
library(car)
library(sortable)
library(shinyWidgets)

# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "[Sample App]"
APP_DESCP  <<- paste(
  "This app is used to let the student learn about the assumptions of models in ANOVA",
  "First, explore the assumptions for each model and how to exam them with plot",
  "Second, use the drag and drop game to math the assumptions with each model",
  "Third, choose the appropriate plot to exam each assumption"
)
# End App Meta Data------------------------------------------------------------

# Global Constants, Functions, and Data Sets ----

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "black",
    ### Create the app header
    dashboardHeader(
      title = "Assumptions of ANOVA Models",
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      titleWidth = 250,
      tags$li(class = "dropdown",
              tags$a(href='https://shinyapps.science.psu.edu/',
                     icon("home"))),
      tags$li(
        class = "dropdown",
        tags$a(target = "_blank", icon("comments"),
               href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=[Assumptions of AONVA Models]"
        )
      )
    ),
    ### Create the sidebar/left navigation menu
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("Drag and Drop Game", tabName = "game1", icon = icon("gamepad")),
        menuItem("Multiple Choices Game", tabName = "game2", icon = icon("gamepad")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ### Create the content
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")),
      tags$style(
        type = "text"),
      tabItems(
        #### Set up the Overview Page
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Assumptiopns of ANOVA Models"),
          p("This app introduces the assumptions for different ANOVA models and how to test those assumptions."),
          p("You can also learn what will happen if assumptions are invalid."),
          h2("Instructions"),
          tags$ol(
            tags$li("Click the go button to enter the prerequisites page."),
            tags$li("Being aware of the assumptions for each model."),
            tags$li("testing the buttons to see what will happen if assumptions are invalided."),
            tags$li("Matching up the assumptions with the model in game."),
            tags$li("Matching up the testing method with assumptions in the other game.")
          ),
          ##### Go Button
          div(style = "text-align: center",
              bsButton(
                inputId = "explore",
                label = "Explore",
                icon = icon("bolt"),
                size = "large")),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Neil J.
            Hatfield, Robert P. Carey, III and Gonghao Liu.",
            br(),
            "We would like to extend a special thanks to the Shiny Program Students.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 7/31/2020 by Gonghao Liu.")
          )
        ),
        #### Set up the Prerequisites Page
        tabItem(
          tabName = "prerequisites",
          h2("Prerequisites"),
          box(
            title = strong("Why testing assumptions is important to ANOVA?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "In general, ANOVA is a statistical technique that assesses potential
            differences in a scale-level dependent variable by a nominal-level
            variable having 2 or more categories. Among the ANOVA model, there
            are some specified models who have different assumptions, we need
            to make sure that the data is suitable for us to use different model."
          ),
          box(
            title = strong("What if the assumptions are not meet?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "In general, we have more than one method to conduct an ANOVA test,
            some method is used for those data who didn't meet one or more assumptions.
            By testing the assumption, we can pick the appropriate method to
            conduct the ANOVA test."
          )
        ),
        #### Set up an Explore Page
        tabItem(
          tabName = "explore",
          h2("Explore"),
          fluidPage(
            tabsetPanel(
              ##### Set up ANOVA page
              tabPanel("ANOVA",
                       fluidRow(strong(p("Scenario:")),
                                p("An experiment was conducted to determine the relationship of honey output and the types of flowers,
                                  there were 3 types of flowers, 3 beehives were randomly assigned to each type of flowers with 9 beehives in total."),
                                br(),
                                selectInput("anovaSelect",
                                            p("Select the assumption you want to test"),
                                            choices = list("Normality of Residual" = "normality",
                                                           "Homoscedasticity" = "homoscedasticity",
                                                           "Independence of Observation" = "independence")),
                                strong(p("Valid Example:")),
                                textOutput("anovaTextValid"),
                                plotOutput("anovaImageValid"),
                                tags$script(HTML(
                                  "$(document).ready(function() {
                                  document.getElementById('anovaImageValid').setAttribute('aria-label',
                                  `This plot output is depend on the user's choice.
                                  For normality, this is a plot of norm qunatile with all the points lay in the 97% confidence envelope.
                                  For homoscedasticity, this is a plot of response versus explanatory, all the points lay in a random position.
                                  For independence, this is a plot of response versus index, all the points lay in a random position.`)
                                  })"
                                )),
                                strong(p("InValid Example:")),
                                textOutput("anovaTextInValid"),
                                plotOutput("anovaImageInValid"),
                                tags$script(HTML(
                                  "$(document).ready(function() {
                                  document.getElementById('anovaImageInvalid').setAttribute('aria-label',
                                  `This plot output is depend on the user's choice.
                                  For normality, this is a plot of norm qunatile with 2 points lay in the 97% confidence envelope.
                                  For homoscedasticity, this is a plot of response versus explanatory, all the points lay in a pattern.
                                  For independence, this is a plot of response versus index, all the points lay in a pattern.`)
                                  })"
                                )
                                )
                       )
              ),
              ##### Set up ANCOVA page
              tabPanel("ANCOVA",
                       fluidRow(strong(p("Scenario:")),
                                p("We are wanting to understand the impact of the type of keyboard on how many hours of pain a person experiences in their hands, wrists, and forearms."),
                                p("We suspect that the number of hours a person spends keyboarding is related to the number of hours of pain that they feel."),
                                p("We have 12 volunteers who will use a specific keyboard we assign them for 2 weeks. During that time, they will record the
                                  number of hours they use the keyboard and the number of hours of repetitive motion pain during the study period."),
                                br(),
                                selectInput("ancovaSelect",
                                            p("Select the assumption you want to test"),
                                            choices = list("Normality of Residual" = "normality",
                                                           "Homoscedasticity" = "homoscedasticity",
                                                           "Independence of Observation" = "independence",
                                                           "Linear Relationship Covarite and The Response" = "linear",
                                                           "homoscedasticity of The Covarite's Slope Parametar" = "slope",
                                                           "No Statistically Significant Potential Outliers" = "outlier")),
                                strong(p("Valid Example:")),
                                textOutput("ancovaTextValid"),
                                plotOutput("ancovaImageValid"),
                                tags$script(HTML(
                                  "$(document).ready(function() {
                                  document.getElementById('ancovaImageValid').setAttribute('aria-label',
                                  `This plot output is depend on the user's choice.
                                  For normality, this is a plot of norm qunatile with all the points lay in the 97% confidence envelope.
                                  For homoscedasticity, this is a plot of response versus explanatory, all the points lay in a random position.
                                  For independence, this is a plot of response versus index, all the points lay in a random position.
                                  For linear, the plot shows that response and explantory have a linear relationship.
                                  For slope, points in the different groups have a similar trend.
                                  For outlier, the plot shows that the data don't have obvious outliers.`)
                                  })"
                                )),
                                strong(p("Invalid Example:")),
                                textOutput("ancovaTextInValid"),
                                plotOutput("ancovaImageInValid"),
                                tags$script(HTML(
                                  "$(document).ready(function() {
                                  document.getElementById('ancovaImageInValid').setAttribute('aria-label',
                                  `This plot output is depend on the user's choice.
                                  For normality, this is a plot of norm qunatile with 2 points lay in the 97% confidence envelope.
                                  For homoscedasticity, this is a plot of response versus explanatory, all the points lay in a pattern.
                                  For independence, this is a plot of response versus index, all the points lay in a pattern.
                                  For linear, the plot shows that response and explantory have no linear relationship.
                                  For slope, points in the different groups have a different trend.
                                  For outlier, the plot shows that the data have obvious outliers.`)
                                  })"
                                )
                                )
                       )
              ),
              ##### Set up blocking page
              tabPanel("Blocking",
                       fluidRow(
                         strong(p("Scenario:")),
                         p("A farmer wants to test out four varieties of barley and see if there is any difference in yield."),
                         p("He has four fields in which he can plant the barley. However, the farmer is aware of differences between each field. For example,"),
                         p(tags$li("One field has a higher clay content in the soil than the others")),
                         p(tags$li("One field has rockier soil than the others")),
                         p(tags$li("Two fields are in wetter climates; two are in drier climates")),
                         p(tags$li("One field very loose soil while another field has much more compacted soil")),
                         p(tags$li("Two fields are relatively flat, one has a hill in the middle, and the last has a valley.")),
                         br(),
                         selectInput("blockingSelect",
                                     p("Select the assumption you want to test"),
                                     choices = list("Normality of Residual" = "normality",
                                                    "Homoscedasticity" = "homoscedasticity",
                                                    "Independence of Observation" = "independence",
                                                    "Interaction of Block and Treatment" = "interaction")),
                         strong(p("Valid Example:")),
                         textOutput("blockingTextValid"),
                         plotOutput("blockingImageValid"),
                         tags$script(HTML(
                           "$(document).ready(function() {
                                  document.getElementById('blockingImageValid').setAttribute('aria-label',
                                  `This plot output is depend on the user's choice.
                                  For normality, this is a plot of norm qunatile with all the points lay in the 97% confidence envelope.
                                  For homoscedasticity, this is a plot of response versus explanatory, all the points lay in a random position.
                                  For independence, this is a plot of response versus index, all the points lay in a random position.
                                  For interaction, points in the different block have a similar trend.`)
                                  })"
                         )),
                         strong(p("Invalid Example:")),
                         textOutput("blockingTextInvalid"),
                         plotOutput("blockingImageInvalid"),
                         tags$script(HTML(
                           "$(document).ready(function() {
                                  document.getElementById('blockingImageInvalid').setAttribute('aria-label',
                                  `This plot output is depend on the user's choice.
                                  For normality, this is a plot of norm qunatile with 2 points lay in the 97% confidence envelope.
                                  For homoscedasticity, this is a plot of response versus explanatory, all the points lay in a pattern.
                                  For independence, this is a plot of response versus index, all the points lay in a pattern.
                                  For interaction, points in the different block have a different trend.`)
                                  })"
                         )
                         )
                       )
              ),
              ##### Set up random effect page
              tabPanel("Random Effect",
                       fluidRow(
                         strong(p("Scenario:")),
                         p("Apex Enterprises is a company that builds roadside restaurants
                            carrying one of several promoted trade names, leases franchises to
                            individuals to operate the restaurants, and provides management
                            services. This company employs a large number of personnel officers
                            who interview applicants for jobs in the restaurants. At the end of
                            the interview, the personnel officer assigns a rating between 0 to 100
                            to indicate the applicant's potential value on the job"),
                         p("Apex would like to know two things: How great is the variation is in
                            ratings among all personnel officers? What is the mean rating given by
                            all personnel officers?"),
                         br(),
                         selectInput("randomEffectSelect",
                                     p("Select the assumption you want to test"),
                                     choices = list("Normality of Residual" = "normality",
                                                    "Homoscedasticity" = "homoscedasticity",
                                                    "Independence of Observation" = "independence",
                                                    "Random Effect" = "random")),
                         strong(p("Valid Example:")),
                         textOutput("randomEffectTextValid"),
                         plotOutput("randomEffectImageValid"),
                         tags$script(HTML(
                           "$(document).ready(function() {
                                  document.getElementById('randomEffectImageValid').setAttribute('aria-label',
                                  `This plot output is depend on the user's choice.
                                  For normality, this is a plot of norm qunatile with all the points lay in the 97% confidence envelope.
                                  For homoscedasticity, this is a plot of response versus explanatory, all the points lay in a random position.
                                  For independence, this is a plot of response versus index, all the points lay in a random position.
                                  For random, this is a plot of norm qunatile with all the points lay in the 80% confidence envelope.`)
                                  })"
                         )),
                         strong(p("Invalid Example:")),
                         textOutput("randomEffectTextInvalid"),
                         plotOutput("randomEffectImageInvalid"),
                         tags$script(HTML(
                           "$(document).ready(function() {
                                  document.getElementById('randomEffectImageInvalid').setAttribute('aria-label',
                                  `This plot output is depend on the user's choice.
                                  For normality, this is a plot of norm qunatile with 2 points lay in the 97% confidence envelope.
                                  For homoscedasticity, this is a plot of response versus explanatory, all the points lay in a pattern.
                                  For independence, this is a plot of response versus index, all the points lay in a pattern.
                                  For random, this is a plot of norm qunatile with 1 point laid in the 80% confidence envelope.`)
                                  })"
                         )
                         )
                       )
              ),
              ##### Set up repeated measure page
              tabPanel("Repeated Measure",
                       fluidRow(
                         strong(p("Scenario:")),
                         p("Beer is big business; the craft brewing industry contributed $79.1 billion to
                the US Economy in 2018 and 550,000+ jobs (PA: $6.335 billion)."),
                         p("Getting a craft beer scored can be quite the achievement. In a single blind
                tasting, judges are given a chilled, properly poured beer and told the style
                category. They then judge the beer on Aroma (24 pts), Appearance (6 pts),
                Flavor (40 pts), Mouthfeel (10 pts), and Overall Impression (20 pts)."),
                         p("We have decided to put several State College beers to the test: "),
                         p(tags$li("Barnstormer (IPA, HVBC)")),
                         p(tags$li("Craftsman (Brown, HVBC)")),
                         p(tags$li("Red Mo (Red, Otto's)")),
                         p(tags$li("King Richard Red (Amber, Robin Hood)")),
                         br(),
                         selectInput("repeatedMeasureSelect",
                                     p("Select the assumption you want to test"),
                                     choices = list("Normality of Residual" = "normality",
                                                    "Homoscedasticity" = "homoscedasticity",
                                                    "Independence of Observation" = "independence",
                                                    "Interaction of Block and Treatment" = "interaction",
                                                    "Random Effect" = "random")),
                         strong(p("Valid Example:")),
                         textOutput("repeatedMeasureTextValid"),
                         plotOutput("repeatedMeasureImageValid"),
                         tags$script(HTML(
                           "$(document).ready(function() {
                                  document.getElementById('repeatedMeasureImageValid').setAttribute('aria-label',
                                  `This plot output is depend on the user's choice.
                                  For normality, this is a plot of norm qunatile with all the points lay in the 97% confidence envelope.
                                  For homoscedasticity, this is a plot of response versus explanatory, all the points lay in a random position.
                                  For independence, this is a plot of response versus index, all the points lay in a random position.
                                  For interaction, points in the different block have a similar trend.
                                  For random, this is a plot of norm qunatile with all the points lay in the 80% confidence envelope.`)
                                  })"
                         )),
                         strong(p("Invalid Example:")),
                         textOutput("repeatedMeasureTextInvalid"),
                         plotOutput("repeatedMeasureImageInvalid"),
                         tags$script(HTML(
                           "$(document).ready(function() {
                                  document.getElementById('repeatedMeasureImageInvalid').setAttribute('aria-label',
                                  `This plot output is depend on the user's choice.
                                  For normality, this is a plot of norm qunatile with 2 points lay in the 97% confidence envelope.
                                  For homoscedasticity, this is a plot of response versus explanatory, all the points lay in a pattern.
                                  For independence, this is a plot of response versus index, all the points lay in a pattern.
                                  For interaction, points in the different block have a different trend.
                                  For random, this is a plot of norm qunatile with 1 point laid in the 80% confidence envelope.`)
                                  })"
                         )
                         )
                       )
              )
            )
          )
        ),

        #### Set up a Game1 Page
        tabItem(
          tabName = "game1",
          withMathJax(),
          h2("Practice/Test Yourself with the drag and drop matching game"),
          ##### Set up ANOVA game1
          tabsetPanel(
            tabPanel(
              "ANOVA",
              fluidRow(
                bucket_list(
                  header = "Pick the assumptions for ANOVA",
                  add_rank_list(
                    text = "Drag assumptions from here",
                    input_id = "dragAnova",
                    labels = c("Normality",
                               "Homoscedasticity",
                               "Independence of Observation",
                               "Linear relationship covarite and the response",
                               "Homoscedasticity of the covarite's slope parametar",
                               "No statistically significiant potential outliers",
                               "Interaction of block and treatment",
                               "Random effect")
                  ),
                  add_rank_list(
                    text = "to here",
                    input_id = "dropAnova"
                  )
                )
              ),
              div(style = "text-align:left",
                  fluidRow(
                    column(
                      6,
                      bsButton(
                        inputId = 'submitAnova',
                        label = "Submit",
                        size = "medium",
                        style = "warning",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markAnova')
                    )
                  )
              )

            ),
            ##### Set up ANCOVA game1
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
                               "Linear relationship covarite and the response",
                               "Homoscedasticity of the covarite's slope parametar",
                               "No statistically significiant potential outliers",
                               "Interaction of block and treatment",
                               "Random effect")
                  ),
                  add_rank_list(
                    text = "to here",
                    input_id = "dropAncova"
                  )
                )
              ),
              div(style = "text-align:left",
                  fluidRow(
                    column(
                      6,
                      bsButton(
                        inputId = 'submitAncova',
                        label = "Submit",
                        size = "medium",
                        style = "warning",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markAncova')
                    )
                  )
              )
            ),
            ##### Set up blocking game1
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
                               "Linear relationship covarite and the response",
                               "Homoscedasticity of the covarite's slope parametar",
                               "No statistically significiant potential outliers",
                               "Interaction of block and treatment",
                               "Random effect")
                  ),
                  add_rank_list(
                    text = "to here",
                    input_id = "dropBlocking"
                  )
                )
              ),
              div(style = "text-align:left",
                  fluidRow(
                    column(
                      6,
                      bsButton(
                        inputId = 'submitBlocking',
                        label = "Submit",
                        size = "medium",
                        style = "warning",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markBlocking')
                    )
                  )
              )
            ),
            ##### Set up random effect game1
            tabPanel(
              "Random Effect",
              fluidRow(
                bucket_list(
                  header = "Pick the assumptions for random effect",
                  add_rank_list(
                    text = "Drag assumptions from here",
                    input_id = "dragRandomEffect",
                    labels = c("Normality",
                               "Homoscedasticity",
                               "Independence of Observation",
                               "Linear relationship covarite and the response",
                               "Homoscedasticity of the covarite's slope parametar",
                               "No statistically significiant potential outliers",
                               "Interaction of block and treatment",
                               "Random effect")
                  ),
                  add_rank_list(
                    text = "to here",
                    input_id = "dropRandomEffect"
                  )
                )
              ),
              div(style = "text-align:left",
                  fluidRow(
                    column(
                      6,
                      bsButton(
                        inputId = 'submitRandomEffect',
                        label = "Submit",
                        size = "medium",
                        style = "warning",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markRandomEffect')
                    )
                  )
              )
            ),
            ##### Set up repeated measure game1
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
                               "Linear relationship covarite and the response",
                               "Homoscedasticity of the covarite's slope parametar",
                               "No statistically significiant potential outliers",
                               "Interaction of block and treatment",
                               "Random effect")
                  ),
                  add_rank_list(
                    text = "to here",
                    input_id = "dropRepeatedMeasure"
                  )
                )
              ),
              div(style = "text-align:left",
                  fluidRow(
                    column(
                      6,
                      bsButton(
                        inputId = 'submitRepeatedMeasure',
                        label = "Submit",
                        size = "medium",
                        style = "warning",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markRepeatedMeasure')
                    )
                  )
              )
            )
          )
        ),
        #### Set up Game 2 Page
        tabItem(
          tabName = "game2",
          withMathJax(),
          h2("Choose the plot which violates the assumption"),
          tabsetPanel(
            ##### Set up page for normality game2
            tabPanel(
              "Normality",
              fluidRow(
                column(
                  4,
                  plotOutput('normalityGamePlot1'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('normalityGamePlot1').setAttribute('aria-label',
                    `This is a plot of normal quantiles. There are 50 points in the 95% confidence envelope`)
                    })"
                  ))
                ),
                column(
                  4,
                  plotOutput('normalityGamePlot2'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('normalityGamePlot2').setAttribute('aria-label',
                    `This is a plot of normal quantiles. There are 50 points in the 95% confidence envelope`)
                    })"
                  ))
                ),
                column(
                  4,
                  plotOutput('normalityGamePlot3'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('normalityGamePlot3').setAttribute('aria-label',
                    `This is a plot of normal quantiles. There are 50 points, most of the points lied in the 95% confidence envelope, while others not`)
                    })"
                  ))
                )),
              fluidRow(
                column(
                  12,
                  selectInput(
                    "nomalitySelected",
                    label = 'Your choice',
                    choices = list(
                      "Plot A" = "plotA",
                      "Plot B" = "plotB",
                      "Plot C" = "plotC")
                  )
                )
              ),
              fluidRow(
                div(style = "text-align:middle",
                    column(
                      6,
                      bsButton(
                        inputId = 'submitNormality',
                        label = "Submit",
                        size = "medium",
                        style = "warning",
                        disabled = FALSE)
                    )
                ),
                column(
                  6,
                  uiOutput('markNormality')
                )
              )
            ),
            ##### Set up page for homoscedasticity game2
            tabPanel(
              "Homoscedasticity",
              fluidRow(
                column(
                  4,
                  plotOutput('homoGamePlot1'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('homoGamePlot1').setAttribute('aria-label',
                    `This is a plot of response versus explanatory, all the points lay in a random position.`)
                    })"
                  ))
                ),
                column(
                  4,
                  plotOutput('homoGamePlot2'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('homoGamePlot2').setAttribute('aria-label',
                    `This is a plot of response versus explanatory, all the points lay in a random position.`)
                    })"
                  ))
                ),
                column(
                  4,
                  plotOutput('homoGamePlot3'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('homoGamePlot3').setAttribute('aria-label',
                    `This is a plot of response versus explanatory, all the points lay in a pattern.`)
                    })"
                  ))
                )
              ),
              fluidRow(
                column(
                  4,
                  selectInput(
                    "homoSelected",
                    label = 'Your choice',
                    choices = list(
                      "Plot A" = "plotA",
                      "Plot B" = "plotB",
                      "Plot C" = "plotC")
                  )
                )
              ),
              fluidRow(
                div(style = "text-align:middle",
                    column(
                      6,
                      bsButton(
                        inputId = 'submitHomo',
                        label = "Submit",
                        size = "medium",
                        style = "warning",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markHomo')
                    )
                )
              )
            ),
            ##### Set up page for independence game2
            tabPanel(
              "Independence of observation",
              fluidRow(
                column(
                  4,
                  plotOutput('indeGamePlot1'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('indeGamePlot1').setAttribute('aria-label',
                    `This is a plot of response versus index, all the points lay in a random position.`)
                    })"
                  ))
                ),
                column(
                  4,
                  plotOutput('indeGamePlot2'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('indeGamePlot2').setAttribute('aria-label',
                    `This is a plot of response versus index, all the points lay in a random position.`)
                    })"
                  ))
                ),
                column(
                  4,
                  plotOutput('indeGamePlot3'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('indeGamePlot3').setAttribute('aria-label',
                    `This is a plot of response versus index, all the points lay in a pattern.`)
                    })"
                  ))
                )
              ),
              fluidRow(
                column(
                  4,
                  selectInput(
                    "indeSelected",
                    label = 'Your choice',
                    choices = list(
                      "Plot A" = "plotA",
                      "Plot B" = "plotB",
                      "Plot C" = "plotC"))
                )
              ),
              fluidRow(
                div(style = "text-align:middle",
                    column(
                      6,
                      bsButton(
                        inputId = 'submitInde',
                        label = "Submit",
                        size = "medium",
                        style = "warning",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markInde')
                    )
                )
              )
            ),
            ##### Set up page for Linear game2
            tabPanel(
              "Linear",
              fluidRow(
                column(
                  4,
                  plotOutput('linearGamePlot1'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('linearGamePlot1').setAttribute('aria-label',
                    `The plot shows that response and explantory have a linear relationship.`)
                    })"
                  ))
                ),
                column(
                  4,
                  plotOutput('linearGamePlot2'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('linearGamePlot2').setAttribute('aria-label',
                    `The plot shows that response and explantory have a linear relationship.`)
                    })"
                  ))
                ),
                column(
                  4,
                  plotOutput('linearGamePlot3'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('linearGamePlot3').setAttribute('aria-label',
                    `The plot shows that response and explantory have no linear relationship.`)
                    })"
                  ))
                )
              ),
              fluidRow(
                column(
                  4,
                  selectInput(
                    "linearSelected",
                    label = 'Your choice',
                    choices = list(
                      "Plot A" = "plotA",
                      "Plot B" = "plotB",
                      "Plot C" = "plotC"))
                )
              ),
              fluidRow(
                div(style = "text-align:middle",
                    column(
                      6,
                      bsButton(
                        inputId = 'submitLinear',
                        label = "Submit",
                        size = "medium",
                        style = "warning",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markLinear')
                    )
                )
              )
            ),
            ##### Set up page for homoscedasticity slope game2
            tabPanel(
              "Homoscedasticity slope",
              fluidRow(
                column(
                  4,
                  plotOutput('slopeGamePlot1'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('slopeGamePlot1').setAttribute('aria-label',
                    `In the plot, points in the different groups have a similar trend.`)
                    })"
                  ))
                ),
                column(
                  4,
                  plotOutput('slopeGamePlot2'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('slopeGamePlot2').setAttribute('aria-label',
                    `In the plot, points in the different groups have a similar trend.`)
                    })"
                  ))
                ),
                column(
                  4,
                  plotOutput('slopeGamePlot3'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('slopeGamePlot3').setAttribute('aria-label',
                    `In the plot, points in the different groups have a different trend`)
                    })"
                  ))
                )
              ),
              fluidRow(
                column(
                  4,
                  tags$style(HTML(".radio-inline {margin-right: 472px;}")),
                  selectInput(
                    "slopeSelected",
                    label = 'Your choice',
                    choices = list(
                      "Plot A" = "plotA",
                      "Plot B" = "plotB",
                      "Plot C" = "plotC")
                  )
                )
              ),
              fluidRow(
                div(style = "text-align:middle",
                    column(
                      6,
                      bsButton(
                        inputId = 'submitSlope',
                        label = "Submit",
                        size = "medium",
                        style = "warning",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markSlope')
                    )
                )
              )
            ),
            ##### Set up page for potential outliers game2
            tabPanel(
              "No potential outliers",
              fluidRow(
                column(
                  4,
                  plotOutput('outGamePlot1'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('outGamePlot1').setAttribute('aria-label',
                    `The plot shows that the data do not have obvious outliers`)
                    })"
                  ))
                ),
                column(
                  4,
                  plotOutput('outGamePlot2'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('outGamePlot2').setAttribute('aria-label',
                    `The plot shows that the data do not have obvious outliers`)
                    })"
                  ))
                ),
                column(
                  4,
                  plotOutput('outGamePlot3'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('outGamePlot3').setAttribute('aria-label',
                    `The plot shows that the data has obvious outliers`)
                    })"
                  ))
                )
              ),
              fluidRow(
                column(
                  4,
                  selectInput(
                    "outSelected",
                    label = 'Your choice',
                    choices = list(
                      "Plot A" = "plotA",
                      "Plot B" = "plotB",
                      "Plot C" = "plotC")
                  )
                )
              ),
              fluidRow(
                div(style = "text-align:middle",
                    column(
                      6,
                      bsButton(
                        inputId = 'submitOut',
                        label = "Submit",
                        size = "medium",
                        style = "warning",
                        disabled = FALSE)),
                    column(
                      6,
                      uiOutput('markOut')
                    )
                )
              )
            ),
            ##### Set up page for interaction of block game2
            tabPanel(
              "Interaction of block",
              fluidRow(
                column(
                  4,
                  plotOutput('interGamePlot1'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('interGamePlot1').setAttribute('aria-label',
                    `In the plot, points in the different block have a similar trend.`)
                    })"
                  ))
                ),
                column(
                  4,
                  plotOutput('interGamePlot2'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('interGamePlot2').setAttribute('aria-label',
                    `In the plot, points in the different block have a similar trend.`)
                    })"
                  ))
                ),
                column(
                  4,
                  plotOutput('interGamePlot3'),
                  tags$script(HTML(
                    "$(document).ready(function() {
                    document.getElementById('interGamePlot3').setAttribute('aria-label',
                    `In the plot, points in the different block have a different trend`)
                    })"
                  ))
                )
              ),
              fluidRow(
                column(
                  4,
                  selectInput(
                    "interSelected",
                    label = 'Your choice',
                    choices = list(
                      "Plot A" = "plotA",
                      "Plot B" = "plotB",
                      "Plot C" = "plotC")
                  )
                )
              ),
              fluidRow(
                div(style = "text-align:middle",
                    column(
                      6,
                      bsButton(
                        inputId = 'submitInter',
                        label = "Submit",
                        size = "medium",
                        style = "warning",
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
        #### Set up the References Page-REQUIRED
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
            "Hatfield, N. J. (2019). Caveats of NHST. [Web App]. Available from
            https://github.com/EducationShinyAppTeam/Significance_Testing_Caveats
            /tree/PedagogicalUpdate1"
          ),
          p(
            class = "hangingindent",
            "Wickham, W. (2016). ggplot2: Elegant graphics for data analysis.
            [R Package]. Springer-Verlag New York. Available from
            https://ggplot2.tidyverse.org"
          ),
          p(
            class = "hangingindent",
            "John Fox and Sanford Weisberg (2019). An {R} Companion to Applied
            Regression, Third Edition. Thousand Oaks CA: Sage. Avaliable from:
            https://socialsciences.mcmaster.ca/jfox/Books/Companion/"
          ),
          p(
            class = "hangingindent",
            "Andrie de Vries, Barret Schloerke and Kenton Russell (2019).
            sortable: Drag-and-Drop in 'shiny' Apps with 'SortableJS'. R package
            version 0.4.2. Avaliable from: https://CRAN.R-project.org/package=sortable"
          ),
          p(
            class = "hangingindent",
            "Victor Perrier, Fanny Meyer and David Granjon (2020). shinyWidgets:
            Custom Inputs Widgets for Shiny. R package version 0.5.3. Avaliable from:
            https://CRAN.R-project.org/package=shinyWidgets"
          )
        )
      )
    )
  )
)

# Define the server ----
server <- function(input, output, session) {
  ## Set Up "GO" Button
  observeEvent(input$explore, {
    updateTabItems(session, "tabs", "prerequisites")
  })

  ## Define what each button does
  observeEvent(input$go1, {
    updateTabItems(session, "tabs", "Explore")
  })

  output$anovaTextValid <- renderText({
    if (input$anovaSelect == "normality"){
      paste("In this plot, the boundary line should envelop almost all the points in the graph")
    }
    else if (input$anovaSelect == "homoscedasticity"){
      paste("The Points here should randomly lay between the line")
    }
    else if (input$anovaSelect == "independence"){
      paste("The Points in this graph should have no pattern")
    }
  })

  output$anovaImageValid <- renderPlot({
    honey <- data.frame(
      Surplus = c(100, 60, 90, 85, 90, 95, 105, 70, 80),
      Varietal = c(rep("Clover", 3), rep("Orange Blossom", 3), rep("Alfalfa", 3))
    )
    if (input$anovaSelect == "normality"){
      car::qqPlot(
        x = honey$Surplus,
        distribution = "norm",
        envelope = 0.97,
        ylab = "Surplus Honey (lbs)"
      )
    }
    else if (input$anovaSelect == "homoscedasticity"){
      stripchart(Surplus ~ Varietal,
                 vertical = TRUE,
                 pch = 20,
                 data = honey)
    }
    else if (input$anovaSelect == "independence"){
      plot(honey$Surplus, type = "b", ylab = "Surplus Honey (lbs)")
    }
  })

  output$anovaTextInValid <- renderText({
    if (input$anovaSelect == "normality"){
      paste("In this plot, some points is located outside of the envelop")
    }
    else if (input$anovaSelect == "homoscedasticity"){
      paste("The points in this graph tend to have a pattern")
    }
    else if (input$anovaSelect == "independence"){
      paste("The points in this graph tend to have a pattern")
    }
  })

  output$anovaImageInValid <- renderPlot({
    honey <- data.frame(
      Surplus = c(50, 40, 55, 85, 80, 82, 105, 180, 192),
      Varietal = c(rep("Clover", 3), rep("Orange Blossom", 3), rep("Alfalfa", 3))
    )
    if (input$anovaSelect == "normality"){
      car::qqPlot(
        x = honey$Surplus,
        distribution = "norm",
        envelope = 0.97,
        ylab = "Surplus Honey (lbs)"
      )
    }
    else if (input$anovaSelect == "homoscedasticity"){
      stripchart(Surplus ~ Varietal,
                 vertical = TRUE,
                 pch = 20,
                 data = honey)
    }
    else if (input$anovaSelect == "independence"){
      plot(honey$Surplus, type = "b", ylab = "Surplus Honey (lbs)")
    }
  })

  output$ancovaTextValid <- renderText({
    if (input$ancovaSelect == "normality"){
      paste("In this plot, the boundary line should envelop almost all the points in the graph")
    }
    else if (input$ancovaSelect == "homoscedasticity"){
      paste("The Points here should randomly lay between the line")
    }
    else if (input$ancovaSelect == "independence"){
      paste("The Points in this graph should have no pattern")
    }
    else if (input$ancovaSelect == "linear"){
      paste("By the graph, we expect to see a linear relationship between covariate and response")
    }
    else if (input$ancovaSelect == "slope"){
      paste("The different covarite represent by different colors here should have homogeneity slope")
    }
    else if (input$ancovaSelect == "outlier"){
      paste("We expected no visualized outliers")
    }
  })


  output$ancovaImageValid <- renderPlot({
    keyboarding <- data.frame(
      kbd.type = c(rep("1", 4), rep("2", 4), rep("3", 4)),
      hrs.kbd = c(60, 72, 61, 50, 54, 68, 66, 59, 56, 56, 55, 51),
      hrs.pain = c(85, 95, 69, 58, 41, 74, 71, 52, 41, 34, 50, 40)
    )
    if (input$ancovaSelect == "normality"){
      car::qqPlot(
        x = keyboarding$hrs.pain,
        distribution = "norm",
        envelope = 0.97,
        ylab = "Hours of Pain"
      )
    }
    else if (input$ancovaSelect == "homoscedasticity"){
      stripchart(hrs.pain ~ kbd.type,
                 vertical = TRUE,
                 pch = 20,
                 data = keyboarding)
    }
    else if (input$ancovaSelect == "independence"){
      plot(keyboarding$hrs.pain, type = "b", ylab = "Hours of Pain")
    }
    else if (input$ancovaSelect == "linear"){
      ggplot2::ggplot(data = keyboarding,
                      mapping = ggplot2::aes(
                        y = hrs.pain,
                        x = hrs.kbd,
                        group = kbd.type,
                        color = kbd.type
                      )) +
        ggplot2::geom_point(size = 3) +
        ggplot2::theme_bw() +
        xlab("Hours Spent Keyboarding") +
        ylab("Hours of Pain") +
        labs(color = "Keyboard Type")
    }
    else if (input$ancovaSelect == "slope"){
      ggplot2::ggplot(data = keyboarding,
                      mapping = ggplot2::aes(
                        y = hrs.pain,
                        x = hrs.kbd,
                        group = kbd.type,
                        color = kbd.type
                      )) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_smooth(method = "lm", se = FALSE) +
        ggplot2::theme_bw() +
        xlab("Hours Spent Keyboarding") +
        ylab("Hours of Pain") +
        labs(color = "Keyboard Type")
    }
    else if (input$ancovaSelect == "outlier"){
      key2 <- rstatix::mahalanobis_distance(keyboarding)
      key2 <- cbind(key2, factor = keyboarding$kbd.type)
      ggplot2::ggplot(data = key2,
                      mapping = ggplot2::aes(
                        y = hrs.pain,
                        x = hrs.kbd,
                        color = factor
                      )) +
        ggplot2::geom_point(size = 3) +
        ggplot2::theme_bw() +
        xlab("Hours Spent Keyboarding") +
        ylab("Hours of Pain") +
        labs(color = "Keyboard", shape = "Potential Outlier")
    }
  })

  output$ancovaTextInValid <- renderText({
    if (input$ancovaSelect == "normality"){
      paste("In this plot, some points is located outside of the envelop")
    }
    else if (input$ancovaSelect == "homoscedasticity"){
      paste("The points in this graph tend to have a pattern")
    }
    else if (input$ancovaSelect == "independence"){
      paste("The points in this graph tend to have a pattern")
    }
    else if (input$ancovaSelect == "linear"){
      paste("There is no linear relationship between covariate and response in the graph")
    }
    else if (input$ancovaSelect == "slope"){
      paste("The different covarite represent by different colors here have no homogeneity slope")
    }
    else if (input$ancovaSelect == "outlier"){
      paste("There are some visualized outliers in the plot")
    }
  })

  output$ancovaImageInValid <- renderPlot({
    keyboarding <- data.frame(
      kbd.type = c(rep("1", 4), rep("2", 4), rep("3", 4)),
      hrs.kbd = c(60, 72, 61, 50, 54, 68, 66, 59, 56, 56, 55, 29),
      hrs.pain = c(190, 200, 69, 58, 41, 74, 71, 52, 4, 2, 3, 41)
    )
    if (input$ancovaSelect == "normality"){
      car::qqPlot(
        x = keyboarding$hrs.pain,
        distribution = "norm",
        envelope = 0.97,
        ylab = "Hours of Pain"
      )
    }
    else if (input$ancovaSelect == "homoscedasticity"){
      stripchart(hrs.pain ~ kbd.type,
                 vertical = TRUE,
                 pch = 20,
                 data = keyboarding)
    }
    else if (input$ancovaSelect == "independence"){
      plot(keyboarding$hrs.pain, type = "b", ylab = "Hours of Pain")
    }
    else if (input$ancovaSelect == "linear"){
      ggplot2::ggplot(data = keyboarding,
                      mapping = ggplot2::aes(
                        y = hrs.pain,
                        x = hrs.kbd,
                        group = kbd.type,
                        color = kbd.type
                      )) +
        ggplot2::geom_point(size = 3) +
        ggplot2::theme_bw() +
        xlab("Hours Spent Keyboarding") +
        ylab("Hours of Pain") +
        labs(color = "Keyboard Type")
    }
    else if (input$ancovaSelect == "slope"){
      ggplot2::ggplot(data = keyboarding,
                      mapping = ggplot2::aes(
                        y = hrs.pain,
                        x = hrs.kbd,
                        group = kbd.type,
                        color = kbd.type
                      )) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_smooth(method = "lm", se = FALSE) +
        ggplot2::theme_bw() +
        xlab("Hours Spent Keyboarding") +
        ylab("Hours of Pain") +
        labs(color = "Keyboard Type")
    }
    else if (input$ancovaSelect == "outlier"){
      key2 <- rstatix::mahalanobis_distance(keyboarding)
      key2 <- cbind(key2, factor = keyboarding$kbd.type)
      ggplot2::ggplot(data = key2,
                      mapping = ggplot2::aes(
                        y = hrs.pain,
                        x = hrs.kbd,
                        color = factor
                      )) +
        ggplot2::geom_point(size = 3) +
        ggplot2::theme_bw() +
        xlab("Hours Spent Keyboarding") +
        ylab("Hours of Pain") +
        labs(color = "Keyboard", shape = "Potential Outlier")
    }
  })

  output$blockingTextValid <- renderText({
    if (input$blockingSelect == "normality"){
      paste("In this plot, the boundary line should envelop almost all the points in the graph")
    }
    else if (input$blockingSelect == "homoscedasticity"){
      paste("The Points here should randomly lay between the line")
    }
    else if (input$blockingSelect == "independence"){
      paste("The Points in this graph should have no pattern")
    }
    else if (input$blockingSelect == "interaction"){
      paste("The data in different group should have similar pattern")
    }
  })

  output$blockingImageValid <- renderPlot({
    barley <- read.csv("blockingValid.csv", header = TRUE)
    barleyModel <- aov(Yield ~ Treatment + Field, data = barley)
    if (input$blockingSelect == "normality"){
      car::qqPlot(
        x = barleyModel$residuals,
        distribution = "norm",
        envelope = 0.97,
        ylab = "Yield (bushels per arce)",
        pch = 19
      )
    }
    else if (input$blockingSelect == "homoscedasticity"){
      stripchart(Yield ~ Treatment,
                 vertical = TRUE,
                 pch = 20,
                 data = barley)
    }
    else if (input$blockingSelect == "independence"){
      plot(barley$Yield, type = "b", ylab = "Yield (bushels per acre)")
    }
    else if (input$blockingSelect == "interaction"){
      ggplot2::ggplot(data = barley,
                      mapping = aes(x = Treatment,
                                    y = Yield,
                                    color = Field,
                                    group = Field)) +
        ggplot2::geom_point(size=2) +
        ggplot2::geom_line(size=1) +
        ggplot2::theme_bw() +
        xlab("Variety") +
        ylab("Yield (bushels per acre)") +
        labs(color = "Field")
    }
  })

  output$blockingTextInvalid <- renderText({
    if (input$blockingSelect == "normality"){
      paste("In this plot, some points is located outside of the envelop")
    }
    else if (input$blockingSelect == "homoscedasticity"){
      paste("The points in this graph tend to have a pattern")
    }
    else if (input$blockingSelect == "independence"){
      paste("The points in this graph tend to have a pattern")
    }
    else if (input$blockingSelect == "interaction"){
      paste("The data in different group should have different pattern.")
    }
  })

  output$blockingImageInvalid <- renderPlot({
    barley <- read.csv("blockingInvalid.csv", header = TRUE)
    barleyModel <- aov(Yield ~ Treatment + Field, data = barley)
    if (input$blockingSelect == "normality"){
      car::qqPlot(
        x = barleyModel$residuals,
        distribution = "norm",
        envelope = 0.5,
        ylab = "Yield (bushels per arce)",
        pch = 19
      )
    }
    else if (input$blockingSelect == "homoscedasticity"){
      stripchart(Yield ~ Treatment,
                 vertical = TRUE,
                 pch = 20,
                 data = barley)
    }
    else if (input$blockingSelect == "independence"){
      plot(barley$Yield, type = "b", ylab = "Yield (bushels per acre)")
    }
    else if (input$blockingSelect == "interaction"){
      ggplot2::ggplot(data = barley,
                      mapping = aes(x = Treatment,
                                    y = Yield,
                                    color = Field,
                                    group = Field)) +
        ggplot2::geom_point(size=2) +
        ggplot2::geom_line(size=1) +
        ggplot2::theme_bw() +
        xlab("Variety") +
        ylab("Yield (bushels per acre)") +
        labs(color = "Field")
    }
  })

  output$randomEffectTextValid <- renderText({
    if (input$randomEffectSelect == "normality"){
      paste("In this plot, the boundary line should envelop almost all the points in the graph.")
    }
    else if (input$randomEffectSelect == "homoscedasticity"){
      paste("The Points here should randomly lay between the line.")
    }
    else if (input$randomEffectSelect == "independence"){
      paste("The Points in this graph should have no pattern.")
    }
    else if (input$randomEffectSelect == "random"){
      paste("In this plot, the boundary line should envelop almost all the points in the graph.")
    }
  })

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
      score ~ (1|officer),
      data = apex,
      REML = TRUE)
    if (input$randomEffectSelect == "normality"){
      car::qqPlot(
        x = resid(apexRE),
        distribution = "norm",
        envelope = 0.92,
        ylab = "score",
        pch = 20,
        main = "Residuals"
      )
    }
    else if (input$randomEffectSelect == "homoscedasticity"){
      plot(apexRE,
           pch = 20,
           xlab = "Fitted Values",
           ylab = "Residuals")
    }
    else if (input$randomEffectSelect == "independence"){
      plot(apex$score, type = "b", ylab = "Score")
    }
    else if (input$randomEffectSelect == "random"){
      car::qqPlot(
        x = lme4::ranef(apexRE)$officer[, "(Intercept)"],
        distribution = "norm",
        envelope = 0.92,
        ylab = "score",
        pch = 20,
        main = "Random Effects"
      )
    }
  })

  output$randomEffectTextInvalid <- renderText({
    if (input$randomEffectSelect == "normality"){
      paste("In this plot, some points is located outside of the envelop")
    }
    else if (input$randomEffectSelect == "homoscedasticity"){
      paste("The points in this graph tend to have a pattern")
    }
    else if (input$randomEffectSelect == "independence"){
      paste("The points in this graph tend to have a pattern")
    }
    else if (input$randomEffectSelect == "random"){
      paste("In this plot, some points is located outside of the envelop")
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
      score ~ (1|officer),
      data = apex,
      REML = TRUE)
    if (input$randomEffectSelect == "normality"){
      car::qqPlot(
        x = resid(apexRE),
        distribution = "norm",
        envelope = 0.92,
        ylab = "score",
        pch = 20,
        main = "Residuals"
      )
    }
    else if (input$randomEffectSelect == "homoscedasticity"){
      plot(apexRE,
           pch = 20,
           xlab = "Fitted Values",
           ylab = "Residuals")
    }
    else if (input$randomEffectSelect == "independence"){
      plot(apex$score, type = "b", ylab = "Score")
    }
    else if (input$randomEffectSelect == "random"){
      car::qqPlot(
        x = lme4::ranef(apexRE)$officer[, "(Intercept)"],
        distribution = "norm",
        envelope = 0.8,
        ylab = "score",
        pch = 20,
        main = "Random Effects"
      )
    }
  })

  output$repeatedMeasureTextValid <- renderText({
    if (input$repeatedMeasureSelect == "normality"){
      paste("In this plot, the boundary line should envelop almost all the points in the graph.")
    }
    else if (input$repeatedMeasureSelect == "homoscedasticity"){
      paste("The Points here should randomly lay between the line.")
    }
    else if (input$repeatedMeasureSelect == "independence"){
      paste("The Points in this graph should have no pattern.")
    }
    else if (input$repeatedMeasureSelect == "interaction"){
      paste("The data in different group should have same pattern.")
    }
    else if (input$randomEffectSelect == "random"){
      paste("In this plot, the boundary line should envelop almost all the points in the graph.")
    }
  })

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
    beerM1 <- lme4::lmer(score ~ beer + (1|judge), data = beer)
    if (input$repeatedMeasureSelect == "normality"){
      car::qqPlot(
        x = residuals(beerM1),
        distribution = "norm",
        envelope = 0.9,
        ylab = "Score",
        pch = 19
      )
    }
    else if (input$repeatedMeasureSelect == "homoscedasticity"){
      plot(beerM1, which = 1, pch = 19)
    }
    else if (input$repeatedMeasureSelect == "independence"){
      plot(beer$score, type = "b", ylab = "Score")
    }
    else if (input$repeatedMeasureSelect == "interaction"){
      ggplot2::ggplot(data = beer,
                      mapping = aes(x = beer,
                                    y = score,
                                    color = judge,
                                    group = judge)) +
        ggplot2::geom_point(size=2) +
        ggplot2::geom_line(size=1) +
        ggplot2::theme_bw() +
        viridis::scale_color_viridis(discrete = TRUE, option = "viridis") +
        xlab("Beer") +
        ylab("Score") +
        labs(color = "Judge")
    }
    else if (input$repeatedMeasureSelect == "random"){
      car::qqPlot(
        x = lme4::ranef(beerM1)$judge[, "(Intercept)"],
        distribution = "norm",
        envelope = 0.9,
        ylab = "score",
        pch = 20
      )
    }
  })

  output$repeatedMeasureTextInvalid <- renderText({
    if (input$repeatedMeasureSelect == "normality"){
      paste("In this plot, some points is located outside of the envelop")
    }
    else if (input$repeatedMeasureSelect == "homoscedasticity"){
      paste("The points in this graph tend to have a pattern")
    }
    else if (input$repeatedMeasureSelect == "independence"){
      paste("The points in this graph tend to have a pattern")
    }
    else if (input$repeatedMeasureSelect == "random"){
      paste("In this plot, some points is located outside of the envelop")
    }
    else if (input$repeatedMeasureSelect == "interaction"){
      paste("The data in different group should have different pattern.")
    }
  })

  output$repeatedMeasureImageInvalid <- renderPlot({
    beer <- data.frame(
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
    beerM1 <- lme4::lmer(score ~ beer + (1|judge), data = beer)
    if (input$repeatedMeasureSelect == "normality"){
      car::qqPlot(
        x = residuals(beerM1),
        distribution = "norm",
        envelope = 0.9,
        ylab = "Score",
        pch = 19
      )
    }
    else if (input$repeatedMeasureSelect == "homoscedasticity"){
      plot(beerM1, which = 1, pch = 19)
    }
    else if (input$repeatedMeasureSelect == "independence"){
      plot(beer$score, type = "b", ylab = "Score")
    }
    else if (input$repeatedMeasureSelect == "interaction"){
      ggplot2::ggplot(data = beer,
                      mapping = aes(x = beer,
                                    y = score,
                                    color = judge,
                                    group = judge)) +
        ggplot2::geom_point(size=2) +
        ggplot2::geom_line(size=1) +
        ggplot2::theme_bw() +
        viridis::scale_color_viridis(discrete = TRUE, option = "viridis") +
        xlab("Beer") +
        ylab("Score") +
        labs(color = "Judge")
    }
    else if (input$repeatedMeasureSelect == "random"){
      car::qqPlot(
        x = lme4::ranef(beerM1)$judge[, "(Intercept)"],
        distribution = "norm",
        envelope = 0.9,
        ylab = "score",
        pch = 20
      )
    }
  })

  observeEvent(input$submit, {
    updateButton(session, "submitAnova", disabled = TRUE)
  })

  observeEvent(input$submitAnova,{
    output$markAnova <- renderUI({
      if ("Normality" %in% input$dropAnova &&
          "Homoscedasticity" %in% input$dropAnova &&
          "Independence of Observation" %in% input$dropAnova &&
          length(input$dropAnova) == 3){
        img(src = "check.png",width = 30)
      }
      else{
        img(src = "cross.png",width = 30)
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
          "Linear relationship covarite and the response" %in% input$dropAncova &&
          "Homoscedasticity of the covarite's slope parametar" %in% input$dropAncova &&
          "No statistically significiant potential outliers" %in% input$dropAncova &&
          length(input$dropAncova) == 6){
        img(src = "check.png",width = 30)
      }
      else{
        img(src = "cross.png",width = 30)
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
        img(src = "check.png",width = 30)
      }
      else{
        img(src = "cross.png",width = 30)
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
          "Random effect" %in% input$dropRandomEffect &&
          length(input$dropRandomEffect) == 4){
        img(src = "check.png",width = 30)
      }
      else{
        img(src = "cross.png",width = 30)
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
          "Random effect" %in% input$dropRepeatedMeasure &&
          "Interaction of block and treatment" %in% input$dropRepeatedMeasure &&
          length(input$dropRepeatedMeasure) == 5){
        img(src = "check.png",width = 30)
      }
      else{
        img(src = "cross.png",width = 30)
      }
    })
  })

  normalityData1 <- rnorm(n = 50, mean = 0, sd = 1)
  normalityData2 <- rnorm(n = 50, mean = 0, sd = 1)
  normalityData3 <-  rbeta(50, 0.7, 1.5)

  output$normalityGamePlot1 <- renderPlot({
    car::qqPlot(
      x = normalityData1,
      distribution = "norm",
      envelope = 0.95,
      ylab = "data",
      main = "Plot A",
      pch = 19
    )
  })

  output$normalityGamePlot2 <- renderPlot({
    car::qqPlot(
      x = normalityData2,
      distribution = "norm",
      envelope = 0.95,
      ylab = "data",
      main = "Plot B",
      pch = 19
    )
  })

  output$normalityGamePlot3 <- renderPlot({
    car::qqPlot(
      x = normalityData3,
      distribution = "norm",
      envelope = 0.8,
      ylab = "data",
      main = "Plot C",
      pch = 19
    )
  })

  observeEvent(input$submitNormality,{
    output$markNormality <- renderUI({
      if (input$nomalitySelected == 'plotC'){
        img(src = "check.png",width = 90)
      }
      else{
        img(src = "cross.png",width = 90)
      }
    })
  })

  observeEvent(input$submit, {
    updateButton(session, "submitNormality", disabled = TRUE)
  })

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

  output$homoGamePlot1 <- renderPlot({
    stripchart(homoData1_2 ~ homoData1_1,
               vertical = TRUE,
               pch = 19,
               data = homoData1,
               xlab = 'Index',
               ylab = 'homoData1')
  })

  output$homoGamePlot2 <- renderPlot({
    stripchart(homoData2_2 ~ homoData2_1,
               vertical = TRUE,
               pch = 19,
               data = homoData2,
               xlab = 'Index',
               ylab = 'homoData2')
  })

  output$homoGamePlot3 <- renderPlot({
    stripchart(homoData3_2 ~ homoData3_1,
               vertical = TRUE,
               pch = 19,
               data = homoData3,
               xlab = 'Index',
               ylab = 'homoData3')
  })

  observeEvent(input$submitHomo,{
    output$markHomo <- renderUI({
      if (input$homoSelected == 'plotC'){
        img(src = "check.png",width = 90)
      }
      else{
        img(src = "cross.png",width = 90)
      }
    })
  })

  observeEvent(input$submit, {
    updateButton(session, "submitHomo", disabled = TRUE)
  })

  indeData1 <- rnorm(n = 50, mean = 0, sd = 1)
  indeData2 <- rnorm(n = 50, mean = 0, sd = 1)
  indeData3 <- ts(1:10, frequency = 4, start = c(1959, 2))

  output$indeGamePlot1 <- renderPlot({
    plot(indeData1,
         type = "b",
         main = "Plot A")
  })

  output$indeGamePlot2 <- renderPlot({
    plot(indeData2,
         type = "b",
         main = "Plot B")
  })

  output$indeGamePlot3 <- renderPlot({
    plot(indeData3,
         type = "b",
         main = "Plot C",
         ylab = "Index",
         xlab = 'Index')
  })

  observeEvent(input$submitInde,{
    output$markInde <- renderUI({
      if (input$indeSelected == 'plotC'){
        img(src = "check.png",width = 90)
      }
      else{
        img(src = "cross.png",width = 90)
      }
    })
  })

  observeEvent(input$submit, {
    updateButton(session, "submitInde", disabled = TRUE)
  })

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

  output$linearGamePlot1 <- renderPlot({
    ggplot2::ggplot(data = linearData1,
                    mapping = ggplot2::aes(
                      y = linearData1_2,
                      x = linearData1_3,
                      group = linearData1_1,
                      color = linearData1_1
                    )) +
      ggplot2::geom_point(size = 3) +
      ggplot2::theme_bw() +
      xlab("X") +
      ylab("Y") +
      labs(color = "Type")
  })

  output$linearGamePlot2 <- renderPlot({
    ggplot2::ggplot(data = linearData2,
                    mapping = ggplot2::aes(
                      y = linearData2_2,
                      x = linearData2_3,
                      group = linearData2_1,
                      color = linearData2_1
                    )) +
      ggplot2::geom_point(size = 3) +
      ggplot2::theme_bw() +
      xlab("X") +
      ylab("Y") +
      labs(color = "Type")
  })

  output$linearGamePlot3 <- renderPlot({
    ggplot2::ggplot(data = linearData3,
                    mapping = ggplot2::aes(
                      y = linearData3_2,
                      x = linearData3_3,
                      group = linearData3_1,
                      color = linearData3_1
                    )) +
      ggplot2::geom_point(size = 3) +
      ggplot2::theme_bw() +
      xlab("X") +
      ylab("Y") +
      labs(color = "Type")
  })

  observeEvent(input$submitLinear,{
    output$markLinear <- renderUI({
      if (input$linearSelected == 'plotC'){
        img(src = "check.png",width = 90)
      }
      else{
        img(src = "cross.png",width = 90)
      }
    })
  })

  observeEvent(input$submit, {
    updateButton(session, "submitLinear", disabled = TRUE)
  })

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

  output$slopeGamePlot1 <- renderPlot({
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
      xlab("X") +
      ylab("Y") +
      labs(color = "Type")
  })

  output$slopeGamePlot2 <- renderPlot({
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
      xlab("X") +
      ylab("Y") +
      labs(color = "Type")
  })

  output$slopeGamePlot3 <- renderPlot({
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
      xlab("X") +
      ylab("Y") +
      labs(color = "Type")
  })

  observeEvent(input$submitSlope,{
    output$markSlope <- renderUI({
      if (input$slopeSelected == 'plotC'){
        img(src = "check.png",width = 90)
      }
      else{
        img(src = "cross.png",width = 90)
      }
    })
  })

  observeEvent(input$submit, {
    updateButton(session, "submitSlope", disabled = TRUE)
  })

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

  output$outGamePlot1 <- renderPlot({
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
      xlab("X") +
      ylab("Y") +
      labs(color = "Type", shape = "Potential Outlier")
  })

  output$outGamePlot2 <- renderPlot({
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
      xlab("X") +
      ylab("Y") +
      labs(color = "Type", shape = "Potential Outlier")
  })

  output$outGamePlot3 <- renderPlot({
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
      xlab("X") +
      ylab("Y") +
      labs(color = "Type", shape = "Potential Outlier")
  })

  observeEvent(input$submitOut,{
    output$markOut <- renderUI({
      if (input$outSelected == 'plotC'){
        img(src = "check.png",width = 90)
      }
      else{
        img(src = "cross.png",width = 90)
      }
    })
  })

  observeEvent(input$submit, {
    updateButton(session, "submitOut", disabled = TRUE)
  })

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

  output$interGamePlot1 <- renderPlot({
    ggplot2::ggplot(data = interData1,
                    mapping = aes(x = interData1_2,
                                  y = interData1_3,
                                  color = interData1_1,
                                  group = interData1_1)) +
      ggplot2::geom_point(size=2) +
      ggplot2::geom_line(size=1) +
      ggplot2::theme_bw() +
      xlab("X") +
      ylab("Y") +
      labs(color = "Group")
  })

  output$interGamePlot2 <- renderPlot({
    ggplot2::ggplot(data = interData2,
                    mapping = aes(x = interData2_2,
                                  y = interData2_3,
                                  color = interData2_1,
                                  group = interData2_1)) +
      ggplot2::geom_point(size=2) +
      ggplot2::geom_line(size=1) +
      ggplot2::theme_bw() +
      xlab("X") +
      ylab("Y") +
      labs(color = "Group")
  })

  output$interGamePlot3 <- renderPlot({
    ggplot2::ggplot(data = interData3,
                    mapping = aes(x = interData3_2,
                                  y = interData3_3,
                                  color = interData3_1,
                                  group = interData3_1)) +
      ggplot2::geom_point(size=2) +
      ggplot2::geom_line(size=1) +
      ggplot2::theme_bw() +
      xlab("X") +
      ylab("Y") +
      labs(color = "Group")
  })

  observeEvent(input$submitInter,{
    output$markInter <- renderUI({
      if (input$interSelected == 'plotC'){
        img(src = "check.png",width = 90)
      }
      else{
        img(src = "cross.png",width = 90)
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
      text = "Learned the assumptions of each model and how to exam them, then test yourself with the game",
      type = "info"
    )
  })

  observeEvent(input$explore, {
    updateTabItems(session, "tabs", "prerequisites")
  })

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
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

# Define UI for App

  ## Create the app page
  dashboardPage(
    skin = "black",
    ### Create the app header
    dashboardHeader(
      title = "Assumptions of ANOVA Models", 
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      titleWidth = 250,
      tags$li(class = "dropdown",
              tags$a(href='https://shinyapps.science.psu.edu/',
                     icon("home")))
    ),
    ### Create the sidebar/left navigation menu
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "tabs",
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

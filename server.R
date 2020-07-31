shinyServer(function(input, output, session) {
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
  
  homoData1 <- rnorm(n = 50, mean = 0, sd = 1)
  homoData2 <- rnorm(n = 50, mean = 0, sd = 1)
  homoData3 <- data.frame(
    homoData2_1 = c(rep('1',10), rep('2',10), rep('3',10), rep('4',10), rep('5',10)),
    homoData2_2 = c(sample(1:100, 10, replace=FALSE), 
                      sample(10:90, 10, replace=FALSE), 
                      sample(20:80, 10, replace=FALSE), 
                      sample(30:70, 10, replace=FALSE), 
                      sample(40:60, 10, replace=FALSE)))
  
  output$homoGamePlot1 <- renderPlot({
    plot(homoData1, pch = 19)
  })
  
  output$homoGamePlot2 <- renderPlot({
    plot(homoData2, pch = 19)
  })
  
  output$homoGamePlot3 <- renderPlot({
    stripchart(homoData2_2 ~ homoData2_1,
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
    linearData1_2 = c(sample(1:10, 10, replace=FALSE), 
                      sample(10:20, 10, replace=FALSE), 
                      sample(20:30, 10, replace=FALSE), 
                      sample(30:40, 10, replace=FALSE), 
                      sample(40:50, 10, replace=FALSE)),
    linearData1_3 = c(sample(1:10, 10, replace=FALSE), 
                      sample(10:20, 10, replace=FALSE), 
                      sample(20:30, 10, replace=FALSE), 
                      sample(30:40, 10, replace=FALSE), 
                      sample(40:50, 10, replace=FALSE))
  )
  linearData2 <- data.frame(
    linearData2_1 = rep(c('1', '2', '3', '4', '5'), 10),
    linearData2_2 = c(sample(1:10, 10, replace=FALSE), 
                      sample(10:20, 10, replace=FALSE), 
                      sample(20:30, 10, replace=FALSE), 
                      sample(30:40, 10, replace=FALSE), 
                      sample(40:50, 10, replace=FALSE)),
    linearData2_3 = c(sample(1:10, 10, replace=FALSE), 
                      sample(10:20, 10, replace=FALSE), 
                      sample(20:30, 10, replace=FALSE), 
                      sample(30:40, 10, replace=FALSE), 
                      sample(40:50, 10, replace=FALSE))
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
    slopeData1_2 = c(sample(1:10, 10, replace=FALSE), 
                      sample(10:20, 10, replace=FALSE), 
                      sample(20:30, 10, replace=FALSE), 
                      sample(30:40, 10, replace=FALSE), 
                      sample(40:50, 10, replace=FALSE)),
    slopeData1_3 = c(sample(1:10, 10, replace=FALSE), 
                      sample(10:20, 10, replace=FALSE), 
                      sample(20:30, 10, replace=FALSE), 
                      sample(30:40, 10, replace=FALSE), 
                      sample(40:50, 10, replace=FALSE))
  )
  slopeData2 <- data.frame(
    slopeData2_1 = rep(c('1', '2', '3', '4', '5'), 10),
    slopeData2_2 = c(sample(1:10, 10, replace=FALSE), 
                      sample(10:20, 10, replace=FALSE), 
                      sample(20:30, 10, replace=FALSE), 
                      sample(30:40, 10, replace=FALSE), 
                      sample(40:50, 10, replace=FALSE)),
    slopeData2_3 = c(sample(1:10, 10, replace=FALSE), 
                      sample(10:20, 10, replace=FALSE), 
                      sample(20:30, 10, replace=FALSE), 
                      sample(30:40, 10, replace=FALSE), 
                      sample(40:50, 10, replace=FALSE))
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
                   sample(900:1000, 2, replace=FALSE)),
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
  
})
  

  

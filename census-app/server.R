# server.R

library(maps)
library(mapproj)
library(maptools)
require(dplyr)
require(FNN)

majors = read.csv("data/majors-list.csv") # Used to create major categories
majors <- na.omit(majors)

# Read in the 2012 and 2013 data
md_12 = read.csv('data/ss12pmd_new.csv')
md_13 = read.csv('data/ss13pmd_new.csv')

md_12_data <- md_12 %>%
  dplyr::filter(!is.na(AGEP) & !is.na(FOD1P) & FOD1P > 20 & !is.na(WAGP) & AGEP >= 16) %>%
  dplyr::select(WAGP,
                FOD1P,
                SCHL,
                RAC1P,
                AGEP,
                PUMA,
                SEX)

md_12_data$YEAR <- rep(2012, length(md_12_data$WAGP))

md_13_data <- md_13 %>%
  dplyr::filter(!is.na(AGEP) & !is.na(FOD1P) & FOD1P > 20 & !is.na(WAGP) & AGEP >= 16) %>%
  dplyr::select(WAGP,
                FOD1P,
                SCHL,
                RAC1P,
                AGEP,
                PUMA,
                SEX)

md_13_data$YEAR <- rep(2013, length(md_13_data$WAGP))

md <- as.data.frame(rbind(as.matrix(md_12_data), as.matrix(md_13_data)))

shinyServer(function(input, output) {
  races <- list(
                  "All" = c(1,2,3,4,5,6,7,8,9), 
                  "White" = c(1), 
                  "Black" = c(2),
                  "American Indian" = c(3),
                  "Asian" = c(6),
                  "Pacific Islander" = c(7),
                  "Other" = c(8),
                  "Two or more races" = c(9)
                )
  sexes <- list("All" = c(1,2), "Male" = c(1), "Female" = c(2))
  degree_types <- list(
                        "All" = seq(21,24),
                        "Bachelor's" = c(21), 
                        "Master's" = c(22), 
                        "Professional (Beyond Master's)" = c(23),
                        "Doctorate" = c(24)
                        )
  
  gor=readShapeSpatial('data/tl_2012_24_puma10/tl_2012_24_puma10.shp')
  gor@data$PUMACE10 <- as.integer(names(table(gor@data$PUMACE10)))
 
  obscount <- obsmean <- obssum <- rep(0, length(gor@data$PUMACE10))
  
  census_filter <- function(incomes, race, ages, degree, sex, majorCategory){
    if(majorCategory == "All"){
      codes = as.numeric(as.character(unique(majors$FOD1P)))
    } else {
      codes <- as.numeric(as.character(majors$FOD1P[majors$Major_Category == majorCategory]))
    }
    
    x <- md %>% filter(
            WAGP >= incomes[1] & WAGP <= incomes[2] &
            RAC1P %in% races[[race]] &
            AGEP >= ages[1] & AGEP <= ages[2] & 
            SCHL %in% degree_types[[degree]] &
            SEX %in% sexes[[sex]] &
            FOD1P %in% codes
    )
    return (x)
  }

  output$geogPlot <- renderPlot({
    x <- census_filter(input$incomes, input$race, input$ages, input$degree, input$sex, input$majorCategory)
    if(length(x$WAGP) <= 1){
      output$text1 <- renderText({ 
        "No matching observations found. Try adjusting your parameters"
      })
    } else {
      output$text1 <- renderText({ 
        paste(length(x$WAGP), "Matching observations found.", sep=" ")
      })
      isolated_puma <- as.data.frame(table(x$PUMA))
      
      counter = 1
      for (i in gor@data$PUMACE10) {
        p <- mean(x$WAGP[x$PUMA == i])
        s <- sum(x$WAGP[x$PUMA == i])
        if(length(p) > 0){
          obsmean[counter] = p
          obssum[counter] = s
        } else {
          obsmean[counter] = 0
          obssum[counter] = 0
        }
        n <- isolated_puma$Freq[isolated_puma$Var1 == i]
        if(length(n) > 0){
          obscount[counter] = n
        } else {
          obscount[counter] = 0
        }
        counter = counter + 1
      }
      gor@data$OBSCOUNT <- obscount
      gor@data$OBSMEAN <- obsmean
      gor@data$OBSSUM <- obssum
      if(input$mapDisplay == "Observation Count"){
        type = gor@data$OBSCOUNT
      } else if (input$mapDisplay == "Observation Mean"){
        type = gor@data$OBSMEAN
      } else {
        type = gor@data$OBSSUM
      }
      shades <- colorRampPalette(c("white", "springgreen3"))(100)
      percents <- as.integer(cut(type, 100, 
                                 include.lowest = TRUE, ordered = TRUE))
      fills <- shades[percents]
      plot(gor, col = fills)
    }
  })

  output$histPlot <- renderPlot({
    x <- census_filter(input$incomes, input$race, input$ages, input$degree, input$sex, input$majorCategory)

    if(length(x$WAGP) <= 1){
      output$text1 <- renderText({ 
        "No matching observations found. Try adjusting your parameters"
      })
    } else {
      w <- x$WAGP
      output$text1 <- renderText({ 
        paste(length(w), "Matching observations found.", sep=" ")
      })
      bins <- seq(min(w), max(w), length.out = input$bins + 1)
      hist(w, breaks = bins, col = 'springgreen3', border = 'white', xlab='Income', main="Histogram of Income")
    }
  })
  
  output$qqPlot <- renderPlot({
    x <- census_filter(input$incomes, input$race, input$ages, input$degree, input$sex, input$majorCategory)
    if(length(x$WAGP) <= 1){
      output$text1 <- renderText({ 
        "No matching observations found. Try adjusting your parameters"
      })
    } else {
      output$text1 <- renderText({ 
        paste(length(x$WAGP), "Matching observations found.", sep=" ")
      })
      if (input$qqMode == "Log of Income") {
        type = log(x$WAGP)
      }else {
        type = x$WAGP
      }
      qqnorm(type,pch=10,col='springgreen3', main="Normal Q-Q plot of Income")
      qqline(type, col="blue")
    }
  })
  output$model_summary <- renderPrint ({
    x <- census_filter(input$incomes, input$race, input$ages, input$degree, input$sex, input$majorCategory)

    x <- na.omit(x)
    if(length(x$WAGP) <= 40){
      output$text1 <- renderText({ 
        "Too few matching observations found. Try adjusting your parameters"
      })
    } else {
      output$text1 <- renderText({ 
        paste(length(x$WAGP), "Matching observations found.", sep=" ")
      })
      train=(x$YEAR==2012)
      train_x=x[train,]
      test_x=x[!train,]
      
      train_x_keep_fod1p = (train_x$FOD1P %in% unique(test_x$FOD1P))
      train_x_keep_age = (train_x$AGEP %in% unique(test_x$AGEP))
      train_x <- train_x[train_x_keep_fod1p & train_x_keep_age,]
      
      test_x_keep_fod1p = (test_x$FOD1P %in% unique(train_x$FOD1P))
      test_x_keep_age = (test_x$AGEP %in% unique(train_x$AGEP))
      test_x <- test_x[test_x_keep_fod1p & test_x_keep_age,]
      
      test_x_w = test_x$WAGP
      cl = train_x$WAGP
      test_x <- test_x[,-1]
      train_x <- train_x[,-1]
      
      knn.pred=knn(train_x, test_x, log(cl), 20)
      p <- exp(1)^(as.numeric(as.character(knn.pred)))
      b <- as.numeric(as.character(knn.pred))
      
      mle <- mean(abs(log(test_x_w) - b))
      mre <- mean(abs(test_x_w - p))

      s_rate <- mean((p < test_x_w+input$bucketSize) & 
                      (p > test_x_w-input$bucketSize))
      s_rate <- round(s_rate, digits=4)
      
      txt1 <- paste(length(cl), "Training Observations", sep = " ")
      txt2 <- paste(length(test_x_w), "Test Observations", sep = " ")
      txt3 <- paste("Mean prediction success rate =", s_rate, "with an acceptable error of +-", input$bucketSize, sep = " ")
      
      txt6 <- paste("Mean train income:", round(mean(cl), digits=2), 
                    "  Mean test income:", round(mean(test_x_w), digits=2), 
                    "  Mean predicted income", round(mean(p), digits=2), sep = " ")
      txt7 <- paste("Mean log error:", mle, sep = " ")
      txt8 <- paste("Mean raw error:", mre, sep = " ")
      cat(txt1,"\n",
          txt2,"\n",
          txt3,"\n",
          txt6,"\n",
          txt7,"\n",
          txt8)
    }
  })
})
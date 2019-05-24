# Filename: totalfitnessfactorscore.R
# Author: Brandon Tomich and Cong Le 

##Building the Marthematical Model using 10-fold cross validation 

###The following code will calulate the predictions and rmse for RGM vs LGM using 10 fold cross validation
```{r , message=FALSE}
#loading needed libraries 
library(tidyverse)
library(caret)
library(modelr)
library(Metrics)

#getting rid of any NA values from the features we chose 
correlationHeathScienceData <- correlationHeathScienceData %>% filter(SF.1 != "NA",SF.2 != "NA", SF.3 != "NA")

#Creating folds from 10-fold cross validation
flds <- createFolds(y = correlationHeathScienceData$RGM, k = 10, list = TRUE, returnTrain = TRUE)

#creating linear model for RGM and LGM (1) 
rgm_vs_lgm_model_1 <- lm(formula = TotalFFScore ~ RGM + LGM, 
                         data =  correlationHeathScienceData[-flds[[1]], ])
#adding data from first training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[flds[[1]], "RGM"], 
                     "LGM" = correlationHeathScienceData[ flds[[1]], "LGM"])

#adding predictions to the first training set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vs_lgm_model_1)
#to calculate the RMSE we will use a package called "Metrics"
rmse_RGM_vs_LGM <- c(rmse(correlationHeathScienceData[flds[[1]], "TotalFFScore"],
                          testDf$pred))

#-----------------------------------------------------------------------------------------------------------
#creating linear model for RGM and LGM (2) 
rgm_vs_lgm_model_2 <- lm(formula = TotalFFScore ~ RGM + LGM, 
                         data =  correlationHeathScienceData[-flds[[2]], ])

#adding data from second training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[2]], "RGM"], 
                     "LGM" = correlationHeathScienceData[ flds[[2]], "LGM"])

# adding predictions to the second traiing set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vs_lgm_model_2)

# calculating the RMSE for model (2)
rmse_RGM_vs_LGM <- c(rmse_RGM_vs_LGM, 
                     rmse(correlationHeathScienceData[flds[[2]], "TotalFFScore"],
                          testDf$pred))

#-----------------------------------------------------------------------------------------------------------
#creating linear model for RGM and LGM (3)
rgm_vs_lgm_model_3 <- lm(formula = TotalFFScore ~ RGM + LGM, 
                         data =  correlationHeathScienceData[-flds[[3]], ])
#adding data from Third training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[3]], "RGM"], 
                     "LGM" = correlationHeathScienceData[ flds[[3]], "LGM"])

# adding predictions to the third training set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vs_lgm_model_3)

# calculating the RMSE for model (3)
rmse_RGM_vs_LGM <- c(rmse_RGM_vs_LGM, 
                     rmse(correlationHeathScienceData[flds[[3]], "TotalFFScore"],
                          testDf$pred))
#-----------------------------------------------------------------------------------------------------------
#creating linear model for RGM and LGM (4)
rgm_vs_lgm_model_4 <- lm(formula = TotalFFScore ~ RGM + LGM, 
                         data =  correlationHeathScienceData[-flds[[4]], ])

#adding data from Fourth training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[4]], "RGM"], 
                     "LGM" = correlationHeathScienceData[ flds[[4]], "LGM"])

# adding predictions to the 4th training set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vs_lgm_model_4)

# calculating the RMSE for model (4)
rmse_RGM_vs_LGM <- c(rmse_RGM_vs_LGM, 
                     rmse(correlationHeathScienceData[flds[[4]], "TotalFFScore"],
                          testDf$pred))

#-----------------------------------------------------------------------------------------------------------
#creating linear model for RGM and LGM (5) 
rgm_vs_lgm_model_5 <- lm(formula = TotalFFScore ~ RGM + LGM, 
                         data =  correlationHeathScienceData[-flds[[5]], ])
#adding data from Fifth training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[5]], "RGM"], 
                     "LGM" = correlationHeathScienceData[ flds[[5]], "LGM"])

# adding predictions to the 5th traiing set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vs_lgm_model_5)

# calculating the RMSE for model (5)
rmse_RGM_vs_LGM <- c(rmse_RGM_vs_LGM, 
                     rmse(correlationHeathScienceData[flds[[5]], "TotalFFScore"],
                          testDf$pred))
#-----------------------------------------------------------------------------------------------------------
#creating linear model for RGM and LGM (6) 
rgm_vs_lgm_model_6 <- lm(formula = TotalFFScore ~ RGM + LGM, 
                         data =  correlationHeathScienceData[-flds[[6]], ])
#adding data from Sixth training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[6]], "RGM"], 
                     "LGM" = correlationHeathScienceData[ flds[[6]], "LGM"])

# adding predictions to the 6th traiing set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vs_lgm_model_6)

# calculating the RMSE for model (6)
rmse_RGM_vs_LGM <- c(rmse_RGM_vs_LGM, 
                     rmse(correlationHeathScienceData[flds[[6]], "TotalFFScore"],
                          testDf$pred))
#-----------------------------------------------------------------------------------------------------------
#creating linear model for RGM and LGM (7)
rgm_vs_lgm_model_7 <- lm(formula = TotalFFScore ~ RGM + LGM, 
                         data =  correlationHeathScienceData[-flds[[7]], ])
#adding data from Seventh training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[7]], "RGM"], 
                     "LGM" = correlationHeathScienceData[ flds[[7]], "LGM"])

# adding predictions to the 7th training set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vs_lgm_model_7)

# calculating the RMSE for model (7)
rmse_RGM_vs_LGM <- c(rmse_RGM_vs_LGM, 
                     rmse(correlationHeathScienceData[flds[[7]], "TotalFFScore"],
                          testDf$pred))

#-----------------------------------------------------------------------------------------------------------
#creating linear model for RGM and LGM (8)
rgm_vs_lgm_model_8 <- lm(formula = TotalFFScore ~ RGM + LGM, 
                         data =  correlationHeathScienceData[-flds[[8]], ])
#adding data from Eighth training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[8]], "RGM"], 
                     "LGM" = correlationHeathScienceData[ flds[[8]], "LGM"])

# adding predictions to the second traiing set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vs_lgm_model_8)

# calculating the RMSE for model (8)
rmse_RGM_vs_LGM <- c(rmse_RGM_vs_LGM, 
                     rmse(correlationHeathScienceData[flds[[8]], "TotalFFScore"],
                          testDf$pred))
#-----------------------------------------------------------------------------------------------------------
#creating linear model for RGM and LGM (9)
rgm_vs_lgm_model_9 <- lm(formula = TotalFFScore ~ RGM + LGM, 
                         data =  correlationHeathScienceData[-flds[[9]], ])
#adding data from Nineth training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[9]], "RGM"], 
                     "LGM" = correlationHeathScienceData[ flds[[9]], "LGM"])

# adding predictions to the second traiing set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vs_lgm_model_9)

# calculating the RMSE for model (9)
rmse_RGM_vs_LGM <- c(rmse_RGM_vs_LGM, 
                     rmse(correlationHeathScienceData[flds[[9]], "TotalFFScore"],
                          testDf$pred))
#-----------------------------------------------------------------------------------------------------------
#creating linear model for RGM and LGM (10)
rgm_vs_lgm_model_10 <- lm(formula = TotalFFScore ~ RGM + LGM, 
                          data =  correlationHeathScienceData[-flds[[10]], ])
#adding data from tenth training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[10]], "RGM"], 
                     "LGM" = correlationHeathScienceData[ flds[[10]], "LGM"])

# adding predictions to the 10th traiing set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vs_lgm_model_10)

# calculating the RMSE for model (10)
rmse_RGM_vs_LGM <- c(rmse_RGM_vs_LGM, 
                     rmse(correlationHeathScienceData[flds[[10]], "TotalFFScore"],
                          testDf$pred))
```

###The following code will calulate the predictions and RMSE for RGM vs VC using 10 fold cross validation
```{r , message=FALSE}
#creating linear model for RGM and VC (1) 
rgm_vc_model_1 <- lm(formula = TotalFFScore ~ RGM + VC, 
                     data =  correlationHeathScienceData[-flds[[1]], ])
#adding data from first training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[1]], "RGM"], 
                     "VC" = correlationHeathScienceData[ flds[[1]], "VC"])

#adding predictions to the first training set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vc_model_1)
#to calculate the RMSE we will use a package called "Metrics"
rmse_RGM_VC <- c(rmse(correlationHeathScienceData[flds[[1]], "TotalFFScore"],
                      testDf$pred))

#-----------------------------------------------------------------------------------------------------------
#creating linear model for RGM and VC (2) 
rgm_vc_model_2 <- lm(formula = TotalFFScore ~ RGM + VC, 
                     data =  correlationHeathScienceData[-flds[[2]], ])

#adding data from second training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[2]], "RGM"], 
                     "VC" = correlationHeathScienceData[ flds[[2]], "VC"])

# adding predictions to the second traiing set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vc_model_2)

# calculating the RMSE for model (2)
rmse_RGM_VC <- c(rmse_RGM_VC, 
                 rmse(correlationHeathScienceData[flds[[2]], "TotalFFScore"],
                      testDf$pred))

#-----------------------------------------------------------------------------------------------------------
#creating linear model for RGM and VC (3)
rgm_vc_model_3 <- lm(formula = TotalFFScore ~ RGM + VC, 
                     data =  correlationHeathScienceData[-flds[[3]], ])
#adding data from Third training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[3]], "RGM"], 
                     "VC" = correlationHeathScienceData[ flds[[3]], "VC"])

# adding predictions to the third training set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vc_model_3)

# calculating the RMSE for model (3)
rmse_RGM_VC <- c(rmse_RGM_VC, 
                 rmse(correlationHeathScienceData[flds[[3]], "TotalFFScore"],
                      testDf$pred))
#-----------------------------------------------------------------------------------------------------------
#creating linear model for RGM and VC (4)
rgm_vc_model_4 <- lm(formula = TotalFFScore ~ RGM + VC, 
                     data =  correlationHeathScienceData[-flds[[4]], ])

#adding data from Fourth training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[4]], "RGM"], 
                     "VC" = correlationHeathScienceData[ flds[[4]], "VC"])

# adding predictions to the 4th training set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vc_model_4)

# calculating the RMSE for model (4)
rmse_RGM_VC <- c(rmse_RGM_VC, 
                 rmse(correlationHeathScienceData[flds[[4]], "TotalFFScore"],
                      testDf$pred))

#-----------------------------------------------------------------------------------------------------------
#creating linear model for RGM and VC (5) 
rgm_vc_model_5 <- lm(formula = TotalFFScore ~ RGM + VC, 
                     data =  correlationHeathScienceData[-flds[[5]], ])
#adding data from Fifth training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[5]], "RGM"], 
                     "VC" = correlationHeathScienceData[ flds[[5]], "VC"])

# adding predictions to the 5th traiing set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vc_model_5)

# calculating the RMSE for model (5)
rmse_RGM_VC <- c(rmse_RGM_VC, 
                 rmse(correlationHeathScienceData[flds[[5]], "TotalFFScore"],
                      testDf$pred))
#-----------------------------------------------------------------------------------------------------------
#creating linear model for RGM and VC (6) 
rgm_vc_model_6 <- lm(formula = TotalFFScore ~ RGM + VC, 
                     data =  correlationHeathScienceData[-flds[[6]], ])
#adding data from Sixth training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[6]], "RGM"], 
                     "VC" = correlationHeathScienceData[ flds[[6]], "VC"])

# adding predictions to the 6th traiing set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vc_model_6)

# calculating the RMSE for model (6)
rmse_RGM_VC <- c(rmse_RGM_VC, 
                 rmse(correlationHeathScienceData[flds[[6]], "TotalFFScore"],
                      testDf$pred))
#-----------------------------------------------------------------------------------------------------------
#creating linear model for RGM and VC (7)
rgm_vc_model_7 <- lm(formula = TotalFFScore ~ RGM + VC, 
                     data =  correlationHeathScienceData[-flds[[7]], ])
#adding data from Seventh training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[7]], "RGM"], 
                     "VC" = correlationHeathScienceData[ flds[[7]], "VC"])

# adding predictions to the 7th training set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vc_model_7)

# calculating the RMSE for model (7)
rmse_RGM_VC <- c(rmse_RGM_VC, 
                 rmse(correlationHeathScienceData[flds[[7]], "TotalFFScore"],
                      testDf$pred))

#-----------------------------------------------------------------------------------------------------------
#creating linear model for RGM and VC (8)
rgm_vc_model_8 <- lm(formula = TotalFFScore ~ RGM + VC, 
                     data =  correlationHeathScienceData[-flds[[8]], ])
#adding data from Eighth training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[8]], "RGM"], 
                     "VC" = correlationHeathScienceData[ flds[[8]], "VC"])

# adding predictions to the second traiing set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vc_model_8)

# calculating the RMSE for model (8)
rmse_RGM_VC <- c(rmse_RGM_VC, 
                 rmse(correlationHeathScienceData[flds[[8]], "TotalFFScore"],
                      testDf$pred))
#-----------------------------------------------------------------------------------------------------------
#creating linear model for RGM and VC (9)
rgm_vc_model_9 <- lm(formula = TotalFFScore ~ RGM + VC, 
                     data =  correlationHeathScienceData[-flds[[9]], ])
#adding data from Nineth training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[9]], "RGM"], 
                     "VC" = correlationHeathScienceData[ flds[[9]], "VC"])

# adding predictions to the second traiing set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vc_model_9)

# calculating the RMSE for model (9)
rmse_RGM_VC <- c(rmse_RGM_VC, 
                 rmse(correlationHeathScienceData[flds[[9]], "TotalFFScore"],
                      testDf$pred))
#-----------------------------------------------------------------------------------------------------------
#creating linear model for RGM and VC (10)
rgm_vc_model_10 <- lm(formula = TotalFFScore ~ RGM + VC, 
                      data =  correlationHeathScienceData[-flds[[10]], ])
#adding data from tenth training partition into a data frame
testDf <- data.frame("RGM" = correlationHeathScienceData[ flds[[10]], "RGM"], 
                     "VC" = correlationHeathScienceData[ flds[[10]], "VC"])

# adding predictions to the 10th traiing set based on rgm_vs_lgm_model
testDf <- testDf %>% add_predictions(rgm_vc_model_10)

# calculating the RMSE for the model (10)
rmse_RGM_VC <- c(rmse_RGM_VC, 
                 rmse(correlationHeathScienceData[flds[[10]], "TotalFFScore"],
                      testDf$pred))
```

##The next set of code will calulate the predictions and RMSE of the model containing the feature SF.1 
```{r, message=FALSE}
#creating linear model for SF.1 (1)
SF.1_model_1 <- lm(formula = TotalFFScore ~ SF.1, 
                   data = correlationHeathScienceData[-flds[[1]], ])
#adding data from first training partition into a data frame
testDf <- data.frame("SF.1" = correlationHeathScienceData[ flds[[1]], "SF.1"])

#adding predictions to the first traiing set based on SR.1_model
testDf <- testDf %>% add_predictions(SF.1_model_1)

# calculating the RMSE for the model (1)
rmse_SF.1 <- c(rmse(correlationHeathScienceData[flds[[1]], "TotalFFScore"],
                    testDf$pred))
#-----------------------------------------------------------------------------------------------------------
#creating linear model for SF.1 (2)
SF.1_model_2 <- lm(formula = TotalFFScore ~ SF.1, 
                   data = correlationHeathScienceData[-flds[[2]], ])
#adding data from second training partition into a data frame
testDf <- data.frame("SF.1" = correlationHeathScienceData[ flds[[2]], "SF.1"])

#adding predictions to the second traiing set based on SR.1_model
testDf <- testDf %>% add_predictions(SF.1_model_2)

# calculating the RMSE for the model (2)
rmse_SF.1 <- c(rmse_SF.1, 
               rmse(correlationHeathScienceData[flds[[2]], "TotalFFScore"],
                    testDf$pred))
#-----------------------------------------------------------------------------------------------------------
#creating linear model for SF.1 (3)
SF.1_model_3 <- lm(formula = TotalFFScore ~ SF.1, 
                   data = correlationHeathScienceData[-flds[[3]], ])
#adding data from third training partition into a data frame
testDf <- data.frame("SF.1" = correlationHeathScienceData[ flds[[3]], "SF.1"])

#adding predictions to the third traiing set based on SR.1_model
testDf <- testDf %>% add_predictions(SF.1_model_3)

# calculating the RMSE for the model (3)
rmse_SF.1 <- c(rmse_SF.1, 
               rmse(correlationHeathScienceData[flds[[3]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.1 (4)
SF.1_model_4 <- lm(formula = TotalFFScore ~ SF.1, 
                   data = correlationHeathScienceData[-flds[[4]], ])

#adding data from fourth training partition into a data frame
testDf <- data.frame("SF.1" = correlationHeathScienceData[ flds[[4]], "SF.1"])

#adding predictions to the fourth traiing set based on SR.1_model
testDf <- testDf %>% add_predictions(SF.1_model_4)

# calculating the RMSE for the model (4)
rmse_SF.1 <- c(rmse_SF.1, 
               rmse(correlationHeathScienceData[flds[[4]], "TotalFFScore"],
                    testDf$pred))

#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.1 (5)
SF.1_model_5 <- lm(formula = TotalFFScore ~ SF.1, 
                   data = correlationHeathScienceData[-flds[[5]], ])

#adding data from fifth training partition into a data frame
testDf <- data.frame("SF.1" = correlationHeathScienceData[ flds[[5]], "SF.1"])

#adding predictions to the fifth traiing set based on SR.1_model
testDf <- testDf %>% add_predictions(SF.1_model_5)

# calculating the RMSE for the model (5)
rmse_SF.1 <- c(rmse_SF.1, 
               rmse(correlationHeathScienceData[flds[[5]], "TotalFFScore"],
                    testDf$pred))

#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.1 (6)
SF.1_model_6 <- lm(formula = TotalFFScore ~ SF.1, 
                   data = correlationHeathScienceData[-flds[[6]], ])

#adding data from sixth training partition into a data frame
testDf <- data.frame("SF.1" = correlationHeathScienceData[ flds[[6]], "SF.1"])

#adding predictions to the sixth traiing set based on SR.1_model
testDf <- testDf %>% add_predictions(SF.1_model_6)

# calculating the RMSE for the model (6)
rmse_SF.1 <- c(rmse_SF.1, 
               rmse(correlationHeathScienceData[flds[[6]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.1 (7)
SF.1_model_7 <- lm(formula = TotalFFScore ~ SF.1, 
                   data = correlationHeathScienceData[-flds[[7]], ])

#adding data from seventh training partition into a data frame
testDf <- data.frame("SF.1" = correlationHeathScienceData[ flds[[7]], "SF.1"])

#adding predictions to the seventh traiing set based on SR.1_model
testDf <- testDf %>% add_predictions(SF.1_model_7)

# calculating the RMSE for the model (7)
rmse_SF.1 <- c(rmse_SF.1, 
               rmse(correlationHeathScienceData[flds[[7]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linar model for SF.1 (8)
SF.1_model_8 <- lm(formula = TotalFFScore ~ SF.1, 
                   data = correlationHeathScienceData[-flds[[8]], ])

#adding data from eighth training partition into a data frame
testDf <- data.frame("SF.1" = correlationHeathScienceData[ flds[[8]], "SF.1"])

#adding predictions to the eighth traiing set based on SR.1_model
testDf <- testDf %>% add_predictions(SF.1_model_8)

# calculating the RMSE for the model (8)
rmse_SF.1 <- c(rmse_SF.1, 
               rmse(correlationHeathScienceData[flds[[8]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linar model for SF.1 (9)
SF.1_model_9 <- lm(formula = TotalFFScore ~ SF.1, 
                   data = correlationHeathScienceData[-flds[[9]], ])

#adding data from nineth training partition into a data frame
testDf <- data.frame("SF.1" = correlationHeathScienceData[ flds[[9]], "SF.1"])

#adding predictions to the nineth traiing set based on SR.1_model
testDf <- testDf %>% add_predictions(SF.1_model_9)

# calculating the RMSE for the model (9)
rmse_SF.1 <- c(rmse_SF.1, 
               rmse(correlationHeathScienceData[flds[[9]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linar model for SF.1 (10)
SF.1_model_10 <- lm(formula = TotalFFScore ~ SF.1, 
                    data = correlationHeathScienceData[-flds[[10]], ])

#adding data from tenth training partition into a data frame
testDf <- data.frame("SF.1" = correlationHeathScienceData[ flds[[10]], "SF.1"])

#adding predictions to the tenth traiing set based on SR.1_model
testDf <- testDf %>% add_predictions(SF.1_model_10)

# calculating the RMSE for the model (10)
rmse_SF.1 <- c(rmse_SF.1, 
               rmse(correlationHeathScienceData[flds[[10]], "TotalFFScore"],
                    testDf$pred))
```

##The next set of code will calulate the predictions and RMSE of the model containing the feature SF.2

```{r , message=FALSE}
#creating linear model for SF.2 (1)
SF.2_model_1 <- lm(formula = TotalFFScore ~ SF.2, 
                   data = correlationHeathScienceData[-flds[[1]], ])

#adding data from first training partition into a data frame
testDf <- data.frame("SF.2" = correlationHeathScienceData[ flds[[1]], "SF.2"])

#adding predictions to the first traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.2_model_1)

# calculating the RMSE for the model (1)
rmse_SF.2 <- c(rmse(correlationHeathScienceData[flds[[1]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.2 (2)
SF.2_model_2 <- lm(formula = TotalFFScore ~ SF.2, 
                   data = correlationHeathScienceData[-flds[[2]], ])

#adding data from second training partition into a data frame
testDf <- data.frame("SF.2" = correlationHeathScienceData[ flds[[2]], "SF.2"])

#adding predictions to the second traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.2_model_2)

# calculating the RMSE for the model (2)
rmse_SF.2 <- c(rmse_SF.2, 
               rmse(correlationHeathScienceData[flds[[2]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.2 (3)
SF.2_model_3 <- lm(formula = TotalFFScore ~ SF.2, data = correlationHeathScienceData[-flds[[3]], ])

#adding data from third training partition into a data frame
testDf <- data.frame("SF.2" = correlationHeathScienceData[ flds[[3]], "SF.2"])

#adding predictions to the third traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.2_model_3)

# calculating the RMSE for the model (3)
rmse_SF.2 <- c(rmse_SF.2, 
               rmse(correlationHeathScienceData[flds[[3]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.2 (4)
SF.2_model_4 <- lm(formula = TotalFFScore ~ SF.2, 
                   data = correlationHeathScienceData[-flds[[4]], ])

#adding data from fourth training partition into a data frame
testDf <- data.frame("SF.2" = correlationHeathScienceData[ flds[[4]], "SF.2"])

#adding predictions to the fourth traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.2_model_4)

# calculating the RMSE for the model (4)
rmse_SF.2 <- c(rmse_SF.2, 
               rmse(correlationHeathScienceData[flds[[4]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.2 (5)
SF.2_model_5 <- lm(formula = TotalFFScore ~ SF.2, 
                   data = correlationHeathScienceData[-flds[[5]], ])

#adding data from fifth training partition into a data frame
testDf <- data.frame("SF.2" = correlationHeathScienceData[ flds[[5]], "SF.2"])

#adding predictions to the fifth traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.2_model_5)

# calculating the RMSE for the model (5)
rmse_SF.2 <- c(rmse_SF.2, 
               rmse(correlationHeathScienceData[flds[[5]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.2 (6)
SF.2_model_6 <- lm(formula = TotalFFScore ~ SF.2, 
                   data = correlationHeathScienceData[-flds[[6]], ])

#adding data from sixth training partition into a data frame
testDf <- data.frame("SF.2" = correlationHeathScienceData[ flds[[6]], "SF.2"])

#adding predictions to the sixth traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.2_model_6)

# calculating the RMSE for the model (6)
rmse_SF.2 <- c(rmse_SF.2, 
               rmse(correlationHeathScienceData[flds[[6]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.2 (7)
SF.2_model_7 <- lm(formula = TotalFFScore ~ SF.2, 
                   data = correlationHeathScienceData[-flds[[7]], ])

#adding data from seventh training partition into a data frame
testDf <- data.frame("SF.2" = correlationHeathScienceData[ flds[[7]], "SF.2"])

#adding predictions to the seventh training set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.2_model_7)

# calculating the RMSE for the model (7)
rmse_SF.2 <- c(rmse_SF.2, 
               rmse(correlationHeathScienceData[flds[[7]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.2 (8)
SF.2_model_8 <- lm(formula = TotalFFScore ~ SF.2, 
                   data = correlationHeathScienceData[-flds[[8]], ])

#adding data from eighth training partition into a data frame
testDf <- data.frame("SF.2" = correlationHeathScienceData[ flds[[8]], "SF.2"])

#adding predictions to the eighth traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.2_model_8)

# calculating the RMSE for the model (8)
rmse_SF.2 <- c(rmse_SF.2, 
               rmse(correlationHeathScienceData[flds[[8]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.2 (9)
SF.2_model_9 <- lm(formula = TotalFFScore ~ SF.2, 
                   data = correlationHeathScienceData[-flds[[9]], ])

#adding data from nineth training partition into a data frame
testDf <- data.frame("SF.2" = correlationHeathScienceData[ flds[[9]], "SF.2"])

#adding predictions to the nineth traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.2_model_9)

# calculating the RMSE for the model (9)
rmse_SF.2 <- c(rmse_SF.2, 
               rmse(correlationHeathScienceData[flds[[9]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.2 (10)
SF.2_model_10 <- lm(formula = TotalFFScore ~ SF.2, 
                    data = correlationHeathScienceData[-flds[[10]], ])

#adding data from tenth training partition into a data frame
testDf <- data.frame("SF.2" = correlationHeathScienceData[ flds[[10]], "SF.2"])

#adding predictions to the tenth traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.2_model_10)

# calculating the RMSE for the model (10)
rmse_SF.2 <- c(rmse_SF.2, 
               rmse(correlationHeathScienceData[flds[[10]], "TotalFFScore"],
                    testDf$pred))
```

##The next set of code will calulate the predictions and RMSE of the model containing the feature SF.3
```{r , message=FALSE}
#creating linear model for SF.3 (1)
SF.3_model_1 <- lm(formula = TotalFFScore ~ SF.3, 
                   data = correlationHeathScienceData[-flds[[1]], ])

#adding data from first training partition into a data frame
testDf <- data.frame("SF.3" = correlationHeathScienceData[ flds[[1]], "SF.3"])

#adding predictions to the first traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.3_model_1)

# calculating the RMSE for the model (1)
rmse_SF.3 <- c(rmse(correlationHeathScienceData[flds[[1]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.3 (2)
SF.3_model_2 <- lm(formula = TotalFFScore ~ SF.3, 
                   data = correlationHeathScienceData[-flds[[2]], ])

#adding data from second training partition into a data frame
testDf <- data.frame("SF.3" = correlationHeathScienceData[ flds[[2]], "SF.3"])

#adding predictions to the second traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.3_model_2)

# calculating the RMSE for the model (2)
rmse_SF.3 <- c(rmse_SF.3, 
               rmse(correlationHeathScienceData[flds[[2]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.3 (3)
SF.3_model_3 <- lm(formula = TotalFFScore ~ SF.3, data = correlationHeathScienceData[-flds[[3]], ])

#adding data from third training partition into a data frame
testDf <- data.frame("SF.3" = correlationHeathScienceData[ flds[[3]], "SF.3"])

#adding predictions to the third traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.3_model_3)

# calculating the RMSE for the model (3)
rmse_SF.3 <- c(rmse_SF.3, 
               rmse(correlationHeathScienceData[flds[[3]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.3 (4)
SF.3_model_4 <- lm(formula = TotalFFScore ~ SF.3, 
                   data = correlationHeathScienceData[-flds[[4]], ])

#adding data from fourth training partition into a data frame
testDf <- data.frame("SF.3" = correlationHeathScienceData[ flds[[4]], "SF.3"])

#adding predictions to the fourth traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.3_model_4)

# calculating the RMSE for the model (4)
rmse_SF.3 <- c(rmse_SF.3, 
               rmse(correlationHeathScienceData[flds[[4]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.3 (5)
SF.3_model_5 <- lm(formula = TotalFFScore ~ SF.3, 
                   data = correlationHeathScienceData[-flds[[5]], ])

#adding data from fifth training partition into a data frame
testDf <- data.frame("SF.3" = correlationHeathScienceData[ flds[[5]], "SF.3"])

#adding predictions to the fifth traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.3_model_5)

# calculating the RMSE for the model (5)
rmse_SF.3 <- c(rmse_SF.3, 
               rmse(correlationHeathScienceData[flds[[5]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.3 (6)
SF.3_model_6 <- lm(formula = TotalFFScore ~ SF.3, 
                   data = correlationHeathScienceData[-flds[[6]], ])

#adding data from sixth training partition into a data frame
testDf <- data.frame("SF.3" = correlationHeathScienceData[ flds[[6]], "SF.3"])

#adding predictions to the sixth traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.3_model_6)

# calculating the RMSE for the model (6)
rmse_SF.3 <- c(rmse_SF.3, 
               rmse(correlationHeathScienceData[flds[[6]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.3 (7)
SF.3_model_7 <- lm(formula = TotalFFScore ~ SF.3, 
                   data = correlationHeathScienceData[-flds[[7]], ])

#adding data from seventh training partition into a data frame
testDf <- data.frame("SF.3" = correlationHeathScienceData[ flds[[7]], "SF.3"])

#adding predictions to the seventh traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.3_model_7)

# calculating the RMSE for the model (7)
rmse_SF.3 <- c(rmse_SF.3, 
               rmse(correlationHeathScienceData[flds[[7]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.3 (8)
SF.3_model_8 <- lm(formula = TotalFFScore ~ SF.3, 
                   data = correlationHeathScienceData[-flds[[8]], ])

#adding data from eighth training partition into a data frame
testDf <- data.frame("SF.3" = correlationHeathScienceData[ flds[[8]], "SF.3"])

#adding predictions to the eighth traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.3_model_8)

# calculating the RMSE for the model (8)
rmse_SF.3 <- c(rmse_SF.3, 
               rmse(correlationHeathScienceData[flds[[8]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.3 (9)
SF.3_model_9 <- lm(formula = TotalFFScore ~ SF.3, 
                   data = correlationHeathScienceData[-flds[[9]], ])

#adding data from nineth training partition into a data frame
testDf <- data.frame("SF.3" = correlationHeathScienceData[ flds[[9]], "SF.3"])

#adding predictions to the nineth traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.3_model_9)

# calculating the RMSE for the model (9)
rmse_SF.3 <- c(rmse_SF.3, 
               rmse(correlationHeathScienceData[flds[[9]], "TotalFFScore"],
                    testDf$pred))
#----------------------------------------------------------------------------------------------------------
#creating linear model for SF.3 (10)
SF.3_model_10 <- lm(formula = TotalFFScore ~ SF.3, 
                    data = correlationHeathScienceData[-flds[[10]], ])

#adding data from tenth training partition into a data frame
testDf <- data.frame("SF.3" = correlationHeathScienceData[ flds[[10]], "SF.3"])

#adding predictions to the tenth traiing set based on SR.2_model
testDf <- testDf %>% add_predictions(SF.3_model_10)

# calculating the RMSE for the model (10)
rmse_SF.3 <- c(rmse_SF.3, 
               rmse(correlationHeathScienceData[flds[[10]], "TotalFFScore"],
                    testDf$pred))
```



## Graphing the RMSE

- The following vectors contain the RMSE's for each fold in each model: 
1. For the RGM vs LGM model the variable "rmse_RGM_vs_LGM" contains a set of 10 RMSE's for each fold 
2.For the RGM vs VC model the variable "rmse_RGM_VC" contains a set of 10 RMSE's for each fold 
3.For the SF.1 model the variable "rmse_SF.1" contains a set of 10 RMSE's for each fold 
4.For the SF.2 model the variable "rmse_SF.2" contains a set of 10 RMSE's for each fold 
5.For the SF.3 model the variable "rmse_SF.3" contains a set of 10 RMSE's for each fold 

```{r , message=FALSE}
barGraph <- data.frame("RMSE" =   c(mean(rmse_RGM_vs_LGM),
                                    mean(rmse_RGM_VC),
                                    mean(rmse_SF.1),
                                    mean(rmse_SF.2),
                                    mean(rmse_SF.3)),
                       "Category"= c("RGM and LGM","RGM and VC","SF.1","SF.2","SF.3") )

ggplot(barGraph) + geom_bar(mapping = aes(x=Category, y = RMSE, color = Category, fill = Category), 
                            stat="identity", width = 0.5) + ggtitle("Linear regression models and RMSE")  
```

As seen from the bar graph the factor "SF.2" has the lowest RMSE, so we will use it to create a function that will predict Total Fitness Factor.
To chose the proper model from SF.2, we look at the vector of RMSE from SF.2 and choose the model which contains the lowest RMSE. 

#The list of RMSE from SF.2 yields 
# [1] 7.437235 7.477029 7.514060 7.503259 7.513530 7.492290 7.495127 7.505391 7.474198 7.458560
# we then choose "7.437235" which is the from the first test fold 

```{r , message=FALSE}
#creating model conatining best RMSE 
SF.2_model_1 <- lm(formula = TotalFFScore ~ SF.2, 
                   data = correlationHeathScienceData[-flds[[1]], ])
summary(SF.2_model_1)
```

After running the summary of the model, we can see that the intercept is "47.95912" and the slope is "-0.50248".
Using the slope and intercept above, we can build our modle to predict Total Fitness Factor using the function below:
  ```{r , message=FALSE}
predictTotalFitnessFactor <- function(SF.2){
  return(47.95912 - (0.50248*SF.2))
}
```


```{r , message=FALSE}
calulatePredictionsAndRMSE <- function(firstFactor, secondFactor, model) {
  for (i in 1:10){
    rgm_vs_lgm_model <- lm(formula = TotalFFScore ~ RGM + LGM, data = correlationHeathScienceData)
    testDf <- data.frame("firstFactor" = correlationHeathScienceData[ flds[[i]], firstCol], 
                         "secondFactor" = correlationHeathScienceData[ flds[[i]], secondCol])
    testDf <- testDf %>% add_predictions(rgm_vs_lgm_model)
    
    rmse <- c(rmse(correlationHeathScienceData[flds[[i]], "TotalFFScore"],testDf$pred))
  }
  return(rmse)
}
```


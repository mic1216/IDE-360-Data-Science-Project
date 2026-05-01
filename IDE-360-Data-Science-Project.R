# INSTALL AND LOAD PACKAGES

  # NB: IF PACKAGE NOT INSTALLED, UNCOMMENT NEXT BLOCK OF CODE. RECOMMENT TO AVOID 
  # ERROR MESSAGES

  #install.packages("psych")
  #install.packages("caTools") 
  #install.packages("VGAM")
  #install.packages("caret")

  # LOAD
  library(caret)
  library(VGAM)
  library(caTools)
  library("psych")

# IMPORTING DATA

  data <- read.csv("HTOPS_HPS_2502_PUF.csv")
  
  print(length(data$EXPNS_DIF != -99))

# CLEANING DATA AND CREATING SUBSET

  data <- subset(data, ANXIOUS != -99 & (NDX14_FOOD == 1 | NDX14_SHELTER == 1 | 
                                    NDX14_MEDICAL == 1 | NDX14_EMOTIONAL == 1 | 
                                    NDX14_FRESHWATER == 1 | NDX14_ELECTRICITY == 1 
                                    | NDX14_NONE_NEEDED == 1) & CREATEART != -99
                 & EXPNS_DIF != -99 & SOCIAL2_first != -99 & SELFCARE != -99 
                 & ANYWORK != -99)
  
# EXTRACTING VARIABLES OF INTEREST FROM THE DATA SETS

  #explanatory variables
  needs <- data.frame(data$NDX14_FOOD,data$NDX14_SHELTER,data$NDX14_MEDICAL,
                      data$NDX14_EMOTIONAL,data$NDX14_FRESHWATER,
                      data$NDX14_ELECTRICITY) #table of needs by individual
  
  needs[needs == -99] <- 0 #Fixing binary statistic to better represent data
  needsScore <- rowSums(needs) #Summing all needs into one score
  
  art <- data$CREATEART - 1 # Whether participant practices art; 1 = Yes, 0 = No
  expnsDifclty <- data$EXPNS_DIF # Difficulty paying expenses; Higher score = Harder
  lonely <- data$SOCIAL2_first # Frequency of loneliness; Lower Score = Higher freq.
  selfCare <- data$SELFCARE # Difficulty of self care tasks; Higher = Harder
  employment <- data$ANYWORK - 1 # Working status; 1 = Working, 0 = Not Working
  
  
  #response variable
  anxiety <- data$ANXIOUS #How anxious an individual has felt over the past two weeks
  
  #creating new data table, exclusively w/ variables of interest
  
  data <- data.frame(needsScore,art,expnsDifclty,lonely,selfCare,employment,anxiety)
  

# EXPLORATORY DATA ANALYSIS

  #SUMMARIES
  
  print("Anxiety Description")
  print(describe(anxiety))
  cat("\n")
  
  print("Needs Score Description")
  print(describe(needsScore))
  cat("\n")
  #DATA SETUP
    
  
  
  #PLOTS
    # BOX PLOTS
    par(mfrow = c(2, 3))
    boxplot(anxiety ~ needsScore,
            main = "Anxiety by Needs Score",
            xlab = "Need",
            ylab = "Anxiety")
    boxplot(anxiety ~ art,
            main = "Anxiety by Participation in Art",
            xlab = "Art",
            ylab = "Anxiety")
    boxplot(anxiety ~ expnsDifclty,
            main = "Anxiety by Difficulty w/ Expenses",
            xlab = "Difficulty",
            ylab = "Anxiety")
    boxplot(anxiety ~ lonely,
            main = "Anxiety by Loneliness",
            xlab = "Loneliness",
            ylab = "Anxiety")
    boxplot(anxiety ~ selfCare,
            main = "Anxiety by Self-Care Ability",
            xlab = "Self Care",
            ylab = "Anxiety")
    boxplot(anxiety ~ employment,
            main = "Anxiety by Employment",
            xlab = "Employment",
            ylab = "Anxiety")
    
    
    #HISTOGRAMS
    #par(mfrow = c(2, 3))
    hist(needsScore, main="Needs Scores", xlab="Needs Score")
    
    

# LOGISTIC REGRESSION ANALYSIS
  
  # SPLITTING DATA SET
  
  split <- sample.split(data$anxiety, SplitRatio = 0.8)
  
  trainData <- subset(data, split == "TRUE")
  testData <- subset(data, split == "FALSE")


  print("Train Data")
  print(summary(trainData$anxiety))

  
  print("Test Data")
  print(describe(testData))

  # MODEL
  
  model <- vglm(anxiety ~ needsScore + art + expnsDifclty + 
                  lonely + selfCare + employment,
                family = cumulative, xdata = trainData)
  
  #training
  predicted_probs <- predict(model, type = "response")
  predicted_classes <- as.numeric(colnames(predicted_probs)[apply(predicted_probs, 
                                                                  1, which.max)])
  
  #applying
  newProbs <- predict(model, newdata = testData, type = "response")
  predictions <- as.numeric(colnames(newProbs)[apply(newProbs, 1, 
                                                            which.max)])
  
  print("predictions:")
  print(describe(predictions))
  print("actual:")
  print(describe(anxiety))
  
# VERIFICATION
  
  # HISTOGRAMS
  par(mfrow = c(2, 1))
  hist(testData$anxiety,main="Actual Anxiety",
       xlab="Anxiety")
  hist(predictions,main="Predicted Anxiety",
       xlab="Anxiety")

  # CONFUSION MATRIX
  
  conMat <- data.frame(matrix(0,ncol = 4, nrow = 4))
  colnames(conMat) <- c("1", "2", "3","4")
  
  for (prediction in 1:length(predictions))
  {
    conMat[testData$anxiety[prediction],predictions[prediction]] <- 
        conMat[testData$anxiety[prediction],predictions[prediction]] + 1
  }
    
  # METRICS
  
  #accuracy
  
  accuracy <- (conMat[1,1] + conMat[2,2] + conMat[3,3] + conMat[4,4]) / length(predictions)
  print("ACCURACY:")
  print(accuracy)
  
  #precision
  
  
  
  # The stupid dumb variables and model suck so stupid dumb much that we cant 
  # use the stupid dumb confusionMatrix function which.. is stupid and dumb. So.
  #cm <- confusionMatrix(data = predictions, reference = as.factor(anxiety))
  #print(cm)
  
  
  
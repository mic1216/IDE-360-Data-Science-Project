# INSTALL AND LOAD PACKAGES

  # NB: IF PACKAGE NOT INSTALLED, UNCOMMENT NEXT LINE OF CODE. RECOMMENT TO AVOID 
  # ERROR MESSAGES
  
  #install.packages("psych")
  #install.packages("caTools") 
  #install.packages("VGAM")
  library(VGAM)
  library(caTools)
  library("psych")

# IMPORTING DATA

  data <- read.csv("HTOPS_HPS_2502_PUF.csv")

# CLEANING DATA AND CREATING SUBSET

  data <- subset(data, ANXIOUS != -99 & (NDX14_FOOD == 1 | NDX14_SHELTER == 1 | 
                                    NDX14_MEDICAL == 1 | NDX14_EMOTIONAL == 1 | 
                                    NDX14_FRESHWATER == 1 | NDX14_ELECTRICITY == 1 
                                    | NDX14_NONE_NEEDED == 1))

# EXTRACTING VARIABLES OF INTEREST FROM THE DATA SETS

  #explanatory variables
  
  foodNeeds <- data$NDX14_FOOD #Inviduals whose most immediate need is food
  shelterNeeds<- data$NDX14_SHELTER #Inviduals whose most immediate need is shelter
  medicalNeeds <- data$NDX14_MEDICAL #Inviduals whose most immediate need is medical
  emotionalNeeds <- data$NDX14_EMOTIONAL #Inviduals whose most immediate need is emotional
  waterNeeds <- data$NDX14_FRESHWATER #Inviduals whose most immediate need is fresh water
  
   
  #response variable
  anxiety <- data$ANXIOUS #How anxious an individual has felt over the past two weeks
  
  #creating new data table, exclusively w/ variables of intrests
  data <- data.frame(foodNeeds,shelterNeeds,medicalNeeds,emotionalNeeds,
                     waterNeeds,anxiety)

# EXPLORATORY DATA ANALYSIS

  #SUMMARIES
  
  print("Anxiety Description")
  print(describe(anxiety))
  cat("\n")
  
  #DATA SETUP
    
    # SEPARATING DATA BY NEED RESPONSE
    
    foodAnx <- subset(data, foodNeeds == 1) #subset for individuals who have food need
    shelterAnx <- subset(data, shelterNeeds == 1) #subset for individuals who have shelter need
    medicalAnx <- subset(data, medicalNeeds == 1) #subset for individuals who have medical need
    emotionalAnx <- subset(data, emotionalNeeds == 1) #subset for individuals who have emotional need
    waterAnx <- subset(data, waterNeeds == 1) #subset for individuals who have fresh water need
    
    # CONCATENATING EXPLANATORY VARIABLES INTO ONE CATEGORICAL VARIABLE
    food <- rep("food", times = nrow(foodAnx))
    shelter <- rep("shelter", times = nrow(shelterAnx))
    medical <- rep("medical", times = nrow(medicalAnx))
    emotional <- rep("emotional", times = nrow(emotionalAnx))
    water <- rep("water", times = nrow(waterAnx))
    
    categories <- c(food,shelter,medical,emotional,water)
    anxByCat <- c(foodAnx[,6],shelterAnx[,6],medicalAnx[,6],emotionalAnx[,6],
                  waterAnx[,6])
    
    #making new datatable to show anxiety by category
    dataByCat <- data.frame(categories,anxByCat)
    
  
  #PLOTS
    boxplot(anxByCat ~ categories, data = dataByCat,
            main = "Anxiety by Need",
            xlab = "Need",
            ylab = "Anxiety")
    
    par(mfrow = c(2, 3))
    hist(foodAnx[,6],main="Anxiety for Individuals with Food Needs",
         xlab="Anxiety")
    hist(shelterAnx[,6],main="Anxiety for Individuals with Shelter Needs",
         xlab="Anxiety")
    hist(medicalAnx[,6],main="Anxiety for Individuals with Medical Needs",
         xlab="Anxiety")
    hist(emotionalAnx[,6],main="Anxiety for Individuals with Emotional Needs",
         xlab="Anxiety")
    hist(waterAnx[,6],main="Anxiety for Individuals with Water Needs",
         xlab="Anxiety")
    

# LOGISTIC REGRESSION ANALYSIS
  
  # SPLITTING DATA SET
  
  split <- sample.split(data, SplitRatio = 0.8)
  
  trainData <- subset(data, split == "TRUE")
  testData <- subset(data, split == "FALSE")
  
  # MODEL
  model <- vglm(anxiety ~ foodNeeds + shelterNeeds + medicalNeeds + 
                  emotionalNeeds + waterNeeds,
                family = multinomial, xdata = trainData)
  
  #training
  predicted_probs <- predict(model, type = "response")
  predicted_classes <- as.numeric(colnames(predicted_probs)[apply(predicted_probs, 1, which.max)])
  
  #applying
  newProbs <- predict(model, newdata = testData, type = "response")
  predictions <- as.numeric(colnames(predicted_probs)[apply(predicted_probs, 1, which.max)])
  
  print("predictions")
  print(describe(predictions))
  
# VERIFICATION
  
  #histograms
  par(mfrow = c(2, 3))
  hist(anxiety,main="Actual Anxiety",
       xlab="Anxiety")
  hist(predictions,main="Predicted Anxiety",
       xlab="Anxiety")
  
  #confusion matrix
  
  
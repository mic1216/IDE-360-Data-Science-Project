# INSTALL AND LOAD PACKAGES

  # NB: IF PSYCH NOT INSTALLED, UNCOMMENT NEXT LINE OF CODE. RECOMMENT TO AVOID 
  # ERROR MESSAGES
  
  #install.packages("psych")
  
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

# EXPLORATORY DATA ANALYSIS

  # summaries
  print("Food Needs")
  print(summary(foodNeeds))
  print(describe(foodNeeds))
  cat("\n")
  
  print("Shelter Needs")
  print(summary(shelterNeeds))
  print(describe(shelterNeeds))
  cat("\n")
  
  print("Medical Needs")
  print(summary(medicalNeeds))
  print(describe(medicalNeeds))
  cat("\n")
  
  print("Emotional Needs")
  print(summary(emotionalNeeds))
  print(describe(emotionalNeeds))
  cat("\n")
  
  print("Water Needs")
  print(summary(waterNeeds))
  print(describe(waterNeeds))
  cat("\n")
  
  print("Anxiety")
  print(summary(anxiety))
  print(describe(anxiety))
  cat("\n")


Assignment 6.1
1. Import the Titanic Dataset from the link Titanic Data Set.
   Perform the following:
     
     
    a. Preprocess the passenger names to come up with a list of titles that represent families
       and represent using appropriate visualization graph.
    
# Load packages
   library('ggplot2') # visualization
   library('ggthemes') # visualization
   library('scales') # visualization
   library('dplyr') # data manipulation
   library('mice') # imputation
   library('randomForest') # classification algorithm
   
train <- read.csv("F:/AcadGild/Files/train.csv", stringsAsFactors = F, na.strings=c("","NA"))
test  <- read.csv("F:/AcadGild/Files/test.csv", stringsAsFactors = F, na.strings=c("","NA"))

# combine training & test data
full  <- bind_rows(train, test) 

# check data summary
str(full)

# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Show title counts by sex
table(full$Sex, full$Title)

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)

# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Create a new variable that shows the family name and the family size
full$Family <- paste(full$Surname, full$Fsize, sep = "_")




b. Represent the proportion of people survived from the family size using a graph.

# What does our family size variable look like? To help us understand how it may relate to survival, let's plot it among the training data.
# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()




c. Impute the missing values in Age variable using Mice Library, create two different
    graphs showing Age distribution before and after imputation.

library(mice)
# No iteration. But I want to get Predictor-Matrix
init = mice(full, maxit=0) 
predM = init$predictorMatrix
# Do not use following columns to impute values in 'Age'. Use the rest.
predM[, c("PassengerId", "Name","Ticket","Cabin")]=0    
imp<-mice(full, m=5, predictorMatrix = predM)
# Get the final data-frame with imputed values filled in 'Age'
full <- complete(imp)
View(full)



# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
# Show number of missing Age values
sum(is.na(full$Age))


# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)



# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)


# Plot age distributions
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))




# Student List for PFDA Lab 30 Group 5
# Wong Kae Lam, TP071552
# Kylie Tan Zen Dong, TP075598
# Liau Shi En, TP073903
# Sean Khoo Khai Jin, TP065519

## Data Import
# to get current working directory of the environment
getwd()

# to set working directory
setwd("C:/Users/Callum Wong/Desktop/Programming for DA")

# to install and load necessary packages for use
install.packages("polycor")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("janitor")
install.packages("corrplot")
install.packages("validate")
install.packages("rattle")
install.packages("ISLR")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
install.packages("MASS")
install.packages("AUC")
install.packages("pROC")
install.packages("e1071")
install.packages("class")
install.packages("caTools")

library(polycor)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(janitor)
library(corrplot)
library(validate)
library(rattle)
library(ISLR)
library(rpart)
library(rpart.plot)
library(caret)
library(MASS)	
library(AUC)
library(pROC)
library(e1071)
library(class)
library(caTools)

# to read .csv file into R environment and import data in .csv file into a data frame
studentData = read.csv("C:/Users/Callum Wong/Desktop/Programming for DA/student_prediction.csv")

# to view data frame in tabular form
View(studentData)

## Data Cleaning
# to determine whether there are any NA values within the data set
sapply(studentData, function(x) sum(is.na(x)))

# to determine whether there are any duplicate rows within the data set
get_dupes(studentData)

## Data Processing
colnames(studentData)[15] <- "PARENTAL_STATUS"

## Data Exploration
# to determine whether data structure is a data frame
is.data.frame(studentData)

# to check data type of each columns in the data frame
str(studentData)

# to check column names of in the data frame
names(studentData)

# to check number of rows and columns of the data frame
cat("The Data Frame has:","\nrows:", dim(studentData)[1],"\ncolumns:", dim(studentData)[2], "\n")

# to display the summary of the data frame
summary(studentData)

## Data Validation
# to check if the values in each relevant columns are within its supposed range 
if(all(in_range(studentData$PARENTAL_STATUS, min="1", max = "3") == TRUE)){
  print("No factors other than 1 to 3 is detected in PARENTAL_STATUS.")
}
if(all(in_range(studentData$SCHOLARSHIP, min="1", max = "5") == TRUE)){
  print("No factors other than 1 to 5 is detected in SCHOLARSHIP.")
}
if(all(in_range(studentData$HS_TYPE, min="1", max = "3") == TRUE)){
  print("No factors other than 1 to 3 is detected in HS_TYPE.")
}
if(all(in_range(studentData$WORK, min="1", max = "2") == TRUE)){
  print("No factors other than 1 to 2 is detected in WORK.")
}
if(all(in_range(studentData$EXP_GPA, min="1", max = "5") == TRUE)){
  print("No factors other than 1 to 5 is detected in EXP_GPA.")
}


## Uni-variate Analysis (Data Exploration)
# Variation of Students' Parental Status
barPlot.Data <- studentData

frequencyDataParentalStatus <- barPlot.Data %>%
  group_by(PARENTAL_STATUS) %>%
  summarise(count = n())

ggplot(data = barPlot.Data) + 
  labs(title = "Variance of Students' Parental Status", x = "Parental Status") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(mapping = aes(x = as.factor(PARENTAL_STATUS), fill = as.factor(PARENTAL_STATUS)), width = 0.7) +
  geom_text(data = frequencyDataParentalStatus, aes(x = as.factor(PARENTAL_STATUS), y = count, label = count), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("red", "green", "blue")) +
  scale_x_discrete(labels = c("1" = "Married", "2" = "Divorced", "3" = "Died - one of them or both")) +
  theme(legend.position="none")

# Variation of Students' Scholarship Type
frequencyDataScholarship <- barPlot.Data %>%
  group_by(SCHOLARSHIP) %>%
  summarise(count = n())

ggplot(data = barPlot.Data) + 
  labs(title = "Frequency of Students' Scholarship Type", x = "Scholarship Type") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(mapping = aes(x = as.factor(SCHOLARSHIP), fill = as.factor(SCHOLARSHIP)), width = 0.7) +
  geom_text(data = frequencyDataScholarship, aes(x = as.factor(SCHOLARSHIP), y = count, label = count), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("green", "red", "blue", "yellow", "purple")) +
  scale_x_discrete(labels = c("1" = "None", "2" = "25%", "3" = "50%", "4" = "75%", "5" = "Full")) +
  theme(legend.position = "none")


# Variation of Students' Additional Work
frequencyDataWork <- barPlot.Data %>% 
  group_by(WORK) %>%
  summarise(count=n())

ggplot(data = barPlot.Data) +
  labs(title = "Frequency of students' Additional Work", x = "Work") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(mapping = aes(x = as.factor(WORK), fill = as.factor(WORK)), width = 0.7) + 
  geom_text(data = frequencyDataWork, aes(x = as.factor(WORK), y = count, label = count),vjust=-0.3,size=3.5) +
  scale_fill_manual(values = c("pink","purple")) +
  scale_x_discrete(labels = c("1" = "Yes","2" = "No")) +
  theme(legend.position = "none")

# Variation of Students' Graduated High-school Type
frequencyDataHS_Type <- barPlot.Data %>%
  group_by(HS_TYPE) %>%
  summarise(count = n())

ggplot(data = barPlot.Data) +
  labs(title = "Frequency of Students' Graduated High-school", x = "Graduated High-school") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(mapping = aes(x = as.factor(HS_TYPE), fill = as.factor(HS_TYPE))) +
  geom_text(data = frequencyDataHS_Type, aes(x=as.factor(HS_TYPE), y=count, label=count), 
            vjust=-0.5, size=3.5)+
  scale_fill_manual(values = c("skyblue","brown","violet"), labels = c("Private", "State", "Other")) +
  scale_x_discrete(drop = FALSE)+
  theme(legend.position = "none")

# Variation of Students' Expected Cumulative Grade Point Average
barPlot.Data$EXP_GPA <- factor(
  barPlot.Data$EXP_GPA, 
  levels = c("1", "2", "3", "4", "5"),
  labels = c("< 2.00", "2.00 - 2.49", "2.50 - 2.99", "3.00 - 3.49", "Above 3.49")
)

frequencyDataEXP_GPA <- barPlot.Data %>%
  group_by(EXP_GPA) %>%
  summarise(count = n())

ggplot(data = barPlot.Data) +
  labs(title = "Variance of Students' Expected CGPA at Graduation", x = "Expected CGPA at Graduation") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(mapping = aes(x = EXP_GPA, fill = EXP_GPA), width = 0.7) +
  geom_text(data = frequencyDataEXP_GPA, aes(x = EXP_GPA, y = count, label = count), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("red", "green", "blue", "orange", "purple")) +
  scale_x_discrete(labels = c("1" = "<2.00", "2" = "2.00-2.49", "3" = "2.50-2.99", "4" = "3.00-3.49", "5" = "above 3.49"), drop = FALSE) +
  theme(legend.position = "none")

## Research Question 1: Does a student studying with a married parental status 
# have a high expected CGPA at graduation? (Wong Kae Lam, TP071552)

# Analysis 1-1: Bi-variate Analysis between EXP_GPA and PARENTAL_STATUS
# Bar Chart
frequencyOfParentalToEXP_GPA <- barPlot.Data %>%
  group_by(EXP_GPA, PARENTAL_STATUS) %>%
  summarise(count = n())

ggplot(data = barPlot.Data, aes(fill = as.factor(PARENTAL_STATUS), x = EXP_GPA)) +
  labs(title = "The co-variance between Parental Status and Expected CGPA at Graduation",
       x = "Expected CGPA at Graduation", y = "Frequency",
       fill = "Parental Status") +
  geom_bar(position = "dodge", width = 0.7) +
  geom_text(data = frequencyOfParentalToEXP_GPA, aes(label = count, x = EXP_GPA, y = count, group = PARENTAL_STATUS), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("red", "green", "blue"),
                    labels = c("Married", "Divorced", "Died - one of them or both")) +
  scale_x_discrete(drop = FALSE) +
  theme_minimal()

# Stacked Bar Chart
ggplot(data = barPlot.Data) +
  labs(title = "The co-variance between Parental Status and Expected CGPA at Graduation",
       x = "Expected CGPA at Graduation", y = "Frequency",
       fill = "Parental Status") +
  geom_bar(position = "fill", width = 0.7, 
           mapping = aes(x = EXP_GPA, fill = as.factor(PARENTAL_STATUS))) +
  scale_fill_manual(values = c("red", "green", "blue"),
                    labels = c("Married", "Divorced", "Died - one of them or both")) +
  scale_x_discrete(drop = FALSE) +
  theme_minimal()

# Analysis 1-2: Chi-squared test
numeric.Data<- studentData
q1Table <- table(numeric.Data$PARENTAL_STATUS,numeric.Data$EXP_GPA)
chisq.test(q1Table)

# Analysis 1-3: Polychoric Correlation
polychor(numeric.Data$PARENTAL_STATUS,numeric.Data$EXP_GPA)

# Analysis 1-4: Multivariate Analysis
multiQ1.Data <- studentData
multiQ1.Data$EXP_GPA <- factor(
  multiQ1.Data$EXP_GPA,
  levels = c(1, 2, 3, 4, 5),
  labels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49", "Above 3.49")
)
multiQ1.Data$PARENTAL_STATUS <- factor(
  multiQ1.Data$PARENTAL_STATUS,
  levels = c(1, 2, 3),
  labels = c("Married", "Divorced", "Died - one of them or both")
)
multiQ1.Data$HS_TYPE <- factor(
  multiQ1.Data$HS_TYPE,
  levels = c(1, 2, 3),
  labels = c("Private", "State", "Other")
)

ggplot (multiQ1.Data, aes (x = PARENTAL_STATUS, fill = HS_TYPE)) + 
  geom_bar(position = "dodge") +
  labs(title = "Parental Status and Graduated High-school Type", 
       x = "Parental Status", 
       y = "Number of Students") +
  geom_text(stat = 'count', aes(label = stat(count)), position = position_dodge(width = 1), vjust = -0.1, size = 3) +
  facet_wrap(~EXP_GPA, scales = "free") +
  theme_classic()

# Research Question 2: Does a student studying on a full scholarship 
# have a high expected CGPA at graduation? (Sean Khoo Khai Jin, TP065519)

# Analysis 2-1: Bi-variate Analysis between EXP_GPA and SCHOLARSHIP
frequencyOfScholarshipToEXP_GPA <- barPlot.Data %>%
  group_by(EXP_GPA, SCHOLARSHIP) %>%
  summarise(count = n())

# Bar Chart
ggplot(data = barPlot.Data, aes(fill = as.factor(SCHOLARSHIP), x = EXP_GPA)) +
  labs(title = "Distribution of Expected CGPA at Graduation by Scholarship Type",
       x = "Expected CGPA at Graduation", y = "Frequency",
       fill = "Scholarship Type") +
  geom_bar(position = "dodge", width = 0.7) +
  geom_text(data = frequencyOfScholarshipToEXP_GPA, aes(label = count, x = EXP_GPA, y = count, group = SCHOLARSHIP),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("blue", "red", "green", "purple", "yellow"),
                    labels = c("None", "25%", "50%", "75%", "Full")) +
  scale_x_discrete(labels = c("1" = "<2.00", "2" = "2.00-2.49", "3" = "2.50-2.99", "4" = "3.00-3.49", "5" = "above 3.49"), drop = FALSE) +
  theme_minimal()

# Stacked Bar Chart
ggplot(data = barPlot.Data) +
  labs(title = "Students by Scholarship Type and Expected CGPA at graduation",
       x = "Expected CGPA at Graduation", y = "Frequency",
       fill = "Scholarship Type") +
  geom_bar(position = "fill", width = 0.7, 
           mapping = aes(x = EXP_GPA, fill = as.factor(SCHOLARSHIP))) +
  scale_fill_manual(values = c("blue", "red", "green", "purple", "yellow"),
                    labels = c("None", "25%", "50%", "75%", "Full")) +
  scale_x_discrete(labels = c("1" = "<2.00", "2" = "2.00-2.49", "3" = "2.50-2.99", "4" = "3.00-3.49", "5" = "above 3.49"), drop = FALSE) +
  theme_minimal()

# Analysis 2-2: Chi-squared test
q2Table <- table(numeric.Data$SCHOLARSHIP, numeric.Data$EXP_GPA)
chisq.test(q2Table)

# Find the critical value 
alpha <- 0.05
df <- 12
critical_value <- qchisq(1 - alpha, df)
critical_value

# Analysis 2-3: Polychoric Correlation
polychor(numeric.Data$SCHOLARSHIP,numeric.Data$EXP_GPA)

# Analysis 2-4: Multivariate Analysis
multiQ2.Data <- studentData
multiQ2.Data$EXP_GPA <- factor(
  multiQ2.Data$EXP_GPA,
  levels = c(1, 2, 3, 4, 5),
  labels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49", "Above 3.49")
)
multiQ2.Data$SCHOLARSHIP <- factor(
  multiQ2.Data$SCHOLARSHIP,
  levels = c(1, 2, 3, 4, 5),
  labels = c("None", "25%", "50%","75%","Full")
)

multiQ2.Data$PARENTAL_STATUS <- factor(
  multiQ2.Data$PARENTAL_STATUS,
  levels = c(1, 2, 3),
  labels = c("Married", "Divorced", "Died - one of them or both")
)

ggplot (multiQ2.Data, aes (x = SCHOLARSHIP, fill = PARENTAL_STATUS)) + 
  geom_bar(position = "dodge") +
  labs(title = "Scholarship Type and Parental Status", 
       x = "Scholarship Type", 
       y = "Number of Students") +
  geom_text(stat = 'count', aes(label = stat(count)), position = position_dodge(width = 1), vjust = -0.1, size = 3) +
  facet_wrap(~EXP_GPA, scales = "free") +
  theme_classic()

# Research Question 3: Does a student studying with no additional work 
# have a high expected CGPA at graduation? (Liau Shi En, TP073903)

# Analysis 3-1: Bi-variate Analysis between EXP_GPA and WORK
# Bar Chart
frequencyOfWorkToEXP_GPA <- barPlot.Data %>%
  group_by(EXP_GPA, WORK) %>%
  summarise(count = n())

ggplot(data = barPlot.Data, aes(fill = as.factor(WORK),x=EXP_GPA)) +
  labs(title = paste("The co-variance between Additional Work and Expected CGPA at Graduation"),
       x="Expected CGPA at Graduation", y="Frequency", fill="Additional Work") +
  geom_bar(position = "dodge", width = 0.7) +
  geom_text(data = frequencyOfWorkToEXP_GPA, aes(label=count, x=EXP_GPA, y=count, group=WORK),
            position=position_dodge(width=0.7), vjust=-0.5, size=3) +
  scale_fill_manual(values = c("pink","purple"),
                    labels = c("Work","No Work"))+
  scale_x_discrete(drop=FALSE, labels=c("1"="<2.00","2"="2.00-2.49","3"="2.50-2.99","4"="3.00-3.49","5"="Above 3.49") )+
  theme_minimal()

# Stacked Bar Chart
ggplot(data = barPlot.Data) +
  labs(title = "The co-variance between Addtional Work and Expected CGPA at Graduation",
       x = "Expected CGPA at Graduation", y = "Frequency",
       fill = "Addtional Work") +
  geom_bar(position = "fill", width = 0.7, 
           mapping = aes(x = EXP_GPA, fill = as.factor(WORK))) +
  scale_fill_manual(values = c("pink", "purple"),
                    labels = c("Yes", "No")) +
  scale_x_discrete(drop = FALSE) +
  theme_minimal()

# Analysis 3-2: Chi-squared test
q3Table <- table(numeric.Data$WORK,numeric.Data$EXP_GPA)
chisq.test(q3Table)

# Analysis 3-3: Polychoric Correlation
polychor(numeric.Data$WORK,numeric.Data$EXP_GPA)

# Analysis 3-4: Multivariate Analysis
multiQ3.Data <- studentData
multiQ3.Data$EXP_GPA <- factor(
  multiQ3.Data$EXP_GPA,
  levels = c(1, 2, 3, 4, 5),
  labels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49", "Above 3.49")
)
multiQ3.Data$WORK <- factor(
  multiQ3.Data$WORK,
  levels = c(1, 2),
  labels = c("Yes", "No")
)
multiQ3.Data$SCHOLARSHIP <- factor(
  multiQ3.Data$SCHOLARSHIP,
  levels = c(1, 2, 3, 4, 5),
  labels = c("None", "25%", "50%","75%","Full")
)

ggplot (multiQ3.Data, aes (x = WORK, fill = SCHOLARSHIP)) + 
  geom_bar(position = "dodge") +
  labs(title = "Additional Work and Scholarship Type", 
       x = "Additional Work", 
       y = "Number of Students") +
  geom_text(stat = 'count', aes(label = stat(count)), position = position_dodge(width = 1), vjust = -0.1, size = 3) +
  facet_wrap(~EXP_GPA, scales = "free") +
  theme_classic()

# Research Question 4: Does a student who graduated from a private-high school have 
# a high expected CGPA at graduation? (Kylie Tan Zen Dong, TP075598)

# Analysis 4-1: Bi-variate Analysis between EXP_GPA and HS_TYPE
# Bar Chart
frequencyOfHSTypeToEXP_GPA <- barPlot.Data %>%
  group_by(EXP_GPA, HS_TYPE) %>%
  summarise(count = n())

ggplot(data = barPlot.Data, aes(fill = as.factor(HS_TYPE), x = EXP_GPA)) +
  labs(title = "Co-variation between Graduated High-School & Expected CGPA at Graduation", 
       x = "Expected CGPA", y = "Frequency", fill="Graduated High-school") +
  geom_bar(position = "dodge", width = 0.7) +
  geom_text(data = frequencyOfHSTypeToEXP_GPA, aes(label = count, x=as.factor(EXP_GPA), y=count, group=HS_TYPE), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("skyblue","brown","violet"), labels = c("Private", "State", "Other")) +
  scale_x_discrete(drop = FALSE) +
  theme_minimal()

# Stacked Bar Chart
ggplot(data = barPlot.Data) +
  labs(title = "The co-variance between Graduated High-School Type & Expected CGPA at Graduation",
       x = "Expected CGPA at Graduation", y = "Frequency",
       fill = "Graduated High-School Type") +
  geom_bar(position = "fill", width = 0.7, 
           mapping = aes(x = EXP_GPA, fill = as.factor(HS_TYPE))) +
  scale_fill_manual(values = c("skyblue","brown","violet"),
                    labels = c("Private", "State", "Other")) +
  scale_x_discrete(drop = FALSE) +
  theme_minimal()

# Analysis 4-2: Chi-squared test
q4Table <- table(numeric.Data$HS_TYPE,numeric.Data$EXP_GPA)
chisq.test(q4Table)

# Analysis 4-3: Polychoric Correlation
polychor(numeric.Data$HS_TYPE,numeric.Data$EXP_GPA)

# Analysis 4-4: Multivariate Analysis
multiQ4.Data <- studentData
multiQ4.Data$EXP_GPA <- factor(
  multiQ4.Data$EXP_GPA,
  levels = c(1, 2, 3, 4, 5),
  labels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49", "Above 3.49")
)
multiQ4.Data$HS_TYPE <- factor(
  multiQ4.Data$HS_TYPE,
  levels = c(1, 2, 3),
  labels = c("Private", "State", "Other")
)
multiQ4.Data$WORK <- factor(
  multiQ4.Data$WORK,
  levels = c(1, 2),
  labels = c("Yes", "No")
)

ggplot (multiQ4.Data, aes (x = HS_TYPE, fill = WORK)) + 
  geom_bar(position = "dodge") +
  labs(title = "Graduated High-school Type and Additional Work", 
       x = "Graduated High-school Type", 
       y = "Number of Students") +
  geom_text(stat = 'count', aes(label = stat(count)), position = position_dodge(width = 1), vjust = -0.1, size = 3) +
  facet_wrap(~EXP_GPA, scales = "free") +
  theme_classic()

#----------------------------
# Decision Tree - Frequency Table 
#----------------------------
# Extra feature 1 (Sean Khoo Khai Jin, TP065519)

# to create a duplicate data set for additional feature use only
clean.Data<- studentData

# to set the seed for reproducibility
set.seed(42)

# to copy relevant columns into a new data frame
decisionTree.Set <- clean.Data %>%
  dplyr::select(EXP_GPA, PARENTAL_STATUS, SCHOLARSHIP, WORK, HS_TYPE)

# to convert values of 'int' data type to 'factor' data type for the relevant columns
decisionTree.Set$HS_TYPE <- factor(
  decisionTree.Set$HS_TYPE,
  levels = c(1, 2, 3),
  labels = c("private", "state", "other")
)

decisionTree.Set$SCHOLARSHIP <- factor(
  decisionTree.Set$SCHOLARSHIP,
  levels = c(1, 2, 3, 4, 5),
  labels = c("None", "25%", "50%", "75%", "Full")
)

decisionTree.Set$PARENTAL_STATUS <- factor(
  decisionTree.Set$PARENTAL_STATUS,
  levels = c(1, 2, 3),
  labels = c("married", "divorced", "died - one of them or both")
)

decisionTree.Set$WORK <- factor(
  decisionTree.Set$WORK,
  levels = c(1, 2),
  labels = c("Yes", "No")
)

decisionTree.Set$EXP_GPA <- factor(
  decisionTree.Set$EXP_GPA,
  levels = c(1, 2, 3, 4, 5),
  labels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49", "above 3.49")
)

# to convert EXP_GPA into a 'Y's and 'N's (Binary target)
decisionTree.Set$EXP_GPA <- factor(
  decisionTree.Set$EXP_GPA,
  levels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49", "above 3.49"),
  labels = c("N", "N", "Y", "Y", "N")
)

# Create an index for splitting the data
index <- createDataPartition(decisionTree.Set$EXP_GPA, p = 0.8, list = FALSE)

# Create training set
training_set <- decisionTree.Set[index, ]

# Create testing set
testing_set <- decisionTree.Set[-index, ]

mytree <- rpart(
  EXP_GPA ~  PARENTAL_STATUS + SCHOLARSHIP + WORK + HS_TYPE, 
  data = training_set, 
  method = "class",
  maxdepth = 3,
  cp = -1
)

fancyRpartPlot(mytree, caption = NULL, type=2)

mytree <- rpart(
  EXP_GPA ~  PARENTAL_STATUS + SCHOLARSHIP + WORK + HS_TYPE, 
  data = training_set, 
  method = "class",
  cp = -1
)

# Lift chart
pbDS <- NULL
pbDS <- predict(mytree, testing_set)
pbDS <- as.data.frame(pbDS)
predictionDS <- data.frame(testing_set$EXP_GPA, pbDS$Y)
colnames(predictionDS) <- c("target", "score")
liftChart <- lift(target ~ score, data = predictionDS, cuts=10, class ="Y")
xyplot(liftChart, main="Decision Tree - Lift Chart", type=c("l","g"), lwd=2
       , scales=list(x=list(alternating=FALSE,tick.number = 10)
                     ,y=list(alternating=FALSE,tick.number = 10)))

#ROC chart
labels <- as.factor(ifelse(predictionDS$target=="Y", 1, 0))
predictions <- predictionDS$score
AUC::auc(AUC::roc(predictions, labels), min = 0, max = 1)
plot(AUC::roc(predictions, labels), min=0, max=1, type="l", main="Decision Tree - ROC Chart",col = "blue")

# confusion matrix
pcDS <- NULL
pcDS <- ifelse(pbDS$N > pbDS$Y, "N", "Y")
summary(as.data.frame(pcDS))
xtab <- table(pcDS, testing_set$EXP_GPA)
caret::confusionMatrix(xtab, positive = "Y")

#--------------------
# Logistic regression
#--------------------
# Extra feature 2 (Kylie Tan Zen Dong, TP075598)

# to copy relevant columns into a new data frame
logisticRegression.Set <- clean.Data %>%
  dplyr::select(EXP_GPA, PARENTAL_STATUS, SCHOLARSHIP, WORK, HS_TYPE)

# Convert values of 'int' data type to 'factor' data type for the relevant columns
logisticRegression.Set$PARENTAL_STATUS <- factor(
  logisticRegression.Set$PARENTAL_STATUS,
  levels = c(1, 2, 3),
)
logisticRegression.Set$SCHOLARSHIP <- factor(
  logisticRegression.Set$SCHOLARSHIP,
  levels = c(1, 2, 3, 4, 5),
)
logisticRegression.Set$WORK <- factor(
  logisticRegression.Set$WORK,
  levels = c(1, 2),
)
logisticRegression.Set$HS_TYPE <- factor(
  logisticRegression.Set$HS_TYPE,
  levels = c(1, 2, 3),
)
logisticRegression.Set$EXP_GPA <- factor(
  logisticRegression.Set$EXP_GPA,
  levels = c(1, 2, 3, 4, 5),
)
# to convert EXP_GPA into a '1's and '0's (Binary target)
logisticRegression.Set$EXP_GPA <- as.factor(ifelse(logisticRegression.Set$EXP_GPA %in% c("3", "4"), 1, 0))
unique(logisticRegression.Set$EXP_GPA)
logisticRegression.Set$EXP_GPA

# to train the logistic regression model
model.LogReg <- glm(EXP_GPA ~ PARENTAL_STATUS + SCHOLARSHIP + WORK + HS_TYPE, 
                    family = binomial(link = "logit"), 
                    data = logisticRegression.Set,
                    control = glm.control(maxit = 1000))

# Lift chart
pbLR <- NULL
pbLR <- predict(model.LogReg, logisticRegression.Set)
pbLR <- as.data.frame(pbLR)
pred.LogReg <- data.frame(logisticRegression.Set$EXP_GPA, 1 / (1 + (exp(1) ^ -pbLR)))
colnames(pred.LogReg) <- c("target", "score")
levels(pred.LogReg$target)
lift.LogReg <- lift(target ~ score, data = pred.LogReg, cuts = 10, class = "1")
xyplot(lift.LogReg, main = "Logistic Regression - Lift Chart", type = c("l", "g"), lwd = 2,
       scales = list(x = list(alternating = FALSE, tick.number = 10),
                     y = list(alternating = FALSE, tick.number = 10)))

# Confusion matrix (detailed)
pcLR <- predict(model.LogReg, logisticRegression.Set, type = "response")
pcLR <- ifelse(pcLR > 0.5, 1, 0)
confusion_matrixLR <- confusionMatrix(factor(pcLR), logisticRegression.Set$EXP_GPA)
confusion_matrixLR

# ROC curve
labelsLR <- as.factor(ifelse(pred.LogReg$target=="1", 1, 0))
predictionsLR <- pred.LogReg$score
AUC::auc(AUC::roc(predictionsLR, labelsLR), min = 0, max = 1)
plot(AUC::roc(predictionsLR, labelsLR), min=0, max=1, type="l", main="Logistic Regression - ROC Chart",col = "blue")


#--------------------
# Naive Bayesian
#--------------------
# Extra feature 3 (Liau Shi En, TP073903)

# to copy relevant columns into a new data frame
naiveBayesian.Set <- clean.Data %>%
  dplyr::select(EXP_GPA, PARENTAL_STATUS, SCHOLARSHIP, WORK, HS_TYPE)

# to convert EXP_GPA into a '1's and '0's (Binary target)
naiveBayesian.Set$EXP_GPA <- as.factor(ifelse(naiveBayesian.Set$EXP_GPA %in% c("3", "4"), 1, 0))

#train the Naive Bayesian model
model.Bayes <- naiveBayes(EXP_GPA~., data = naiveBayesian.Set)
model.Bayes

#confusion matrix (detailed)
pcNB <-NULL
pcNB <- predict(model.Bayes, naiveBayesian.Set, type = "class")
summary(pcNB)
xtabNB <- table(pcNB, naiveBayesian.Set$EXP_GPA)
caret::confusionMatrix(xtabNB, positive = "1")


#lift chart
pbNaiveBayes <-NULL
pbNaiveBayes <- predict(model.Bayes, naiveBayesian.Set, type = "raw")
pbNaiveBayes <- as.data.frame(pbNaiveBayes)
pred.Bayes <- data.frame(naiveBayesian.Set$EXP_GPA,pbNaiveBayes$"1")
colnames(pred.Bayes) <- c("target","score")
pred.Bayes$target <- as.factor(pred.Bayes$target)
lift.Bayes <- lift(target ~ score, data = pred.Bayes, cuts=10, class="1")
xyplot(lift.Bayes, main="Bayesian Classifier - Lift Chart", type=c("l","g"), lwd=2
       , scales=list(x=list(alternating=FALSE,tick.number = 10)
                     ,y=list(alternating=FALSE,tick.number = 10)))

# ROC curve
labelsNB <- as.factor(ifelse(pred.Bayes$target=="1", 1, 0))
predictionsNB <- pred.Bayes$score
AUC::auc(AUC::roc(predictionsNB, labelsNB), min = 0, max = 1)
plot(AUC::roc(predictionsNB, labelsNB), min=0, max=1, type="l", main="Bayesian Classifier - ROC Chart",col = "blue")

#----------------------------------
# K Nearest Neighbors - Regression
#----------------------------------
# Extra feature 4 (Wong Kae Lam, TP071552)

# to copy relevant columns into a new data frames
knndata <- clean.Data %>%
  dplyr::select(EXP_GPA, PARENTAL_STATUS, SCHOLARSHIP, WORK, HS_TYPE)
knntest <- clean.Data %>%
  dplyr::select(EXP_GPA, PARENTAL_STATUS, SCHOLARSHIP, WORK, HS_TYPE)

# to convert data in every column to 'factor' data type
knndata$PARENTAL_STATUS <- factor(
  knndata$PARENTAL_STATUS,
  levels = c(1, 2, 3),
)
knndata$SCHOLARSHIP <- factor(
  knndata$SCHOLARSHIP,
  levels = c(1, 2, 3, 4, 5),
)
knndata$WORK <- factor(
  knndata$WORK,
  levels = c(1, 2),
)
knndata$HS_TYPE <- factor(
  knndata$HS_TYPE,
  levels = c(1, 2, 3),
)
knndata$EXP_GPA <- factor(
  knndata$EXP_GPA,
  levels = c(1, 2, 3, 4, 5),
)

knntest$PARENTAL_STATUS <- factor(
  knntest$PARENTAL_STATUS,
  levels = c(1, 2, 3),
)
knntest$SCHOLARSHIP <- factor(
  knntest$SCHOLARSHIP,
  levels = c(1, 2, 3, 4, 5),
)
knntest$WORK <- factor(
  knntest$WORK,
  levels = c(1, 2),
)
knntest$HS_TYPE <- factor(
  knntest$HS_TYPE,
  levels = c(1, 2, 3),
)
knntest$EXP_GPA <- factor(
  knntest$EXP_GPA,
  levels = c(1, 2, 3, 4, 5),
)
# to check the data type to make sure it is already in 'factor' data type
str(knndata)
str(knntest)


## For this library function, input training data should not have the response variable
# to separate the response variable
train_data = knndata[,-1]
# to separate the response variable from the test data
test_data = knntest[,-1]

## to determine the number of categories for the response variable
cls<-factor(knndata[,1])
cls


## to calculate the test accuracy of the model, we need to know the class of the test data
##Separating test data categories/class
actual<-factor(knntest[,1])
actual

# to set the seed for reproducibility
set.seed(42)

## Creating K-NN Model using knn() function
##The Objective of this KNN model is to classify and predict EXP_GPA
##EXP_GPA is the target(response) variable and remaining targeted columns are the independent variables
knnmod<-knn(train=train_data,test=test_data,cl=cls,k=3,
            prob=FALSE)

## to predict the new test data
predicted<-knnmod

## to calculate the classification error
check_error = function(actual, predicted) {
  mean(actual != predicted)
}

check_error(actual,predicted)


# to calculate the accuracy and other parameters using caret functions
report<-confusionMatrix (actual, predicted)
report


# to try the different values of k
ks = 1:10 ##Try k values from 1 to 10
##Storing errors in an array
store_error = rep(x=0, times = length(ks))

for (i in seq_along(ks)) {
  predicted = knn(train = train_data,
                  test = test_data,
                  cl = cls,
                  k = ks[i])
  store_error[i] = check_error(actual, predicted)
}

## to plot Error Vs K-values graph

plot(store_error, 
     type='b', 
     col = "blue", 
     cex = 1, pch = 20,
     xlab = "K - values",
     ylab = "classification error",
     main = "Error Rate vs K-Values")



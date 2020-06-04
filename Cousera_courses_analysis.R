##LOAD THE DATASET
df <- read.csv('coursea_data.csv')

###EXPLORATORY DATA ANALYSIS
head(df)
str(df)
summary(df)


###CHECK IF THERE EXIST SOME MISSING VALUES
any(is.na(df))

table(df$course_title)

###DIMENSIONAL REDUCTION OF THE COURSE ORGERNIZATION COLUMN

courese_org <- function(orgname){
  orgname <- as.character(orgname)
  for (i in orgname) {
    if(grepl('University',i)==T){
      return('University')
    }else if(grepl('Google',i)==T){
      return('Google')
    }else if(grepl('IBM',i)==T){
      return('Ibm')
    }else if(grepl('College',i)==T){
      return('University')
   
    }else if(grepl('Business',i)==T){
      return('University')
    }else if(grepl('deeplearning.ai',i)==T){
      return('deeplearning.ai')
    }else if(grepl('Universidad',i)==T){
      return('University')
    }else if(grepl('Universitat',i)==T){
      return('University')
    }else if(grepl('Institute',i)==T){
      return('University')
    }else if(grepl('Universiteit',i)==T){
      return('University')
    }else if(grepl('Universit??',i)==T){
      return('University')
    }else if(grepl('Universit????t',i)==T){
      return('University')
    }else if(grepl('Medicine',i)==T){
      return('University')
    }
    else{
      return('Others')
    }
  }
   
}
df$orgarnization <-  sapply(df$course_organization,courese_org)
table(df$orgarnization)
summary(df)
head(df)
str(df)
df$orgarnization <- factor(df$orgarnization)
str(df)
A <- subset(df, orgarnization == 'Others')
A$course_organization
###Key words are ##Programming ##Health ##Data Science##
colnames(df)

#DIMENSIONAL REDUCTION OF COURSE TITLE AND FEATURE ENGINEERING


courese_type <- function(course){
  
  for (i in course) {
    if(grepl('Programming',i)==T){
      return('I.T')
    }else if(grepl('Python',i)==T){
      return('I.T')
    }else if(grepl('Data',i)==T){
      return('I.T')
    }else if(grepl('Artificial intelligence',i)==T){
      return('I.T')
      
    }else if(grepl('Machine learning',i)==T){
      return('I.T')
    }else if(grepl('Self-Driving Cars',i)==T){
      return('I.T')
    }else if(grepl('Breastfeeding',i)==T){
      return('Health')
    }else if(grepl('Infection',i)==T){
      return('Health')
    }
    else if(grepl('Health',i)==T){
      return('Health')
    }
    else if(grepl('Anatomy',i)==T){
      return('Health')
    }else if(grepl('Cancer',i)==T){
      return('Health')
    }else if(grepl('Antibiotic',i)==T){
      return('Health')
    }else if(grepl('Antimicrobial',i)==T){
      return('Health')
    }else if(grepl('Management',i)==T){
      return('Management')
    }else if(grepl('Marketing',i)==T){
      return('Marketing')
    }else if(grepl('Communication',i)==T){
      return('Marketing')
    }else if(grepl('Branding',i)==T){
      return('Marketing')
    }else if(grepl('Software',i)==T){
      return('I.T')
    }else if(grepl('Supply',i)==T){
      return('Management')
    }else if(grepl('JavaScript',i)==T){
      return('I.T')
    }else if(grepl('AI For Medical',i)==T){
      return('I.T')
    }else if(grepl('AWS Fundamentals',i)==T){
      return('I.T')
    }else if(grepl('Agile',i)==T){
      return('I.T')
    }else if(grepl('Algorithm',i)==T){
      return('I.T')
    }else if(grepl('Google',i)==T){
      return('I.T')
    }else if(grepl('Mathematics',i)==T){
      return('I.T')
    }else if(grepl('Statistics',i)==T){
      return('I.T')
    }
    else if(grepl('Business',i)==T){
      return('Finance')
    }else if(grepl('Financial',i)==T){
      return('Finance')
    }else if(grepl('FinTech',i)==T){
      return('Finance')
    }
    else if(grepl('Business Analytics',i)==T){
      return('I.T')
    } else if(grepl('Cybersecurity',i)==T){
      return('I.T')
    }else if(grepl('Design',i)==T){
      return('I.T')
    }
    else if(grepl('Web Development',i)==T){
      return('I.T')
    }
    else if(grepl('Systems Engineering',i)==T){
      return('I.T')
    }
    else if(grepl('Machine Learning',i)==T){
      return('I.T')
    }else if(grepl('Introduction to HTML5',i)==T){
      return('I.T')
    }
    else if(grepl('TensorFlow',i)==T){
      return('I.T')
    }
    else if(grepl('Design',i)==T){
      return('I.T')
    }
    else{
      return('Others')
    }
  }
  
}

df$course <- sapply(df$course_title, courese_type)
table(df$course)
b <- subset(df, course == 'Others')
b$course_title
str(df)
df$course <- factor(df$course)
str(df)
summary(df$course_students_enrolled)


#df$course_students_enrolled <- as.numeric(df$course_students_enrolled)
str(df$course_students_enrolled)
df$number_of_students <- sapply(df$course_students_enrolled, changenum)
df$number_of_students
str(df)

library(ggplot2)
colnames(df)
pl <- ggplot(df, aes(course_difficulty))
pl + geom_bar(aes(color='black', fill='blue'))

pl <- ggplot(df, aes(course_Certificate_type))
pl + geom_bar(aes(color='black', fill= factor(course_difficulty)))

pl2 <- ggplot(df, aes(course_difficulty))
pl2 + geom_bar(aes(color='black', fill=factor(orgarnization)))



library(stringr)
library(dplyr)
library(tidyr)


df$st <- as.numeric(str_extract(df$course_students_enrolled,"[[:digit:]]+\\.*[[:digit:]]*"))
df$n[str_detect(df$course_students_enrolled,"k$")] <- 1000
df$n[str_detect(df$course_students_enrolled,"m$")] <- 1000000
df$n
df$st
#df$ <- df$st * as.numeric(df$n)
df <- df %>% mutate(stn = st* as.numeric(n))
str(df)
str(df)
colnames(df)

summary(df$stn)

subset(df, stn ==3200000)


pl3 <- ggplot(df, aes(course_rating, number_of_students))
pl4 <- pl3 + geom_point(aes(color=factor(orgarnization), alpha=1.5, size=1.5))
pl5 <-pl4 + xlab('Course Rating') + ylab('Number of Students') + ggtitle('Number of Students Vs Course Rating')
pl5 + theme_bw()



#subset(df, course_rating == 5)
#colored  by course type
pl3 <- ggplot(df, aes(course_rating, number_of_students))
pl4 <- pl3 + geom_point(aes(color=factor(course), alpha=1.5, size=1.5))
pl5 <-pl4 + xlab('Course Rating') + ylab('Number of Students') + ggtitle('Number of Students Vs Course Rating')
pl5 + theme_bw()
colnames(df)


#colored by certificate type
pl3 <- ggplot(df, aes(course_rating, number_of_students))
pl4 <- pl3 + geom_point(aes(color=factor(course_Certificate_type), alpha=1.5, size=1.5))
pl5 <-pl4 + xlab('Course Rating') + ylab('Number of Students') + ggtitle('Number of Students Vs Course Rating')
pl5 + theme_bw() 


#color by course dificulty
pl3 <- ggplot(df, aes(course_rating, number_of_students))
pl4 <- pl3 + geom_point(aes(color=factor(course_difficulty), alpha=1.5, size=1.5))
pl5 <-pl4 + xlab('Course Rating') + ylab('Number of Students') + ggtitle('Number of Students Vs Course Rating')
pl5 + theme_bw() 

help('dplyr')



###Select columns
df2 <- select(df, orgarnization,
              course_Certificate_type,
              course_rating,course_difficulty,
              course,stn)
head(df2)


str(df2)
####TRAIN AND TEST SPLIT
library(caTools)
sample <- sample.split(df2, SplitRatio = 0.7)
#####TRAIN DATA
train <- subset(df2, sample == T)
###TEST DATA
test <- subset(df2, sample == F)

model <- lm(stn ~ ., data = train)
summary(model)

predict_vals <- predict(model, test)
head(predict_vals)

mydf <- data.frame(test$stn, predict_vals)
head(mydf, 30)
library(neuralnet) 

str(df2)
train$orgarnization <- as.numeric(train$orgarnization)
train$course_Certificate_type <- as.numeric(train$course_Certificate_type)
train$course_difficulty <- as.numeric(train$course_difficulty)
train$course <- as.numeric(train$course)
trainn <- scale(train)
head(trainn)
model2 <- neuralnet(stn ~ ., data = trainn)
plot(model)
summary(model)

test$orgarnization <- as.numeric(test$orgarnization)
test$course_Certificate_type <- as.numeric(test$course_Certificate_type)
test$course_difficulty <- as.numeric(test$course_difficulty)
test$course <- as.numeric(test$course)
tests <- scale(test)
head(tests)

real_vals <- unscale(tests)

computed_vals <- compute(model2, test[,1:5])

head(computed_vals, )
library(randomForest)
model3 <- randomForest(stn ~ ., data = train)
summary(model3)

preds <- predict(model3, test)
preds
datafra <- data.frame(test$stn, preds,predict_vals)
head(datafra)

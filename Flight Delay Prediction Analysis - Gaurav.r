library(tidyverse)
library(lubridate)
library(ggplot2)
library(readxl)
library(openxlsx)
library(dplyr)
library(caTools)
library(pROC)
library(repr) 

df = read_excel("Flight_Delay_time_difference.xlsx")
head(df,n=3)

#sapply(df, class)

sapply(df, function(x) sum(is.na(x))) 

colnames(df)

names(df)[10] <- "diff_minutes"
names(df)[1] <- "schedtime"
colnames(df)

library(gridExtra)

par(mfrow = c(2, 2)) 
car1 = ggplot(data = df) +
  geom_bar(mapping = aes(x = carrier),fill="#FF9999", colour="grey")
des1 = ggplot(data = df) +
  geom_bar(mapping = aes(x = dest),fill = 'blue')
des1 = des1+ coord_flip()
ori1 = ggplot(data = df) +
  geom_bar(mapping = aes(x = origin),fill = 'green')
ori1 = ori1 + coord_flip()
del1 = ggplot(data = df) +
  geom_bar(mapping = aes(x = delay),fill = 'purple')
grid.arrange(car1,des1,ori1,del1,nrow=2 ,ncol=2)

options(repr.plot.width=8, repr.plot.height=5)

df %>%  
  group_by(carrier, delay) %>%  
  summarize(Count = n()) %>% 
  ggplot(aes(x=carrier, y=Count, fill=delay)) + 
  geom_bar(stat='identity', position= "dodge")


library(ggpie)

options(repr.plot.width=4, repr.plot.height=4)

ggpie(data = df, group_key = "weather", count_type = "full",label_info = "all",label_type = "horizon",label_pos = "in",label_size = 4)+ ggtitle("Weather stats") + 
  theme(plot.title = element_text(hjust = 0.5))

a = df %>% 
  group_by(carrier) %>% 
  summarise(Total_Delay_Minutes = sum(diff_minutes)) 
a

options(repr.plot.width=6, repr.plot.height=4)
dl5 = ggplot(a, aes(x = carrier, y = Total_Delay_Minutes) ) + geom_bar(aes(fill=Total_Delay_Minutes),stat = "identity") 
dl5

df1 = df
head(df1,n=7)

df1$schedtime <- NULL
df1$deptime <- NULL
head(df1,n=2)

df1$carrier<- as.numeric(factor(df1$carrier))
head(df1,n=2)

df1$dest<- as.numeric(factor(df1$dest))
df1$origin<- as.numeric(factor(df1$origin))
head(df1,n=7)

df1$delay <- ifelse(df1$delay == "ontime",1,0)
head(df1,n=7)

correlation_matrix <- round(cor(df1),2)
correlation_matrix
#head(correlation_matrix[, 1:9])

corrplot::corrplot(cor(df1))

colnames(df1)

set.seed(123)

sample <- sample.split(df1$delay, SplitRatio = 0.7)
train  <- subset(df1, sample == TRUE)
test   <- subset(df1, sample == FALSE)

dim(train)
dim(test)

#df2 <- train %>% mutate_at(c('carrier','dest', 'distance', 'origin', 'weather', 'dayweek', 'daymonth', 'diff_minutes'), ~(scale(.) %>% as.vector))
#df2
df2 <- train %>% mutate_all(~(scale(.) %>% as.vector))
head(df2,n=7)
dim(df2)

model <- glm(delay ~.,family=binomial(link='logit'),data=train)
summary(model)

model1 <- glm(delay ~ carrier + diff_minutes ,family=binomial(link='logit'),data=train)
summary(model1)

anova(model1, test="Chisq")

fitted.results <- predict(model1,test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$delay)
sprintf(misClasificError, fmt = '%#.3f')        # Apply sprintf function
k = 1-misClasificError
r = sprintf(k, fmt = '%#.2f')
print(paste('Accuracy',r))

probabilities <- model1 %>% predict(test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Ontime", "Delay")

df3 = test
head(df3,n=2)

df3$New_delay <- predict(model1,test, type = "response")
head(df3,n=3)

df4 <- cbind(df3,predicted.classes)
head(df4,n=3)

names(df4)[11] <- "Final_Prediction"
head(df4,n=3)

options(repr.plot.width=5, repr.plot.height=5)

h1 <- hist(df3$New_delay,
main="Predicted Probability Histogram",
xlab="Probability",
xlim=c(-0.5,1),
col="darkmagenta",
border="brown")
h1

roc(df3$delay,df3$New_delay)

g4 = ggroc(roc(df3$delay,df3$New_delay)) +
  theme_minimal() + 
  ggtitle("My ROC curve") + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed")
g4

a1 = auc(df3$delay,df3$New_delay)
b1= round(a1, digits = 2)
print(paste0("The Value of AUC: ", b1))




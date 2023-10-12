#task2
library(dplyr)  
df <- data.frame(select(X040522_Data_Mid_term_test_Final, c(2,3,4,5,9,23,26,8))  
                 %>%  filter(exchangename == 'HANOI STOCK EXCHANGE'))

set.seed(929)
df <- df[sample(1:nrow(df),100), ] 
as_tibble(df)
df <- rename(df,'Trade_credit'= receivable)
View(df)

#Check and replace NA 
summary(df)
sum(is.na(df))
which(is.na(df))
df$Trade_credit[is.na(df$Trade_credit)]=median(df$Trade_credit,na.rm=T)
df$Trade_credit[df$Trade_credit == 0] <- mean(df$Trade_credit[df$Trade_credit != 0])
df$totalasset[is.na(df$totalasset)]=median(df$totalasset,na.rm=T)
df$totalequity[is.na(df$totalequity)]=median(df$totalequity,na.rm=T)
df$totaldebt[is.na(df$totaldebt)]=median(df$totaldebt,na.rm=T)
df$totaldebt[df$totaldebt == 0] <- mean(df$totaldebt[df$totaldebt != 0])

sum(is.na(df))
#create size variance
unique(df$industry)
df$size <- ifelse(df$totalasset < quantile(df$totalasset, probs = 0.25),'1' ,
                  ifelse(df$totalasset < median(df$totalasset),'2',
                         ifelse(df$totalasset < quantile(df$totalasset, 
                                                         probs = 0.75) ,'3' , '4')))
options(scipen=999)
View(df)

#task3
#top 5 highest and lowest trade credit
df_max<-df %>% slice_max(Trade_credit, n = 5)
df_min<- df %>% slice_min(Trade_credit, n = 5)
options(scipen=999)
View(df_max)
View(df_min)

#descriptive statistics
sta<-df %>%
  group_by(size)  %>%
  summarize(mean_receivable = mean(Trade_credit),
            median_receivable = median(Trade_credit),
            min_receivable = min(Trade_credit),
            max_receivable = max(Trade_credit),
            std_receivable=sd(Trade_credit),
            count = n())
View(sta)


df$equity_class <-ifelse(df$totalequity < median(df$totalequity),'below median' 
                         ,'above median')

sta1<-df %>%
  group_by(equity_class)  %>%
  summarize(mean_receivable = mean(Trade_credit),
            median_receivable = median(Trade_credit),
            min_receivable = min(Trade_credit),
            max_receivable = max(Trade_credit),
            std_receivable=sd(Trade_credit),
            count = n())
View(sta1)

#task4
library(ggplot2)
#histogram of trade credit
ggplot(df, aes(x = Trade_credit, fill='Trade_credit')) +
  geom_histogram()+
  scale_x_continuous(labels = scales::comma)

#scatter plot of trade credit with the continuous variable
ggplot(df, aes(x = totalequity , y = Trade_credit)) +
  geom_point()+
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = "lm")+
  scale_x_continuous(labels = scales::comma)

#boxplot of trade credit with the discrete variable
df %>%
  filter(!is.na(Trade_credit), !is.na(size)) %>%
  ggplot(aes(x = size, y = Trade_credit, fill=size)) +
  geom_boxplot() +
  coord_flip()+
  scale_y_continuous(labels = scales::comma) 

#plot for combination of continuous, discrete variables and trade credit
ggplot(df, aes(x = Trade_credit, y = totalequity, color = as.factor(size))) +
  geom_point() +
  scale_color_manual(values = c("#270181", "yellow","green","red"))+
  scale_x_continuous(labels = scales::comma)

#task5
library(tidyverse)
#check linearity
plot(Trade_credit ~ totalequity, data=df)
plot(Trade_credit ~ size, data=df)
#check correlation
df$size <- as.numeric(df$size)
cor.test(df$size, df$totalequity) 

install.packages('car') 
library(car)
install.packages("stargazer")
library(stargazer)
#regression model
summary(Trade_credit.lm<-lm(Trade_credit ~  size +totalequity  +totaldebt , data = df))
stargazer(Trade_credit.lm, type ="text")
#check multicollinearity
car::vif(Trade_credit.lm) 

par(mfrow=c(2,2))
plot(Trade_credit.lm)

#check heteroskedasticity
install.packages('lmtest')
library(lmtest) 
bptest(Trade_credit.lm) 
shapiro.test(resid(Trade_credit.lm)) 

#improving model
summary(Trade_credit.lm1<-lm(log(abs(Trade_credit)) ~  log(abs(totalequity))+ log(abs(totaldebt)), data = df))
car::vif(Trade_credit.lm1)
bptest(Trade_credit.lm1)
shapiro.test(resid(Trade_credit.lm1))
par(mfrow=c(2,2))
plot(Trade_credit.lm1)

#task6
#Count the number of firms in an industry
for(i in df$industry)
{
  print(count(df, industry))
  break
}
#Visualize
ggplot(df, aes(x = industry, fill=as.factor(industry))) +
  geom_bar()

#Count the number of firms each industry and with trade credit above a certain value
for(i in df$industry)
{ print(df %>% 
          filter(Trade_credit > 6.074e+10) %>%
          group_by(industry) %>%
          count(industry))
  break
}



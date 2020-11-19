library(astsa)
library(lubridate)
library(fpp2)
library(tidyverse)
library(htmltools)
library(reshape2)
library(plotly)
library(stats)
library(FactoMineR)
library(dplyr)
library(ggfortify)
library(factoextra)
library(ggplot2)
library(lazyeval)
library(farver)
library(corrplot)
library(crosstalk)
library(ggpubr)
```

covid <- read.csv("covid-data.csv", header=T)



#Date
covid$weekday <- weekdays(as.Date(covid$date))
covid$week_N <- isoweek(ymd(covid$date))

#Ratio
weekly.averages=covid %>% filter(new_cases>100) %>% group_by(location,week_N) %>% summarize(weekly.mean=mean(new_cases, na.rm=T))
db_week_avg=covid %>% left_join(weekly.averages, by = c("location","week_N")) %>% filter(new_cases>100) %>% mutate(ratio.cases=new_cases/weekly.mean)

 

db_week_avg <- subset(db_week_avg,subset =week_N>34&location=="France"|location=="Russia"|location=="India"|location=="United States"|location=="Brazil"|location=="Italy"|location=="Mexico"|location=="Sweden"|location=="Germany")


db_daily_avg=db_week_avg%>% group_by(location,weekday, gdp_per_capita,human_development_index) %>% summarize(ratio_mean=mean(ratio.cases, na.rm=T))

db_daily_avg$weekday<-factor(db_daily_avg$weekday,levels=c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"),ordered=TRUE)

kruskal.test(db_daily_avg$ratio_mean~db_daily_avg$weekday)
ggqqplot(db_daily_avg$ratio_mean)
shapiro.test(db_daily_avg$ratio_mean)
ks.test(db_daily_avg$ratio_mean, "pnorm", mean=mean(db_daily_avg$ratio_mean), sd=sd(db_daily_avg$ratio_mean))
cor.test(formula = ~ new_cases + new_tests, data = covid, method="spearman")
cor.test(formula = ~ new_cases + new_tests, data = covid, method="pearson")


db_daily_avg$location=factor(db_daily_avg$location)

fig <- db_daily_avg %>%
  plot_ly(
    x = ~weekday,
    y = ~ratio_mean,
    frame = ~location,
    type = 'bar',
    color = ~location,
    showlegend = T)%>%
    layout(xaxis = list(autorange = TRUE),
         yaxis = list(autorange = TRUE),
         barmode = "overlay")

fig

shared <- SharedData$new(db_daily_avg)
p2 <- shared %>%
  plot_ly(
    x = ~location,
    y = ~ratio_mean,
    frame = ~weekday,
    type = 'bar',
    color = ~location,
    showlegend = T)%>%
  layout(xaxis = list(autorange = TRUE),
         yaxis = list(autorange = TRUE),
         barmode = "overlay")




# Scatterplot of house_price vs. employment rate
p1 <- shared %>%
  plot_ly(x = ~gdp_per_capita, y = ~human_development_index, frame = ~weekday, hoverinfo = "text", text = ~location)%>%
  add_markers()%>%
  layout(xaxis = list(autorange = TRUE),
         yaxis = list(autorange = TRUE))
  
# Polish the linked scatterplots
subplot(p1, p2, titleX = TRUE) %>% hide_legend() %>%
  layout(xaxis = list(autorange = TRUE),
         yaxis = list(autorange = TRUE))

#db_daily_avg
Lundi_pays<-filter(db_daily_avg,weekday=='lundi')
Mardi_pays<-filter(db_daily_avg,weekday=='mardi')
Mercredi_pays<-filter(db_daily_avg,weekday=='mercredi')
Jeudi_pays<-filter(db_daily_avg,weekday=='jeudi')
Vendredi_pays<-filter(db_daily_avg,weekday=='vendredi')
Samedi_pays<-filter(db_daily_avg,weekday=='samedi')
Dimanche_pays<-filter(db_daily_avg,weekday=='dimanche')
 
#creating the dataframe 
location <- Lundi_pays$location
Lundi <- c(Lundi_pays$ratio_mean)
Mardi <- c(Mardi_pays$ratio_mean)
Mercredi <- c(Mercredi_pays$ratio_mean)
Jeudi <- c(Jeudi_pays$ratio_mean)
Vendredi <- c(Vendredi_pays$ratio_mean)
Samedi <- c(Samedi_pays$ratio_mean)
Dimanche <- c(Dimanche_pays$ratio_mean)

pca_data_ratio_day <- data.frame(location,Lundi,Mardi,Mercredi,Jeudi,Vendredi,Samedi,Dimanche)
 
#ratio for PCA
rownames(pca_data_ratio_day) <- pca_data_ratio_day[,1] 

#Assigning row names from 1st column
pca_data_ratio_day[,1] <- NULL
res.pca <- PCA(pca_data_ratio_day, scale = T)
eig.val <- get_eigenvalue(res.pca)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 75))
autoplot(kmeans(pca_data_ratio_day, 4), data = pca_data_ratio_day, label = TRUE, loadings.label.vjust = 1.2)
par(mar=c(7,2,1,2))
data.scaled <- scale(pca_data_ratio_day)
 
# Calculate the (Euclidean) distances: data.dist
data.dist <- dist(data.scaled)
dend <- data.dist %>% hclust() %>% as.dendrogram()
plot(dend)


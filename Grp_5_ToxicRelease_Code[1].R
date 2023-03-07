install.packages("psych")
library("psych")
library("dplyr")
install.packages("tibble")
library("tibble")
library(plotly)

toxic <- read.csv("/Users/shashankrallapalli/Documents/Toxic_Release.csv")

#cleaning
toxic <- toxic %>%
  select(-c(X,X.1,X.2,X.3)) 
toxic$benzoghiperylene[is.na(toxic$benzoghiperylene)] <- mean(toxic$benzoghiperylene, na.rm = TRUE)
toxic$diisocyanates[is.na(toxic$diisocyanates)] <- mean(toxic$diisocyanates, na.rm = TRUE)
toxic$dioxinanddioxinlikecompounds[is.na(toxic$dioxinanddioxinlikecompounds)] <- mean(toxic$dioxinanddioxinlikecompounds, na.rm = TRUE)
toxic$hydrochloricacid1995andafteracid[is.na(toxic$hydrochloricacid1995andafteracid)] <- mean(toxic$hydrochloricacid1995andafteracid, na.rm = TRUE)
toxic$nhexane[is.na(toxic$nhexane)] <- mean(toxic$nhexane, na.rm = TRUE)
toxic$nitratecompounds[is.na(toxic$nitratecompounds)] <- mean(toxic$nitratecompounds, na.rm = TRUE)
toxic$polycyclicaromaticcompounds[is.na(toxic$polycyclicaromaticcompounds)] <- mean(toxic$polycyclicaromaticcompounds, na.rm = TRUE)
toxic$total_release_carcinogen[is.na(toxic$total_release_carcinogen)] <- mean(toxic$total_release_carcinogen, na.rm = TRUE)
toxic$total_release_metal[is.na(toxic$total_release_metal)] <- mean(toxic$total_release_metal, na.rm = TRUE)

summary(toxic)

subset1 <- toxic[toxic$year == "2013" | toxic$year == "2014",]
subset2 <- toxic[toxic$st == "MA" | toxic$st =="NJ" | toxic$st =="NY",]
describeBy(x=subset1, group = subset1$year)
describeBy(x=subset2, group = subset2$st)


#Exploratory data analysis
subset3 <- toxic[toxic$year == '2013',]
d3 <- subset3[order(subset3$total_release_carcinogen, decreasing = TRUE),]
d3 <- head(d3, n=10)

d1 <- subset3[order(subset3$total_release_metal, decreasing = TRUE),]
d1 <- head(d1, n=10)


fig1 <- plot_ly(d3, x = ~facility_name, y = ~total_release_carcinogen, type = 'bar', textposition = 'auto',
                marker = list(color = 'rgb(158,202,225)',
                              
                              line = list(color = 'rgb(8,48,107)', width = 1.5)))

fig1 <- fig1 %>% layout(title = "Facilities with most carcinogen relases",
                        
                        xaxis = list(title = "Facility name"),
                        
                        yaxis = list(autorange = F, range =c(0,80e6),title = "total release of carcinogen"))


fig1

fig2 <- plot_ly(d3, x = ~facility_name, y = ~total_release_metal, type = 'bar', textposition = 'auto',
                marker = list(color = 'rgb(158,202,225)',
                              
                              line = list(color = 'rgb(8,48,107)', width = 1.5)))

fig2 <- fig2 %>% layout(title = "Facilities with most Metal relases",
                        
                        xaxis = list(title = "Facility name"),
                        
                        yaxis = list(autorange = F, range = c(0,200e6),title = "total release of Metals"))


fig2



#Final project Inferential Statistics
#ANOVA
#Mean of total release of carcinogens for MA, NJ and NY for all years.
#Ho = All means are equal
#H1 = All means are not equal

alpha <- 0.05

sample <- subset2[c('total_release_carcinogen','st')]

sample$st <- as.factor(sample$st)
a <- aov(total_release_carcinogen ~ st, data = sample)
s <- summary(a)

df.numerator <- s[[1]][1,"Df"]
df.denominator <- s[[1]][2,"Df"]

f.value <- s[[1]][[1,"F value"]]
p.value <- s[[1]][[1,"Pr(>F)"]]

ifelse(p.value > alpha, 'Fail to reject null hypothesis', "reject null hypothesis")
TukeyHSD(a)

#Anova Test
#H0 = 
alpha <- 0.05

sample1 <- subset1[c('total_release_carcinogen','year')]

sample1$year <- as.factor(sample1$year)
a <- aov(total_release_carcinogen ~ year, data = sample1)
s <- summary(a)

df.numerator <- s[[1]][1,"Df"]
df.denominator <- s[[1]][2,"Df"]

f.value <- s[[1]][[1,"F value"]]
p.value <- s[[1]][[1,"Pr(>F)"]]

ifelse(p.value > alpha, 'Fail to reject null hypothesis', "reject null hypothesis")
TukeyHSD(a)

#Linear Regression
#Total toxic release is dependent on carcinogens and metals for all the years
#H0 = Total toxic release is dependent
#H1 = Total toxic release is independent
set.seed(9999)
New <- toxic%>%
  select(c("Total.Toxic.Releases.Per.Facility","total_release_carcinogen", "total_release_metal"))

correlation_matrix <- cor(New)
corrplot(correlation_matrix)

c <- lm(Total.Toxic.Releases.Per.Facility ~ total_release_carcinogen +
          total_release_metal, data = New)
summary(c)
scatterplot(data = New, Total.Toxic.Releases.Per.Facility ~ total_release_carcinogen | total_release_metal)

toxic$federal_facility <- as.factor(toxic$federal_facility)
 
#logistic regression
set.seed(123)
sample3 <- subset3%>%
  select(c(facility_name,federal_facility, total_release_carcinogen, Total.of.Water.and.Air,Total.Toxic.Releases.Per.Facility, total_release_metal,methanol,benzoghiperylene,hydrochloricacid1995andafteracid,toluene))
sample3 <- sample_n(sample3, 1000)
sample4 <- sample3%>%
  select(c(total_release_carcinogen, Total.of.Water.and.Air,Total.Toxic.Releases.Per.Facility, total_release_metal))
correlation_matrix <- cor(sample4)
corrplot(correlation_matrix)
sample3$federal_facility <- as.factor(sample3$federal_facility)
I <- createDataPartition(sample3$federal_facility, p = 0.7, list = FALSE)
Trn <- sample3[I,]
Tst <- sample3[-I,]

# glm function to make two models 
mdl1 <- glm(federal_facility~. , data = Trn, family = binomial(link = "logit"))
summary(mdl1)

mdl2 <- glm(federal_facility ~Total.Toxic.Releases.Per.Facility + Total.of.Water.and.Air, data = Trn, family = binomial(link = "logit"))
summary(mdl2)
coef(mdl2)
exp(coef(mdl2))
# A test data that is used to predict the model 
tst1 <- data.frame(Total.Toxic.Releases.Per.Facility = c(min(sample3$Total.Toxic.Releases.Per.Facility), mean(sample3$Total.Toxic.Releases.Per.Facility), max(sample3$Total.Toxic.Releases.Per.Facility)))
tst1$probs <- predict(mdl2, tst1, type = 'response')
tst1

#train dataset probabilities
probabilities.train <- predict(mdl2, newdata = Trn, type = 'response')
predicted.classes.min <- as.factor(ifelse(probabilities.train <= 0.5, 'Yes', 'No'))

confusionMatrix(predicted.classes.min, Trn$federal_facility, positive = "Yes")

#test set predictions
probabilities.test <- predict(mdl2, newdata = Tst, type = 'response')
predicted.classes.min <- as.factor(ifelse(probabilities.test >= 0.5, 'Yes', 'No'))

#Creating a confusion matrix 
confusionMatrix(predicted.classes.min, Tst$federal_facility, positive = NULL, prevalence = NULL, mode = "sens_spec")

#ROC Curve
rocplt <- roc(Tst$federal_facility, probabilities.test)
plot(rocplt, col = 'blue', ylab = 'Sensitivity - TP Rate', xlab = 'Specificity - FP Rate')

#Area under the curve 
AUC1 <- auc(rocplt)
AUC1

#Lasso regression
#Train and the test data 
set.seed(143)
subset4 <- toxic%>%
  select(-c(federal_facility,tri_facility_id, parent_company_name, parent_company_db_number, st, county,city,facility_name,street_address, latitude, longitude,zip))
sample5 <- sample_n(subset4, 1000)
Ind <- createDataPartition(sample5$Air.Pollutants, p = 0.7, list = FALSE)
Trn <- sample5[Ind,]
Tst <- sample5[-Ind,]

#matrix without a response variable
Trn_x= model.matrix(TRIMETHYLBENZENE_124 ~., Trn)[,-40]
Tst_x= model.matrix(TRIMETHYLBENZENE_124 ~., Tst)[,-40]

#Taking the response variable into another variable
Trn_y <- Trn$Air.Pollutants
Tst_y <- Tst$Air.Pollutants

#lamda value and plot

ds.lasso <- cv.glmnet(Trn_x, Trn_y)
plot(ds.lasso)

log(ds.lasso$lambda.min)
log(ds.lasso$lambda.1se)

#Fit model using lambda min
model1<- glmnet(Trn_x, Trn_y, alpha = 1, lambda = ds.lasso$lambda.min)
summary(model1)
coef(model1)

#Fit model using lambda 1se
model2<- glmnet(Trn_x, Trn_y, alpha = 1, lambda = ds.lasso$lambda.1se)
summary(model2)
coef(model2)

ols<- lm(Air.Pollutants ~., data =Trn)
coef(ols)
summary(ols)
plot(ols)
#Predict each model and find out RMSE value
predols<- predict(ols, new = Tst)
rmse(Tst$Air.Pollutants, predols)

predtrn<- predict(model2, newx = Trn_x)
rmse(Trn_y, predtrn)

predtst<- predict(model2, newx = Tst_x)
rmse(Tst_y, predtst)

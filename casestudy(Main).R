library(naniar)
library(dplyr)
library(ggplot2)
library(caret)
library(psych)
library(Hmisc)
## Step 1  - Read in Data
data=read.csv(file.choose())
names(data)
class(data)
summary(data)
n_complete(data) # to find the complete values

miss_count <-data %>%
  miss_scan_count(search = list("N/A","na","NA"))

#Missing Data
n_miss(data) # to find the number of missing data 
n_miss(data$Age)
n_miss(data$Female)
n_miss(data$Racegrp)
n_miss(data$Educ)
n_miss(data$Unmarried)
n_miss(data$Income)
n_miss(data$CareSource)
n_miss(data$Insured)
n_miss(data$Weight)
n_miss(data$Height)
n_miss(data$BMI)
n_miss(data$Obese)
n_miss(data$Waist)
n_miss(data$SBP)
n_miss(data$DBP)
n_miss(data$HDL)
n_miss(data$LDL)
n_miss(data$Total.Chol)
n_miss(data$Dyslipidemia)
n_miss(data$PVD)
n_miss(data$Activity)
n_miss(data$PoorVision)
n_miss(data$Smoker)
n_miss(data$Hypertension)
n_miss(data$Fam.Hypertension)
n_miss(data$Diabetes)
n_miss(data$Fam.Diabetes)
n_miss(data$Stroke)
n_miss(data$CVD)
n_miss(data$Fam.CVD)
n_miss(data$CHF)
n_miss(data$Anemia)
n_miss(data$CKD) # We have to predict 


#Proportion of Data 

prop_complete(data)
prop_miss(data)


#miss Var summary 
summary_miss<-miss_var_summary(data) #as per variable
summary_misscases<- miss_case_summary(data) # as per rows
miss_table<-miss_var_table(data) 

#using Groupby 
group_waist <- data %>%
  group_by(Waist)%>%
  miss_var_summary()

#visualization of missing data 

vis_miss(data) # provides an overview of the missing data
vis_miss(data, cluster = TRUE)


# Look at missing at variables and cases
gg_miss_var(data)
gg_miss_var(data,aes(x=x,y=y))+
  geom_bar(stat ='identity',aes(fill=category))
gg_miss_case(data, order_cases = FALSE)


gg_miss_var(data, facet = Racegrp)

#missing combinations that co-occur
gg_miss_upset(data)+
  geom_bar(aes(color='yellow',fill=category),stat ='identity')

mice_plot <- aggr(data, col=c('grey','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(train), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
install.packages("VIM")
library(VIM)
#Missingness in each variable changes across a factor
gg_miss_fct(x=data,fct = Activity)

#visualisation over the span of missingness
gg_miss_span(data,Unmarried, span_every = 2000)


#Explore the missing data Dependence 

shadow<-as_shadow(data)
bind<-bind_shadow(data_in)







out_sample=which(is.na(data$CKD)==1)
data_out=data[out_sample,]   ## the ones without a disease status
data_in=data[-out_sample,]   ## the ones with a disease status



## Step 2  - Missing Data
summary(data_in)
dim(data_in)

data_in%>%
  bind_shadow()%>%
  group_by(CKD_NA)%>%
  summarise(mean = mean(Age))


ggplot(data_in,
       aes(x = Weight))+
  geom_density()

data_in %>%
  bind_shadow()%>%
  ggplot(aes(x = Age,
             color = Weight_NA ))+
  geom_density()

data_in %>%
  bind_shadow() %>%
  ggplot(aes( x = Weight_NA,
              y = Age))+
  geom_boxplot()


is.na(data_in)
vis_miss(data_in)
gg_miss_var(data_in)
gg_miss_upset(data_in)
cor.ci(data_in, method = 'spearman')

?na.omit
data_in=na.omit(data_in)
dim(data_in)

## Step 3 and 4 - Correlation
cor(data_in)
summary(data_in)
data_new=model.matrix(~-1+Racegrp+CareSource,data=data_in)
summary(data_new)
data_in=data_in[,-c(4,8)]
data_in=cbind(data_in,data_new)
cor(data_in)
names(data_in)
data_in=data_in[,-33]
cor(data_in)
corPlot(data_in)
  ## any highly correlated - large relation to PCA


## Step 5 - Run a Regression
model=lm(CKD~.,data=data_in)
summary(model)

summary(predict(model))


## Step 6 - Screening tool

#  GET CREATIVE :))))  :)) ha ha 

## Hint:  Do you have Diabetes?  (Yes = c points or No = d points)

## PCA intro

#How do we plot all 33 variables?

dim(data_in)
install.packages('pca3d')
library(pca3d)

pca <- prcomp( data_in[,-32], scale.= TRUE )
pca3d( pca, group= data_in[,32] )


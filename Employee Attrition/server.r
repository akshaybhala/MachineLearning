# Importing Libraries
library(tidyverse)
library(dplyr)
library(arules)
library(ggplot2)
library(arulesViz)
library(shiny)
library(rsconnect)

#rsconnect::setAccountInfo(name='akshaybhala', token='225DBFB24AE6B118E38A76797BAB22C6', secret='vn8RdpMfbdPAmOKQWdA+ZIPdHZhHl4k9xV8RlBov')
                        

#Reading file using read_csv
myData <- read.csv("employee_attrition.csv")
myData[myData==""] <- NA

#Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Removing Outliers function
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  #boxplot(var_name, main="With outliers")
  #hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  #boxplot(var_name, main="Without outliers")
  #hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  #title("Outlier Check of", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  dt[as.character(substitute(var))] <- invisible(var_name)
  assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
  cat("Outliers successfully removed", "n")
  return(invisible(dt))
  
  
}

# Coverting the columns to factors

converttofactor <- function(vec)
{
  vec <- trimws(as.character(vec))
  vec <- as.factor(vec)
}

#Checking if outliers exist for numeric columns consisting NA's
summary(myData$DistanceFromHome)

outlierKD(myData, DistanceFromHome)

myData$DistanceFromHome[(is.na(myData$DistanceFromHome))]<- round(mean(myData$DistanceFromHome,na.rm=TRUE))

summary(myData$JobLevel)

#Replacing Na's using mode for ordinal values
myData$JobLevel[(is.na(myData$JobLevel))]<- getmode(myData$JobLevel)

summary(myData$PercentSalaryHike)

myData$PercentSalaryHike[(is.na(myData$PercentSalaryHike))]<- getmode(myData$PercentSalaryHike)

summary(myData$PerformanceRating)

myData$PerformanceRating[(is.na(myData$PerformanceRating))]<- getmode(myData$PerformanceRating)

summary(myData$RelationshipSatisfaction)

myData$RelationshipSatisfaction[(is.na(myData$RelationshipSatisfaction))]<- getmode(myData$RelationshipSatisfaction)

summary(myData$TotalWorkingYears)

outlierKD(myData, TotalWorkingYears)

myData$TotalWorkingYears[(is.na(myData$TotalWorkingYears))]<- round(mean(myData$TotalWorkingYears,na.rm=TRUE))

summary(myData$YearsSinceLastPromotion)

myData$YearsSinceLastPromotion[(is.na(myData$YearsSinceLastPromotion))]<- round(mean(myData$YearsSinceLastPromotion,na.rm=TRUE))

summary(myData$YearsWithCurrManager)

outlierKD(myData, YearsWithCurrManager)

myData$YearsWithCurrManager[(is.na(myData$YearsWithCurrManager))]<- round(mean(myData$YearsWithCurrManager,na.rm=TRUE))


#Replacing Na's using mode function for columns consisting characters

myData$Gender[(is.na(myData$Gender))]<- getmode(myData$Gender)

myData$OverTime[(is.na(myData$OverTime))]<- getmode(myData$OverTime)

#Checking are there any Na's left
sum(is.na(myData))


# Converting to Factors
myData$Education <- converttofactor(myData$Education)
myData$EnvironmentSatisfaction <- converttofactor(myData$EnvironmentSatisfaction)
myData$JobInvolvement <- converttofactor(myData$JobInvolvement)
myData$JobLevel <- converttofactor(myData$JobLevel)
myData$JobSatisfaction <- converttofactor(myData$JobSatisfaction)
myData$NumCompaniesWorked <- converttofactor(myData$NumCompaniesWorked)
myData$PerformanceRating <- converttofactor(myData$PerformanceRating)
myData$RelationshipSatisfaction <- converttofactor(myData$RelationshipSatisfaction)
myData$StockOptionLevel <- converttofactor(myData$StockOptionLevel)
myData$TrainingTimesLastYear <- converttofactor(myData$TrainingTimesLastYear)
myData$NumCompaniesWorked <- converttofactor(myData$NumCompaniesWorked)
myData$WorkLifeBalance<-converttofactor(myData$WorkLifeBalance)

myData$Attrition <- as.factor(myData$Attrition)
myData$BusinessTravel <- as.factor(myData$BusinessTravel)
myData$Department <- as.factor(myData$Department)
myData$EducationField <- as.factor(myData$EducationField)
myData$Gender <- as.factor(myData$Gender)
myData$JobRole <- as.factor(myData$JobRole)
myData$MaritalStatus <- as.factor(myData$MaritalStatus)
myData$OverTime <- as.factor(myData$OverTime)


str(myData)


df1 <- myData

# removing columns like EmployeeCount/EmployeeNumber/Over18/StandardHours
df1 <- df1[,c(-9,-10,-22,-27)] 


#Discretization
df1$Age <- discretize(df1$Age, method = "frequency", breaks = 3, 
                      labels = c("young", "adult", "old"), order = T)
df1$DailyRate <- discretize(df1$DailyRate, method = "frequency", breaks = 4, 
                            labels = c("low", "Medium", "High","Higher"), order = T)
df1$DistanceFromHome<-discretize(df1$DistanceFromHome,method = "frequency", breaks = 4, 
                                 labels = c("low", "Medium", "High","Higher"), order = T) 
df1$HourlyRate<-discretize(df1$HourlyRate,method = "frequency", breaks = 4, 
                           labels = c("low", "Medium", "High","Higher"), order = T)                  
df1$MonthlyIncome<-discretize(df1$MonthlyIncome,method = "frequency", breaks = 4, 
                              labels = c("low", "Medium", "High","Higher"), order = T)                  
df1$MonthlyRate<-discretize(df1$MonthlyRate,method = "frequency", breaks = 4, 
                            labels = c("low", "Medium", "High","Higher"), order = T)   
df1$PercentSalaryHike<-discretize(df1$PercentSalaryHike,method = "frequency", breaks = 4, 
                                  labels = c("<5%", "5%<hike<10%", "10%<Hike<20%",">20%"), order = T)
df1$YearsAtCompany<-discretize(df1$YearsAtCompany,method = "frequency", breaks = 4, 
                               labels = c("<5years", "5<Years<10", "10<Years<20",">20"), order = T)
df1$YearsInCurrentRole<-discretize(df1$YearsInCurrentRole,method = "frequency", breaks = 4, 
                                   labels = c("<5years", "5<Years<10", "10<Years<20",">20"), order = T)
df1$TotalWorkingYears<-cut(df1$TotalWorkingYears, breaks = 5,
                                 labels = c("<5years", "5<Years<10", "10<Years<20","20<years<25",">25"), order = T)
df1$YearsSinceLastPromotion<-cut(df1$YearsSinceLastPromotion, breaks = 5,
                                 labels = c("<5years", "5<Years<10", "10<Years<20","20<years<25",">25"), order = T)
df1$YearsWithCurrManager<-discretize(df1$YearsWithCurrManager,method = "frequency", breaks = 4, 
                                     labels = c("<5years", "5<Years<10", "10<Years<20",">20"), order = T)

#Converting data frame to transaction matrix
SS<-as(df1,"transactions")

#Server function
server<-function(input, output){
  rules <- reactive({Attrition_rules <- apriori(SS, parameter=list (supp= as.numeric(input$sup),conf = as.numeric(input$conf) , minlen= as.numeric(input$len)+1, maxlen = as.numeric(input$mlen),maxtime=as.numeric(input$time), target = "rules"), appearance = list (rhs=c("Attrition=No")))})
  
  rules1 <- reactive({Attrition_rules1 <- apriori(SS, parameter=list (supp= as.numeric(input$sup),conf = as.numeric(input$conf) , minlen= as.numeric(input$len)+1, maxlen = as.numeric(input$mlen),maxtime=as.numeric(input$time), target = "rules"), appearance = list (rhs=c("Attrition=Yes")))})
  
  output$plot <- renderPlot({
  Attrition_rules <-rules()
  
  p <- plot(Attrition_rules)
  print (p)}, height = 600)
  
  output$rules <- DT::renderDataTable({
    Attrition_rules <- rules()
    rulesdf <- DATAFRAME(Attrition_rules)
    rulesdf
    #inspect(head(sort(Attrition_rules, by='confidence'),10))
  })
  
   output$plot1 <- renderPlot({
    Attrition_rules1 <-rules1()
    
    q <- plot(Attrition_rules1)
    print (q)}, height = 600)
  
  output$rules1 <- DT::renderDataTable({
    Attrition_rules1 <- rules1()
    rules1df <- DATAFRAME(Attrition_rules1)
    rules1df
  })
}  


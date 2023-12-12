####   ########   Question - PEOPLE WITH A POOR HEALTH (GENERAL HEALTH STATUS)
## To set the working directory
setwd("C:/Users/DEBBIE/Music/RSTUDIO/Business inteligence/BI INDI ASS")

## Libraries which would be used for the analysis
library(readxl)  
library(ggplot2)
library(dplyr)   
library(skimr)
library(broom)  
library(margins)
library(stargazer)  
library(caret) 
library(haven)#to read dta. file
library(gridExtra)


### Call the necessary dataset #####
health2 <- read_dta("eqls_2007.dta")
skim(health2)

#############============= DATA RESTRICTIONS ======== ###################
########### =========== Selecting outcome and explanatory variables to be used for analaysis====###

# outcome variable for "In General your health is" is Y11_Q42.
# key explanatory: Y11_Q40f (how satisfied are you with your health), Y11_Q43(do you have any chronic illness or disability), Y11_Q44 (limited mobility due to illness) Y11_Q47c (waiting time to see a doctor), Y11_Q47a (what is the distance to the hospital), Y11_Q47d(cost of seeing a doctor), Y11_Q53a, 
#control variables; Y11_EmploymentStatus, Y11_Q19b, Y11_Q19c.

######## SELECTION OF THE VARIABLES TO BE USED FOR THE ANALYSIS
phealth <- health2 %>% select(Y11_Q42, Y11_Q43, Y11_Q44, Y11_EmploymentStatus, Y11_Q53a, Y11_Q19b, Y11_Q19c, Y11_Q40f, Y11_Q47a, Y11_Q47c, Y11_Q47d)

##############DESCRIPTIVE STATISTICS###############
skim(phealth)
summary(phealth)

table(phealth$Y11_Q42) 
table(phealth$Y11_Q43) 
table(phealth$Y11_Q44) 
table(phealth$Y11_EmploymentStatus)
table(phealth$Y11_Q53a) 
table(phealth$Y11_Q19b) 
table(phealth$Y11_Q19c) 
table(phealth$Y11_Q40f) 
table(phealth$Y11_Q47a) 
table(phealth$Y11_Q47c) 
table(phealth$Y11_Q47d) 

###########=========== identify missing values =============################
table(phealth$Y11_Q42, useNA = "ifany") #64 missing values
table(phealth$Y11_Q43, useNA = "ifany") #486 missing values
table(phealth$Y11_Q44, useNA = "ifany") #25751 missing values
table(phealth$Y11_EmploymentStatus, useNA = "ifany") 
table(phealth$Y11_Q53a, useNA = "ifany") #626 missing values
table(phealth$Y11_Q19b, useNA = "ifany") #129 missing values
table(phealth$Y11_Q19c, useNA = "ifany") #118 missing values
table(phealth$Y11_Q40f, useNA = "ifany") #329 missing values
table(phealth$Y11_Q47a, useNA = "ifany") #2399 missing values
table(phealth$Y11_Q47c, useNA = "ifany") #2670 missing values
table(phealth$Y11_Q47d, useNA = "ifany") #3739 missing values


############################# DATA TRANSFORMATIONS #######################
############## renaming and filtering variables for analysis #####################

# This creates a new variable called 'genhealth' by changing the Y11_Q42, illness' - Y11_Q43, 'limitedmob' - Y11_Q44, 
#the datacode book says that all "no" in Q43 should skip. and all yes should respond to Q44 Hence, Q44 is a filter question for Q43, meaning we know the value for the missings even a respondent has not answered the question. creating a new variable called 'limitedmob' 


phealth <- phealth %>% rename(genhealth = Y11_Q42)
phealth <- phealth %>% filter(!is.na(Y11_Q43)) %>% rename(illness = Y11_Q43) 
phealth <- phealth %>% rename(limitedmob = Y11_Q44) %>% mutate(limitedmob = case_when(limitedmob == 1 ~ 1, limitedmob == 2 ~ 2, TRUE ~ 3))
phealth <- phealth %>% rename(healthquality = Y11_Q53a) %>% filter(healthquality > 0)
phealth <- phealth %>% rename(employedstats = Y11_EmploymentStatus)
phealth <- phealth %>% rename(accomodation_rot = Y11_Q19b) %>% filter(accomodation_rot > 0)
phealth <- phealth %>% rename(accomodation_leak = Y11_Q19c) %>% filter(accomodation_leak > 0) 
phealth <- phealth %>% rename(health_sats = Y11_Q40f) %>% filter(health_sats > 0) 
phealth <- phealth %>% rename(distance_doc = Y11_Q47a) %>% filter(distance_doc > 0) 
phealth <- phealth %>% rename(wait_time = Y11_Q47c) %>% filter(wait_time > 0) 
phealth <- phealth %>% rename(cost_doc = Y11_Q47d) %>% filter(cost_doc > 0)


skim(phealth)

############# ================ SELECTING VARIABLES FOR ANALAYSIS ======#########################

health <- phealth %>% select(genhealth, healthquality, illness, limitedmob, cost_doc, employedstats, accomodation_leak, accomodation_rot, distance_doc, wait_time, health_sats, genhealth)

healthvis <- health %>% select(genhealth, healthquality, illness, limitedmob, cost_doc, employedstats, accomodation_leak, accomodation_rot, distance_doc, wait_time, health_sats, genhealth)
healthvis <- healthvis[complete.cases(healthvis), ]

skim(health)
#mutating outcome variable for analysis
health <- health %>% mutate(genhealthbin = ifelse(health$genhealth %in% c(1, 2, 3), 0, ifelse(health$genhealth %in% c(4, 5), 1, health$genhealth)))

health <- health[complete.cases(health), ]

############################# Mutating variables for analysis #################
#1. Explanatory variable Chronic illness or mental health 
health <- health %>% mutate(illness = ifelse(illness == 1,1,0))

#2. Limited mobility due to illness
health <- health %>% mutate(limitedmob = ifelse(limitedmob %in% c(1, 2), 1, 0))

#3. rot in accomodation (possible reasons behind bad health)
health <- health %>% mutate(accomodation_rot = case_when(accomodation_rot == 1 ~ "yes", accomodation_rot == 2 ~ "no"))
health <- health %>% mutate(accomodation_rot = ifelse(accomodation_rot == "yes",1,0))

#4. Leaks, damps in accomodation (possible reasons behind bad health)
health <- health %>% mutate(accomodation_leak = case_when(accomodation_leak == 1 ~ "yes", accomodation_leak == 2 ~ "no"))
health <- health %>% mutate(accomodation_leak = ifelse(accomodation_leak == "yes",1,0))

#5. Are you satisfied with your health rating
health <- health %>% mutate(health_sats = case_when(health_sats >= 1 & health_sats <= 4 ~ "dissatisfied", health_sats >= 5 & health_sats <= 10 ~ "satisfied")) %>% mutate(health_sats = ifelse(health_sats == "dissatisfied",1,0))

#6. Distance to a doctor 
health <- health %>% mutate(distance_doc = case_when(distance_doc == 1 ~ "verydifficult", distance_doc == 2 ~ "alildifficult", distance_doc == 3 ~ "notdifficult")) %>% mutate(distance_doc = ifelse(distance_doc == "verydifficult" | distance_doc == "alildifficult",1,0))

#7. waiting time to see a doctor
health <- health %>% mutate(wait_time = case_when(wait_time == 1 ~ "verydifficult", wait_time == 2 ~ "alildifficult", wait_time == 3 ~ "notdifficult")) %>% mutate(wait_time = ifelse(wait_time == "verydifficult" | wait_time == "alildifficult",1,0))

#8. cost implications of seeing a doctor
health <- health %>% mutate(cost_doc = case_when(cost_doc == 1 ~ "verydifficult", cost_doc == 2 ~ "alildifficult", cost_doc == 3 ~ "notdifficult")) %>% mutate(cost_doc = ifelse(cost_doc == "verydifficult" | cost_doc == "alildifficult",1,0))

# data selection, filter and variable transformation, selecting a variable to serve as a unique identifier
health <- health %>% mutate(healthqualitysq = healthquality * healthquality, cid = row_number())
table(health$healthquality, useNA = "ifany")

#nominal variables, use as factors
health$genhealthbin <- as.integer(health$genhealthbin)
health$healthquality <- as.integer(health$healthquality)
health$healthqualitysq <- as.integer(health$healthqualitysq)

skim(health)

#############VISUALIZATION##########
# Create a color palette with shades of blue
colors <- colorRampPalette(c("lightblue", "darkblue"))(length(unique(healthvis$genhealth)))

# Labels for factor(genhealth)
genhealth_labels <- c("1" = "Very Good", "2" = "Good", "3" = "Fair", "4" = "Bad", "5" = "Very Bad")

### Figure 1 shows the proportion of genhealth 
Figure1 <- ggplot(data = healthvis, aes(x = factor(genhealth), fill = factor(genhealth))) + geom_bar(data = subset(healthvis, genhealth %in% c("4", "5", "3")), position = "identity") + scale_fill_manual(values = c("skyblue", "darkblue", "blue"), labels = c("Fair", "Bad", "Very Bad")) +
  scale_x_discrete(labels = c("1" = "Very Good", "2" = "Good", "3" = "Fair", "4" = "Bad", "5" = "Very Bad")) + theme_classic() + labs(y = "proportion", x = "genhealth", title = "Proportion View of bad health") + theme(legend.position = "bottom", plot.title = element_text(size = 10, face = "bold.italic"),      axis.title.x = element_text(color = "darkred", size = 10, face = "italic"), axis.title.y = element_text(color = "darkred", size = 10, face = "italic"))
Figure1

### Figure 2 shows the proportion of bad health paired with respondents who have a chronic illness 
Figure2 <- ggplot(data = healthvis, aes(x = factor(illness), fill = factor(genhealth))) + geom_bar(data = subset(healthvis, genhealth %in% c("4", "5", "3"))) + scale_fill_manual(values = c("skyblue", "darkblue", "blue"), labels = c("Fair", "Bad", "Very Bad")) + labs(title = "Bad health (General Health Status) vs Illness", x = "Illness", y = "Proportion") + theme_classic() + scale_x_discrete(labels = c("1" = "Yes", "2" = "No")) + theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold.italic"), axis.text = element_text(size = 10), axis.title = element_text(size = 10, color = "darkred", face = "bold.italic"))
Figure2

### Figure 3 shows respondents who have limited mobility due to a illness and bad health
Figure3 <- ggplot(data = healthvis, aes(x = factor(genhealth), fill = factor(limitedmob))) + geom_bar(data = subset(healthvis, genhealth %in% c("4", "5", "3")), position = "fill", color = "skyblue") + scale_fill_manual(name = "Limitedmob", labels = c("yes severly", "yes to some extent", "no"), values = c("skyblue", "darkblue", "blue")) + labs(title = "Genhealth vs Limitedmob", x = "Genhealth", y = "limitedmob") + theme_classic() + scale_x_discrete(labels = c("1" = "Very Good", "2" = "Good", "3" = "Fair", "4" = "Bad", "5" = "Very Bad")) + theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 10), axis.text = element_text(size = 10), axis.title = element_text(size = 10, color = "darkred", face = "italic"))
Figure3

#### Figure4 shows the representation of bad health vs respondents who have rot in their accomodation
Figure4 <- ggplot(data = healthvis, aes(x = factor(accomodation_rot), fill = factor(genhealth))) + geom_bar(data = subset(healthvis, genhealth %in% c("4", "5", "3")), position = "fill", color = "skyblue") + scale_fill_manual(values = c("skyblue", "darkblue", "blue"), labels = genhealth_labels) + labs(title = "Bad Health Plot vs Rot in Accomodation", x = "Rot in accomodation", y = "") + scale_x_discrete(labels = c("1" = "Yes", "2" = "No")) + theme_minimal() + theme(plot.title = element_text(size=10, face="bold.italic"), axis.title.x = element_text(color = "darkred", size = 10, face = "italic"), axis.title.y = element_text(color = "darkred", size = 10, face = "italic")) 
Figure4

#Create Dashboard
grid.arrange(Figure1,Figure2,Figure3,Figure4)

############################# REGRESSION ############################
## Prediction analysis with linear regression model
## Dividing data into test and train via random selection

# 1. data split
set.seed(12345) ## This ensures the same random selection for everytime you run a model 
train <- health %>% sample_frac(0.80) ## 80% of the data is in the train dataset
test  <- anti_join(health, train, by = "cid") ## 20% of the data is in the train dataset

# 2. descriptive stats
skim(train)
skim(test)

# 3. fit the model
reg <- lm(data = train, genhealth ~ limitedmob + cost_doc + health_sats + illness + employedstats + accomodation_rot + accomodation_leak + distance_doc + wait_time + healthquality + healthqualitysq)
summary(reg)

#4 graph plot showing the fitted value 
train <- augment(reg, train)
ggplot(data = train) + geom_point(aes(y = .fitted, x = health_sats), alpha = 0.1, color = "navyblue") + geom_point(aes(y = genhealth, x = health_sats), alpha = 0.1, color = "blue") + theme_minimal() + labs(x = "general health", y = "satisfied with your health") + ggtitle("Fitted Values vs Actual Health Quality") + theme(plot.title = element_text(size = 10, face = "bold.italic"), axis.title = element_text(size = 10), axis.text = element_text(size = 10), legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#5. prediction
test <- augment(reg, newdata = test)
# the residual is the difference between customer_satisfaction and the .fitted which is the estimated
#   customer satisfaction. The mean of the residuals is zero by definition (assumption of OLS) but the 
#   square of the residuals are positive. 
test %>% summarise(RMSE = sqrt(mean(.resid^2)))
train %>% summarise(RMSE = sqrt(mean(.resid^2)))

# 6. new draw
train2 <- health %>% sample_frac(0.80)
test2  <- anti_join(health, train2, by = "cid")
reg2 <- lm(data = train2, genhealth ~ limitedmob + cost_doc + health_sats + illness + employedstats + accomodation_rot + accomodation_leak + distance_doc + wait_time + healthquality + healthqualitysq)
summary(reg2)
 
train2 <- augment(reg2, train2)         
test2 <- augment(reg2, newdata = test2)
test2 %>% summarise(RMSE = sqrt(mean(.resid^2)))
train2 %>% summarise(RMSE = sqrt(mean(.resid^2)))


###############DATA PREPARATION FOR LOGIT MODEL
# outcome variable; rating of general health ~ what is your general health

myformula <- formula(genhealthbin ~ limitedmob + cost_doc + health_sats + illness + employedstats + accomodation_rot + accomodation_leak + distance_doc + wait_time + healthquality + healthqualitysq)

logit <- glm(data = train, myformula, family = binomial(link = "logit"))
summary(logit)

# 3. calculate marginal effects
# get marginal effects with the margins() function
logit_marginal <- margins(logit) 
summary(logit_marginal)

# 4. interpretation

# 5. accuracy
# for logit models, you need to specify the prediction because the default produces log-odds
# specify "link" to get the xb with 1/(1+exp(-xb)) = response
logit_pred <- augment(logit, type.predict = "response", newdata = test2)
summary(logit_pred$.fitted)
# create dummy variable
logit_pred <- logit_pred %>% mutate(pred_genhealthbin_logit = ifelse(.fitted > 0.5,"verygood","verybad"))
# truth table
table(logit_pred$pred_genhealthbin_logit, logit_pred$genhealthbin, useNA = "ifany")
# accuracy: (5114+484)/6032 = 92.80%


# analysis
lm <- lm(data = train, formula = myformula)
logit <- glm(data = train, formula = myformula, family = binomial(link = "logit"))
stargazer(lm, logit, type = "text")


# Convert to factors with the same levels
test$genhealthbin <- factor(test$genhealthbin, levels = c(0, 1))
if (exists("pred_logit")) {
  pred_logit <- factor(ifelse(pred_logit >= 0.5, 1, 0), levels = c(0, 1))
} else {
  # Define pred_logit or assign appropriate values
  # pred_logit <- ...
}


# Percentage of accuracy - logit
pred_logit <- predict(logit, type = "response", newdata = test)
predicted <- round(pred_logit)
head(data.frame(observed = test$genhealthbin, predicted = predicted))
tab <- table(predicted, test$genhealthbin)
sum(diag(tab))/sum(tab)*100 #Accuracy of prediction is 93.02%

# Compute sensitivity and specificity
pred_logit <- predict(logit, type = "response", newdata = test)
sensitivity <- ifelse(pred_logit >= 0.5, 1, 0)
specificity <- ifelse(pred_logit < 0.5, 0, 1)

# Create a data frame with observed and predicted values
results <- data.frame(observed = test$genhealthbin, sensitivity = sensitivity, specificity = specificity)

# Calculate sensitivity and specificity
sensitivity <- sum(results$observed == 1 & results$sensitivity == 1) / sum(results$observed == 1) * 100
specificity <- sum(results$observed == 0 & results$specificity == 1) / sum(results$observed == 0) * 100

# Print sensitivity and specificity
cat("Sensitivity:", sensitivity, "%\n")
cat("Specificity:", specificity, "%\n")

# accuracy: 93.02%
# sensitivity:  64.11%
# specificity:  3.39%


#the accuracy of the logistic model is slightly higher, which would suggest that it is a better fit. 



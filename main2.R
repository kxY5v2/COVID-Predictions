#Set working directory
setwd("C:/Users/zero/Desktop/Intro to Data Science/final_project")

# Function to ensure packages are downloaded
EnsurePackage<-function(x) {
  x <- as.character(x)
  
  if (!require(x,character.only=TRUE)) {
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

# Packages to ensure
EnsurePackage("dplyr")
EnsurePackage("lubridate")
EnsurePackage("readr")
EnsurePackage("ggplot2")
EnsurePackage("kernlab")
EnsurePackage("e1071")
EnsurePackage("workflows")
EnsurePackage("parsnip")
EnsurePackage("recipes")
EnsurePackage("glmnet")
EnsurePackage("tidyverse")
EnsurePackage("tidyquant")
EnsurePackage("timetk")
EnsurePackage("yardstick")

# Importing Data from File
raw_data <- read_csv("United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")
# Make a copy to not mess with original
data <- raw_data

# Importing Second Data Source
dailyData <- read_csv("national-history_1.csv")
jData = dailyData
jData = jData[,1:13]
jData = jData[,-2:-12]
newnames = c("date", "cases")
colnames(jData) = newnames
jData$date = as.Date(jData$date, "%m/%d/%y")

# Munging the Data
data=data[,1:8]
data=data[,-4:-7]
data=data[data$state != "RMI",]
data=data[data$state != "NYC",]
data=data[data$state != "PR",]
data=data[data$state != "AS",]
data=data[data$state != "GU",]
data=data[data$state != "FSM",]
data=data[data$state != "PW",]
data=data[data$state != "MP",]
# Order data by state alphabetically
data=data[order(data$state),]

# Monthyly Total Cases
data$submission_date = as.Date(data$submission_date, "%m/%d/%y")
month_total = data %>%
  select(submission_date, state, tot_cases, tot_death) %>%
  filter(submission_date == as.Date("2020-01-31") 
         | submission_date == as.Date("2020-02-29")
         | submission_date == as.Date("2020-03-31") 
         | submission_date == as.Date("2020-04-30")
         | submission_date == as.Date("2020-05-31") 
         | submission_date == as.Date("2020-06-30") 
         | submission_date == as.Date("2020-07-31") 
         | submission_date == as.Date("2020-08-31")
         | submission_date == as.Date("2020-09-30") 
         | submission_date == as.Date("2020-10-31")
         | submission_date == as.Date("2020-11-30")
         # Otional to grab 
         # Most recent submission at time of writing
         | submission_date == as.Date("2020-12-04")
  )

#########################################################################################################
#                                   LINEAR MODEL
#########################################################################################################
# Find total cases for country in january
jan_cases <- subset(month_total, submission_date == as.Date("2020-01-31"))
# Remove DC
jan_cases <- jan_cases[-8,]
jan_cases<- jan_cases$tot_cases
jan_sum <- sum(jan_cases)

# Find total cases for country in February
feb_cases <- subset(month_total, submission_date == as.Date("2020-02-29"))
# Remove DC
feb_cases <- feb_cases[-8,]
feb_cases<- feb_cases$tot_cases
feb_sum <- sum(feb_cases)

# Find total cases for country in March
march_cases <- subset(month_total, submission_date == as.Date("2020-03-31"))
# Remove DC
march_cases <- march_cases[-8,]
march_cases<- march_cases$tot_cases
march_sum <- sum(march_cases)

# Find total cases for country in April
april_cases <- subset(month_total, submission_date == as.Date("2020-04-30"))
# Remove DC
april_cases <- april_cases[-8,]
april_cases<- april_cases$tot_cases
april_sum <- sum(april_cases)

# Find total cases for country in May
may_cases <- subset(month_total, submission_date == as.Date("2020-05-31"))
# Remove DC
may_cases <- may_cases[-8,]
may_cases<- may_cases$tot_cases
may_sum <- sum(may_cases)

# Find total cases for country in June
june_cases <- subset(month_total, submission_date == as.Date("2020-06-30"))
# Remove DC
june_cases <- june_cases[-8,]
june_cases<- june_cases$tot_cases
june_sum <- sum(june_cases)

# Find total cases for country in July
july_cases <- subset(month_total, submission_date == as.Date("2020-07-31"))
# Remove DC
july_cases <- july_cases[-8,]
july_cases<- july_cases$tot_cases
july_sum <- sum(july_cases)

# Find total cases for country in August
august_cases <- subset(month_total, submission_date == as.Date("2020-08-31"))
# Remove DC
august_cases <- august_cases[-8,]
august_cases<- august_cases$tot_cases
august_sum <- sum(august_cases)

# Find total cases for country in September
september_cases <- subset(month_total, submission_date == as.Date("2020-09-30"))
# Remove DC
september_cases <- september_cases[-8,]
september_cases<- september_cases$tot_cases
september_sum <- sum(september_cases)

# Find total cases for country in October
october_cases <- subset(month_total, submission_date == as.Date("2020-10-31"))
# Remove DC
october_cases <- october_cases[-8,]
october_cases<- october_cases$tot_cases
october_sum <- sum(october_cases)

# Find total cases in country in November
novemberber_cases <- subset(month_total, submission_date == as.Date("2020-11-30"))
# Remove DC
novemberber_cases <- novemberber_cases[-8,]
novemberber_cases<- novemberber_cases$tot_cases
novemberber_sum <- sum(novemberber_cases)

# Most recent
dec_cases <- subset(month_total, submission_date == as.Date("2020-12-04"))
# Remove DC
dec_cases = dec_cases[-8,]
dec_cases = dec_cases$tot_cases
dec_sum <- sum(dec_cases)

# Approximate populations from:
# https://multpl.com/united-states-population/table/by-month
jan_pop   <- 329140000
feb_pop   <- 329240000
march_pop <- 329340000
april_pop <- 329460000
may_pop   <- 329590000
june_pop  <- 329730000
july_pop  <- 329880000
aug_pop   <- 330050000
sep_pop   <- 330220000
oct_pop   <- 330380000
nov_pop   <- 330530000
# From Census.gov
dec_pop   <- 330693000

# For testing Data [jan - oct]
cases_pm <- c(jan_sum, feb_sum, march_sum, april_sum, may_sum, 
              june_sum, july_sum, august_sum, september_sum, october_sum)
# Population per month
population_pm <- c(jan_pop, feb_pop, march_pop, april_pop, may_pop, june_pop, july_pop,
                   aug_pop, sep_pop, oct_pop)

# Making a dataframe
df <- data.frame(population_pm, cases_pm)

# Plotting dataframe, cases to population
cases_to_pop <- plot(df)

# Making the model
model <- lm(formula = cases_pm ~ population_pm, data = df)
summary(model)
        
# Abline
abline(model)

#Predicting November cases
pred_model = lm(formula = cases_pm ~ population_pm, data = df)
nov_pred = data.frame(population_pm = 330550000)
print('November Predicted Cases')
prediction <- predict(pred_model, nov_pred, type="response")
prediction

#plotting prediction

pred_cases_pm <- c( feb_sum, march_sum, april_sum, may_sum, 
                    june_sum, july_sum, august_sum, september_sum, october_sum, prediction)
pred_pop_pm <- c( feb_pop, march_pop, april_pop, may_pop, june_pop, july_pop,
                  aug_pop, sep_pop, oct_pop, nov_pop)

novgpred <- ggplot(pred_model,aes((x=pred_pop_pm),(y=pred_cases_pm), group = 1))
novgpred <- novgpred + geom_line() + geom_point() 
novgpred <- novgpred + geom_text(aes(label = pred_cases_pm))
novgpred <- novgpred + theme(axis.text.x = element_text(angle = 45, hjust = 1))
novgpred

#plotting actual november
cases_with_nov_pm <- c(jan_sum, feb_sum, march_sum, april_sum, may_sum, 
                       june_sum, july_sum, august_sum, september_sum, october_sum, novemberber_sum)
population_with_nov_pm <- c(jan_pop, feb_pop, march_pop, april_pop, may_pop, june_pop, july_pop,
                            aug_pop, sep_pop, oct_pop, nov_pop)
novgactframe <- data.frame(population_with_nov_pm, cases_with_nov_pm)

novgact <- ggplot(novgactframe,aes((x=population_with_nov_pm),(y=cases_with_nov_pm), group = 1))
novgact <- novgact + geom_line() + geom_point() 
novgact <- novgact + geom_text(aes(label = cases_with_nov_pm))
novgact <- novgact + theme(axis.text.x = element_text(angle = 45, hjust = 1))
novgact

# November actual
print("November Actual Cases:")
novemberber_sum

# Lets see if we can better the model by removing the first two months
new_casespm = cases_pm[3:10]
# Matching Populations
new_pop_pm = population_pm[3:10]
# Make it into a data frame
new_df = data.frame(new_pop_pm, new_casespm)
plot(new_df)

# Updated model
u_model <- lm(formula = new_casespm ~ new_pop_pm, data = new_df)
summary(u_model)

# Abline
abline(u_model)

# New prediction
new_nov_pred = data.frame(new_pop_pm = 330550000)
new_prediction <- predict(u_model, new_nov_pred, type = "response")
new_prediction

#plotting better prediction
newer_prediction <- unname(new_prediction)

new_casespm <- c(march_sum, april_sum, may_sum, 
                 june_sum, july_sum, august_sum, september_sum, october_sum, newer_prediction)
new_pop_pm <- c(march_pop, april_pop, may_pop, june_pop, july_pop,
                aug_pop, sep_pop, oct_pop, nov_pop)

novpred2 <- ggplot(new_df,aes((x=new_pop_pm),(y=new_casespm), group = 1))
novpred2 <- novpred2 + geom_line() + geom_point() 
novpred2 <- novpred2 + geom_text(aes(label = new_casespm))
novpred2 <- novpred2 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
novpred2

# New prediction
new_nov_pred = data.frame(new_pop_pm = 330550000)
new_prediction <- predict(u_model, new_nov_pred, type = "response")
new_prediction

# Differnece in Prediction
difference = new_prediction - prediction
difference

# Notes
print("Removing the first two months fixed out model by over 500,000 cases.")
print("But why isn't it more accurate?")
print("There was an large increase in cases from october to november that couldn't have been
      predicted by a linear model")
print("The cause of the spike?")
print("Probably the holidays.")

# Predicting December with March through November
to_dec_cases = c(march_sum, april_sum, may_sum, june_sum, july_sum, 
                 august_sum, september_sum, october_sum, novemberber_sum)
to_dec_pop = c(march_pop, april_pop, may_pop, june_pop, july_pop,
               aug_pop, sep_pop, oct_pop, nov_pop)

# New Dataframe
dec_df = data.frame(to_dec_pop, to_dec_cases)
plot(dec_df)

# New model
to_dec = lm(formula = to_dec_cases ~ to_dec_pop, data = dec_df)
summary(to_dec)

# Abline
abline(to_dec)

# Prediction
predict_pop = data.frame(to_dec_pop = dec_pop)
predict(to_dec, predict_pop, type = "response")

# Comparison
print("The actual count of December cases so far (taken the 4th) is ")
dec_sum

print("Not too bad!")

#plot

decgpred <- ggplot(dec_df,aes((x=to_dec_pop), (y=to_dec_cases), group=1))
decgpred <- decgpred + geom_line() + geom_point() 
decgpred <- decgpred + geom_text(aes(label = to_dec_cases))
decgpred <- decgpred + theme(axis.text.x = element_text(angle = 45, hjust = 1))
decgpred 

#########################################################################################################
#                                   TIMESERIES MODEL
#########################################################################################################
# Jackson's Prediction Model
train_data = jData %>% filter( date < ymd("2020-10-1"))
test_data = jData %>% filter( date >= ymd("2020-11-1"))

recipe_spec_timeseries = recipe(cases ~ ., data = train_data) %>%
  step_timeseries_signature(date) 

bake(prep(recipe_spec_timeseries), new_data = train_data)

recipe_spec_final = recipe_spec_timeseries %>%
  step_rm(date) %>%
  step_rm(contains("iso"),
          contains("second"), contains("minute"), contains("hour"),
          contains("am.pm"), contains("xts")) %>%
  step_interact(~ date_month.lbl * date_day) %>%
  step_interact(~ date_month.lbl * date_mweek) %>%
  step_interact(~ date_month.lbl * date_wday.lbl * date_yday) %>%
  step_dummy(contains("lbl"), one_hot = TRUE)

bake(prep(recipe_spec_final), new_data = train_data)

model_spec_glmnet = linear_reg(mode = "regression", penalty = 10, mixture = 0.7) %>%
  set_engine("glmnet")

workflow_glmnet = workflow() %>%
  add_recipe(recipe_spec_final) %>%
  add_model(model_spec_glmnet)

workflow_glmnet

workflow_trained = workflow_glmnet %>% fit(data = train_data)

prediction_tbl = workflow_trained %>%
  predict(test_data) %>%
  bind_cols(test_data)

prediction_tbl



predPlot =   ggplot(aes( x = date), data = jData) +
  geom_line(aes(y = cases)) +
  labs(title = "Daily Covid Cases For the United States") +
  geom_line(aes(y = .pred), data = prediction_tbl, color="darkred", size = 1.5)

predPlot

prediction_tbl %>% metrics(cases, .pred)




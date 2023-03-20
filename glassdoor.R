# Load R libraries we’ll use.
library(stargazer)
library(broom)
library(dplyr)

# Load employee data
mydata <- read.csv("C:/Users/julon/Downloads/Glassdoor Gender Pay Gap.csv")

# Create five employee age bins to simplify number of age groups.
mydata$age_bin <- 0
mydata$age_bin <- ifelse(mydata$Age < 25, 1, mydata$age_bin) 
mydata$age_bin <- ifelse(mydata$Age >= 25 & mydata$Age < 35, 2, mydata$age_bin) 
mydata$age_bin <- ifelse(mydata$Age >= 35 & mydata$Age < 45, 3, mydata$age_bin) 
mydata$age_bin <- ifelse(mydata$Age >= 45 & mydata$Age < 55, 4, mydata$age_bin) 
mydata$age_bin <- ifelse(mydata$Age >= 55, 5, mydata$age_bin) 

# Take the natural logarithm of base pay (for percentage pay gap interpretation in regressions).
mydata$log_base <- log(mydata$BasePay, base = exp(1))

# Create dummy indicator for gender (male = 1, female = 0).
mydata$male <- ifelse(mydata$Gender == "Male", 1, 0) # Male = 1, Female = 0

# Create an overall table of summary statistics for the data.
stargazer(mydata, type = "html", out = "summary.htm")
browseURL("summary.htm")

# Create a table showing overall male-female pay differences in base pay.
summary_base <- group_by(mydata, Gender)
summary_base <- summarise(summary_base, meanBasePay = mean(BasePay, na.rm = TRUE),
                          medBasePay = median(BasePay, na.rm = TRUE), cnt = sum(!(is.na(BasePay))) )
View(summary_base)

# How are employees spread out among job titles?
summary_job <- group_by(mydata, JobTitle, Gender)
summary_job <- summarise(summary_job, meanTotalPay = mean(totalPay, na.rm = TRUE), cnt =
                           sum(!(is.na(JobTitle))) ) %>% arrange(desc(JobTitle, Gender))
View(summary_job)

# No controls. ("unadjusted" pay gap.)
model1 <- lm(log_base ~ Male, data = mydata)

# Add controls for age, education and performance evaluations.
model2 <- lm(log_base ~ Male + PerfEval + age_bin + Education, data = mydata)

# Add all controls. ("adjusted" pay gap.)
model3 <- lm(log_base ~ Male + PerfEval + age_bin + Education + Dept + Seniority + JobTitle, data = mydata)

# Publish a clean table of regression results.
stargazer(model1, model2, model3, type = "html", out = "results.htm")
browseURL("results.htm")

# All controls with job title interaction terms.
job_results <- lm(log_base ~ Male*JobTitle + PerfEval + age_bin + Education + Seniority + Dept, data = mydata)

# Publish a clean table of regression results.
stargazer(job_results, type = "html", out = "job.htm")
browseURL("job.htm")

eachjob_results <- lm(log_base ~ Male*JobTitle + Male + PerfEval + age_bin + Education + Seniority + Dept, data = mydata)
stargazer(eachjob_results, type = "html", out = "eachjob.htm")
browseURL("eachjob.htm")
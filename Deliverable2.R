#PART 1
file <- read.csv("salaries.csv",stringsAsFactors = T) #Open the csv and convert string into factors
#View(file)
print(summary(file[,c("work_year","experience_level","employment_type","job_title","salary_in_usd","employee_residence","remote_ratio","company_location","company_size")])) #Print the summary of every content except salary and salary currency (removed later) 

#PART 2
#Exercise 1
library(lubridate)
file$work_year <- year(ymd(paste0(file$work_year, ",2,2"))) 
#View(file)

#Exercise 2 -> Not necessary

#Exercise 3
file <- file[,c("work_year","experience_level","employment_type","job_title","employee_residence","remote_ratio","company_location","company_size","salary_in_usd")] #Remove not relevant contents (salary and salary currency -> salary in usd ) and put salary_in_usd as last column
#View(file)

#Exercise 4
elements <- boxplot(file$salary_in_usd)
print(elements$out) #Find the outliers, we don´t remove them (explanation on the report)

#Exercise 5
set.seed(123)  #Prepare the seed to set 2% of the values of salary in usd to NA
na_percent <- 0.02
num_values_na <- round(nrow(file) * na_percent)
na_index <- sample(1:nrow(file), num_values_na) #Obtaint random indexes for the rows that are going to be converted to NA
file$salary_in_usd[na_index] <- NA #Asign NA to those indexes

set.seed(321)  #Repeat for remote ratio
na_index2 <- sample(1:nrow(file), num_values_na) #Obtaint random indexes for the rows that are going to be converted to NA
file$remote_ratio[na_index2] <- NA #Asign NA to those indexes
#View(file)

not_na <- file[-which(is.na(file$salary_in_usd)), "salary_in_usd"]
file[which(is.na(file$salary_in_usd)), "salary_in_usd"] <- rep(sample(min(not_na):max(not_na), num_values_na, T)) #Generate random values for salary_in_usd in missing values
file[which(is.na(file$remote_ratio)), "remote_ratio"] <- sample(c(0,50,100),num_values_na,T) #Generate random values for remote ratio in missing values

#Exercise 6

file$nomalized_salary <- (file$salary_in_usd - min(file$salary_in_usd))/(max(file$salary_in_usd) - min(file$salary_in_usd))

#Exercise 7

km <- 40 #We have calculated an average of 40 daily km
consumption <- 7.5/100 #Average car consumption per km
gas_expense <- km*consumption*260*1.32 #Average yearly gas expense with 260 year/working days and 1.32$/l gas
file$salary_after_gas_expense <- file[,"salary_in_usd"]-(100-file[,"remote_ratio"])/100*gas_expense #New column with an estimation of the salary after gas expenses



#Exercise 8
cuantiles <- quantile(file$salary_in_usd, probs = c(0.25, 0.5, 0.75))

assign_salary_cat <- function(salary) {
  if (salary <= cuantiles[1]) {
    return("lowest 25%")
  } else if (salary <= cuantiles[2]) {
    return("lowest 50%")
  } else if (salary <= cuantiles[3]) {
    return("highest 50%")
  } else {
    return("highest 75%")
  }
}

file$salary_category <- sapply(file$salary_in_usd, assign_salary_cat)
View(file)

#Excercise 9
save(file, file = "RDataDeliverable2.RData")


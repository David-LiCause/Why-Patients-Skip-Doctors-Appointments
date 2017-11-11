
library(dplyr)
library(tidyr)
library(ggplot2)
library(yhat)
library(ggthemes)
library(Cairo)
library(scales)

options(scipen = 99)

# Import the data
appt <- read.csv("KaggleV2-May-2016.csv", header=T, stringsAsFactors = F) 
# Convert gender and outcome variable to numeric
appt$Gender <- as.numeric(as.factor(appt$Gender)) - 1
appt[appt$No.show=="No", "No.show"] <- 0
appt[appt$No.show=="Yes", "No.show"] <- 1
appt$No.show <- as.numeric(appt$No.show)
# Extract the length of time in days between the patient scheduling the appointment and the appointment date
appt$schedule_time_days <- as.numeric(as.Date(appt$AppointmentDay) - as.Date(appt$ScheduledDay))
# Extract the day of the week from the appointment date timestamp
appt$appt_day <- factor(weekdays(as.Date(appt$AppointmentDay)))
# Extract month from the appointment date timestamp
appt$appt_month <- factor(months(as.Date(appt$AppointmentDay)))
# Convert neighborhood to factor
appt$Neighbourhood <- factor(appt$Neighbourhood)
# Select relevant columns for ANOVA and dominance analysis 
appt <- appt %>%
  select(14, 3, 6, 8:13, 15:17, 7)

# ANOVA for day of week
appt_day <- appt %>%
  select(appt_day, No.show)
av_day <- aov(No.show ~ appt_day, data=appt_day)
summary(av_day)

# Visualize difference in no show averages across days of the week
appt_day$appt_day <- factor(appt_day$appt_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
appt_day$No.show <- as.character(appt_day$No.show)
appt_day[appt_day$No.show==1, "No.show"] <- "No Show"
appt_day[appt_day$No.show==0, "No.show"] <- "Attended Appointment"
ggplot(appt_day, aes(x=appt_day)) + 
  geom_bar(aes(fill=No.show)) +
  ggtitle("Doctor's Appointments by Day of Week") +
  labs(x="", y="Number of Appointments") +
  theme_minimal() +
  scale_colour_tableau() +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(size=18, hjust=.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=11),
        axis.text.x = element_text(angle = 90)) 

# ANOVA for month of appointment
appt_month <- appt %>%
  select(appt_month, No.show)
av_month <- aov(No.show ~ appt_month, data=appt_month)
summary(av_month)

# Visualize difference in no show averages across months of the year
appt_month$No.show <- as.character(appt_month$No.show)
appt_month$appt_month <- factor(appt_month$appt_month, levels=c("April", "May", "June"))
appt_month[appt_month$No.show==1, "No.show"] <- "No Show"
appt_month[appt_month$No.show==0, "No.show"] <- "Attended Appointment"
ggplot(appt_month, aes(x=appt_month)) + 
  geom_bar(aes(fill=No.show)) +
  ggtitle("Doctor's Appointments by Month") +
  labs(x="", y="Number of Appointments") +
  theme_minimal() +
  scale_colour_tableau() +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(size=18, hjust=.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=11),
        axis.text.x = element_text(angle = 90)) 

# ANOVA for neighborhood
appt_nbhd <- appt %>%
  select(Neighbourhood, No.show)
av_nbhd <- aov(No.show ~ Neighbourhood, data=appt_nbhd)
summary(av_nbhd)

# Visualize difference in no show averages across neighborhoods
appt_nbhd_sum <- appt_nbhd %>%
  group_by(Neighbourhood) %>%
  summarize(pct_no_shows = mean(No.show, na.rm=T)*100) %>%
  arrange(desc(pct_no_shows))
appt_nbhd_sum$Neighbourhood <- factor(as.character(appt_nbhd_sum$Neighbourhood), levels = c(as.character(appt_nbhd_sum$Neighbourhood)))
ggplot(appt_nbhd_sum, aes(x=Neighbourhood, y=pct_no_shows)) + 
  geom_bar(stat="identity",  fill="#3683c1") +
  ggtitle("Doctor's Appointments by Neighborhood") +
  labs(x="", y="Percentage of No Shows") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(size=18, hjust=.5),
        axis.title = element_text(size=11),
        axis.text.x=element_blank())

# Get pearson correlation between continous predictor variables and no show status
temp <- appt %>%
select(Gender, Age, Scholarship, Hipertension, Diabetes, Alcoholism, Handcap, SMS_received, schedule_time_days)
temp_cor <- data.frame(apply(temp, 2, function(x) cor.test(x, appt$No.show, method='p')$estimate))
colnames(temp_cor) <- "pearson_correlation_coeff"
temp_cor <- temp_cor %>%
mutate(feature = rownames(temp_cor)) %>%
select(2,1) %>%
arrange(desc(abs(pearson_correlation_coeff)))
temp_cor$feature <- factor(temp_cor$feature, levels = temp_cor$feature)
ggplot(temp_cor, aes(x=feature, y=pearson_correlation_coeff)) + 
geom_bar(stat="identity", fill="#3683c1") +
ggtitle("Correlation with Outcome Variable by Feature") +
labs(x="", y="Pearson Correlation Coefficiant") +
theme_minimal() +
scale_y_continuous(labels = comma) +
expand_limits(y = c(-.2, .2)) +
guides(fill=FALSE) +
theme(plot.title = element_text(size=18, hjust=.5),
axis.title = element_text(size=11),
axis.text = element_text(size=11),
axis.text.x = element_text(angle = 90)) 

# Perform dominance analysis
# Conduct all possible subsets regression on the NPS data 
aps <- aps(appt, dv = "No.show", ivlist = c("Gender", "Age", "Scholarship", "Hipertension", "Diabetes", "Alcoholism", "Handcap", "SMS_received", "schedule_time_days", "appt_day", "appt_month"))
# With the output from all possible subsets regression, conduct dominance analysis
da <- dominance(aps)
total_variance_pred <- sum(da$GD)
da_results <- data.frame(da$GD)
da_results$feature <- row.names(da_results)
colnames(da_results) <- c("general_dominance_weights", "feature")
da_results <- da_results %>%
select(2,1) %>%
mutate(da_results_relative = general_dominance_weights / sum(general_dominance_weights, na.rm=T)) %>%
arrange(desc(da_results_relative))

da_results[,c(2,3)] <- da_results[,c(2,3)]*100
da_results$feature <- factor(da_results$feature, levels = c(da_results$feature))

ggplot(da_results, aes(x=feature, y=general_dominance_weights)) + 
geom_bar(stat="identity", fill="#3683c1") +
expand_limits(y = 10) +
ggtitle("Dominance Analysis - General Dominance Weights") +
labs(x="", y="Percentage Variance Explained (%)") +
theme_minimal() +
scale_y_continuous(labels = comma) +
theme(plot.title = element_text(size=18, hjust=.5),
axis.title = element_text(size=11),
axis.text = element_text(size=11),
axis.text.x = element_text(angle = 90)) 

ggplot(da_results, aes(x=feature, y=da_results_relative)) + 
  geom_bar(stat="identity", fill="#3683c1") +
  expand_limits(y=100) +
  ggtitle("Dominance Analysis - Relative Dominance Weights") +
  labs(x="", y="Relative Percentage Variance Explained (%)") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(size=18, hjust=.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=11),
        axis.text.x = element_text(angle = 90)) 

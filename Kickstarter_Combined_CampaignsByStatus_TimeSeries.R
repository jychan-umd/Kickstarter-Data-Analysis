#______ Starting with the cleaned file (12 Nov 2018) ______ #

Kickstarter_Complete_Cleaned <- read.csv("/users/Janice/Documents/Grad school 2018/INFM600/Team project/Kickstarter_Complete_Cleaned.cs")

#_____ Owen Henry _____#

drop_cols <- names(Kickstarter_Complete_Cleaned) %in% c("name", "country", "deadline", "usd.pledged")

dataset <- Kickstarter_Complete_Cleaned[!drop_cols]

#_____ Vyjayanthi Kamath _____#

names(dataset)
table(dataset$state)
View(dataset)


#_____ Time Series Analysis - Owen Henry _____#

dataset$launched <- as.Date(dataset$launched, format = "%m/%d/%Y" )

install.packages("ggplot2")

dataset$launched_year <- format(dataset$launched,"%Y")

dataset$launched_month <- format(dataset$launched, "%B")

success_fail_subset <- subset(dataset, state=='successful' | state=='failed')

success_subset <- subset(dataset, state=='successful')

failed_subset <- subset(dataset, state == 'failed')

all_aggr_data = aggregate(success_fail_subset$state, by=list(Category = dataset$launched_year), FUN=length)

success_aggr = aggregate(success_subset$state, by=list(Category = success_subset$launched_year), FUN=length)

failed_aggr = aggregate(failed_subset$state, by=list(Category = failed_subset$launched_year), FUN=length)

plot(aggr_data$Category, aggr_data$x, type = "l")

plot(success_aggr$category, success_aggr$x, type = "l")

plot(failed_aggr$Category, failed_aggr$x, type = "l")

#_____ Successful Campaigns Analysis - Janice Chan ______#

# looking at only successful campaigns

successfulCampaigns <- subset(dataset,state=="successful")

# below is on successful campaigns where currency is USD, answering 3 initial questions

successfulUSD <- subset(successfulCampaigns,currency=="USD")

# work on pledged for all
meanPledgedSuccessfulUSD <- mean(successfulUSD$pledged, na.rm = TRUE)
medianPledgedSuccessfulUSD <- median(successfulUSD$pledged, na.rm = TRUE)

# clean up of backers + work on all backers - DID NOT EXCLUDE THOSE WITH 0 BACKERS, COME BACK TO THIS
class(successfulUSD$backers)
successfulUSD2 <- successfulUSD
successfulUSD2$backers <- as.numeric(as.character(successfulUSD$backers))
meanBackersSuccessfulUSD <- mean(successfulUSD2$backers, na.rm = TRUE)
medianBackersSuccessfulUSD <- median(successfulUSD2$backers, na.rm = TRUE)

# clean up of goal + work on all goals
class(successfulUSD$goal)
successfulUSD3 <- successfulUSD2
successfulUSD3$goal <- as.numeric(as.character(successfulUSD2$goal))
meanGoalSuccessfulUSD <- mean(successfulUSD3$goal, na.rm = TRUE) # There are some goals set to 0.01 - should we include them?
medianGoalSuccessfulUSD <- median(successfulUSD3$goal, na.rm = TRUE)


# by categories
meanPledgedByCat <- aggregate(successfulUSD3$pledged, by=list(category=successfulUSD3$main_category), FUN=mean, na.rm = TRUE)
colnames(meanPledgedByCat) <- c("Category", "MeanPledged")
meanPledgedByCat

medianPledgedByCat <- aggregate(successfulUSD3$pledged, by=list(category=successfulUSD3$main_category), FUN=median, na.rm = TRUE)
colnames(medianPledgedByCat) <- c("Category", "MedianPledged")
medianPledgedByCat

meanBackersByCat <- aggregate(successfulUSD3$backers, by=list(category=successfulUSD3$main_category), FUN=mean, na.rm = TRUE)
colnames(meanBackersByCat) <- c("Category", "MeanBackers")
meanBackersByCat

medianBackersByCat <- aggregate(successfulUSD3$backers, by=list(category=successfulUSD3$main_category), FUN=median, na.rm = TRUE)
colnames(medianBackersByCat) <- c("Category", "MedianBackers")
medianBackersByCat

meanGoalByCat <- aggregate(successfulUSD3$goal, by=list(category=successfulUSD3$main_category), FUN=mean, na.rm = TRUE)
colnames(meanGoalByCat) <- c("Category", "MeanGoal")
meanGoalByCat

medianGoalByCat <- aggregate(successfulUSD3$goal, by=list(category=successfulUSD3$main_category), FUN=median, na.rm = TRUE)
colnames(medianGoalByCat) <- c("Category", "MedianGoal")
medianGoalByCat

# plotting work - Outliers? Y-axis on Mean Pledged plot?

plot(meanPledgedByCat$Category, meanPledgedByCat$MeanPledged, type = "p")

plot(medianPledgedByCat$Category, medianPledgedByCat$MedianPledged, type = "p")


plot(meanBackersByCat$Category, meanBackersByCat$MeanBackers, type = "p")

plot(medianBackersByCat$Category, medianBackersByCat$MedianBackers, type = "p")

plot(meanGoalByCat$Category, meanGoalByCat$MeanGoal, type = "p")

plot(medianGoalByCat$Category, medianGoalByCat$MedianGoal, type = "p")


#______ Failed Campaigns Analysis - Vyjayanthi Kamath _____#

failed<-subset(dataset, currency == 'USD' & state=="failed")
View(failed)
table(failed$currency)

failed_final<-failed
failed_final$backers<-as.numeric(as.character(failed$backers))
failed_final$goal<-as.numeric(as.character(failed$goal))

#finding mean and median backers
mean_backers<-mean(failed_final$backers, na.rm=TRUE)
median_backers<-median(failed_final$backers, na.rm=TRUE)


#finding mean and median pledged amounts
cat_pledged_mean<-aggregate(failed_final$pledged, by=list(category=failed_final$main_category), FUN=mean,
                              na.rm = TRUE)
cat_pledged_med<-aggregate(failed_final$pledged, by=list(category=failed_final$main_category), FUN=median,
                             na.rm = TRUE)

#finding mean and median goal
cat_goal_mean<-aggregate(failed_final$goal, by=list(category=failed_final$main_category), 
                           FUN=mean, na.rm = TRUE)
cat_goal_median<-aggregate(failed_final$goal, by=list(category=failed_final$main_category), 
                         FUN=median, na.rm = TRUE)

#finding mean and median backers
cat_backers_mean<-aggregate(failed_final$backers, by=list(category=failed_final$main_category), 
                              FUN=mean, na.rm = TRUE)

cat_backers_med<-aggregate(failed_final$backers, by=list(category=successfulUSD3$main_category), 
                             FUN=median, na.rm = TRUE)







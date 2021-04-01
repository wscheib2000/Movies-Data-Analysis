library(ggplot2)


movies_data <- read.csv("C:/Users/Will/Documents/tmdb_5000_movies.csv", stringsAsFactors = FALSE)
movies_data <- movies_data[,c(1,12,13,18)]


movies_data <- movies_data[-nchar(movies_data$release_date)!=0,]
movies_data <- movies_data[movies_data$budget!=0,]
movies_data$perc_return <- 100*movies_data$revenue/movies_data$budget



movies_data$days_since_jan_1 <- NA
for(i in c(1:length(movies_data$title))){
  year <- substr(movies_data[i,2],1,4)
  movies_data[i,6] <- as.numeric(movies_data[i,2]-as.Date(paste0(year,"-01-01")))
}

movies_data$dotw <- as.POSIXlt(movies_data$release_date)$wday

movies_data <- movies_data[complete.cases(movies_data),]

weekly_means <- data.frame(c(1:52),NA)
names(weekly_means) <- c("week", "median")
movies_data$week <- NA
for(i in c(0:51)){
  weekly_means[i+1,2] <- median(movies_data[movies_data$days_since_jan_1 >= i*7 & movies_data$days_since_jan_1 < (i+1)*7,5])
  movies_data[movies_data$days_since_jan_1 >= i*7 & movies_data$days_since_jan_1 < (i+1)*7,"week"] <- i+1
}
movies_data[is.na(movies_data$week)==TRUE,"week"] <- 52

movies_data$month <- as.numeric(substr(movies_data$release_date, 6, 7))

monthly_means <- data.frame(c(1:12),NA)
names(monthly_means) <- c("month", "median")
for (i in c(1:12)){
  monthly_means[i,2] <- median(movies_data[movies_data$month==i,5])
}

daily_means <- data.frame(c(0:6),NA)
names(daily_means) <- c("dotw", "median")
for (i in c(0:6)){
  daily_means[i+1,2] <- median(movies_data[movies_data$dotw==i,5])
}

#Eliminate outliers
movies_data <- movies_data[movies_data$perc_return < 1000,]

#Generate ggplots
ggplot(monthly_means, aes(factor(month), median)) + geom_point()
ggplot(movies_data, aes(factor(month),perc_return)) + geom_boxplot()
ggplot(weekly_means, aes(factor(week), median)) + geom_point()
ggplot(movies_data, aes(factor(week),perc_return)) + geom_boxplot()
ggplot(daily_means, aes(factor(dotw),median)) + geom_point()
ggplot(movies_data, aes(factor(dotw),perc_return)) + geom_boxplot()

monthly_pvals <- data.frame(c(1:12),NA)
names(monthly_pvals) <- c("month","p.value")
for (i in c(1:12)){
  monthly_pvals[i,2] <- t.test(movies_data[movies_data$month==i,5],mu=mean(movies_data[,5]))$p.value
}
monthly_pvals$less.05 <- monthly_pvals$p.value < 0.05
monthly_pvals$less.01 <- monthly_pvals$p.value < 0.01

weekly_pvals <- data.frame(c(1:52),NA)
names(weekly_pvals) <- c("week","p.value")
for (i in c(1:52)){
  weekly_pvals[i,2] <- t.test(movies_data[movies_data$week==i,5],mu=mean(movies_data[,5]))$p.value
}
weekly_pvals$less.05 <- weekly_pvals$p.value < 0.05
weekly_pvals$less.01 <- weekly_pvals$p.value < 0.01

daily_pvals <- data.frame(c(0:6),NA)
names(daily_pvals) <- c("dotw","p.value")
for (i in c(0:6)){
  daily_pvals[i+1,2] <- t.test(movies_data[movies_data$dotw==i,5],mu=mean(movies_data[,5]))$p.value
}
daily_pvals$less.05 <- daily_pvals$p.value < 0.05
daily_pvals$less.01 <- daily_pvals$p.value < 0.01
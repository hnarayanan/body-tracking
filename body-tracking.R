# Load useful libraries
library(TTR)

# Read the data file
data <- read.csv("body-data.csv", head=TRUE)

# Human-friendly names and units for the different variables in the
# data file
name <- function(variable) {
    full_names <- c("Date",
                    "Person A's weight",
                    "Person A's fat content",
                    "Person A's water content",
                    "Person A's muscle content",
                    "Did Person A go to the gym?",
                    "Person B's weight",
                    "Person B's fat content",
                    "Person B's water content",
                    "Person B's muscle content",
                    "Did Person B go to the gym?")
    number <- match(variable, names(data))
    return(full_names[number])
}

unit <- function(variable) {
    units <- c("yyyy-mm-dd",
               "kg", "%", "%", "%",
               "yes/no",
               "kg", "%", "%", "%",
               "yes/no")
    number <- match(variable, names(data))
    return(units[number])
}

# Goal values for the different quantities
goal <- function(variable) {
    goals <- c(NA, 70.0, 18.0, NA, NA, 1, 54.0, 22.0, NA, NA, 1)
    number <- match(variable, names(data))
    return(goals[number])
}

# Time to maintain these goals
average_period <- 7

# Open file for plotting
graphics.off()
pdf("body-results.pdf", width=19, height=6)
par(mfrow=c(1, 3))
par(mar=c(5, 5, 3, 2))

# Let R know that our date is actually a date
data$date <- as.Date(data$date)

# List of interesting time-varying quantities
variables <- c("a.mass", "a.fat", "a.musc",
	       "b.mass", "b.fat", "b.musc")

for (variable in variables) {

    # Get some descriptions of the recorded data
    variable_name <- name(variable)
    variable_unit <- unit(variable)
    variable_goal <- goal(variable)
    variable_range <- range(data[, variable], na.rm=TRUE)
    goal_exists <- !is.na(variable_goal)

    # Expand the plot ranges if the goal lies outside the data
    if(goal_exists) {
        if (variable_goal < variable_range[1]) {
            variable_range[1] <- variable_goal
        }
        if (variable_goal > variable_range[2]) {
            variable_range[2] <- variable_goal
        }
    }

    # Assign colours to points based on whether we went to the gym
    gym_variable <- paste(substr(variable, 1, 2), "gym", sep="")
    went_to_gym <- factor(data[, gym_variable],
                          labels=c("black", "red"))
    went_to_gym[which(is.na(went_to_gym))] <- "black"

    # Plot the recorded data
    plot(data$date, data[, variable],
         xlab="Date", ylab=paste(variable_name,
                                 " (", variable_unit, ")", sep=""),
         ylim=variable_range,
         cex.lab=1.75,
         col=went_to_gym)

    # Superimpose the exponential moving average over the last days
    lines(data$date, EMA(data[, variable], average_period))

    # Superimpose the goal if one exists
    if(goal_exists) {
        abline(h = rep(variable_goal, dim(data)[1]), col="red")
    }

    # Generate the legend
    gym_days <- sum(data[, gym_variable], na.rm=TRUE)
    total_days <- as.numeric(range(data$date)[2] -
                             range(data$date)[1]) + 1
    gym_title <- paste("Gym days: ", gym_days, " out of ", total_days,
                       " (about ", format(gym_days/total_days*100,
                                          digits=2), " %)", sep="")

    change_per_day <- lm(data[, variable]~data$date)$coefficients[2]
    fit_title <- paste("Weekly moving average (about",
                       format(change_per_day, digits=2),
                       variable_unit, "per day)")

    # Position the legend according to the trend in the data
    if(change_per_day < 0.0) {
        position <- "topright"
    } else {
        position <- "bottomright"
    }

    # Display the legend
    if(goal_exists) {
        days_to_goal <- (variable_goal - data[, variable]
                         [dim(data)[1] - 1])/change_per_day
        goal_title <- paste("Ideal goal:", variable_goal,
                            variable_unit, "(about",
                            format(days_to_goal, digits=2), "days)")
        legend(position,
               pch=c('o', '-', '-'),
               y=c(gym_title, fit_title, goal_title),
               col=c("red", "black", "red"))
    } else {
        legend(position,
               pch=c('o', '-'),
               y=c(gym_title, fit_title),
               col=c("red", "black"))
    }

}

# Close plot file
graphics.off()

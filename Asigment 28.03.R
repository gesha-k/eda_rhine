library(data.table)
library(ggplot2)

runoff_day <- readRDS('data/runoff_day.rds')
runoff_month <- readRDS('./data/runoff_month.rds')
runoff_summary <- readRDS('./data/runoff_summary.rds')

#1Transform runoff_stats to tidy format and then use it to plot mean, median, minimum and maximum for each location as a scaterplot (x axis station, y axis runoff) with different colors and point types for each statistic.
stations <- runoff_day[, .(mean_day = round(mean(value), 0),
                                  sd_day = round(sd(value), 0),
                                  min_day = round(min(value), 0),
                                  max_day = round(max(value), 0)), by = sname]

stations_tidy <- melt(stations, id.vars = 'sname', variable.name = 'stats', value.name = 'runoff')

ggplot(stations_tidy, aes(sname, runoff, shape = stats, col = stats)) + 
  geom_point(aes(col = stats, shape = stats))

#2 Estimate the skewness and coefficient of variation for each record (a) as a new column in runoff_stats and (b) as a new data.table.
stations[, variation_coefficient := sd_day / mean_day, by = sname]
dt_variation_coefficient_skewness <- stations[, .(sname, skewness, variation_coefficient)]

#3 Can you plot each boxplot (facet) of monthly runoff with different fill colour according to the runoff class?
runoff_classes <- runoff_summary[, .(sname, runoff_class)]
runoff_monthly_class <- runoff_month[runoff_classes, on = 'sname']

ggplot(runoff_monthly_class, aes(x = factor(month), y = value, fill = runoff_class)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free')

#4 Use boxplot to plot daily runoff per station. What do you observe regarding outliers? Why do you think this happens?
ggplot(runoff_day, aes(x = sname, y = value)) +
  geom_boxplot()

#5 Create your own classes for area and altitude and plot them in a scater plot similar to this one.
colours <-  c("yellow", "blue", "green", "grey")

runoff_summary[, area_class := factor('small')]
runoff_summary[area >= 11000 & area < 110000, area_class := factor('medium')]
runoff_summary[area >= 110000, area_class := factor('large')]

runoff_summary[, alt_class := factor('low')]
runoff_summary[altitude >= 100 & altitude < 400, alt_class := factor('medium')]
runoff_summary[altitude >= 400, alt_class := factor('high')]
runoff_summary

dt <- runoff_summary[, .(sname, area, alt_class)]
classes <- stations[dt, on = 'sname']

ggplot(classes, aes(x = mean_day, y = area, col = sname, cex = alt_class)) +
  geom_point() +
  scale_color_manual(values = colorRampPalette(colours)(20)) +
  theme_bw()

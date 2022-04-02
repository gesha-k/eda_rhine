#Assigment 01.04.2022
# 1. In our boxplot comparison of DOMA, BASR and KOEL we have used summer and winter 
##period. Can you repeat it for annual and monthly data? Is there is some useful new 
##information presented?
runoff_year_key[year <= 2010, age_range := factor('before_2010')]
runoff_year_key[year > 2010, age_range := factor('after_2010')]

ggplot(runoff_year_key, aes(age_range, value, fill = age_range)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Age Range") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

runoff_month_key[year <= 2010, age_range := factor('before_2010')]
runoff_month_key[year > 2010, age_range := factor('after_2010')]

ggplot(runoff_month_key, aes(factor(month), value, fill = age_range)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Month") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

# 2. In their research, Middelkoop and colleagues also mentioned changes in the 
##high/low runoff. Do our data agree with their results? We define high runoff as 
##the daily runoff above the 0.9 quantile and low runoff as the daily runoff below the
##0.1 quantile. Then we can estimate the mean high/low runoff per station. Finally, we 
##also compare the number of days with values above/below 0.9 and 0.1 correspondingly 
##(hint: .N function in data.table might help).

library(data.table)
library(ggplot2)

precip_day <- readRDS('data/raw/precip_day.rds')
colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")

precip_day[, month := month(date)]
precip_day[month == 12 | month == 1 | month == 2, season := 'winter']
precip_day[month == 3 | month == 4 | month == 5, season := 'spring']
precip_day[month == 6 | month == 7 | month == 8, season := 'summer']
precip_day[month == 9 | month == 10 | month == 11, season := 'autumn']
precip_day[, season := factor(season, levels = c('winter', 'spring', 'summer', 'autumn'))]

precip_day[, year := year(date)]
precip_winter <- precip_day[season == 'winter', 
                            .(value = sum(value)), by = year]
precip_summer <- precip_day[season == 'summer', 
                            .(value = sum(value)), by = year]
year_thres <- 1990
to_plot <- rbind(cbind(precip_winter, season = factor('winter')), 
                 cbind(precip_summer, season = factor('summer'))) 

to_plot[year < year_thres, period := factor('pre_1990')]
to_plot[year >= year_thres, period := factor('aft_1990')]
to_plot[year < year_thres, period := factor('pre_1990')]
to_plot[year >= year_thres, period := factor('aft_1990')]
ggplot(to_plot[year >= 1960], aes(season, value, fill = period)) +
  geom_boxplot() +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Precitation") +
  theme_bw()
ggplot(to_plot[season == 'summer' & year >= 1960], aes(x = year, y = value)) +
  geom_line(col = colset_4[3])+
  geom_point(col = colset_4[3])+
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Summer Precipitation") +
  theme_bw()

ggplot(to_plot[season == 'winter' & year >= 1960], aes(x = year, y = value)) +
  geom_line(col = colset_4[3])+
  geom_point(col = colset_4[3])+
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Winter Precipitation") +
  theme_bw()

# 3. How sensitive are slopes to adding new data? Redo the 1950-today regression 
##plots, but instead of 2016, use data till 2010. What do you observe? What if you 
##used linear regression instead of loess?

runoff_winter[, value_norm := scale(value), sname]
runoff_summer[, value_norm := scale(value), sname]
n_stations <- nrow(runoff_summary)

ggplot(runoff_winter[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff") +
  theme_bw()
ggplot(runoff_summer[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Summer runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff") +
  theme_bw()
ggplot(runoff_winter[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'lm', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff") +
  theme_bw()
ggplot(runoff_summer[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'lm', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Summer runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff") +
  theme_bw()

# Explorer’s questions

# 1. In retrospect, is DOMA a representative station? Why do you think its behaviour 
##is so different than the other stations?
# Answer: In my opinion DOMA cannot be representative station because we can see
##that data from DOMA is very different from other stationn(because of high altitude)

# 3. What are your thoughts about the changes in Rhine runoff after completing EDA?
# Answer: We can see that runoff during the period of research is changing. I think it happens because of global heating




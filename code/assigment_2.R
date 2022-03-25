
ggplot(data = runoff_stations, aes(x = lon, y = lat, col = altitude)) +
  geom_point() + 
  geom_text(label = runoff_stations$sname) + 
  scale_color_gradient(low = 'dark green', high = 'brown') +
  theme_bw()

library(data.table)
library(ggplot2)
runoff_stations <- readRDS('./data/runoff_stations_raw.rds')
dt <- runoff_stations[, .(sname, area, altitude)]
head(dt)
runoff_stations [, z2 := abs(rnorm(20))]
runoff_stations[, c('sname', 'area', 'altitude')]
ggplot(data = runoff_stations, aes(x = area, y = altitude, col = size)) +
  geom_point(aes(colour = z2)) +
  geom_text(aes(area, altitude, label = sname, colour = area))
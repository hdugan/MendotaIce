########### Reddit Data Viz Challenge - Dec 2018 
# https://www.reddit.com/r/dataisbeautiful/comments/a2p5f0/battle_dataviz_battle_for_the_month_of_december/
# By Hilary Dugan @hildug

# Load libraries
library(tidyverse)
library(lubridate)
# library(devtools)
# install_github('thomasp85/transformr')
library(transformr)

ice = read_csv('~/Dropbox/RandomR/Mendota_Ice/MendotaIceData.csv')

############ Create dataframe for ice duration grid
pts.grid2 <- expand.grid(x=seq.Date(from = as.Date('2001-02-15'),to = as.Date('2001-06-15'),by = '10 days'),
                         y=seq.Date(from = as.Date('2000-11-7'),to = as.Date('2001-06-15'),by = '10 days'))
pts.grid2$z <- pts.grid2$x - pts.grid2$y
# Labels for contour lines 
a = pts.grid2 %>% filter(x == as.Date('2001-02-15') | y == as.Date('2000-11-17')) %>%
  mutate(labels = paste0(z,' days')) %>%
  filter(z >= 10 & z <= 170)
##################################################################
# Clean data 
ices = ice %>% 
  mutate(year = as.numeric(substr(WINTER,start = 1,stop = 4))) %>%
  mutate(DAYS = as.numeric(DAYS)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(CLOSED = first(CLOSED), OPENED = last(OPENED), DAYS = sum(DAYS,na.rm = T)) %>%
  mutate(firstice = as.Date(paste0(CLOSED,'-',year),'%d-%b-%Y'), 
         lastice = as.Date(paste0(OPENED,'-',year),'%d-%b-%Y')) %>%
  mutate(yearF = ifelse(yday(firstice) < 100,year + 1, year)) %>%
  mutate(yearC = ifelse(yday(lastice) < 300,year + 1, year)) %>%
  mutate(firstice = as.Date(paste0(CLOSED,'-',yearF),'%d-%b-%Y'), 
         lastice = as.Date(paste0(OPENED,'-',yearC),'%d-%b-%Y')) %>%
  mutate(duration = lastice-firstice) %>%
  mutate(year1 = ifelse(yearF-year == 0,2000,2001), year2 = ifelse(yearC-year == 0,2000,2001)) %>%
  mutate(first2000 = as.Date(paste0(CLOSED,'-',year1),'%d-%b-%Y'), 
         last2000 = as.Date(paste0(OPENED,'-',year2),'%d-%b-%Y')) %>%
  dplyr::select(year:lastice,duration,first2000,last2000) %>%
  filter(DAYS > 0)

##################################################################
########### How to reverse date axis ######################
#https://stackoverflow.com/questions/43625341/reverse-datetime-posixct-data-axis-in-ggplot/43626186
c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
  library(scales)
  a <- as.trans(a)
  b <- as.trans(b)
  name <- paste(a$name, b$name, sep = "-")
  
  trans <- function(x) a$trans(b$trans(x))
  inv <- function(x) b$inverse(a$inverse(x))
  
  trans_new(name, trans, inverse = inv, breaks = breaks, format=format)
}
rev_date <- c_trans("reverse", "time")

##################################################################
########## Create Animation ###########
##################################################################
p = ggplot(ices, aes(x = (last2000), y = as.POSIXct(first2000))) +
  geom_contour(data= pts.grid2, aes(x = (x), y = as.POSIXct(y), z = as.numeric(z)), 
               breaks=brks, colour="grey50",alpha = 0.4) +
  geom_text(data = a,
            aes(x = x, y = as.POSIXct(y), label = labels), 
            color = 'grey50', angle = 323, nudge_x = 2,alpha = 0.5, size = 6) +
  geom_point(aes(color = year),alpha = 0.8, size = 6, show.legend = T) +
  transition_states(year, transition_length=1, state_length=1,wrap = F) +
  labs(title = 'Lake Mendota Ice Cover, 1855 - {closest_state}') + shadow_mark() +
  scale_colour_viridis_c(option = 'B',name = "Year",
                         limits = c(1855, 2018),
                         breaks = seq(1875,2000,by=25)) +
  scale_size(range = c(0, 6)) +
  scale_y_continuous(trans = rev_date,
                     labels=date_format("%b-%d"),
                     limits = as.POSIXct(c('2001-02-15','2000-11-15'))) +
  scale_x_date(date_breaks = "1 month", 
               labels=date_format("%b-%d"),
               limits = as.Date(c('2001-02-15','2001-05-15'))) +
  ylab('Date of Ice-On') + xlab('Date of Ice-Off') +
  theme_minimal(base_size = 20) + theme(legend.key.height = grid::unit(1.5, "inches"))

image <- animate(p,fps=15,nframes = 163*2,width = 1000,height = 800,renderer = gifski_renderer(loop = F))
anim_save(filename = '~/Downloads/mendotaIce.gif',animation = image)

####################################################################################################
## With linear trend line ##
years = unique(ices$year)
lmTrends = data.frame(year = rep(years,each=2), x = NA, y = NA)
for (i in 1:163) {
  indxYear = years[i]
  df = ices %>% filter(year <= indxYear)
  b = lm(as.POSIXct(first2000) ~ last2000, data = df)
  lmTrends$x[i*2-1] = as.Date('2001-05-15')
  lmTrends$x[i*2] = as.Date('2001-02-25')
  lmTrends$y[i*2-1] = predict(b, newdata = data.frame(last2000 = as.Date('2001-05-15')))
  lmTrends$y[i*2] = predict(b, newdata = data.frame(last2000 = as.Date('2001-02-25')))
}
lmTrends$x =  as.Date(lmTrends$x,origin = as.Date('1970-01-01'))
lmTrends$y =  as.POSIXct(lmTrends$y,origin = as.POSIXct('1970-01-01'))

# Create plot
p = ggplot(ices, aes(x = last2000, y = as.POSIXct(first2000))) +
  geom_contour(data= pts.grid2, aes(x = (x), y = as.POSIXct(y), z = as.numeric(z)),
               breaks=brks, colour="grey50",alpha = 0.5) +
  geom_text(data = a,
            aes(x = x, y = as.POSIXct(y), label = labels),
            color = 'grey50', angle = 323, nudge_x = 2,alpha = 0.5, size = 6) +
  geom_path(data = lmTrends, aes(x = x, y = y),color = 'grey40') +
  geom_point(aes(color = year),alpha = 0.8, size = 6, show.legend = T) +
  transition_time(time = year) +
  # transition_states(year, transition_length=1, state_length=1,wrap = F) +
  shadow_mark(exclude_layer = c(3)) + #Don't retain lines 
  labs(title = 'Lake Mendota Ice Cover, 1855 - {frame_time}',
       caption = "Viz: Hilary Dugan - @hildug\nSource: http://www.aos.wisc.edu/~sco/lakes/Mendota-ice.html") +  
  scale_colour_viridis_c(option = 'B',name = "Year",
                         limits = c(1855, 2018),
                         breaks = seq(1875,2000,by=25)) +
  scale_size(range = c(0, 6)) +
  scale_y_continuous(trans = rev_date,
                     labels = date_format("%b-%d"),
                     limits = as.POSIXct(c('2001-02-15','2000-11-15'))) +
  scale_x_date(date_breaks = "1 month",
               labels=date_format("%b-%d"),
               limits = as.Date(c('2001-02-15','2001-05-16'))) +
  ylab('Date of Ice-On') + xlab('Date of Ice-Off') +
  theme_minimal(base_size = 20) + theme(legend.key.height = grid::unit(1.5, "inches")) 

image <- animate(p,fps=10,nframes = 163,width = 1000,height = 800,renderer = gifski_renderer(loop = F))
anim_save(filename = '~/Downloads/mendotaIce2.gif',animation = image)

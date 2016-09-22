rm(list = ls()))
gc()

# setup

# setup -------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(stringr)

set.seed(98136)

theme_set(theme_bw(base_size = 16))

#Nb. the raw data uses NULL to indicate missing values (these might be interesting themselves at some point)
dat <- read.delim("airbnb_session_data.txt", sep = "|", stringsAsFactors = F, na.strings = "NULL")

# Unfortunately dplyr doesn't play well with date times although ggplot2 handles
# them smoothly. However if the dates are in Date format instead of POSIXct then
# all should be mostly well.

# basic desc --------------------------------------------------------------
table(table(unique(dat$id_visitor)))
dat <- dat %>% arrange(id_visitor, dim_session_number)
dat %>% group_by(id_visitor) %>% tally()
# 630 visitors made 7756 visits to the site
# who's the biggest recidivist?
glimpse(dat[dat$dim_session_number == max(dat$dim_session_number),])
# 702, wow


# sort by user and ts_min
# generate session_time
# generate cum_time
dat <- dat %>% arrange(id_visitor, ts_min)
tmin <- strptime(dat$ts_min, format = "%Y-%m-%d %H:%M:%S")
tmax <- strptime(dat$ts_max, format = "%Y-%m-%d %H:%M:%S")
dat$mins <- as.numeric(difftime(tmax, tmin, units = "mins"))

dat <- dat %>% group_by(id_visitor) %>% mutate(cutime = cumsum(mins))

# plots -------------------------------------------------------------------

# Init some colors
dat$act.col <- ifelse(dat$sent_booking_request == 1, "red",
                   ifelse(dat$sent_message == 1, "green",
                          ifelse(dat$did_search == 1, "blue", NA)))
# actually that's not quite playing out the way I'd prefer later on w/ ggplot aes
dat$act <- ifelse(dat$sent_booking_request == 1, "Booking req",
                  ifelse(dat$sent_message == 1, "Sent msg",
                         ifelse(dat$did_search == 1, "Searched", NA)))

# plot
p <- ggplot(data = dat, aes(x = dim_session_number, y = cutime, group = id_visitor))
p + geom_line()
# add points for actions
p + geom_line() + geom_point(aes(x = dim_session_number, y = cutime), color = act.col)

# switch to hours
dat$cuhours <- dat$cutime/60
p <- ggplot(data = dat, aes(x = dim_session_number, y = cuhours, group = id_visitor))
p + geom_line() + geom_point(aes(x = dim_session_number, y = cuhours), color = act.col)
# log scale
p + geom_line() + geom_point(aes(x = dim_session_number, y = cuhours), color = act.col) +
    scale_y_continuous(limits = c(0, 100)) +
    scale_x_continuous(limits = c(0, 400))

# let's filter it and let ggplot2 auto-scale
sub <- dat %>% filter(cuhours < 50)
ggplot(data = sub, aes(x = dim_session_number, y = cuhours, group = id_visitor)) +
    geom_line() +
    geom_point(aes(x = dim_session_number, y = cuhours), color = sub$act.col, size = .8)


# Try a plot with hourly activity, nice of them to use 24-hour time
dat$hour <- as.numeric(str_sub(dat$ts_min, 12, 13))
hourly <- dat %>% group_by(hour) %>%
    summarize(bookings = sum(sent_booking_request))
qplot(data = hourly, x = hour, y = bookings, geom = "line")


# Daily activity ----------------------------------------------------------

daily = dat %>% group_by(date) %>%
    summarize(bk = sum(sent_booking_request),
              msg = sum(sent_message),
              srch = sum(did_search),
              usrs = length(unique(id_visitor)))

glimpse(daily)
# try:
qplot(data = daily, x = date, y = usrs, geom = "line")

# suppose we want to do this over devices
# I suppose a regex would work fine too, I like stringr b/c it's easy to read
dat$device <- str_sub(dat$dim_device_app_combo, 1,
                      str_locate(dat$dim_device_app_combo, " - ")[,1] - 1)
# ddev for daily devices
ddev = dat %>% group_by(date, device) %>%
    summarize(bk = sum(sent_booking_request),
              msg = sum(sent_message),
              srch = sum(did_search),
              usrs = length(unique(id_visitor)))
# ddev$dy <- strptime(ddev$day, format = "%Y-%m-%d")

ggplot(data = ddev, aes(x = date, y = usrs, group = device)) +
    geom_line() +
    facet_wrap(~device)

# Iphones and desktops are clearly winners but I'd be negligent if I said that
# any sort of visualization was the least bit necessary to know that. In fact it
# looks like they updated their tallying to get rid of the Unknown so those
# probably don't mean much.
# We could make a couple of different variables to roll these up - for example
# was the device a table? Was it an Apple/Android/Desktop device? 
# Let's do tablet/phone/desktop/other b/c I think mode is more interesting/useful
# than users choice of android/apple
table(dat$device)   # little helpful reminder
dat$dev2 <- ifelse(dat$device == "Desktop", "Desktop",
                   ifelse(dat$device == "iPad" | dat$device == "Android Tablet", "Tablet",
                          ifelse(dat$device == "iPhone" | dat$device == "Android Phone", "Phone",
                                 "Other/Unknown")))

# just a little qplot or two
qplot(data = dat, x = date, y = cuhours, color = dev2) +
    labs(color = "Device cat")
# it occurs to me that it would be nice to actually modify the earlier ggplot
# so that the lines don't all originate at 0, that was not aesthetically pleasing
p <- ggplot(data = dat, aes(x = date, y = cuhours, group = id_visitor)) +
    geom_line()
p + geom_point(aes(x = date, y = cuhours), color = dat$act.col)
# Yes, there we go...
p + geom_point(aes(x = date, y = cuhours, color = act)) +
    labs(color = "Action") + 
    facet_wrap(~dev2)


# Returning to the non-device data and searching/msgs/booking
ggplot(data = daily, aes(x = date, y = srch)) +
    geom_line() +
    geom_line(aes(x = date, y = bk), color = "red") +
    geom_line(aes(x = date, y = msg), color = "green")


# action - action ---------------------------------------------------------

table(dat$did_search, dat$next_sent_message)
table(dat$sent_message, dat$next_sent_message)
table(dat$sent_message, dat$next_sent_booking_request)

ftable(table("Sea" = dat$did_search, 
             "Msg" = dat$sent_message, 
             "Bk" = dat$sent_booking_request))

# interestingly (or not perhaps) 'do nothing' is the modal outcome and 
# send message but don't search slightly edges out search and do nothing else

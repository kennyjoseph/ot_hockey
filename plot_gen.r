require(stringr)
require(ggplot2)
require(lubridate)

get_decade <- function(v){
  if (v$year >= 2000){
    return("2000s")
  } else if(v$year >= 1990){
    return("1990s")
  } else if(v$year >= 1980){
    return("1980s")
  } else if(v$year >= 1970){
    return("1970s")
  } else if(v$year >= 1960){
    return("1960s")
  }  else {
    return("pre 1960s")
  }
}

dat <- read.csv("~/Desktop/overtime_games.csv",header=F,stringsAsFactors=F)
names(dat) <- c("date","site","series","score","player","time","winner")
dat$date_real <- mdy(dat$date)
dat$year <- year(dat$date_real)
dat[dat$year > 2014,]$year <- dat[dat$year > 2014,]$year - 100
dat$time_in_sec <- 0
dat$decade <- 0 
for(i in 1:nrow(dat)){
  q <- str_split(dat[i,]$time,":")[[1]]
  dat[i,]$time_in_sec  <- as.integer(q[1])*60 + as.integer(q[2])
  dat[i,]$decade <- get_decade(dat[i,])
}

ggplot(dat, aes(time_in_sec/60/20)) + geom_histogram(binwidth=.25) + xlab("Number of OT Periods") + ylab("Number of Games") + scale_x_continuous(limits=c(0,6),breaks=1:6)

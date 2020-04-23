########################################
####### LOAD IN PACKAGES/FONTS  ########
########################################


require(pacman)

p_load(rpart,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes)

# ggtext is not on CRAN and is still being developed,
# but it provides fantastic 
# you'll have to make a selection when you run these lines
remotes::install_github("wilkelab/ggtext",force=T)
library("ggtext")


#import fonts for ggplot
font_import()
loadfonts(device="win")
# see a list of fonts. 
fonts()  
#I use Book Antiqua, so make sure you have it, or change the family option in the theme

############################################
###### READ IN FILES/ CLEAN THE DATA #######
############################################

# read in the most up-to-date Covid data from NYT
NYT <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")


# add leading 0s to fips codes if necessary, so it will join with the map/data from the census bureau
NYT$fips <- ifelse(str_length(NYT$fips)==4,paste0(0,NYT$fips),NYT$fips)
NYT$fips <- ifelse(NYT$county=="New York City",36061,NYT$fips)
NYT$fips <- ifelse(NYT$county=="Kansas City",20085,NYT$fips)
NYT$date <- ymd(NYT$date)


# read in the file containing the captions for each plot
quotes <- read.table("~/Covid-19-map/titles.txt",sep="-")
quotes$V1 <- paste0(quotes$V1,"2020")
quotes$V1 <- mdy(quotes$V1)
quotes$V2 <- str_replace_all(quotes$V2,"â€™","\'")
quotes$V2 <- str_replace(quotes$V2," ","")
quotes$V2 <- str_replace_all(quotes$V2,"\n","\\n")
names(quotes) <- c("date","quote")


# you'll need an api key from the census to pull their data. 
# if you don't have one, request one here: http://api.census.gov/data/key_signup.html
# as far as I can tell, everyone is approved and it doesn't take too long

census_api_key("YOUR-KEY-HERE",install=T)

# pull population data and US shapefiles from Census
# expect a warning about hawaii and alaska
census_data <- get_acs(geography = "county",
                       variables=c(
                         #medincome="B19013_001", #median income
                         total="B01003_001"), # total population count
                       geometry=T,                      # pull the shapefiles for maps
                       shift_geo=T)

# now clean up the NYT data
# there are no observations for a county before its first reported case
# so spread the data so that each county has same number of observations
# (one obs for each county-day, starting with the date of the first recorded case)

NYT_data <- expand.grid(date=unique(NYT$date),fips=unique(census_data$GEOID)) 
NYT_data <- left_join(NYT_data,NYT)
NYT_data$cases <- ifelse(is.na(NYT_data$cases),0,NYT_data$cases)
NYT_data$deaths <- ifelse(is.na(NYT_data$deaths),0,NYT_data$deaths)

# want to calculate total cases/deaths in last 14 days, so make a temporary data frame
# with two dates, 14 days apart
temp <- NYT_data %>% mutate(date14=date+int_length(14)) %>% dplyr::select(date14,fips,cases,deaths)
names(temp) <- c("date14","fips","cases14","deaths14")

#join the temp data to the NYT_data and calculate the change in deaths/cases between the two dates
NYT_data <- left_join(NYT_data,temp,by=c("date"="date14","fips"="fips"))
NYT_data$cases14 <- ifelse(is.na(NYT_data$cases14),0,NYT_data$cases14)
NYT_data$deaths14 <- ifelse(is.na(NYT_data$deaths14),0,NYT_data$deaths14)
NYT_data <- NYT_data %>% mutate(cases14 = cases-cases14)
NYT_data <- NYT_data %>% mutate(deaths14 = deaths-deaths14)
NYT_data$cases14 <-  ifelse(NYT_data$cases14 < 0, 0, NYT_data$cases14)
NYT_data$deaths14 <-  ifelse(NYT_data$deaths14 < 0, 0, NYT_data$deaths14)

# join the NYT_data and census_data using GEOID and fips variables
dat <- left_join(census_data,NYT_data,by=c("GEOID"="fips"))
# calculate area of each county
dat$area <- st_area(dat) %>% as.numeric()
# divide population (var name "estimate") by area to get population density in each county
dat <- dat %>% mutate(density=estimate*100000/area)

# in a separate file, calculate the geographic center of each county.
# these centroids will be used to position the yellow/red markers
dat2 <- st_centroid(dat)
# expect a warning: st_centroid assumes attributes are constant over geometries of x
# not a problem for our purposes

# Create 2 variables for cases and deaths per capita. 
# I don't use these in my map, but per capita values
# may be more appropriate for other contexts
dat2 <- dat2 %>% mutate(per_cap = cases/estimate)
dat2 <- dat2 %>% mutate(per_cap_death = deaths/estimate)

# change date variables to date objects
dat2$date <- ymd(dat2$date)
dat$date <- ymd(dat$date)

# calculate the number of days the NYT data spans 
# this value should change every day while the NYT repo is active
max_days <- interval(min(dat2$date),max(dat2$date)) %>% int_length()/86400

# write a function to insert quotation marks around the titles
textwrap <- function (s) {
  if (is_empty(s)) {
    s1 <- ""
  } else {
    s1 <- paste0("“",s,"”")
  }
}

###############################################
######## CREATE FRAMES FOR GIF ################
###############################################

# this big ol' loop creates max_days+1 maps
# each map will become a frame in the gif
# it may be best to run it in chunks if your computer is a potato like mine
# (e.g. run it for 0:20, then 21:40, etc. instead of 0:max_days)
# the maps toward the end are more complicated and will take longer

for (j in 0:max_days) {
  
  # get the date j days after the first case in the NYT data
  i <- ymd("2020-01-21") + ddays(j)
  
  # make titles, but don't put quotation marks around non-quotes
  if (!j %in% c(0,7,8,9,10,11,20,25,29,30,31,46,47,52,64,74)) {
    quote <- textwrap(quotes$quote[which(quotes$date == i)])
  } else {
    quote <- quotes$quote[which(quotes$date == i)]
  }
  
  #### optional split for Feb 2 (j = 12)
  #### quote <- "“We pretty much shut it down coming in from China.”<br><i style='color:#ebfff8'>[Nearly 40,000 more people will arrive from China.]</i>"
  #### quote <- paste0("“We pretty much shut it down coming in from China.”","<br>","[Nearly 40,000 more people will arrive from China.]")
  
  #### optional split for April 12 (j = 87)
  #### quote <- "“LIBERATE MINNESOTA!”"
  #### quote <- "“LIBERATE MICHIGAN!”"
  #### quote <- "“LIBERATE VIRGINIA, and save your great 2nd Amendment. It is under siege!”"

  # grab the correct numbers for national cases/deaths in the two weeks leading up to date i
  num_cases <- sum(dat2[which(dat2$date==i & !is.na(dat2$cases14)),]$cases14,na.rm=T) %>% format(big.mark=",")
  num_deaths <- sum(dat2[which(dat2$date==i & !is.na(dat2$deaths14)),]$deaths14,na.rm=T) %>% format(big.mark=",")
  
  # make the map using ggdraw (from cowplot), a wrapper for ggplot
  temp_plot <- ggdraw() +
    
    # this line plots the heat map for population density
    geom_sf(data=dat[which(dat$date==i),],aes(alpha=log(density),
                                              fill=log(density)),color=NA,size=0,show.legend = F) +
    
    # make a pertty color gradient for the heatmap
    scale_fill_gradient(
      low = "#4545FF",
      high = "#000022",
      space = "Lab",
      na.value = "grey50",
      guide = "colourbar",
      aesthetics = "fill"
    ) +
    
    # add yellow dots for cases
    geom_sf(data=dat2[which(dat2$date==i & dat2$cases14>0),],
            aes(size=cases14),color="yellow",alpha=.7,show.legend = F) +
    
    # add red dots for deaths
    geom_sf(data=dat2[which(dat2$date==i & dat2$deaths14>0),],
            aes(size=deaths14),color="red",alpha=.5,show.legend = F) +
    
    # this line forces every frame to use the same scaling for dot sizes
    scale_size_continuous(limits=c(1,max(dat2$cases14,na.rm=T)),range=c(1.5,18)) +
    
    # title is the quote/action trump said/took
    # subtitle is date and running count of deaths and cases
    # caption is the same for every plot
    labs(x="",y="",
         subtitle = paste0(format(i, format="        %B %d, %Y"), "\n",
                           "        Confirmed cases, last 14 days: ",num_cases,"\n",
                           "        Confirmed deaths, last 14 days: ",num_deaths),
         title = quote,
         caption = " Data: US Census Bureau, NYT        
           Each county is a separate observation        
          Yellow circles indicate confirmed cases        
          Red circles indicate deaths        
          Darker counties have higher population density        ") +
    
    # technical stuff that formats all of the text nicely
    # this portion is where ggtext is used
    # which allows ggplot to read (some) html and markdown code
    theme(plot.title.position = "plot",
          plot.title = element_textbox_simple(colour = "darkred",size=13,
                                              vjust=-0.5,
                                              family="Book Antiqua",
                                              face="bold",
                                              lineheight = 1.17,
                                              padding = margin(12, 12, 0, 12),
                                              margin = margin(0, 0, 5.5, 0),
                                              minheight = grid::unit(50, "pt"),
                                              halign=.5,
                                              valign=.5),
          plot.subtitle = element_text(size=10,vjust=-3,hjust=0,family="Book Antiqua",face="bold"),
          plot.caption = element_textbox(colour="grey32",size=9,hjust=1,vjust=70,
                                         padding = margin(-25, 10, 11, 10), margin = margin(-25, 3, 3, 3),
                                         family="Book Antiqua"),
          plot.background = element_rect(fill="#ebfff8",color=NA,rel(10)))
  # note that if you preview the plot in the plot viewer, 
  # it may look a little different than the saved file will look
  
  # the files need to be in chronological order in the folder
  # so add leading 0s to file names
  k <- ifelse(str_length(j)==1,paste0("00",j),j)
  k <- ifelse(str_length(j)==2,paste0("0",j),k)
  
  # each plot is saved in the frames folder, should automatically be saved in the correct order
  # i always check the first few to be sure nothing goes wrong with the plot formatting
  # (titles hanging off the plot, text over top of the map, etc.)
  ggsave(paste0("~/Covid-19-map/frames/map",k,".png"),temp_plot,width=6.79,height=6.04,units="in")
  
}

##########################################
######### BUILD GIF IN STAGES ############
##########################################

# grab vector of file names for frames
frames <- list.files(path = "~/Covid-19-map/frames/", pattern = "*.png", full.names = T)

# create a vector of display times for each frame
# frames with longer titles get longer display time
times <- 1:length(frames) - 1
for (j in 0:(length(frames)-1)) {
  i <- ymd("2020-01-21") + ddays(j)
  pause <- str_length(quotes$quote[which(quotes$date==i)])*100/20 %>% round()
  times[j+1] <- ifelse(is_empty(quotes$quote[which(quotes$date==i)]),25,pause)
  times[j+1] <- ifelse(times[j+1] > 25 & times[j+1] < 200,200,times[j+1])
  times[j+1] <- ifelse(j == 0,250,times[j+1])
  times[j+1] <- ifelse(j == max_days,800,times[j+1])
}

# R can build this gif in one shot, but it crashes my computer
# So I create a set of partial gifs and merge them on exgif.com
# each partial gif has 10 frames

sections <- floor(length(frames)/10) + ifelse(length(frames) %% 10 > 0,1,0)

for (i in 1:sections) {

  gifs <- (i*10-9):(i*10)
  
  if (i == sections) {
    gifs <- gifs[1:(length(frames) %% 10)]
  }
  
  k <- ifelse(str_length(i)==1,paste0("0",i),i)
  
  frames[gifs] %>% 
    purrr::map(image_read) %>% # reads each path file
    image_join() %>% # joins image
    image_animate(delay=times[gifs],optimize=T) %>% 
    image_write(path=paste0("~/Covid-19-map/partial_gifs/partial",k,".gif"))
  
}


# if you want to try building the gif in one shot
# (eschewing the admittedly inelegant ezgif step),
# try this:

frames[1:max_days] %>% 
  purrr::map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(delay=times[1:max_days],optimize=T) %>% 
  image_write(path=paste0("~/Covid-19-map/THE_MAP2.gif"))
#  Create daily SST plots for publication to Twitter and elsewhere
#  Author: Jordan Watson, Alaska Fisheries Science Center, NOAA Fisheries, jordan.watson@noaa.gov
#  Data sources: AKFIN, 

#  Some heatwave code taken from the fantastic vignette for the heatwaveR package (https://robwschlegel.github.io/heatwaveR/)
#  Citation: Schlegel RW, Smit AJ (2018). “heatwaveR: A central algorithm for the detection of heatwaves and cold-spells.” 
#  Journal of Open Source Software, 3(27), 821. doi: 10.21105/joss.00821.

#---------------------------------------------------------------
#  Load packages
#---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(cowplot)
library(magick)
#library(httr)
#library(jsonlite)
library(heatwaveR)
library(gridExtra)

#---------------------------------------------------------------
#  Define Global parameters
#---------------------------------------------------------------

#  Load 508 compliant NOAA colors
OceansBlue1='#0093D0'
OceansBlue2='#0055A4' # rebecca dark blue
Crustacean1='#FF8300'
UrchinPurple1='#7F7FFF'
SeagrassGreen4='#D0D0D0' # This is just grey
#SeagrassGreen1='#93D500'
#WavesTeal1='#1ECAD3'
#CoralRed1='#FF4438'

#  Assign colors to different time series.
current.year.color <- "black"#CoralRed1 #OceansBlue1
last.year.color <- OceansBlue1#WavesTeal1
mean.color <- UrchinPurple1

#  Set default plot theme
theme_set(theme_cowplot())
#--------------------------------------------------------------------------------------------------

#  Define the latest dataset
latestdata <- "Data/crwsst_bering_19850401_through_110520.RDS"


#--------------------------------------------------------------------------------------------------
#  Create top panel
#--------------------------------------------------------------------------------------------------

#  Specify legend position coordinates (top panel)
mylegx <- 0.525
mylegy <- 0.865

#  Specify NOAA logo position coordinates (top panel)
mylogox <- 0.045
mylogoy <- 0.285

#  Query data from public web API 
#  The WebAPI is currently undergoing updates and is temporarily unavailable. Email the author for the data. 
data <- #httr::content(httr::GET('https://apex.psmfc.org/akfin/data_marts/akmp/GET_TIME_SERIES_REGIONAL_AVG_TEMPS'), type = "text/csv") %>% 
  readRDS(latestdata) %>% 
  rename_all(tolower) %>% 
  #right_join(expand.grid(date=seq.Date(as.Date(min(.$date)),as.Date(max(.$date)),"days"), #Run this to get missing data to appear as a gap in the time series
  #                       ecosystem_sub=unique(.$ecosystem_sub))) %>% 
  mutate(read_date=date,
         esr_region=ecosystem_sub,
         month=month(read_date),
         day=day(read_date),
         year=year(read_date),
         newdate=as.Date(ifelse(month==12,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),#  Create a dummy year so that each year can more easily be overlain
                                as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
         year2=ifelse(month==12,year+1,year)) %>% # To have our years go from Dec-Nov, force December to be part of the subsequent year.
  arrange(read_date) 

#  Set year criteria to automatically identify the current and previous years
current.year <- max(data$year)
last.year <- current.year-1
mean.years <- 1986:2015 # We use the oldest 30-year time series as our climatological baseline.
mean.lab <- "Mean 1986-2015"

#  Create plotting function that will allow selection of 2 ESR regions
myplotfun <- function(region1,region2){
  mylines_base <- ggplot() +
    geom_line(data=data %>% filter(year2<last.year & esr_region%in%(c(region1,region2))), # Older years are grey lines.
              aes(newdate,meansst,group=factor(year2),col='mygrey'),size=0.3) +
    geom_line(data=data %>% filter(year2==last.year & esr_region%in%(c(region1,region2))), # The previous year
              aes(newdate,meansst,color='last.year.color'),size=0.75) +
    geom_line(data=data %>% 
                filter(year%in%mean.years & esr_region%in%(c(region1,region2))) %>% # The mean from 1986-2015
                group_by(esr_region,newdate) %>% 
                summarise(meantemp=mean(meansst,na.rm=TRUE)),
              aes(newdate,meantemp,col='mean.color'),size=0.5) +
    geom_line(data=data %>% filter(year2==current.year & esr_region%in%(c(region1,region2))), # This year
              aes(newdate,meansst,color='current.year.color'),size=0.95) +
    facet_wrap(~esr_region,ncol=2) + 
    scale_color_manual(name="",
                       breaks=c('current.year.color','last.year.color','mygrey','mean.color'),
                       values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'mygrey'=SeagrassGreen4,'mean.color'=mean.color),
                       labels=c(current.year,last.year,paste0('1985-',last.year-1),mean.lab)) +
    ylab("Mean Sea Surface Temperature (C)") + 
    xlab("") +
    scale_x_date(date_breaks="1 month",
                 date_labels = "%b",
                 expand = c(0.025,0.025)) + 
    theme(legend.position=c(mylegx,mylegy),
          legend.text = element_text(size=8,family="sans"),
          legend.background = element_blank(),
          legend.title = element_blank(),
          strip.text = element_text(size=10,color="white",family="sans",face="bold"),
          strip.background = element_rect(fill=OceansBlue2),
          axis.title.y = element_text(size=10,family="sans"),
          axis.text.y = element_text(size=10,family="sans"),
          panel.border=element_rect(colour="black",size=0.75),
          axis.text.x=element_blank(),
          legend.key.size = unit(0.35,"cm"),
          plot.margin=unit(c(-0.1,0.05,0,0),"cm")) 
  
  ggdraw(mylines_base)
}

#png(paste0("figure_output/SST_Twitter_",format(Sys.Date(),"%Y_%m_%d"),".png"),width=6,height=3.375,units="in",res=120)
#myplotfun("NBS","EBS")
#dev.off()

#  Plot a two panel version with no NOAA logo
#jpeg(paste0("SST_ESR/2020/EBS/SST_Bering_ESR_",format(Sys.Date(),"%Y_%m_%d"),".jpeg"),width=6,height=4,units="in",quality=100,res=300)
#myplotfun("Northern Bering Sea","Southeastern Bering Sea")
#dev.off()

#  Plot a two panel version with NOAA logo
#jpeg(paste0("Figures/SST_Bering_ESR_",format(Sys.Date(),"%Y_%m_%d"),".jpeg"),width=6,height=4,units="in",quality=100,res=300)
#myplotfun("Northern Bering Sea","Southeastern Bering Sea") +
#  draw_image("Figures/fisheries_header_logo_jul2019.png",scale=0.2,x=mylogox,y=mylogoy,hjust=0.35) +
#  annotate("text",x=0.14,y=0.048,label=paste0("       Data: NOAA Coral Reef Watch SST, courtesy of NOAA CoastWatch West Coast; coastwatch.pfeg.noaa.gov/erddap\n           Contact: Jordan.Watson@noaa.gov, Alaska Fisheries Science Center, NOAA Fisheries (Updated: ",format(Sys.Date(),"%m-%d-%Y"),")\n             Data are modeled satellite products and periodic discrepancies may exist across sensors and products."),
#           hjust=0.1,size=2.57,family="sans",fontface=2,color=OceansBlue2)
#dev.off()

p1 <- myplotfun("Northern Bering Sea","Southeastern Bering Sea") +
  draw_image("Figures/fisheries_header_logo_jul2019.png",scale=0.2,x=mylogox,y=mylogoy,hjust=0.35)

#--------------------------------------------------------------------------------------------------
# End Top Panel
#--------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------
# Create Bottom Panel
#--------------------------------------------------------------------------------------------------

#  Create custom categories for lines
lineColCat <- c(
  "Temperature" = "black",
  "Baseline" = "gray20",
  "Moderate (1x Threshold)" = "gray60",
  "Strong (2x Threshold)" = "gray60",
  "Severe (3x Threshold)" = "gray60",
  "Extreme (4x Threshold)" = "gray60"
)

#  Create flame fill parameters
fillColCat <- c(
  "Moderate" = "#ffc866",
  "Strong" = "#ff6900",
  "Severe" = "#9e0000",
  "Extreme" = "#2d0000"
)

#  Modified flame fill parameters
Moderate = "#ffc866"
Strong = "#ff6900"
Severe = "#9e0000"
Extreme = "#2d0000"

#  Format plot (modified from theme_cowplot)
mytheme <- theme(strip.text = element_text(size=10,color="white",family="sans",face="bold"),
                 strip.background = element_rect(fill=OceansBlue2),
                 axis.title = element_text(size=10,family="sans",color="black"),
                 axis.text = element_text(size=10,family="sans",color="black"),
                 panel.border=element_rect(colour="black",fill=NA,size=0.5),
                 panel.background = element_blank(),
                 plot.margin=unit(c(0.65,0,0.65,0),"cm"),
                 legend.position=c(0.6,0.7),
                 legend.background = element_blank(),
                 legend.key.size = unit(1,"line"))

# Use heatwaveR package to detect marine heatwaves.
# I run the functions separately for each spatial stratum and then merge. Yeah, ghetto.
mhw <- (detect_event(ts2clm(readRDS(latestdata) %>%
                              filter(Ecosystem_sub=="Southeastern Bering Sea") %>% 
                              rename(t=date,temp=meansst) %>% 
                              arrange(t), climatologyPeriod = c("1985-12-01", "2015-11-30"))))$clim %>% 
  mutate(region="2020 Southeastern Bering Sea Heatwaves") %>% 
  bind_rows((detect_event(ts2clm(readRDS(latestdata) %>%
                                   filter(Ecosystem_sub=="Northern Bering Sea") %>% 
                                   rename(t=date,temp=meansst) %>% 
                                   arrange(t), climatologyPeriod = c("1985-12-01", "2015-11-30"))))$clim %>% 
              mutate(region="2020 Northern Bering Sea Heatwaves"))

# Calculate threshold values for heatwave categories. This code directly from Schegel & Smit
clim_cat <- mhw %>%
  group_by(region) %>% 
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff,
                year=year(t))

#  Create annotation text for plot
mhw_lab <- data.frame(region=c("2020 Northern Bering Sea Heatwaves","2020 Southeastern Bering Sea Heatwaves"),
                      t=c(as_date("2019-12-05"),as_date("2019-12-05")),
                      temp=c((1*max(clim_cat$temp,na.rm=TRUE)),(0.95*max(clim_cat$temp,na.rm=TRUE))),
                      mylab=c("Heatwave intensity increases\n(darker colors) as\nwaters become warmer.",
                              "Heatwaves occur when daily\nSST exceeds the 90th\npercentile of normal for\n5 consecutive days."))

#png("SST_ESR/2020/EBS/Watson_Fig5.png",width=7,height=5,units="in",res=300)
#  Plotting code only slightly modified from heatwaveR vignette
p2 <- ggplot(data = clim_cat %>% filter(t>=as.Date("2019-12-01")), aes(x = t, y = temp,)) +
  geom_flame(aes(y2 = thresh, fill = Moderate)) +
  geom_flame(aes(y2 = thresh_2x, fill = Strong)) +
  geom_flame(aes(y2 = thresh_3x, fill = Severe)) +
  geom_flame(aes(y2 = thresh_4x, fill = Extreme)) +
  geom_line(aes(y = thresh_2x, col = "Strong (2x Threshold)"), size = 0.5, linetype = "dotted") +
  geom_line(aes(y = thresh_3x, col = "Severe (3x Threshold)"), size = 0.5, linetype = "dotted") +
  geom_line(aes(y = thresh_4x, col = "Extreme (4x Threshold)"), size = 0.5, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Baseline"), size = 0.5,linetype="dashed") +
  geom_line(aes(y = thresh, col = "Moderate (1x Threshold)"), size = 0.5,linetype= "dotted") +
  geom_line(aes(y = temp, col = "Temperature"), size = 0.5) +
  geom_text(data=mhw_lab,aes(x=t,y=temp,label=mylab),hjust=0,size=3,family="sans") +
  scale_colour_manual(name = NULL, values = lineColCat,
                      breaks = c("Temperature", "Baseline", "Moderate (1x Threshold)"),guide=FALSE) +
  scale_fill_manual(name = "Heatwave Intensity", values = c(Extreme,Severe,Strong,Moderate),labels=c("Extreme","Severe","Strong","Moderate")#, guide = FALSE
  ) +
  scale_x_date(limits=c(as_date("2019-12-01"),as_date("2020-11-30")),date_breaks="1 month",date_labels = "%b",expand=c(0.01,0)) +
  labs(y = "Sea Surface Temperature (°C)", x = NULL) + 
  facet_wrap(~region,ncol=2) +
  mytheme + 
  theme(strip.text=element_text(size=9),
        legend.position=c(0.055,0.45),
        legend.title = element_text(size=9),
        legend.key.size = unit(0.75,"line"),
        legend.text = element_text(size=8),
        axis.title.x=element_blank(),
        axis.text.x=element_text(color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
        #legend.margin=margin(l=-2.75,t = -8.5, unit='cm')
        plot.margin=unit(c(-0.7,0.05,1,0),"cm"))


#dev.off()
#  Draw figure with text annotations
p2 <- ggdraw(p2) + 
  annotate("text",x=0.170,y=0.065,label=paste0("NOAA Coral Reef Watch data, courtesy NOAA CoastWatch West Coast; coastwatch.pfeg.noaa.gov (Updated: ",
                                               format(max(data$date),"%m-%d-%Y"),
                                               ")\n        Data are modeled satellite products and periodic discrepancies or gaps may exist across sensors and products.\n                                    Contact: Jordan.Watson@noaa.gov, Alaska Fisheries Science Center "),
           hjust=0.1,size=2.57,family="sans",fontface=1,color=OceansBlue2,lineheight=0.96)


png("SST_4_panel_no_legend.png",width=6,height=6,units="in",res=300)
grid.arrange(p1,p2,ncol=1)
dev.off()

jpeg(paste0("SST_4_panel_bering",format(max(data$date),"%Y_%m_%d"),".jpeg"),width=6,height=6,units="in",quality=100,res=180)
grid.arrange(p1,p2,ncol=1)
dev.off()


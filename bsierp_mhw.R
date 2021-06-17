library(tidyverse)
library(magrittr) #  For the %<>% operator
library(lubridate)
library(readxl)
library(cowplot)
library(heatwaveR)

#------------------------------------------------------------------------------------------------------
# #  Connect to the AKFIN database
# library(DBI) #  For database query
# library(odbc) #  For database query
# con <- dbConnect(odbc::odbc(), "akfin",
#                  UID=rstudioapi::askForPassword("Enter AKFIN Username"),
#                  PWD= rstudioapi::askForPassword("Enter AFSC Password"))
# 
# #  Query how many records of each event_type (e.g., at sea, in port, at dock) there are per year. Takes a few minutes.
# dbFetch(dbSendQuery(con,"WITH src AS
#   (
#     select id,
#             latitude,
#             longitude,
#             bsierp_name
#     FROM afsc.erddap_crw_sst_spatial_lookup
#     where (bsierp_name is not null and bsierp_name<>'NA')
# ), temperatures as
# (
#   select temp,
#           bsierp_name,
#           crw_id,
#           read_date 
#   from afsc.erddap_crw_sst a
#   inner join src b 
#   on a.crw_id=b.id
# )
#   select round(avg(temp),2) as avgtemp,
#         bsierp_name,
#         read_date
#   from temperatures
#   group by bsierp_name,read_date")) %>%
#  saveRDS("Data/bsierp_sst_daily.RDS")
# #------------------------------------------------------------------------------------------------------

data <- readRDS("Data/bsierp_sst_daily.RDS") %>% 
  rename_all(tolower) %>% 
  rename(meansst=`round(avg(temp),2)`) %>% 
  mutate(date=as_date(as_datetime(read_date)))

data %>% 
  ggplot(aes(date,meansst,group=bsierp_name)) +
  geom_line()







#--------------------------------------------------------------------------------------------------
# Create Bottom Panel
#--------------------------------------------------------------------------------------------------

#---------------------------------------------------------------
#  Define Global parameters
#---------------------------------------------------------------

#  Load 508 compliant NOAA colors
OceansBlue1='#0093D0'
OceansBlue2='#0055A4' # rebecca dark blue
Crustacean1='#FF8300'
UrchinPurple1='#7F7FFF'
SeagrassGreen4='#D0D0D0' # This is just grey


#  Set default plot theme
theme_set(theme_cowplot())

#  Create custom categories for lines
lineColCat <- c(
  "Temperature" = "black",
  "Baseline" = OceansBlue1,
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
mhw <- lapply(unique(data$bsierp_name),function(x)(detect_event(ts2clm(data %>% 
                              filter(bsierp_name==x) %>% 
                              rename(t=date,temp=meansst) %>% 
                              arrange(t), climatologyPeriod = c("1985-12-01", "2015-11-30"))))$clim %>%
                mutate(bsierp=x)) %>% 
  bind_rows()

# Calculate threshold values for heatwave categories. This code directly from Schegel & Smit
clim_cat <- mhw %>%
  group_by(bsierp) %>% 
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff,
                year=year(t)) %>% 
  arrange(t)

# Specify date range for flame plots
startdate <- as.Date("2017-12-01")
enddate <- as.Date("2019-11-30")

ggplot(data = clim_cat %>% filter(t>=startdate & t<=enddate), aes(x = t, y = temp)) +
  geom_line(aes(y = temp, col = "Temperature"), size = 0.85) +
  geom_flame(aes(y2 = thresh, fill = Moderate)) +
  geom_flame(aes(y2 = thresh_2x, fill = Strong)) +
  geom_flame(aes(y2 = thresh_3x, fill = Severe)) +
  geom_flame(aes(y2 = thresh_4x, fill = Extreme)) +
  geom_line(aes(y = seas, col = "Baseline"), size = 0.65,linetype="solid") +
  geom_line(aes(y = thresh, col = "Moderate (1x Threshold)"), size = 0.5,linetype= "dotted") +
  scale_colour_manual(name = NULL, values = lineColCat,
                      breaks = c("Temperature", "Baseline", "Moderate (1x Threshold)"),guide=FALSE) +
  scale_fill_manual(name = "Heatwave Intensity", values = c(Extreme,Severe,Strong,Moderate),labels=c("Extreme","Severe","Strong","Moderate")#, guide = FALSE
  ) +
  scale_x_date(limits=c(startdate,enddate),date_breaks="1 month",date_labels = "%b",expand=c(0.01,0)) +
  labs(y = "Sea Surface Temperature (°C)", x = NULL) + 
  facet_wrap(~bsierp,ncol=2) +

  mytheme + 
  theme(strip.text=element_text(size=9),
        legend.position="none",
        legend.title = element_text(size=9),
        legend.key.size = unit(0.75,"line"),
        legend.text = element_text(size=8),
        axis.title.x=element_blank(),
        axis.text.x=element_text(color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
        )

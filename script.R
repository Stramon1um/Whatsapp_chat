library(ggplot2)
library(scales)
library(gridExtra)
library(grid)
library(plyr)
library(zoo)
library(lubridate)

library(ggthemes) #per l'estensione HEATMAP di GGPLOT 
library(data.table)
library(ggTimeSeries)

setwd("~/Dropbox/R_script/Sara/whatsapp_chat")

#setwd('C:/Users/MM43328/Dropbox/Sara/whatsapp_chat')
#setwd('C:/Users/mmaver/Dropbox/Sara/whatsapp_chat')


dat <- read.csv('chat_pulita_4.csv', header = TRUE, sep=";")
#table(dat$sender)

count<-count(dat, vars = "sender")

dat$iso <- paste(dat$date, dat$time, dat$morning, sep=" ") 
dat$iso <- strptime(dat$iso, "%d/%m/%Y %I:%M:%S %p")

dat$month <- months(dat$iso, abbreviate = TRUE) 
dat$day <- weekdays(dat$iso, abbreviate = TRUE) 
dat$hour <- format(dat$iso, "%H")

dat$date <- as.Date(as.character(dat$date), format = "%d/%m/%Y") #oppure %y

datebreaks <- seq(as.Date("2018-07-16"), as.Date("2019-11-22"), by="1 week")

########

p1<-ggplot(dat, aes(x = date, fill = sender)) + geom_bar(stat="count") +
  scale_x_date(breaks=datebreaks, labels=date_format("%d %b %y")) +
  scale_fill_brewer(palette="Set2") +
  #ggtitle ("Whatsapp messages") + 
  ylab("Messages") +
  theme(#plot.title = element_text(color="Black", size=20, hjust = 0.5),
    #axis.title.x = element_text(color="Black", size=14),
    axis.title.y = element_text(color="Black", size=14),
    axis.title.x = element_blank())+
  theme(axis.text.x = element_text(face = "bold", angle = 30, hjust = 1, size=9),
        axis.text.y = element_text(face = "bold", hjust = 1, size=12))+
  theme(legend.position="none")

p3<-ggplot(dat, aes(x = day, fill = sender)) + geom_bar(stat="count", position = 'dodge') +
  scale_fill_brewer(palette="Set2") +
  theme(plot.title = element_text(color="Black", size=20, hjust = 0.5),
        #axis.title.x = element_text(color="Black", size=14),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(face = "bold", angle = 30, hjust = 1, size=12),
        axis.text.y = element_text(face = "bold", hjust = 1, size=12)) +
  theme(legend.position="none")

p2<-ggplot(dat, aes(x = month, fill = sender)) + geom_bar(stat="count", position = 'dodge') +
  scale_fill_brewer(palette="Set2") + ylab("Messages") +
  theme(plot.title = element_text(color="Black", size=20, hjust = 0.5),
        #axis.title.x = element_text(color="Black", size=14),
        axis.title.y = element_text(color="Black", size=14),
        axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(face = "bold", angle = 30, hjust = 1, size=12),
        axis.text.y = element_text(face = "bold", hjust = 1, size=12)) +
  theme(legend.position="none")

p5<-ggplot(dat, aes(x = hour, fill = sender)) + geom_bar(stat="count", position = 'dodge') +
  scale_fill_brewer(palette="Set2") + ylab("Messages") +
  theme(plot.title = element_text(color="Black", size=20, hjust = 0.5),
        #axis.title.x = element_text(color="Black", size=14),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(face = "bold", angle = 30, hjust = 1, size=12),
        axis.text.y = element_text(face = "bold", hjust = 1, size=12)) +
  theme(legend.position="none")

p4<-ggplot(count, aes(x=sender, y=freq, fill=sender)) +
  #geom_text(aes(label=), position=position_dodge(width=0.9), vjust=-0.25) +
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_brewer(palette="Set2") +
  theme(plot.title = element_text(color="Black", size=20, hjust = 0.5),
        #axis.title.x = element_text(color="Black", size=14),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(face = "bold", angle = 30, hjust = 1, size=12),
        axis.text.y = element_text(face = "bold", hjust = 1, size=12)) +
  theme(legend.position="none")

pie<-ggplot(count, aes(x = "", y=freq, fill = sender)) + 
  geom_bar(width = 1, stat = "identity") +
  scale_fill_brewer(palette="Set2") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5))+
  coord_polar(theta = "y", start=0)+
  labs(fill="class", 
       x=NULL, 
       y=NULL)

######HEATMAPS

dat_2<-count(dat, c("date"))
dat_2$iso <- paste(dat_2$date) 
#dat_2$iso <- strptime(dat_2$iso, "%d/%m/%y")
dat_2$iso<-as.POSIXct(dat_2$iso)

dat_2$month <- months(dat_2$iso, abbreviate = TRUE) 
dat_2$day <- weekdays(dat_2$iso, abbreviate = TRUE)
dat_2$daymonth <- format(dat_2$iso, "%d") 
dat_2$week <- format(dat_2$iso, "%W")
dat_2$year <- format(dat_2$iso, "%Y")

dat_2$yearmonth <- as.yearmon(dat_2$iso)

dat_2<- ddply(dat_2,.(yearmonth), transform, monthweek=ceiling(day(iso) / 7))
#df <- ddply(df,.(yearmonthf), transform, monthweek=1+week-min(week))  # pare che sballi il conteggio delle settimane
dat_2 <- dat_2[, c("year", "month", "week", "monthweek", "day", "freq","daymonth")]
dat_2$month = factor(dat_2$month, levels=c('gen','feb','mar','apr','mag','giu','lug','ago','set','ott','nov','dic'))

p6<-ggplot(dat_2, aes(daymonth, month, fill = freq)) + 
    geom_tile(colour = "white") + 
    #facet_grid(year~month) + 
    scale_fill_gradient(low="green", high="red") +
    labs(x="",
       y="",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Whatsapp messages between Sara & Mauro", 
       fill="Messages")

p7<-ggplot(dat_2, aes(monthweek, day, fill = freq)) + 
    geom_tile(colour = "white") + 
    facet_grid(year~month) + 
    scale_fill_gradient(low="green", high="red") +
    labs(x="",
       y="",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Whatsapp messages between Sara & Mauro", 
       fill="Messages")

dat_3<-count(dat, c("date"))

ggplot_calendar_heatmap(dat_3,'date','freq')+
  xlab('') + 
  ylab('') + 
  scale_fill_continuous(low = 'green', high = 'red') + 
  facet_wrap(~Year, ncol = 1)+
  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_blank())


#
##
### conteggio messaggi per giorno
####
######
dat_3<-count(dat, c("date")) #per 2° heatmap e conteggio 

dat_3$date <- as.Date(strptime(dat_3$date, "%d/%m/%Y"))  #oppure %y #imposta in un solo step il formato data alla colonna

p8<-ggplot_calendar_heatmap(dat_3,'date','freq')

# adding some formatting
p8 + 
  xlab('') + 
  ylab('') + 
  scale_fill_continuous(low = 'green', high = 'red') + 
  facet_wrap(~Year, ncol = 1)

#########

#grid.arrange(p1, p2, p3, p4,
#  widths = c(2, 1),
#  layout_matrix = rbind(c(1, 1, 4),
                      #  c(2, 3)))

lay <- rbind(c(1,1,4),
             c(2,3,5))

#png("Rplot10.png", units="px", width=4096, height=2160, res=210)
grid.arrange(p1, p2, p3, p4, p5, layout_matrix = lay, 
             top = textGrob("Whatsapp messages between Sara & Mauro",gp=gpar(fontsize=22,font=2)))
dev.off()

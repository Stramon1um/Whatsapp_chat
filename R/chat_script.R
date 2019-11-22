library(ggplot2)
library(scales)
library(gridExtra)
library(grid)
library(plyr)
library(zoo)
library(lubridate)

dat <- read.csv('chat.csv', header = TRUE, sep=";") #read the csv file

count<-count(dat, vars = "sender")

dat$iso <- paste(dat$date, dat$time, dat$morning, sep=" ") 
dat$iso <- strptime(dat$iso, "%d/%m/%Y %I:%M:%S %p")
dat$month <- months(dat$iso, abbreviate = TRUE) 
dat$day <- weekdays(dat$iso, abbreviate = TRUE) 
dat$hour <- format(dat$iso, "%H")
dat$date <- as.Date(as.character(dat$date), format = "%d/%m/%Y") #if the first plot (p1) doesn't work, try with %y

datebreaks <- seq(as.Date("2018-07-16"), as.Date("2019-11-22"), by="2 week") #select the range

p1<-ggplot(dat, aes(x = date, fill = sender))+ 
    geom_bar(stat="count")+
    scale_x_date(breaks=datebreaks, labels=date_format("%d %b %y"))+
    scale_fill_brewer(palette="Set2")+
    ylab("Messages")+
    theme_bw()+
    theme(axis.title.y = element_text(color="Black", size=14),
          axis.title.x = element_blank(),
          axis.text.x = element_text(face = "bold", angle = 30, hjust = 1, size=9),
          axis.text.y = element_text(face = "bold", hjust = 1, size=12),
          legend.position="none")

p2<-ggplot(dat, aes(x = day, fill = sender))+
    geom_bar(stat="count", position = 'dodge')+
    scale_fill_brewer(palette="Set2")+
    theme_bw()+
    theme(plot.title = element_text(color="Black", size=20, hjust = 0.5),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(face = "bold", angle = 30, hjust = 1, size=12),
          axis.text.y = element_text(face = "bold", hjust = 1, size=12),
          legend.position="none")

p3<-ggplot(dat, aes(x = month, fill = sender))+
    geom_bar(stat="count", position = 'dodge')+
    scale_fill_brewer(palette="Set2")+ 
    ylab("Messages") +
    theme_bw()+
    theme(plot.title = element_text(color="Black", size=20, hjust = 0.5),
          axis.title.y = element_text(color="Black", size=14),
          axis.title.x = element_blank(),
          axis.text.x = element_text(face = "bold", angle = 30, hjust = 1, size=12),
          axis.text.y = element_text(face = "bold", hjust = 1, size=12),
          legend.position="none")

p4<-ggplot(dat, aes(x = hour, fill = sender))+ 
    geom_bar(stat="count", position = 'dodge')+
    scale_fill_brewer(palette="Set2")+ 
    ylab("Messages")+
    theme_bw()+
    theme(plot.title = element_text(color="Black", size=20, hjust = 0.5),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(face = "bold", angle = 30, hjust = 1, size=12),
          axis.text.y = element_text(face = "bold", hjust = 1, size=12),
          legend.position="none")

p5<-ggplot(count, aes(x=sender, y=freq, fill=sender))+ 
    geom_bar(position = 'dodge', stat='identity')+
    geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25)+
    scale_fill_brewer(palette="Set2")+
    theme_bw()+
    theme(plot.title = element_text(color="Black", size=20, hjust = 0.5),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(face = "bold", angle = 30, hjust = 1, size=12),
          axis.text.y = element_text(face = "bold", hjust = 1, size=12),
          legend.position="none")

lay <- rbind(c(1,1,5),
             c(3,2,4))

png("Chat_plot.png", units="px", width=4096, height=2160, res=210) #To save the plot in PNG
grid.arrange(p1, p2, p3, p4, p5, layout_matrix = lay, 
             top = textGrob("Whatsapp messages",gp=gpar(fontsize=22,font=2)))
dev.off()


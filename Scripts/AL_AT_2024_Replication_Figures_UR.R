# -------------------------------------------------------------------------------
# Title: How Insured Are Workers Against Unemployment? Unemployment 
# Insurance and the Distribution of Liquid Wealth 
# Purpose: Reproduce the charts from the FRBC's Commentary
# File: Script to generate Figure 1
# Authors: Andre  Victor D. Luduvice and Anaya Truss-Williams
# Cleveland, September 2024
# -------------------------------------------------------------------------------

#### Housekeeping ---------------------------------------------------------------

# Set colors
color1 = "#2875a8"   #blue
color2 = "#7fb6ca"   #blue 2

#### Loading data ---------------------------------------------------------------

# consult README.docx in Replication Package folder for freely available sources of the below data. 
unrate <- read_excel(paste0(getwd(), "/Data/Haver_LF_Data.xlsx"), sheet = "unrate")
lfp <- read_excel(paste0(getwd(), "/Data/Haver_LF_Data.xlsx"), sheet = "lfp")
cont_claims <- read_excel(paste0(getwd(), "/Data/Haver_LF_Data.xlsx"), sheet = "cont_claims")

#### Data transformations -------------------------------------------------------

# calculating continued claims as % of labor force 
cont_claims <- merge(cont_claims,lfp,all=T)
cont_claims$liumlfp <- (cont_claims$lium / cont_claims$lf)*100 # cont claims / lf

# preparing data for long formatting
unrate$variable <- "lr"
cont_claims$variable <- "liumlfp"
unrate$value <- unrate$lr
cont_claims$value <- cont_claims$liumlfp

# formatting dates
unrate$date <- as.Date(as.yearmon(unrate$date, format="%Y-%b")) + days(1)
cont_claims$date <- as.Date(as.yearmon(cont_claims$date, format="%Y-%b")) + days(1)

# filtering data to the past 24 years
unrate <- unrate %>% select(date,variable,value) %>% filter(date <= "2024-07-02") %>% filter(date >= today() - years(24))
cont_claims <- cont_claims %>% select(date,variable,value) %>% filter(date <= "2024-07-02") %>% filter(date >= today() - years(24))

# # calculating reference value of insured workers from 2010 to 2020 in the text
# unrate_avg <- unrate %>% filter(date >= "2010-01-01") %>% filter(date <= "2019-12-31") %>% summarise(avg = mean(value))
# cont_claims_avg <- cont_claims %>% filter(date >= "2010-01-01") %>% filter(date <= "2019-12-31") %>% summarise(avg = mean(value))
# cont_claims_avg/unrate_avg # shows that it's less than thirty percent

# formatting to long 
graph1 <- rbind(unrate,cont_claims)

#### Recession shading ----------------------------------------------------------

add_rec_shade<-function(st_date,ed_date,shade_color="gray")
{
  
  fredr_set_key("...") # REQUIRED: API for FRED website
  
  st_date<-as.Date("2000-01-01")
  ed_date<-as.Date(Sys.Date())
  
  recession<-fredr(series_id = "USRECD",observation_start = as.Date(st_date),observation_end = as.Date(ed_date))
  
  recession$diff<-recession$value-lagpad(recession$value,k=1)
  recession<-recession[!is.na(recession$diff),]
  recession.start<-recession[recession$diff==1,]$date
  recession.end<-recession[recession$diff==(-1),]$date
  
  if(length(recession.start)>length(recession.end))
  {recession.end<-c(recession.end,Sys.Date())}
  if(length(recession.end)>length(recession.start))
  {recession.start<-c(min(recession$date),recession.start)}
  
  recs<-as.data.frame(cbind(recession.start,recession.end))
  recs$recession.start<-as.Date(as.numeric(recs$recession.start),origin=as.Date("1970-01-01"))
  recs$recession.end<-as.Date(recs$recession.end,origin=as.Date("1970-01-01"))
  if(nrow(recs)>0)
  {
    rec_shade<-geom_rect(data=recs, inherit.aes=F, 
                         aes(xmin=recession.start, xmax=recession.end, ymin=-Inf, ymax=+Inf), 
                         fill=shade_color, alpha=0.4)
    return(rec_shade)
  }
}

#### Figure 1 - unemp.rate and continued claims for workforce -------------------

group.colors=c(color2,color1)
group.color= c("Unemployment Rate"=color1,"Continued Claims"= color2)

fig_one <- ggplot() + geom_hline(yintercept=seq(0,15, by=5), linewidth=0.2, linetype=2) +
  add_rec_shade(min(graph1$date),max(graph1$date))+
  geom_line(data = graph1, aes(date, value, colour = variable, linetype = variable),linewidth=2) +
  scale_color_manual(values=group.colors,guide = FALSE, labels=paste(names(group.color))) +
  scale_linetype_manual("", values=c(1,2),guide = FALSE) + theme_classic() +
  guides(colour = guide_legend(override.aes = list(size=2, color=c(color1,color2), 
                                                   linetype=c(3,1),linewidth=c(1.5,1.5))))+
  theme_classic() + labs(y = "Percent of Labor Force",
                         x = "") +
  scale_x_date(date_breaks = "3 years", date_labels="%Y", expand=c(0.02,0)) + 
  theme(axis.title = element_text(size=20, color="black"),
        axis.text.y=element_text(size=20, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", linewidth = 1),
        plot.title=element_text(size=18, color="black"),
        legend.key.size = unit(.69, 'cm'),
        legend.text=element_text(size=15),
        legend.text.align = 1,
        legend.title=element_blank(),
        legend.key = element_rect(colour = "transparent", fill="transparent"),
        legend.background = element_rect(linetype = 1, size = 0.5, colour = 1),
        legend.justification = c(0,1),
        legend.position=c(0,1)) 

ggsave(paste0(Chart_Path, "Figure_1.png"), width = 12, height=6, unit="in")

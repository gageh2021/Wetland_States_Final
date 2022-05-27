pacman::p_load(stats,dplyr,tidyr,broom,ggplot2,ggpubr,FSA,RColorBrewer,here,ggridges,plotly,lmtest)

#Please check that the working directory displays correctly using 'here()'.
#It should show that the current directory ends with .../Wetland_Data_Repository

wetland_data<-read.table(here("Data","Wetland_Data.csv"),header=T,sep=",")
attach(wetland_data)
proportion_data<-read.table(here("Data","Wetland_Proportion.csv"),header=T,sep=",")
attach(proportion_data)

#Digitization Results (Wetland States) -------------------

A<-filter(wetland_data,State=="A")
B<-filter(wetland_data,State=="B")
C<-filter(wetland_data,State=="C")
D<-filter(wetland_data,State=="D")
E<-filter(wetland_data,State=="E")

#Summary Statistics

mean(A$Area_km)
mean(B$Area_km)
mean(C$Area_km)
mean(D$Area_km)
mean(E$Area_km)

sd(A$Area_km)
sd(B$Area_km)
sd(C$Area_km)
sd(D$Area_km)
sd(E$Area_km)

sum(Area_km)
sum(A$Area_km)
sum(B$Area_km)
sum(C$Area_km)
sum(D$Area_km)
sum(E$Area_km)

count(A)
count(B)
count(C)
count(D)
count(E)

#Relative Frequency

barplot_1<-ggplot(data=proportion_data, aes(x=State, y=Relative_Proportion))+
  geom_bar(stat="identity", fill="#98022e")+labs(y="Relative Frequency (%)",x="Successional State")+
  theme_classic()+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))
barplot_1
ggsave(filename = here("Figures","Barplot1_Frequency_State.png"))

#Wetland Area

av1<-lm(Area_km~State)
anova(av1)
par(mfrow=c(1,1))
plot(av1)
shapiro.test(Area_km)
bartlett.test(wetland_data$Area_km~wetland_data$State)
kruskal.test(Area_km~State,data=wetland_data)
dunnTest(Area_km ~ State,data=wetland_data, method="bonferroni")

boxplot_1<-ggplot(wetland_data,aes(x=State,y=Area_km))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y=expression(bold(paste('Wetland Area (km'^2*')'))), x="Successional State")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_1
ggsave(filename = here("Figures","Boxplot1_Area_State.png"))

av2<-lm(Area_Proportion~State)
anova(av2)
par(mfrow=c(2,2))
plot(av2)
shapiro.test(Area_Proportion)
bartlett.test(wetland_data$Area_Proportion~wetland_data$State)
kruskal.test(Area_Proportion~State,data=wetland_data)
dunnTest(Area_Proportion ~ State,data=wetland_data, method="bonferroni")

boxplot_2<-ggplot(wetland_data,aes(x=State,y=Area_Proportion))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y="Proportion of Total Area (%)", x="Successional State")+
  theme(plot.title=element_text(size=16, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=15,face='bold'),  
        axis.title.y=element_text(size=15,face='bold'),  
        axis.text.x=element_text(size=12, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=12,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_2
ggsave(filename = here("Figures","Boxplot2_Proportion_State.png"))

barplot_2<-ggplot(data=proportion_data, aes(x=State, y=Area_Proportion_km))+
  geom_bar(stat="identity", fill="#98022e")+labs(y="Proportion of Total Area (%)",x="Successional State")+
  theme_classic()+
  theme(plot.title=element_text(size=16, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))
barplot_2
ggsave(filename = here("Figures","Barplot2_Proportion_State.png"))

#Open Water Cover

water_filtered<-filter(RdNBR_data,State!="E") #Filter out state E as it should have no water
water_filtered #Please substitute for wetland_data if you would like to exclude state E classification error

mean(A$Open_water)
mean(B$Open_water)
mean(C$Open_water)
mean(D$Open_water)
mean(E$Open_water)

sd(A$Open_water)
sd(B$Open_water)
sd(C$Open_water)
sd(D$Open_water)
sd(E$Open_water)

av6_aov<-aov(Open_water~State,data=wetland_data)
anova(av6_aov)
par(mfrow=c(2,2))
plot(av6_aov)
shapiro.test(wetland_data$Open_water)
bartlett.test(wetland_data$Open_water~wetland_data$State)
TukeyHSD(av6_aov, conf.level=.95)

boxplot_7<-ggplot(wetland_data,aes(x=State,y=Open_water))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y='Proportion of Open Water (%)', x="Successional State")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_7
ggsave(filename = here("Figures","Boxplot7_Open_Water_State.png"))

#Burn Severity -------------------

mean(A$RdNBR_mean)
sd(A$RdNBR_SD)
mean(B$RdNBR_mean)
sd(B$RdNBR_SD)
mean(C$RdNBR_mean)
sd(C$RdNBR_SD)
mean(D$RdNBR_mean)
sd(D$RdNBR_SD)
mean(E$RdNBR_mean)
sd(E$RdNBR_SD)

av3<-lm(wetland_data$RdNBR_mean~wetland_data$State)
anova(av3)
plot(av3)
par(mfrow=c(1,1))
shapiro.test(wetland_data$RdNBR_mean)
bartlett.test(wetland_data$RdNBR_mean~wetland_data$State)
kruskal.test(RdNBR_mean~State,data=wetland_data)
dunnTest(RdNBR_mean ~ State,data=wetland_data, method="bonferroni")

boxplot_3<-ggplot(wetland_data,aes(x=State,y=RdNBR_mean))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y='Mean RdNBR', x="Successional State")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_3
ggsave(filename = here("Figures","Boxplot3_Mean_RdNBR_State.png"))

av4<-lm(RdNBR_SD~State,data=wetland_data)
anova(av4)
par(mfrow=c(2,2))
plot(av4)
shapiro.test(wetland_data$RdNBR_SD)
bartlett.test(wetland_data$RdNBR_SD~wetland_data$State)
kruskal.test(RdNBR_SD~State,data=wetland_data)
dunnTest(RdNBR_SD ~ State,data=wetland_data, method="bonferroni")

mean(A$RdNBR_SD)
sd(A$RdNBR_SD)
mean(B$RdNBR_SD)
sd(B$RdNBR_SD)
mean(C$RdNBR_SD)
sd(C$RdNBR_SD)
mean(D$RdNBR_SD)
sd(D$RdNBR_SD)
mean(E$RdNBR_SD)
sd(E$RdNBR_SD)

boxplot_4<-ggplot(wetland_data,aes(x=State,y=RdNBR_SD))+theme_classic()+geom_boxplot(notch=FALSE,outlier.shape = NA)+
  labs(y='Standard Deviation of RdNBR', x="Successional State")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))+
  geom_jitter(alpha=0.7,colour='#98022e')
boxplot_4
ggsave(filename = here("Figures","Boxplot4_SD_RdNBR_State.png"))

cor1<-lm(RdNBR_mean~Area_km,data=wetland_data)
summary(cor1)
plot(cor1)
shapiro.test(wetland_data$Area_km)
shapiro.test(wetland_data$RdNBR_mean)
bartlett.test(wetland_data$RdNBR_SD~B$RdNBR_mean)
cor1_np<-cor.test(wetland_data$Area_km,wetland_data$RdNBR_mean,method="kendall")
cor1_np

Mean_plot<-ggplot(wetland_data,aes(x=Area_km,y=RdNBR_mean,colour=State))+theme_classic()+geom_point(aes(shape=State))+geom_smooth(method='lm',se=FALSE)+
  scale_colour_manual(values=c('#3C0A0A','#7F0D30','#C10048','red','#F98484'))+labs(y='Mean RdNBR', x=expression(bold(paste('Wetland Area (km'^2*')'))))+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'), legend.title = element_text(size = 15.5,face='bold'),
        legend.text = element_text(size=14),legend.position = c(0.82,0.9),legend.key.size = unit(2,"line"),legend.box.background = element_rect(colour = "black"))+scale_shape_manual(values=c(15, 1, 17, 0, 19))+
  labs(colour="Successional State",shape='Successional State')+guides(color = guide_legend(nrow = 2))
Mean_plot
ggsave(filename = here("Figures","Scatter1_Mean_RdNBR_Area.png"))

cor2<-lm(RdNBR_SD~Area_km,data=wetland_data)
summary(cor2)
plot(cor2)
shapiro.test(wetland_data$RdNBR_SD)
cor2_np<-cor.test(wetland_data$Area_km,wetland_data$RdNBR_SD,method="kendall")
cor2_np

St_dev_plot<-ggplot(wetland_data,aes(x=Area_km,y=RdNBR_SD,colour=State))+theme_classic()+geom_point()+geom_smooth(method='lm',se=FALSE)+
  scale_colour_manual(name="Successional States",values=c('#3C0A0A','#7F0D30','#C10048','red','#F98484'))+
  labs(y='Standard Deviation of RdNBR', x=expression(bold(paste('Wetland Area (km'^2*')'))))+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),
        axis.title.x=element_text(size=20,face='bold'), 
        axis.title.y=element_text(size=20,face='bold'),
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),
        axis.text.y=element_text(size=17,colour='black'), legend.title = element_text(size = 13,face='bold'),
        legend.text = element_text(size=12),legend.position = c(0.85,0.8),legend.box.background = element_rect(colour = "black"))+xlim(0,0.3)+scale_shape_manual(name='Successional States',values=c(15, 1, 17, 0, 19))
St_dev_plot
ggsave(filename = here("Figures","Scatter2_SD_RdNBR_Area.png"))

cor3<-lm(RdNBR_SD~RdNBR_mean,data=wetland_data)
summary(cor4)
cor3_np<-cor.test(wetland_data$RdNBR_mean,wetland_data$RdNBR_SD,method="kendall")
cor3_np

cor3_np_A<-cor.test(A$RdNBR_mean,A$RdNBR_SD,method="kendall")
cor3_np_A
cor3_np_B<-cor.test(B$RdNBR_mean,B$RdNBR_SD,method="kendall")
cor3_np_B
cor3_np_C<-cor.test(C$RdNBR_mean,C$RdNBR_SD,method="kendall")
cor3_np_C
cor3_np_D<-cor.test(D$RdNBR_mean,D$RdNBR_SD,method="kendall")
cor3_np_D
cor3_np_E<-cor.test(E$RdNBR_mean,E$RdNBR_SD,method="kendall")
cor3_np_E

SD_Mean_plot<-ggplot(wetland_data,aes(x=RdNBR_SD,y=RdNBR_mean))+theme_classic()+geom_point(aes(shape=State,colour=State),size=5)+geom_smooth(method='lm',se=FALSE,colour='black')+
  scale_colour_manual(name="Successional State", values=c('#F98484','red','#C10048','#7F0D30','#3C0A0A'))+
  labs(y='Mean RdNBR', x="Standard Deviation of RdNBR")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'),legend.title = element_text(size = 17,face='bold'),
        legend.text = element_text(size=15),legend.position = c(0.8,0.85),
        legend.box.background = element_rect(colour = "black"))+
  scale_shape_manual(name="Successional State", values=c(15, 1, 17, 0, 19))
SD_Mean_plot
ggsave(filename = here("Figures","Scatter3_Mean_RdNBR_SD_RdNBR.png"))

#Density plot

Mean_density<-ggplot(data=wetland_data,aes(x=RdNBR_mean,colour=State,fill=State))+theme_classic()+
  geom_density(alpha=0.4)+scale_colour_manual(name="Successional State",values=c('#FFAD83','#FF5600','#C10048','#7F0D30','#3C0A0A'))+
  scale_fill_manual(name="Successional State", values=c('#FFAD83','#FF5600','#C10048','#7F0D30','#3C0A0A'))+
  labs(x="Mean RdNBR",y="Relative Frequency")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        legend.title = element_text(size = 17,face='bold'),
        legend.text = element_text(size=15),legend.position = c(0.85,0.85),
        legend.box.background = element_rect(colour = "black"))
Mean_density
ggsave(filename = here("Figures","Density1_Mean_RdNBR.png"))

Mean_density<-ggplot(data=wetland_data,aes(x=RdNBR_mean,y=State,fill=stat(x)))+theme_classic()+
  geom_density_ridges_gradient(scale = 2, size = 0.3, rel_min_height = 0.0001)+scale_colour_manual(name="Successional State",values=c('#FFAD83','#FF5600','#C10048','#7F0D30','#3C0A0A'))+
  scale_fill_viridis_c(name = "Mean RdNBR", option = "C")+
  labs(x="Mean RdNBR",y="State")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        legend.title = element_text(size = 17,face='bold'),
        legend.text = element_text(size=15),legend.position = c(0.85,0.85),
        legend.box.background = element_rect(colour = "black"))
Mean_density
ggsave(filename = here("Figures","Density2_Mean_RdNBR.png"))

#Beaver Dams ----------------------

beaver_data<-filter(wetland_data,State!="E")
wilcox.test(RdNBR_mean ~ Beaver_Dam, data=beaver_data) 

Dam_Y<-filter(beaver_data,Beaver_Dam=="Y")
Dam_N<-filter(beaver_data,Beaver_Dam=="N")

mean(Dam_Y$RdNBR_mean)
sd(Dam_Y$RdNBR_mean)
mean(Dam_N$RdNBR_mean)
sd(Dam_N$RdNBR_mean)

mean(Dam_Y$Open_water)
sd(Dam_Y$Open_water)
mean(Dam_N$Open_water)
sd(Dam_N$Open_water)

barplot_10<-ggplot(data=proportion_data, aes(x=State, y=Dam_Proportion))+
  geom_bar(stat="identity", fill="#98022e")+labs(y="Proportion of Wetlands with a Beaver dam (%)",x="Successional State")+
  theme_classic()+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))
barplot_10
ggsave(filename = here("Figures","Barplot10_Dam_Proportion.png"))

boxplot_9<-ggplot(data=beaver_data, aes(x=Beaver_Dam, y=RdNBR_mean))+
  geom_boxplot(outlier.shape = NA)+labs(y="Mean RdNBR",x="Beaver Dam")+
  theme_classic()+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))+geom_jitter(alpha=0.7,colour='#98022e')
boxplot_9
ggsave(filename = here("Figures","Boxplot9_Dam_RdNBR.png"))

wilcox.test(Open_water ~ Beaver_Dam, data=beaver_data)

boxplot_10<-ggplot(data=beaver_data, aes(x=Beaver_Dam, y=Open_water))+
  geom_boxplot(outlier.shape = NA)+labs(y="Proportion of Open Water (%)",x="Beaver Dam")+
  theme_classic()+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5,lineheight=1.2),  
        axis.title.x=element_text(size=20,face='bold'),  
        axis.title.y=element_text(size=20,face='bold'),  
        axis.text.x=element_text(size=17, vjust=.5,colour='black'),  
        axis.text.y=element_text(size=17,colour='black'))+geom_jitter(alpha=0.7,colour='#98022e')
boxplot_10
ggsave(filename = here("Figures","Boxplot10_Dam_Open_Water.png"))
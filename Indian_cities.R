getwd()
setwd("~/Documents")
read.csv("cities_r2.csv")
getwd()
setwd("~/Documents")
read.csv("cities_r2.csv")
data <- read.csv('cities_r2.csv',sep=',',h=TRUE)
data
#############################PopulationTotal##################################
pop_tot <- data.frame(data$state_name, data$population_total)
pop_tot
pt_agg <- aggregate(pop_tot$data.population_total, by=list(states=pop_tot$data.state_name), FUN=sum)
pt_agg
ss <- subset.data.frame(pt_agg,pt_agg$x >15000000)
ss
write.csv(ss,"pie.csv",row.names = FALSE)
read.csv("pie.csv")
ss1 = read.csv('pie.csv',sep=',',h=TRUE)
ss1$per=round(ss1$x/sum(ss1$x)*100)
ss1
library(ggplot2)
library(plotrix)

x <- ss1$x
y <- ss1$states
lbls = paste(y,ss1$per)
lbls= paste(lbls,"%",sep="")
y
pie3D(x,labels=lbls,explode=0.1,
      main="Pie Chart of States\n(With Its Total Population Above 150lakhs) ")
?pie3D()
names(pt_agg)
########################################################################################





#========================STACKED GRAPH=========================================
##################### @State Names Vs Effective Literacy Rate Total@#####################

tot_grad <- data.frame(data$state_name, data$total_graduates)
tot_grad
tg_agg <- aggregate(tot_grad$data.total_graduates, by=list(states=tot_grad$data.state_name), FUN=sum)
tg_agg
tot_gradf <- data.frame(data$state_name, data$female_graduates)
tot_gradf
tgf_agg <- aggregate(tot_gradf$data.female_graduates, by=list(states=tot_gradf$data.state_name), FUN=sum)
tgf_agg
tg_agg$female_grad <- tgf_agg$x
tot_gradm <- data.frame(data$state_name, data$male_graduates)
tot_gradm
tgm_agg <- aggregate(tot_gradm$data.male_graduates, by=list(states=tot_gradm$data.state_name), FUN=sum)
tgm_agg
tg_agg$female_grad <- tgf_agg$x
names(tg_agg)
tg_agg$male_grad <- tgm_agg$x
colnames(tg_agg)=c("states_name","tot_grad","female_grad","male_grad")
tg_ss <- subset(tg_agg,tg_agg$tot_grad > 1000000)
names(tg_ss)
library(ggplot2)
library(gcookbook) 
e_l_r_t<- data.frame(data$state_name, data$effective_literacy_rate_total)
e_l_r_t
e_l_r_t_agg <- aggregate(e_l_r_t$data.effective_literacy_rate_total, by=list(states=e_l_r_t$data.state_name), FUN=sum)
e_l_r_t_agg
names(e_l_r_t_agg)
colnames(e_l_r_t_agg)=c("states","Effective Literacy Rate Total")

ggplot(e_l_r_t_agg, aes(x=states, y=`Effective Literacy Rate Total`, fill=states)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dotted"))+ggtitle(("State Names Vs Effective Literacy Rate Total"))
#########################################################################################################################################################


#=========================================STACKED GRAPH=================
#################################@State Names Vs Total Graduates According To Majority Gender Graduates#####################
tg_agg
subn=subset(tg_agg,tg_agg$female_grad >tg_agg$male_grad)
subn$gender="female"
subn1=subset(tg_agg,tg_agg$male_grad >tg_agg$female_grad)
subn1$gender="male"
grad_info = rbind(subn,subn1)
names(grad_info)
colnames(grad_info)=c("StateName","TotalGraduates","FemaleGraduates","MaleGraduates","Gender")
ggplot(grad_info, aes(x=StateName, y=TotalGraduates,fill=Gender)) +
  geom_bar(stat="identity")+                        
  # Use a larger dot
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dotted"))+ggtitle(("State Names Vs Total Graduates According To Majority Gender Graduates"))
########################################################################################################

###########################@Total Literates Vs Total Graduates In Each City Of Maharashtra@#####
tot_grad <- data.frame(data$state_name, data$total_graduates)
tot_grad
tg_agg <- aggregate(tot_grad$data.total_graduates, by=list(states=tot_grad$data.state_name), FUN=sum)
tg_agg
li_tot <- data.frame(data$state_name,data$literates_total)
li_tot
lt_agg <- aggregate(li_tot$data.literates_total,by=list(states=li_tot$data.state_name),FUN=sum)
lt_agg
tg_agg$lit_tatal <- lt_agg$x
colnames(tg_agg)=c("states_name","tot_grad","tot_lit")
names(tg_agg)
library(gcookbook)
bsub <- subset.data.frame(data,data$state_name == "MAHARASHTRA")
names(bsub)
bsub1 <- subset(bsub,bsub$population_total>1000000)
TotalPopulation=bsub1$population_total
TotalLiterates=bsub1$literates_total
qplot(TotalPopulation,TotalLiterates,data=bsub1,color=name_of_city,size=total_graduates,main="Total Literates Vs Total Graduates In Each City Of Maharashtra")

#########################################################################################

##########################PopulationTotal(0-6)VsPopulationGender(0-6)###############
tot_child <- data.frame(data$state_name, data$X0.6_population_male)
tot_child
tc_agg <- aggregate(tot_child$data.X0.6_population_male, by=list(states=tot_child$data.state_name), FUN=sum)
tc_agg
colnames(tc_agg)=c("States","PopulationTotal0_6")
names(tc_agg)

tot_childM <- data.frame(data$state_name, data$X0.6_population_female)
tot_childM
tcm_agg <- aggregate(tot_childM$data.X0.6_population_female, by=list(states=tot_childM$data.state_name), FUN=sum)
tcm_agg
names(tcm_agg)
tot_childF <- data.frame(data$state_name, data$X0.6_population_male)
tot_childF
tcf_agg <- aggregate(tot_childF$data.X0.6_population_male, by=list(states=tot_childF$data.state_name), FUN=sum)
tcf_agg
names(tcf_agg)
colnames(tcm_agg)=c("States","x")
names(tc_agg)
tc_agg$PopulationMale0_6 <- tcf_agg$x
tc_agg$PopulationFemale0_6 <- tcm_agg$x
names(tc_agg)
PopulationGender <- tc_agg$PopulationMale0_6
ggplot(tc_agg, aes(PopulationTotal0_6)) + 
  geom_line(aes(y =PopulationGender,  colour = "PopulationGender")) +
  geom_line(aes(y =PopulationFemale0_6,  colour = "PopulationFemale0_6")) +ggtitle("PopulationTotal(0-6)VsPopulationGender(0-6)")
##########################################################################################


############################################# regression for TotalGraduatesVsTotalLiterates##########################
li_tot <- data.frame(data$state_name,data$literates_total)
li_tot
lt_agg <- aggregate(li_tot$data.literates_total,by=list(states=li_tot$data.state_name),FUN=sum)
lt_agg
tot_grad <- data.frame(data$state_name, data$total_graduates)
tot_grad
tg_agg <- aggregate(tot_grad$data.total_graduates, by=list(states=tot_grad$data.state_name), FUN=sum)
tg_agg
lt_agg$total_grad <- tg_agg$x
colnames(lt_agg)<-c("states","total_lit","total_grad")
x <- c(lt_agg$total_lit)
y <- c(lt_agg$total_grad)
relation <- lm(y~x)
a <- data.frame(x=1600000)
result <- predict(relation,a)
print(result)
png(file="linear_regression.png")
plot(x,y,col=lt_agg$states,main="Literates & Graduates- Regression",
     abline(lm(y~x)),cex=1.3,pch=16,xlab="Total Literates",ylab="Total Graduates")
dev.off()

##########################################################################################

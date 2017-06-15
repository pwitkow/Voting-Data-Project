
#Trump Vs. Clinton Graph

windows()
daters$Bins<-cut(daters$Count, c(0,50,100,500,10500))

ggplot(daters, aes(x=DScore, y=Prop.H, color="black",linetype='solid')) +
    geom_point(aes(size =Bins, shape="solid",alpha=.2, color='black'),pch=21,bg='gray') + 
    geom_text(hjust = 1, size = 2, label=' ') +
	coord_cartesian(ylim=c(.1,1)) +
  	stat_smooth(method="lm", fullrange=T, se=F)+
	#geom_abline(aes(intercept=0.4742, slope=-.102984, color="black", linetype='dotted'), size=1)+
	
	xlab("Standardized Explicit Gender-Career Bias")+
	ylab("Proportion of Votes \n for Clinton")+
	guides(alpha='none')+guides(linetype=FALSE)+
	scale_color_manual(values=c('black'), guide=F)+
	#scale_linetype_manual(name="Comparison",
	#	values=c("solid", "dotted"),labels=c("Clinton v Sanders","Clinton v. Trump"
	#				))+
	scale_size_discrete(name="Project Implicit \nResponses Per County", 
		labels=c("20-50","51-100","101-500",">500"))+
	theme_bw()+
	theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border =element_rect(colour = "black", fill=NA, size=2),
	panel.background = element_blank())
	#labs(size='Population Size')


#Sanders v. Hillary 


Bdaters$Bins<-cut(Bdaters$Count, c(0,50,100,500,10500))
windows()
ggplot(Bdaters, aes(x=ExpBias, y=Prop.H, color="black",linetype=as.factor(Caucus))) +
    geom_point(aes(size =Bins, shape="solid",alpha=.2, color='black'),pch=21,bg='gray') + 
    geom_text(hjust = 1, size = 2, label=' ') +
	coord_cartesian(ylim=c(.1,1), xlim=c(-4, 6)) +
  	stat_smooth(method="lm", fullrange=T, se=F)+
	#geom_smooth(daters[daters$Caucus=1,],aes(x=DScore, y=Prop.H, color="black",linetype='solid'), 
	#						method=lm, se=FALSE)+
	
	xlab("Standardized Explicit Gender-Career Bias")+
	ylab("Proportion of Votes \n for Clinton")+
	guides(alpha='none')+#guides(linetype=FALSE)+
	scale_color_manual(values=c('black'), guide=F)+
	scale_linetype_manual(name="Comparison",
	values=c("solid", "dotted"),labels=c("Primary","Caucus"))+
	scale_size_discrete(name="Project Implicit \nResponses Per County", 
		labels=c("20-50","51-100","101-500",">500"))+
	theme_bw()+
	theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border =element_rect(colour = "black", fill=NA, size=2),
	panel.background = element_blank())
	#labs(size='Population Size')

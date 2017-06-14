

windows()
ggplot(daters, aes(x=AssoCareer, y=Prop.H, color="black",linetype='solid')) +
    geom_point(aes(size =Bins, shape="solid",alpha=.2, color='black'),pch=21,bg='gray') + 
    geom_text(hjust = 1, size = 2, label=' ') +
	coord_cartesian(ylim=c(0,1)) +
  	stat_smooth(method="lm", fullrange=T, se=F)+
	geom_abline(aes(intercept=0.4742, slope=-.102984, color="black", linetype='dotted'), size=1)+
	
	xlab("Standardized Explicit Gender-Career Bias")+
	ylab("Proportion of Votes for Clinton")+
	guides(alpha='none')+
	scale_color_manual(values=c('black'), guide=F)+
	scale_linetype_manual(name="Comparison",
		values=c("solid", "dotted"),labels=c("Clinton v Sanders","Clinton v. Trump"
					))+
	scale_size_discrete(name="Project Implicit \nResponses Per County", 
		labels=c("20-50","51-100","101-500",">500"))+
	theme_bw()+
	theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border =element_rect(colour = "black", fill=NA, size=2),
	panel.background = element_blank())
	#labs(size='Population Size')
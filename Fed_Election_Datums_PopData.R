setwd("C:\\Users\\Phillip\\Google Drive\\Where Bais Against Females Berns You - A Study of Implicit Bias and Voting Data\\Data Files\\Fed_Election_Data")
#load libraries
library('plyr')
library('nlme')
library('ggplot2')
library('Hmisc')
install.packages("ggplot2", repos="http://cran.rstudio.com/", dependencies=TRUE)

#Functions_________________________________________________________________________________________________________________________

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}
cleanNames<-function(x){
	s<-gsub("De Witt", "Dewitt", x, fixed=T)
	s<-gsub("Saint ", "St. ", s, fixed=T)
	return(s)
}
addCounty<-function(x){
	d<-paste(x, "County", sep=' ')
	return(d)
}
difGates<-function(x,y, z){
	daters<-MainData[MainData$Count>=20,]
	moodel<-lm(Prop.H~(DScore+(Age+Sex+Asian+Black+Latin+White+EduLevel+Income+Poli+AssoCareer+AssoFamily)),
			data=daters, na.action=na.omit)
	return(coef(summary(moodel))[z,y])
}
	
#data files________________________________________________________________________________________________________________________

#Load the FIPS table and make some adjustments so its standardized with voting data
matchCodes<-read.table("C:\\Users\\Phillip\\Google Drive\\Where Bais Against Females Berns You - A Study of Implicit Bias and Voting Data\\Project Implicit Data\\CountyCodeName.txt",
				sep=",", header=F, quote="", col.names=c("State", "StateEFP", "CountyFP", "CountyName", "ClassFP"))
matchCodes<-as.data.frame(matchCodes)
matchCodes$FIPS<-	#merge state abbv. with county number for FIPS 
	gsub(" ", "",(do.call(paste, as.data.frame(matchCodes[,c(1,3)], stringsAsFactors=FALSE))), fixed=T)
#matchCodes$CountyFP<-ifelse(matchCodes$CountyFP<100 &matchCodes$CountyFP>=10 , paste("0",matchCodes$CountyFP, sep=''),
#				ifelse(matchCodes$CountyFP<10, paste("00",matchCodes$CountyFP, sep=''),
#				matchCodes$CountyFP))
matchCodes$Nums<-gsub(" ", "",(do.call(paste, as.data.frame(matchCodes[,c(2,3)], stringsAsFactors=FALSE))), fixed=T)
matchCodes$CountyName<-as.character(matchCodes$CountyName)
matchCodes$CountyName<-tolower(matchCodes$CountyName)	#lc Everything
matchCodes$CountyName<-unlist(lapply(matchCodes$CountyName,simpleCap)) #UC first letter in each word
matchCodes$CountyName<-unlist(lapply(matchCodes$CountyName,cleanNames))

#AK is all one district
matchCodes[matchCodes$State=='AK',]$FIPS<-matchCodes[matchCodes$State=='AK',]$FIPS[1]
matchCodes[matchCodes$State=='AK',]$Nums<-matchCodes[matchCodes$State=='AK',]$Nums[1]

#HAND WRITTEN COUNTY BECAUSE THEY THEIR VOTING DISTRICTS AREN'T THE SAME THING
disOne<-c('Rock', "Nobles",'Jackson', 'Martin','Faribault', 'Freeborn','Mower','Fillmore', 'Houston',
	    'Winona','Olmsted','Dodge', 'Steele', 'Waseca', 'Blue Earth', 'Watonwan', 'Brown','Nicollet',
	    'Le Sueur', 'Rice')
disOne<-unlist(lapply(disOne, addCounty))
disTwo<-c("Goodhue", 'Wabasha', 'Dakota', "Scott")
disTwo<-unlist(lapply(disTwo, addCounty))
disThree<-c('Hennepin County')	#Also dis5
disFour<-c('Ramsey County')
disSix<-c('Benton','Sherburne','Anoka','Washington','Wright','Carver')
disSix<-unlist(lapply(disSix, addCounty))
disSeven<-c('Cottonwood', 'Murray', 'Pipestone','Lincoln', 'Lyon','Redwood','Yellow Medicine',
		'Renville','Sibley','Mcleod','Lac Qui Parle', 'Chippewa', 'Kandiyohi','Meeker', 'Big Stone',
		'Swift', 'Traverse', 'Stevens', 'Pope','Grant','Douglas','Todd','Wilkin','Otter Tail','Clay',
		'Becker', 'Norman', 'Mahnomen','Polk','Clearwater','Beltrami','Red Lake', 'Pennington','Marshall', 
		'Kittson','Roseau', 'Lake Of The Woods')
disSeven<-unlist(lapply(disSeven, addCounty))
disEight<-c('Koochiching', 'St. Louis','Lake','Cook','Itasca','Hubbard','Cass','Wadena', 'Crow Wing',
		'Aitkin','Carlton','Morrison','Mille Lacs','Kanabec','Pine','Isanti','Chisago')
disEight<-unlist(lapply(disEight, addCounty))

#Give them the names
matchCodes[matchCodes$State=='MN',]$CountyName<-
	ifelse(matchCodes[matchCodes$State=='MN',]$CountyName %in% disOne, '1st District',
	ifelse(matchCodes[matchCodes$State=='MN',]$CountyName %in% disTwo, '2nd District',
	ifelse(matchCodes[matchCodes$State=='MN',]$CountyName %in% disThree, '3rd District',
	ifelse(matchCodes[matchCodes$State=='MN',]$CountyName %in% disFour, '4th District',
	ifelse(matchCodes[matchCodes$State=='MN',]$CountyName %in% disSix, '6th District',
	ifelse(matchCodes[matchCodes$State=='MN',]$CountyName %in% disSeven, '7th District',
	ifelse(matchCodes[matchCodes$State=='MN',]$CountyName %in% disEight, '8th District', 'Der'
								)))))))


#Voting data________________________________________________________________________________________________________________

vData<-list.files(pattern='.csv')
vData<-lapply(vData, read.csv, head=T, stringsAsFactors=F)
votData<-do.call(rbind, vData)

#organize a bunch of stuff, like getting rid of commas in numbers, dashes, making captial letters
#are standardized all all that stuff

votData$Popular<-as.integer(votData$Popular)
votData$State=gsub('-'," ", votData$State, fixed=T) #Take Dashes out of names
votData$State<-unlist(lapply(votData$State,simpleCap))#Capitalize First Letter in each name
votData$Place<-as.character(votData$Place)
votData$Place<-tolower(votData$Place) #lc everything 
votData$Place<-gsub("&apos;","'", votData$Place, fixed=T)
votData$Place<-unlist(lapply(votData$Place,simpleCap))	#UC everything in each 


#Convert to Abbvs for codes
votData$State<-state.abb[match(votData$State, state.name)]	


votData$Place<-unlist(lapply(votData$Place,cleanNames))	
votData[votData$State=='VA' & votData$Place=='James City',]$Place<-"James City County"
votData[votData$State=='VA' & votData$Place=='Charles City',]$Place<-"Charles City County"


#match state names for FIPS codes
vM <- match(paste(votData$State,votData$Place),
		paste(matchCodes$State,matchCodes$CountyName))
votData$FIPS<-matchCodes$FIPS[vM]
votData$Nums<-matchCodes$Nums[vM]

votData<-votData[!is.na(votData$State),]

#AK is all one district
votData[votData$State=="AK" ,]$FIPS<-matchCodes[matchCodes$State=='AK',]$FIPS[1]
votData[votData$State=="AK",]$Nums<-matchCodes[matchCodes$State=='AK',]$Nums[1]

#IAT DATA-------------------------------------------------------------------------------------------------------------------------------------------
iatData<-read.csv("C:\\Users\\Phillip\\Google Drive\\Where Bais Against Females Berns You - A Study of Implicit Bias and Voting Data\\Project Implicit Data\\Gender-Career IAT.public.2005-2015.csv", head=T)

#load data into data-frame
iatDataframe<-data.frame(Fin=iatData$session_status, dScore=iatData$D_biep.Male_Career_all,
			year=iatData$year, County=iatData$CountyNo, State=iatData$STATE, 
			AssoCareer=iatData$assocareer, AssoFamily=iatData$assofamily)


#Only use Complete data
iatDataframe<-iatDataframe[iatDataframe$Fin=='C',]

#merge state abbv. with county number for FIPS 
iatDataframe$FIPS<-	
	gsub(" ", "",(do.call(paste, as.data.frame(iatDataframe[,c("State","County")], stringsAsFactors=FALSE))), fixed=T)


#Make all alsaka and maine respondents from the same voting district
iatDataframe[iatDataframe$State=="AK",]$FIPS<-matchCodes[matchCodes$State=='AK',]$FIPS[1]
iatDataframe[iatDataframe$State=="ME",]$FIPS<-matchCodes[matchCodes$State=='ME',]$FIPS[1]


#get all the fips that need to be replace with the right fips for minneosta
mNFips1<-matchCodes[matchCodes$State=='MN' & matchCodes$CountyName=='1st District',]$FIPS
mNFips2<-matchCodes[matchCodes$State=='MN' & matchCodes$CountyName=='2nd District',]$FIPS
mNFips6<-matchCodes[matchCodes$State=='MN' & matchCodes$CountyName=='6th District',]$FIPS
mNFips7<-matchCodes[matchCodes$State=='MN' & matchCodes$CountyName=='7th District',]$FIPS
mNFips8<-matchCodes[matchCodes$State=='MN' & matchCodes$CountyName=='8th District',]$FIPS

iatDataframe[iatDataframe$State=='MN',]$FIPS<-
	ifelse(iatDataframe[iatDataframe$State=='MN',]$FIPS %in% mNFips1, mNFips1[1],
	ifelse(iatDataframe[iatDataframe$State=='MN',]$FIPS %in% mNFips2, mNFips2[1],
	ifelse(iatDataframe[iatDataframe$State=='MN',]$FIPS %in% mNFips6, mNFips6[1],
	ifelse(iatDataframe[iatDataframe$State=='MN',]$FIPS %in% mNFips7, mNFips7[1],
	ifelse(iatDataframe[iatDataframe$State=='MN',]$FIPS %in% mNFips8, mNFips8[1],
	iatDataframe[iatDataframe$State=='MN',]$FIPS)))))


#Transfrom explicit measures into z-scores
iatDataframe$AssoCareer<-(iatDataframe$AssoCareer-mean(iatDataframe$AssoCareer, 
					na.rm=T))/sd(iatDataframe$AssoCareer, na.rm=T) 
iatDataframe$AssoFamily<-(iatDataframe$AssoFamily-mean(iatDataframe$AssoFamily, 
					na.rm=T))/sd(iatDataframe$AssoFamily, na.rm=T)



#Condense all variables by FIPS
Final<-ddply(iatDataframe, c("FIPS"), summarise, DScore=mean(dScore, na.rm=T),
					Count=length(FIPS), Pop=length(dScore), State=State[1],
					AssoFamily=mean(AssoFamily, na.rm=T),AssoCareer=mean(AssoCareer, na.rm=T))

#______________________________________________________________________________________________________________

#______________________________________________________________________________________________________________
#population data
Pop_data<-read.csv("C:\\Users\\Phillip\\Google Drive\\Where Bais Against Females Berns You - A Study of Implicit Bias and Voting Data\\County Demographics Master.csv",
						 head=T)
Pop_data<-as.data.frame(Pop_data)

state_abb<-function(x){
	v=state.abb[grep(x, state.name)][1]
	return(v)}

#get state names and attach to fips code
Pop_data$State<-gsub("^.*\\, " , "" ,Pop_data$County.Name)
Pop_data$State<-unlist(lapply(Pop_data$State, state_abb))

#remove state numbers and do some trickery to get zeros off
Pop_data$FIPS<-substring(as.character(Pop_data$FIPS),3)
Pop_data$FIPS<-as.numeric(Pop_data$FIPS)
Pop_data$FIPS<-as.character(Pop_data$FIPS)

Pop_data$FIPS<-	#merge state abbv. with county number for FIPS 
	gsub(" ", "",(do.call(paste, as.data.frame(Pop_data[,c(14,1)], stringsAsFactors=FALSE))), fixed=T)
#Political 2012 data_________________________________________________________________________
poli_data=read.csv("C:\\Users\\Phillip\\Google Drive\\Where Bais Against Females Berns You - A Study of Implicit Bias and Voting Data\\US_elect_county.csv", head=T)
poli_data<-as.data.frame(poli_data)
 #remove all the state "headers"
poli_data<-poli_data[poli_data$FIPS!=0,]
 #trickery for getting county fips
poli_data$FIPS<-substr(
		as.character(poli_data$FIPS), start=3, nchar(poli_data$FIPS))#remove first to elements of the string
poli_data$FIPS<-as.numeric(poli_data$FIPS)
poli_data$FIPS<-as.character(poli_data$FIPS)

 #combine with state abbv
poli_data$FIPS<-
	gsub(" ", "",(do.call(paste, as.data.frame(poli_data[,c(1,3)], stringsAsFactors=FALSE))), fixed=T)

 #take proportion of votes for obama
 #X.=obama votes , X..1=Romney Vote
poli_data$Party<-as.numeric(poli_data$X.)/(as.numeric(poli_data$X.)+as.numeric(poli_data$X..1))


#Religion Stuff__________________________________________________________________________
rel_data<-read.csv("C:\\Users\\Phillip\\Google Drive\\Where Bais Against Females Berns You - A Study of Implicit Bias and Voting Data\\rel_data.csv", head=T)
rel_data<-as.data.frame(rel_data)

#get just the necessary variables
rel_data<-rel_data[,c("FIPS", "STNAME", "TOTRATE")]

#get the state Abb's
rel_data$Abb<-unlist(lapply(rel_data$STNAME, state_abb))

#get rid of zeros
rel_data$FIPS<-substring(as.character(rel_data$FIPS),3)
rel_data$FIPS<-as.numeric(rel_data$FIPS)
rel_data$FIPS<-as.character(rel_data$FIPS)

rel_data$FIPS<-
	gsub(" ", "",(do.call(paste, as.data.frame(rel_data[,c(4,1)], stringsAsFactors=FALSE))), fixed=T)
rel_data<-rel_data[complete.cases(rel_data),]
#Turn it into z-scores
rel_data$TOTRATEZ<-(rel_data$TOTRATE-(mean(rel_data$TOTRATE, na.rm=T)))/sd(rel_data$TOTRATE, na.rm=T)

#Doing Stuff__________________________________________________________________________________________________________					
#Main Model data shaping and analysis

MainData<-votData[votData$Candidate=='H.Clinton' |votData$Candidate=='D.Trump',-c(1)]

#Reshape to long
MainData<-w <- reshape(MainData, timevar = "Candidate",
  idvar = c("State", "Place", "FIPS", "Nums"),direction = "wide")
MainData$Prop.H<-MainData$Popular.H.Clinton/(MainData$Popular.H.Clinton+MainData$Popular.D.Trump)

#Attach County IAT-Data
mM<-match(MainData$FIPS, Final$FIPS)
dM<-match(MainData$FIPS, Pop_data$FIPS)
pM<-match(MainData$FIPS, poli_data$FIPS)
rM<-match(MainData$FIPS, rel_data$FIPS)


#apply Dscore
MainData$DScore<-Final$DScore[mM]
MainData$AssoFamily<-Final$AssoFamily[mM]
MainData$AssoCareer<-Final$AssoCareer[mM]
MainData$CheckFIPS<-Final$FIPS[mM]
MainData$Count<-Final$Count[mM]
#demographic data
MainData$White<-Pop_data$White[dM]/Pop_data$Total[dM]
MainData$Black<-Pop_data$Black[dM]/Pop_data$Total[dM]
MainData$Latin<-Pop_data$Latino[dM]/Pop_data$Total[dM]
MainData$Asian<-Pop_data$Asian[dM]/Pop_data$Total[dM]
MainData$Income<-Pop_data$HH.Income[dM]
MainData$EduLevel<-Pop_data$X.BachelorAbove[dM]
MainData$Age<-Pop_data$Median.Age[dM]
MainData$Sex<-Pop_data$Sex.ratio..males.per.100.females[dM]/100

#higher means more Liberal
MainData$Poli<-poli_data$Party[pM]

#Religious Data
MainData$Religous<-rel_data$TOTRATEZ[rM]


#Remove All Unmatched Counties

MainData<-MainData[!(is.na(MainData$CheckFIPS)),]
#Set cutoffs in data for model
daters<-MainData#[MainData$Count>=20,]

#get rid of NA rows 
daters<-daters[!(rowSums(is.na(daters)) > 0),]


corTests<-daters[,c(12,11,10,21,22,18,16,17,15,20,19,23,24)]
corTests<-corTests[complete.cases(corTests),]
#corTest needs to be a matrix
corrs<-rcorr(as.matrix(corTests))$r
ps<-rcorr(as.matrix(corTests))$P



MainModel<-lm(Prop.H~DScore
			+Age  #Avg age of county
			+Sex	# % of females
			+Asian #% of Asians
			+Black #% African American
			+Latin #%Latin American	
			+White #% White American
			+EduLevel #Avg Edu level 
			+Income # Avg Income
			+Poli	#Avg political standing
			+AssoCareer	#Avg degree Explicit men-career
			+AssoFamily #Avg degree of Explicit women-family
			+Religous,
			data=daters, na.action=na.omit)
summary(MainModel, correlation=F)

#add cooks distance
daters$cooksDis<-cooks.distance(MainModel)

#take out the outlies and rerun 
daters<-daters[daters$cooksDis<=0.001753,] #N & K values in excel sheet


#stuff to print model results
library(broom)
#get the model in a matrix
modelTable<-tidy(MainModel)


#plotty bits
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



setwd("C:\\Users\\Phillip\\Google Drive\\Where Bais Against Females Berns You - A Study of Implicit Bias and Voting Data\\Data Files\\Fed_Election_Data")
#load libraries here
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

#HAND WRITTEN COUNTY BECAUSE THEY THEIR VOTING DISTRICTS AREN'T THE SAME THING!!
#Can you belive it?
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

#standardize variables by getting rid of commas in numbers, dashes, making captial letters

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

#IAT Data________________________________________________________________________________________________________________

iatData<-read.csv("C:\\Users\\Phillip\\Google Drive\\Where Bais Against Females Berns You - A Study of Implicit Bias and Voting Data\\Project Implicit Data\\Gender-Career IAT.public.2005-2015.csv", head=T)

#load data into data-frame
iatDataframe<-data.frame(Fin=iatData$session_status, dScore=iatData$D_biep.Male_Career_all,
	Age=iatData$age, year=iatData$year, eduLevel=iatData$edu_14, ethnic=iatData$ethnic, sex=iatData$sex,
	County=iatData$CountyNo, State=iatData$STATE, Income=iatData$anninc, PoliSix=iatData$politicalid_6, 
	PoliSev=iatData$politicalid_7, AssoCareer=iatData$assocareer, AssoFamily=iatData$assofamily, 
	ID=iatData$session_id, Religiosity=iatData$religionid)

#Only use Complete data
iatDataframe<-iatDataframe[iatDataframe$Fin=='C',]

#_____Changing age to a z-score_______#
iatDataframe$Age<-(iatDataframe$Age-mean(iatDataframe$Age, 
					na.rm=T))/sd(iatDataframe$Age, na.rm=T)
#______________________________________#

#merge state abbv. with county number for FIPS 
iatDataframe$FIPS<-	
	gsub(" ", "",(do.call(paste, as.data.frame(iatDataframe[,c("State","County")], stringsAsFactors=FALSE))), fixed=T)


#Make all alsaka respondents from the same voting district
iatDataframe[iatDataframe$State=="AK",]$FIPS<-matchCodes[matchCodes$State=='AK',]$FIPS[1]


#Reform minnosota because its coutnies != voting districts

	#get all the fips that need to be replace
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

iatDataframe$eduLevel<-ifelse(iatDataframe$eduLevel>=10, 10, iatDataframe$eduLevel)

#_____Changing eduLevel to a z-score_______#
iatDataframe$eduLevel<-(iatDataframe$eduLevel-mean(iatDataframe$eduLevel, 
					na.rm=T))/sd(iatDataframe$eduLevel, na.rm=T)
#__________________________________________#

#_____Changing Income to a z-score_______#
iatDataframe$Income<-(iatDataframe$Income-mean(iatDataframe$Income, 
					na.rm=T))/sd(iatDataframe$Income, na.rm=T)
#__________________________________________#

iatDataframe$Males<-ifelse(iatDataframe$sex=='m', 1,0)
iatDataframe$Females<-ifelse(iatDataframe$sex=='f', 1,0)


#Binary of whether a respondant was part of some race; 1 is yes
# 0 is no
iatDataframe$White<-ifelse(iatDataframe$ethnic==5, 1, 0)
iatDataframe$Black<-ifelse(iatDataframe$ethnic==3, 1, 0)
iatDataframe$Latin<-ifelse(iatDataframe$ethnic==4, 1, 0)
iatDataframe$Asian<-ifelse(iatDataframe$ethnic==2, 1, 0)

#Turn politcal scores into z socre then combine them into new column
meanPoli6<-mean(iatDataframe$PoliSix, na.rm=T)
sdPoli6<-sd(iatDataframe$PoliSix, na.rm=T)

meanPoli7<-mean(iatDataframe$PoliSev, na.rm=T)
sdPoli7<-sd(iatDataframe$PoliSix, na.rm=T)
iatDataframe$PoliScore<-ifelse(is.na(iatDataframe$PoliSix), #if Poli6 has "NA" value
				((iatDataframe$PoliSev-meanPoli7)/sdPoli7),
				((iatDataframe$PoliSix-meanPoli6)/sdPoli6))



#Transfrom explicit measures into z-scores
iatDataframe$AssoCareer<-(iatDataframe$AssoCareer-mean(iatDataframe$AssoCareer, 
					na.rm=T))/sd(iatDataframe$AssoCareer, na.rm=T) 
iatDataframe$AssoFamily<-(iatDataframe$AssoFamily-mean(iatDataframe$AssoFamily, 
					na.rm=T))/sd(iatDataframe$AssoFamily, na.rm=T)

#standardize relgiosity scores
iatDataframe$Religiosity<-(iatDataframe$Religiosity-mean(iatDataframe$Religiosity, 
					na.rm=T))/sd(iatDataframe$Religiosity, na.rm=T)

#Take out political party if necssary for analysis
#iatDataframe<-iatDataframe[iatDataframe$PoliScore>0,]



#Condense all variables by FIPS
Final<-ddply(iatDataframe, c("FIPS"), summarise, Income=median(Income, na.rm=T),
							     DScore=mean(dScore, na.rm=T),
			     Age=mean(Age, na.rm=T), EduLevel=mean(eduLevel, na.rm=T),
			     Sex=(mean(Females, na.rm=T))/((mean(Females, na.rm=T)+ mean(Males, na.rm=T))),
			     White=mean(White,na.rm=T), Black=mean(Black, na.rm=T), Latin=mean(Latin, na.rm=T),
				Asian=mean(Asian, na.rm=T), Poli=mean(PoliScore, na.rm=T),
				AssoFamily=mean(AssoFamily, na.rm=T),AssoCareer=mean(AssoCareer, na.rm=T),
				Count=length(FIPS), Pop=length(dScore))

#____Percentage of Latin z-Score____#
Final$Latin<-(Final$Latin-mean(Final$Latin, 
					na.rm=T))/sd(Final$Latin, na.rm=T)
#__________________________________________#

#____ Religion_________________________________________--#
rel_data<-read.csv("C:\\Users\\Phillip\\Google Drive\\Where Bais Against Females Berns You - A Study of Implicit Bias and Voting Data\\rel_data.csv", head=T)
rel_data<-as.data.frame(rel_data)

#get just the necessary variables
rel_data<-rel_data[,c("FIPS", "STNAME", "TOTRATE")]

#get the state Abb's
rel_data$Abb<-state.abb[match(rel_data$STNAME, state.name)]	

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

#get only trump and hillary data
MainData<-votData[votData$Candidate=='H.Clinton' |votData$Candidate=='D.Trump',-c(1)]

#Reshape to long
MainData<-w <- reshape(MainData, timevar = "Candidate",
  idvar = c("State", "Place", "FIPS", "Nums"),direction = "wide")
MainData$Prop.H<-MainData$Popular.H.Clinton/(MainData$Popular.H.Clinton+MainData$Popular.D.Trump)

#Attach County IAT-Data
mM<-match(MainData$FIPS, Final$FIPS)
MainData$Income<-Final$Income[mM]
MainData$DScore<-Final$DScore[mM]
MainData$Age<-Final$Age[mM]
MainData$EduLevel<-Final$EduLevel[mM]
MainData$Sex<-Final$Sex[mM]
MainData$White<-Final$White[mM]
MainData$Black<-Final$Black[mM]
MainData$Latin<-Final$Latin[mM]
MainData$Asian<-Final$Asian[mM]
MainData$Poli<-Final$Poli[mM]
MainData$AssoFamily<-Final$AssoFamily[mM]
MainData$AssoCareer<-Final$AssoCareer[mM]
MainData$CheckFIPS<-Final$FIPS[mM]
MainData$Count<-Final$Count[mM]
MainData$Nums<-as.numeric(MainData$Nums)

#Religion
rM<-match(MainData$FIPS, rel_data$FIPS)
MainData$Religous<-rel_data$TOTRATEZ[rM]

#Remove All Unmatched Counties
MainData<-MainData[!(is.na(MainData$CheckFIPS)),]
#Set cutoff and do statistics stuff
daters<-MainData#[MainData$Count>=20,]

#get rid of NA rows 
daters<-daters[rowSums(is.na(daters)) <= 0,]

corTests<-daters[,c(21,20,11,12,14,18,16,17,15,13,10,19,24)]
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

#add cooks distance numbers to the data frame 
daters$cooksDis<-cooks.distance(MainModel)

daters<-daters[daters$cooksDis<=0.002137,] #N & K values in excel sheet

daters$Bins<-cut(daters$Count, c(20,50,100,500,10500))

#bubble plot for (blank) scorce predicting Hillary votes
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






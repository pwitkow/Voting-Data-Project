# Title : Primaries with PI data 
setwd("C:\\Users\\Phillip\\Google Drive\\Where Bais Against Females Berns You - A Study of Implicit Bias and Voting Data\\Data Files\\Voting Data")
#put whatever libraries here
library('plyr')
library('nlme')
library('ggplot2')
library('Hmisc')
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
state_abb<-function(x){
	v=state.abb[grep(x, state.name)][1]
	return(v)}
	
#data files________________________________________________________________________________________________________________________

#Load the FIPS table and make some adjustments so its standardized with voting data
matchCodes<-read.table("C:\\Users\\Phillip\\Google Drive\\Where Bais Against Females Berns You - A Study of Implicit Bias and Voting Data\\Project Implicit Data\\CountyCodeName.txt",
				sep=",", header=F, quote="", col.names=c("State", "StateEFP", "CountyFP", "CountyName", "ClassFP"))
matchCodes<-as.data.frame(matchCodes)
matchCodes$FIPS<-	#merge state abbv. with county number for FIPS 
	gsub(" ", "",(do.call(paste, as.data.frame(matchCodes[,c(1,3)], stringsAsFactors=FALSE))), fixed=T)
matchCodes$CountyFP<-ifelse(matchCodes$CountyFP<100 &matchCodes$CountyFP>=10 , paste("0",matchCodes$CountyFP, sep=''),
				ifelse(matchCodes$CountyFP<10, paste("00",matchCodes$CountyFP, sep=''),
				matchCodes$CountyFP))
matchCodes$Nums<-gsub(" ", "",(do.call(paste, as.data.frame(matchCodes[,c(2,3)], stringsAsFactors=FALSE))), fixed=T)
matchCodes$CountyName<-as.character(matchCodes$CountyName)
matchCodes$CountyName<-tolower(matchCodes$CountyName)	#lc Everything
matchCodes$CountyName<-unlist(lapply(matchCodes$CountyName,simpleCap)) #UC first letter in each word
matchCodes$CountyName<-unlist(lapply(matchCodes$CountyName,cleanNames))

#AK is all one district
matchCodes[matchCodes$State=='AK',]$FIPS<-matchCodes[matchCodes$State=='AK',]$FIPS[1]

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

#organize a bunch of stuff, like getting rid of commas in numbers, dashes, making captial letters
#are standardized all all that stuff
votData$Popular<-gsub(',',"", votData$Popular, fixed=T)
votData$Popular<-as.integer(votData$Popular)
votData$State=gsub('-'," ", votData$State, fixed=T) #Take Dashes out of names
votData$State<-unlist(lapply(votData$State,simpleCap))#Capitalize First Letter in each name
votData$Place<-as.character(votData$Place)
votData$Place<-tolower(votData$Place) #lc everything 
votData$Place<-gsub("&apos;","'", votData$Place, fixed=T)
votData$Place<-unlist(lapply(votData$Place,simpleCap))	#UC everything in each 


#Convert to Abbvs for codes
votData$State<-state.abb[match(votData$State, state.name)]	

#Clean up some peculiar Crap
votData$Place<-unlist(lapply(votData$Place,cleanNames))	
votData[votData$State=='VA' & votData$Place=='James City',]$Place<-"James City County"
votData[votData$State=='VA' & votData$Place=='Charles City',]$Place<-"Charles City County"


#match state names for FIPS codes
vM <- match(paste(votData$State,votData$Place),
		paste(matchCodes$State,matchCodes$CountyName))
votData$FIPS<-matchCodes$FIPS[vM]
votData$Nums<-matchCodes$Nums[vM]

#AK is all one district
votData[votData$State=="AK",]$FIPS<-matchCodes[matchCodes$State=='AK',]$FIPS[1]



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


#Dscore becores z-scored
iatDataframe$dScore<-(iatDataframe$dScore-mean(iatDataframe$dScore, 
					na.rm=T))/sd(iatDataframe$dScore, na.rm=T)


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
#In this authors opinion, it should be declared a wildlife
#sancutary for gerry-manders!

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




#combine explicit measures
iatDataframe$ExpBias<-iatDataframe$AssoCareer-iatDataframe$AssoFamily
iatDataframe$ExpBias<-(iatDataframe$ExpBias-mean(iatDataframe$ExpBias, 
					na.rm=T))/sd(iatDataframe$ExpBias, na.rm=T)

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
Final<-ddply(iatDataframe, c("FIPS"), summarise, Income=median(Income, na.rm=T),Pop=length(dScore),	     
			     Age=mean(Age, na.rm=T), EduLevel=mean(eduLevel, na.rm=T),
			     Sex=(mean(Females, na.rm=T))/((mean(Females, na.rm=T)+ mean(Males, na.rm=T))),
			     White=mean(White,na.rm=T), Black=mean(Black, na.rm=T), Latin=mean(Latin, na.rm=T),
				Asian=mean(Asian, na.rm=T), Poli=mean(PoliScore, na.rm=T),
				ExpBias_SE=(sd(ExpBias, na.rm=T)/sqrt(Pop)), ExpBias=mean(ExpBias, na.rm=T),
				Exp.FC=mean(AssoFamily, na.rm=T), Exp.MW=mean(AssoCareer, na.rm=T),
				Count=length(FIPS),DScore=mean(dScore, na.rm=T),
			      DScore_SE=(sd(dScore, na.rm=T)/sqrt(Pop)) ) 

#replacements for those with N==1
DScore_SE_Rep=mean(Final$DScore_SE[Final$Pop==2], na.rm=T)
ExpBias_SE_Rep=mean(Final$ExpBias_SE[Final$Pop==2], na.rm=T)

#apply replacements
Final$DScore_SE[Final$Pop==1 | Final$DScore_SE==0 | is.na(Final$DScore_SE)]<-DScore_SE_Rep
Final$ExpBias_SE[Final$Pop==1| Final$ExpBias_SE==0 | is.na(Final$ExpBias_SE)]<-ExpBias_SE_Rep

#weights for Dscore and Exp_Bias
Final$DScore_wieght<-log(1/(Final$DScore_SE^2)) 
Final$ExpBias_wieght<-log(1/(Final$ExpBias_SE^2)) 

Final$weight<-((Final$DScore_wieght+Final$ExpBias_wieght)/2)

#____ Religion_________________________________________--#
rel_data<-read.csv("C:\\Users\\Phillip\\Google Drive\\Where Bais Against Females Berns You - A Study of Implicit Bias and Voting Data\\rel_data.csv", head=T)
rel_data<-as.data.frame(rel_data)

#get just the necessary variables
rel_data<-rel_data[,c("FIPS", "STNAME", "TOTRATE")]

#get the state Abb's
rel_data$Abb<-unlist(lapply(rel_data$STNAME, state_abb))

#get rid of zeros
rel_data$FIPS<-substring(as.character(rel_data$FIPS),nchar(as.character(rel_data$FIPS))-2)
rel_data$FIPS<-as.numeric(rel_data$FIPS)
rel_data$FIPS<-as.character(rel_data$FIPS)

rel_data$FIPS<-
	gsub(" ", "",(do.call(paste, as.data.frame(rel_data[,c(4,1)], stringsAsFactors=FALSE))), fixed=T)

rel_data<-rel_data[complete.cases(rel_data),]
#Turn it into z-scores
rel_data$TOTRATEZ<-(rel_data$TOTRATE-(mean(rel_data$TOTRATE, na.rm=T)))/sd(rel_data$TOTRATE, na.rm=T)

#load in data for the primary dates and match it up
days<-as.data.frame(read.csv('C:\\Users\\Phillip\\Google Drive\\Where Bais Against Females Berns You - A Study of Implicit Bias and Voting Data\\PrimaryDate.csv', head=T))
days$State<-as.character(days$State)

#fix day names 
days$State<-unlist(lapply(days$State,simpleCap))
days$State<-state.abb[match(days$State, state.name)]

#average the dates when republicans and dems 
days$avgNumDays<-ifelse((days$DemNumDays >0 & days$RepNumDays >0), 
				 (days$DemNumDays+days$RepNumDays)/2, ifelse(days$DemNumDays<0,days$RepNumDays,days$DemNumDays))  


#Modeled Parameters_______________________________________________________
quad_data<-read.csv("C:\\Users\\Phillip\\Google Drive\\Where Bais Against Females Berns You - A Study of Implicit Bias and Voting Data\\quads.csv")
quad_data<-as.data.frame(quad_data)
quad_data$FIPS<-as.character(quad_data$FIPS)


#Doing Stuff__________________________________________________________________________________________________________					
#Main Model data shaping and analysis

#this is the data for hillary versus bernie
#MainData<-votData[votData$Candidate=='H.Clinton' |votData$Candidate=='B.Sanders',-c(1)]

#Data for hillary versus trump, with adjustments for the fact that 
#some republican counties did not report the outcomes for primary 
#votes
MainData<-votData[votData$Candidate=='H.Clinton' |votData$Candidate=='D.Trump',-c(1)]
HillData<-votData[votData$Candidate=='H.Clinton',-c(1)] 
TrumData<-votData[votData$Candidate=='D.Trump',-c(1)]$FIPS
MainData<-MainData[MainData$FIPS %in% TrumData,-c(1,6)]


#Reshape to long
MainData<-w <- reshape(MainData, timevar = "Candidate",
  idvar = c("State", "Place", "FIPS", "Nums"),direction = "wide")
MainData$Prop.H<-MainData$Popular.H.Clinton/(MainData$Popular.H.Clinton+MainData$Popular.D.Trump)

MainData<-MainData[ , -which(names(MainData) %in% c("Nums"))]

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
MainData$Wieght<-Final$weight[mM]
MainData$ExpBias<-Final$ExpBias[mM]
MainData$Exp.FF<-Final$Exp.FC[mM]
MainData$Exp.MW<-Final$Exp.MW[mM]
#MainData$Nums<-as.numeric(MainData$Nums)

#Religion
rM<-match(MainData$FIPS, rel_data$FIPS)
MainData$Religous<-rel_data$TOTRATEZ[rM]

#primary dates
mDS<-match(MainData$State, days$State)
MainData$numDays<-days$avgNumDays[mDS]

qM<-match(MainData$FIPS, quad_data$FIPS)
#quad Modeled components
MainData$ACFF<-quad_data$ACFF[qM]
MainData$ACMC<-quad_data$ACMC[qM]

#Remove All Unmatched Counties
MainData<-MainData[!(is.na(MainData$CheckFIPS)),]

#Set cutoff and do statistics stuff
daters<-MainData

#get rid of NA rows 
daters<-daters[!(rowSums(is.na(daters)) > 0),]

#takes out all data with an AVERAGE date after may 1st
daters<-daters[daters$numDays<90,]

#is there a faster way to do this using matrix multiplication?
daters<-ddply(daters, c("FIPS", "Prop.H"), summarise, DScore=DScore*Wieght, Age=Age*Wieght,  
			Sex=Sex*Wieght, Asian=Asian*Wieght, Black=Black*Wieght, Latin=Latin*Wieght, 
			White=White*Wieght, EduLevel=EduLevel*Wieght, Income=Income*Wieght,
			Poli=Poli*Wieght, ExpBias=ExpBias*Wieght, Exp.FF=Exp.FF, Exp.MW=Exp.MW, 
			Religous=Religous*Wieght, ACFF=ACFF*Wieght, ACMC=ACMC*Wieght,
			 Wieght=Wieght, numDays=numDays)

corTests<-daters[,c(13,3,15, 14, 18, 17, 4, 10, 11, 5,6,7,8,9,12,16,20)]
#corTest needs to be a matrix
corrs<-rcorr(as.matrix(corTests))$r
ps<-rcorr(as.matrix(corTests))$P


#setwd for export 
setwd('C:\\Users\\Phillip\\Google Drive\\Where Bais Against Females Berns You - A Study of Implicit Bias and Voting Data\\CombinedBias')
MainModel<-lm(Prop.H~DScore
			+ExpBias
			+Age  #Avg age of county
			+Sex	# % of females
			+Asian #% of Asians
			+Black #% African American
			+Latin #%Latin American	
			+White #% White American
			+EduLevel #Avg Edu level 
			+Income # Avg Income
			+Poli	#Avg political standing
			+Religous
			+numDays,
			data=daters, na.action=na.omit)

summary(MainModel, correlation=F)

df<-tidy(MainModel)



#Bernie Data-------------------------------------------------------------------------------------------------
BMainData<-votData[votData$Candidate=='H.Clinton' |votData$Candidate=='B.Sanders',-c(1)]

#Reshape to long
BMainData<-w <- reshape(BMainData, timevar = "Candidate",
  idvar = c("State", "Place", "FIPS", "Nums"),direction = "wide")
BMainData$Prop.H<-BMainData$Popular.H.Clinton/(BMainData$Popular.H.Clinton+BMainData$Popular.B.Sanders)
BMainData<-BMainData[ , -which(names(BMainData) %in% c("Nums"))]

#Attach County IAT-Data
BmM<-match(BMainData$FIPS, Final$FIPS)
BMainData$Income<-Final$Income[BmM]
BMainData$DScore<-Final$DScore[BmM]
BMainData$Age<-Final$Age[BmM]
BMainData$EduLevel<-Final$EduLevel[BmM]
BMainData$Sex<-Final$Sex[BmM]
BMainData$White<-Final$White[BmM]
BMainData$Black<-Final$Black[BmM]
BMainData$Latin<-Final$Latin[BmM]
BMainData$Asian<-Final$Asian[BmM]
BMainData$Poli<-Final$Poli[BmM]
BMainData$AssoFamily<-Final$AssoFamily[BmM]
BMainData$AssoCareer<-Final$AssoCareer[BmM]
BMainData$CheckFIPS<-Final$FIPS[BmM]
BMainData$Count<-Final$Count[BmM]
BMainData$Wieght<-Final$weight[BmM]
BMainData$ExpBias<-Final$ExpBias[BmM]
BMainData$Exp.FF<-Final$Exp.FC[BmM]
BMainData$Exp.MW<-Final$Exp.MW[BmM]
#BMainData$Nums<-as.numeric(BMainData$Nums)

#Religion
BrM<-match(BMainData$FIPS, rel_data$FIPS)
BMainData$Religous<-rel_data$TOTRATEZ[BrM]

#primary dates
bDS<-match(BMainData$State, days$State)
BMainData$numDays<-days$DemNumDays[bDS]

BqM<-match(BMainData$FIPS, quad_data$FIPS)
#quad Modeled components
BMainData$ACFF<-quad_data$ACFF[BqM]
BMainData$ACMC<-quad_data$ACMC[BqM]


BMainData<-BMainData[!(is.na(BMainData$CheckFIPS)),]
Bdaters<-BMainData

#get rid of NA rows 
Bdaters<-Bdaters[!(rowSums(is.na(Bdaters)) > 0),]

#assign caucus or primary 
cauc<-c('AK', 'CO', 'HI', 'ID', 'KS', 'ME', 'MI', 'NE', 'NV', 'ND', 'UT', 'WA','WY')

Bdaters$Caucus<-ifelse((Bdaters$State %in% cauc), 1, 0)


#is there a faster way to do this using matrix multiplication?
Bdaters<-ddply(Bdaters, c("FIPS", "Prop.H"), summarise, DScore=DScore*Wieght, Age=Age*Wieght,  
			Sex=Sex*Wieght, Asian=Asian*Wieght, Black=Black*Wieght, Latin=Latin*Wieght, 
			White=White*Wieght, EduLevel=EduLevel*Wieght, Income=Income*Wieght,
			Poli=Poli*Wieght, ExpBias=ExpBias*Wieght, Exp.FF=Exp.FF, Exp.MW=Exp.MW, 
			Religous=Religous*Wieght, ACFF=ACFF*Wieght, ACMC=ACMC*Wieght,
			 Wieght=Wieght,numDays=numDays, Caucus=Caucus)

BcorTests<-Bdaters[,c(13,3,15, 14, 18, 17, 4, 10, 11, 5,6,7,8,9,12,16,20, 21)]
#corTest needs to be a matrix
Bcorrs<-rcorr(as.matrix(BcorTests))$r
Bps<-rcorr(as.matrix(BcorTests))$P


BMainModel<-lm(Prop.H~(DScore*Caucus)
			+(ExpBias*Caucus)
			+Age  #Avg age of county
			+Sex	# % of females
			+Asian #% of Asians
			+Black #% African American
			+Latin #%Latin American	
			+White #% White American
			+EduLevel #Avg Edu level 
			+Income # Avg Income
			+Poli	#Avg political standing
			+Religous#
			#+Caucus
			+numDays,
			data=Bdaters, na.action=na.omit)

summary(BMainModel, correlation=F)

Bdf<-tidy(BMainModel)


#Hillary vs. The Zodiac Killer__________________________________________________________________-

CMainData<-votData[votData$Candidate=='H.Clinton' |votData$Candidate=='T.Cruz',-c(1)]
CruzData<-votData[votData$Candidate=='T.Cruz',-c(1)]$FIPS
CMainData<-CMainData[CMainData$FIPS %in% CruzData,-c(1,6)]

#Reshape to long
CMainData<- reshape(CMainData, timevar = "Candidate",
  idvar = c("State", "Place", "FIPS", "Nums"),direction = "wide")
CMainData$Prop.H<-CMainData$Popular.H.Clinton/(CMainData$Popular.H.Clinton+CMainData$Popular.T.Cruz)

#Attach County IAT-Data
CmM<-match(CMainData$FIPS, Final$FIPS)
CrM<-match(CMainData$FIPS, rel_data$FIPS)

CMainData$Income<-Final$Income[CmM]
CMainData$DScore<-Final$DScore[CmM]
CMainData$Age<-Final$Age[CmM]
CMainData$EduLevel<-Final$EduLevel[CmM]
CMainData$Sex<-Final$Sex[CmM]
CMainData$White<-Final$White[CmM]
CMainData$Black<-Final$Black[CmM]
CMainData$Latin<-Final$Latin[CmM]
CMainData$Asian<-Final$Asian[CmM]
CMainData$Poli<-Final$Poli[CmM]
CMainData$AssoFamily<-Final$AssoFamily[CmM]
CMainData$AssoCareer<-Final$AssoCareer[CmM]
CMainData$CheckFIPS<-Final$FIPS[CmM]
CMainData$Count<-Final$Count[CmM]
CMainData$Nums<-as.numeric(CMainData$Nums)
CMainData$Wieght<-Final$weight[CmM]
CMainData$ExpBias<-Final$ExpBias[CmM]

#Religious Data
CMainData$Religous<-rel_data$TOTRATEZ[CrM]

#primary dates
cDS<-match(CMainData$State, days$State)
CMainData$numDays<-days$avgNumDays[cDS]

CqM<-match(CMainData$FIPS, quad_data$FIPS)
#quad Modeled components
CMainData$ACFF<-quad_data$ACFF[CqM]
CMainData$ACMC<-quad_data$ACMC[CqM]

#Remove All Unmatched Counties
CMainData<-CMainData[!(is.na(CMainData$CheckFIPS)),]

#Set cutoffs in data for model
Cdaters<-CMainData

#get rid of anythin with an NA
Cdaters<-Cdaters[!(rowSums(is.na(Cdaters)) > 0),]

#takes out all data with an AVERAGE date after may 1st
Cdaters<-Cdaters[Cdaters$numDays<90,]

Cdaters<-ddply(Cdaters, c("FIPS", "Prop.H"), summarise, DScore=DScore*Wieght, Age=Age*Wieght,  
			Sex=Sex*Wieght, Asian=Asian*Wieght, Black=Black*Wieght, Latin=Latin*Wieght, 
			White=White*Wieght, EduLevel=EduLevel*Wieght, Income=Income*Wieght,
			Poli=Poli*Wieght, ExpBias=ExpBias*Wieght,numDays=numDays,
			Religous=Religous*Wieght, ACFF=ACFF*Wieght, ACMC=ACMC*Wieght,
			 Wieght=Wieght)


CMainModel<-lm(Prop.H~DScore
			+ExpBias
			+Age  #Avg age of county
			+Sex	# % of females
			+Asian #% of Asians
			+Black #% African American
			+Latin #%Latin American	
			+White #% White American
			+EduLevel #Avg Edu level 
			+Income # Avg Income
			+Poli	#Avg political standing
			+Religous
			+numDays,
			data=Cdaters, na.action=na.omit)
summary(CMainModel)

Cdf<-tidy(CMainModel)




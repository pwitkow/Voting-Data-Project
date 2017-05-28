setwd("C:\\Users\\Phillip\\Google Drive\\Where Bais Against Females Berns You - A Study of Implicit Bias and Voting Data\\Data Files\\Voting Data")
#put whatever libraries here
library('plyr')
library('nlme')
library('ggplot2')
library('Hmisc')
library('broom')

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
votData[votData$State=="AK",]$Nums<-matchCodes[matchCodes$State=='AK',]$Nums[1]

#collapse Main to the state lvl so its commensurate with Repubs
maine_data<-votData[votData$State=="ME",]
maine_data$Nums<-as.numeric(maine_data$Nums)
maine_data<-ddply(maine_data, c("Candidate", "State", "Party"), summarise,
		Votes=mean(Votes, na.rm=T), Popular=mean(Popular, na.rm=T), FIPS=FIPS[1],
		X=X[1], Place=Place[1], Nums=Nums[1])

votData<-votData[!(votData$State=="ME"),]
votData<-rbind(votData, maine_data)

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






#Transfrom explicit measures into z-scores
iatDataframe$AssoCareer<-(iatDataframe$AssoCareer-mean(iatDataframe$AssoCareer, 
					na.rm=T))/sd(iatDataframe$AssoCareer, na.rm=T) 
iatDataframe$AssoFamily<-(iatDataframe$AssoFamily-mean(iatDataframe$AssoFamily, 
					na.rm=T))/sd(iatDataframe$AssoFamily, na.rm=T)

#Condense all variables by FIPS
Final<-ddply(iatDataframe, c("FIPS"), summarise, DScore=mean(dScore, na.rm=T),
					Count=length(FIPS), Pop=length(dScore), State=State[1],
				AssoFamily_SE=(sd(AssoFamily, na.rm=T)),AssoFamily=mean(AssoFamily, na.rm=T),
				AssoCareer_SE=(sd(AssoCareer, na.rm=T)), AssoCareer=mean(AssoCareer, na.rm=T),
				Count=length(FIPS),DScore=mean(dScore, na.rm=T),
			      DScore_SE=(sd(dScore, na.rm=T)/sqrt(Pop)) )

#replacements for those with N==1
DScore_SE_Rep=mean(Final$DScore_SE[Final$Pop==2], na.rm=T)
Career_SE_Rep=mean(Final$AssoCareer_SE[Final$Pop==2], na.rm=T)
Family_SE_Rep=mean(Final$AssoFamily_SE[Final$Pop==2], na.rm=T)

#apply replacements
Final$DScore_SE[Final$Pop==1]<-DScore_SE_Rep
Final$AssoCareer_SE[Final$Pop==1]<-Career_SE_Rep
Final$AssoFamily_SE[Final$Pop==1 | Final$AssoFamily_SE==0 | is.na(Final$AssoFamily_SE)]<-Family_SE_Rep
Final$AssoCareer_SE[Final$Pop==1 | Final$AssoCareer_SE==0 | is.na(Final$AssoCareer_SE)]<-Career_SE_Rep
Final$DScore_SE[Final$Pop==1 | Final$DScore_SE==0 | is.na(Final$DScore_SE)]<-DScore_SE_Rep

#weights for Dscore and Exp_Bias
Final$DScore_wieght<-log(1/(Final$DScore_SE^2)) 
Final$Family_wieght<-log(1/(Final$AssoFamily_SE^2)) 
Final$Career_wieght<-log(1/(Final$AssoFamily_SE^2))

Final$weight<-((Final$DScore_wieght+Final$Family_wieght+Final$Career_wieght)/3)




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
Pop_data$FIPS<-substring(as.character(Pop_data$FIPS),nchar(as.character(Pop_data$FIPS))-2)
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
poli_data$FIPS<-substring(
		as.character(poli_data$FIPS), nchar(as.character(poli_data$FIPS))-2)#remove first to elements of the string
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
rel_data$FIPS<-substring(as.character(rel_data$FIPS),nchar(as.character(rel_data$FIPS))-2)
rel_data$FIPS<-as.numeric(rel_data$FIPS)
rel_data$FIPS<-as.character(rel_data$FIPS)

rel_data$FIPS<-
	gsub(" ", "",(do.call(paste, as.data.frame(rel_data[,c(4,1)], stringsAsFactors=FALSE))), fixed=T)
rel_data<-rel_data[complete.cases(rel_data),]
#Turn it into z-scores
rel_data$TOTRATEZ<-(rel_data$TOTRATE-(mean(rel_data$TOTRATE, na.rm=T)))/sd(rel_data$TOTRATE, na.rm=T)

#load in data for the primary dates and match it up_____________________________
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
#HillData<-votData[votData$Candidate=='H.Clinton',-c(1)] 
TrumData<-votData[votData$Candidate=='D.Trump',-c(1)]$FIPS
MainData<-MainData[MainData$FIPS %in% TrumData,-c(1,6)]


#Reshape to long
MainData<-w <- reshape(MainData, timevar = "Candidate",
  idvar = c("State", "Place", "FIPS", "Nums"),direction = "wide")
MainData$Prop.H<-MainData$Popular.H.Clinton/(MainData$Popular.H.Clinton+MainData$Popular.D.Trump)

#Attach County IAT-Data
mM<-match(MainData$FIPS, Final$FIPS)
dM<-match(MainData$FIPS, Pop_data$FIPS)
pM<-match(MainData$FIPS, poli_data$FIPS)
rM<-match(MainData$FIPS, rel_data$FIPS)
mDS<-match(MainData$State, days$State)
qM<-match(MainData$FIPS, quad_data$FIPS)


#apply Dscore
MainData$DScore<-Final$DScore[mM]
MainData$AssoFamily<-Final$AssoFamily[mM]
MainData$AssoCareer<-Final$AssoCareer[mM]
MainData$CheckFIPS<-Final$FIPS[mM]
MainData$Count<-Final$Count[mM]
MainData$Wieght<-Final$weight[mM]

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

#primary dates
MainData$numDays<-days$avgNumDays[mDS]

#quad Modeled components
MainData$ACFF<-quad_data$ACFF[qM]
MainData$ACMC<-quad_data$ACMC[qM]

#Remove All Unmatched Counties (no response counties)
MainData<-MainData[!(is.na(MainData$CheckFIPS)),]
#Set cutoffs in data for model
daters<-MainData   #[MainData$Count>=20,]

#get rid of NA rows 
daters<-daters[!(rowSums(is.na(daters)) > 0),]

#takes out all data with an AVERAGE date after may 1st
#daters<-daters[daters$numDays<90,]

#is there a faster way to do this using matrix multiplication?
daters<-ddply(daters, c("FIPS", "Prop.H"), summarise, DScore=DScore*Wieght, Age=Age*Wieght,  
			Sex=Sex*Wieght, Asian=Asian*Wieght, Black=Black*Wieght, Latin=Latin*Wieght, 
			White=White*Wieght, EduLevel=EduLevel*Wieght, Income=Income*Wieght,
			Poli=Poli*Wieght,AssoCareer=AssoCareer*Wieght,AssoFamily=AssoFamily*Wieght, 
			Religous=Religous*Wieght, numDays=numDays, 
			ACFF=ACFF*Wieght, ACMC=ACMC*Wieght, #modeled params get weighted
			Wieght=Wieght)

setwd('C:\\Users\\Phillip\\Google Drive\\Where Bais Against Females Berns You - A Study of Implicit Bias and Voting Data\\Weighted+Quad_Regressions')


MainModel<-lm(Prop.H~DScore
			+ACFF
			+ACMC
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
			+Religous
			+numDays,
			data=daters, na.action=na.omit)
summary(MainModel, correlation=F)

ht<-tidy(MainModel)

dim(daters)



#BERNIE ANALYSIS-----------------------------------------------------

BMainData<-votData[votData$Candidate=='H.Clinton' |votData$Candidate=='B.Sanders',-c(1)]

#Reshape to long
BMainData<-w <- reshape(BMainData, timevar = "Candidate",
  idvar = c("State", "Place", "FIPS", "Nums"),direction = "wide")
BMainData$Prop.H<-BMainData$Popular.H.Clinton/(BMainData$Popular.H.Clinton+BMainData$Popular.B.Sanders)

#find the matching parts
BmM<-match(BMainData$FIPS, Final$FIPS)
BdM<-match(BMainData$FIPS, Pop_data$FIPS)
BpM<-match(BMainData$FIPS, poli_data$FIPS)
BrM<-match(BMainData$FIPS, rel_data$FIPS)
bDS<-match(BMainData$State, days$State)
BqM<-match(BMainData$FIPS, quad_data$FIPS)

#Attach County IAT-Data
BMainData$DScore<-Final$DScore[BmM]
BMainData$AssoFamily<-Final$AssoFamily[BmM]
BMainData$AssoCareer<-Final$AssoCareer[BmM]
BMainData$Count<-Final$Count[BmM]
BMainData$CheckFIPS<-Final$FIPS[BmM]
BMainData$Wieght<-Final$weight[BmM]

#demographic data
BMainData$White<-Pop_data$White[BdM]/Pop_data$Total[BdM]
BMainData$Black<-Pop_data$Black[BdM]/Pop_data$Total[BdM]
BMainData$Latin<-Pop_data$Latino[BdM]/Pop_data$Total[BdM]
BMainData$Asian<-Pop_data$Asian[BdM]/Pop_data$Total[BdM]
BMainData$Income<-Pop_data$HH.Income[BdM]
BMainData$EduLevel<-Pop_data$X.BachelorAbove[BdM]
BMainData$Age<-Pop_data$Median.Age[BdM]
BMainData$Sex<-Pop_data$Sex.ratio..males.per.100.females[BdM]/100

#higher means more Liberal
BMainData$Poli<-poli_data$Party[BpM]

#Religious Data
BMainData$Religous<-rel_data$TOTRATE[BrM]

#primary dates
BMainData$numDays<-days$DemNumDays[bDS]

#quad Modeled components
BMainData$ACFF<-quad_data$ACFF[BqM]
BMainData$ACMC<-quad_data$ACMC[BqM]


#Remove All Unmatched Counties

BMainData<-BMainData[!(is.na(BMainData$CheckFIPS)),]
Bdaters<-BMainData     #[BMainData$Count>=20,]

#get rid of NA rows 
Bdaters<-Bdaters[!(rowSums(is.na(Bdaters)) > 0),]

#label states with caucuses
#assign caucus or primary 
caucus<-c('AK', 'CO', 'HI', 'ID', 'KS', 'ME', 'MI', 'NE', 'NV', 'ND', 'UT', 'WA','WY')
Bdaters$Caucus<-ifelse(Bdaters$State %in% caucus, 1, 0)

Bdaters<-ddply(Bdaters, c("FIPS", "Prop.H"), summarise, DScore=DScore*Wieght, Age=Age*Wieght,  
			Sex=Sex*Wieght, Asian=Asian*Wieght, Black=Black*Wieght, Latin=Latin*Wieght, 
			White=White*Wieght, EduLevel=EduLevel*Wieght, Income=Income*Wieght,
			Poli=Poli*Wieght,AssoCareer=AssoCareer*Wieght,AssoFamily=AssoFamily*Wieght, 
			Religous=Religous*Wieght, Caucus=Caucus, Wieght=Wieght, numDays=numDays,
			ACFF=ACFF*Wieght, ACMC=ACMC*Wieght)

#make the model
BMainModel<-lm(Prop.H~DScore
			+ACFF
			+ACMC
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
			+Religous
			+Caucus
			+numDays,
			data=Bdaters, na.action=na.omit)
summary(BMainModel, correlation=F)

BB__tt<-tidy(BMainModel)


#_______________________________________________________________________________________________________________


#Hillary Vs. Ted Cruz analysis

CMainData<-votData[votData$Candidate=='H.Clinton' |votData$Candidate=='T.Cruz',-c(1)]
CruzData<-votData[votData$Candidate=='T.Cruz',-c(1)]$FIPS
CMainData<-CMainData[CMainData$FIPS %in% CruzData,-c(1,6)]

#Reshape to long
CMainData<-w <- reshape(CMainData, timevar = "Candidate",
  idvar = c("State", "Place", "FIPS", "Nums"),direction = "wide")
CMainData$Prop.H<-CMainData$Popular.H.Clinton/(CMainData$Popular.H.Clinton+CMainData$Popular.T.Cruz)

#Attach County IAT-Data
CmM<-match(CMainData$FIPS, Final$FIPS)
CdM<-match(CMainData$FIPS, Pop_data$FIPS)
CpM<-match(CMainData$FIPS, poli_data$FIPS)
CrM<-match(CMainData$FIPS, rel_data$FIPS)
cDS<-match(CMainData$State, days$State)
CqM<-match(CMainData$FIPS, quad_data$FIPS)

#apply Dscore
CMainData$DScore<-Final$DScore[CmM]
CMainData$AssoFamily<-Final$AssoFamily[CmM]
CMainData$AssoCareer<-Final$AssoCareer[CmM]
CMainData$CheckFIPS<-Final$FIPS[CmM]
CMainData$Count<-Final$Count[CmM]
CMainData$Wieght<-Final$weight[CmM]

#demographic data
CMainData$White<-Pop_data$White[CdM]/Pop_data$Total[CdM]
CMainData$Black<-Pop_data$Black[CdM]/Pop_data$Total[CdM]
CMainData$Latin<-Pop_data$Latino[CdM]/Pop_data$Total[CdM]
CMainData$Asian<-Pop_data$Asian[CdM]/Pop_data$Total[CdM]
CMainData$Income<-Pop_data$HH.Income[CdM]
CMainData$EduLevel<-Pop_data$X.BachelorAbove[CdM]
CMainData$Age<-Pop_data$Median.Age[CdM]
CMainData$Sex<-Pop_data$Sex.ratio..males.per.100.females[CdM]/100

#higher means more Liberal
CMainData$Poli<-poli_data$Party[CpM]

#Religious Data
CMainData$Religous<-rel_data$TOTRATEZ[CrM]

#election data
CMainData$numDays<-days$avgNumDays[cDS]

#quad Modeled components
CMainData$ACFF<-quad_data$ACFF[CqM]
CMainData$ACMC<-quad_data$ACMC[CqM]

#Remove All Unmatched Counties

CMainData<-CMainData[!(is.na(CMainData$CheckFIPS)),]
#Set cutoffs in data for model
Cdaters<-CMainData     #[CMainData$Count>=20,]

#get rid of NA rows 
Cdaters<-Cdaters[!(rowSums(is.na(Cdaters)) > 0),]

Cdaters<-Cdaters[Cdaters$numDays<90,]

Cdaters<-ddply(Cdaters, c("FIPS", "Prop.H"), summarise, DScore=DScore*Wieght, Age=Age*Wieght,  
			Sex=Sex*Wieght, Asian=Asian*Wieght, Black=Black*Wieght, Latin=Latin*Wieght, 
			White=White*Wieght, EduLevel=EduLevel*Wieght, Income=Income*Wieght,
			Poli=Poli*Wieght,AssoCareer=AssoCareer*Wieght,AssoFamily=AssoFamily*Wieght, 
			Religous=Religous*Wieght, Wieght=Wieght, numDays=numDays,
			ACFF=ACFF*Wieght, ACMC=ACMC*Wieght)

CMainModel<-lm(Prop.H~DScore
			+ACFF
			+ACMC
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
			+Religous
			+numDays,
			data=Cdaters, na.action=na.omit)
summary(CMainModel, correlation=F)


XX__tt<-tidy(CMainModel)






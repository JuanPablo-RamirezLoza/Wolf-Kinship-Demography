
# kinship relationships at equilibrium


# Packages used to build the sub-models
library(NetLogoR)
library(testthat)
library(kinship2)
library(SciViews)
library(DescTools)
load(file="popSim.Rdata")
popSim2 <- popSim ## CHANGED AGAIN FINAL
popSim3 <- popSim ## another test
popSim4 <- popSim # anotherone 4
popSim5 <- popSim # anotherone 5


# all indiv ever lived in the pop

# calculate relatedness for all indiv alive at the end, and for females alive only
#withParentID <- NLwith(agents = popSim[[(nYearSim + 1)]], var = "motherID", val = 0:1000000) # extract individuals with parent ID
#FemwithParentID <- NLwith(agents = withParentID, var = "sex", val = "F") # females with parent ID
#allRelatedness <- relatedness(listAllInd = popSim, whoInd = of(agents = withParentID, var = "who"))
#FemRelatedness <- relatedness(listAllInd = popSim, whoInd = of(agents = FemwithParentID, var = "who"))

#dim(allRelatedness)
#dim(FemRelatedness)


##############################################################################
#  Kinship matrix for mothers ################################################
##############################################################################

# For individuals ever alive in the pop (dead or not at last time step)
dataall  <- unique(do.call(rbind, popSim)  ) # all indiv ever lived in population
allwithparents <- dataall[!is.na(dataall$motherID),] #se data for individuals with known parents only
#### What does the comma do? 
#### comma => take all columns (index of lines to keep before comma, index of columns after comma). Here we want to keep all columns, just the line without NA for motherID

# for now, keep females only, and female-related kin only
AllFem <- allwithparents[allwithparents$sex=="F",]  # all females with known mother

# ID of all mothers and grandmothers ever lived in pop
IDMothers <- unique(AllFem[,'motherID'] ) # mothers of at least one female
IDGrandMothers <- unique( AllFem[which(AllFem$who %in% IDMothers),'motherID'] )# grand mother of at least one female

# sisters (same mother ID)
siblings <- tapply(X=AllFem$who,INDEX=AllFem$motherID) # associate per brood based on motherID
sistersdup <- split(x=AllFem$who,f=siblings) # list per brood, with duplicates
sisters <- lapply(sistersdup,unique) # list per brood without duplicates

# For individuals alive at last time step
datalast <- as.matrix(popSim[[26]])
datalastF <- datalast[ (datalast$sex=="F" & !is.na(datalast$motherID) ) ,] # keep only females with known mother 

#ego IDs
datalastF$who # ID of females alive with known mothers the last time step
nego <- nrow(datalastF) # 90 here

# Mothers alive at last time step
IDMothersAlive <- datalastF$who[datalastF$who %in% IDMothers]
# Ages of mothers alive at last time step
AgeMothersAlive <- datalastF$age[datalastF$who %in% IDMothers]

# Grand Mothers alive at last time step
IDGrandMothersAlive <- datalastF$who[datalastF$who %in% IDGrandMothers]
# Age of Grand Mothers alive at last time step
AgeGrandMothersAlive <- datalastF$age[datalastF$who %in% IDGrandMothers]

# sisters at last time step
siblingsAlive <- tapply(X=datalastF$who,INDEX=datalastF$motherID) # all females arranged in a list in same component with their sisters
sistersdupAlive <- split(x=datalastF$who,f=siblingsAlive)
sistersAlive <- lapply(sistersdupAlive,unique)

# check this is correct
# for sisters #1940 and  #2783 should be 12 and 7 yo
datalastF[datalastF$who=='1940',]
datalastF[datalastF$who=='2783',]


# LOOP FOR PR MOTHER ALIVE
nbMAalive <- kinship_MA <- matrix(NA, nrow=max(ages),ncol=max(ages))
nbGMAalive <- kinship_GMA <- matrix(NA, nrow=max(ages),ncol=max(ages))

n_ego_agei <- n_MA <- n_GMA <-Pr_MA_egoagei <- Pr_GMA_egoagei <-NULL
mums <- list()
for(i in ages){  #loop over ego age
  ego_agei <- datalastF[datalastF$age==i,] # ego age i alive at last time step
  n_ego_agei[i] <-nrow(ego_agei) # nb of ego age i
  if(n_ego_agei[i]==0){
    nbMAalive[i,] <- rep(NA,length(ages))
  }else if(n_ego_agei[i]>0){ 
    
    # for mothers
    Mothers.ID <- ego_agei$motherID # mothers ID of ego age i
    Mothers.Alive.ID <- ego_agei$motherID[which(Mothers.ID %in% IDMothersAlive)] # alive mothers ID of ego age i
    mums[[i]] <- Mothers.Alive.ID
    n_MA[i] <- length(Mothers.Alive.ID) # nb of ego age i with mother alive at last time step
    Pr_MA_egoagei[i]  <- n_MA[i]/ n_ego_agei[i]  # proba mother alive for ego age i
    
    # for grand mothers
    GM.ID <- AllFem$motherID[match(Mothers.ID, AllFem$who )] # check the ID of the Grand Mother
    GM.Alive.ID <- GM.ID[GM.ID %in% IDGrandMothersAlive]
    n_GMA[i] <- length(GM.Alive.ID) # nb of ego age i with mother alive at last time step
    Pr_GMA_egoagei[i]  <- n_GMA[i]/ n_ego_agei[i]  # proba mother alive for ego age i
    
    # age of mother AND GRAND MOTHER if alive at last time step
    Mage <- datalastF$age[match(Mothers.Alive.ID, datalastF$who )] # match ID of mothers alive at last time step in datalast (which includes all females alive at last time step)
    GMage <- datalastF$age[match(GM.Alive.ID, datalastF$who )] # match ID of mothers alive at last time step in datalast (which includes all females alive at last time step)
    
    for(j in ages){ # loop over mothers ages
      # FOR MOTHERS
      nbMAalive[i,j] <- sum(Mage==j) 
      kinship_MA[i,j] <- sum(Mage==j) / n_ego_agei[i] # kinship matrix Pr of mother alive
      # FOR GRAND MOTHERS
      nbGMAalive[i,j] <- sum(GMage==j) 
      kinship_GMA[i,j] <- sum(GMage==j) / n_ego_agei[i] # kinship matrix Pr of mother alive
      
    } # end loop over mother age
  } # end if loop 
} # end loop over ego age

Res_summary <- data.frame(ego.age=ages,
                          nb.ego=n_ego_agei,  # number of ego aged i at last time step
                          nb.ego.MA=n_MA,  # number of ego aged i with mother alive at last time step
                          Pr_MA =Pr_MA_egoagei, # proba mother alive for ego age i
                          nb.ego.GMA=n_GMA,  # number of ego aged i with mother alive at last time step
                          Pr_GMA =Pr_GMA_egoagei) # proba mother alive for ego age i

round(Res_summary,2) # summary per ego age

kinship_MA # kinship matrix for mothers
kinship_GMA # kinship matrix for grand mothers

####### LOOPS


###############################################################################
## KINSHIP MATRIX FOR SISTERS #################################################
###############################################################################

Egoage1 <- dataF[dataF$age==1,]
Egoage2 <- dataF[dataF$age==2,]
Egoage3 <- dataF[dataF$age==3,]
Egoage4 <- dataF[dataF$age==4,]
Egoage5 <- dataF[dataF$age==5,]
Egoage6 <- dataF[dataF$age==6,]
Egoage7 <- dataF[dataF$age==7,]
Egoage8 <- dataF[dataF$age==8,]
Egoage9 <- dataF[dataF$age==9,]
Egoage10 <- dataF[dataF$age==10,]
Egoage11 <- dataF[dataF$age==11,]
Egoage12 <- dataF[dataF$age==12,]
Egoage13 <- dataF[dataF$age==13,]
Egoage14 <- dataF[dataF$age==14,]
mothersforage1<-c(Egoage1$motherID)
mothersforage2<-c(Egoage2$motherID)
mothersforage3<-c(Egoage3$motherID)
mothersforage4<-c(Egoage4$motherID)
mothersforage5<-c(Egoage5$motherID)
mothersforage6<-c(Egoage6$motherID)
mothersforage7<-c(Egoage7$motherID)
mothersforage8<-c(Egoage8$motherID)
mothersforage9<-c(Egoage9$motherID)
mothersforage10<-c(Egoage10$motherID)
mothersforage11<-c(Egoage11$motherID)
mothersforage12<-c(Egoage12$motherID)
mothersforage13<-c(Egoage13$motherID)
mothersforage14<-c(Egoage14$motherID)

# EGO AGE CLASS 1

#Same cohort sisters (age 1)
siblingsAlive1 <- tapply(X=Egoage1$who,INDEX=Egoage1$motherID)
sistersdupAlive1 <- split(x=Egoage1$who,f=siblingsAlive1)
sistersAlive1 <- lapply(sistersdupAlive1,unique) 
avg.sisters1<-sum(lengths(sistersAlive1)*(lengths(sistersAlive1)-1))/sum(lengths(sistersAlive1))

#Average number of sisters in every other age class for an individual of age 1
avg.sisters2<-sum(table(mothersforage1[mothersforage1 %in% mothersforage2]))/nrow(Egoage1)
avg.sisters3<-sum(table(mothersforage1[mothersforage1 %in% mothersforage3]))/nrow(Egoage1)
avg.sisters4<-sum(table(mothersforage1[mothersforage1 %in% mothersforage4]))/nrow(Egoage1)
avg.sisters5<-sum(table(mothersforage1[mothersforage1 %in% mothersforage5]))/nrow(Egoage1)
avg.sisters6<-sum(table(mothersforage1[mothersforage1 %in% mothersforage6]))/nrow(Egoage1)
avg.sisters7<-sum(table(mothersforage1[mothersforage1 %in% mothersforage7]))/nrow(Egoage1)
avg.sisters8<-sum(table(mothersforage1[mothersforage1 %in% mothersforage8]))/nrow(Egoage1)
avg.sisters9<-sum(table(mothersforage1[mothersforage1 %in% mothersforage9]))/nrow(Egoage1)
avg.sisters10<-sum(table(mothersforage1[mothersforage1 %in% mothersforage10]))/nrow(Egoage1)
avg.sisters11<-sum(table(mothersforage1[mothersforage1 %in% mothersforage11]))/nrow(Egoage1)
avg.sisters12<-sum(table(mothersforage1[mothersforage1 %in% mothersforage12]))/nrow(Egoage1)
avg.sisters13<-sum(table(mothersforage1[mothersforage1 %in% mothersforage13]))/nrow(Egoage1)
avg.sisters14<-sum(table(mothersforage1[mothersforage1 %in% mothersforage14]))/nrow(Egoage1)

sistervector1<-c(avg.sisters1,avg.sisters2,avg.sisters3,avg.sisters4,avg.sisters5,avg.sisters6,avg.sisters7,avg.sisters8,
                 avg.sisters9,avg.sisters10,avg.sisters11,avg.sisters12,avg.sisters13,avg.sisters14)

sum(sistervector1) 



# EGO AGE CLASS 2
###################### 

#Same cohort sisters (age 2)
siblingsAlive2 <- tapply(X=Egoage2$who,INDEX=Egoage2$motherID)
sistersdupAlive2 <- split(x=Egoage2$who,f=siblingsAlive2)
sistersAlive2 <- lapply(sistersdupAlive2,unique) 
avg.sisters2.2<-sum(lengths(sistersAlive2)*(lengths(sistersAlive2)-1))/sum(lengths(sistersAlive2))

#Average number of sisters in every other age class for an individual of age 2
avg.sisters1.2<-sum(table(mothersforage2[mothersforage2 %in% mothersforage1]))/nrow(Egoage2)

avg.sisters3.2<-sum(table(mothersforage2[mothersforage2 %in% mothersforage3]))/nrow(Egoage2)
avg.sisters4.2<-sum(table(mothersforage2[mothersforage2 %in% mothersforage4]))/nrow(Egoage2)
avg.sisters5.2<-sum(table(mothersforage2[mothersforage2 %in% mothersforage5]))/nrow(Egoage2)
avg.sisters6.2<-sum(table(mothersforage2[mothersforage2 %in% mothersforage6]))/nrow(Egoage2)
avg.sisters7.2<-sum(table(mothersforage2[mothersforage2 %in% mothersforage7]))/nrow(Egoage2)
avg.sisters8.2<-sum(table(mothersforage2[mothersforage2 %in% mothersforage8]))/nrow(Egoage2)
avg.sisters9.2<-sum(table(mothersforage2[mothersforage2 %in% mothersforage9]))/nrow(Egoage2)
avg.sisters10.2<-sum(table(mothersforage2[mothersforage2 %in% mothersforage10]))/nrow(Egoage2)
avg.sisters11.2<-sum(table(mothersforage2[mothersforage2 %in% mothersforage11]))/nrow(Egoage2)
avg.sisters12.2<-sum(table(mothersforage2[mothersforage2 %in% mothersforage12]))/nrow(Egoage2)
avg.sisters13.2<-sum(table(mothersforage2[mothersforage2 %in% mothersforage13]))/nrow(Egoage2)
avg.sisters14.2<-sum(table(mothersforage2[mothersforage2 %in% mothersforage14]))/nrow(Egoage2)

sistervector2<-c(avg.sisters1.2,avg.sisters2.2,avg.sisters3.2,avg.sisters4.2,avg.sisters5.2,avg.sisters6.2,
                 avg.sisters7.2,avg.sisters8.2,avg.sisters9.2,avg.sisters10.2,avg.sisters11.2,avg.sisters12.2,
                 avg.sisters13.2,avg.sisters14.2)

sum(sistervector2) 


# EGO AGE CLASS 3
###################### 
#Same cohort sisters (age 3)
siblingsAlive3 <- tapply(X=Egoage3$who,INDEX=Egoage3$motherID)
sistersdupAlive3 <- split(x=Egoage3$who,f=siblingsAlive3)
sistersAlive3 <- lapply(sistersdupAlive3,unique) 
avg.sisters3.3<-sum(lengths(sistersAlive3)*(lengths(sistersAlive3)-1))/sum(lengths(sistersAlive3))

#Average number of sisters in every other age class for an individual of age 1
avg.sisters1.3<-sum(table(mothersforage3[mothersforage3 %in% mothersforage1]))/nrow(Egoage3)
avg.sisters2.3<-sum(table(mothersforage3[mothersforage3 %in% mothersforage3]))/nrow(Egoage3)

avg.sisters4.3<-sum(table(mothersforage3[mothersforage3 %in% mothersforage4]))/nrow(Egoage3)
avg.sisters5.3<-sum(table(mothersforage3[mothersforage3 %in% mothersforage5]))/nrow(Egoage3)
avg.sisters6.3<-sum(table(mothersforage3[mothersforage3 %in% mothersforage6]))/nrow(Egoage3)
avg.sisters7.3<-sum(table(mothersforage3[mothersforage3 %in% mothersforage7]))/nrow(Egoage3)
avg.sisters8.3<-sum(table(mothersforage3[mothersforage3 %in% mothersforage8]))/nrow(Egoage3)
avg.sisters9.3<-sum(table(mothersforage3[mothersforage3 %in% mothersforage9]))/nrow(Egoage3)
avg.sisters10.3<-sum(table(mothersforage3[mothersforage3 %in% mothersforage10]))/nrow(Egoage3)
avg.sisters11.3<-sum(table(mothersforage3[mothersforage3 %in% mothersforage11]))/nrow(Egoage3)
avg.sisters12.3<-sum(table(mothersforage3[mothersforage3 %in% mothersforage12]))/nrow(Egoage3)
avg.sisters13.3<-sum(table(mothersforage3[mothersforage3 %in% mothersforage13]))/nrow(Egoage3)
avg.sisters14.3<-sum(table(mothersforage3[mothersforage3 %in% mothersforage14]))/nrow(Egoage3)

sistervector3<-c(avg.sisters1.3,avg.sisters2.3,avg.sisters3.3,avg.sisters4.3,avg.sisters5.3,avg.sisters6.3,
                 avg.sisters7.3,avg.sisters8.3,avg.sisters9.3,avg.sisters10.3,avg.sisters11.3,avg.sisters12.3,
                 avg.sisters13.3,avg.sisters14.3)

sum(sistervector3) 

#########################################################################################
###   KINSHIP MATRIX FOR CHILDREN   #####################################################
#########################################################################################

#EGO AGE 4 AS EXAMPLE

Egoage4 <- dataF[dataF$age==4,]
negoage4 <-nrow(Egoage4)
Age4Mothers <- Egoage4[which(Egoage4$who %in% dataF$motherID),]

Age4MothersOffspring <- dataF[which(dataF$motherID %in% Age4Mothers$who),]

Offspring4.1 <- Age4MothersOffspring[which(Age4MothersOffspring$age==1),]
Offspring4.2 <- Age4MothersOffspring[which(Age4MothersOffspring$age==2),]
Offspring4.3 <- Age4MothersOffspring[which(Age4MothersOffspring$age==3),]
Offspring4.4 <- Age4MothersOffspring[which(Age4MothersOffspring$age==4),]
Offspring4.5 <- Age4MothersOffspring[which(Age4MothersOffspring$age==5),]
Offspring4.6 <- Age4MothersOffspring[which(Age4MothersOffspring$age==6),]
Offspring4.7 <- Age4MothersOffspring[which(Age4MothersOffspring$age==7),]
Offspring4.8 <- Age4MothersOffspring[which(Age4MothersOffspring$age==8),]
Offspring4.9 <- Age4MothersOffspring[which(Age4MothersOffspring$age==9),]
Offspring4.10 <- Age4MothersOffspring[which(Age4MothersOffspring$age==10),]
Offspring4.11 <- Age4MothersOffspring[which(Age4MothersOffspring$age==11),]
Offspring4.12 <- Age4MothersOffspring[which(Age4MothersOffspring$age==12),]
Offspring4.13 <- Age4MothersOffspring[which(Age4MothersOffspring$age==13),]
Offspring4.14 <- Age4MothersOffspring[which(Age4MothersOffspring$age==14),]
nOffspring4.1 <- nrow(Offspring4.1)
nOffspring4.2 <- nrow(Offspring4.2)
nOffspring4.3 <- nrow(Offspring4.3)
nOffspring4.4 <- nrow(Offspring4.4)
nOffspring4.5 <- nrow(Offspring4.5)
nOffspring4.6 <- nrow(Offspring4.6)
nOffspring4.7 <- nrow(Offspring4.7)
nOffspring4.8 <- nrow(Offspring4.8)
nOffspring4.9 <- nrow(Offspring4.9)
nOffspring4.10 <- nrow(Offspring4.10)
nOffspring4.11 <- nrow(Offspring4.11)
nOffspring4.12 <- nrow(Offspring4.12)
nOffspring4.13 <- nrow(Offspring4.13)
nOffspring4.14 <- nrow(Offspring4.14)

offspringvector4<-c(nOffspring4.1,nOffspring4.2,nOffspring4.3,nOffspring4.4,nOffspring4.5,nOffspring4.6,nOffspring4.7,nOffspring4.8,
                    nOffspring4.9,nOffspring4.10,nOffspring4.11,nOffspring4.12,nOffspring4.13,nOffspring4.14)
avgoffspring4 <- offspringvector4/negoage4
avgoffspring4
sum(avgoffspring4)


#########################################################################################
###   KINSHIP MATRIX FOR GRANDCHILDREN    ###############################################
#########################################################################################
Egoage14<-dataF[dataF$age==14]
negoage14 <-nrow(Egoage14)
IDdaughtersDeadOrAlive<-unique(dataall[which(dataall$motherID %in% Egoage14$who),'who'])
AliveGrandChildren <- dataF[which(dataF$motherID %in% IDdaughtersDeadOrAlive)]

nGCAge14.1 <-nrow(AliveGrandChildren[which(AliveGrandChildren$age==1),])
nGCAge14.2 <-nrow(AliveGrandChildren[which(AliveGrandChildren$age==2),])
nGCAge14.3 <-nrow(AliveGrandChildren[which(AliveGrandChildren$age==3),])
nGCAge14.4 <-nrow(AliveGrandChildren[which(AliveGrandChildren$age==4),])
nGCAge14.5 <-nrow(AliveGrandChildren[which(AliveGrandChildren$age==5),])
nGCAge14.6 <-nrow(AliveGrandChildren[which(AliveGrandChildren$age==6),])
nGCAge14.7 <-nrow(AliveGrandChildren[which(AliveGrandChildren$age==7),])
nGCAge14.8 <-nrow(AliveGrandChildren[which(AliveGrandChildren$age==8),])
nGCAge14.9 <-nrow(AliveGrandChildren[which(AliveGrandChildren$age==9),])
nGCAge14.10 <-nrow(AliveGrandChildren[which(AliveGrandChildren$age==10),])
nGCAge14.11 <-nrow(AliveGrandChildren[which(AliveGrandChildren$age==11),])
nGCAge14.12 <-nrow(AliveGrandChildren[which(AliveGrandChildren$age==12),])
nGCAge14.13 <-nrow(AliveGrandChildren[which(AliveGrandChildren$age==13),])
nGCAge14.14 <-nrow(AliveGrandChildren[which(AliveGrandChildren$age==14),])

GCAge14vector <- c(nGCAge14.1,nGCAge14.2,nGCAge14.3,nGCAge14.4,nGCAge14.5,nGCAge14.6,nGCAge14.7,nGCAge14.8,nGCAge14.9,
                   nGCAge14.10,nGCAge14.11,nGCAge14.12,nGCAge14.13,nGCAge14.14)
AvgGC14<- GCAge14vector/negoage14
AvgGC14

#########################################################################################
###   KINSHIP MATRIX FOR AUNTS    #######################################################
#########################################################################################
Egoage1 <- dataF[dataF$age==1,] 
negoage1 <-nrow(Egoage1) 
Egoage1.MID <- Egoage1[!is.na(Egoage1$motherID),] 
IDMothersDeadOrAlive <- unique(dataall[which(dataall$who %in% Egoage1.MID$motherID),'who'])
IDGMDeadOrAlive <- unique(dataall[which(dataall$who %in% IDMothersDeadOrAlive),'motherID'])


AllAunts <- dataF[which(dataF$motherID %in% IDGMDeadOrAlive)] #All alive daughters of ego's grandmother, including her mother

#Split Aunts by their mother ID
GroupAunts <- tapply(X=AllAunts$who,INDEX=AllAunts$motherID)
AuntsGrouped <- split(x=AllAunts$who,f=GroupAunts)
AuntsGroupedF <- lapply(AuntsGrouped,unique) 


#After we group AllAunts$who by their mother ID, to get the number of aunts we need to subtract 1 from each group of the list
# EXCEPT if the group includes more than one mother, in which case they are all aunts.  

#They have to be mothers to an alive individual of the focal age class, and they have to be alive themselves. 
MothersAlive.egoage1 <- dataF[which(dataF$who %in% Egoage1$motherID),]

#Tried with the loop below but couldn't get it to work. 


n <- c(rep(NA,length(AuntsGroupedF)))

for (i in 1:length(AuntsGroupedF)){
  if (length(which(MothersAlive.egoage1$who %in% AuntsGroupedF[[i]]) > 1)) { 
    n[i] <- length(AuntsGroupedF[[1]])
  } else if (length(IDMothersDeadOrAlive[[which(IDMothersDeadOrAlive %in% AuntsGroupedF[[i]])]]) == 1){
    n[i] <- length(AuntsGroupedF[[1]])-1
  }
}

# This loop would only give us the expected number of aunts for individuals of each class. 
# We would still need to find a way to split the expected number by age of the aunts too. 
# This is tricky because after splitting them by mother ID we are left only with the ID of the aunts, 
# but we don't keep their age. 

# If we split them by age first, and then by mother ID, for each individual in a group of sisters of a given age, 
# we also need to check if their sisters of other ages have had children. 
# I haven't figured out how to do this, since the split function 
# gives the groups new numbers starting from one, it doesn't keep track of mother ID. 

#Split by age
Aunts1 <-AllAunts[AllAunts$age==1,]
Aunts2 <-AllAunts[AllAunts$age==2,]
Aunts3 <-AllAunts[AllAunts$age==3,]
Aunts4 <-AllAunts[AllAunts$age==4,]
Aunts5 <-AllAunts[AllAunts$age==5,]
Aunts6 <-AllAunts[AllAunts$age==6,]
Aunts7 <-AllAunts[AllAunts$age==7,]
Aunts8 <-AllAunts[AllAunts$age==8,]
Aunts9 <-AllAunts[AllAunts$age==9,]
Aunts10 <-AllAunts[AllAunts$age==10,]
Aunts11 <-AllAunts[AllAunts$age==11,]
Aunts12 <-AllAunts[AllAunts$age==12,]
Aunts13 <-AllAunts[AllAunts$age==13,]
Aunts14 <-AllAunts[AllAunts$age==14,]

#Split Aunts by their age and mother ID
GroupAunts1 <- tapply(X=Aunts1$who,INDEX=Aunts1$motherID)
AuntsGrouped1 <- split(x=Aunts1$who,f=GroupAunts1)
AuntsGroupedF1 <- lapply(AuntsGrouped1,unique) 

GroupAunts2 <- tapply(X=Aunts2$who,INDEX=Aunts2$motherID)
AuntsGrouped2 <- split(x=Aunts2$who,f=GroupAunts2)
AuntsGroupedF2 <- lapply(AuntsGrouped2,unique) 

GroupAunts3 <- tapply(X=Aunts3$who,INDEX=Aunts3$motherID)
AuntsGrouped3 <- split(x=Aunts3$who,f=GroupAunts3)
AuntsGroupedF3 <- lapply(AuntsGrouped3,unique) 

GroupAunts4 <- tapply(X=Aunts4$who,INDEX=Aunts4$motherID)
AuntsGrouped4 <- split(x=Aunts4$who,f=GroupAunts4)
AuntsGroupedF4 <- lapply(AuntsGrouped4,unique) 

GroupAunts5 <- tapply(X=Aunts5$who,INDEX=Aunts5$motherID)
AuntsGrouped5 <- split(x=Aunts5$who,f=GroupAunts5)
AuntsGroupedF5 <- lapply(AuntsGrouped5,unique) 

GroupAunts6 <- tapply(X=Aunts6$who,INDEX=Aunts6$motherID)
AuntsGrouped6 <- split(x=Aunts6$who,f=GroupAunts6)
AuntsGroupedF6 <- lapply(AuntsGrouped6,unique) 

GroupAunts7 <- tapply(X=Aunts7$who,INDEX=Aunts7$motherID)
AuntsGrouped7 <- split(x=Aunts7$who,f=GroupAunts7)
AuntsGroupedF7 <- lapply(AuntsGrouped7,unique) 

GroupAunts8 <- tapply(X=Aunts8$who,INDEX=Aunts8$motherID)
AuntsGrouped8 <- split(x=Aunts8$who,f=GroupAunts8)
AuntsGroupedF8 <- lapply(AuntsGrouped8,unique) 

GroupAunts9 <- tapply(X=Aunts9$who,INDEX=Aunts9$motherID)
AuntsGrouped9 <- split(x=Aunts9$who,f=GroupAunts9)
AuntsGroupedF9 <- lapply(AuntsGrouped9,unique) 

GroupAunts10 <- tapply(X=Aunts10$who,INDEX=Aunts10$motherID)
AuntsGrouped10 <- split(x=Aunts10$who,f=GroupAunts10)
AuntsGroupedF10 <- lapply(AuntsGrouped10,unique) 

GroupAunts11 <- tapply(X=Aunts11$who,INDEX=Aunts11$motherID)
AuntsGrouped11 <- split(x=Aunts11$who,f=GroupAunts11)
AuntsGroupedF11 <- lapply(AuntsGrouped11,unique) 

GroupAunts12 <- tapply(X=Aunts12$who,INDEX=Aunts12$motherID)
AuntsGrouped12 <- split(x=Aunts12$who,f=GroupAunts12)
AuntsGroupedF12 <- lapply(AuntsGrouped12,unique) 

GroupAunts13 <- tapply(X=Aunts13$who,INDEX=Aunts13$motherID)
AuntsGrouped13 <- split(x=Aunts13$who,f=GroupAunts13)
AuntsGroupedF13 <- lapply(AuntsGrouped13,unique) 

GroupAunts14 <- tapply(X=Aunts14$who,INDEX=Aunts14$motherID)
AuntsGrouped14 <- split(x=Aunts14$who,f=GroupAunts14)
AuntsGroupedF14 <- lapply(AuntsGrouped14,unique) 

mothersAunts1<-c(Aunts1$motherID)
mothersAunts2<-c(Aunts2$motherID)
mothersAunts3<-c(Aunts3$motherID)
mothersAunts4<-c(Aunts4$motherID)
mothersAunts5<-c(Aunts5$motherID)
mothersAunts6<-c(Aunts6$motherID)
mothersAunts7<-c(Aunts7$motherID)
mothersAunts8<-c(Aunts8$motherID)
mothersAunts9<-c(Aunts9$motherID)
mothersAunts10<-c(Aunts10$motherID)
mothersAunts11<-c(Aunts11$motherID)
mothersAunts12<-c(Aunts12$motherID)
mothersAunts13<-c(Aunts13$motherID)
mothersAunts14<-c(Aunts14$motherID)


#########################################################################################
###   KINSHIP MATRIX FOR COUSINS    #####################################################
#########################################################################################







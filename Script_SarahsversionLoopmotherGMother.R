
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
<<<<<<< HEAD
popSim4 <- popSim # anotherone 4
popSim6 <- popSim # anotherone 6
=======
>>>>>>> 96e81b034d25ca15fc30f18cd600e1cfdb7f895c

#popSim

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

popSim3 <- popSim

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

table(datalastF$age)
ages<- 1:11

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
    nbGMAalive[i,] <-rep(NA,length(ages))
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

##############################################################################################
###           LOOP FOR SISTERS                                                            ####
##############################################################################################

AvgSisAlive <- matrix(NA, nrow=max(ages),ncol=max(ages))

n_ego_agei <-NULL

for(i in ages){  #loop over ego age
  ego_agei <- datalastF[datalastF$age==i,] # ego age i alive at last time step
  n_ego_agei[i] <-nrow(ego_agei) # nb of ego age i
  
  if(n_ego_agei[i]==0){
    AvgSisAlive[i,] <- rep(NA,length(ages))
    
  }else if(n_ego_agei[i]>0){ 
    Mothers.ID.ego <- ego_agei$motherID # mothers ID of ego age i
 
    for(j in ages){ # loop over sisters ages
      sis_agej <- datalastF[datalastF$age==j] #individuals of age j alive at last time step
      
      if (j==i){ #same cohort sisters
        siblingsAlivei <- tapply(X=sis_agej$who,INDEX=sis_agej$motherID)
        sistersdupAlivei <- split(x=sis_agej$who,f=siblingsAlivei)
        sistersAlivei <- lapply(sistersdupAlivei,unique) 
        AvgSisAlive[i,j]<-sum(lengths(sistersAlivei)*(lengths(sistersAlivei)-1))/sum(lengths(sistersAlivei))
        
      } else if (j!=i){ #sisters from different cohorts
        Mothers.ID.sis <- sis_agej$motherID
        AvgSisAlive[i,j]<- sum(table(Mothers.ID.ego[Mothers.ID.ego %in% Mothers.ID.sis])*table(Mothers.ID.sis[Mothers.ID.sis %in% Mothers.ID.ego]))/n_ego_agei[i]
      } # close "if" sisters from different cohorts
    } #close loop over sisters ages
  } #close "if" there are more than zero ego age i
} #close loop over age i

AvgSisAlive # kinship matrix for sisters

#########################################################################################
###   LOOP FOR CHILDREN & GRANDCHILDREN             #####################################
#########################################################################################

nbChAlive <- kinship_Ch <- matrix(NA, nrow=max(ages),ncol=max(ages))
nbGChAlive <- kinship_GCh <- matrix(NA, nrow=max(ages),ncol=max(ages))

n_ego_agei <- n_Ch <- n_Gch <-Avg_Ch_egoagei <- Avg_GCh_egoagei <-NULL

children <- list()

for(i in ages){  #loop over ego age
  ego_agei <- datalastF[datalastF$age==i,] # ego age i alive at last time step
  n_ego_agei[i] <-nrow(ego_agei) # nb of ego age i
  if(n_ego_agei[i]==0){
    nbChAlive[i,] <- rep(NA,length(ages))
    nbGChAlive[i,] <- rep(NA,length(ages))
  }else if(n_ego_agei[i]>0){ 
    
    # for Children
    Children.ID.i <- datalastF$who[which(datalastF$motherID %in% ego_agei$who)] # ID of children of ego age i
    n_Ch[i] <- length(Children.ID.i) # nb of children of ego age i alive at last time step
    Avg_Ch_egoagei[i]  <- n_Ch[i]/ n_ego_agei[i]  # average number of children of ego age i
    
    # for grand children
    DaughtersDAi <- unique(AllFem$who[which(AllFem$motherID %in% ego_agei$who)]) #dead or alive daughters of ego age i
    GChildren.ID.i <- datalastF$who[which(datalastF$motherID %in% DaughtersDAi)] #ID of alive grandchildren of ego age i           
    n_Gch[i] <- length(GChildren.ID.i)
    Avg_GCh_egoagei[i]  <- n_Gch[i]/ n_ego_agei[i]  # average number of grandchildren of ego age i
    
    # age of alive children and grandchildren at last time step
    Chage <- datalastF$age[match(Children.ID.i, datalastF$who )] #match ID of children with datalastF to get their age
    GChage <- datalastF$age[match(GChildren.ID.i, datalastF$who )] # match ID of grandchildren with datalastF to get their age
    
    for(j in ages){ # loop over children and grandchildren ages
      # FOR MOTHERS
      nbChAlive[i,j] <- sum(Chage==j) 
      kinship_Ch[i,j] <- sum(Chage==j) / n_ego_agei[i] # kinship matrix for expected number of children
      # FOR GRAND MOTHERS
      nbGChAlive[i,j] <- sum(GChage==j) 
      kinship_GCh[i,j] <- sum(GChage==j) / n_ego_agei[i] # kinship matrix for expected number of grandchildren

    } # end loop over children & grand children age
  } # end if loop 
} # end loop over ego age

Res_summary.desc <- data.frame(ego.age=ages,
                          nb.ego=n_ego_agei,  # number of ego aged i at last time step
                          nb.Ch=n_Ch,  # number of children of ego age i
                          Avg_Ch =Avg_Ch_egoagei, # expected number of children for ego age i
                          nb.GCh=n_Gch,  # number of grandchildren of ego age i
                          Avg_GCh =Avg_GCh_egoagei) # proba mother alive for ego age i

round(Res_summary.desc,2) # summary per ego age

kinship_Ch # kinship matrix for children
kinship_GCh # kinship matrix for grandchildren


#########################################################################################
###   LOOP FOR AUNTS    #######################################################
#########################################################################################

#Aunts
kinship_Aunts <- matrix(NA, nrow=max(ages),ncol=max(ages))
n_ego_agei <- n_Aunts <-NULL


for(i in ages){  #loop over ego age
  ego_agei <- datalastF[datalastF$age==i,] # ego age i alive at last time step
  n_ego_agei[i] <-nrow(ego_agei) # nb of ego age i
  if(n_ego_agei[i]==0){
    kinship_Aunts[i,] <- rep(NA,length(ages))
    
  }else if(n_ego_agei[i]>0){ 
    
    # find mothers' & grandmothers' IDs
    Mothers.ID <- ego_agei$motherID # mothers ID of ego age i
    GM.ID <- AllFem$motherID[match(Mothers.ID, AllFem$who )] 
    
    #find grandmother's daughters
    AllPosAuntsi <- AllFem[which(AllFem$motherID %in% GM.ID),] #all possible aunts (i.e. daughters of grandmother: either mother or aunt of ego)
    PosAuntsAlivei <- datalastF[which(AllPosAuntsi$who %in% datalastF$who)] #all possible aunts alive at last time step
    
    #potential aunts split by their mother ID
    AuntsAlivei <- tapply(X=PosAuntsAlivei$who,INDEX=PosAuntsAlivei$motherID)
    AuntsdupAlivei <- split(x=PosAuntsAlivei$who,f=AuntsAlivei)
    AuntsAlivei <- lapply(AuntsdupAlivei,unique) 

  }
}
    
 


###AUNTS
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

#Aunts
kinship_Aunts <- matrix(NA, nrow=max(ages),ncol=max(ages))
n_ego_agei <- n_Aunts <-NULL

for(i in ages){  #loop over ego age
  ego_agei <- datalastF[datalastF$age==i,] # ego age i alive at last time step
  n_ego_agei[i] <-nrow(ego_agei) # nb of ego age i
  if(n_ego_agei[i]==0){
    kinship_Aunts[i,] <- rep(NA,length(ages))
    
  }else if(n_ego_agei[i]>0){ 
    
    # find mothers & aunts
    Mothers.ID <- ego_agei$motherID # mothers ID of ego age i
    GM.ID <- AllFem$motherID[match(Mothers.ID, AllFem$who )] #ID of grandmothers of ego age i
    AllPosAuntsi <- AllFem[which(AllFem$motherID %in% GM.ID),] #all possible aunts (i.e. daughters of grandmother: either mother or aunt of ego)
    
    
    #potential aunts split by their mother ID
    AuntsAlivei <- tapply(X=PosAuntsAlivei$who,INDEX=PosAuntsAlivei$motherID)
    AuntsdupAlivei <- split(x=PosAuntsAlivei$who,f=AuntsAlivei)
    AuntsAlivei <- lapply(AuntsdupAlivei,unique) 
    
  }
}

grandmotherID <- AllFem[which(AllFem$who %in% datalastF$motherID),'motherID']

c(grandmotherID)

Femdata_withGM <- cbind(datalastF,list(grandmotherID))
grandmotherID

class(datalast)

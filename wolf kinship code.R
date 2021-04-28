# kinship relationships at equilibrium

# all indiv ever lived in the pop
dataall  <- unique(do.call(rbind, popSim)  )
allwithparents <- dataall[!is.na(dataall$motherID),] #### What does the comma do? ####
AllFem <- allwithparents[allwithparents$sex=="F",]  # all females with known mother
IDMothers <- unique(AllFem[,'motherID'] ) # mothers of at least one female
IDGrandMothers <- unique(AllFem[which(AllFem$who %in% IDMothers),'motherID'])

siblings <- tapply(X=AllFem$who,INDEX=AllFem$motherID)
sistersdup <- split(x=AllFem$who,f=siblings)
sisters <- lapply(sistersdup,unique)

# for replicate j at the last time step
data <- as.matrix(popSim[[26]])
length(data$who) # 182 individuals the last year
data$who # ID alive the last year
dim(dataF <- data[data$sex=="F",]) # 89 females
dataF$who # ID fem alive the last year
sum(is.na(data$motherID)) # 1 with unknown parent the last year

table(data$age) # age 1 to 14

# Mothers alive at last time step
IDMothersAlive <- dataF$who[dataF$who %in% IDMothers]
# Grand Mothers alive at last time step
IDGrandMothersAlive <- dataF$who[dataF$who %in% IDGrandMothers]

# sisters at last time step
siblingsAlive <- tapply(X=dataF$who,INDEX=dataF$motherID)
sistersdupAlive <- split(x=dataF$who,f=siblingsAlive)
sistersAlive <- lapply(sistersdupAlive,unique)

# calculate relatedness for all indiv alive at the end, and for females alive only
withParentID <- NLwith(agents = popSim[[(nYearSim + 1)]], var = "motherID", val = 0:1000000) # extract individuals with parent ID
FemwithParentID <- NLwith(agents = withParentID, var = "sex", val = "F") # females with parent ID
allRelatedness <- relatedness(listAllInd = popSim, whoInd = of(agents = withParentID, var = "who"))
FemRelatedness <- relatedness(listAllInd = popSim, whoInd = of(agents = FemwithParentID, var = "who"))

dim(allRelatedness)
dim(FemRelatedness)


# for each female alive of age j : proba of mother alive
ages <- 1:14
PrAlive <- data.frame(age=ages, Mother=rep(NA,length(ages)),  GrandMother=rep(NA,length(ages)), Sisters = rep(NA, length(ages)),nfem=rep(NA,length(ages)))
for(j in 1:14){
  Femagej <- dataF[dataF$age==j,]           ######### What does the comma do? ###########
  nFemAge <- nrow(Femagej)  # select subdata for age 1
  PrAlive[j,5] <- nFemAge
  if(nFemAge==0){
    PrAlive[j,2] <- NA
    PrAlive[j,3] <- NA
    PrAlive[j,4] <- NA
    PrAlive[j,5] <- NA
  }else if(nFemAge>0){ 
    IDmothersj <-unique(Femagej$motherID) 
    PrAlive[j,2] <- sum(IDmothersj %in% IDMothersAlive) / nFemAge
    IDGrandMothersj <- dataF$motherID[ which(dataF$who  %in% IDmothersj) ] 
    
    #above we get the mother ID of those individuals whose ID is found in the table of mothers of females of age j
    #i.e. the ID of the grandmothers of all females with age j. 
    PrAlive[j,3] <- sum(IDGrandMothersj %in% IDGrandMothersAlive) / nFemAge
    
    siblingsAlivej <- tapply(X=Femagej$who,INDEX=Femagej$motherID) ## Only splitting individuals of a given age by mother,
    sistersdupAlivej <- split(x=Femagej$who,f=siblingsAlivej)      ## but siblings could be from different cohorts. 
    sistersAlivej <- lapply(sistersdupAlivej,unique)
    avg.nsistersj<-sum(lengths(sistersAlivej)*(lengths(sistersAlivej)-1))/sum(lengths(sistersAlivej))
      PrAlive[j,4]<-avg.nsistersj
  }
}

#   IDMothers <- unique(dataall[dataF$age==j,'motherID']) # find mothers
#  }
#  if(sum(dataF$who %in% IDMothers)==0) {PrMotherAlive[j,2] <- 0
#  }else if(sum(dataF$who %in% IDMothers)>0){
#    IDMothersalive <- dataF$who[which(IDMothers %in% dataF$who)] # 14 check if mothers still alive  
#    PrMotherAlive[j,2] <- (sum(dataF[dataF$age==j,'motherID'] %in% IDMothersalive) ) / nFemAge # number of females with mother still alive
#    }

#  if(sum(IDMothers %in% dataF$who)==0) {PrGrandMotherAlive[j,2] <- 0
#  }else if(sum(IDMothers %in% dataF$who)>0){
#    IDGrandMothers <- dataF$motherID[ which(dataF$who  %in% IDMothers) ]
#    IDGrandMothersalive <- dataF$who[which(dataF$who %in% IDGrandMothers)] # find alive grand mothers
#    PrGrandMotherAlive[j,2] <- (sum(dataF[dataF$age==j,'motherID'] %in% IDGrandMothersalive) ) / nFemAge # number of females with mother still alive
#    } 
#  }


PrAlive1 <- data.frame(age=ages, Mother=rep(NA,length(ages)),  GrandMother=rep(NA,length(ages)),nfem=rep(NA,length(ages)) )

  Femage1 <- dataF[dataF$age==1,]                                      
  nFemAge1 <- nrow(Femage1)  # select subdata for age 1
  PrAlive[1,4] <- nFemAge1
  if(nFemAge1==0){
    PrAlive[1,2] <- NA
    PrAlive[1,3] <- NA
  }else if(nFemAge1>0){ 
    IDmothers1 <-unique(Femage1$motherID) 
    PrAlive1[1,2] <- sum(IDmothers1 %in% IDMothersAlive) / nFemAge1
    
    IDGrandMothers1 <- dataF$motherID[ which(dataF$who  %in% IDmothers1) ]
    PrAlive1[1,3] <- sum(IDGrandMothers1 %in% IDGrandMothersAlive) / nFemAge1
    
  }

PrAlive1



##############################################################################
#  Kinship matrix for mothers ################################################
##############################################################################

#MANUALLY

#Ego age 1 
  Egoage1 <- dataF[dataF$age==1,] 
  negoage1 <-nrow(Egoage1)
  Egoage1.MID <- Egoage1[!is.na(Egoage1$motherID),]
  Mothers.Alive.ID <- dataF[which(dataF$who %in% Egoage1.MID$motherID),]
  Motherage1<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==1),]
  Motherage2<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==2),]
  Motherage3<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==3),]
  Motherage4<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==4),]
  Motherage5<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==5),]
  Motherage6<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==6),]
  Motherage7<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==7),]
  Motherage8<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==8),]
  Motherage9<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==9),]
  Motherage10<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==10),]
  Motherage11<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==11),]
  Motherage12<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==12),]
  Motherage13<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==13),]
  Motherage14<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==14),]
  nmotherage1.egoage1 <- nrow(Motherage1)
  nmotherage2.egoage1 <- nrow(Motherage2)
  nmotherage3.egoage1 <- nrow(Motherage3)
  nmotherage4.egoage1 <- nrow(Motherage4)
  nmotherage5.egoage1 <- nrow(Motherage5)
  nmotherage6.egoage1 <- nrow(Motherage6)
  nmotherage7.egoage1 <- nrow(Motherage7)
  nmotherage8.egoage1 <- nrow(Motherage8)
  nmotherage9.egoage1 <- nrow(Motherage9)
  nmotherage10.egoage1 <- nrow(Motherage10)
  nmotherage11.egoage1 <- nrow(Motherage11)
  nmotherage12.egoage1 <- nrow(Motherage12)
  nmotherage13.egoage1 <- nrow(Motherage13)
  nmotherage14.egoage1 <- nrow(Motherage14)
  motheragevector<-c(nmotherage1.egoage1,nmotherage2.egoage1,nmotherage3.egoage1,nmotherage4.egoage1,nmotherage5.egoage1,
                     nmotherage6.egoage1,nmotherage7.egoage1,nmotherage8.egoage1,nmotherage9.egoage1,nmotherage10.egoage1,
                     nmotherage11.egoage1,nmotherage12.egoage1,nmotherage13.egoage1,nmotherage14.egoage1)
  Mother_aliveness_by_age1 <- motheragevector/negoage1
  Mother_aliveness_by_age1
  sum(Mother_aliveness_by_age1) #check: it is the same as the total probability of a female of age 1 having her mother alive
  
  
  #Ego age 2
  Egoage2 <- dataF[dataF$age==2,] 
  negoage2 <-nrow(Egoage2)
  Egoage2.MID <- Egoage2[!is.na(Egoage2$motherID),]
  Mothers.Alive.ID2 <- dataF[which(dataF$who %in% Egoage2.MID$motherID),]
  Motherage1.2<- Mothers.Alive.ID2[which(Mothers.Alive.ID2$age==1),]
  Motherage2.2<- Mothers.Alive.ID2[which(Mothers.Alive.ID2$age==2),]
  Motherage3.2<- Mothers.Alive.ID2[which(Mothers.Alive.ID2$age==3),]
  Motherage4.2<- Mothers.Alive.ID2[which(Mothers.Alive.ID2$age==4),]
  Motherage5.2<- Mothers.Alive.ID2[which(Mothers.Alive.ID2$age==5),]
  Motherage6.2<- Mothers.Alive.ID2[which(Mothers.Alive.ID2$age==6),]
  Motherage7.2<- Mothers.Alive.ID2[which(Mothers.Alive.ID2$age==7),]
  Motherage8.2<- Mothers.Alive.ID2[which(Mothers.Alive.ID2$age==8),]
  Motherage9.2<- Mothers.Alive.ID2[which(Mothers.Alive.ID2$age==9),]
  Motherage10.2<- Mothers.Alive.ID2[which(Mothers.Alive.ID2$age==10),]
  Motherage11.2<- Mothers.Alive.ID2[which(Mothers.Alive.ID2$age==11),]
  Motherage12.2<- Mothers.Alive.ID2[which(Mothers.Alive.ID2$age==12),]
  Motherage13.2<- Mothers.Alive.ID2[which(Mothers.Alive.ID2$age==13),]
  Motherage14.2<- Mothers.Alive.ID2[which(Mothers.Alive.ID2$age==14),]
  nmotherage1.egoage2 <- nrow(Motherage1.2)
  nmotherage2.egoage2 <- nrow(Motherage2.2)
  nmotherage3.egoage2 <- nrow(Motherage3.2)
  nmotherage4.egoage2 <- nrow(Motherage4.2)
  nmotherage5.egoage2 <- nrow(Motherage5.2)
  nmotherage6.egoage2 <- nrow(Motherage6.2)
  nmotherage7.egoage2 <- nrow(Motherage7.2)
  nmotherage8.egoage2 <- nrow(Motherage8.2)
  nmotherage9.egoage2 <- nrow(Motherage9.2)
  nmotherage10.egoage2 <- nrow(Motherage10.2)
  nmotherage11.egoage2 <- nrow(Motherage11.2)
  nmotherage12.egoage2 <- nrow(Motherage12.2)
  nmotherage13.egoage2 <- nrow(Motherage13.2)
  nmotherage14.egoage2 <- nrow(Motherage14.2)
 
  motheragevector2<-c(nmotherage1.egoage2,nmotherage2.egoage2,nmotherage3.egoage2,nmotherage4.egoage2,nmotherage5.egoage2,
                     nmotherage6.egoage2,nmotherage7.egoage2,nmotherage8.egoage2,nmotherage9.egoage2,nmotherage10.egoage2,
                     nmotherage11.egoage2,nmotherage12.egoage2,nmotherage13.egoage2,nmotherage14.egoage2)
  Mother_aliveness_by_age2 <- motheragevector2/negoage2
  Mother_aliveness_by_age2
  sum(Mother_aliveness_by_age2)
  
  #Ego age 3
  Egoage3 <- dataF[dataF$age==3,] 
  negoage3 <-nrow(Egoage3)
  Egoage3.MID <- Egoage3[!is.na(Egoage3$motherID),]
  Mothers.Alive.ID3 <- dataF[which(dataF$who %in% Egoage3.MID$motherID),]
  Motherage1.3<- Mothers.Alive.ID3[which(Mothers.Alive.ID3$age==1),]
  Motherage2.3<- Mothers.Alive.ID3[which(Mothers.Alive.ID3$age==2),]
  Motherage3.3<- Mothers.Alive.ID3[which(Mothers.Alive.ID3$age==3),]
  Motherage4.3<- Mothers.Alive.ID3[which(Mothers.Alive.ID3$age==4),]
  Motherage5.3<- Mothers.Alive.ID3[which(Mothers.Alive.ID3$age==5),]
  Motherage6.3<- Mothers.Alive.ID3[which(Mothers.Alive.ID3$age==6),]
  Motherage7.3<- Mothers.Alive.ID3[which(Mothers.Alive.ID3$age==7),]
  Motherage8.3<- Mothers.Alive.ID3[which(Mothers.Alive.ID3$age==8),]
  Motherage9.3<- Mothers.Alive.ID3[which(Mothers.Alive.ID3$age==9),]
  Motherage10.3<- Mothers.Alive.ID3[which(Mothers.Alive.ID3$age==10),]
  Motherage11.3<- Mothers.Alive.ID3[which(Mothers.Alive.ID3$age==11),]
  Motherage12.3<- Mothers.Alive.ID3[which(Mothers.Alive.ID3$age==12),]
  Motherage13.3<- Mothers.Alive.ID3[which(Mothers.Alive.ID3$age==13),]
  Motherage14.3<- Mothers.Alive.ID3[which(Mothers.Alive.ID3$age==14),]
  nmotherage1.egoage3 <- nrow(Motherage1.3)
  nmotherage2.egoage3 <- nrow(Motherage2.3)
  nmotherage3.egoage3 <- nrow(Motherage3.3)
  nmotherage4.egoage3 <- nrow(Motherage4.3)
  nmotherage5.egoage3 <- nrow(Motherage5.3)
  nmotherage6.egoage3 <- nrow(Motherage6.3)
  nmotherage7.egoage3 <- nrow(Motherage7.3)
  nmotherage8.egoage3 <- nrow(Motherage8.3)
  nmotherage9.egoage3 <- nrow(Motherage9.3)
  nmotherage10.egoage3 <- nrow(Motherage10.3)
  nmotherage11.egoage3 <- nrow(Motherage11.3)
  nmotherage12.egoage3 <- nrow(Motherage12.3)
  nmotherage13.egoage3 <- nrow(Motherage13.3)
  nmotherage14.egoage3 <- nrow(Motherage14.3)
  
  motheragevector3<-c(nmotherage1.egoage3,nmotherage2.egoage3,nmotherage3.egoage3,nmotherage4.egoage3,nmotherage5.egoage3,
                      nmotherage6.egoage3,nmotherage7.egoage3,nmotherage8.egoage3,nmotherage9.egoage3,nmotherage10.egoage3,
                      nmotherage11.egoage3,nmotherage12.egoage3,nmotherage13.egoage3,nmotherage14.egoage3)
  Mother_aliveness_by_age3 <- motheragevector3/negoage3
  Mother_aliveness_by_age3
  sum(Mother_aliveness_by_age3)
  
  #Ego age 4
  Egoage4 <- dataF[dataF$age==4,] 
  negoage4 <-nrow(Egoage4)
  Egoage4.MID <- Egoage4[!is.na(Egoage4$motherID),]
  Mothers.Alive.ID4 <- dataF[which(dataF$who %in% Egoage4.MID$motherID),]
  Motherage1.4<- Mothers.Alive.ID4[which(Mothers.Alive.ID4$age==1),]
  Motherage2.4<- Mothers.Alive.ID4[which(Mothers.Alive.ID4$age==2),]
  Motherage3.4<- Mothers.Alive.ID4[which(Mothers.Alive.ID4$age==3),]
  Motherage4.4<- Mothers.Alive.ID4[which(Mothers.Alive.ID4$age==4),]
  Motherage5.4<- Mothers.Alive.ID4[which(Mothers.Alive.ID4$age==5),]
  Motherage6.4<- Mothers.Alive.ID4[which(Mothers.Alive.ID4$age==6),]
  Motherage7.4<- Mothers.Alive.ID4[which(Mothers.Alive.ID4$age==7),]
  Motherage8.4<- Mothers.Alive.ID4[which(Mothers.Alive.ID4$age==8),]
  Motherage9.4<- Mothers.Alive.ID4[which(Mothers.Alive.ID4$age==9),]
  Motherage10.4<- Mothers.Alive.ID4[which(Mothers.Alive.ID4$age==10),]
  Motherage11.4<- Mothers.Alive.ID4[which(Mothers.Alive.ID4$age==11),]
  Motherage12.4<- Mothers.Alive.ID4[which(Mothers.Alive.ID4$age==12),]
  Motherage13.4<- Mothers.Alive.ID4[which(Mothers.Alive.ID4$age==13),]
  Motherage14.4<- Mothers.Alive.ID4[which(Mothers.Alive.ID4$age==14),]
  nmotherage1.egoage4 <- nrow(Motherage1.4)
  nmotherage2.egoage4 <- nrow(Motherage2.4)
  nmotherage3.egoage4 <- nrow(Motherage3.4)
  nmotherage4.egoage4 <- nrow(Motherage4.4)
  nmotherage5.egoage4 <- nrow(Motherage5.4)
  nmotherage6.egoage4 <- nrow(Motherage6.4)
  nmotherage7.egoage4 <- nrow(Motherage7.4)
  nmotherage8.egoage4 <- nrow(Motherage8.4)
  nmotherage9.egoage4 <- nrow(Motherage9.4)
  nmotherage10.egoage4 <- nrow(Motherage10.4)
  nmotherage11.egoage4 <- nrow(Motherage11.4)
  nmotherage12.egoage4 <- nrow(Motherage12.4)
  nmotherage13.egoage4 <- nrow(Motherage13.4)
  nmotherage14.egoage4 <- nrow(Motherage14.4)
  
  motheragevector4<-c(nmotherage1.egoage4,nmotherage2.egoage4,nmotherage3.egoage4,nmotherage4.egoage4,nmotherage5.egoage4,
                      nmotherage6.egoage4,nmotherage7.egoage4,nmotherage8.egoage4,nmotherage9.egoage4,nmotherage10.egoage4,
                      nmotherage11.egoage4,nmotherage12.egoage4,nmotherage13.egoage4,nmotherage14.egoage4)
  Mother_aliveness_by_age4 <- motheragevector4/negoage4
  Mother_aliveness_by_age4
  sum(Mother_aliveness_by_age4)
  
  PrAlive
  
  #Ego age 5
  Egoage5 <- dataF[dataF$age==5,] 
  negoage5 <-nrow(Egoage5)
  Egoage5.MID <- Egoage5[!is.na(Egoage5$motherID),]
  Mothers.Alive.ID5 <- dataF[which(dataF$who %in% Egoage5.MID$motherID),]
  Motherage1.5<- Mothers.Alive.ID5[which(Mothers.Alive.ID5$age==1),]
  Motherage2.5<- Mothers.Alive.ID5[which(Mothers.Alive.ID5$age==2),]
  Motherage3.5<- Mothers.Alive.ID5[which(Mothers.Alive.ID5$age==3),]
  Motherage4.5<- Mothers.Alive.ID5[which(Mothers.Alive.ID5$age==4),]
  Motherage5.5<- Mothers.Alive.ID5[which(Mothers.Alive.ID5$age==5),]
  Motherage6.5<- Mothers.Alive.ID5[which(Mothers.Alive.ID5$age==6),]
  Motherage7.5<- Mothers.Alive.ID5[which(Mothers.Alive.ID5$age==7),]
  Motherage8.5<- Mothers.Alive.ID5[which(Mothers.Alive.ID5$age==8),]
  Motherage9.5<- Mothers.Alive.ID5[which(Mothers.Alive.ID5$age==9),]
  Motherage10.5<- Mothers.Alive.ID5[which(Mothers.Alive.ID5$age==10),]
  Motherage11.5<- Mothers.Alive.ID5[which(Mothers.Alive.ID5$age==11),]
  Motherage12.5<- Mothers.Alive.ID5[which(Mothers.Alive.ID5$age==12),]
  Motherage13.5<- Mothers.Alive.ID5[which(Mothers.Alive.ID5$age==13),]
  Motherage14.5<- Mothers.Alive.ID5[which(Mothers.Alive.ID5$age==14),]
  nmotherage1.egoage5 <- nrow(Motherage1.5)
  nmotherage2.egoage5 <- nrow(Motherage2.5)
  nmotherage3.egoage5 <- nrow(Motherage3.5)
  nmotherage4.egoage5 <- nrow(Motherage4.5)
  nmotherage5.egoage5 <- nrow(Motherage5.5)
  nmotherage6.egoage5 <- nrow(Motherage6.5)
  nmotherage7.egoage5 <- nrow(Motherage7.5)
  nmotherage8.egoage5 <- nrow(Motherage8.5)
  nmotherage9.egoage5 <- nrow(Motherage9.5)
  nmotherage10.egoage5 <- nrow(Motherage10.5)
  nmotherage11.egoage5 <- nrow(Motherage11.5)
  nmotherage12.egoage5 <- nrow(Motherage12.5)
  nmotherage13.egoage5 <- nrow(Motherage13.5)
  nmotherage14.egoage5 <- nrow(Motherage14.5)
  
  motheragevector5<-c(nmotherage1.egoage5,nmotherage2.egoage5,nmotherage3.egoage5,nmotherage4.egoage5,nmotherage5.egoage5,
                      nmotherage6.egoage5,nmotherage7.egoage5,nmotherage8.egoage5,nmotherage9.egoage5,nmotherage10.egoage5,
                      nmotherage11.egoage5,nmotherage12.egoage5,nmotherage13.egoage5,nmotherage14.egoage5)
  Mother_aliveness_by_age5 <- motheragevector5/negoage5
  Mother_aliveness_by_age5
  sum(Mother_aliveness_by_age5)
  
  PrAlive
  
  #Ego age 6
  Egoage6 <- dataF[dataF$age==6,] 
  negoage6 <-nrow(Egoage6)
  Egoage6.MID <- Egoage6[!is.na(Egoage6$motherID),]
  Mothers.Alive.ID6 <- dataF[which(dataF$who %in% Egoage6.MID$motherID),]
  Motherage1.6<- Mothers.Alive.ID6[which(Mothers.Alive.ID6$age==1),]
  Motherage2.6<- Mothers.Alive.ID6[which(Mothers.Alive.ID6$age==2),]
  Motherage3.6<- Mothers.Alive.ID6[which(Mothers.Alive.ID6$age==3),]
  Motherage4.6<- Mothers.Alive.ID6[which(Mothers.Alive.ID6$age==4),]
  Motherage5.6<- Mothers.Alive.ID6[which(Mothers.Alive.ID6$age==5),]
  Motherage6.6<- Mothers.Alive.ID6[which(Mothers.Alive.ID6$age==6),]
  Motherage7.6<- Mothers.Alive.ID6[which(Mothers.Alive.ID6$age==7),]
  Motherage8.6<- Mothers.Alive.ID6[which(Mothers.Alive.ID6$age==8),]
  Motherage9.6<- Mothers.Alive.ID6[which(Mothers.Alive.ID6$age==9),]
  Motherage10.6<- Mothers.Alive.ID6[which(Mothers.Alive.ID6$age==10),]
  Motherage11.6<- Mothers.Alive.ID6[which(Mothers.Alive.ID6$age==11),]
  Motherage12.6<- Mothers.Alive.ID6[which(Mothers.Alive.ID6$age==12),]
  Motherage13.6<- Mothers.Alive.ID6[which(Mothers.Alive.ID6$age==13),]
  Motherage14.6<- Mothers.Alive.ID6[which(Mothers.Alive.ID6$age==14),]
  nmotherage1.egoage6 <- nrow(Motherage1.6)
  nmotherage2.egoage6 <- nrow(Motherage2.6)
  nmotherage3.egoage6 <- nrow(Motherage3.6)
  nmotherage4.egoage6 <- nrow(Motherage4.6)
  nmotherage5.egoage6 <- nrow(Motherage5.6)
  nmotherage6.egoage6 <- nrow(Motherage6.6)
  nmotherage7.egoage6 <- nrow(Motherage7.6)
  nmotherage8.egoage6 <- nrow(Motherage8.6)
  nmotherage9.egoage6 <- nrow(Motherage9.6)
  nmotherage10.egoage6 <- nrow(Motherage10.6)
  nmotherage11.egoage6 <- nrow(Motherage11.6)
  nmotherage12.egoage6 <- nrow(Motherage12.6)
  nmotherage13.egoage6 <- nrow(Motherage13.6)
  nmotherage14.egoage6 <- nrow(Motherage14.6)
  
  motheragevector6<-c(nmotherage1.egoage6,nmotherage2.egoage6,nmotherage3.egoage6,nmotherage4.egoage6,nmotherage5.egoage6,
                      nmotherage6.egoage6,nmotherage7.egoage6,nmotherage8.egoage6,nmotherage9.egoage6,nmotherage10.egoage6,
                      nmotherage11.egoage6,nmotherage12.egoage6,nmotherage13.egoage6,nmotherage14.egoage6)
  Mother_aliveness_by_age6 <- motheragevector6/negoage6
  Mother_aliveness_by_age6
  sum(Mother_aliveness_by_age6)
  
  PrAlive

  
  #Ego age 7
  Egoage7 <- dataF[dataF$age==7,] 
  negoage7 <-nrow(Egoage7)
  Egoage7.MID <- Egoage7[!is.na(Egoage7$motherID),]
  Mothers.Alive.ID7 <- dataF[which(dataF$who %in% Egoage7.MID$motherID),]
  Motherage1.7<- Mothers.Alive.ID7[which(Mothers.Alive.ID7$age==1),]
  Motherage2.7<- Mothers.Alive.ID7[which(Mothers.Alive.ID7$age==2),]
  Motherage3.7<- Mothers.Alive.ID7[which(Mothers.Alive.ID7$age==3),]
  Motherage4.7<- Mothers.Alive.ID7[which(Mothers.Alive.ID7$age==4),]
  Motherage5.7<- Mothers.Alive.ID7[which(Mothers.Alive.ID7$age==5),]
  Motherage6.7<- Mothers.Alive.ID7[which(Mothers.Alive.ID7$age==6),]
  Motherage7.7<- Mothers.Alive.ID7[which(Mothers.Alive.ID7$age==7),]
  Motherage8.7<- Mothers.Alive.ID7[which(Mothers.Alive.ID7$age==8),]
  Motherage9.7<- Mothers.Alive.ID7[which(Mothers.Alive.ID7$age==9),]
  Motherage10.7<- Mothers.Alive.ID7[which(Mothers.Alive.ID7$age==10),]
  Motherage11.7<- Mothers.Alive.ID7[which(Mothers.Alive.ID7$age==11),]
  Motherage12.7<- Mothers.Alive.ID7[which(Mothers.Alive.ID7$age==12),]
  Motherage13.7<- Mothers.Alive.ID7[which(Mothers.Alive.ID7$age==13),]
  Motherage14.7<- Mothers.Alive.ID7[which(Mothers.Alive.ID7$age==14),]
  nmotherage1.egoage7 <- nrow(Motherage1.7)
  nmotherage2.egoage7 <- nrow(Motherage2.7)
  nmotherage3.egoage7 <- nrow(Motherage3.7)
  nmotherage4.egoage7 <- nrow(Motherage4.7)
  nmotherage5.egoage7 <- nrow(Motherage5.7)
  nmotherage6.egoage7 <- nrow(Motherage6.7)
  nmotherage7.egoage7 <- nrow(Motherage7.7)
  nmotherage8.egoage7 <- nrow(Motherage8.7)
  nmotherage9.egoage7 <- nrow(Motherage9.7)
  nmotherage10.egoage7 <- nrow(Motherage10.7)
  nmotherage11.egoage7 <- nrow(Motherage11.7)
  nmotherage12.egoage7 <- nrow(Motherage12.7)
  nmotherage13.egoage7 <- nrow(Motherage13.7)
  nmotherage14.egoage7 <- nrow(Motherage14.7)
  
  motheragevector7<-c(nmotherage1.egoage7,nmotherage2.egoage7,nmotherage3.egoage7,nmotherage4.egoage7,nmotherage5.egoage7,
                      nmotherage6.egoage7,nmotherage7.egoage7,nmotherage8.egoage7,nmotherage9.egoage7,nmotherage10.egoage7,
                      nmotherage11.egoage7,nmotherage12.egoage7,nmotherage13.egoage7,nmotherage14.egoage7)
  Mother_aliveness_by_age7 <- motheragevector7/negoage7
  Mother_aliveness_by_age7
  sum(Mother_aliveness_by_age7)
  
  PrAlive
  
  #Ego age 8
  Egoage8 <- dataF[dataF$age==8,] 
  negoage8 <-nrow(Egoage8)
  Egoage8.MID <- Egoage8[!is.na(Egoage8$motherID),]
  Mothers.Alive.ID8 <- dataF[which(dataF$who %in% Egoage8.MID$motherID),]
  Motherage1.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==1),]
  Motherage2.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==2),]
  Motherage3.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==3),]
  Motherage4.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==4),]
  Motherage5.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==5),]
  Motherage6.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==6),]
  Motherage7.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==7),]
  Motherage8.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==8),]
  Motherage9.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==9),]
  Motherage10.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==10),]
  Motherage11.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==11),]
  Motherage12.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==12),]
  Motherage13.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==13),]
  Motherage14.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==14),]
  nmotherage1.egoage8 <- nrow(Motherage1.8)
  nmotherage2.egoage8 <- nrow(Motherage2.8)
  nmotherage3.egoage8 <- nrow(Motherage3.8)
  nmotherage4.egoage8 <- nrow(Motherage4.8)
  nmotherage5.egoage8 <- nrow(Motherage5.8)
  nmotherage6.egoage8 <- nrow(Motherage6.8)
  nmotherage7.egoage8 <- nrow(Motherage7.8)
  nmotherage8.egoage8 <- nrow(Motherage8.8)
  nmotherage9.egoage8 <- nrow(Motherage9.8)
  nmotherage10.egoage8 <- nrow(Motherage10.8)
  nmotherage11.egoage8 <- nrow(Motherage11.8)
  nmotherage12.egoage8 <- nrow(Motherage12.8)
  nmotherage13.egoage8 <- nrow(Motherage13.8)
  nmotherage14.egoage8 <- nrow(Motherage14.8)
  
  motheragevector8<-c(nmotherage1.egoage8,nmotherage2.egoage8,nmotherage3.egoage8,nmotherage4.egoage8,nmotherage5.egoage8,
                      nmotherage6.egoage8,nmotherage7.egoage8,nmotherage8.egoage8,nmotherage9.egoage8,nmotherage10.egoage8,
                      nmotherage11.egoage8,nmotherage12.egoage8,nmotherage13.egoage8,nmotherage14.egoage8)
  Mother_aliveness_by_age8 <- motheragevector8/negoage8
  Mother_aliveness_by_age8
  sum(Mother_aliveness_by_age8)
  
  PrAlive
  
  #Ego age 8
  Egoage8 <- dataF[dataF$age==8,] 
  negoage8 <-nrow(Egoage8)
  Egoage8.MID <- Egoage8[!is.na(Egoage8$motherID),]
  Mothers.Alive.ID8 <- dataF[which(dataF$who %in% Egoage8.MID$motherID),]
  Motherage1.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==1),]
  Motherage2.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==2),]
  Motherage3.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==3),]
  Motherage4.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==4),]
  Motherage5.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==5),]
  Motherage6.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==6),]
  Motherage7.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==7),]
  Motherage8.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==8),]
  Motherage9.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==9),]
  Motherage10.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==10),]
  Motherage11.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==11),]
  Motherage12.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==12),]
  Motherage13.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==13),]
  Motherage14.8<- Mothers.Alive.ID8[which(Mothers.Alive.ID8$age==14),]
  nmotherage1.egoage8 <- nrow(Motherage1.8)
  nmotherage2.egoage8 <- nrow(Motherage2.8)
  nmotherage3.egoage8 <- nrow(Motherage3.8)
  nmotherage4.egoage8 <- nrow(Motherage4.8)
  nmotherage5.egoage8 <- nrow(Motherage5.8)
  nmotherage6.egoage8 <- nrow(Motherage6.8)
  nmotherage7.egoage8 <- nrow(Motherage7.8)
  nmotherage8.egoage8 <- nrow(Motherage8.8)
  nmotherage9.egoage8 <- nrow(Motherage9.8)
  nmotherage10.egoage8 <- nrow(Motherage10.8)
  nmotherage11.egoage8 <- nrow(Motherage11.8)
  nmotherage12.egoage8 <- nrow(Motherage12.8)
  nmotherage13.egoage8 <- nrow(Motherage13.8)
  nmotherage14.egoage8 <- nrow(Motherage14.8)
  
  motheragevector8<-c(nmotherage1.egoage8,nmotherage2.egoage8,nmotherage3.egoage8,nmotherage4.egoage8,nmotherage5.egoage8,
                      nmotherage6.egoage8,nmotherage7.egoage8,nmotherage8.egoage8,nmotherage9.egoage8,nmotherage10.egoage8,
                      nmotherage11.egoage8,nmotherage12.egoage8,nmotherage13.egoage8,nmotherage14.egoage8)
  Mother_aliveness_by_age8 <- motheragevector8/negoage8
  Mother_aliveness_by_age8
  sum(Mother_aliveness_by_age8)
  
  PrAlive
  
  
  #Ego age 9
  Egoage9 <- dataF[dataF$age==9,] 
  negoage9 <-nrow(Egoage9)
  Egoage9.MID <- Egoage9[!is.na(Egoage9$motherID),]
  Mothers.Alive.ID9 <- dataF[which(dataF$who %in% Egoage9.MID$motherID),]
  Motherage1.9<- Mothers.Alive.ID9[which(Mothers.Alive.ID9$age==1),]
  Motherage2.9<- Mothers.Alive.ID9[which(Mothers.Alive.ID9$age==2),]
  Motherage3.9<- Mothers.Alive.ID9[which(Mothers.Alive.ID9$age==3),]
  Motherage4.9<- Mothers.Alive.ID9[which(Mothers.Alive.ID9$age==4),]
  Motherage5.9<- Mothers.Alive.ID9[which(Mothers.Alive.ID9$age==5),]
  Motherage6.9<- Mothers.Alive.ID9[which(Mothers.Alive.ID9$age==6),]
  Motherage7.9<- Mothers.Alive.ID9[which(Mothers.Alive.ID9$age==7),]
  Motherage8.9<- Mothers.Alive.ID9[which(Mothers.Alive.ID9$age==8),]
  Motherage9.9<- Mothers.Alive.ID9[which(Mothers.Alive.ID9$age==9),]
  Motherage10.9<- Mothers.Alive.ID9[which(Mothers.Alive.ID9$age==10),]
  Motherage11.9<- Mothers.Alive.ID9[which(Mothers.Alive.ID9$age==11),]
  Motherage12.9<- Mothers.Alive.ID9[which(Mothers.Alive.ID9$age==12),]
  Motherage13.9<- Mothers.Alive.ID9[which(Mothers.Alive.ID9$age==13),]
  Motherage14.9<- Mothers.Alive.ID9[which(Mothers.Alive.ID9$age==14),]
  nmotherage1.egoage9 <- nrow(Motherage1.9)
  nmotherage2.egoage9 <- nrow(Motherage2.9)
  nmotherage3.egoage9 <- nrow(Motherage3.9)
  nmotherage4.egoage9 <- nrow(Motherage4.9)
  nmotherage5.egoage9 <- nrow(Motherage5.9)
  nmotherage6.egoage9 <- nrow(Motherage6.9)
  nmotherage7.egoage9 <- nrow(Motherage7.9)
  nmotherage8.egoage9 <- nrow(Motherage8.9)
  nmotherage9.egoage9 <- nrow(Motherage9.9)
  nmotherage10.egoage9 <- nrow(Motherage10.9)
  nmotherage11.egoage9 <- nrow(Motherage11.9)
  nmotherage12.egoage9 <- nrow(Motherage12.9)
  nmotherage13.egoage9 <- nrow(Motherage13.9)
  nmotherage14.egoage9 <- nrow(Motherage14.9)
  
  motheragevector9<-c(nmotherage1.egoage9,nmotherage2.egoage9,nmotherage3.egoage9,nmotherage4.egoage9,nmotherage5.egoage9,
                      nmotherage6.egoage9,nmotherage7.egoage9,nmotherage8.egoage9,nmotherage9.egoage9,nmotherage10.egoage9,
                      nmotherage11.egoage9,nmotherage12.egoage9,nmotherage13.egoage9,nmotherage14.egoage9)
  Mother_aliveness_by_age9 <- motheragevector9/negoage9
  Mother_aliveness_by_age9
  sum(Mother_aliveness_by_age9)
  
  PrAlive
  
  
  #Ego age 10
  Egoage10 <- dataF[dataF$age==10,] 
  negoage10 <-nrow(Egoage10)
  Egoage10.MID <- Egoage10[!is.na(Egoage10$motherID),]
  Mothers.Alive.ID10 <- dataF[which(dataF$who %in% Egoage10.MID$motherID),]
  Motherage1.10<- Mothers.Alive.ID10[which(Mothers.Alive.ID10$age==1),]
  Motherage2.10<- Mothers.Alive.ID10[which(Mothers.Alive.ID10$age==2),]
  Motherage3.10<- Mothers.Alive.ID10[which(Mothers.Alive.ID10$age==3),]
  Motherage4.10<- Mothers.Alive.ID10[which(Mothers.Alive.ID10$age==4),]
  Motherage5.10<- Mothers.Alive.ID10[which(Mothers.Alive.ID10$age==5),]
  Motherage6.10<- Mothers.Alive.ID10[which(Mothers.Alive.ID10$age==6),]
  Motherage7.10<- Mothers.Alive.ID10[which(Mothers.Alive.ID10$age==7),]
  Motherage8.10<- Mothers.Alive.ID10[which(Mothers.Alive.ID10$age==8),]
  Motherage9.10<- Mothers.Alive.ID10[which(Mothers.Alive.ID10$age==9),]
  Motherage10.10<- Mothers.Alive.ID10[which(Mothers.Alive.ID10$age==10),]
  Motherage11.10<- Mothers.Alive.ID10[which(Mothers.Alive.ID10$age==11),]
  Motherage12.10<- Mothers.Alive.ID10[which(Mothers.Alive.ID10$age==12),]
  Motherage13.10<- Mothers.Alive.ID10[which(Mothers.Alive.ID10$age==13),]
  Motherage14.10<- Mothers.Alive.ID10[which(Mothers.Alive.ID10$age==14),]
  nmotherage1.egoage10 <- nrow(Motherage1.10)
  nmotherage2.egoage10 <- nrow(Motherage2.10)
  nmotherage3.egoage10 <- nrow(Motherage3.10)
  nmotherage4.egoage10 <- nrow(Motherage4.10)
  nmotherage5.egoage10 <- nrow(Motherage5.10)
  nmotherage6.egoage10 <- nrow(Motherage6.10)
  nmotherage7.egoage10 <- nrow(Motherage7.10)
  nmotherage8.egoage10 <- nrow(Motherage8.10)
  nmotherage9.egoage10 <- nrow(Motherage9.10)
  nmotherage10.egoage10 <- nrow(Motherage10.10)
  nmotherage11.egoage10 <- nrow(Motherage11.10)
  nmotherage12.egoage10 <- nrow(Motherage12.10)
  nmotherage13.egoage10 <- nrow(Motherage13.10)
  nmotherage14.egoage10 <- nrow(Motherage14.10)
  
  motheragevector10<-c(nmotherage1.egoage10,nmotherage2.egoage10,nmotherage3.egoage10,nmotherage4.egoage10,nmotherage5.egoage10,
                      nmotherage6.egoage10,nmotherage7.egoage10,nmotherage8.egoage10,nmotherage9.egoage10,nmotherage10.egoage10,
                      nmotherage11.egoage10,nmotherage12.egoage10,nmotherage13.egoage10,nmotherage14.egoage10)
  Mother_aliveness_by_age10 <- motheragevector10/negoage10
  Mother_aliveness_by_age10
  sum(Mother_aliveness_by_age10)
  
  PrAlive
  

  #Ego age 11
  Egoage11 <- dataF[dataF$age==11,] 
  negoage11 <-nrow(Egoage11)
  Egoage11.MID <- Egoage11[!is.na(Egoage11$motherID),]
  Mothers.Alive.ID11 <- dataF[which(dataF$who %in% Egoage11.MID$motherID),]
  Motherage1.11<- Mothers.Alive.ID11[which(Mothers.Alive.ID11$age==1),]
  Motherage2.11<- Mothers.Alive.ID11[which(Mothers.Alive.ID11$age==2),]
  Motherage3.11<- Mothers.Alive.ID11[which(Mothers.Alive.ID11$age==3),]
  Motherage4.11<- Mothers.Alive.ID11[which(Mothers.Alive.ID11$age==4),]
  Motherage5.11<- Mothers.Alive.ID11[which(Mothers.Alive.ID11$age==5),]
  Motherage6.11<- Mothers.Alive.ID11[which(Mothers.Alive.ID11$age==6),]
  Motherage7.11<- Mothers.Alive.ID11[which(Mothers.Alive.ID11$age==7),]
  Motherage8.11<- Mothers.Alive.ID11[which(Mothers.Alive.ID11$age==8),]
  Motherage9.11<- Mothers.Alive.ID11[which(Mothers.Alive.ID11$age==9),]
  Motherage10.11<- Mothers.Alive.ID11[which(Mothers.Alive.ID11$age==10),]
  Motherage11.11<- Mothers.Alive.ID11[which(Mothers.Alive.ID11$age==11),]
  Motherage12.11<- Mothers.Alive.ID11[which(Mothers.Alive.ID11$age==12),]
  Motherage13.11<- Mothers.Alive.ID11[which(Mothers.Alive.ID11$age==13),]
  Motherage14.11<- Mothers.Alive.ID11[which(Mothers.Alive.ID11$age==14),]
  nmotherage1.egoage11 <- nrow(Motherage1.11)
  nmotherage2.egoage11 <- nrow(Motherage2.11)
  nmotherage3.egoage11 <- nrow(Motherage3.11)
  nmotherage4.egoage11 <- nrow(Motherage4.11)
  nmotherage5.egoage11 <- nrow(Motherage5.11)
  nmotherage6.egoage11 <- nrow(Motherage6.11)
  nmotherage7.egoage11 <- nrow(Motherage7.11)
  nmotherage8.egoage11 <- nrow(Motherage8.11)
  nmotherage9.egoage11 <- nrow(Motherage9.11)
  nmotherage10.egoage11 <- nrow(Motherage10.11)
  nmotherage11.egoage11 <- nrow(Motherage11.11)
  nmotherage12.egoage11 <- nrow(Motherage12.11)
  nmotherage13.egoage11 <- nrow(Motherage13.11)
  nmotherage14.egoage11 <- nrow(Motherage14.11)
  
  motheragevector11<-c(nmotherage1.egoage11,nmotherage2.egoage11,nmotherage3.egoage11,nmotherage4.egoage11,nmotherage5.egoage11,
                       nmotherage6.egoage11,nmotherage7.egoage11,nmotherage8.egoage11,nmotherage9.egoage11,nmotherage10.egoage11,
                       nmotherage11.egoage11,nmotherage12.egoage11,nmotherage13.egoage11,nmotherage14.egoage11)
  Mother_aliveness_by_age11 <- motheragevector11/negoage11
  Mother_aliveness_by_age11
  sum(Mother_aliveness_by_age11)
  
  PrAlive
  
  
  #Ego age 12
  Egoage12 <- dataF[dataF$age==12,] 
  negoage12 <-nrow(Egoage12)
  Egoage12.MID <- Egoage12[!is.na(Egoage12$motherID),]
  Mothers.Alive.ID12 <- dataF[which(dataF$who %in% Egoage12.MID$motherID),]
  Motherage1.12<- Mothers.Alive.ID12[which(Mothers.Alive.ID12$age==1),]
  Motherage2.12<- Mothers.Alive.ID12[which(Mothers.Alive.ID12$age==2),]
  Motherage3.12<- Mothers.Alive.ID12[which(Mothers.Alive.ID12$age==3),]
  Motherage4.12<- Mothers.Alive.ID12[which(Mothers.Alive.ID12$age==4),]
  Motherage5.12<- Mothers.Alive.ID12[which(Mothers.Alive.ID12$age==5),]
  Motherage6.12<- Mothers.Alive.ID12[which(Mothers.Alive.ID12$age==6),]
  Motherage7.12<- Mothers.Alive.ID12[which(Mothers.Alive.ID12$age==7),]
  Motherage8.12<- Mothers.Alive.ID12[which(Mothers.Alive.ID12$age==8),]
  Motherage9.12<- Mothers.Alive.ID12[which(Mothers.Alive.ID12$age==9),]
  Motherage10.12<- Mothers.Alive.ID12[which(Mothers.Alive.ID12$age==10),]
  Motherage11.12<- Mothers.Alive.ID12[which(Mothers.Alive.ID12$age==11),]
  Motherage12.12<- Mothers.Alive.ID12[which(Mothers.Alive.ID12$age==12),]
  Motherage13.12<- Mothers.Alive.ID12[which(Mothers.Alive.ID12$age==13),]
  Motherage14.12<- Mothers.Alive.ID12[which(Mothers.Alive.ID12$age==14),]
  nmotherage1.egoage12 <- nrow(Motherage1.12)
  nmotherage2.egoage12 <- nrow(Motherage2.12)
  nmotherage3.egoage12 <- nrow(Motherage3.12)
  nmotherage4.egoage12 <- nrow(Motherage4.12)
  nmotherage5.egoage12 <- nrow(Motherage5.12)
  nmotherage6.egoage12 <- nrow(Motherage6.12)
  nmotherage7.egoage12 <- nrow(Motherage7.12)
  nmotherage8.egoage12 <- nrow(Motherage8.12)
  nmotherage9.egoage12 <- nrow(Motherage9.12)
  nmotherage10.egoage12 <- nrow(Motherage10.12)
  nmotherage11.egoage12 <- nrow(Motherage11.12)
  nmotherage12.egoage12 <- nrow(Motherage12.12)
  nmotherage13.egoage12 <- nrow(Motherage13.12)
  nmotherage14.egoage12 <- nrow(Motherage14.12)
  
  motheragevector12<-c(nmotherage1.egoage12,nmotherage2.egoage12,nmotherage3.egoage12,nmotherage4.egoage12,nmotherage5.egoage12,
                       nmotherage6.egoage12,nmotherage7.egoage12,nmotherage8.egoage12,nmotherage9.egoage12,nmotherage10.egoage12,
                       nmotherage11.egoage12,nmotherage12.egoage12,nmotherage13.egoage12,nmotherage14.egoage12)
  Mother_aliveness_by_age12 <- motheragevector12/negoage12
  Mother_aliveness_by_age12
  sum(Mother_aliveness_by_age12)
  
  PrAlive
  
  #Ego age 13
  Egoage13 <- dataF[dataF$age==13,] 
  negoage13 <-nrow(Egoage13)
  Egoage13.MID <- Egoage13[!is.na(Egoage13$motherID),]
  Mothers.Alive.ID13 <- dataF[which(dataF$who %in% Egoage13.MID$motherID),]
  Motherage1.13<- Mothers.Alive.ID13[which(Mothers.Alive.ID13$age==1),]
  Motherage2.13<- Mothers.Alive.ID13[which(Mothers.Alive.ID13$age==2),]
  Motherage3.13<- Mothers.Alive.ID13[which(Mothers.Alive.ID13$age==3),]
  Motherage4.13<- Mothers.Alive.ID13[which(Mothers.Alive.ID13$age==4),]
  Motherage5.13<- Mothers.Alive.ID13[which(Mothers.Alive.ID13$age==5),]
  Motherage6.13<- Mothers.Alive.ID13[which(Mothers.Alive.ID13$age==6),]
  Motherage7.13<- Mothers.Alive.ID13[which(Mothers.Alive.ID13$age==7),]
  Motherage8.13<- Mothers.Alive.ID13[which(Mothers.Alive.ID13$age==8),]
  Motherage9.13<- Mothers.Alive.ID13[which(Mothers.Alive.ID13$age==9),]
  Motherage10.13<- Mothers.Alive.ID13[which(Mothers.Alive.ID13$age==10),]
  Motherage11.13<- Mothers.Alive.ID13[which(Mothers.Alive.ID13$age==11),]
  Motherage12.13<- Mothers.Alive.ID13[which(Mothers.Alive.ID13$age==12),]
  Motherage13.13<- Mothers.Alive.ID13[which(Mothers.Alive.ID13$age==13),]
  Motherage14.13<- Mothers.Alive.ID13[which(Mothers.Alive.ID13$age==14),]
  nmotherage1.egoage13 <- nrow(Motherage1.13)
  nmotherage2.egoage13 <- nrow(Motherage2.13)
  nmotherage3.egoage13 <- nrow(Motherage3.13)
  nmotherage4.egoage13 <- nrow(Motherage4.13)
  nmotherage5.egoage13 <- nrow(Motherage5.13)
  nmotherage6.egoage13 <- nrow(Motherage6.13)
  nmotherage7.egoage13 <- nrow(Motherage7.13)
  nmotherage8.egoage13 <- nrow(Motherage8.13)
  nmotherage9.egoage13 <- nrow(Motherage9.13)
  nmotherage10.egoage13 <- nrow(Motherage10.13)
  nmotherage11.egoage13 <- nrow(Motherage11.13)
  nmotherage12.egoage13 <- nrow(Motherage12.13)
  nmotherage13.egoage13 <- nrow(Motherage13.13)
  nmotherage14.egoage13 <- nrow(Motherage14.13)
  
  motheragevector13<-c(nmotherage1.egoage13,nmotherage2.egoage13,nmotherage3.egoage13,nmotherage4.egoage13,nmotherage5.egoage13,
                       nmotherage6.egoage13,nmotherage7.egoage13,nmotherage8.egoage13,nmotherage9.egoage13,nmotherage10.egoage13,
                       nmotherage11.egoage13,nmotherage12.egoage13,nmotherage13.egoage13,nmotherage14.egoage13)
  Mother_aliveness_by_age13 <- motheragevector13/negoage13
  Mother_aliveness_by_age13
  sum(Mother_aliveness_by_age13)
  
  PrAlive

  
  #Ego age 14
  Egoage14 <- dataF[dataF$age==14,] 
  negoage14 <-nrow(Egoage14)
  Egoage14.MID <- Egoage14[!is.na(Egoage14$motherID),]
  Mothers.Alive.ID14 <- dataF[which(dataF$who %in% Egoage14.MID$motherID),]
  Motherage1.14<- Mothers.Alive.ID14[which(Mothers.Alive.ID14$age==1),]
  Motherage2.14<- Mothers.Alive.ID14[which(Mothers.Alive.ID14$age==2),]
  Motherage3.14<- Mothers.Alive.ID14[which(Mothers.Alive.ID14$age==3),]
  Motherage4.14<- Mothers.Alive.ID14[which(Mothers.Alive.ID14$age==4),]
  Motherage5.14<- Mothers.Alive.ID14[which(Mothers.Alive.ID14$age==5),]
  Motherage6.14<- Mothers.Alive.ID14[which(Mothers.Alive.ID14$age==6),]
  Motherage7.14<- Mothers.Alive.ID14[which(Mothers.Alive.ID14$age==7),]
  Motherage8.14<- Mothers.Alive.ID14[which(Mothers.Alive.ID14$age==8),]
  Motherage9.14<- Mothers.Alive.ID14[which(Mothers.Alive.ID14$age==9),]
  Motherage10.14<- Mothers.Alive.ID14[which(Mothers.Alive.ID14$age==10),]
  Motherage11.14<- Mothers.Alive.ID14[which(Mothers.Alive.ID14$age==11),]
  Motherage12.14<- Mothers.Alive.ID14[which(Mothers.Alive.ID14$age==12),]
  Motherage13.14<- Mothers.Alive.ID14[which(Mothers.Alive.ID14$age==13),]
  Motherage14.14<- Mothers.Alive.ID14[which(Mothers.Alive.ID14$age==14),]
  nmotherage1.egoage14 <- nrow(Motherage1.14)
  nmotherage2.egoage14 <- nrow(Motherage2.14)
  nmotherage3.egoage14 <- nrow(Motherage3.14)
  nmotherage4.egoage14 <- nrow(Motherage4.14)
  nmotherage5.egoage14 <- nrow(Motherage5.14)
  nmotherage6.egoage14 <- nrow(Motherage6.14)
  nmotherage7.egoage14 <- nrow(Motherage7.14)
  nmotherage8.egoage14 <- nrow(Motherage8.14)
  nmotherage9.egoage14 <- nrow(Motherage9.14)
  nmotherage10.egoage14 <- nrow(Motherage10.14)
  nmotherage11.egoage14 <- nrow(Motherage11.14)
  nmotherage12.egoage14 <- nrow(Motherage12.14)
  nmotherage13.egoage14 <- nrow(Motherage13.14)
  nmotherage14.egoage14 <- nrow(Motherage14.14)
  
  motheragevector14<-c(nmotherage1.egoage14,nmotherage2.egoage14,nmotherage3.egoage14,nmotherage4.egoage14,nmotherage5.egoage14,
                       nmotherage6.egoage14,nmotherage7.egoage14,nmotherage8.egoage14,nmotherage9.egoage14,nmotherage10.egoage14,
                       nmotherage11.egoage14,nmotherage12.egoage14,nmotherage13.egoage14,nmotherage14.egoage14)
  Mother_aliveness_by_age14 <- motheragevector14/negoage14
  Mother_aliveness_by_age14
  sum(Mother_aliveness_by_age14)
  
  PrAlive
  
  #FINAL MATRIX
  final.vector<-c(Mother_aliveness_by_age1,Mother_aliveness_by_age2,Mother_aliveness_by_age3,Mother_aliveness_by_age4,
                  Mother_aliveness_by_age5,Mother_aliveness_by_age6,Mother_aliveness_by_age7,Mother_aliveness_by_age8,
                  Mother_aliveness_by_age9,Mother_aliveness_by_age10,Mother_aliveness_by_age11,Mother_aliveness_by_age12,
                  Mother_aliveness_by_age13,Mother_aliveness_by_age14)
  
  Colnames<-c(1:14)
  Rownames<-c(1:14)
  final.kinship.matrix<-matrix(final.vector,nrow=14,ncol=14,byrow=TRUE)
  

  
####### LOOPS

  #LOOP WITH ONE VARIABLE
  for (i in 1:14){
    Egoagei <- dataF[dataF$age==i,]
    negoagei <- nrow(Egoagei)
    Egoagei.MID <- Egoagei[!is.na(Egoagei$motherID),]
    Mothers.Alive.ID <- dataF[which(dataF$who %in% Egoagei.MID$motherID),]
    Motherage1<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==1),]
    Motherage2<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==2),]
    Motherage3<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==3),]
    Motherage4<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==4),]
    Motherage5<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==5),]
    Motherage6<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==6),]
    Motherage7<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==7),]
    Motherage8<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==8),]
    Motherage9<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==9),]
    Motherage10<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==10),]
    Motherage11<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==11),]
    Motherage12<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==12),]
    Motherage13<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==13),]
    Motherage14<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==14),]
    nmotherage1.egoagei <- nrow(Motherage1)
    nmotherage2.egoagei <- nrow(Motherage2)
    nmotherage3.egoagei <- nrow(Motherage3)
    nmotherage4.egoagei <- nrow(Motherage4)
    nmotherage5.egoagei <- nrow(Motherage5)
    nmotherage6.egoagei <- nrow(Motherage6)
    nmotherage7.egoagei <- nrow(Motherage7)
    nmotherage8.egoagei <- nrow(Motherage8)
    nmotherage9.egoagei <- nrow(Motherage9)
    nmotherage10.egoagei <- nrow(Motherage10)
    nmotherage11.egoagei <- nrow(Motherage11)
    nmotherage12.egoagei <- nrow(Motherage12)
    nmotherage13.egoagei <- nrow(Motherage13)
    nmotherage14.egoagei <- nrow(Motherage14)
    mother.vector<-c(nmotherage1.egoage1,nmotherage2.egoage1,nmotherage3.egoage1,nmotherage4.egoage1,nmotherage5.egoage1,
                     nmotherage6.egoage1,nmotherage7.egoage1,nmotherage8.egoage1,nmotherage9.egoage1,nmotherage10.egoage1,
                     nmotherage11.egoage1,nmotherage12.egoage1,nmotherage13.egoage1,nmotherage14.egoage1)
    Mother_aliveness_by_age <- mother.vector/negoagei
    Mother_aliveness_by_age
    
  }
  
  
  #LOOP WITH TWO VARIABLES
  library("foreach")
  egoage=c(rep(1:14,each = 14))
  kinage=c(rep(1:14,14))
  kinship.matrix <-data.frame(Ego_age =egoage, Mother_age=kinage, Aliveness.prob = rep(NA,length(egoage)), n =rep(NA,length(egoage)))
  
  foreach(i = egoage, j = kinage) %do% {
    Egoagei <- dataF[dataF$age==i,] 
    negoagei <- nrow(Egoagei)
    Egoagei.MID <- Egoagei[!is.na(Egoagei$motherID),]
    Mothers.Alive.ID <- dataF[which(dataF$who %in% Egoagei.MID$motherID),]
    Motheragej<- Mothers.Alive.ID[which(Mothers.Alive.ID$age==j),]
    nmotheragej.egoagei <- nrow(Motheragej)
    mother.vector<-
      if (nmotheragej.egoagei == 0){
        kinship.matrix[i,3] <-NA
      } else if (nmotheragej.egoagei>0){
        kinship.matrix[i,3] <- nmotheragej.egoagei/negoagei
      }
  }
  
  
  kinship.matrix
  
###################################################################################################################
##   KINSHIP MATRIX FOR GRANDMOTHERS   ############################################################################
###################################################################################################################

  #Ego age 1 
  Egoage1 <- dataF[dataF$age==1,] #All individuals of age 1 alive at last timestep
  negoage1 <-nrow(Egoage1) #number of individuals aged 1
  Egoage1.MID <- Egoage1[!is.na(Egoage1$motherID),] # 1 year olds with mother ID
  IDMothersDeadOrAlive <- unique(dataall[which(dataall$who %in% Egoage1.MID$motherID),'who']) #ID of 1 year olds mothers, regardless of alive/dead status
  IDGMDeadOrAlive <- unique(dataall[which(dataall$who %in% IDMothersDeadOrAlive),'motherID'])
  AliveGMData.egoage1<-dataF[which(dataF$who %in% IDGMDeadOrAlive),]
  GMAge1<- AliveGMData.egoage1[which(AliveGMData.egoage1$age==1),]
  GMAge2<- AliveGMData.egoage1[which(AliveGMData.egoage1$age==2),]
  GMAge3<- AliveGMData.egoage1[which(AliveGMData.egoage1$age==3),]
  GMAge4<- AliveGMData.egoage1[which(AliveGMData.egoage1$age==4),]
  GMAge5<- AliveGMData.egoage1[which(AliveGMData.egoage1$age==5),]
  GMAge6<- AliveGMData.egoage1[which(AliveGMData.egoage1$age==6),]
  GMAge7<- AliveGMData.egoage1[which(AliveGMData.egoage1$age==7),]
  GMAge8<- AliveGMData.egoage1[which(AliveGMData.egoage1$age==8),]
  GMAge9<- AliveGMData.egoage1[which(AliveGMData.egoage1$age==9),]
  GMAge10<- AliveGMData.egoage1[which(AliveGMData.egoage1$age==10),]
  GMAge11<- AliveGMData.egoage1[which(AliveGMData.egoage1$age==11),]
  GMAge12<- AliveGMData.egoage1[which(AliveGMData.egoage1$age==12),]
  GMAge13<- AliveGMData.egoage1[which(AliveGMData.egoage1$age==13),]
  GMAge14<- AliveGMData.egoage1[which(AliveGMData.egoage1$age==14),]
  nGMage1.egoage1 <- nrow(GMAge1)
  nGMage2.egoage1 <- nrow(GMAge2)
  nGMage3.egoage1 <- nrow(GMAge3)
  nGMage4.egoage1 <- nrow(GMAge4)
  nGMage5.egoage1 <- nrow(GMAge5)
  nGMage6.egoage1 <- nrow(GMAge6)
  nGMage7.egoage1 <- nrow(GMAge7)
  nGMage8.egoage1 <- nrow(GMAge8)
  nGMage9.egoage1 <- nrow(GMAge9)
  nGMage10.egoage1 <- nrow(GMAge10)
  nGMage11.egoage1 <- nrow(GMAge11)
  nGMage12.egoage1 <- nrow(GMAge12)
  nGMage13.egoage1 <- nrow(GMAge13)
  nGMage14.egoage1 <- nrow(GMAge14)
  GMagevector<-c(nGMage1.egoage1,nGMage2.egoage1,nGMage3.egoage1,nGMage4.egoage1,nGMage5.egoage1,
                     nGMage6.egoage1,nGMage7.egoage1,nGMage8.egoage1,nGMage9.egoage1,nGMage10.egoage1,
                     nGMage11.egoage1,nGMage12.egoage1,nGMage13.egoage1,nGMage14.egoage1)
  GM_aliveness_by_age1 <- GMagevector/negoage1
  GM_aliveness_by_age1
  sum(GM_aliveness_by_age1) #check: it is the same as the total probability of a female of age 1 having her grandmother alive
 
  PrAlive
  
  
  #Ego age 2
  Egoage2 <- dataF[dataF$age==2,] #All individuals of age 2 alive at last timestep
  negoage2 <-nrow(Egoage2) #number of individuals aged 2
  Egoage2.MID <- Egoage2[!is.na(Egoage2$motherID),] # 2 year olds with mother ID
  IDMothersDeadOrAlive2 <- unique(dataall[which(dataall$who %in% Egoage2.MID$motherID),'who']) #ID of 1 year olds mothers, regardless of alive/dead status
  IDGMDeadOrAlive2 <- unique(dataall[which(dataall$who %in% IDMothersDeadOrAlive2),'motherID'])
  AliveGMData.egoage2<-dataF[which(dataF$who %in% IDGMDeadOrAlive2),]
  GMAge1.2<- AliveGMData.egoage2[which(AliveGMData.egoage2$age==1),]
  GMAge2.2<- AliveGMData.egoage2[which(AliveGMData.egoage2$age==2),]
  GMAge3.2<- AliveGMData.egoage2[which(AliveGMData.egoage2$age==3),]
  GMAge4.2<- AliveGMData.egoage2[which(AliveGMData.egoage2$age==4),]
  GMAge5.2<- AliveGMData.egoage2[which(AliveGMData.egoage2$age==5),]
  GMAge6.2<- AliveGMData.egoage2[which(AliveGMData.egoage2$age==6),]
  GMAge7.2<- AliveGMData.egoage2[which(AliveGMData.egoage2$age==7),]
  GMAge8.2<- AliveGMData.egoage2[which(AliveGMData.egoage2$age==8),]
  GMAge9.2<- AliveGMData.egoage2[which(AliveGMData.egoage2$age==9),]
  GMAge10.2<- AliveGMData.egoage2[which(AliveGMData.egoage2$age==10),]
  GMAge11.2<- AliveGMData.egoage2[which(AliveGMData.egoage2$age==11),]
  GMAge12.2<- AliveGMData.egoage2[which(AliveGMData.egoage2$age==12),]
  GMAge13.2<- AliveGMData.egoage2[which(AliveGMData.egoage2$age==13),]
  GMAge14.2<- AliveGMData.egoage2[which(AliveGMData.egoage2$age==14),]
  nGMage1.egoage2 <- nrow(GMAge1.2)
  nGMage2.egoage2 <- nrow(GMAge2.2)
  nGMage3.egoage2 <- nrow(GMAge3.2)
  nGMage4.egoage2 <- nrow(GMAge4.2)
  nGMage5.egoage2 <- nrow(GMAge5.2)
  nGMage6.egoage2 <- nrow(GMAge6.2)
  nGMage7.egoage2 <- nrow(GMAge7.2)
  nGMage8.egoage2 <- nrow(GMAge8.2)
  nGMage9.egoage2 <- nrow(GMAge9.2)
  nGMage10.egoage2 <- nrow(GMAge10.2)
  nGMage11.egoage2 <- nrow(GMAge11.2)
  nGMage12.egoage2 <- nrow(GMAge12.2)
  nGMage13.egoage2 <- nrow(GMAge13.2)
  nGMage14.egoage2 <- nrow(GMAge14.2)
  GMagevector2<-c(nGMage1.egoage2,nGMage2.egoage2,nGMage3.egoage2,nGMage4.egoage2,nGMage5.egoage2,
                 nGMage6.egoage2,nGMage7.egoage2,nGMage8.egoage2,nGMage9.egoage2,nGMage10.egoage2,
                 nGMage11.egoage2,nGMage12.egoage2,nGMage13.egoage2,nGMage14.egoage2)
  GM_aliveness_by_age2 <- GMagevector2/negoage2
  GM_aliveness_by_age2
  sum(GM_aliveness_by_age2) #check: it is the same as the total probability of a female of age 2 having her grandmother alive
  
  PrAlive
  
  
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
  
  #Average number of sisters in every other age class for an individual of age 1
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

  
  
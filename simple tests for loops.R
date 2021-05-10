<<<<<<< HEAD

#Loop Tests 

ages.LT<-1:14

# Population with two grandmothers (IDs 25 and 26), each with three daughters (19,20,21 & 22,23,24, respectively)
# Each grandmother daughters has 3 daughters:
#1,2,3  4,5,6  7,8,9  are ID25's grand-daughters
#10,11,12  13,14,15  16,17,18  are ID26's grand-daughters

LT.AllFem <- data.frame(
  who = 1:26, # 26 individuals
  sex = c(rep("F",26)), #all females
  age = c(2,3,4,1,2,3,2,2,3,4,5,5,2,1,1,3,3,3,6,7,8,9,10,11,13,14),
  alpha = c(rep(NA,26)), 
  packID = c(rep(NA,26)),
  disp = c(rep(NA,26)),
  dism = c(rep(NA,26)),
  motherID = c(rep(19,3),rep(20,3),rep(21,3),rep(22,3),rep(23,3),rep(24,3),rep(25,3),rep(26,3),27,28),
  fatherID = c(rep(NA,26)),
  cohort = c(rep(NA,26))
)

# ID of all mothers and grandmothers ever lived in pop
IDMothers.LT <- unique(LT.AllFem[,'motherID'] ) # mothers of at least one female
IDGrandMothers.LT <- unique(LT.AllFem[which(LT.AllFem$who %in% IDMothers.LT),'motherID'] )# grand mother of at least one female

#To keep all individuals alive in the last time step
LT.datalastF <- data.frame(
  who = 1:26, # 26 individuals
  sex = c(rep("F",26)), #all females
  age = c(2,3,4,1,2,3,2,2,3,4,5,5,2,1,1,3,3,3,6,7,8,9,10,11,13,14),
  alpha = c(rep(NA,26)), 
  packID = c(rep(NA,26)),
  disp = c(rep(NA,26)),
  dism = c(rep(NA,26)),
  motherID = c(rep(19,3),rep(20,3),rep(21,3),rep(22,3),rep(23,3),rep(24,3),rep(25,3),rep(26,3),27,28),
  fatherID = c(rep(NA,26)),
  cohort = c(rep(NA,26))
)

#Picking individual 14, a 1-year-old, for tests:

#To Kill its grandmother (14 years old):
#LT.datalastF <- LT.AllFem[-26,]     #CHECKED

#To kill its mother (10 years old):
#LT.datalastF <- LT.AllFem[-23,]    ##CHECKED

#To kill one aunt (9 years old)
#LT.datalastF <- LT.AllFem[-22,]    ##CHECKED

#To kill a different cohort sibling (2 years old)
#LT.datalastF <- LT.AllFem[-13,]                      ##CHECKED

#To kill a same cohort sibling (1 year old)
#LT.datalastF <- LT.AllFem[-15,]                      ##CHECKED

#To kill a cousin (3 years old)
#LT.datalastF <- LT.AllFem[-16,]                      ##CHECKED


# Mothers alive at last time step
IDMothersAlive.LT <- LT.datalastF$who[LT.datalastF$who %in% IDMothers.LT]

# Grand Mothers alive at last time step
IDGrandMothersAlive.LT <- LT.datalastF$who[LT.datalastF$who %in% IDGrandMothers.LT]



#Mother & Grandmother
nb_MotherAlive.LT <- kinship_Mother.LT <- matrix(NA, nrow=max(ages.LT),ncol=max(ages.LT))
nb_GrandmotherAlive.LT <- kinship_Grandmother.LT <- matrix(NA, nrow=max(ages.LT),ncol=max(ages.LT))

n_MA.LT <- n_GMA.LT <-Pr_MA_egoagei.LT <- Pr_GMA_egoagei.LT <-NULL
mums.LT <- list()

#Sisters
kinship_Sisters.LT <- matrix(NA, nrow=max(ages.LT),ncol=max(ages.LT))

#Aunts
kinship_Aunts.LT <- matrix(NA, nrow=max(ages.LT),ncol=max(ages.LT))

# Children
kinship_Children.LT <- matrix(NA, nrow=max(ages.LT),ncol=max(ages.LT))

# Grand Children
kinship_GrandChildren.LT <- matrix(NA, nrow=max(ages.LT),ncol=max(ages.LT))

# Cousins
kinship_Cousins.LT <- matrix(NA, nrow=max(ages.LT),ncol=max(ages.LT))

#Nieces
kinship_Nieces.LT <- matrix(NA, nrow=max(ages.LT),ncol=max(ages.LT))

#Summary of results
Res_summary.LT <- data.frame(ego.age=ages.LT,
                          nb.ego=c(rep(NA,length(ages.LT))),  # number of ego aged i at last time step
                          nb.ego.MA=c(rep(NA,length(ages.LT))),  # number of ego aged i with mother alive at last time step
                          Pr_MA =c(rep(NA,length(ages.LT))), # proba mother alive for ego age i
                          nb.ego.GMA=c(rep(NA,length(ages.LT))),  # number of ego aged i with mother alive at last time step
                          Pr_GMA =c(rep(NA,length(ages.LT))),
                          Avg_Sis = c(rep(NA,length(ages.LT))),
                          Avg_Aunts = c(rep(NA,length(ages.LT))),
                          Avg_Ch = c(rep(NA,length(ages.LT))),
                          Avg_GCh = c(rep(NA,length(ages.LT))),
                          Avg_Cou = c(rep(NA,length(ages.LT))),
                          Avg_Nie = c(rep(NA,length(ages.LT)))
)


for(i in ages.LT){  #loop over ego age
  ego_agei <- LT.datalastF[LT.datalastF$age==i,] # ego age i alive at last time step
  
  if(nrow(ego_agei)==0){
    nb_MotherAlive.LT[i,] <- rep(NA,length(ages.LT))
    nb_GrandmotherAlive.LT[i,] <-rep(NA,length(ages.LT))
    kinship_Mother.LT[i,] <- rep(NA, length(ages.LT))
    kinship_Grandmother.LT[i,] <- rep(NA, length(ages.LT))
    kinship_Sisters.LT[i,] <- rep(NA,length(ages.LT))
    kinship_Aunts.LT[i,] <- rep(NA,length(ages.LT))
    kinship_Children.LT[i,] <- rep(NA,length(ages.LT))
    kinship_Cousins.LT[i,] <- rep(NA,length(ages.LT))
    kinship_Nieces.LT[i,] <- rep(NA, length(ages.LT))
    
  }else if(nrow(ego_agei)>0){ 
    Mothers.ID.ego <- ego_agei$motherID # mothers ID of ego age i
    Mothers.Alive.ID <- ego_agei$motherID[which(Mothers.ID.ego %in% IDMothersAlive.LT)] # alive mothers ID of ego age i, repeated by number of daughters
    mums.LT[[i]] <- Mothers.Alive.ID
    n_MA.LT[i] <- length(Mothers.Alive.ID) # nb of ego age i with mother alive at last time step
    Pr_MA_egoagei.LT[i]  <- n_MA.LT[i]/ nrow(ego_agei)  # proba mother alive for ego age i
    
    GM.ID <- LT.AllFem$motherID[match(Mothers.ID.ego, LT.AllFem$who)] # GM ID of ego age i
    GM.Alive.ID <- GM.ID[GM.ID %in% IDGrandMothersAlive.LT]
    n_GMA.LT[i] <- length(GM.Alive.ID) # nb of ego age i with grandmother alive at last time step
    Pr_GMA_egoagei.LT[i]  <- n_GMA.LT[i]/ nrow(ego_agei)  # proba mother alive for ego age i
    
    # age of mother AND GRAND MOTHER if alive at last time step
    Mage <- LT.datalastF$age[match(Mothers.Alive.ID, LT.datalastF$who )] # match ID of mothers alive at last time step in datalast (which includes all females alive at last time step)
    GMage <- LT.datalastF$age[match(GM.Alive.ID, LT.datalastF$who )] # match ID of mothers alive at last time step in datalast (which includes all females alive at last time step)
    
    for(j in ages.LT){ # loop over ages
      ind_agej <- LT.datalastF[LT.datalastF$age==j,] #individuals of age j alive at last time step
      
      # FOR MOTHERS
      nb_MotherAlive.LT[i,j] <- sum(Mage==j) 
      kinship_Mother.LT[i,j] <- sum(Mage==j) / nrow(ego_agei) # kinship matrix Pr of mother alive
      # FOR GRAND MOTHERS
      nb_GrandmotherAlive.LT[i,j] <- sum(GMage==j) 
      kinship_Grandmother.LT[i,j] <- sum(GMage==j) / nrow(ego_agei) # kinship matrix Pr of mother alive
      
      
      GM.IDs.ind_agej <-LT.AllFem$motherID[match(ind_agej$motherID, LT.AllFem$who )] # GM ID of ind age j alive at last time step, the GM can can be dead or alive
      nb.aunts.agej.ego <- nb.sis.agej.ego <- nb.children.agej.ego <- nb.grandchildren.agej.ego <- nb.cousins.agej.ego <- nb.nieces.agej.ego <- NULL # empty object to store results
      
      for(e in 1:nrow(ego_agei)){ # loop over each ego aged i
        ego.GM.ID <- GM.ID[e]
       
        #For nieces
        DaugthersOfEgoMother <- LT.AllFem[which(LT.AllFem$motherID==ego_agei$motherID[e]),] #Get all the daughters of ego's mother
        AllSistersDeadorAlive <- DaugthersOfEgoMother[-which(DaugthersOfEgoMother$who==ego_agei$who[e]),] #get only ego's sisters (eliminate ego)
        nb.nieces.agej.ego[e] <- sum(ind_agej$motherID %in% AllSistersDeadorAlive$who)
        
        nb.children.agej.ego[e] <- sum(ind_agej$motherID %in% ego_agei$who[e]) # nb of children of ego e that are age j
        nb.grandchildren.agej.ego[e] <- sum(GM.IDs.ind_agej %in% ego_agei$who[e]) # nb of grandchildren of ego e that are age j
        
        if (sum(is.na(ego.GM.ID))==1) {
          nb.aunts.agej.ego[e] <- 0
        } else if (sum(is.na(ego.GM.ID))==0) {
            
          if (sum(ind_agej$who==Mothers.ID.ego[e])==0){ # if ego's mother is dead
              nb.aunts.agej.ego[e] <- sum(ind_agej$motherID == GM.ID[e] )
          } else if (sum(ind_agej$who==Mothers.ID.ego[e])>0){ # if ego's mother is alive
            ind_agej_withoutegosmother <- ind_agej[-which(ind_agej$who==Mothers.ID.ego[e]),] # all indiv alive at last time step except from ego's mother
            nb.aunts.agej.ego[e] <- sum(ind_agej_withoutegosmother$motherID == GM.ID[e])
          }  
        }
        
        if (j==i){ #same cohort sisters
          nb.sis.agej.ego[e] <- sum(ind_agej$motherID==Mothers.ID.ego[e]) - 1
            if (sum(is.na(ego.GM.ID))==1) {
              nb.cousins.agej.ego[e] <- 0
            } else if (sum(is.na(ego.GM.ID))==0) {
          nb.cousins.agej.ego[e] <-sum(GM.IDs.ind_agej %in% ego.GM.ID) - nb.sis.agej.ego[e] - 1 # nb of cousins of ego e that are age j
        } 
          } else if (j!=i){ 
          nb.sis.agej.ego[e] <- sum(ind_agej$motherID==Mothers.ID.ego[e])
            if (sum(is.na(ego.GM.ID))==1) {
            nb.cousins.agej.ego[e] <- 0
          } else if (sum(is.na(ego.GM.ID))==0) {
          nb.cousins.agej.ego[e] <-sum(GM.IDs.ind_agej %in% ego.GM.ID) - nb.sis.agej.ego[e]
          }
          }
      } # end loop on ego e
      
      kinship_Sisters.LT[i,j] <- mean(nb.sis.agej.ego)
      kinship_Aunts.LT[i,j] <- mean(nb.aunts.agej.ego)
      kinship_Children.LT[i,j] <- mean(nb.children.agej.ego)
      kinship_GrandChildren.LT[i,j] <- mean(nb.grandchildren.agej.ego)   
      kinship_Cousins.LT[i,j] <-  mean(nb.cousins.agej.ego)
      kinship_Nieces.LT[i,j] <- mean(nb.nieces.agej.ego)
    } # end loop on sister's and aunt's ages
  } # end else if
  
  Res_summary.LT[i,"nb.ego"] <- nrow(ego_agei)
  Res_summary.LT[i,"nb.ego.MA"] <- n_MA.LT[i]
  Res_summary.LT[i,"Pr_MA"] <- Pr_MA_egoagei.LT[i]
  Res_summary.LT[i,"nb.ego.GMA"] <- n_GMA.LT[i]
  Res_summary.LT[i,"Pr_GMA"] <- Pr_GMA_egoagei.LT[i]
  Res_summary.LT[i,"Avg_Sis"] <- sum(kinship_Sisters.LT[i,])
  Res_summary.LT[i,"Avg_Aunts"] <- sum(kinship_Aunts.LT[i,])
  Res_summary.LT[i,"Avg_Ch"] <- sum(kinship_Children.LT[i,])
  Res_summary.LT[i,"Avg_GCh"] <- sum(kinship_GrandChildren.LT[i,])
  Res_summary.LT[i,"Avg_Cou"] <- sum(kinship_Cousins.LT[i,])
  Res_summary.LT[i,"Avg_Nie"] <- sum(kinship_Nieces.LT[i,])
} # end loop on ego's age

round(Res_summary.LT,2) # summary per ego age


# THERE WERE TWO ISSUES
#I
# For individuals who have NA as their grandmotherID or their motherID (such as dispersers, who can appear in AllFem, but not in datalastF)
# Their entire age class appears as having NA aunts in every other age class. 

#For example, changing the motherID of individual 24 to NA (making the grandmother of individuals 16,17,18 have a grandmother with ID NA)
#Individuals 16-18 are all 3-year-olds. Hence, this change leads to the average number of aunts for age 3 being NA, as well as all of its
# row in the kinship matrix. 
#Change in population to see:
#motherID = c(rep(19,3),rep(20,3),rep(21,3),rep(22,3),rep(23,3),rep(24,3),rep(25,3),rep(26,2),NA,27,28),
#                                                                                          ^ ^ ^ 

# II
# In the simulated population, the grandmothers have motherIDs, but their mothers don't
# hence the motherID of their mothers is taken as being NA, and it seems like, because both grandmothers
# have 'NA' as their grandmother ID, they are counted as each others cousins. 
# This could happen in the real IBM with sons of dispersers. 

#solved by:
#Adding lines 136-138, 150-152, 157-159 (all the same code):
# If an individual's grandmother ID is equal to NA, then it has 0 cousins and 0 aunts. Which seems reasonable, since the mother
# of this individual must have been a disperser and we don't expect her to have sisters (ego's aunts) in the population. It follows
# ego also wouldn't have cousins. 

# (III?) -> had to change line to get ego's grandmother ID, regardless of her being alive or dead.
#           The line works here, but not in the main script. The main script's line doesn't work here. 
#           maybe the change was because the data is in data frames here, and in AgentMatrix in the real model?
=======

#Loop Tests 

ages.LT<-1:14

# Population with two grandmothers (IDs 25 and 26), each with three daughters (19,20,21 & 22,23,24, respectively)
# Each grandmother daughters has 3 daughters:
#1,2,3  4,5,6  7,8,9  are ID25's grand-daughters
#10,11,12  13,14,15  16,17,18  are ID26's grand-daughters

LT.AllFem <- data.frame(
  who = 1:26, # 26 individuals
  sex = c(rep("F",26)), #all females
  age = c(2,3,4,1,2,3,2,2,3,4,5,5,2,1,1,3,3,3,6,7,8,9,10,11,13,14),
  alpha = c(rep(NA,26)), 
  packID = c(rep(NA,26)),
  disp = c(rep(NA,26)),
  dism = c(rep(NA,26)),
  motherID = c(rep(19,3),rep(20,3),rep(21,3),rep(22,3),rep(23,3),rep(24,3),rep(25,3),rep(26,3),27,28),
  fatherID = c(rep(NA,26)),
  cohort = c(rep(NA,26))
)

# ID of all mothers and grandmothers ever lived in pop
IDMothers.LT <- unique(LT.AllFem[,'motherID'] ) # mothers of at least one female
IDGrandMothers.LT <- unique(LT.AllFem[which(LT.AllFem$who %in% IDMothers.LT),'motherID'] )# grand mother of at least one female

#To keep all individuals alive in the last time step
LT.datalastF <- data.frame(
  who = 1:26, # 26 individuals
  sex = c(rep("F",26)), #all females
  age = c(2,3,4,1,2,3,2,2,3,4,5,5,2,1,1,3,3,3,6,7,8,9,10,11,13,14),
  alpha = c(rep(NA,26)), 
  packID = c(rep(NA,26)),
  disp = c(rep(NA,26)),
  dism = c(rep(NA,26)),
  motherID = c(rep(19,3),rep(20,3),rep(21,3),rep(22,3),rep(23,3),rep(24,3),rep(25,3),rep(26,3),27,28),
  fatherID = c(rep(NA,26)),
  cohort = c(rep(NA,26))
)

#Picking individual 14, a 1-year-old, for tests:

#To Kill its grandmother (14 years old):
#LT.datalastF <- LT.AllFem[-26,]     #CHECKED

#To kill its mother (10 years old):
#LT.datalastF <- LT.AllFem[-23,]    ##CHECKED

#To kill one aunt (9 years old)
#LT.datalastF <- LT.AllFem[-22,]    ##CHECKED

#To kill a different cohort sibling (2 years old)
#LT.datalastF <- LT.AllFem[-13,]                      ##CHECKED

#To kill a same cohort sibling (1 year old)
#LT.datalastF <- LT.AllFem[-15,]                      ##CHECKED

#To kill a cousin (3 years old)
#LT.datalastF <- LT.AllFem[-16,]                      ##CHECKED


# Mothers alive at last time step
IDMothersAlive.LT <- LT.datalastF$who[LT.datalastF$who %in% IDMothers.LT]

# Grand Mothers alive at last time step
IDGrandMothersAlive.LT <- LT.datalastF$who[LT.datalastF$who %in% IDGrandMothers.LT]



#Mother & Grandmother
nb_MotherAlive.LT <- kinship_Mother.LT <- matrix(NA, nrow=max(ages.LT),ncol=max(ages.LT))
nb_GrandmotherAlive.LT <- kinship_Grandmother.LT <- matrix(NA, nrow=max(ages.LT),ncol=max(ages.LT))

n_MA.LT <- n_GMA.LT <-Pr_MA_egoagei.LT <- Pr_GMA_egoagei.LT <-NULL
mums.LT <- list()

#Sisters
kinship_Sisters.LT <- matrix(NA, nrow=max(ages.LT),ncol=max(ages.LT))

#Aunts
kinship_Aunts.LT <- matrix(NA, nrow=max(ages.LT),ncol=max(ages.LT))

# Children
kinship_Children.LT <- matrix(NA, nrow=max(ages.LT),ncol=max(ages.LT))

# Grand Children
kinship_GrandChildren.LT <- matrix(NA, nrow=max(ages.LT),ncol=max(ages.LT))

# Cousins
kinship_Cousins.LT <- matrix(NA, nrow=max(ages.LT),ncol=max(ages.LT))

#Nieces
kinship_Nieces.LT <- matrix(NA, nrow=max(ages.LT),ncol=max(ages.LT))

#Summary of results
Res_summary.LT <- data.frame(ego.age=ages.LT,
                          nb.ego=c(rep(NA,length(ages.LT))),  # number of ego aged i at last time step
                          nb.ego.MA=c(rep(NA,length(ages.LT))),  # number of ego aged i with mother alive at last time step
                          Pr_MA =c(rep(NA,length(ages.LT))), # proba mother alive for ego age i
                          nb.ego.GMA=c(rep(NA,length(ages.LT))),  # number of ego aged i with mother alive at last time step
                          Pr_GMA =c(rep(NA,length(ages.LT))),
                          Avg_Sis = c(rep(NA,length(ages.LT))),
                          Avg_Aunts = c(rep(NA,length(ages.LT))),
                          Avg_Ch = c(rep(NA,length(ages.LT))),
                          Avg_GCh = c(rep(NA,length(ages.LT))),
                          Avg_Cou = c(rep(NA,length(ages.LT))),
                          Avg_Nie = c(rep(NA,length(ages.LT)))
)


for(i in ages.LT){  #loop over ego age
  ego_agei <- LT.datalastF[LT.datalastF$age==i,] # ego age i alive at last time step
  
  if(nrow(ego_agei)==0){
    nb_MotherAlive.LT[i,] <- rep(NA,length(ages.LT))
    nb_GrandmotherAlive.LT[i,] <-rep(NA,length(ages.LT))
    kinship_Mother.LT[i,] <- rep(NA, length(ages.LT))
    kinship_Grandmother.LT[i,] <- rep(NA, length(ages.LT))
    kinship_Sisters.LT[i,] <- rep(NA,length(ages.LT))
    kinship_Aunts.LT[i,] <- rep(NA,length(ages.LT))
    kinship_Children.LT[i,] <- rep(NA,length(ages.LT))
    kinship_Cousins.LT[i,] <- rep(NA,length(ages.LT))
    kinship_Nieces.LT[i,] <- rep(NA, length(ages.LT))
    
  }else if(nrow(ego_agei)>0){ 
    Mothers.ID.ego <- ego_agei$motherID # mothers ID of ego age i
    Mothers.Alive.ID <- ego_agei$motherID[which(Mothers.ID.ego %in% IDMothersAlive.LT)] # alive mothers ID of ego age i, repeated by number of daughters
    mums.LT[[i]] <- Mothers.Alive.ID
    n_MA.LT[i] <- length(Mothers.Alive.ID) # nb of ego age i with mother alive at last time step
    Pr_MA_egoagei.LT[i]  <- n_MA.LT[i]/ nrow(ego_agei)  # proba mother alive for ego age i
    
    GM.ID <- LT.AllFem$motherID[match(Mothers.ID.ego, LT.AllFem$who)] # GM ID of ego age i
    GM.Alive.ID <- GM.ID[GM.ID %in% IDGrandMothersAlive.LT]
    n_GMA.LT[i] <- length(GM.Alive.ID) # nb of ego age i with grandmother alive at last time step
    Pr_GMA_egoagei.LT[i]  <- n_GMA.LT[i]/ nrow(ego_agei)  # proba mother alive for ego age i
    
    # age of mother AND GRAND MOTHER if alive at last time step
    Mage <- LT.datalastF$age[match(Mothers.Alive.ID, LT.datalastF$who )] # match ID of mothers alive at last time step in datalast (which includes all females alive at last time step)
    GMage <- LT.datalastF$age[match(GM.Alive.ID, LT.datalastF$who )] # match ID of mothers alive at last time step in datalast (which includes all females alive at last time step)
    
    for(j in ages.LT){ # loop over ages
      ind_agej <- LT.datalastF[LT.datalastF$age==j,] #individuals of age j alive at last time step
      
      # FOR MOTHERS
      nb_MotherAlive.LT[i,j] <- sum(Mage==j) 
      kinship_Mother.LT[i,j] <- sum(Mage==j) / nrow(ego_agei) # kinship matrix Pr of mother alive
      # FOR GRAND MOTHERS
      nb_GrandmotherAlive.LT[i,j] <- sum(GMage==j) 
      kinship_Grandmother.LT[i,j] <- sum(GMage==j) / nrow(ego_agei) # kinship matrix Pr of mother alive
      
      
      GM.IDs.ind_agej <-LT.AllFem$motherID[match(ind_agej$motherID, LT.AllFem$who )] # GM ID of ind age j alive at last time step, the GM can can be dead or alive
      nb.aunts.agej.ego <- nb.sis.agej.ego <- nb.children.agej.ego <- nb.grandchildren.agej.ego <- nb.cousins.agej.ego <- nb.nieces.agej.ego <- NULL # empty object to store results
      
      for(e in 1:nrow(ego_agei)){ # loop over each ego aged i
        ego.GM.ID <- LT.AllFem[match(ego_agei[['motherID']][e], LT.AllFem$who ),]$motherID # ego's grand mother ID, dead or alive
#in main script, this line is: ego.GM.ID <- AllFem[match(ego_agei[e]$motherID, AllFem$who ),]$motherID # ego's grand mother ID, dead or alive
    # Main script doesn't run with line used here. This loop doesn't run with main script's line. 
        
        #For nieces
        DaugthersOfEgoMother <- LT.AllFem[which(LT.AllFem$motherID==ego_agei$motherID[e]),] #Get all the daughters of ego's mother
        AllSistersDeadorAlive <- DaugthersOfEgoMother[-which(DaugthersOfEgoMother$who==ego_agei$who[e]),] #get only ego's sisters (eliminate ego)
        nb.nieces.agej.ego[e] <- sum(ind_agej$motherID %in% AllSistersDeadorAlive$who)
        
        nb.children.agej.ego[e] <- sum(ind_agej$motherID %in% ego_agei$who[e]) # nb of children of ego e that are age j
        nb.grandchildren.agej.ego[e] <- sum(GM.IDs.ind_agej %in% ego_agei$who[e]) # nb of grandchildren of ego e that are age j
        
        if (sum(is.na(ego.GM.ID))==1) {
          nb.aunts.agej.ego[e] <- 0
        } else if (sum(is.na(ego.GM.ID))==0) {
            
          if (sum(ind_agej$who==Mothers.ID.ego[e])==0){ # if ego's mother is dead
              nb.aunts.agej.ego[e] <- sum(ind_agej$motherID == GM.ID[e] )
          } else if (sum(ind_agej$who==Mothers.ID.ego[e])>0){ # if ego's mother is alive
            ind_agej_withoutegosmother <- ind_agej[-which(ind_agej$who==Mothers.ID.ego[e]),] # all indiv alive at last time step except from ego's mother
            nb.aunts.agej.ego[e] <- sum(ind_agej_withoutegosmother$motherID == GM.ID[e])
          }  
        }
        
        if (j==i){ #same cohort sisters
          nb.sis.agej.ego[e] <- sum(ind_agej$motherID==Mothers.ID.ego[e]) - 1
            if (sum(is.na(ego.GM.ID))==1) {
              nb.cousins.agej.ego[e] <- 0
            } else if (sum(is.na(ego.GM.ID))==0) {
          nb.cousins.agej.ego[e] <-sum(GM.IDs.ind_agej %in% ego.GM.ID) - nb.sis.agej.ego[e] - 1 # nb of cousins of ego e that are age j
        } 
          } else if (j!=i){ 
          nb.sis.agej.ego[e] <- sum(ind_agej$motherID==Mothers.ID.ego[e])
            if (sum(is.na(ego.GM.ID))==1) {
            nb.cousins.agej.ego[e] <- 0
          } else if (sum(is.na(ego.GM.ID))==0) {
          nb.cousins.agej.ego[e] <-sum(GM.IDs.ind_agej %in% ego.GM.ID) - nb.sis.agej.ego[e]
          }
          }
      } # end loop on ego e
      
      kinship_Sisters.LT[i,j] <- mean(nb.sis.agej.ego)
      kinship_Aunts.LT[i,j] <- mean(nb.aunts.agej.ego)
      kinship_Children.LT[i,j] <- mean(nb.children.agej.ego)
      kinship_GrandChildren.LT[i,j] <- mean(nb.grandchildren.agej.ego)   
      kinship_Cousins.LT[i,j] <-  mean(nb.cousins.agej.ego)
      kinship_Nieces.LT[i,j] <- mean(nb.nieces.agej.ego)
    } # end loop on sister's and aunt's ages
  } # end else if
  
  Res_summary.LT[i,"nb.ego"] <- nrow(ego_agei)
  Res_summary.LT[i,"nb.ego.MA"] <- n_MA.LT[i]
  Res_summary.LT[i,"Pr_MA"] <- Pr_MA_egoagei.LT[i]
  Res_summary.LT[i,"nb.ego.GMA"] <- n_GMA.LT[i]
  Res_summary.LT[i,"Pr_GMA"] <- Pr_GMA_egoagei.LT[i]
  Res_summary.LT[i,"Avg_Sis"] <- sum(kinship_Sisters.LT[i,])
  Res_summary.LT[i,"Avg_Aunts"] <- sum(kinship_Aunts.LT[i,])
  Res_summary.LT[i,"Avg_Ch"] <- sum(kinship_Children.LT[i,])
  Res_summary.LT[i,"Avg_GCh"] <- sum(kinship_GrandChildren.LT[i,])
  Res_summary.LT[i,"Avg_Cou"] <- sum(kinship_Cousins.LT[i,])
  Res_summary.LT[i,"Avg_Nie"] <- sum(kinship_Nieces.LT[i,])
} # end loop on ego's age

round(Res_summary.LT,2) # summary per ego age


# THERE WERE TWO ISSUES
#I
# For individuals who have NA as their grandmotherID or their motherID (such as dispersers, who can appear in AllFem, but not in datalastF)
# Their entire age class appears as having NA aunts in every other age class. 

#For example, changing the motherID of individual 24 to NA (making the grandmother of individuals 16,17,18 have a grandmother with ID NA)
#Individuals 16-18 are all 3-year-olds. Hence, this change leads to the average number of aunts for age 3 being NA, as well as all of its
# row in the kinship matrix. 
#Change in population to see:
#motherID = c(rep(19,3),rep(20,3),rep(21,3),rep(22,3),rep(23,3),rep(24,3),rep(25,3),rep(26,2),NA,27,28),
#                                                                                          ^ ^ ^ 

# II
# In the simulated population, the grandmothers have motherIDs, but their mothers don't
# hence the motherID of their mothers is taken as being NA, and it seems like, because both grandmothers
# have 'NA' as their grandmother ID, they are counted as each others cousins. 
# This could happen in the real IBM with sons of dispersers. 

#solved by:
#Adding lines 136-138, 150-152, 157-159 (all the same code):
# If an individual's grandmother ID is equal to NA, then it has 0 cousins and 0 aunts. Which seems reasonable, since the mother
# of this individual must have been a disperser and we don't expect her to have sisters (ego's aunts) in the population. It follows
# ego also wouldn't have cousins. 

# (III?) -> had to change line to get ego's grandmother ID, regardless of her being alive or dead.
#           The line works here, but not in the main script. The main script's line doesn't work here. 
#           maybe the change was because the data is in data frames here, and in AgentMatrix in the real model?
>>>>>>> 6a010f0fb09f29c09e7525def4e91ec38417e925

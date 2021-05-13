# kinship relationships at equilibrium


# Packages used to build the sub-models
library(NetLogoR)
library(testthat)
library(kinship2)
library(SciViews)
library(DescTools)
load(file="popSim.Rdata")

# all indiv ever lived in the pop, list with t elements (pop at each time step)
popSim

# For individuals ever alive in the pop (dead or not at last time step)
dataall  <- unique(do.call(rbind, popSim)  ) # all indiv ever lived in population
allwithparents <- dataall[!is.na(dataall$motherID),] #se data for individuals with known parents only
#### What does the comma do? 
#### comma => take all columns (index of lines to keep before comma, index of columns after comma). Here we want to keep all columns, just the line without NA for motherID

# for now, keep females only, and female-related kin only
AllFem <- allwithparents[allwithparents$sex=="F",]  # all females with known mother

# Ages - calculated among all females with known mother ever alive
ages<- min(AllFem$age):max(AllFem$age)


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


############################################################
##              LOOP FOR ALL KIN                          ##
############################################################

#Mother & Grandmother
nb_MotherAlive <- kinship_Mother <- matrix(NA, nrow=max(ages),ncol=max(ages))
nb_GrandmotherAlive <- kinship_Grandmother <- matrix(NA, nrow=max(ages),ncol=max(ages))

n_MA <- n_GMA <-Pr_MA_egoagei <- Pr_GMA_egoagei <-NULL
mums <- list()

#Sisters
kinship_Sisters <- matrix(NA, nrow=max(ages),ncol=max(ages))

#Aunts
kinship_Aunts <- matrix(NA, nrow=max(ages),ncol=max(ages))

# Children
kinship_Children <- matrix(NA, nrow=max(ages),ncol=max(ages))

# Grand Children
kinship_GrandChildren <- matrix(NA, nrow=max(ages),ncol=max(ages))

# Cousins
kinship_Cousins <- matrix(NA, nrow=max(ages),ncol=max(ages))

#Nieces
kinship_Nieces <- matrix(NA, nrow=max(ages),ncol=max(ages))

#Summary of results
Res_summary <- data.frame(ego.age=ages,
                          nb.ego=c(rep(NA,length(ages))),  # number of ego aged i at last time step
                          nb.ego.MA=c(rep(NA,length(ages))),  # number of ego aged i with mother alive at last time step
                          Pr_MA =c(rep(NA,length(ages))), # proba mother alive for ego age i
                          nb.ego.GMA=c(rep(NA,length(ages))),  # number of ego aged i with mother alive at last time step
                          Pr_GMA =c(rep(NA,length(ages))),
                          Avg_Sis = c(rep(NA,length(ages))),
                          Avg_Aunts = c(rep(NA,length(ages))),
                          Avg_Ch = c(rep(NA,length(ages))),
                          Avg_GCh = c(rep(NA,length(ages))),
                          Avg_Cou = c(rep(NA,length(ages))),
                          Avg_Nie = c(rep(NA,length(ages)))
)

for(i in ages){  #loop over ego age
  ego_agei <- datalastF[datalastF$age==i,] # ego age i alive at last time step
  
  if(nrow(ego_agei)==0){
    nb_MotherAlive[i,] <- rep(NA,length(ages))
    nb_GrandmotherAlive[i,] <-rep(NA,length(ages))
    kinship_Mother[i,] <- rep(NA, length(ages))
    kinship_Grandmother[i,] <- rep(NA, length(ages))
    kinship_Sisters[i,] <- rep(NA,length(ages))
    kinship_Aunts[i,] <- rep(NA,length(ages))
    kinship_Children[i,] <- rep(NA,length(ages))
    kinship_Cousins[i,] <- rep(NA,length(ages))
    kinship_Nieces[i,] <- rep(NA, length(ages))
    
    
  }else if(nrow(ego_agei)>0){ 
    Mothers.ID.ego <- ego_agei$motherID # mothers ID of ego age i
    Mothers.Alive.ID <- ego_agei$motherID[which(Mothers.ID.ego %in% IDMothersAlive)] # alive mothers ID of ego age i, repeated by number of daughters
    mums[[i]] <- Mothers.Alive.ID
    n_MA[i] <- length(Mothers.Alive.ID) # nb of ego age i with mother alive at last time step
    Pr_MA_egoagei[i]  <- n_MA[i]/ nrow(ego_agei)  # proba mother alive for ego age i
    
    GM.ID <- AllFem$motherID[match(Mothers.ID.ego, AllFem$who)] # GM ID of ego age i
    GM.Alive.ID <- GM.ID[GM.ID %in% IDGrandMothersAlive]
    n_GMA[i] <- length(GM.Alive.ID) # nb of ego age i with grandmother alive at last time step
    Pr_GMA_egoagei[i]  <- n_GMA[i]/ nrow(ego_agei)  # proba mother alive for ego age i
    
    # age of mother AND GRAND MOTHER if alive at last time step
    Mage <- datalastF$age[match(Mothers.Alive.ID, datalastF$who )] # match ID of mothers alive at last time step in datalast (which includes all females alive at last time step)
    GMage <- datalastF$age[match(GM.Alive.ID, datalastF$who )] # match ID of mothers alive at last time step in datalast (which includes all females alive at last time step)
    
    for(j in ages){ # loop over ages
      ind_agej <- datalastF[datalastF$age==j] #individuals of age j alive at last time step
      
      # FOR MOTHERS
      nb_MotherAlive[i,j] <- sum(Mage==j) 
      kinship_Mother[i,j] <- sum(Mage==j) / nrow(ego_agei) # kinship matrix Pr of mother alive
      # FOR GRAND MOTHERS
      nb_GrandmotherAlive[i,j] <- sum(GMage==j) 
      kinship_Grandmother[i,j] <- sum(GMage==j) / nrow(ego_agei) # kinship matrix Pr of mother alive
      
      
      GM.IDs.ind_agej <-AllFem$motherID[match(ind_agej$motherID, AllFem$who )] # GM ID of ind age j alive at last time step, the GM can can be dead or alive
      nb.aunts.agej.ego <- nb.sis.agej.ego <- nb.children.agej.ego <- nb.grandchildren.agej.ego <- nb.cousins.agej.ego <- nb.nieces.agej.ego <- NULL # empty object to store results
      
      for(e in 1:nrow(ego_agei)){ # loop over each ego aged i
        
        ego.GM.ID <- GM.ID[e] #ego's grand mother ID, dead or alive
        
        #For nieces
        DaugthersOfEgoMother <- AllFem[which(AllFem$motherID==ego_agei$motherID[e]),] #Get all the daughters of ego's mother
        AllSistersDeadorAlive <- DaugthersOfEgoMother[-which(DaugthersOfEgoMother$who==ego_agei$who[e]),] #get only ego's sisters (eliminate ego)
        nb.nieces.agej.ego[e] <- sum(ind_agej$motherID %in% AllSistersDeadorAlive$who)
        
        
        nb.children.agej.ego[e] <- sum(ind_agej$motherID %in% ego_agei$who[e]) # nb of children of ego e that are age j
        nb.grandchildren.agej.ego[e] <- sum(GM.IDs.ind_agej %in% ego_agei$who[e]) # nb of grandchildren of ego e that are age j
        
        if (sum(is.na(ego.GM.ID))==1) { #Added
          nb.aunts.agej.ego[e] <- 0       #Added
        } else if (sum(is.na(ego.GM.ID))==0) {   #Added
          
          if (sum(ind_agej$who==Mothers.ID.ego[e])==0){ # if ego's mother is dead
            nb.aunts.agej.ego[e] <- sum(ind_agej$motherID == GM.ID[e] )
          } else if (sum(ind_agej$who==Mothers.ID.ego[e])>0){ # if ego's mother is alive
            ind_agej_withoutegosmother <- ind_agej[-which(ind_agej$who==Mothers.ID.ego[e]),] # all indiv alive at last time step except for ego's mother
            nb.aunts.agej.ego[e] <- sum(ind_agej_withoutegosmother$motherID == GM.ID[e])
          }  #Added
        }
        
        if (j==i){ #same cohort sisters
          nb.sis.agej.ego[e] <- sum(ind_agej$motherID==Mothers.ID.ego[e]) - 1
          
          if (sum(is.na(ego.GM.ID))==1) { #Added
            nb.cousins.agej.ego[e] <- 0         #Added
          } else if (sum(is.na(ego.GM.ID))==0) {      #Added
            
            nb.cousins.agej.ego[e] <-sum(GM.IDs.ind_agej %in% ego.GM.ID) - nb.sis.agej.ego[e] - 1 # nb of cousins of ego e that are age j
          }  #Added
        } else if (j!=i){ 
          nb.sis.agej.ego[e] <- sum(ind_agej$motherID==Mothers.ID.ego[e])
          
          if (sum(is.na(ego.GM.ID))==1) {     #Added
            nb.cousins.agej.ego[e] <- 0        #Added
          } else if (sum(is.na(ego.GM.ID))==0) {     #Added
            
            nb.cousins.agej.ego[e] <-sum(GM.IDs.ind_agej %in% ego.GM.ID) - nb.sis.agej.ego[e]
          }
        }  #Added
      } # end loop on ego e
      
      kinship_Sisters[i,j] <- mean(nb.sis.agej.ego)
      kinship_Aunts[i,j] <- mean(nb.aunts.agej.ego)
      kinship_Children[i,j] <- mean(nb.children.agej.ego)
      kinship_GrandChildren[i,j] <- mean(nb.grandchildren.agej.ego)   
      kinship_Cousins[i,j] <-  mean(nb.cousins.agej.ego)
      kinship_Nieces[i,j] <- mean(nb.nieces.agej.ego)
    } # end loop on sister's and aunt's ages
  } # end else if
  
  Res_summary[i,"nb.ego"] <- nrow(ego_agei)
  Res_summary[i,"nb.ego.MA"] <- n_MA[i]
  Res_summary[i,"Pr_MA"] <- Pr_MA_egoagei[i]
  Res_summary[i,"nb.ego.GMA"] <- n_GMA[i]
  Res_summary[i,"Pr_GMA"] <- Pr_GMA_egoagei[i]
  Res_summary[i,"Avg_Sis"] <- sum(kinship_Sisters[i,])
  Res_summary[i,"Avg_Aunts"] <- sum(kinship_Aunts[i,])
  Res_summary[i,"Avg_Ch"] <- sum(kinship_Children[i,])
  Res_summary[i,"Avg_GCh"] <- sum(kinship_GrandChildren[i,])
  Res_summary[i,"Avg_Cou"] <- sum(kinship_Cousins[i,])
  Res_summary[i,"Avg_Nie"] <- sum(kinship_Nieces[i,])
} # end loop on ego's age

round(Res_summary,2) # summary per ego age

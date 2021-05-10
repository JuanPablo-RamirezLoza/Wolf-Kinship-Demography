
# kinship relationships at equilibrium

setwd("~/Documents/PROJECTS/KINSHIP DEMO/stageJuanPablo/debugJP_getkinshipfromIBM")

# Packages used to build the sub-models
library(NetLogoR)
library(testthat)
library(kinship2)
library(SciViews)
library(DescTools)
load(file="popSim.Rdata")

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


## SISTERS AND AUNTS
# sisters
kinship_Sisters <- matrix(NA, nrow=max(ages),ncol=max(ages))
#Aunts
kinship_Aunts <- matrix(NA, nrow=max(ages),ncol=max(ages))
# Children
kinship_Children <- matrix(NA, nrow=max(ages),ncol=max(ages))
# Grand Children
kinship_GrandChildren <- matrix(NA, nrow=max(ages),ncol=max(ages))
# Cousins
kinship_Cousins <- matrix(NA, nrow=max(ages),ncol=max(ages))

n_ego_agei <- n_Aunts <-NULL

for(i in ages){  #loop over ego age
  ego_agei <- datalastF[datalastF$age==i,] # ego age i alive at last time step

  if(nrow(ego_agei)==0){
    kinship_Sisters[i,] <- rep(NA,length(ages))
    kinship_Aunts[i,] <- rep(NA,length(ages))
    kinship_Children[i,] <- rep(NA,length(ages))
    kinship_Cousins[i,] <- rep(NA,length(ages))
    
  }else if(nrow(ego_agei)>0){ 
    Mothers.ID.ego <- ego_agei$motherID # mothers ID of ego age i
    GM.ID <- AllFem$motherID[match(Mothers.ID.ego, AllFem$who )] # GM ID of ego age i
    
    for(j in ages){ # loop over ages
      ind_agej <- datalastF[datalastF$age==j] #individuals of age j alive at last time step
      
      GM.IDs.ind_agej <-AllFem$motherID[match(ind_agej$motherID, AllFem$who )] # GM ID of ind age j alive at last time step, weather the GM can can be dead or alive
      
      nb.aunts.agej.ego <- nb.sis.agej.ego <- nb.children.agej.ego <- nb.grandchildren.agej.ego <- nb.cousins.agej.ego <- NULL # empty object to store results
      
      for(e in 1:nrow(ego_agei)){ # loop over each ego aged i
        ego.GM.ID <- AllFem[match(ego_agei[e]$motherID, AllFem$who ),]$motherID # ego's grand mother ID, dead or alive
        
        nb.children.agej.ego[e] <- sum(ind_agej$motherID %in% ego_agei$who[e]) # nb of children of ego e that are age j
        nb.grandchildren.agej.ego[e] <- sum(GM.IDs.ind_agej %in% ego_agei$who[e]) # nb of grandchildren of ego e that are age j
        
        if (sum(ind_agej$who==Mothers.ID.ego[e])==0){ # if ego's mother is dead
          nb.aunts.agej.ego[e] <- sum(ind_agej$motherID == GM.ID[e] )
        } else if (sum(ind_agej$who==Mothers.ID.ego[e])>0){ # if ego's mother is alive
          ind_agej_withoutegosmother <- ind_agej[-which(ind_agej$who==Mothers.ID.ego[e]),] # all indiv alive at last time step except from ego's mother
          nb.aunts.agej.ego[e] <- sum(ind_agej_withoutegosmother$motherID == GM.ID[e])
        }   
        
        if (j==i){ #same cohort sisters
         nb.sis.agej.ego[e] <- sum(ind_agej$motherID==Mothers.ID.ego[e]) - 1
         nb.cousins.agej.ego[e] <-sum(GM.IDs.ind_agej %in% ego.GM.ID) - nb.sis.agej.ego[e] - 1 # nb of cousins of ego e that are age j
         } else if (j!=i){ 
          nb.sis.agej.ego[e] <- sum(ind_agej$motherID==Mothers.ID.ego[e])
          nb.cousins.agej.ego[e] <-sum(GM.IDs.ind_agej %in% ego.GM.ID) - nb.sis.agej.ego[e]
          }
        
      } # end loop on ego e
      
      kinship_Sisters[i,j] <- mean(nb.sis.agej.ego)
      kinship_Aunts[i,j] <- mean(nb.aunts.agej.ego)
      kinship_Children[i,j] <- mean(nb.children.agej.ego)
      kinship_GrandChildren[i,j] <- mean(nb.grandchildren.agej.ego)   
      kinship_Cousins[i,j] <-  mean(nb.cousins.agej.ego)
    } # end loop on sister's and aunt's ages
  } # end else if
} # end loop on ego's age

### Author: Jun Zhao
### Date: 06/19/17

## this code is written to add the HOV information of different links from the original input_link and input_node file

input_link = read.csv("D:/Public/HERE\ Network\ Table/input_DTALite/input_link_and_node_from_weiyi_0614/input_link.csv", header = TRUE)
input_node = read.csv("D:/Public/HERE\ Network\ Table/input_DTALite/input_link_and_node_from_weiyi_0614/input_node.csv", header = TRUE)

# read the condition and lane information from the output file of HERE
Lane = read.table("D:/Public/HERE\ Network\ Table/LaneTable.txt",header = TRUE,sep = ",")

ConditionDateTime = read.table("D:/Public/HERE\ Network\ Table/ConditionDateTime.txt",header = TRUE,sep = ",")

ConditionToll = read.table("D:/Public/HERE\ Network\ Table/ConditionToll.txt",header = TRUE,sep = ",")

ConditionHov = read.table("D:/Public/HERE\ Network\ Table/ConditionHov.txt",header = TRUE,sep = ",")

ConditionType = read.table("D:/Public/HERE\ Network\ Table/ConditionType.txt",header = TRUE,sep = ",")

LaneNavStrand = read.table("D:/Public/HERE\ Network\ Table/LaneNavStrand.txt",header = TRUE,sep = ",")


LinkStudyArea = input_link

LaneStudyArea = Lane[Lane$LINK_ID %in% LinkStudyArea$link_id,]
dim(LaneStudyArea)

LaneNavStrandStudyArea = LaneNavStrand[LaneNavStrand$LANE_ID %in% LaneStudyArea$LANE_ID,]
dim(LaneNavStrandStudyArea)

ConditionDateTimeStudyArea = ConditionDateTime[ConditionDateTime$CONDITION_ID %in% LaneNavStrandStudyArea$CONDITION_ID,]
dim(ConditionDateTimeStudyArea)

ConditionTollStudyArea = ConditionToll[ConditionToll$CONDITION_ID %in% LaneNavStrandStudyArea$CONDITION_ID,]
dim(ConditionTollStudyArea)

ConditionHovStudyArea = ConditionHov[ConditionHov$CONDITION_ID %in% LaneNavStrandStudyArea$CONDITION_ID,]
dim(ConditionHovStudyArea)

ConditionTypeStudyArea = ConditionType[ConditionType$CONDITION_ID %in% LaneNavStrandStudyArea$CONDITION_ID,]
dim(ConditionTypeStudyArea)


## this function is used to find the LinkID for each ConditionID
FindLinkID = function(condition_id, LaneStudyArea, LaneNavStrandStudyArea){
  # the condition_id is a vector
  length = numeric(length(condition_id))
  vLinkID = numeric(length(condition_id))
  for(i in 1:length(condition_id)){
    condition_id_index = condition_id[i]
    LaneID = LaneNavStrandStudyArea$LANE_ID[which(LaneNavStrandStudyArea$CONDITION_ID == condition_id_index)]
    # one condition_id_index should have more than one LaneIDs
    LinkID = LaneStudyArea$LINK_ID[which(LaneStudyArea$LANE_ID %in% LaneID)]
    
    # ?? different LaneID could have some LinkID ??
    
    
    # return LinkID
    LinkID = unique(LinkID)
    length[i] = length(LinkID)
    vLinkID[i] = LinkID
  }
  # return(length) # after testing, the linkID are all 1
  return(vLinkID)

}

## test of the function
# length_linkID = FindLinkID(ConditionHovStudyArea$CONDITION_ID,LaneStudyArea, LaneNavStrandStudyArea)

LinkID_HOV = FindLinkID(ConditionHovStudyArea$CONDITION_ID[2],LaneStudyArea, LaneNavStrandStudyArea)



## function to generate a new artificial link between two nodes:

GenerateNewLink = function(New_Link_Node_From, New_Link_Node_To, New_Link_Node_From_X, New_Link_Node_From_Y, New_Link_Node_To_X, New_Link_Node_To_Y,LinkStudyAreaHov){
  NewLink = as.data.frame(matrix(ncol = dim(LinkStudyAreaHov)[2], nrow = 1))
  colnames(NewLink) = c(colnames(LinkStudyAreaHov))
  # below are all the default value of this artificial link
  NewLink$IS_HOV = 1
  NewLink$MIN_PASSENGERS = 2 # we set the minimum passenger number to 2, which also allows passenger = 3 to run in the link
  NewLink$LENGTH = 0.01 # the length of the artificial link is only 0.01 mile
  NewLink$REF_NODE_I = New_Link_Node_From
  NewLink$NON_REF_NO = New_Link_Node_To
  NewLink$NUM_LANES = 1
  NewLink$SPD_LIMIT = 70
  NewLink$FFSPEED = 100
  NewLink$CAPACITY = 2000
  NewLink$FUNCTIONAL = 1 # seen as 'Freeway'
  NewLink$TRAVEL_DIR = 1 # here we all use 1, meaning from direction
  NewLink$Ref_N_X = New_Link_Node_From_X
  NewLink$Ref_N_Y = New_Link_Node_From_Y
  NewLink$FROM_N_X = New_Link_Node_From_X
  NewLink$FROM_N_Y = New_Link_Node_From_Y
  NewLink$TO_N_X = New_Link_Node_To_X
  NewLink$TO_N_Y = New_Link_Node_To_Y
  NewLink$HYBRID_CAR = 'Y'
  NewLink$MOTORCYCLE = 'Y'
  NewLink$ALTERNATE_FUEL_CARPOOL = 'Y'
  NewLink$FEE_PAY_CARPOOL = 'Y'
  
  return (NewLink)
  
  # here it is only one link of the from node to the to node
  # when completing the network, we need to also generate the link of the opposite direction
  
}


## this function is used to add hov links to the existing link layer

AddHovLinks = function(LinkStudyArea, LaneStudyArea, ConditionHovStudyArea, LaneNavStrandStudyArea){
  ## arguments inculude:
  # LinkStudyArea: Link table in the study area
  # LaneStudyArea: Lane table in the study area
  # ConditionHovStudyArea: the hov condition in of lanes (include the link_Id where this HOV lane lies)
  # LaneNavStrand: in the study area which showing the relationship between the LANE_ID and LINK_ID
  
  ## first, we should add the new columns to Link Table
  LinkStudyAreaHov = LinkStudyArea
  LinkStudyAreaHov$IS_HOV = 0
  LinkStudyAreaHov$MIN_PASSENGERS = NA
  LinkStudyAreaHov$HYBRID_CAR = NA
  LinkStudyAreaHov$MOTORCYCLE = NA
  LinkStudyAreaHov$ALTERNATE_FUEL_CARPOOL = NA
  LinkStudyAreaHov$FEE_PAY_CARPOOL = NA
  
  condition_id_hov = ConditionHovStudyArea$CONDITION_ID
  
  # a new empty dataframe to add the new links 
  LinksToAdd = as.data.frame(matrix(ncol = dim(LinkStudyAreaHov)[2], nrow = 1))
  colnames(LinksToAdd) = c(colnames(LinkStudyAreaHov))
  
  LinkID = numeric() # define an empty numeric vector of LinkID
  
  LinkID_index = 0
  
  for (i in 1:length(condition_id_hov)){
    if (round(i/100,0) == i/100) cat("the ", i, " output linkID is:", LinkID_tmp, "\n")
    
    
    condition_id = condition_id_hov[i]
    LinkID_tmp = unique(FindLinkID(condition_id, LaneStudyArea, LaneNavStrandStudyArea))
    # here we use unique() because the different condition_id
    
    LinkID_index = LinkID_index + length(LinkID_tmp)
    LinkID[(LinkID_index - length(LinkID_tmp) + 1):LinkID_index] = LinkID_tmp
    for (j in 1:length(LinkID_tmp)){
      # to check if the LinkID has appeared before or not
      bLinkID = sum(LinkID == LinkID[j]) == 1
      if (bLinkID){
        
        # if the linkID hasn't appeared before
        # duplicate the nodes, and add the new link to the link table, then set the original lane -1
        LinkStudyAreaHov$NUM_LANES[which(LinkStudyAreaHov$LINK_ID == LinkID_tmp[j])] = LinkStudyArea$NUM_LANES[which(LinkStudyAreaHov$LINK_ID == LinkID_tmp[j])] - 1
        
        
        LinkTableHovTmp = as.data.frame(LinkStudyAreaHov[which(LinkStudyAreaHov$LINK_ID == LinkID_tmp[j]),])
        LinkTableHovTmp$MIN_PASSENGERS = ConditionHovStudyArea$MIN_PASSENGERS[which(ConditionHovStudyArea$CONDITION_ID == condition_id)]
        LinkTableHovTmp$HYBRID_CAR = ConditionHovStudyArea$HYBRID_CAR[which(ConditionHovStudyArea$CONDITION_ID == condition_id)]
        LinkTableHovTmp$MOTORCYCLE = ConditionHovStudyArea$MOTORCYCLE[which(ConditionHovStudyArea$CONDITION_ID == condition_id)]
        LinkTableHovTmp$ALTERNATE_FUEL_CARPOOL = ConditionHovStudyArea$ALTERNATE_FUEL_CARPOOL[which(ConditionHovStudyArea$CONDITION_ID == condition_id)]
        LinkTableHovTmp$FEE_PAY_CARPOOL = ConditionHovStudyArea$FEE_PAY_CARPOOL[which(ConditionHovStudyArea$CONDITION_ID == condition_id)]
        
        LinkTableHovTmp$IS_HOV = 1 # change the attribute of is_hov_lane to 1
        LinkTableHovTmp$NUM_LANES = 1 # suppose now there is only one Hov lane
        
        # change the node X and Y of the from_node
        LinkTableHovTmp$FROM_N_X = LinkStudyArea$FROM_N_X[which(LinkStudyArea$LINK_ID == LinkID_tmp[j])] - 0.0001
        LinkTableHovTmp$FROM_N_Y = LinkStudyArea$FROM_N_Y[which(LinkStudyArea$LINK_ID == LinkID_tmp[j])] - 0.0001
        
        # to node id will not change any more
        # LinkTableHovTmp$TO_N_X = LinkStudyArea$TO_N_X[which(LinkStudyArea$LINK_ID == LinkID_tmp[j])] - 0.00001
        # LinkTableHovTmp$TO_N_Y = LinkStudyArea$TO_N_Y[which(LinkStudyArea$LINK_ID == LinkID_tmp[j])] - 0.00001
        
        # assign the new link id to the now node
        # first, we need to define the node id to the new node 
        # find the detailed description in:
        # https://www.evernote.com/shard/s613/sh/d3983128-c9d0-4def-a1f8-29f0392c7faa/77954f40842fc3afd378095bfa05d40c
        
        New_Link_Node_From = 2000000000 + i
        New_Link_Node_To = LinkTableHovTmp$REF_NODE_I
        
        LinkTableHovTmp$REF_NODE_I = 2000000000 + i
        
        
        # for debug
        # cat(LinkTableArtificial, "\n")
        
        LinksToAdd = rbind(LinksToAdd, LinkTableHovTmp)
        
        
        ###### another artifical link to link the new node to the network
        # the start node is the new artificial node
        # the end node is the old node which is near the new node and the old node is the origion of 
        # the non-hov lane
        New_Link_Node_To_X = LinkTableHovTmp$FROM_N_X + 0.0001
        New_Link_Node_To_Y = LinkTableHovTmp$FROM_N_Y + 0.0001
        
        New_Link_Node_From_X = LinkTableHovTmp$FROM_N_X
        New_Link_Node_From_Y = LinkTableHovTmp$FROM_N_Y
        
        
        # add two more artificial links to link the start node and end node
        LinkTableArtificial = GenerateNewLink(New_Link_Node_From, New_Link_Node_To, New_Link_Node_From_X, New_Link_Node_From_Y, New_Link_Node_To_X, New_Link_Node_To_Y, LinkStudyAreaHov)
        
        LinksToAdd = rbind(LinksToAdd, LinkTableArtificial)
        
        
      }
      
      if (!bLinkID){
        cat("the duplicated linkID is: ", LinkID_tmp[j] ,"\n")
        LinkStudyArea$NUM_LANES[which(LinkStudyAreaHov$LINK_ID == LinkID_tmp[j])] = LinkStudyArea$NUM_LANES[which(LinkStudyAreaHov$LINK_ID == LinkID_tmp[j])] - 1
        LinksToAdd$NUM_LANES[which(LinksToAdd$LINK_ID == LinkID_tmp[j])] = LinksToAdd$NUM_LANES[which(LinksToAdd$LINK_ID == LinkID_tmp[j])] + 1
        
      }
    }
    
    
    
    
  }
  
  
  # the output is a new LinkStudyArea table (data.frame) with the new HOV Links added
  LinkStudyAreaWithHov = rbind(LinkStudyAreaHov, LinksToAdd[-1,]) # LinksToAdd need to delete the first row because it is all NAs
  
  return(LinkStudyAreaWithHov)
}

LinkStudyAreaWithHovLanes = AddHovLinks(LinkStudyArea, LaneStudyArea, ConditionHovStudyArea, LaneNavStrandStudyArea)

## output the table
# write.csv(LinkStudyAreaWithHovLanes, "D:/Public/HERE\ Network\ Table/LinkStudyAreaWithHovLanes.csv")







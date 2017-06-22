### Author: Jun Zhao
### Date: 06/19/17


## this code is written to add the HOV LANE information of different links from the original input_link and input_node file of DTALite

###############################################################################################################
### PART 1: READ FILES #######
###############################################################################################################

library(taRifx) 
library(stringr) # in order to use the function of destring to read the numbers out of a string


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


# only use the data in the study area

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


###############################################################################################################
### PART 2: ADD HOV LINKS #######
###############################################################################################################


# # need the from_node and to_node information in the LinkStudyArea data
# # this function is used to add the FROM_N_X, FROM_N_Y, TO_N_X AND TO_N_Y information to the LinkStudyArea data
# # it takes a while to run this function
# AddNodeXandY = function(LinkStudyArea){
#   len = dim(LinkStudyArea)[1]
#   LinkStudyArea$FROM_N_X = double(len)
#   LinkStudyArea$FROM_N_Y = double(len)
#   LinkStudyArea$TO_N_X = double(len)
#   LinkStudyArea$TO_N_Y = double(len)
#   for (i in 1:len){
#     original_geometry = as.character(LinkStudyArea$original_geometry[i])
#     if (round(i/1000,0) == i/1000) cat(i,original_geometry,"\n")
#     
#     NodeXandY = FindNodeXandY(original_geometry[1])
#     LinkStudyArea$FROM_N_X[i] = NodeXandY[1,1]
#     LinkStudyArea$FROM_N_Y[i] = NodeXandY[1,2]
#     
#     LinkStudyArea$TO_N_X[i] = NodeXandY[2,1]
#     LinkStudyArea$TO_N_Y[i] = NodeXandY[2,2]
#   }
#   LinkStudyArea
# }
# 
# LinkStudyArea = AddNodeXandY(LinkStudyArea)
# write.csv(LinkStudyArea, "D:/Public/HERE\ Network\ Table/LinkStudyAreaWithNodeXY.csv")


# after getting this results, we will directly use the result and will not run this function again
LinkStudyArea =  read.csv("D:/Public/HERE\ Network\ Table/LinkStudyAreaWithNodeXY.csv", header = T)





## this function is used to find the LinkID for each ConditionID
# and test if the link id is unique or not
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
    cat(LinkID,"\n")
    
    length[i] = length(LinkID)
    vLinkID[i] = LinkID
  }
  # return the vector of LinkID  # after testing, the length of linkID are all 1, meaning condition_id only relates to one linkID
  
  return(vLinkID)

}

## test of the function
# linkID = FindLinkID(ConditionHovStudyArea$CONDITION_ID[1:1000],LaneStudyArea, LaneNavStrandStudyArea)




## this function is used to read the node x and y from the 'original geometry' in input_link file 
## !!!! the first corrdinate is for the from node and the second coordiante is for the to node

FindNodeXandY = function(OriginalGeometry){
  # the original geometry information contains the node information
  NodeXandY = as.character(OriginalGeometry)
  NodeXandY = str_extract_all(NodeXandY,"\\(?[0-9,.,-]+\\)?")[[1]]
  NodeXandYFrom = as.numeric(unlist(strsplit(NodeXandY[1],",")))[1:2]
  NodeXandYTo = as.numeric(unlist(strsplit(NodeXandY[2],",")))[1:2]
  
  # returns a matrix of both the node_id of the from node and to node
  # looks like this:
  #                     [,1]     [,2]
  # NodeXandYFrom -76.69124 39.36211
  # NodeXandYTo   -76.69501 39.36481
  rbind(NodeXandYFrom,NodeXandYTo)
}


# FindNodeXandY(input_link$original_geometry[1])




## function to generate a new artificial link between two nodes:

GenerateNewLink = function(New_Link_Node_From, New_Link_Node_To, New_Link_Node_From_X, New_Link_Node_From_Y, New_Link_Node_To_X, New_Link_Node_To_Y, LinkStudyAreaHov){
  NewLink = as.data.frame(matrix(ncol = dim(LinkStudyAreaHov)[2], nrow = 1))
  colnames(NewLink) = c(colnames(LinkStudyAreaHov))
  # below are all the default value of this artificial link
  NewLink$IS_HOV = 1
  NewLink$MIN_PASSENGERS = 2 # we set the minimum passenger number to 2, which also allows passenger = 3 to run in the link
  NewLink$length = 0.001 # the length of the artificial link is only 0.001 mile
  NewLink$from_node_id = New_Link_Node_From
  NewLink$to_node_id = New_Link_Node_To
  NewLink$number_of_lanes = 1
  NewLink$speed_limit = 70
  NewLink$lane_capacity_in_vhc_per_hour = 2600
  NewLink$link_type = 1 # seen as 'Freeway'
  NewLink$direction = 1 # here we all use 1, meaning from direction
  NewLink$BPR_alpha_term = 0.15
  NewLink$BPR_beta_term = 4
  

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
  
  # define a new empty matrix to add the new links 
  LinksToAdd = as.data.frame(matrix(ncol = dim(LinkStudyAreaHov)[2], nrow = 1))
  colnames(LinksToAdd) = c(colnames(LinkStudyAreaHov))
  
  LinkID = numeric() # define an empty numeric vector of LinkID
  
  LinkID_index = 0
  
  for (i in 1:length(condition_id_hov)){
    
    
    condition_id = condition_id_hov[i]
    LinkID_tmp = unique(FindLinkID(condition_id, LaneStudyArea, LaneNavStrandStudyArea))
    # here we use unique() because the different condition_id
    
    LinkID_index = LinkID_index + length(LinkID_tmp)
    LinkID[(LinkID_index - length(LinkID_tmp) + 1):LinkID_index] = LinkID_tmp
    #if (round(i/100,0) == i/100) 
    cat("the ", i, " output linkID is:", LinkID_tmp, "\n")
    
    for (j in 1:length(LinkID_tmp)){
      # to check if the LinkID has appeared before or not
      bLinkID = sum(LinkID == LinkID[j]) == 1
      if (bLinkID){
        
        # if the linkID hasn't appeared before
        # duplicate the nodes, and add the new link to the link table, then set the original lane -1
        LinkStudyAreaHov$number_of_lanes[which(LinkStudyAreaHov$LINK_ID == LinkID_tmp[j])] = LinkStudyArea$NUM_LANES[which(LinkStudyAreaHov$LINK_ID == LinkID_tmp[j])] - 1
        
        
        LinkTableHovTmp = as.data.frame(LinkStudyAreaHov[which(LinkStudyAreaHov$link_id == LinkID_tmp[j]),])[1,]
        # could have the same link_id for different link, which is the reversiable lanes
        # for example the link_id 1236555997
        # to simplify, we just consider the first link
        
        LinkTableHovTmp$MIN_PASSENGERS = ConditionHovStudyArea$MIN_PASSENGERS[which(ConditionHovStudyArea$CONDITION_ID == condition_id)]
        LinkTableHovTmp$HYBRID_CAR = ConditionHovStudyArea$HYBRID_CAR[which(ConditionHovStudyArea$CONDITION_ID == condition_id)]
        LinkTableHovTmp$MOTORCYCLE = ConditionHovStudyArea$MOTORCYCLE[which(ConditionHovStudyArea$CONDITION_ID == condition_id)]
        LinkTableHovTmp$ALTERNATE_FUEL_CARPOOL = ConditionHovStudyArea$ALTERNATE_FUEL_CARPOOL[which(ConditionHovStudyArea$CONDITION_ID == condition_id)]
        LinkTableHovTmp$FEE_PAY_CARPOOL = ConditionHovStudyArea$FEE_PAY_CARPOOL[which(ConditionHovStudyArea$CONDITION_ID == condition_id)]
        
        LinkTableHovTmp$IS_HOV = 1 # change the attribute of is_hov_lane to 1
        LinkTableHovTmp$number_of_lanes = 1 # suppose now there is only one Hov lane
        
        # find the node id of the from node and to node of original geometry 
        # the output of this FindNodeXandY are like this
        #                    [,1]     [,2]
        # NodeXandYFrom -76.69124 39.36211
        # NodeXandYTo   -76.69501 39.36481
        FROM_N = FindNodeXandY(LinkTableHovTmp$original_geometry[1])[1,]
        TO_N = FindNodeXandY(LinkTableHovTmp$original_geometry[1])[2,]
        
        
        
        # change the node X and Y of the from_node
        LinkTableHovTmp$FROM_N_X = FROM_N[1] - 0.0001
        LinkTableHovTmp$FROM_N_Y = FROM_N[2] - 0.0001
        LinkTableHovTmp_from_node_ori_id = LinkTableHovTmp$from_node_id # we save the ori_from_node_id in order to assign the value to the new artificial link
        LinkTableHovTmp$from_node_id = 2000000000 + i # the new link has a different from_node id, which is the new node that we create
        
        
        # to node id will not change any more
        # LinkTableHovTmp$TO_N_X = LinkStudyArea$TO_N_X[which(LinkStudyArea$LINK_ID == LinkID_tmp[j])] - 0.00001
        # LinkTableHovTmp$TO_N_Y = LinkStudyArea$TO_N_Y[which(LinkStudyArea$LINK_ID == LinkID_tmp[j])] - 0.00001
        
        # assign the new link id to the now node
        # first, we need to define the node id to the new node 
        # find the detailed description in:
        # https://www.evernote.com/shard/s613/sh/d3983128-c9d0-4def-a1f8-29f0392c7faa/77954f40842fc3afd378095bfa05d40c
        
        New_Link_Node_From = 2000000000 + i
        New_Link_Node_To = LinkTableHovTmp_from_node_ori_id
        
        
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
        # the from link
        LinkTableArtificialFrom = GenerateNewLink(New_Link_Node_From, New_Link_Node_To, New_Link_Node_From_X, New_Link_Node_From_Y, New_Link_Node_To_X, New_Link_Node_To_Y, LinkStudyAreaHov)
        
        # the to link
        LinkTableArtificialTo = GenerateNewLink(New_Link_Node_To, New_Link_Node_From, New_Link_Node_To_X, New_Link_Node_To_Y, New_Link_Node_From_X, New_Link_Node_From_Y, LinkStudyAreaHov)
        
        LinksToAdd = rbind(LinksToAdd, LinkTableArtificialFrom, LinkTableArtificialTo)
        
        
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

# the LinkStudyArea that we use should not have duplicated link_IDs
# dim(LinkStudyArea)
# LinkStudyArea = LinkStudyArea[which(duplicated(LinkStudyArea$link_id)==0 | LinkStudyArea$link_id == 0),]
# dim(LinkStudyArea)

LinkStudyAreaWithHovLanes = AddHovLinks(LinkStudyArea, LaneStudyArea, ConditionHovStudyArea, LaneNavStrandStudyArea)

## output the table
write.csv(LinkStudyAreaWithHovLanes, "D:/Public/HERE\ Network\ Table/LinkStudyAreaWithHovLanesDTA.csv")
write.csv(LinkStudyAreaWithHovLanes,"D:/Jun_Zhao/network\ conflation/alexandria_node_link_dtalie/input_link.csv")



###############################################################################################################
### PART 3: CONVERT TO DTALITE INPUT DATA FORMAT #######
###############################################################################################################

# input_link = read.csv("D:/Public/HERE\ Network\ Table/LinkStudyAreaWithHovLanesDTA.csv", header = TRUE)

input_link = LinkStudyAreaWithHovLanes
GenerateInputNodeForDTALite = function(input_link){
  num = dim(input_link)[1]
  input_node_from = data.frame(
    name = character(num),
    node_id = integer(num),
    QEM_reference_node_id = integer(num),
    control_type = integer(num),
    control_type_name = character(num),
    x = double(num),
    y = double(num),
    geometry = character(num)
  )
  
  input_node_to = data.frame(
    name = character(num),
    node_id = integer(num),
    QEM_reference_node_id = integer(num),
    control_type = integer(num),
    control_type_name = character(num),
    x = double(num),
    y = double(num),
    geometry = character(num)
  )
  
  input_node_from$node_id = input_link$from_node_id
  input_node_from$control_type =  rep(0,num)
  input_node_from$control_type_name = rep('unknown_control',num)
  input_node_from$QEM_reference_node_id = rep(0,num)
  input_node_from$x = input_link$FROM_N_X
  input_node_from$y = input_link$FROM_N_Y
  
  input_node_to$node_id = input_link$to_node_id
  input_node_to$control_type =  rep(0,num)
  input_node_to$control_type_name = rep('unknown_control',num)
  input_node_to$control_type_name = rep(0,num)
  input_node_to$x = input_link$TO_N_X
  input_node_to$y = input_link$TO_N_Y
  
  dim(input_node_from)
  dim(input_node_to)
  
  # we should delete these nodes without x and y information
  input_node_from = input_node_from[!is.na(input_node_from$x),]
  input_node_to = input_node_to[!is.na(input_node_to$x),]
  
  
  
  input_node = rbind(input_node_from, input_node_to)
  
  input_node = input_node[which(duplicated(input_node$node_id)==0),]
  
  return(input_node)
}

input_node = GenerateInputNodeForDTALite(input_link)



# the following code is used to check if there are two nodes sharing same x and y
XYString = apply(input_node[,c("x","y")],1,paste,collapse = "-")
input_node$XYString = XYString

input_node = input_node[which(duplicated(XYString) == 0),]

write.csv(input_node,"D:/Jun_Zhao/network\ conflation/alexandria_node_link_dtalie/input_node.csv")

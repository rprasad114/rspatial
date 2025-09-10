# Function to create buffer rings
# Updated by Shiguo Jiang, 2024.4.12
#
# Inputs:     pt: a point. e.g., pt=BRCenter
#          dists: a sequence of distances. e.g., dists = seq(2000, 24000, 2000)
#
# Output:  rings: simple feature of concentric rings

st_buffer_rings = function(pt, dists)
{
  pacman::p_load(sf)
  n = length(dists)
  
  # initialize an empty list
  ringList = vector('list', n)
  
  # create buffer
  rings = st_buffer(pt, dists[1])
  
  # add a column named as distance to the buffer
  rings$distance = dists[1]
  
  # extract only the distance column along with the geomeetry
  rings = rings['distance']
  
  # store the buffer to the list
  ringList[[1]] = rings
  
  for (i in 2:n) # i=2
  {
    ringT = st_buffer(pt, dists[i])
    ringT$distance = dists[i]
    ringT = ringT['distance']
    ringList[[i]] = ringT
    
    # When st_difference() is called, it sends warnings: "attribute variables are
    # assumed to be spatially constant throughout all geometries".
    # Since this is the case. We want to suppress the warnings.
    
    suppressWarnings({ringD = st_difference(ringT, ringList[[i-1]])})
    
    ringD = ringD['distance']
    rings = rbind(rings, ringD)
  }
  
  return(rings)
}

# Find centroids summary of polygons.
#
# reference:
# https://stackoverflow.com/questions/52522872/r-sf-package-centroid-within-polygon
# https://stackoverflow.com/questions/44327994/calculate-centroid-within-inside-a-spatialpolygon
#
# Inputs:           x: (sf) spatial polygon
#       ensure_within: (logical) TRUE to ensure point within polygon.
#  of_largest_polygon: (logical) TRUE to use largest polygon when 
#                      determining centroid.
#
# Output:          cx: (sf) spatial object with centroids within spatial feature.
#
# Will return the usual geometric centroid when `ensure_within` is FALSE.
#
# Otherwise the point returned will be the geometric centroid if it lies within
# the polygon else a point determined by `st_point_on_surface`.
#
# of_largest_polygon used when determining geometric centroid. 
# An effort to apply this idea to st_point_on_surface() routine.
#
# Based on https://github.com/r-spatial/sf/issues/1302#issuecomment-606473789
#
# Updated by Shiguo Jiang on 2024.4.13

st_centroids = function(x, ensure_within=TRUE, of_largest_polygon = FALSE)
{
  pacman::p_load(sf, dplyr)
  
  st_largest_ring = function(x)
  {
    pacman::p_load(sf, dplyr)
    
    if (nrow(x) < 1) return(x)
    
    seq(1, nrow(x)) %>%
      lapply(function(i){
        x[i, ] %>%
          st_set_agr("identity") %>%
          st_cast("POLYGON") %>%
          mutate(st_area = st_area(st_geometry(.))) %>%
          top_n(1, st_area) %>%
          select(-st_area)
      }) %>%
      data.table::rbindlist() %>%
      st_as_sf()
  }
  
  cx = st_centroid(st_set_agr(x, "identity"), 
                   of_largest_polygon = of_largest_polygon)
  
  if (ensure_within)
  {
    i_within = st_within(cx, x, sparse = TRUE) %>%
      as.data.frame() %>%
      filter(row.id == col.id) %>%
      pull(row.id)
    
    i_nwithin = setdiff(seq(1, nrow(x)), i_within)
    
    st_geometry(cx[i_nwithin, ]) = (x[i_nwithin, ] %>% {
        if (of_largest_polygon == TRUE)
          st_largest_ring(.)
        else
          .
      } %>%
        st_geometry() %>%
        st_point_on_surface()
    )
  }
  
  return(cx)
}
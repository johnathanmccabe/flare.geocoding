
#' This function allows the postcode.io/postcodes endpoint to be called directly from R
#'
#' @param postcodes A character vector of postcodes for which geocoding information is saught.
#' @return A data frame containing geocoding information. For more details see http://postcodes.io/
#' Only valid postcodes will have rows returned so there may be a different number of rows in the output than input.
#' If no valid postcodes are found then the function returns NULL.
#' @details The returned data frame will contain the following fields
#' postcode, quality, eastings, northings, country, nhs_ha, longitude, latitude,
#' parliamentary_constituency, european_electoral_region, primary_care_trust, region,
#' lsoa, msoa, incode, outcode, admin_district, parish, admin_county, admin_ward, ccg,
#' nuts, codes.admin_district, codes.admin_county, codes.admin_ward, codes.parish, codes.ccg, codes.nuts
#'
#' The httr package is used as the underlying transport for retrieving the data so please ensure that this has been configured correctly.
#' e.g. httr::set_config(ssl_verifypeer = 0L) may need to be called
#'
#' Only one row is returned for each valid postcode (i.e. no duplicates)
#' @examples postcodes(c("BR1 4AT"))
geocode_from_postcode <- function(post_codes){

  #' Description Cleans the results list from poscodes.io
  #'
  #' @param result_list The result list returned from the API
  #' @return A cleaned list with nulls replaced with NA
  #' @details Replaces null with NA
  #' @examples
  clean_result_list <- function(result_list){

    r <- lapply(1:length(result_list), function(i){
      if(is.null(result_list[[i]])){
        NA
      } else {
        result_list[[i]]
      }
    })


    names(r) <- names(result_list)

    r$codes <- lapply(1:length(r$codes), function(i){
      if(is.null(r$codes[[i]])){
        NA
      } else {
        r$codes[[i]]
      }
    })

    names(r$codes) = names(result_list$codes)

    retVal = as.data.frame(r, stringsAsFactors = F)

    return(retVal)
  }


  request_url = "https://api.postcodes.io/postcodes/"

  #query for unique postcodes only then rejoin at the end
  p <- unique(post_codes)


  ## need to batch requests as the maximum that can be requested at once is 100
  n_batch <- ceiling(length(p)/100)


  retVal <- do.call("rbind", lapply(1:n_batch, function(n){

    batch_postcodes <- post_codes[(100*(n-1)+1):(min(length(p), 100*n))]

    if(length(batch_postcodes) == 1){
      geo_response = httr::GET(url = paste0(request_url, as.character(batch_postcodes[1])))
    } else {
      postcode_list <- list(postcodes = batch_postcodes)

      geo_response <- httr::POST(url = request_url,
                                 body = postcode_list,
                                 encode = "json",
                                 httr::authenticate("","","basic"))
    }

    geo_content <- httr::content(geo_response)

    ## transform into data.table
    if(length(batch_postcodes) == 1){
      r <- geo_content$result

      if(is.null(r$postcode)){
        retVal = NULL
      } else {
        retVal = clean_result_list(r)
      }
    } else {
      l <- lapply(1:length(geo_content$result), function(i){
        r <- geo_content$result[[i]]$result

        retVal = clean_result_list(r)

        return(retVal)
      })

      retVal <- base::Map(as.data.frame, l[!sapply(l,function(a){is.null(a$postcode)})])
      retVal <- data.table::rbindlist(retVal, fill = T)
    }

    return(retVal)
  }))

  return(retVal)
}



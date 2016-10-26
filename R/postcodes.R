
#' This function allows the postcode.io/postcodes endpoint to be called directly from R
#'
#' @param postcodes A character vector of postcodes for which geocoding information is saught.
#' @return A data frame containing geocoding information. For more details see http://postcodes.io/
#' @details The returned data frame will contain the following fields
#' postcode, quality, eastings, northings, country, nhs_ha, longitude, latitude,
#' parliamentary_constituency, european_electoral_region, primary_care_trust, region,
#' lsoa, msoa, incode, outcode, admin_district, parish, admin_county, admin_ward, ccg,
#' nuts, codes.admin_district, codes.admin_county, codes.admin_ward, codes.parish, codes.ccg, codes.nuts
#' @examples postcodes(c("BR1 4AT"))
postcodes <- function(post_codes){

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

    retVal = as.data.frame(r)

    return(retVal)
  }


  httr::set_config(httr::config(ssl_verifypeer = 0L))

  request_url = "https://api.postcodes.io/postcodes/"

  if(length(post_codes) == 1){
    geo_response = httr::GET(url = paste0(request_url, as.character(post_codes[1])))
  } else {
    postcode_list <- list(postcodes = post_codes)

    geo_response <- httr::POST(url = request_url,
                               body = postcode_list,
                               encode = "json")
  }

  geo_content <- httr::content(geo_response)

  ## transform into data.table
  if(length(post_codes) == 1){
    r <- geo_content$result

    retVal = clean_result_list(r)

  } else {
    l <- lapply(1:length(geo_content), function(i){
      r <- geo_content$result[[i]]$result

      retVal = clean_result_list(r)

      return(retVal)
    })

    retVal <- base::Map(as.data.frame, l)
    retVal <- data.table::rbindlist(retVal)
  }



  httr::reset_config()

  return(retVal)
}



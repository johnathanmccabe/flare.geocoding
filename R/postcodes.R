
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

    result_list <- lapply(1:length(r), function(i){
      if(is.null(r[[i]])){
        NA
      } else {
        r[[i]]
      }
    })

    names(result_list) <- names(r)

    retVal = as.data.frame(result_list)

  } else {
    l <- lapply(1:length(geo_content), function(i){
      r <- geo_content$result[[i]]$result

      retVal <- lapply(1:length(r), function(i){
        if(is.null(r[[i]])){
          NA
        } else {
          r[[i]]
        }
      })

      names(retVal) <- names(r)

      return(retVal)
    })

    retVal <- base::Map(as.data.frame, l)
    retVal <- data.table::rbindlist(retVal)
  }



  httr::reset_config()

  return(retVal)
}

post_codes = "TW135dq"
post_codes = c("TW135dq", "BT323sz")


postcodes(post_codes)


xx <- colnames(postcodes(post_codes))


# see http://stackoverflow.com/questions/32035119/how-to-solve-clipboard-buffer-is-full-and-output-lost-error-in-r-running-in-wi
write.table(xx, "clipboard-16384", sep="\t", row.names=FALSE)

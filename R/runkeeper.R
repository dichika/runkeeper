#' @export
getToken <- function(app_name=NULL, key=NULL, secret=NULL){
  app_name <- ifelse(is.null(app_name), options()$HEALTH_GRAPH_APPNAME, app_name)
  key <- ifelse(is.null(key), options()$HEALTH_GRAPH_KEY, key)
  secret <- ifelse(is.null(secret), options()$HEALTH_GRAPH_SECRET, secret)
  app <- httr::oauth_app(app_name, key = key, secret = secret)
  endpoints <- httr::oauth_endpoint(authorize="authorize",
                                    access="token",
                                    base_url="https://runkeeper.com/apps")
  token <- httr::oauth2.0_token(endpoints,app)  
}

#' @export
getFitnessActivityFeed <- function(token){
  accept <- "application/vnd.com.runkeeper.FitnessActivityFeed+json"
  url <- "https://api.runkeeper.com/fitnessActivities/"
  header <- httr::add_headers(
    Host = "api.runkeeper.com",
    Authorization = sprintf("Bearer %s", token$credentials$access_token),
    Accept = accept
  )
  cont <- httr::content(httr::GET(url, header), as="text")
  jsonlite::fromJSON(cont)
}

#' @export
getFitnessActivity <- function(num, token){
  accept <- "application/vnd.com.runkeeper.FitnessActivity+json"
  url <- sprintf("https://api.runkeeper.com/fitnessActivities/%s", num)
  header <- httr::add_headers(
    Host = "api.runkeeper.com",
    Authorization = sprintf("Bearer %s", token$credentials$access_token),
    Accept = accept
  )
  cont <- httr::content(httr::GET(url, header), as="text")
  jsonlite::fromJSON(cont)
}


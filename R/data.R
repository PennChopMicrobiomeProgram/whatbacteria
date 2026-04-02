#' Gram stain and aerobic status of bacterial taxa
#' @format A data frame with the following columns:
#' \describe{
#'   \item{taxon}{The name of the taxon}
#'   \item{rank}{The rank of the taxon}
#'   \item{aerobic_status}{
#'     The aerobic status. One of "aerobe", "facultative anaerobe", or
#'     "obligate anaerobe".}
#'   \item{gram_stain}{
#'     How the taxon appears when Gram-stained. One of "Gram-positive" or
#'     "Gram-negative".}
#'   \item{doi}{DOI of the publication from which the information was obtained.}
#' }
"taxon_phenotypes"

#' Antibiotic susceptibility of bacterial taxa
#' @format A data frame with the following columns:
#' \describe{
#'   \item{taxon}{The name of the taxon}
#'   \item{rank}{The rank of the taxon}
#'   \item{antibiotic}{The antibiotic or antibiotic class}
#'   \item{value}{
#'     The susceptibility of the taxon to the antibiotic, one of "susceptible"
#'     or "resistant".}
#'   \item{doi}{DOI of the publication from which the information was obtained.}
#' }
"taxon_susceptibility"

#' Synonyms for bacterial taxa
#' @format A data frame with the following columns:
#' \describe{
#'   \item{name}{The synonym}
#'   \item{rank}{The rank of the taxon}
#'   \item{correct_name}{The "correct" name for the taxon, as it is used in
#'     other package data.}
#' }
"taxon_synonyms"
#' Fetch susceptibility values for a given lineage and antibiotic
#'
#' @param lineage A character vector of taxonomic assignments or lineages
#' @param antibiotic The name of the antibiotic or antibiotic class in \code{db}
#' @param db A data frame with columns named "taxon", "rank", "antibiotic",
#'   and "value"
#' @param synonyms A data frame of taxonomic synonyms with columns "name" and
#'   "correct_name"
#' @return A vector of assigned susceptibility values, which should be either
#'   "susceptible", "resistant", or \code{NA}
#' @details
#' To determine susceptibility, the database is first filtered to include only
#' rows relevant to the antibiotic of interest. Then, the filtered database is
#' split into ranks. The susceptibility values are determined for each rank by
#' matching taxa from the rank-specific database to the vector of lineages.
#' If a lineage matches to multiple taxa of different ranks, the value of the
#' taxon with the lowest rank is selected.
#'
#' The taxonomic ranks, in order from highest to lowest, are Kingdom, Phylum,
#' Class, Order, Family, Genus, and Species. The ranks in the database must be
#' capitalized, exactly as they are written here.
#' @examples
#' what_antibiotic(
#'   c("Enterococcus faecalis", "Lactobacillus", "Lactobacillus delbrueckii"),
#'   "vancomycin")
#' @export
what_antibiotic <- function (lineage,
                             antibiotic,
                             db = whatbacteria::taxon_susceptibility,
                             synonyms = whatbacteria::taxon_synonyms) {
  is_relevant <- db$antibiotic %in% antibiotic
  db <- db[is_relevant, c("taxon", "rank", "value")]

  susceptibility_values <- match_annotation(lineage, db, synonyms)
  susceptibility_values
}

#' Fetch the phenotype values for a given lineage and phenotype
#'
#' @param lineage A character vector of taxonomic assignments or lineages
#' @param phenotype The name of the column in \code{db} that contains the
#'   phenotype of interest
#' @param db A data frame with columns named "taxon", "rank", and the column
#'   name specified in \code{phenotype}
#' @param synonyms A data frame of taxonomic synonyms with columns "name" and
#'   "correct_name"
#' @return A vector of assigned phenotype values
#' @details
#' This function operates much like \code{antibiotic_susceptibility}, except
#' that it pulls phenotype values from the database instead of susceptibility
#' information. To subsequently determine susceptibility from phenotype, this
#' function uses the named vector provided in the \code{susceptibility}
#' argument.
#'
#' As a reminder, the taxonomic ranks, in order from highest to lowest, are
#' Kingdom, Phylum, Class, Order, Family, Genus, and Species. The ranks in the
#' database must be capitalized, exactly as they are written here.
#' @examples
#' what_phenotype(
#'   c("Bacteroidetes", "Firmicutes", "Firmicutes; Negativicutes"),
#'   "gram_stain")
#' @export
what_phenotype <- function (lineage,
                            phenotype,
                            db = whatbacteria::taxon_phenotypes,
                            synonyms = whatbacteria::taxon_synonyms) {
  db <- db[, c("taxon", "rank", phenotype)]
  # match_annotation() requires a column named "value"
  colnames(db)[3] <- "value"
  match_annotation(lineage, db, synonyms)
}

#' Determine the annotation values for each lineage
#'
#' @param lineage A vector of taxonomic assignments or lineages
#' @param db A data frame with columns named "taxon", "rank", and "value"
#' @param synonyms A data frame of with columns "name" and "correct_name"
#' @return A vector of assigned values
#'
#' @export
match_annotation <- function (lineage, db, synonyms = NULL) {
  lineage_vectors <- prepare_lineage(lineage, synonyms = synonyms)
  get_rank_specific_db <- function (r) {
    rank_is_r <- db[["rank"]] %in% r
    db[rank_is_r, ]
  }
  db_ranks <- lapply(rev(taxonomic_ranks), get_rank_specific_db)
  names(db_ranks) <- rev(taxonomic_ranks)

  get_values_by_rank <- function (rank_specific_db) {
    taxa_idx <- match_split_lineage_taxa(lineage_vectors, rank_specific_db[["taxon"]])
    rank_specific_db[["value"]][taxa_idx]
  }
  values_by_rank <- vapply(
    db_ranks,
    get_values_by_rank,
    rep("a", length(lineage_vectors)))

  if (length(lineage_vectors) == 1) {
    assigned_values <- first_non_na_value(values_by_rank)
  } else {
    assigned_values <- apply(values_by_rank, 1, first_non_na_value)
  }
  assigned_values
}

# The 'official' taxonomic ranks supported by this package
taxonomic_ranks <- c(
  "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

# Return the first value that is not NA. If all values are NA, return NA. The
# resultant vector will not have names.
first_non_na_value <- function (x) {
  unname(x[first_true_idx(!is.na(x))])
}

# For each lineage, return the index of the taxon that is found within the
# lineage. If no taxa are found, return NA for that element. If multiple taxa
# are found, we issue a warning and return the index of the first taxon in the
# vector of taxa.
match_taxa <- function (lineages, taxa) {
  n_lineages <- length(lineages)
  if (length(taxa) == 0) {
    return(rep_len(NA_character_, length(lineages)))
  }

  taxa_patterns <- paste0("(?<=__|\\b)(?:", taxa, ")\\b")
  lineage_matches <- vapply(
    X = taxa_patterns,
    FUN = grepl,
    FUN.VALUE = rep_len(TRUE, n_lineages),
    x = lineages,
    perl = TRUE,
    USE.NAMES = TRUE)

  # If the user passes only one lineage, lineage_matches will be a vector
  # rather than an array. After some trial and error, I found that it's better
  # to deal with this at each stage of the computation, rather than trying to
  # coerce the vector to an array up front.
  if (n_lineages == 1) {
    multi_matches <- sum(lineage_matches) > 1
  } else {
    multi_matches <- rowSums(lineage_matches) > 1
  }
  if (any(multi_matches)) {
    warning(
      "The following lineages match more than one taxon:\n",
      paste(lineages[multi_matches], collapse = "\n"), "\n")
  }

  if (n_lineages == 1) {
    taxon_idx <- first_true_idx(lineage_matches)
  } else {
    taxon_idx <- apply(lineage_matches, 1, first_true_idx)
  }
  taxon_idx
}

#' Return the first index of a boolean vector that is TRUE. If all elements of
#' the vector are FALSE, return NA. Tempted to call this function minwhich.
#'
#' @param x A logical vector
#' @return index of first true in vector or NA
first_true_idx <- function (x) {
  if (any(x)) {
    min(which(x == TRUE))
  } else {
    NA_integer_
  }
}

split_lineage_noranks <- function(lineage, pattern = "(; ?)|( - )") {
  strsplit(lineage, split = pattern, perl = TRUE)
}

clean_taxa <- function(taxa) {
  # Remove rank prefix
  taxa <- sub("[kpcofgsx]__", "", taxa)
  # Remove brackets from genus names
  taxa <- gsub("\\[(\\w+)\\]", "\\1", taxa)
  # Remove leading and trailing whitespace
  taxa <- trimws(taxa)
  taxa
}

resolve_taxa <- function(name, synonyms = whatbacteria::taxon_synonyms) {
  if (is.null(synonyms)) {
    return(name)
  }
  synonym_idx <- match(tolower(name), tolower(synonyms$name))
  ifelse(
    !is.na(synonym_idx),
    synonyms$correct_name[synonym_idx],
    name
  )
}

match_split_lineage_taxa <- function(lineage_vectors, taxa) {
  # Convert to lowercase for case-insensitive matching
  lineage_vectors_lc <- lapply(lineage_vectors, tolower)
  taxa_lc <- tolower(taxa)
  taxa_match_idxs <- lapply(lineage_vectors_lc, match, table = taxa_lc)
  # If multiple elements in the lineage match to a taxon, we issue a warning.
  is_multimatch <- vapply(taxa_match_idxs, function(x) sum(!is.na(x)) > 1, FUN.VALUE = TRUE)
  if (any(is_multimatch)) {
    warn_multimatch(
      lineage_vectors[is_multimatch],
      taxa_match_idxs[is_multimatch],
      taxa
    )
  }
  # If multiple taxa are matched for a single lineage, we take the first
  # (highest-ranking) taxon match
  first_taxa_matches <- vapply(taxa_match_idxs, first_non_na_value, FUN.VALUE = 1)
  first_taxa_matches
}

warn_multimatch <- function (multimatch_lineages, multimatch_idxs, taxa) {
  lineage_toprint <- lapply(multimatch_lineages, paste, collapse = "; ")

  multimatch_idxs <- lapply(multimatch_idxs, function (x) x[!is.na(x)])
  multimatch_taxa_names <- lapply(multimatch_idxs, function (x) taxa[x])
  taxa_toprint <- lapply(multimatch_taxa_names, paste, collapse = ", ")

  message_details <- paste(
    "Lineage",
    lineage_toprint,
    "matches multiple taxa of the same rank:",
    taxa_toprint,
    collapse = "\n")
  message <- paste(
    "Multiple taxa matched for one or more lineages:",
    message_details,
    collapse = "\n"
  )
  warning(message)
}

prepare_lineage <- function (x, synonyms = whatbacteria::taxon_synonyms) {
  x |>
    split_lineage_noranks() |>
    lapply(clean_taxa) |>
    lapply(resolve_taxa, synonyms)
}

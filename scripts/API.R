library(httr)
library(jsonlite)

# Define SPARQL query
sparql_query <- 'SELECT ?city ?cityLabel ?cityLabelNl ?cityLabelFr ?cityLabelDe ?cityLabelDa ?countryLabel ?population ?sitelinks WHERE {\n  ?city wdt:P31/wdt:P279* wd:Q515;\n        wdt:P17 ?country.\n  FILTER(?country IN (wd:Q31, wd:Q55, wd:Q35, wd:Q183))\n  \n  OPTIONAL { ?city wdt:P1082 ?population. }\n  ?city wikibase:sitelinks ?sitelinks.\n  \n  OPTIONAL { ?city rdfs:label ?cityLabelNl FILTER (lang(?cityLabelNl) = "nl") }\n  OPTIONAL { ?city rdfs:label ?cityLabelFr FILTER (lang(?cityLabelFr) = "fr") }\n  OPTIONAL { ?city rdfs:label ?cityLabelDe FILTER (lang(?cityLabelDe) = "de") }\n  OPTIONAL { ?city rdfs:label ?cityLabelDa FILTER (lang(?cityLabelDa) = "da") }\n  \n  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }\n}\nORDER BY DESC(?sitelinks)'

# Set endpoint and send request
res <- GET(
  url = "https://query.wikidata.org/sparql",
  add_headers(Accept = "application/sparql-results+json"),
  query = list(query = sparql_query)
)

# Parse JSON
json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))

# Extract results into data.frame
df <- json$results$bindings

df <- df[!duplicated(df$city$value), ]

cities <- data.frame(
  city = df$cityLabel$value,
  city_nl = df$cityLabelNl$value,
  city_fr = df$cityLabelFr$value,
  city_de = df$cityLabelDe$value,
  city_da = df$cityLabelDa$value,
  country = df$countryLabel$value,
  population = as.numeric(df$population$value),
  sitelinks = df$sitelinks$value,
  stringsAsFactors = FALSE
)

# Show results
head(cities)

city_variants <- unique(na.omit(unlist(
  cities[, c("city", "city_fr", "city_nl", "city_de", "city_da")]
)))

match_city <- function(alt_names, city_list, max_dist = 2) {
  alt_names <- alt_names[!is.na(alt_names) & nzchar(alt_names)]
  if (length(alt_names) == 0) return(NA)
  
  # Try exact match first
  exact_match <- alt_names[alt_names %in% city_list]
  if (length(exact_match) > 0) return(exact_match[1])
  
  # Fuzzy match
  for (alt in alt_names) {
    dists <- stringdist(tolower(alt), tolower(city_list), method = "lv")
    if (any(!is.na(dists) & dists <= max_dist)) {
      return(city_list[which.min(dists)])
    }
  }
  
  return(NA)
}

alt_names_split <- strsplit(spatial_pop_clean$Alternative_Name, ";")
alt_names_trimmed <- lapply(alt_names_split, trimws)

spatial_pop_clean$matched_city <- sapply(alt_names_trimmed, match_city, city_list = city_variants)

library(stringdist)
library(dplyr)

city_names <- unique(cities$city)

# Split semicolon-separated names into lists
alt_names_split <- strsplit(spatial_pop_clean$Alternative_Name, ";")
alt_names_trimmed <- lapply(alt_names_split, trimws)

match_city <- function(alt_names, city_list, max_dist = 1) {
  # Remove NA or empty alt names
  alt_names <- alt_names[!is.na(alt_names) & nzchar(alt_names)]
  if (length(alt_names) == 0) return(NA)
  
  # First: check for exact match
  exact_matches <- alt_names[alt_names %in% city_list]
  if (length(exact_matches) > 0) return(exact_matches[1])
  
  # Otherwise: fuzzy match (Levenshtein)
  for (alt in alt_names) {
    dists <- stringdist(tolower(alt), tolower(city_list), method = "lv")
    if (any(!is.na(dists) & dists <= max_dist)) {
      return(city_list[which.min(dists)])
    }
  }
  
  return(NA)  # No match
}


# Apply matching function to all rows
spatial_pop_clean$matched_city <- sapply(alt_names_trimmed, match_city, city_list = city_names)

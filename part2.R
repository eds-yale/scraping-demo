library(data.table)
library(reshape2)
library(plyr)
library(stringr)
library(rvest)
library(tidyverse)

options (stringsAsFactors = FALSE)

#read in links gathered with Python
links <- read.csv("links.csv")

#delete erroneous link
links <- links[!links$link == "http://fts.unocha.org",]

#prime a data frame for running loop, with columns corresponding to link url, project title, and each keyword
projlinks <- as.data.frame(matrix(nrow = length(links), ncol = 25))
colnames(projlinks) <- c("link", "projtitle", "risk", "disaster", "recovery", "climate", "climate_change", "reconstruction",
                         "early_warning", "vulnerability", "adaptation", "mitigation", "national_society", "resilience",
                         "sustainable", "drought", "food_security", "shocks", "resilient", "flood_migration", "environmental",
                         "famine", "renewable", "land_management", "seasonal")
projlinks$link <- links

#build in random sleep command to mimic human activity
randsleep <- rnorm(nrow(projlinks), mean = 5, sd = 3)
randsleep <- abs(randsleep)

#loop to count keywords
# ***WORTH CHECKING IF THINGS LIKE "SEASONALLY" ARE ACCEPTABLE, PRESENTLY ARE BEING COUNTED
for(i in 1:15){
  webtext = read_html(projlinks$link[i]) %>%
    html_text(trim = TRUE)
  projlinks$projtitle[i] <- str_extract(webtext, "(?<=Project Title)(.)*(?=Project Code)")
  projlinks$risk[i] <- str_count(webtext, "(?i)risk")
  projlinks$disaster[i] <- str_count(webtext, "(?i)disaster")
  projlinks$recovery[i] <- str_count(webtext, "(?i)recovery")
  projlinks$climate[i] <- str_count(webtext, "(?i)climate")
  projlinks$climate_change[i] <- str_count(webtext, "(?i)climate change")
  projlinks$reconstruction[i] <- str_count(webtext, "(?i)reconstruction")
  projlinks$early_warning[i] <- str_count(webtext, "(?i)early warning")
  projlinks$vulnerability[i] <- str_count(webtext, "(?i)vulnerability")
  projlinks$adaptation[i]  <- str_count(webtext, "(?i)adaptation")
  projlinks$mitigation[i] <- str_count(webtext, "(?i)mitigation")
  projlinks$national_society[i] <- str_count(webtext, "(?i)national society")
  projlinks$resilience[i] <- str_count(webtext, "(?i)resilience")
  projlinks$sustainable[i] <- str_count(webtext, "(?i)sustainable")
  projlinks$drought[i] <- str_count(webtext, "(?i)drought")
  projlinks$food_security[i] <- str_count(webtext, "(?i)food security")
  projlinks$shocks[i] <- str_count(webtext, "(?i)shocks")
  projlinks$resilient[i] <- str_count(webtext, "(?i)resilient")
  projlinks$flood_migration[i] <- str_count(webtext, "(?i)flood migration")
  projlinks$environmental[i] <- str_count(webtext, "(?i)environmental")
  projlinks$famine[i] <- str_count(webtext, "(?i)famine")
  projlinks$renewable[i] <- str_count(webtext, "(?i)renewable")
  projlinks$land_management[i] <- str_count(webtext, "(?i)land management")
  projlinks$seasonal[i] <- str_count(webtext, "(?i)seasonal")
  Sys.sleep(randsleep[i])
  print(paste(Sys.time(), "--", i, "iteration"))
}

#write out
write.csv(projlinks, "keywords by project (scraped).csv", row.names = FALSE)


#create small summary
keywordmelt <- melt(projlinks, id.vars = c("link", "projtitle"), variable.name = "keyword", value.name = "count")

keywordmelt$type <- ifelse(keywordmelt$keyword %in% c("risk", "disaster", "recovery", "climate", "climate_change", "reconstruction",
                                                      "early_warning", "vulnerability", "adaptation", "mitigation", "national_society"),
                           "ICRC", "Inducted")

keywordsumm <-
  group_by(keywordmelt, keyword, type) %>%
  summarize(
    n.mention = length(which(count > 0)),
    prop.mention = n.mention/nrow(projlinks),
    avg.mention = mean(count),
    max.mention = max(count)
  )

write.csv(keywordsumm, "keywords summary.csv", row.names = FALSE)

library(ggplot2)

ggplot(keywordmelt, aes(x = keyword, y = count)) +
  geom_boxplot(alpha = 0.3) +
  coord_flip()

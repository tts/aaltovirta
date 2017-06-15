library(dplyr)
library(shiny)
library(shinydashboard)
library(ggvis)
library(DT)
library(rCharts)
library(rdrop2)
library(plotly)
library(sparkline)

dataForCharts <- read.csv("toshiny_altm_2016_2017.csv", 
                          stringsAsFactors = F,
                          header = T,
                          encoding = "UTF-8")


dataForCharts$isoadoi <- sapply(dataForCharts$X_best_open_url, function(x){
  ifelse(!is.na(x), 1, 0)
})
 
names(dataForCharts)[names(dataForCharts) == 'fields'] <- 'Field'

dataForCharts$AltmetricScore <- ceiling(dataForCharts$AltmetricScore)
dataForCharts[dataForCharts == 0] <- NA

dataForCharts$CitesSeries <- paste(ifelse(!is.na(dataForCharts$Citations),
                                                   dataForCharts$Citations, 
                                                   0),
                                            ifelse(!is.na(dataForCharts$Citations2017), 
                                                   dataForCharts$Citations2017,
                                                   0), sep = ",")

dataForCharts$TweetsSeries <- paste(ifelse(!is.na(dataForCharts$Twitter),
                                               dataForCharts$Twitter, 
                                               0),
                                        ifelse(!is.na(dataForCharts$Twitter2017), 
                                               dataForCharts$Twitter2017,
                                               0), sep = ",")

dataForCharts$MendeleySeries <- paste(ifelse(!is.na(dataForCharts$Mendeley),
                                               dataForCharts$Mendeley, 
                                               0),
                                        ifelse(!is.na(dataForCharts$Mendeley2017), 
                                               dataForCharts$Mendeley2017,
                                               0), sep = ",")

dataForCharts <- dataForCharts %>% 
  mutate(CitesChange = ifelse(is.na(Citations2017), 0, Citations2017) - ifelse(is.na(Citations), 0, Citations),
         TweetsChange = ifelse(is.na(Twitter2017), 0, Twitter2017) - ifelse(is.na(Twitter), 0, Twitter),
         MendeleyChange = ifelse(is.na(Mendeley2017), 0, Mendeley2017) - ifelse(is.na(Mendeley), 0, Mendeley))


heatmap_all <- read.csv("heatmap_allschools.csv", stringsAsFactors = F, row.names = 1)
heatmap_arts <- read.csv("heatmap_arts.csv", stringsAsFactors = F, row.names = 1)
heatmap_biz <- read.csv("heatmap_biz.csv", stringsAsFactors = F, row.names = 1)
heatmap_chem <- read.csv("heatmap_chem.csv", stringsAsFactors = F, row.names = 1)
heatmap_elec <- read.csv("heatmap_elec.csv", stringsAsFactors = F, row.names = 1)
heatmap_eng <- read.csv("heatmap_eng.csv", stringsAsFactors = F, row.names = 1)
heatmap_sci <- read.csv("heatmap_sci.csv", stringsAsFactors = F, row.names = 1)

# Plotly conf
f2 <- list(
  size = 9
)

ax <- list(
  title = "",
  tickangle = 45,
  tickfont = f2
)

ay <- list(
  title = "",
  tickfont = f2
)

m <- list(
  l = 300,
  r = 10,
  b = 150,
  t = 50,
  pad = 6
)

w <- 800
h <- 600

metrics <- sort(c("AltmetricScore", "NrOfAuthors", "Mendeley", "Twitter", "Facebook", 
                  "GooglePlus", "CiteULike",
                  "AnyTypeOfPosts", "Blogs", "DeliciousUsers",
                  "YouTubeChannels", "RedditUsers", "NewsOutlets", "StackExchange", "ForumUsers", 
                  "ResearchHighlightPlatforms", "SinaWeiboUsers", "Citations"))

scales <- c("linear", "log")

schools <- c("ARTS", "BIZ", "CHEM", "ELEC", "ENG", "SCI")

# Stats of All schools. In case there are coauthorship between Schools, 
# duplicates are excluded
uniquedois <- dataForCharts[!duplicated(dataForCharts[,1]), ]
nrow_uniquedois <- nrow(uniquedois)
with_altmetrics_uniquedois <- paste0(nrow(uniquedois[!is.na(uniquedois$AltmetricScore),]),
                                     " (",
                                     floor(nrow(uniquedois[!is.na(uniquedois$AltmetricScore),]) / nrow_uniquedois * 100),
                                     "%)")

with_oadoi_uniquedois <- paste0(nrow(uniquedois[!is.na(uniquedois$X_best_open_url),]),
                                " (",
                                floor(nrow(uniquedois[!is.na(uniquedois$X_best_open_url),]) / nrow_uniquedois * 100),
                                "%)")


datadate <- "2016-05-13"
datadate2017 <- "2017-05-30"
citedate <- "2016-05-13"
oadate <- "2017-05-29"

# DT sparkline
# https://stackoverflow.com/a/41480619
line_string <- "type: 'line', lineColor: 'black', fillColor: '#ccc', highlightLineColor: 'orange', highlightSpotColor: 'orange', width: 80, height: 60"

cb <- JS(paste0("function (oSettings, json) {\n  $('.sparkSamples:not(:has(canvas))').sparkline('html', { ",
                line_string, " });\n}"), collapse = "")


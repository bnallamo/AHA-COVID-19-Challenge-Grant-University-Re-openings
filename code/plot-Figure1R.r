library("knitr")
knitr::opts_chunk$set(fig.align="center", fig.width=6, fig.height=6)
options(width=90)
require(latticeExtra) # USCancerRates, mapplot
require(maps) # map
require(classInt) # classIntervals, findCols
require(grid) # viewport, pushViewport
require(pals) # brewer.blues, stevens.pinkgreen

## log confirmed case (Figure1R)
data = read.csv("..\\data\\data_for_map.csv")
stateName = read.csv("..\\data\\stateCode.csv")
colnames(stateName) = c("state", "state_Code")
dataForPlot = data.frame(state_Code = data$state, county = data$county, x = log(data$totalnow * (data$TOT_POP / 10000) + 1))

dataForPlot$county = as.vector(dataForPlot$county)
dataForPlot$county[1276] = "st clair"
dataForPlot$county[1277] = "st joseph"

dataForPlot = merge(dataForPlot, stateName, by = "state_Code", all.x = T)
rownames(dataForPlot) = paste(tolower(dataForPlot$state), dataForPlot$county, sep = ",")


m3 <- mapplot(rownames(dataForPlot) ~ x, data = dataForPlot,
              colramp=brewer.reds,
              map = map("county", plot = FALSE, fill = TRUE,
                        projection = "tetra"),
              scales = list(draw = FALSE),
              breaks=classIntervals(dataForPlot$x, n=10, style='quantile')$brks, xlab = "log confirmed cases : Sept. 1 - Oct. 22", cex.main=5)
suppressWarnings(print( m3 ))
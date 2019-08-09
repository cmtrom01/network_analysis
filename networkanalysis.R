##Network analysis in R using mlVAR and qgraph

##import libraries
library(mlVAR)
library(qgraph)

##read data and store it as a data frame
data <- read.table("/Users/christrombley/Downloads/nwdata.csv", header=TRUE, sep=",")

##declare vars needed for mlVAR
vars <- c("vomit", "exercise", "bodycheck", "binge", "restrict", "afternoon")
idvar <- "id"
beepvar <- "beep"
dayvar <- "day"
data$id <- as.numeric(as.factor(data[[idvar]]))

##call mlVAR function and store it as a var
res <- mlVAR(data, vars, idvar, beepvar, dayvar, lags = 1,
             temporal = "correlated", estimator = "lmer", contemporaneous = "correlated", nCores = 1, verbose = TRUE,
             scale = TRUE, scaleWithin = FALSE, AR = FALSE,
             MplusSave = TRUE, MplusName = "mlVAR", iterations = "(2000)",
             chains = nCores)

##print mlVAR var
print
## print summary of mlVAR
summary(res)


##contemporaneous plot
plot(res, "contemporaneous", layout = "circle", nonsig = "hide", rule = "and", edge.labels = TRUE)
##temporal plot
plot(res, "temporal", layout = "circle", nonsig = "hide")
##between plot
plot(res, "between", layout = "circle")


## save and load file
save(res, file = "res.Rdata")
load("res.Rdata")
traceback() 
pdf("groupnetworkcog.pdf")
dev.off()
pdf("mlVARnetwork.pdf", height=12, width=14)

##call to temporal plot func and save it as a var
t <- plot(res, "temporal", nonsig = "hide", label.cex=1.3, alpha="0.15",
          vsize=9, legend=F, edge.labels = TRUE)

##call to contemporaneous plot func and save it as a var
c <- plot(res, "contemporaneous", nonsig = "hide", rule = "and", label.cex=1.3,
          vsize=9, alpha="0.15", legend=F, edge.labels = TRUE)

##call to between plot func and store it as a var
b <- plot(res, "between", nonsig = "hide", rule = "and", alpha="0.15", label.cex=1.3,
          vsize=9, legend=F, edge.labels = TRUE)
dev.off()

##centrality plot code for all three plots
pdf ("c1")
c1 <- centralityPlot(t)
dev.off()
pdf ("c2")
c2 <- centralityPlot(c)
dev.off()
pdf ("c3")
c3 <- centralityPlot(b)
dev.off()

##write csv files
BPedges <-getWmat(t)
write.csv(BPedges, "BPedges.csv")
BPedgescontemp <-getWmat(c)
write.csv(BPedgescontemp, "BPedgescontemp.csv")
BPedgesbetween <-getWmat(b)
write.csv(BPedgesbetween, "BPedgesbetween.csv")
centralityTable(t)
write.csv(t, "directedcentrality.csv")
centralityTable(c)
write.csv(c, "contempcentrality.csv")
centralityTable(b)
write.csv(b, "betweencentrality.csv")







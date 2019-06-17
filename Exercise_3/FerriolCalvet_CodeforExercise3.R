#' ---
#' title: "Exercise on Microarray Data Analysis"
#' author: "Ferriol Calvet"
#' date: "June 17, 2019"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' 
## ----message=FALSE, warning=FALSE----------------------------------------
## ----loadPackages, message=FALSE, warning=FALSE, eval=FALSE-----------
require("colorspace")
require("gplots")
require("ggplot2")
require("ggrepel")

require("BiocManager")
require("ReactomePA")
require("org.Mm.eg.db")
require("annotate")
require("mogene10sttranscriptcluster.db")
require("pvca")
require("genefilter")
require("limma")
require("arrayQualityMetrics")
require("oligo")

#' 
#' # Read files
#' We need two different informations as input, on the one hand the targets file, and on the other hand the expression data. We will read the targets file as a normal tsv file, and for the data we will use _read.celfiles_ function. We will also store the targets dataframe in _AnnotatedDataFrame_ format.   Finally, we make sure that the names of the rows and the columns are the appropiate ones.
## ------------------------------------------------------------------------
## ----ReadTargets---------------------------------------------------------
targets <- read.csv2("./data/targets.csv", header = TRUE, sep = "\t") 
print(targets)

## ----ReadCELfiles, message=FALSE, results='hide', warning=FALSE----------
require(oligo)
celFiles <- list.celfiles("./data", full.names = TRUE)
my.targets <-read.AnnotatedDataFrame(file.path("./data","targets.csv"), 
                                     header = TRUE, row.names = 1, 
                                     sep="\t")
rawData <- read.celfiles(celFiles, phenoData = my.targets)
# dim(rawData@assayData$exprs)

## ----ChangeName----------------------------------------------------------
colnames(rawData) <-rownames(pData(rawData)) <- my.targets@data$ShortName

#' # Raw data evaluation
#' To perform the raw data evaluation we will use two different plots. First we will plot the Principal Component Analysis of our data, that allows us to see how the different samples are clustering or how are they distributed in relation to one another, and then we will use the boxplot to see how are the expression values of each sample distributed.
## ----warning=FALSE-------------------------------------------------------
require(ggplot2)
require(ggrepel)
plotPCA3 <- function (datos, labels, factor, title, scale,colores, size = 1.5, glineas = 0.25) {
  data <- prcomp(t(datos),scale=scale)
  # plot adjustments
  dataDf <- data.frame(data$x)
  Group <- factor
  loads <- round(data$sdev^2/sum(data$sdev^2)*100,1)
  # main plot
  p1 <- ggplot(dataDf,aes(x=PC1, y=PC2)) +
    theme_classic() +
    geom_hline(yintercept = 0, color = "gray70") +
    geom_vline(xintercept = 0, color = "gray70") +
    geom_point(aes(color = Group), alpha = 0.55, size = 3) +
    coord_cartesian(xlim = c(min(data$x[,1])-5,max(data$x[,1])+5)) +
    scale_fill_discrete(name = "Group")
  # avoiding labels superposition
  p1 + geom_text_repel(aes(y = PC2 + 0.25, label = labels),segment.size = 0.25, size = size) + 
    labs(x = c(paste("PC1",loads[1],"%")),y=c(paste("PC2",loads[2],"%"))) +  
    ggtitle(paste("Principal Component Analysis for: ",title,sep=" "))+ 
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values=colores)
  }

## ----PCARaw, message=FALSE, fig.cap="Visualization of the two first Principal Components for raw data"----
plotPCA3(exprs(rawData), labels = targets$ShortName, factor = targets$Group, 
         title="Raw data", scale = FALSE, size = 3, 
         colores = c("red", "blue", "green", "yellow"))

## ----BoxplotRaw, message=FALSE, fig.cap="Boxplot for arrays intensities (Raw Data)"----
boxplot(rawData, cex.axis=0.5, las=2,  which="all", 
         col = c(rep("red", 6), rep("blue", 6), rep("green", 6), rep("yellow", 6)),
         main="Distribution of raw intensity values")

# ----QCRaw, message=FALSE, warning=FALSE, eval=FALSE---------------------
# require(arrayQualityMetrics)
# arrayQualityMetrics(rawData, outdir = file.path("./results", "QCDir.Raw"), force=TRUE)
# head(dim(exprs(rawData)))


#' 
#' # Normalization
#' As we see that there are some irregularities in the boxplot and that in the PCA some groups are not very compact, we will normalize the data using the _rma_ function of the package _oligo_. From now on we will work with this normalized data.
#' 
## ------------------------------------------------------------------------
## ----Normalization---------------------------------------------
eset_rma <- rma(rawData)

#' 
#' # Quality check of normalized data
#' After normalizing the data we perform the same quality check as before, however in this case we will use the function _arrayQualityMetrics_ that outputs the results of this quality check automatically in a folder that can be found in the results directory.
## ----warning=FALSE-------------------------------------------------------
## ----QCNorm, message=FALSE, warning=FALSE, eval=FALSE--------------------
arrayQualityMetrics(eset_rma, outdir = file.path("./results", "QCDir.Norm"), force=TRUE)

## ------------------------------------------------------------------------
## ----PCANorm, message=FALSE, fig.cap="Visualization of first two principal components for normalized data"----
plotPCA3(exprs(eset_rma), labels = targets$ShortName, factor = targets$Group, 
         title="Normalized data", scale = FALSE, size = 3, 
         colores = c("red", "blue", "green", "yellow"))

## ----BoxplotNorm, message=FALSE, fig.cap="Distribution of  intensities for normalized data"----
boxplot(eset_rma, cex.axis=0.5, las=2, 
         col = c(rep("red", 6), rep("blue", 6), rep("green", 6), rep("yellow", 6)),
         main="Boxplot for arrays intensity: Normalized Data")

pData(eset_rma) <- targets

#' 
#' 
#' # Variation Plot
#' Now once we have made sure that the data is properly normalized, we look at the variance for each of the genes in the array. This way we will see the proportion of genes that are highly variable, if any.
## ------------------------------------------------------------------------
## ----SDplot, fig.cap="Values of standard deviations allong all samples for all genes ordered from smallest to biggest"----
sds <- apply (exprs(eset_rma), 1, sd)
sdsO<- sort(sds)
plot(1:length(sdsO), sdsO, main="Distribution of variability for all genes",
     sub="Vertical lines represent 90% and 95% percentiles",
     xlab="Gene index (from least to most variable)", ylab="Standard deviation")
abline(v=length(sds)*c(0.9,0.95), col="red")

#' In this plot we can see that for almost 95 % of the genes, the standard deviation is below 0.5. This means that for these genes there is not enough variance to detect differential expression, and they should be removed from the analysis.
#' 
#' # Filter genes using variance cutoff
#' After concluding that some genes need to be discarded from the pool of candidate differentially expressed genes, we filter those genes whose variance is not in the top 25% when using the Interquartile Range as function.  
#' After the filtering is applied we obtain an expression set without all these genes that have been filtered out, and this is the data that will be used to continue the analysis.
## ------------------------------------------------------------------------
## ----Filtering1, results='hide', message=FALSE---------------------------
require(genefilter)
require(mogene10sttranscriptcluster.db)
annotation(eset_rma) <- "mogene10sttranscriptcluster.db"

filtered <- nsFilter(eset_rma, 
                     require.entrez = TRUE, remove.dupEntrez = TRUE,
                     var.filter=TRUE, var.func=IQR, var.cutoff=0.75, 
                     filterByQuantile=TRUE, feature.exclude = "^AFFX")

## ----FilterResults1, results='hide', echo=FALSE--------------------------
names(filtered)
class(filtered$eset)

## ----FilterResults2------------------------------------------------------
print(filtered$filter.log)
eset_filtered <-filtered$eset

## ----SaveData1, results='hide', message=FALSE----------------------------
write.csv(exprs(eset_rma), file="./results/normalized.Data.csv")
write.csv(exprs(eset_filtered), file="./results/normalized.Filtered.Data.csv")
save(eset_rma, eset_filtered, file="./results/normalized.Data.Rda")

## ----LoadSavedData-------------------------------------------------------
if (!exists("eset_filtered")) load (file="./results/normalized.Data.Rda")

#' 
#' # Build Design and Contrast matrices
#' After having the data prepared, we now build the design and contrast matrices. The first one helps us to discriminate the 4 different groups in the experiment, and the second one allows us to define the comparisons that are rellevant for the analysis.  
#' In our case the rellevant comparisons are KO vs control in quadriceps and in soleus, and also soleus control vs quadriceps control.
## ------------------------------------------------------------------------
## ----DesignMatrix, message=FALSE-----------------------------------------
require(limma)
designMat<- model.matrix(~0+Group, pData(eset_filtered))
colnames(designMat) <- c("quad_ctrl","quad_KO","sol_ctrl","sol_KO")
print(designMat)

## ----. setContrasts------------------------------------------------------
cont.matrix <- makeContrasts (
  quadctrlvsquadKO = quad_KO-quad_ctrl,
  solctrlvssolKO = sol_KO-sol_ctrl,
  quadctrlvssolCtrl = sol_ctrl-quad_ctrl,
  levels=designMat)
print(cont.matrix)


#' 
#' # Fit a linear model and select significant hits
#' With the design and the contrast matrices defined we can go on with the analysis, and fit the linear model. By fitting this linear model, we estimate the parameters that define the relationship between the values of expression and the different groups defined in the design matrix.  
#' Once we have completed this step, we build the comparisons and we store in three different tables the results fro the three different comparison we defined in the contrast matrix.
## ------------------------------------------------------------------------
## ---- linearmodelfit-----------------------------------------------------
require(limma)
fit<-lmFit(eset_filtered, designMat)
fit.main<-contrasts.fit(fit, cont.matrix)
fit.main<-eBayes(fit.main)
class(fit.main)

## ---- topTabs1-----------------------------------------------------------
topTab_quadctrlvsquadKO <- topTable (fit.main, number=nrow(fit.main), coef="quadctrlvsquadKO", adjust="fdr")
head(topTab_quadctrlvsquadKO)

## ---- topTabs2-----------------------------------------------------------
topTab_solctrlvssolKO <- topTable (fit.main, number=nrow(fit.main), coef="solctrlvssolKO", adjust="fdr")
head(topTab_solctrlvssolKO)

## ---- topTabs3-----------------------------------------------------------
topTab_quadctrlvssolCtrl  <- topTable (fit.main, number=nrow(fit.main) , coef="quadctrlvssolCtrl", adjust="fdr")
head(topTab_quadctrlvssolCtrl)


#' 
#' 
#' # Annotate the top hits of each comparison
#' This three tables we have just defined with the results of the linear model, can be annotated using the proper reference for our array, remember that, in this case, the annotation package that allows us to annotate our data is _"mogene10sttranscriptcluster.db"_.
## ----warning=FALSE-------------------------------------------------------
## ----GeneAnnotation, message=FALSE, warning=FALSE------------------------
annotatedTopTable <- function(topTab, anotPackage)
{
  topTab <- cbind(PROBEID=rownames(topTab), topTab)
  myProbes <- rownames(topTab)
  thePackage <- eval(parse(text = anotPackage))
  geneAnots <- select(thePackage, myProbes, c("SYMBOL", "ENTREZID", "GENENAME"))
  annotatedTopTab<- merge(x=geneAnots, y=topTab, by.x="PROBEID", by.y="PROBEID")
return(annotatedTopTab)
}

## ----annotateTopTables---------------------------------------------------
topAnnotated_quadctrlvsquadKO <- annotatedTopTable(topTab_quadctrlvsquadKO,
                                                   anotPackage="mogene10sttranscriptcluster.db")
topAnnotated_solctrlvssolKO <- annotatedTopTable(topTab_solctrlvssolKO,
                                                 anotPackage="mogene10sttranscriptcluster.db")
topAnnotated_quadctrlvssolCtrl <- annotatedTopTable(topTab_quadctrlvssolCtrl,
                                                    anotPackage="mogene10sttranscriptcluster.db")

write.csv(topAnnotated_quadctrlvsquadKO, file="./results/topAnnotated_quadctrlvsquadKO.csv")
write.csv(topAnnotated_solctrlvssolKO, file="./results/topAnnotated_solctrlvssolKO.csv")
write.csv(topAnnotated_quadctrlvssolCtrl, file="./results/topAnnotated_quadctrlvssolCtrl.csv")

#' 
#' # Volcano plot of all the significant genes displaying the names for top 10 genes
#' After annotating the genes that are cantidates to be differentially expressed in each of the comparisons, we can use volcano plots to show how significant and how differentially expressed they are.
## ------------------------------------------------------------------------
## ----volcanoPlot, fig.cap="Volcano plot for the comparison between KO and WT in COLD temperature. The names of the top 10 genes (i.e. the first ten genes in the topTable) are shown in the plot"----
require(mogene10sttranscriptcluster.db)
geneSymbols <- select(mogene10sttranscriptcluster.db, rownames(fit.main), c("SYMBOL"))
SYMBOLS<- geneSymbols$SYMBOL
for (i in colnames(cont.matrix)){
  volcanoplot(fit.main, coef=i, highlight=10, names=SYMBOLS,
              main=paste("Differentially expressed genes",i, sep="\n"))
  abline(v=c(-1,1), col="red")
}

# Save plots in a PDF document
pdf("results/Volcanos.pdf")
for (i in colnames(cont.matrix)){
  volcanoplot(fit.main, coef=i, highlight=10, names=SYMBOLS,
              main=paste("Differentially expressed genes",i, sep="\n"))
  abline(v=c(-1,1), col="red")
}
dev.off()

#' 
#' # Significant genes comparison
#' We can also check the intersections of the different sets of differentially expressed genes for each comparison using a Venn diagram.
## ------------------------------------------------------------------------
## ----decideTests.1-------------------------------------------------------
require(limma)
res<-decideTests(fit.main, method="separate", adjust.method="fdr", p.value=0.05, lfc=1)

## ----resumeDecideTests---------------------------------------------------
sum.res.rows<-apply(abs(res),1,sum)
res.selected<-res[sum.res.rows!=0,] 
print(summary(res))

## ---- vennDiagram, fig.cap="Venn diagram showing the genes in common between the three comparisons performed"----
vennDiagram (res.selected[,1:3], cex=0.9)
title("Genes in common between the three comparisons\n Genes selected with FDR < 0.05 and logFC > 1")


#' 
#' # Heatmaps
## ------------------------------------------------------------------------
## ----data4Heatmap--------------------------------------------------------
probesInHeatmap <- rownames(res.selected)
HMdata <- exprs(eset_filtered)[rownames(exprs(eset_filtered)) %in% probesInHeatmap,]

geneSymbols <- select(mogene10sttranscriptcluster.db, rownames(HMdata), c("SYMBOL"))
SYMBOLS<- geneSymbols$SYMBOL
rownames(HMdata) <- SYMBOLS
write.csv(HMdata, file = file.path("./results/data4Heatmap.csv"))

## ----heatmapNoclustering, fig.cap="Heatmap for expression data without any grouping"----
my_palette <- colorRampPalette(c("blue", "red"))(n = 299)
require(gplots)

heatmap.2(HMdata,
          Rowv = FALSE,
          Colv = FALSE,
          main = "Differentially expressed genes \n FDR < 0,05, logFC >=1",
          scale = "row",
          col = my_palette,
          sepcolor = "white",
          sepwidth = c(0.05,0.05),
          cexRow = 0.5,
          cexCol = 0.9,
          key = TRUE,
          keysize = 1.5,
          density.info = "histogram",
          ColSideColors = c(rep("red",6),rep("blue",6), rep("green",6), rep("yellow",6)),
          tracecol = NULL,
          dendrogram = "none",
          srtCol = 30)

## ----heatmapClustering, fig.cap="Heatmap for expression data grouping genes (rows) and samples (columns) by their similarity"----
heatmap.2(HMdata,
          Rowv = TRUE,
          Colv = TRUE,
          dendrogram = "both",
          main = "Differentially expressed genes \n FDR < 0,05, logFC >=1",
          scale = "row",
          col = my_palette,
          sepcolor = "white",
          sepwidth = c(0.05,0.05),
          cexRow = 0.5,
          cexCol = 0.9,
          key = TRUE,
          keysize = 1.5,
          density.info = "histogram",
          ColSideColors = c(rep("red",6),rep("blue",6), rep("green",6), rep("yellow",6)),
          tracecol = NULL,
          srtCol = 30)


#' 
#' # Functional interpretation
#' Finally we select only the significant genes using as threshold 0.05 for the adjusted p-value. Once we have these genes selected, we will look for enrichment using the function _enrichPathway_ in the _ReactomePA_ package. After this step we have a list of the enriched pathways with the amount of genes differentially expressed belonging to that pathway, the p-value of each pathway and many other informations.  
#' To summarize these pathways we can use a network showing as nodes the genes, and also the association to a particular pathway, we have done this for 4 different enriched pathways usign the function _cnetplot_.
## ------------------------------------------------------------------------
## ----selectGenes---------------------------------------------------------
listOfTables <- list(quadctrlvsquadKO = topTab_quadctrlvsquadKO, 
                     solctrlvssolKO  = topTab_solctrlvssolKO, 
                     quadctrlvssolCtrl = topTab_quadctrlvssolCtrl)

listOfSelected <- list()
for (i in 1:length(listOfTables)){
  # select the toptable
  topTab <- listOfTables[[i]]
  # select the genes to be included in the analysis
  whichGenes<-topTab["adj.P.Val"] < 0.05
  selectedIDs <- rownames(topTab)[whichGenes]
  # convert the ID to Entrez
  EntrezIDs<- select(mogene10sttranscriptcluster.db, selectedIDs, c("ENTREZID"))
  EntrezIDs <- EntrezIDs$ENTREZID
  listOfSelected[[i]] <- EntrezIDs
  names(listOfSelected)[i] <- names(listOfTables)[i]
}
sapply(listOfSelected, length)

## ----BiologicalSig-------------------------------------------------------
require(ReactomePA)

listOfData <- listOfSelected[1:3]
comparisonsNames <- names(listOfData)

organism <- "mouse"

for (i in 1:length(listOfData)){
  data <- listOfData[[i]]
  genesIn <- listOfSelected[[i]]
  comparison <- comparisonsNames[i]
  enrich.result <- enrichPathway(gene = genesIn,
                                 pvalueCutoff = 0.05,
                                 readable = T,
                                 organism =  organism,
                                 minGSSize = 5,
                                 maxGSSize = 500,
                                 pAdjustMethod = "BH")

    
  if (length(rownames(enrich.result@result)) != 0) {
    print(paste("For", comparison, "comparison, the enriched pathways are:"))
    print(enrich.result@result$Description[1:10])
    print("")
    
    print(cnetplot(enrich.result, categorySize = "geneNum", schowCategory = 15, 
         vertex.label.cex = 0.75))
    
    pdf(file = paste0("./results/","ReactomePAcnetplot.",comparison,".pdf"))
    print(cnetplot(enrich.result, categorySize = "geneNum", schowCategory = 15,
         vertex.label.cex = 0.75))
    dev.off()
  }
}

#' 

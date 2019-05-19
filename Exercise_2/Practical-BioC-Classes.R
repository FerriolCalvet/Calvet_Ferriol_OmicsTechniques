## ----include=FALSE-------------------------------------------------------
require(knitr)
opts_chunk$set(
concordance=FALSE, echo=TRUE, cache=TRUE, warning=FALSE, error=FALSE, message=FALSE)


## ----loadBiobase---------------------------------------------------------
library(Biobase)


## ----AnnotatedDataFramephenoData.1---------------------------------------
samplenames <- letters[1:10]
dataf <- data.frame(treated=sample(c(TRUE, FALSE), 10, replace=TRUE),
                    sex=sample(c("Male", "Female"), 10, replace=TRUE),
                    mood=sample(c("Happy", "Dont't care", "Grumpy"), 10, replace=TRUE),
                    names=samplenames, row.names="names")
dataDesc = data.frame(c("Treated with dark chocolate", "Sex", "Mood while eating"))

pdata <- new("AnnotatedDataFrame", data=dataf,dataDesc)


## ----label=MIAME---------------------------------------------------------
my.desc <- new("MIAME", name="LPS_Experiment",
            lab="National Cancer Institute",
            contact="Lakshman Chelvaraja",
            title="Molecular basis of age associated cytokine dysregulation in LPS stimulated macrophages ",
            url="http://www.jleukbio.org/cgi/content/abstract/79/6/1314")
print(my.desc)


## ------------------------------------------------------------------------
data(sample.ExpressionSet)
sample.ExpressionSet

x <- exprs(sample.ExpressionSet)
myExpressionSet <- new("ExpressionSet", x, pdata, my.desc)
ExpressionSet(assayData = x)
ExpressionSet(assayData = x, phenoData = pdata)

## ------------------------------------------------------------------------
library(GEOquery)
# gds <- getGEO("GDS507")
gsm <- getGEO(filename=system.file("extdata/GSM11805.txt.gz",package="GEOquery"))


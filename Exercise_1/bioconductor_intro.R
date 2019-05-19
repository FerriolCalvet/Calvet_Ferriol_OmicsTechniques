if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install()

library(BiocManager)
library(Biobase)
library(methods)
data(package="Biobase")
data("sample.ExpressionSet")
slotNames(sample.ExpressionSet)
methods(class=class(sample.ExpressionSet))

sample.ExpressionSet

dim(exprs(sample.ExpressionSet))
# Dimensions of the array

dim(pData(sample.ExpressionSet))
# Row of the phenodata corresponds to the number of columns of the expression data.

head(pData(sample.ExpressionSet))

packages = c(
    "dplyr",
    "ranger", 
    "PEIP",
    "data.table",
    "countrycode",
    "stringr",
    "ggrepel",
    "tidyverse",
    "ggpubr",
    "passport",
    "ggplot2",
    "wesanderson",
    "lmtest",
    "sandwich",
    "xtable",
    "quantmod"
    )

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

```{r setup}
library(Rxtsum)
```

# A few words about Rxtsum

The package Rxtsum replicates the between/within standard deviation decomposition of Stata's xtsum package. In particular, it follows the procedure described here : http://stephenporter.org/files/xtsum_handout.pdf

It is used as follows:
```
Rxtsum(data, id, col)
```
Where data is a dataframe which can be grouped (for example panel data where the group is the individual), id is the grouping variable, and col is a character vector for which you want the within/between decomposition. Obviously, col have to be numeric.

```{r, warning = FALSE}
library(dplyr)
```

An overview of the different functions of the packge:
```{r, include = TRUE}
Rxtsum::Rxtsum(iris, Species, c("Sepal.Length", "Sepal.Width"))
Rxtsum::f.between_df(iris, Species, c("Sepal.Length", "Sepal.Width"))
Rxtsum::f.within_df(iris, Species, c("Sepal.Length", "Sepal.Width"))
Rxtsum::f.overall_df(iris, Species, c("Sepal.Length", "Sepal.Width"))
```




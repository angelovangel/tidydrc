# tidydrc
tidydrc

## Description
Tidy modelling of dose-response relationships with the `drc` package in `R`.    
This is a wrapper for [`drc`](https://cran.r-project.org/web/packages/drc/index.html) by Christian Ritz, which is probably the best package for modelling dose-response.  
`tidydrc` contains two functions which make it easier to generate and plot these models. Install it with:

`devtools::install_github("angelovangel/tidydrc")`

## Examples
The `tidydrc()` function returns a dataframe with list-columns (the data,
predictions and coefficients). It is thus easy to implement in tidy workflows.
For example, to fit Michaelis-Menten kinetics models for treated and untreated samples in
the `Puromycin` dataset (built-in) and get the Km values with std. error:
```r
mm <- tidydrc(Puromycin, conc, rate, model = MM.3(), state)
names(mm$drmod) <- as.character(mm$state)
map(mm$drmod, ED, 50) %>% map_df(as_tibble, .id = "sample")
```

The list-column dataframe can be directly piped to `tidydrc_plot()`
```r
tidydrc(S.alba, Dose, DryMatter, model = LL.4(), Herbicide) %>%
tidydrc_plot(ed50 = TRUE, color = ~Herbicide) + 
scale_x_log10()
```

A more involved example...

## References
Ritz, C., Baty, F., Streibig, J. C., Gerhard, D. (2015) Dose-Response Analysis Using R PLOS ONE, 10(12), e0146021

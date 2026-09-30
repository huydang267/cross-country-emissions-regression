# Cross-Country Methane Emissions Regression

Log-log OLS regression of total methane emissions on income, population and agricultural activity across 166 countries in 2022, estimated in R.

## Business problem

Methane has a much higher short-term warming potential than carbon dioxide, and its main sources (livestock, crop production, waste and energy) are closely tied to economic development. Policy makers and climate-focused investors need to know which country characteristics are associated with higher methane emissions, and by how much. This project estimates how emissions scale with GDP per capita, population, livestock production, agriculture's share of GDP and fertiliser use, tests whether emissions follow an environmental Kuznets curve (rising and then falling with income), and checks which functional form describes the data best.

## Data

| Item | Detail |
|---|---|
| Source | World Bank, [World Development Indicators](https://databank.worldbank.org/source/world-development-indicators) |
| Size | 166 countries, 2022, 11 columns, no missing values (18 KB) |
| License | [CC BY 4.0](https://datacatalog.worldbank.org/public-licenses) |
| File | [data/methane_emissions_2022.csv](data/methane_emissions_2022.csv), with indicator codes in [data/README.md](data/README.md) |

## Method

1. **Preparation.** Loaded the WDI extract, fixed misspelled column names, and took natural logs of methane emissions, GDP per capita, population and fertiliser use.
2. **Equation 1 (log-log OLS).** Regressed log(methane) on log(GDP per capita), log(population), the livestock production index, agricultural value added (% of GDP) and log(fertiliser use).
3. **Inference.** Interpreted elasticities and semi-elasticities with t-tests and 95% confidence intervals, computed a manual t-test for the population elasticity, and computed the exact percentage effect of the agricultural share.
4. **Equation 2 (Kuznets test).** Added [log(GDP per capita)]² and computed the turning point. Tested the curvature with an individual t-test and a joint F-test on both income terms.
5. **Multicollinearity.** Examined the correlation matrix of the regressors and explained how correlation between regressors inflates the variance of an estimate through the variance inflation factor (VIF).
6. **Gauss-Markov assumptions.** Assessed MLR.1 to MLR.5 and tested for heteroskedasticity with the Breusch-Pagan and White tests.
7. **Equation 3 (levels).** Re-estimated the model with emissions, GDP per capita and population in levels, and compared the three specifications.

## Results

**Equation 1 (preferred model)**, n = 166, R² = 0.9182, adjusted R² = 0.9157, F(5,160) = 359.34:

| Variable | Estimate | Std. error | t | p-value | 95% CI |
|---|---|---|---|---|---|
| Intercept | -18.8096 | 1.0018 | -18.777 | < 0.001 | [-20.7880, -16.8312] |
| log(GDP per capita) | 0.3618 | 0.0833 | 4.344 | 2.48e-5 | [0.1973, 0.5264] |
| log(Population) | 1.0851 | 0.0268 | 40.564 | < 0.001 | [1.0323, 1.1380] |
| Livestock index | 0.0072 | 0.0023 | 3.093 | 0.0023 | [0.0026, 0.0117] |
| Agri. value added (% GDP) | -0.0058 | 0.0085 | -0.676 | 0.5000 | [-0.0226, 0.0111] |
| log(Fertiliser use) | -0.1195 | 0.0453 | -2.639 | 0.0091 | [-0.2090, -0.0301] |

VIF (computed for this portfolio version; not in the original report):

| Variable | VIF |
|---|---|
| log(GDP per capita) | 3.468 |
| log(Population) | 1.146 |
| Livestock index | 1.097 |
| Agri. value added (% GDP) | 2.883 |
| log(Fertiliser use) | 2.105 |

Key findings:

- **Income.** A 1% increase in GDP per capita is associated with a 0.3618% increase in methane emissions, ceteris paribus. The effect is inelastic, and its 95% CI [0.197, 0.526] lies strictly above zero.
- **Population.** The population elasticity is 1.085, and its 95% CI [1.032, 1.138] lies above 1, so emissions scale slightly more than proportionally with population.
- **No Kuznets curve in the data.** In Equation 2 the squared income term is insignificant (t = -0.39, p = 0.696), and adjusted R² falls from 0.9157 to 0.9152. The implied turning point is about 13,782 times the highest GDP per capita in the sample ($134,508). The joint F-test (F(2,159) = 9.46, p < 0.001) is driven by the linear income term.
- **Multicollinearity.** log(GDP per capita) and agricultural value added are strongly negatively correlated (r = -0.803), which reflects structural transformation. The VIF for the agricultural share is 2.88, which inflates its standard error by about 1.7 times. This is moderate and well below the usual thresholds of 5 or 10, so multicollinearity only partly explains its insignificance; the point estimate itself is small (-0.0058). The correlation is imperfect, so MLR.3 holds and the coefficients are identified. Unbiasedness also requires MLR.4, which omitted variables may violate (see Diagnostics).
- **Diagnostics.** The Breusch-Pagan test (p = 0.134) and the White test (p = 0.445) both fail to reject homoskedasticity at the 5% level. Omitted variables such as energy mix and regulation are the main threat to a causal reading (MLR.4), so the estimates are associations.
- **Specification choice.** Equation 3 in levels fits worse (R² = 0.8164, adjusted R² = 0.8107), and only population is significant in it. Equation 1 is preferred because it gives constant elasticities across variables that span several orders of magnitude.

## Repo structure

```
cross-country-emissions-regression/
├── methane_regression.R            # Estimates Equations 1 to 3 and draws the 13 report figures
├── data/
│   ├── methane_emissions_2022.csv  # World Bank WDI extract, 166 countries, 2022
│   └── README.md                   # Source, license and indicator codes
└── README.md
```

## How to run

Requires R 4.x and the `car` and `lmtest` packages. Tested with R 4.5.1.

```bash
git clone https://github.com/huydang267/cross-country-emissions-regression.git
cd cross-country-emissions-regression
Rscript -e 'install.packages(c("car", "lmtest"))'
Rscript methane_regression.R
```

The script draws the 13 result tables and plots from the report (regression tables, confidence intervals, correlation heatmap, turning point, significance tests, scatter plot and heteroskedasticity tests). With `Rscript` they are written to `Rplots.pdf`. In RStudio they appear in the Plots pane.

## Context

Course project, RMIT University (Melbourne campus, cross-campus semester), ECON1066 Basic Econometrics, Semester 1 2026.

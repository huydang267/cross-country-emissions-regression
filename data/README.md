# Data

`methane_emissions_2022.csv` is a cross-section of 166 countries for the year 2022, extracted from the World Bank **World Development Indicators (WDI)**. It has no missing values and is about 18 KB.

- Source: World Bank, World Development Indicators, https://databank.worldbank.org/source/world-development-indicators
- License: Creative Commons Attribution 4.0 (CC BY 4.0), https://datacatalog.worldbank.org/public-licenses

Citation: World Bank, *World Development Indicators* (data for 2022), DataBank, The World Bank, Washington, DC. The extract was supplied as course data for ECON1066 in 2026.

| Column | WDI indicator | Code |
|---|---|---|
| `TotalMethane Emission` | Methane (CH4) emissions (total) excluding LULUCF (Mt CO2e) | EN.GHG.CH4.MT.CE.AR5 |
| `GDPpc` | GDP per capita, PPP (constant 2021 international $) | NY.GDP.PCAP.PP.KD |
| `TotalPop` | Population, total | SP.POP.TOTL |
| `LiveStockIndex` | Livestock production index (2014-2016 = 100) | AG.PRD.LVSK.XD |
| `AgriValueAddedin` | Agriculture, forestry, and fishing, value added (% of GDP) | NV.AGR.TOTL.ZS |
| `FertilzerUse` | Fertilizer consumption (kilograms per hectare of arable land) | AG.CON.FERT.ZS |
| `RuralPop%` | Rural population (% of total population), not used in the models | SP.RUR.TOTL.ZS |

`Time`, `Time Code`, `Country Name` and `Country Code` identify the observation. Column names are kept as supplied; `methane_regression.R` renames the misspelled ones when it loads the file.

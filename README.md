# Transboundary damages
Supporting code and data for analysis on health damages incurred from transferred pollution in the U.S.

# Emissions data
Contains code for procecssing raw data from the National Emissions Inventory (NEI) for use in the AP3 model.

Because of the large size of the NEI data, the data is not posted here but is available at https://www.epa.gov/air-emissions-inventories/national-emissions-inventory-nei or upon request. Data files should include Onroad, Nonroad, Nonpoint, Facility level, and process emissions worksheets. 

The 2014 eGrid data is also used to supplement the NEI and is available from https://www.epa.gov/energy/emissions-generation-resource-integrated-database-egrid-questions-and-answers or on request. Note that downloaded NEI files should be placed in their corresponding folders and renamed to match naming convention in Emissions breakdown V3.R.

Users should ensure that working directory references and filenames are updated at the beginning of Emissions breakdown V3.R.

# Analysis data and code
Code and data for post-processing results from the AP3 model runs. Scripts include:

1. Damage maps V2.R - plots main damage results (including bar plot summaries and maps)
2. County shares analysis code V2.R - analysis on export/import ratios
3. Lorenz curve bootstrap.R - helper function code to run bootstrap for Gini coefficient calbulations
4. Supplementary data for regressions.R - Adds covariate data and runs regressions
5. regression.Rnw - summarizes regression results 

The beginning of these scripts include path references to where the AP3 model output is stored. Because of the size of the data, this data is not available on Github but can be supplied by request to briansergi@gmail.com. In addition, R workspaces containing the results from the analysis are provided. 



# qPCRWranggled
 A tool for automatical annotation of Takara TP800 qPCR data.  

 ## Usage
 1. Do the experiment and save the results in xlsx file.
 2. Generate experiment setting file in xlsx format. (See [example](./data/example_experiment/setting.xlsx))
 3. Run the following code in your R studio:
 ```
 ## Example
source(here::here("./R/GetWranggledQpcrData.R"))

 df <- GetWranggledQpcrData(
input = here::here("./data/example_experiment/20210817.xlsx"),
experiment = here::here("./data/example_experiment/setting.xlsx"),
output = here::here("./data/example_experiment/20210817_wranggled.xlsx"),
Ct_max = 40,
Tm_min = 60
)
 ```

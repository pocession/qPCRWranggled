#' GetWranggledQpcrData
#'
#' @description
#' This function reads raw data of Takara TP800 qPCR instrument and output a nicely-formatted excel file.
#' 
#' @author Tsunghan Hsieh
#'
#' @param input A character string specifying the name and path of raw data .xlsx file
#' 
#' @param experiment  A character string specifying the name and path of experiment setting .xlsx file.
#' This experiment setting must contain two sheets: Target.ID and Sample.ID
#' The Target.ID sheet must contain Target.ID and Target columns
#' The Sample.ID sheet must contain Sample.ID and Sample columns
#' @param output A character string specifying the name and path of output .xlsx file.
#' @param Ct_max A number specifying the maximum Ct value for samples not detected, default = 40
#' @param Tm_min A number specifying the minumum temperature value for non-specific samples, default = 60 
#' @return a dataframe of the output data
#'
#' @export
#'
#' @importFrom here here
#' @importFrom openxlsx read.xlsx write.xlsx
#' @importFrom assertthat assert_that
#' @importFrom dplyr select mutate group_by summarise left_join anti_join ungroup
#'
#' @examples
#' \dontrun{
#' df <- GetWranggledQpcrData(
#' input = here::here("./data/example_experiment/20210817.xlsx"),
#' experiment = here::here("./data/example_experiment/setting.xlsx"),
#' output = here::here("./data/example_experiment/20210817_wranggled.xlsx"),
#' Ct_max = 40,
#' Tm_min = 60
#' )
#' }
#' 

GetWranggledQpcrData <- function(input, experiment, output, Ct_max = 40, Tm_min = 60) {
  # check input ----------------------------------------------------------------
  assertthat::assert_that(is.character(input), 
                          msg = "The given argument is not a string.\n")
  
  assertthat::assert_that(grepl("\\.xlsx$", input), 
                          msg = "The given file to the input file is not a .xlsx file.\n")
  
  assertthat::assert_that(file.exists(here::here(input)), 
                          msg = "The given file path to the input file does not exist.\n")
  
  assertthat::assert_that(is.character(experiment), 
                          msg = "The given argument is not a string.\n")
  
  assertthat::assert_that(grepl("\\.xlsx$", experiment), 
                          msg = "The given file to the experiment file is not a .xlsx file.\n")
  
  assertthat::assert_that(file.exists(here::here(experiment)), 
                          msg = "The given file path to the expertiment file does not exist.\n")
  
  assertthat::assert_that(is.character(output), 
                          msg = "The given argument is not a string.\n")
  
  assertthat::assert_that(grepl("\\.xlsx$", output), 
                          msg = "The given file to the output file is not a .xlsx file.\n")
  
  assertthat::assert_that(is.numeric(Ct_max), 
                          msg = "The given argument is not a number.\n")
  
  assertthat::assert_that(is.numeric(Tm_min), 
                          msg = "The given argument is not a number.\n")
  
  # read input -----------------------------------------------------------------
  data <- openxlsx::read.xlsx(xlsxFile = input)
  
  ## Modify column names
  col_names <- colnames(data)
  col_names <- gsub("\\(", "_", col_names)
  col_names <- gsub("\\)", "", col_names)
  col_names <- gsub("\\#", "", col_names)
  
  colnames(data) <- col_names
  remove(col_names)
  
  # read experiment ------------------------------------------------------------
  target <- openxlsx::read.xlsx(xlsxFile = experiment, sheet = 1, skipEmptyRows = TRUE)
  sample <- openxlsx::read.xlsx(xlsxFile = experiment, sheet = 2, skipEmptyRows = TRUE)
  
  # Modify data ----------------------------------------------------------------
  df <- data |>
    dplyr::left_join(target, by = "Target.ID") |>
    dplyr::left_join(sample, by = "Sample.ID") |>
    dplyr::select(-c(Target.ID, Sample.ID)) |>
    dplyr::mutate(Target.ID = Target,
                  Sample.ID = Sample) |>
    dplyr::select(-c(Target, Sample)) |>
    dplyr::mutate(Ct = dplyr::case_when(Ct_CP %in% "--" ~ Ct_max,
                                        TRUE ~ as.numeric(Ct_CP))) |>
    dplyr::mutate(Tm = dplyr::case_when(Tm.1 %in% "--" ~ Tm_min,
                                        TRUE ~ as.numeric(Tm.1)))
  
  # Save the output ------------------------------------------------------------
  openxlsx::write.xlsx(df, output)
}
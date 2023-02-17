#' Title Function to read in csv files
#'
#' @param csv input table
#' @param dird input directory path
#'
#' @return a table able to be read with vector data
#'
#' @export

rdfl = function(csv, dird)
{
  fle = paste(dird, csv, sep = "")
  tab = read.table(fle, header = TRUE, sep = ",")
  return(tab)
}



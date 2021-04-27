.onAttach <- function(...) {
  packageStartupMessage(' ')
  packageStartupMessage('cvAUC version: ', utils::packageDescription('cvAUC')$Version)
  packageStartupMessage(' ')
}

`:=` = function(...) NULL

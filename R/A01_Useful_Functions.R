# package_check -----------------------------------------------------------


package_check <- function(packs) {
  installed_p <- packs %in% rownames(installed.packages())

  packs_to_install <- packs[!installed_p]

  if (any(installed_p)) {
    install.packages(packs_to_install)
  }

  lapply(packs, \(x) library(x, character.only = TRUE))

}

saveRDS(package_check, "A01_Useful_Functions/package_check.rds")



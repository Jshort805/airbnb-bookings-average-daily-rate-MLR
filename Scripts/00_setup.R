# 00_setup.R -- install the packages this project uses.
# Run once per environment (the devcontainer does it automatically).
# tidyverse already ships with the rocker/tidyverse image.

pkgs <- c("regclass", "caret", "here", "janitor", "languageserver", "renv")
missing_pkgs <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

# After the first successful run, lock package versions once with:
#   renv::init()       # creates renv.lock and a project library
#   renv::snapshot()   # re-run whenever a package is added

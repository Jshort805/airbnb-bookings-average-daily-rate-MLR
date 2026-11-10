#Install the packages for this project
#Run once per environment (the devcontainer does it automatically).

pkgs<- c("regclass", "caret", "here", "janitor", "languageserver", "renv")
missing_pkgs<- setdiff(pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs)
}

#After the first successful run, lock package versions once in terminal with:
#renv::init()      
#renv::snapshot()  
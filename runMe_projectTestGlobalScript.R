getOrUpdatePkg <- function(p, minVer, repo) {
  if (!isFALSE(try(packageVersion(p) < minVer, silent = TRUE) )) {
    if (missing(repo)) repo = c("predictiveecology.r-universe.dev", getOption("repos"))
    install.packages(p, repos = repo)
  }
}

getOrUpdatePkg("Require", "0.3.1.9015")
getOrUpdatePkg("SpaDES.project", "0.0.8.9023")

################### RUNAME

if (SpaDES.project::user("tmichele")) setwd("~/projects/Edehzhie/")

################ SPADES CALL
# pkgload::load_all("~/projects/reproducible")
out <- SpaDES.project::setupProject(
  runName = "Edehzhie",
  paths = list(projectPath = runName,
               scratchPath = "~/scratch"),
  modules =
    file.path("PredictiveEcology",
              c("canClimateData@canadianProvs",
                paste0(# terra-migration
                c("Biomass_speciesData",
                  "Biomass_borealDataPrep",
                  "Biomass_core"),
                "@terra-migration"),
                paste0(# development
                  c("Biomass_speciesFactorial",
                    # "Biomass_speciesParameters", # Stalls, and we don't need for now
                    "fireSense_dataPrepFit",
                    "fireSense_IgnitionFit",
                    "fireSense_EscapeFit",
                    "fireSense_SpreadFit",
                    "fireSense_dataPrepPredict",
                    "fireSense_IgnitionPredict",
                    "fireSense_EscapePredict",
                    "fireSense_SpreadPredict"),
                  "@development")
              )),
  functions = "tati-micheletti/Edehzhie@master/inputs/outterFuns.R",
  options = list(spades.allowInitDuringSimInit = TRUE,
                 gargle_oauth_email = if (user("tmichele")) "tati.micheletti@gmail.com" else NULL,
                 SpaDES.project.fast = TRUE,
                 reproducible.inputPaths = if (user("tmichele")) "~/data" else NULL),
  times = list(start = 2011,
               end = 2025),
  studyArea = studyAreaGenerator(destPath = paths[["inputPath"]]),
  rasterToMatch = rtmGenerator(sA = studyArea, destPath = paths[["inputPath"]]),
  studyAreaLarge = studyAreaGenerator(large = TRUE, destPath = paths[["inputPath"]]),
  rasterToMatchLarge = rtmGenerator(sA = studyAreaLarge, destPath = paths[["inputPath"]]),
  sppEquiv = sppEquiv_CA(runName),
  params = list(.globals = list(sppEquivCol = runName,
                                dataYear = "2011"),
                fireSense_SpreadFit = list(lower = c(0.22, 0.001, rep(-16, times = 6)),
                                           upper = c(c(0.29, 10, rep(32, times = 6)))),
                fireSense_IgnitionFit = list(lb = list(coef = 0,
                #I got this from running only dataPrepFit, then quantile(MDC, probs = 0.05) NOTE: Ian's notes
                                                       knots = list(MDC = 100)),
                                             ub = list(coef = 20,
                #and the upper quantile was 0.9 NOTE: Ian's notes
                                                       knots = list(MDC = 156))),
                canClimateData = list(.runName = runName,
                                      .useCache = ".inputObjects",
                                      climateGCM = "CanESM5",
                                      climateSSP = "370",
                                      historicalFireYears = 1991:2020)
                ),
  packages = c("googledrive", 'RCurl', 'XML',
               "PredictiveEcology/LandR@development (>= 1.1.0.9074)",
               "PredictiveEcology/SpaDES.core@optionsAsArgs (>= 2.0.2.9010)",
               "tati-micheletti/reproducible@reproducibleTempCacheDir (>= 2.0.8.9012)",
               "PredictiveEcology/climateData@stopMessageHelp (>= 1.0.4)",
               "tati-micheletti/fireSenseUtils@development"),
  useGit = "sub"
)

snippsim <- do.call(SpaDES.core::simInitAndSpades, out)
  
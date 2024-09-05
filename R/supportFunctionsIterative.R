#' Creates RDS files for iterative REMIND-EDGE runs from csv input files.
#' Existing files are overwritten silently. Does not return anything.
#'
#' @param inputPath the path to the folder containing the input (csv-) files
#' @param filename name of the file
#' @param SSPscenario SSP scenario
#' @param demScenario demand scenario
#' @param transportPolScenario Transport policy scenario
#' @import data.table
#' @importFrom magrittr %>%
#' @importFrom rmndt magpie2dt
#' @export

# Loads the csv input files chooses the correct scenario and
# converts the files into RDS local files
csv2RDS <- function(filename, inputPath, SSPscenario, demScenario,
                    transportPolScenario) {
  colNames <- c(
  # first five and the last column name are equal, the others depend on the
  # file to be read
    "period", "region", "SSPscen", "demScen", "transportPolScen",
    switch(
      filename,
      "scenSpecPrefTrends" = c("sector", "subsectorL1", "subsectorL2",
                               "subsectorL3", "vehicleType", "technology",
                               "level", "variable", "unit"),

      "initialIncoCosts"   = c("univocalName", "technology", "variable", "unit",
                               "type"),

      "timeValueCosts"     = c("univocalName", "variable", "unit"),
      "f29_trpdemand"      = "all_in",
      # default
      c("univocalName", "technology", "variable", "unit")
    ),
    "value")

  # read either .cs4r or .rds files; prefer .rds if both exist
  if (0 == file.access(file.path(inputPath, paste0(filename, '.rds')), 4)) {
    tmp <- readRDS(file.path(inputPath, paste0(filename, ".rds"))) %>%
      # filter combinations in magclass object for faster conversion of less
      # data
      `[`(,,paste(SSPscenario, demScenario, transportPolScenario,
                  sep = '.')) %>%
      magpie2dt(regioncol = colNames[2],
                yearcol = colNames[1],
                datacols = head(colNames[c(-1, -2)], -1),
                valcol = tail(colNames, 1))
  }
  else {
    tmp <- fread(input = file.path(inputPath, paste0(filename, ".cs4r")),
                 skip = 5, col.names = eval(colNames)) %>%
      `[`(  SSPscen == SSPscenario
          & demScen == demScenario
          & transportPolScen == transportPolScenario)
  }

  return(setNames(list(tmp %>%
                         `[`(,colNames[3:5] := NULL) %>%
                         `[`(!is.na(value))),
                  filename))
}

toolLoadRDSinputs <- function(edgeTransportFolder, inputFiles) {

  loadRDS <- function(filename, edgeTransportFolder) {
    filePath <- list.files(file.path(".", edgeTransportFolder), paste0(filename, ".RDS"), recursive = TRUE, full.names = TRUE)
    tmp <- readRDS(filePath)
  }

  inputData <- sapply(inputFiles, loadRDS, edgeTransportFolder, simplify = FALSE, USE.NAMES = TRUE)
  return(inputData)
}
#' Load iterative inputs
#'
#' @param edgeTransportFolder transport folder
#' @param inputFolder the path to the folder containing the input (csv-) files
#' @param inputFiles names of the input files
#' @param numberOfRegions regional resolution
#' @param SSPscenario SSP scenario
#' @param demScenario demand scenario
#' @param transportPolScenario Transport policy scenario
#' @importFrom reporttransport storeData
#' @import data.table
#' @export

toolLoadIterativeInputs <- function(edgeTransportFolder, inputFolder, inputFiles, numberOfRegions, SSPscenario, transportPolScenario, demScenario) {

  # Model input parameters from the package
  ## Exponents discrete choice model
  lambdasDiscreteChoice <- fread(system.file("extdata/genParLambdasDiscreteChoiceModel.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)

  ## Transport policy scenario inconvenience cost factors
  scenParIncoCost <- fread(system.file("extdata/scenParIncoCost.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)
  scenParIncoCost <- scenParIncoCost[SSPscen == gsub("gdp_", "", SSPscenario) & transportPolScen == gsub("ICEban", "", transportPolScenario)][, c("SSPscen", "transportPolScen") := NULL]

  annuityCalc <- fread(system.file("extdata/genParAnnuityCalc.csv", package = "edgeTransport", mustWork = TRUE), header = TRUE)
  # Interest Rate and vehicle service life for annuity calculation
  # NOTE: right now there is only "default". If we add scenario specific annuity parameters, we can shift annuityCalc to the scenPar's
  if  (gsub("ICEban", "", transportPolScenario) %in% annuityCalc$transportPolScen){
    annuityCalc <- annuityCalc[transportPolScen == gsub("ICEban", "", transportPolScenario)][, transportPolScen := NULL]} else {
    annuityCalc <- annuityCalc[transportPolScen == "default"][, transportPolScen := NULL]
  }

  mapEdgeToREMIND <- fread(system.file("extdata/helpersMappingEdgeTtoREMINDcategories.csv", package = "edgeTransport", mustWork = TRUE))
  regionmappingISOto21to12 <- fread(system.file("extdata", "helpersRegionmappingISOto21to12.csv",
                                                package = "edgeTransport"))
  reportingNames <- fread(system.file("extdata", "helpersReportingNames.csv",
                                      package = "edgeTransport"), skip = 1)
  reportingAggregation <- fread(system.file("extdata", "helpersReportingAggregation.csv",
                                            package = "edgeTransport"), skip = 1)
  mitigationTechMap <- fread(system.file("extdata", "helpersMitigationTechmap.csv",
                                         package = "edgeTransport"))
  regionmappingISOto21to12 <- fread(system.file("extdata", "helpersRegionmappingISOto21to12.csv",
                                                package = "edgeTransport"))

  ## Decision tree
  # map on decisiontree for provided spatial resolution
  if (numberOfRegions == 21) {
    decisionTree <- toolLoadDecisionTree("regionCode21")
  } else if (numberOfRegions == 12) {
    decisionTree <- toolLoadDecisionTree("regionCode12")
  } else {
    stop("EDGE-Transport iterative does not suppoert the spatial resolution of ", numberOfRegions, "regions provided by the REMIND gdx. Choose either 12 or 21 regions")
  }

  categories <- c("trn_pass_road_LDV_4W", "trn_pass_road_LDV_2W", "trn_freight_road", "trn_pass", "trn_freight")
  filterEntries <- getFilterEntriesUnivocalName(categories, decisionTree)
  filterEntries[["trackedFleet"]] <- c(filterEntries[["trn_pass_road_LDV_4W"]], filterEntries[["trn_freight_road"]],
                                       getFilterEntriesUnivocalName("Bus", decisionTree)[["Bus"]])

  # Input from REMIND input data
  # In the first iteration input data needs to be loaded
  if (!dir.exists(file.path(edgeTransportFolder)))  {
    print("Loading csv data from input folder and creating RDS files...")
  }

  RDSfiles <- list()
  for (filename in inputFiles) {
    if (length(list.files(file.path(".", edgeTransportFolder), paste0(filename, ".RDS"), recursive = TRUE, full.names = TRUE)) < 1) {
      RDSfiles <- append(RDSfiles, csv2RDS(filename, inputFolder, SSPscenario, demScenario, transportPolScenario))
    }
  }
  if (!is.null(RDSfiles$f29_trpdemand)) {
    mapEdgeSectorToREMIND <- merge(mapEdgeToREMIND, unique(decisionTree[, c("sector", "univocalName")]), by = "univocalName", allow.cartesian = TRUE, all.x = TRUE)
    mapEdgeSectorToREMIND <- mapEdgeSectorToREMIND[!is.na(all_in)]
    mapEdgeSectorToREMIND <- unique(mapEdgeSectorToREMIND[, c("all_in", "sector")])
    RDSfiles$f29_trpdemand <- merge(RDSfiles$f29_trpdemand[period >= 1990], mapEdgeSectorToREMIND, by = "all_in")[, all_in := NULL]
    ## convert unit
    trillionToBillion <- 1e3
    RDSfiles$f29_trpdemand[, value := value
                           * trillionToBillion]
    RDSfiles$f29_trpdemand[, unit := ifelse(sector %in% c("trn_pass", "trn_aviation_intl"), "billion pkm/yr", "billion tkm/yr")][, variable := "ES"]
    setcolorder(RDSfiles$f29_trpdemand, c("region", "period", "sector", "value", "unit"))
  }
  if (length(RDSfiles) > 0)
    storeData(file.path(".", edgeTransportFolder), varsList = RDSfiles)

  if (!length(RDSfiles) == length(inputFiles))
    RDSfiles <- toolLoadRDSinputs(edgeTransportFolder, inputFiles)

  # Time resolution
  dtTimeRes <- unique(RDSfiles$scenSpecEnIntensity[, c("univocalName", "period")])
  highRes <- unique(dtTimeRes$period)
  lowResUnivocalNames <- copy(dtTimeRes)
  lowResUnivocalNames <- lowResUnivocalNames[, .(test = all(highRes %in% period)), by = univocalName]
  lowResUnivocalNames <- lowResUnivocalNames[test == FALSE, univocalName]
  lowTimeRes <- unique(dtTimeRes[univocalName %in% lowResUnivocalNames]$period)
  helpers <- list(dtTimeRes = dtTimeRes,
                  lowTimeRes = lowTimeRes,
                  decisionTree = decisionTree,
                  filterEntries = filterEntries,
                  mapEdgeToREMIND  = mapEdgeToREMIND,
                  reportingNames = reportingNames,
                  reportingAggregation = reportingAggregation,
                  mitigationTechMap = mitigationTechMap,
                  regionmappingISOto21to12 = regionmappingISOto21to12)

  # general model parameters
  genModelPar <- list(
    lambdasDiscreteChoice = lambdasDiscreteChoice,
    annuityCalc = annuityCalc
  )

  # transport scenario (SSPscen + demScen + polScen) specific model parameters
  scenModelPar <- list(
    scenParIncoCost = scenParIncoCost
  )

  return(
    list(
      genModelPar = genModelPar,
      scenModelPar = scenModelPar,
      RDSfiles = RDSfiles,
      helpers = helpers
    )
  )
}

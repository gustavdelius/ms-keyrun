remotes::install_github("NOAA-EDAB/ms-keyrun", ref = "sarah_wgsamsim")
library(mskeyrun)
library(dplyr)

# Species parameters ----

# `focalSpecies` contains various species code for 11 species
View(focalSpecies)
unique(focalSpecies$LongName)
# There are 16 rows because some species have two different codes in the
# commercial fishing database NESPP3
nrow(focalSpecies)

# Let's use the long name as the species name in the mizer model but also
# keep their modelName and the SVSPP code because that is used in other tables.
sp = data.frame(species = unique(focalSpecies$LongName),
                name = unique(focalSpecies$modelName),
                SVSPP = unique(focalSpecies$SVSPP))


# `realBiolPar` holds the length-weight parameters
# The relation is W = aL^b e^Z where Z ~ IIDN(0,sigma^2)
# so that E(W) = ae^((sigma^2)/2) L^b 
realBiolPar = realBiolPar
sp <- sp |>
    left_join(realBiolPar, by = join_by(name == Name)) |>
    mutate(species, name, SVSPP,
           a = WLa * exp(sigma^2 / 2), 
           b = WLb, WLsigma = sigma, .keep = "none")

# Survey abundance data ----
# `surveyIndex`
surveyIndex = surveyIndex

realSurveyLennumcomp = realSurveyLennumcomp

# `surveyIndexAll` contains the survey indices for all species
# Could perhaps be used to determine what proportion of total biomass
# comes from focal species
surveyIndexAll = surveyIndexAll

# `catchIndex` contains commercial landings and discards
catchIndex = catchIndex
unique(catchIndex$units)

# Diet data ----

# `surveyDietcomp` surprisingly does not contain the size of the predator
surveyDietcomp = surveyDietcomp

simFocalSpecies = simFocalSpecies

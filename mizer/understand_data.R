# Here I just explore the simulated data a bit

library(mskeyrun)
library(dplyr)
library(ggplot2)

# Species parameters ----

species <- data.frame(
    Name = c("Long_rough_dab",
             "Green_halibut",
             "Mackerel",
             "Haddock",
             "Saithe",
             "Redfish",
             "Blue_whiting",
             "Norwegian_ssh",
             "North_atl_cod",
             "Polar_cod",
             "Capelin"),
    Long.Name = c("Long rough dab",
                  "Greenland halibut",
                  "Mackerel",
                  "Haddock",
                  "Saithe",
                  "Redfish",
                  "Blue whiting",
                  "Norwegian spring spawning herring",
                  "Northeast Atlantic Cod",
                  "Polar cod",
                  "Capelin"),
    Latin = c("*Hippoglossoides platessoides*",
              "*Reinhardtius hippoglossoides*",
              "*Scomber scombrus*",
              "*Melongrammus aeglefinus*",
              "*Pollachius virens*",
              "*Sebastes mentella*",
              "*Micromesistius poutassou*",
              "*Clupea harengus*",
              "*Gadus morhua*",
              "*Boreogadus saida*",
              "*Mallotus villosus*"),
    Code = c("LRD", "GRH", "MAC", "HAD", "SAI", "RED", 
             "BWH", "SSH", "NCO", "PCO", "CAP")
)

head(simStartPars)
unique(simStartPars$units)

head(simBiolPar)

# Survey data ----
## Biomass ----

head(simSurveyIndex)

svbio <- simSurveyIndex |> filter(variable == "biomass")
ggplot(svbio) +
    geom_line(aes(x = year, y = value, color = survey)) +
    theme(legend.position = "top") +
    xlab("model year") +
    ylab("tons") +
    facet_wrap(~Name, scales = "free")

## Length composition ----

head(simSurveyLencomp)

# convert strings to factors
lencomp <- simSurveyLencomp |>
    mutate(across(where(is.character), as.factor, .names = "{col}"))
summary(lencomp)

# We see that all observations have the same value for `ModSim`, `variable` and 
# `units`. There are two surveys, one in the fall and one in the spring.

## Age-size data ----

### Mean weight at age ----

head(simSurveyWtatAge)
meanwatage <- simSurveyWtatAge |>
    group_by(Name, age) |>
    summarise(w = mean(value))

ggplot(meanwatage) +
    geom_line(aes(x = age, y = w)) +
    facet_wrap(~Name, scales = "free")

### Age-length composition

head(simSurveyAgeLencomp)
unique(simSurveyAgeLencomp$units)

Lencomp <- simSurveyAgeLencomp |>
    group_by(Name, survey, year, lenbin) |>
    summarise(value = sum(value))


head(simSurveyAgecomp)

meanw <- simSurveyAgeLencomp |>
    left_join(simBiolPar, by = "Name") |>
    # calculate mean of the weights at the length-class boundaries
    mutate(w = WLa * (lenbin ^ WLb + (lenbin + 1) ^ WLb) / 2) |>
    group_by(Name, agecl) |>
    summarise(w = weighted.mean(w, value))
    
## Diet ----

head(simSurveyDietcomp)

# Fishery data ----

## Catch

head(simCatchIndex)
unique(simCatchIndex$fishery)

head(simFisheryLencomp)
unique(simFisheryLencomp$units)


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# Import all data ---------------------------------------------------------

#list all files with trap per night in the name
files <- dir("Input/NEON_count-small-mammals/", pattern = "mam_pertrapnight", recursive = TRUE, full.names = TRUE)

#fread that list of files
ls.files <- lapply(files, fread)

#rbind the files
trapdat <- rbindlist(ls.files, fill = TRUE)



# Begin basic cleaning and inventory -------------------------------------------

#remove first three cols that are long IDs
trapdat[, uid := NULL][, nightuid := NULL][, namedLocation := NULL]

#what col names have date in them?
grepv("date", colnames(trapdat), ignore.case = TRUE, )

#look at format of date
trapdat[, head(collectDate)]

#make date and year
trapdat[, date := ymd(collectDate)]
trapdat[, y := year(date)]
trapdat[, m := month(date)]



# look into data ----------------------------------------------------------

#number of unique sites and plots
trapdat[, length(unique(siteID))]
trapdat[, length(unique(plotID))]

#sample size by taxon ID
trapdat[, .N, taxonID]

#collect info of sites for only PELE and PEMA sampling
siteinfo <- trapdat[taxonID == "PELE" | taxonID == "PEMA", 
                    .(lat = mean(decimalLatitude), 
                      long = mean(decimalLongitude),
                      elevation = mean(elevation),
                      habitat = getmode(nlcdClass),
                      numb_years = length(unique(y)),
                      numb_plots = length(unique(plotID)),
                      N = .N),
                    siteID]



# mice data ---------------------------------------------------------------

#grab instances of PELE and PEMA mice
mice <- trapdat[taxonID == "PELE" | taxonID == "PEMA"]

#get sample size + weight + foot BY (year, month, and species)
pop <- mice[, .(N = .N, 
                weight = mean(weight, na.rm = TRUE), 
                foot = mean(hindfootLength, na.rm = TRUE)), 
            by = .(y, m, taxonID)]

#reorder
setorder(pop, m, y)

#make year a factor, make month an integer
pop[, y := as.factor(y)]
pop[, m := as.factor(m)]


ggplot(pop)+
  geom_line(aes(x = m, y = N, group = taxonID, color = taxonID))+
  labs(x = "month", y = "sample size")+
  facet_wrap(~y)


ggplot(pop)+
  geom_line(aes(x = m, y = weight, group = taxonID, color = taxonID))+
  labs(x = "month", y = "mean weight (g)")+
  facet_wrap(~y)


ggplot(pop)+
  geom_line(aes(x = m, y = foot, group = taxonID, color = taxonID))+
  labs(x = "month", y = "mean foot length (mm)")+
  facet_wrap(~y)


ggplot(mice[m == 9])+
  geom_point(aes(x = decimalLatitude, y = weight, color = taxonID))+
  geom_smooth(aes(x = decimalLatitude, y= weight, color = taxonID), method = "lm")+
  labs(x = "Latitude", y = "Weight (g)", title = "September weights")


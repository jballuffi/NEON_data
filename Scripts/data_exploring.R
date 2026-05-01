
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# Import all data ---------------------------------------------------------

#predator bird data previously cleaned/prepped
preddat <- readRDS("Output/Data/predator_abundance.rds")

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



# get general mice densities and body size ---------------------------------------------------------------

#grab instances of PELE and PEMA mice
mice <- trapdat[taxonID == "PELE" | taxonID == "PEMA"]

#make year a factor, make month an integer
mice[, y := as.factor(y)]
mice[, m := as.factor(m)]

#get mice abundance + weight + foot BY (year, month)
byyear <- mice[, .(N = .N, 
                   weight = mean(weight, na.rm = TRUE), 
                   foot = mean(hindfootLength, na.rm = TRUE)), 
               by = .(y, m, taxonID)]

#reorder
setorder(byyear, taxonID, m, y)


#get sample size + weight + foot BY (year, month, and species)
bysite <- mice[, .(N = .N, 
                weight = mean(weight, na.rm = TRUE), 
                foot = mean(hindfootLength, na.rm = TRUE)), 
            by = .(siteID, y, taxonID)]




# merge bysite with predator abundance ---------------------------------------------

#change col name that is the same as Mice
setnames(preddat, "N", "predabund")

#merge pop and pred datasets
bysite <- merge(bysite, preddat, by = c("y", "siteID"), all.x = TRUE)

#anywhere with NA predators make a zero. This might be wrong
#correct later if needed
bysite[is.na(predabund), predabund := 0]



# explore full mice dataset ----------------------------------------------------

#look at body mass in september in response to latitude
ggplot(mice[m == 9])+
  geom_point(aes(x = decimalLatitude, y = weight, color = taxonID))+
  geom_smooth(aes(x = decimalLatitude, y= weight, color = taxonID), method = "lm")+
  labs(x = "Latitude", y = "Weight (g)", title = "September weights")


ggplot(mice[adultTicksAttached == "N" | adultTicksAttached == "Y"])+
  geom_boxplot(aes(fill = adultTicksAttached, y = weight, x = taxonID))+
  scale_fill_discrete(name = "Ticks")+
  facet_wrap(~m, scales = "free")

ggplot(mice[sex == "F" & pregnancyStatus == "pregnant" | pregnancyStatus == "not"])+
  geom_boxplot(aes(x = pregnancyStatus, y = weight))





# explore the by year dataset --------------weighted.mean()# explore the by year dataset -----------------------------------------

#mice abundance by month for each year
ggplot(byyear)+
  geom_line(aes(x = m, y = N, group = taxonID, color = taxonID))+
  labs(x = "month", y = "sample size")+
  facet_wrap(~y)

#mice weight by month for each year
ggplot(byyear)+
  geom_line(aes(x = m, y = weight, group = taxonID, color = taxonID))+
  labs(x = "month", y = "mean weight (g)")+
  facet_wrap(~y)

#mice foot length by month for each year
ggplot(byyear)+
  geom_line(aes(x = m, y = foot, group = taxonID, color = taxonID))+
  labs(x = "month", y = "mean foot length (mm)")+
  facet_wrap(~y)



# explore the by site data set ----------------------------------------------

#is there any pattern between mice abundance and predator abundance
ggplot(bysite)+
  geom_point(aes(x = N, y = predabund))+
  geom_smooth(aes(x = N, y = predabund), method = "lm")+
  facet_wrap(~taxonID)

bysite[predabund > 0, predbin := "yes"]
bysite[predabund == 0, predbin := "no"]

ggplot(bysite)+
  geom_boxplot(aes(x = predbin, y = N))




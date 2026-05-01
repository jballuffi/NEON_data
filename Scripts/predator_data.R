
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# import all data ---------------------------------------------------------

birdfiles <- dir("Input/NEON_count-landbird/", pattern = "brd_countdata", recursive = TRUE, full.names = TRUE)

#fread that list of files
ls.files <- lapply(birdfiles, fread)

#rbind the files
birddat <- rbindlist(ls.files, fill = TRUE)



# get just birds of prey -----------------------------------------------------

#remove first three cols that are long IDs
birddat[, uid := NULL][, domainID := NULL][, namedLocation := NULL]

#get a list of all species recorded
species <- birddat[, .(species = unique(scientificName))]

#get genus of species name
birddat[, genus := tstrsplit(scientificName, " ", keep = 1)]

#make a list of potential genera for birds of prey
preygenera <- c("Buteo", "Accipiter", "Falco", "Haliaeetus", "Buteogallus", "Circus", "Aquila", "Elanus", 
"Pandion", "Cathartes", "Coragyps", "Gymnogyps", "Bubo", "Strix", "Tyto")

#take only data that falls into these genera
preddat <- birddat[genus %in% preygenera] 



# prep and clean data -----------------------------------------------------

#make date col
preddat[, date := tstrsplit(startDate, "T", keep = 1)]
preddat[, date := ymd(date)]

#create year and month cols
preddat[, y := year(date)]
preddat[, m := month(date)]

#total number of sites
preddat[, length(unique(siteID))]

#total number of years
preddat[, length(unique(y))]

#point ID is a factor
preddat[, pointID := as.character(pointID)]



# basic abundances of birds of prey by site -------------------------------

#get total 
abund <- preddat[, .(N = sum(clusterSize)), by = .(y, siteID)]

#make year a factor
abund[, y := as.factor(y)]

#minimum pred count and max
abund[, min(N)]
abund[, max(N)]

#distribution of predator abundance
ggplot(abund)+
  geom_histogram(aes(x = N))

#predator abundance by site
ggplot(abund)+
  geom_boxplot(aes(x = siteID, y = N))

#predator abundance by year
ggplot(abund)+
  geom_boxplot(aes(x = y, y = N))

#the distribution of predator abundance is definitely spatial
summary(lm(N ~ y, abund))
summary(lm(N ~ siteID, abund))



# save predator abundances ------------------------------------------------

saveRDS(abund, "Output/Data/predator_abundance.rds")



library(data.table)
library(tidyr)

# college data
college = fread('../data/college.csv')
by_county = college[,.(total = sum(enrollment)), by = c('county', 'state', 'plan')]
college_county = spread(by_county, plan, total)
college_county[is.na(college_county)] = 0
college_county$county = tolower(college_county$county)

# county level data
a = fread('../data/county_level/Education.csv')
b = fread('../data/county_level/Health Professional Shortage Areas_ Primary Care, by County, 2020.csv')
c = fread('../data/county_level/PopulationEstimates.csv')
d = fread('../data/county_level/PovertyEstimates.csv')
e = fread('../data/county_level/Unemployment.csv')
names(a)[1] = 'fcode'
names(c)[1] = 'fcode'
names(d)[1:3] = c('fcode', 'state', 'county')
names(e)[1] = 'fcode'
names(b)[1:2] = c('county', 'state')
d = d[a, on = 'fcode', nomatch = FALSE]
d = d[e, on = 'fcode', nomatch = FALSE]
d = d[c, on = 'fcode', nomatch = FALSE]
d = d[State != 'PR']
d = d[,c(1,2,3,4,5,8,9,10,11,14,17)]
d$county = tolower(d$county)
b$county = tolower(b$county)
dcounty = paste(d$county,d$state)
bcounty = paste(b$county,b$state)
setdiff(dcounty, bcounty)
setdiff(bcounty, dcounty)
county_level = d[b, on = c('county', 'state'), nomatch = FALSE]
county_level = county_level[!duplicated(paste(county, state))]

# merge county and college
ucounty = paste(college_county$county, college_county$state)
countycounty = paste(county_level$county, county_level$state)
setdiff(ucounty, countycounty)
college_county_merged = college_county[county_level, on = c('state', 'county')]
college_county_merged = college_county_merged[,-10]

# merge 1point3acres.com 
b = fread('../data/cases.csv')
b8 = b[confirmed_date >= '8' & confirmed_date <= '9']
b9 = b[confirmed_date >= '9']
b$county_name = tolower(b$county_name)
bb = b[,.(total = sum(confirmed_count)), by = c('state_name', 'county_name')]
b8$county_name = tolower(b8$county_name)
b9$county_name = tolower(b9$county_name)
b8 = b8[,.(total8 = sum(confirmed_count)), by = c('state_name', 'county_name')]
b9 = b9[,.(total9 = sum(confirmed_count)), by = c('state_name', 'county_name')]
s = merge(b8,b9, by = c('state_name', 'county_name'), all = TRUE)
s = merge(s,bb, by = c('state_name', 'county_name'), all = TRUE)
s = s[,1:4]
names(s) = c('state', 'county', 'total8', 'total9')
setnafill(s,fill = 0, cols = c('total8', 'total9'))
s$county = tolower(s$county)
county1 = s$county
county1[which(county1 == 'obrien')] = "o'brien"
county1[which(county1 == 'lasalle')] = 'la salle'
county1[which(county1 == 'dekalb' & s$state == 'IN')] = 'de kalb'
county1[which(county1 == 'laporte' & s$state == 'IN')] = 'la porte'
county1[which(county1 == 'prince georges')] = "prince george's"
county1[which(county1 == 'queen annes')] = "queen anne's"
county1[which(county1 == 'st. marys')] = "st. mary's"
county1[which(county1 == 'de baca')] = 'debaca'
county1[which(county1 == 'mckean' & s$state == 'PA')] = 'mc kean'
s$county = county1

county2 = gsub(' county', '', college_county_merged$county)
county2 = gsub(' city and borough', '', county2)
county2 = gsub(' borough', '', county2)
county2 = gsub(' parish', '', county2)
county2 = gsub(' census', '', county2)
county2 = gsub(' area', '', county2)
county2[which(college_county_merged$state == 'VA' & !(county2 %in% c('fairfax city', 'franklin city', 'richmond city', 'roanoke city')))] = 
  gsub(' city', '',county2[which(college_county_merged$state == 'VA' & !(county2 %in% c('fairfax city', 'franklin city', 'richmond city', 'roanoke city')))])


st = paste(county2, college_county_merged$state)
ct = paste(s$county, s$state)
setdiff(ct,st)
setdiff(st,ct)
college_county_merged$county = county2
college_county_1point = college_county_merged[s, on = c('county', 'state'), nomatch = FALSE]

# +age group
age = fread('../data/cc-est2019-alldata.csv')
state_abbr = fread('../data/stateCode.csv')
names(state_abbr) = c('state', 'abbr')
age = age[state_abbr, on = .(STNAME==state), nomatch = FALSE]
age$STNAME = age$abbr
age = age[YEAR == 12]
tot = age[AGEGRP == 0, c(4,5,8)]
agee = age[AGEGRP != 0, c(4,5,7,8)]
agee = agee[tot, on = c('STNAME', 'CTYNAME')]
agee = agee[,.(state = STNAME, county = CTYNAME,age_group =  AGEGRP, p = TOT_POP/i.TOT_POP)]
agee = spread(agee, age_group, p)
names(agee)[3:20] = paste('age_group_', names(agee)[3:20], sep = '')
agee$county = tolower(agee$county)
county3 = gsub(' county', '', agee$county)
county3 = gsub(' city and borough', '', county3)
county3 = gsub(' borough', '', county3)
county3 = gsub(' parish', '', county3)
county3 = gsub(' census', '', county3)
county3 = gsub(' area', '', county3)
county3 = gsub(' municipality', '', county3)
county3[which(agee$state == 'VA')] = gsub(' city', '', county3[which(agee$state == 'VA')])
county3[which(county3 == "lasalle" & agee$state == 'LA')] = 'la salle'
county3[which(county3 == "doña ana")] = 'dona ana'
county3[which(county3 == 'lasalle' & agee$state == 'IL')] = 'la salle'
county3[which(county3 == 'dekalb' & agee$state == 'IN')] = 'de kalb'
county3[which(county3 == 'laporte' & agee$state == 'IN')] = 'la porte'
county3[which(county3 == 'de baca' & agee$state == 'NM')] = 'debaca'
county3[which(county3 == 'mckean' & agee$state == 'PA')] = 'mc kean'
ageeee = paste(county3, agee$state)
res222 = paste(college_county_1point$county, college_county_1point$state)
agee$county = county3
withage = agee[college_county_1point, on = c('state', 'county'), nomatch = FALSE]


# arrange columns
withage$age_group_0_19  = withage$age_group_1 + withage$age_group_2 + withage$age_group_3 + withage$age_group_4
withage$age_group_20_49 = withage$age_group_5 + withage$age_group_6 + 
  withage$age_group_7 + withage$age_group_8 + withage$age_group_9 + withage$age_group_10
withage$age_group_50_64 = withage$age_group_11 + withage$age_group_12 + withage$age_group_13
withage$age_group_65 = withage$age_group_14 + withage$age_group_15 + withage$age_group_16 + withage$age_group_17 + 
  withage$age_group_18

withage$`Fully in person` = withage$`Fully in person` / as.numeric(gsub(',', '', withage$POP_ESTIMATE_2019))
withage$`Fully online` = withage$`Fully online` / as.numeric(gsub(',', '', withage$POP_ESTIMATE_2019))
withage$Hybrid = withage$Hybrid / as.numeric(gsub(',', '', withage$POP_ESTIMATE_2019))
withage$Other = withage$Other / as.numeric(gsub(',', '', withage$POP_ESTIMATE_2019))
withage$`Primarily in person` = withage$`Primarily in person` / as.numeric(gsub(',', '', withage$POP_ESTIMATE_2019))
withage$`Primarily online` = withage$`Primarily online` / as.numeric(gsub(',', '', withage$POP_ESTIMATE_2019))
withage$Undetermined = withage$Undetermined / as.numeric(gsub(',', '', withage$POP_ESTIMATE_2019))
withage$MEDHHINC_2018 = as.numeric(gsub(',', '', withage$MEDHHINC_2018))
withage = setnafill(withage, fill = 0, cols = names(withage)[21:27])
withage$percent_of_some_co = withage$`Percent of adults completing some college or associate's degree, 2014-18` + 
  withage$`Percent of adults with a bachelor's degree or higher, 2014-18`

withage = withage[,c(1,2,39,40,41,42,21:29,43,34:38)]
names(withage)[19] = 'shortage'
withage$POP_ESTIMATE_2019 = as.numeric(gsub(',','', withage$POP_ESTIMATE_2019))

withage = withage[!duplicated(paste(withage$state, withage$county))]
withage$August_proportion = withage$total8 / withage$POP_ESTIMATE_2019
withage$September_proportion = withage$total9 / withage$POP_ESTIMATE_2019
withage = withage[,c(1:19,22,23)]

withage$inperson = withage$`Fully in person` + withage$`Primarily in person`
withage$online = withage$`Fully online` + withage$`Primarily online`
withage = withage[,c(1:6,22,9,23,14:21)]

# +land area
data = as.data.frame(withage)
data$state = as.factor(data$state)
land = read.csv("../data/landArea.csv")
colnames(land) = c("county", "state", "landArea")
land$county = tolower(land$county)
dataMerge = data.frame()
for (state in levels(data$state)){
  dat1 = data[data$state == state,]
  dat2 = land[land$state == state,-2]
  dat_ = merge(dat1, dat2, by = "county", all.x = T)
  if (nrow(dat_) > nrow(dat1)){
    print(state)
  }
  dataMerge = rbind(dataMerge, dat_)
}
# + race
data = dataMerge
race = read.csv("../data/race.csv")
stateCode = read.csv("../data/stateCode.csv")
colnames(stateCode) = c("STNAME", "state")
race = merge(race, stateCode, by = "STNAME",  all.x = T)
race$CTYNAME = tolower(race$CTYNAME)
race$CTYNAME = gsub(" county", "", race$CTYNAME)
race$CTYNAME = gsub(" borough", "", race$CTYNAME)
race$CTYNAME = gsub(" census area", "", race$CTYNAME)
race$CTYNAME = gsub(" city and", "", race$CTYNAME)
race$CTYNAME = gsub(" municipality", "", race$CTYNAME)
race$CTYNAME = gsub(" parish", "", race$CTYNAME)
raceProcessed = data.frame(state = race$state, county = race$CTYNAME, TOT_POP = race$TOT_POP, WA = race$WA_FEMALE + race$WA_MALE, BA = race$BA_FEMALE + race$BA_MALE, IA = race$IA_FEMALE + race$IA_MALE, AA = race$AA_FEMALE + race$AA_MALE, NA_POP = race$NA_FEMALE + race$NA_MALE, TOM = race$TOM_FEMALE + race$TOM_MALE, WAC = race$WAC_FEMALE + race$WAC_MALE, BAC = race$BAC_FEMALE + race$BAC_MALE, IAC = race$IAC_FEMALE + race$IAC_MALE, AAC = race$AAC_FEMALE + race$AAC_MALE, NAC = race$NAC_FEMALE + race$NAC_MALE)

dataMerge = data.frame()
for (state in levels(data$state)){
  dat1 = data[data$state == state,]
  dat2 = raceProcessed[raceProcessed$state == state, -1]
  if (state == "VA"){
    dat2$county = gsub(" city", "", dat2$county)
  }
  dat_ = merge(dat1, dat2, by = "county", all.x = T)
  if (nrow(dat_) > nrow(dat1)){
    print(state)
  }
  dataMerge = rbind(dataMerge, dat_)
}

# arrange columns
data = data.table(dataMerge)
data = data[!duplicated(paste(state, county))]
data$population_density = data$POP_ESTIMATE_2019 / data$landArea
data$WA = data$WA / data$TOT_POP
data$BA = data$BA / data$TOT_POP
data$IA = data$IA / data$TOT_POP
data$AA = data$AA / data$TOT_POP
data$NA_POP = data$NA_POP / data$TOT_POP
data$TOM = data$TOM / data$TOT_POP
data$enrollment_proportion = data$inperson+ data$Hybrid

# + 1point3acres more statistics
b = fread('../data/cases2.csv')
bnow = b[confirmed_date >= '9' |( confirmed_date >= '10' & confirmed_date <='11')]
bnow = bnow[,.(totalnow = sum(confirmed_count)), by = c('state_name', 'county_name')]
bnow_30 = b[(confirmed_date >= '9/23' & confirmed_date <='9/3')|confirmed_date == '9/30/2020'|( confirmed_date >= '10' & confirmed_date <='11')]
bnow_30 = bnow_30[,.(totalnow_30 = sum(confirmed_count)), by = c('state_name', 'county_name')]
names(bnow) = c('state', 'county', 'totalnow')
names(bnow_30) = c('state', 'county', 'totalnow_30')
bnow$county = tolower(bnow$county)
bnow_30$county = tolower(bnow_30$county)
bb = b[,.(total = sum(confirmed_count)), by = c('state_name', 'county_name')]
names(bb) = c('state', 'county', 'total')
bb$county = tolower(bb$county)
bnow = bnow[bb, on = c('state', 'county')]
bnow = bnow[,c(1,2,3)]
setnafill(bnow, fill = 0, cols = c('totalnow'))
bnow_30 = bnow_30[bb, on = c('state', 'county')]
bnow_30 = bnow_30[,c(1,2,3)]
setnafill(bnow_30, fill = 0, cols = c('totalnow_30'))
county4 = bnow$county
county4[which(county4 == "obrien")] = "o'brien"
county4[which(county4 == "lasalle" & bnow$state == 'IL')] = 'la salle'
county4[which(county4 == 'dekalb' & bnow$state == 'IN')] = 'de kalb'
county4[which(county4 == 'laporte')] = 'la porte'
county4[which(county4 == 'queen annes')] = "queen anne's"
county4[which(county4 == "st. marys")] = "st. mary's"
county4[which(county4 == "wayne--detroit")] = 'wayne'
county4[which(county4 == 'wayne--non detroit')] = 'wayne'
county4[which(county4 == 'de baca')] = 'debaca'
county4[which(county4 == 'mckean')] = 'mc kean'
county4[which(county4 == 'buena vista city')] = 'buena vista'
county4[which(county4 == 'charles city')] = 'charles'
county4[which(county4 == 'james city')] = 'james'
county4[which(county4 == 'manassas city')] = 'manassas'
bnow$county = county4
bnow = bnow[,.(totalnow = sum(totalnow)), by = c('state', 'county')]
county4 = bnow_30$county
county4[which(county4 == "obrien")] = "o'brien"
county4[which(county4 == "lasalle" & bnow_30$state == 'IL')] = 'la salle'
county4[which(county4 == 'dekalb' & bnow_30$state == 'IN')] = 'de kalb'
county4[which(county4 == 'laporte')] = 'la porte'
county4[which(county4 == 'queen annes')] = "queen anne's"
county4[which(county4 == "st. marys")] = "st. mary's"
county4[which(county4 == "wayne--detroit")] = 'wayne'
county4[which(county4 == 'wayne--non detroit')] = 'wayne'
county4[which(county4 == 'de baca')] = 'debaca'
county4[which(county4 == 'mckean')] = 'mc kean'
county4[which(county4 == 'buena vista city')] = 'buena vista'
county4[which(county4 == 'charles city')] = 'charles'
county4[which(county4 == 'james city')] = 'james'
county4[which(county4 == 'manassas city')] = 'manassas'
bnow_30$county = county4
bnow_30 = bnow_30[,.(totalnow_30 = sum(totalnow_30)), by = c('state', 'county')]
data = bnow[data, on = c('county', 'state')]
data = bnow_30[data, on = c('county', 'state')]

# arrange columns
data$totalnow = data$totalnow / data$POP_ESTIMATE_2019
data$totalnow_30 = data$totalnow_30 / data$POP_ESTIMATE_2019
data$total_oct = data$totalnow - data$September_proportion
data$August_proportion = data$August_proportion / 31
data$September_proportion = data$September_proportion / 30
data$totalnow = data$totalnow / 53
data$totalnow_30 = data$totalnow_30/30
data$total_oct = data$total_oct / 23
setnafill(data, fill = 0, cols = c('August_proportion', 'September_proportion', 'totalnow', 'totalnow_30'))
# data = data[which(!is.na(population_density))]
# data = data[which(totalnow != max(totalnow))]
data$minority = 1 - data$WA
data$September_proportion = data$September_proportion * 10000
data$August_proportion = data$August_proportion * 10000
data$totalnow = data$totalnow * 10000
data$totalnow_30 = data$totalnow_30 * 10000
data$total_oct = data$total_oct * 10000
# fwrite(data,'data_new_5.csv')

# + test rate
test_rate = fread('../data/covid19_tests_performed_by_state.csv')
state_abbr = fread('../data/stateCode.csv')
names(state_abbr) = c('state', 'abbr')
state_test_rate = state_abbr[test_rate, on = .(state == State)]
state_test_rate = state_test_rate[,c(2,3)]
names(state_test_rate) = c('state', 'test_people')
population = fread('../data/county_level/PopulationEstimates.csv')
population = population[FIPStxt %% 1000 == 0]
population = population[,.(state = State, Population_state = as.numeric(gsub(',','', POP_ESTIMATE_2019)))]
state_test_rate = population[state_test_rate, on = 'state']
state_test_rate = state_test_rate[,.(state = state, test_rate = test_people / Population_state)]
data2 = data[state_test_rate, on = 'state', nomatch = FALSE]

# + health care beds
beds = fread('../data/HealthCareProviderCapacity.csv')
beds = beds[,c(1,2)]
names(beds) = c('state', 'total_beds_state')
beds = state_abbr[beds, on = 'state', nomatch = FALSE]
beds = population[beds, on = .(state == abbr)]
beds = beds[,.(state = state, beds_per_10000 = total_beds_state / Population_state * 10000)]
data3 = data2[beds, on = 'state', nomatch = FALSE]
fwrite(data3, '../data/data_all_city_final.csv')
data3 = data3[college_county_merged[,c(1,2,6,9)], on = c('county', 'state'), nomatch = FALSE]
setnafill(data3,fill = 0, cols = c('Other', 'Undetermined'))
data3 = data3[which(data3$Other == 0 & data3$Undetermined == 0)]
data3 = data3[,-c(39,40)]
negative = b[confirmed_date >= '8' | (confirmed_date >= '10' & confirmed_date <= '11')]
negative = negative[confirmed_count <= -5]
negative = negative[,.(state = state_name, county = county_name)]
negative$county = tolower(negative$county)
data3 = data3[which(!is.na(population_density))]
data3 = data3[!negative, on = c('state', 'county')]
fwrite(data3,'../data/data_final_final.csv')
data_nc = data3[which(data3$POP_ESTIMATE_2019 < 450000)]
fwrite(data_nc, '../data/data_without_large_city_final.csv')
require(tidyverse)
require(readr)

#ACT ----
ACT_Clean <- read.csv('Data/ACT_School_Scores_2019.csv') %>%
  filter(Valid.Tests >= 10) %>%
  mutate(Participation.Rate = as.numeric(as.character(Participation.Rate)),
         Average.English.Score = as.numeric(as.character(Average.English.Score)),
         Average.Math.Score = as.numeric(as.character(Average.Math.Score)),
         Average.Reading.Score = as.numeric(as.character(Average.Reading.Score)),
         Average.Science.Score = as.numeric(as.character(Average.Math.Score)),
         Average.Composite.Score = as.numeric(as.character(Average.Composite.Score)),
         Number.Scoring.21.or.Higher = as.numeric(as.character(Number.Scoring.21.or.Higher)),
         Number.Scoring.Below.19 = as.numeric(as.character(Number.Scoring.Below.19))) %>% 
  select(-Percent.Scoring.21.or.Higher, -Percent.Scoring.Below.19)


#Police Incidence ----
Police_Incidents_Clean <- read.csv('Data/MNPD_Incidents_2019.csv') %>%
  mutate(Primary.Key = as.character(Primary.Key),
         Report.Type.Description = case_when(Report.Type == 'D' ~ 'DISPATCHED',
                                             Report.Type == 'O' ~ 'OTHER',
                                             Report.Type == 'S' ~ 'SELF-INITIATED',
                                             Report.Type == 'T' ~ 'TELESERVE',
                                             Report.Type == 'W' ~ 'WALK-IN',
                                             T ~ as.character(Report.Type.Description)),
         Incident.Occurred = as.POSIXct(Incident.Occurred, format = '%m/%d/%Y %I:%M:%S %p'),
         Incident.Reported = as.POSIXct(Incident.Reported, format = '%m/%d/%Y %I:%M:%S %p'),
         Offense.NIBRS = as.character(Offense.NIBRS),
         Offense.Description = case_when(Offense.NIBRS == '09A' ~ 'Murder and Non-negligent Manslaughter',
                                         Offense.NIBRS == '09B' ~ 'Negligent Manslaughter',
                                         Offense.NIBRS == '09C' ~ 'Justifiable Homicide',
                                         Offense.NIBRS == '100' ~ 'Kidnaping/Abduction',
                                         Offense.NIBRS == '11A' ~ 'Forcible Rape',
                                         Offense.NIBRS == '11B' ~ 'Forcible Sodomy',
                                         Offense.NIBRS == '11C' ~ 'Sexual Assault with an Object',
                                         Offense.NIBRS == '11D' ~ 'Forcible Fondling',
                                         Offense.NIBRS == '120' ~ 'Robbery',
                                         Offense.NIBRS == '13A' ~ 'Aggravated Assault',
                                         Offense.NIBRS == '13B' ~ 'Simple Assault',
                                         Offense.NIBRS == '13C' ~ 'Intimidation',
                                         Offense.NIBRS == '13D' ~ 'Stalking',
                                         Offense.NIBRS == '200' ~ 'Arson',
                                         Offense.NIBRS == '210' ~ 'Extortion/Blackmail',
                                         Offense.NIBRS == '220' ~ 'Burglary/Breaking & Entering',
                                         Offense.NIBRS == '23A' ~ 'Pocket-picking',
                                         Offense.NIBRS == '23B' ~ 'Purse-snatching',
                                         Offense.NIBRS == '23C' ~ 'Shoplifting',
                                         Offense.NIBRS == '23D' ~ 'Theft From Building',
                                         Offense.NIBRS == '23E' ~ 'From Coin-Operated Machine or Device',
                                         Offense.NIBRS == '23F' ~ 'Theft From Motor Vehicle',
                                         Offense.NIBRS == '23G' ~ 'Theft of Motor Vehicle Parts or Accessories',
                                         Offense.NIBRS == '23H' ~ 'All Other Larceny',
                                         Offense.NIBRS == '240' ~ 'Motor Vehicle Theft',
                                         Offense.NIBRS == '250' ~ 'Counterfeiting/Forgery',
                                         Offense.NIBRS == '26A' ~ 'False/Pretenses/Swindle/Confidence Game',
                                         Offense.NIBRS == '26B' ~ 'Credit Card/Automatic Teller Machine Fraud',
                                         Offense.NIBRS == '26C' ~ 'Impersonation',
                                         Offense.NIBRS == '26D' ~ 'Welfare Fraud',
                                         Offense.NIBRS == '26E' ~ 'Wire Fraud',
                                         Offense.NIBRS == '26F' ~ 'Identity Theft',
                                         Offense.NIBRS == '26G' ~ 'Hacking/Computer Invasion',
                                         Offense.NIBRS == '270' ~ 'Embezzlement',
                                         Offense.NIBRS == '280' ~ 'Stolen Property Offenses (Receiving, etc.)',
                                         Offense.NIBRS == '290' ~ 'Destruction/Damage/Vandalism of Property', 
                                         Offense.NIBRS == '35A' ~ 'Drug/Narcotic Violations',
                                         Offense.NIBRS == '35B' ~ 'Drug Equipment Violations',
                                         Offense.NIBRS == '36A' ~ 'Incest',
                                         Offense.NIBRS == '36B' ~ 'Statutory Rape',
                                         Offense.NIBRS == '370' ~ 'Pornography/Obscene Material',
                                         Offense.NIBRS == '39A' ~ 'Betting/Wagering',
                                         Offense.NIBRS == '39B' ~ 'Operating/Promoting/Assisting Gambling',
                                         Offense.NIBRS == '39C' ~ 'Gambling Equipment Violations',
                                         Offense.NIBRS == '39D' ~ 'Sports Tampering',
                                         Offense.NIBRS == '40A' ~ 'Prostitution',
                                         Offense.NIBRS == '40B' ~ 'Assisting or Promoting Prostitution',
                                         Offense.NIBRS == '40C' ~ 'Purchasing Prostitution',
                                         Offense.NIBRS == '510' ~ 'Bribery',
                                         Offense.NIBRS == '520' ~ 'Weapon Law Violations',
                                         Offense.NIBRS == '610' ~ 'Abortion',
                                         Offense.NIBRS == '620' ~ 'Accidental Injury',
                                         Offense.NIBRS == '630' ~ 'Aircraft Accident',
                                         Offense.NIBRS == '640' ~ 'Non-Reportable Homicide',
                                         Offense.NIBRS == '64A' ~ 'Human Trafficking, Commercial Sex Acts',
                                         Offense.NIBRS == '64B' ~ 'Human Trafficking, Involuntary Servitude',
                                         Offense.NIBRS == '650' ~ 'Bombing',
                                         Offense.NIBRS == '660' ~ 'Civil Rights',
                                         Offense.NIBRS == '680' ~ 'Death - Accidental',
                                         Offense.NIBRS == '685' ~ 'Death - Natural',
                                         Offense.NIBRS == '690' ~ 'Death - Suicide',
                                         Offense.NIBRS == '695' ~ 'Death - Other',
                                         Offense.NIBRS == '700' ~ 'Escape',
                                         Offense.NIBRS == '710' ~ 'Violation of Order of Protection',
                                         Offense.NIBRS == '715' ~ 'Found/Lost Property',
                                         Offense.NIBRS == '720' ~ 'Harassment',
                                         Offense.NIBRS == '730' ~ 'Indecent Exposure',
                                         Offense.NIBRS == '735' ~ 'Legal Intervention',
                                         Offense.NIBRS == '740' ~ 'Matter of Record',
                                         Offense.NIBRS == '750' ~ 'Obscene Conduct',
                                         Offense.NIBRS == '760' ~ 'Overdose - Accidental',
                                         Offense.NIBRS == '770' ~ 'Psychological Evaluation',
                                         Offense.NIBRS == '780' ~ 'Recovered Property',
                                         Offense.NIBRS == '790' ~ 'Riots - Inciting',
                                         Offense.NIBRS == '800' ~ 'Watercraft Accident',
                                         Offense.NIBRS == '810' ~ 'Lost Property',
                                         Offense.NIBRS == '820' ~ 'Suspicious Object',
                                         Offense.NIBRS == '850' ~ 'Violation of Order of Protection',
                                         Offense.NIBRS == '90A' ~ 'Bad Checks',
                                         Offense.NIBRS == '90B' ~ 'Curfew/Loitering/Vagrancy/Violations',
                                         Offense.NIBRS == '90C' ~ 'Disorderly Conduct',
                                         Offense.NIBRS == '90D' ~ 'Driving Under The Influence',
                                         Offense.NIBRS == '90E' ~ 'Drunkenness',
                                         Offense.NIBRS == '90F' ~ 'Family Offenses, Nonviolent',
                                         Offense.NIBRS == '90G' ~ 'Liquor Law Violations',
                                         Offense.NIBRS == '90H' ~ 'Peeping Tom',
                                         Offense.NIBRS == '90I' ~ 'Runaway',
                                         Offense.NIBRS == '90J' ~ 'Trespass of Real Property',
                                         Offense.NIBRS == '90Z' ~ 'All Other Offenses',
                                         T ~ ''),
         Weapon.Primary = as.character(Weapon.Primary),
         Weapon.Description = case_when(Weapon.Primary == '01' ~ 'Handgun',
                                        Weapon.Primary == '01A' ~ 'Handgun Automatic',
                                        Weapon.Primary == '01S' ~ 'Handgun Semi-Automatic',
                                        Weapon.Primary == '02' ~ 'Rifle',
                                        Weapon.Primary == '02A' ~ 'Rifle Automatic',
                                        Weapon.Primary == '02S' ~ 'Rifle Semi-Automatic',
                                        Weapon.Primary == '03' ~ 'Shotgun',
                                        Weapon.Primary == '04' ~ 'Revolver',
                                        Weapon.Primary == '05' ~ 'Other Firearm',
                                        Weapon.Primary == '06' ~ 'Knife/Cutting Instrument',
                                        Weapon.Primary == '07' ~ 'Blunt Object',
                                        Weapon.Primary == '08' ~ 'Motor Vehicle',
                                        Weapon.Primary == '09' ~ 'Personal (Hands, Etc.)',
                                        Weapon.Primary == '10' ~ 'Poison',
                                        Weapon.Primary == '11' ~ 'Explosives',
                                        Weapon.Primary == '12' ~ 'Fire/Incendiary Devices',
                                        Weapon.Primary == '13' ~ 'Drugs/Narcotics/Sleeping Pills',
                                        Weapon.Primary == '14' ~ 'Asphyxiation',
                                        Weapon.Primary == '15' ~ 'Other',
                                        Weapon.Primary == '16' ~ 'Unknown',
                                        Weapon.Primary == '17' ~ 'None',
                                        Weapon.Primary == '18' ~ 'Bondage',
                                        T ~ 'Unknown'),
         Victim.Type = as.character(Victim.Type),
         Victim.Description = case_when(Victim.Type == 'B' ~ 'Business',
                                        Victim.Type == 'F' ~ 'Financial Institution',
                                        Victim.Type == 'G' ~ 'Government',
                                        Victim.Type == 'I' ~ 'Individual (18 and Over)',
                                        Victim.Type == 'O' ~ 'Other',
                                        Victim.Type == 'P' ~ 'Police Officer',
                                        Victim.Type == 'R' ~ 'Religious',
                                        Victim.Type == 'S' ~ 'Society',
                                        Victim.Type == 'U' ~ 'Unknown',
                                        T ~ 'Unknown'),
         Victim.Gender = as.character(Victim.Gender),
         Victim.Gender = if_else(Victim.Gender == 'W', 'F', Victim.Gender),
         Victim.Race = case_when(Victim.Ethnicity == 'Hispanic' ~ 'Hispanic',
                                 Victim.Race == 'A' ~ 'Asian',
                                 Victim.Race == 'B' ~ 'Black',
                                 Victim.Race == 'H' ~ 'Hispanic',
                                 Victim.Race == 'O' ~ 'Other',
                                 Victim.Race == 'I' ~ 'Native American',
                                 Victim.Race == 'P' ~ 'Pacific Islander',
                                 Victim.Race == 'U' ~ 'Unknown',
                                 Victim.Race == 'W' ~ 'White',
                                 T ~ ''),
         Victim.County.Resident = if_else(grepl('Resident', Victim.County.Resident),
                                          as.character(Victim.County.Resident),
                                          'UNKNOWN')) %>%
  #removing unnecessary columns
  select(-Report.Type, -Incident.Status.Code, -Location.Code, -Offense.NIBRS, -Weapon.Primary, -Victim.Type, -Victim.Ethnicity, -Mapped.Location)

#Nashville School Attendance ----
Nashville_School_Attendance_Clean <- read.csv('Data/MNPS_Attendance_Data_May20.csv') %>%
  rename(School.ID = SchoolID,
         Active.Enrollment = ActiveEnrollment) %>%
  mutate(School.Name = as.character(SchoolName),
         Chronically.Absent.Count = as.numeric(as.character(ChronicallyAbsentCount))) %>%
  select(School.ID, School.Name, Active.Enrollment, Chronically.Absent.Count)


#Nashville School Behavior ----
Fix_Percents <- function(Col, Total) {
  Less = grepl('<', Col)
  More = grepl('>', Col)
  Percent = grepl('%', Col)
  Col = case_when(More & Percent ~ 0.95,
                  Less & Percent ~ 0.05,
                  Less ~ 10/Total,
                  Percent ~ as.numeric(gsub('%', '', Col))/100,
                  T ~ 0)
  Col = case_when(Less & Percent ~ as.character(ceiling(Total*Col)),
                  More & Percent ~ as.character(floor(Total*Col)),
                  T ~ as.character(round(Total*Col)))
  Col = case_when(Less ~ paste('<', Col),
                  More ~ paste('>', Col),
                  T ~ Col)
  return(Col)
}

Nashville_School_Behavior_Clean <- read.csv('Data/MNPS_Behavior_Data_May20.csv') %>%
  rename(Total.Subgroup.Enrollment = TotalSubgroupEnrollment) %>%
  mutate(School.Name = as.character(SCHOOL),
         Subgroup = if_else(DataValue == 'Y', 
                            as.character(Subgroup),
                            as.character(DataValue)),
         Suspension = as.character(Suspension),
         Suspension = Fix_Percents(Suspension, Total.Subgroup.Enrollment),
         Expulsion = as.character(Expulsion),
         Expulsion = Fix_Percents(Expulsion, Total.Subgroup.Enrollment),
         Remandment = as.character(Remandment),
         Remandment = Fix_Percents(Remandment, Total.Subgroup.Enrollment)) %>%
  select(School.Name, Total.Subgroup.Enrollment:Subgroup, Suspension:Remandment)


#Nashville School Enrollment ----
Nashville_School_Enrollment_Clean <- read.csv('Data/MNPS_Enrollment_Data_Apr20.csv') %>%
  rename(Native.American = American.Indian.or.Alaska.Native,
         Pacific.Islander = Native.Hawaiian.or.Other.Pacific.Islander,
         Black = Black.or.African.American,
         Hispanic = Hispanic.Latino) %>%
  mutate(Native.American = as.character(Native.American),
         Native.American = Fix_Percents(Native.American, Total.Enrollment),
         Asian = as.character(Asian),
         Asian = Fix_Percents(Asian, Total.Enrollment),
         Black = as.character(Black),
         Black = Fix_Percents(Black, Total.Enrollment),
         Hispanic = as.character(Hispanic),
         Hispanic = Fix_Percents(Hispanic, Total.Enrollment),
         Pacific.Islander = as.character(Pacific.Islander),
         Pacific.Islander = Fix_Percents(Pacific.Islander, Total.Enrollment),
         White = as.character(White),
         White = Fix_Percents(White, Total.Enrollment),
         Male = as.character(Male),
         Male = Fix_Percents(Male, Total.Enrollment),
         Female = as.character(Female),
         Female = Fix_Percents(Female, Total.Enrollment),
         Economically.Disadvantaged = as.character(Economically.Disadvantaged),
         Economically.Disadvantaged = Fix_Percents(Economically.Disadvantaged, Total.Enrollment),
         Students.with.Disabilities = as.character(Students.with.Disabilities),
         Students.with.Disabilities = Fix_Percents(Students.with.Disabilities, Total.Enrollment),
         Limited.English.Proficiency = as.character(Limited.English.Proficiency),
         Limited.English.Proficiency = Fix_Percents(Limited.English.Proficiency, Total.Enrollment)) %>%
  select(School.Level:Total.Enrollment, Native.American:Limited.English.Proficiency)


#311 Service Requests ----
Request_311_Clean <- read.csv('Data/Nashville_311_Service_Requests.csv') %>%
  rename(Request.Num = Request..) %>%
  mutate(Time.Opened = as.POSIXct(Date...Time.Opened, format = '%m/%d/%Y %I:%M:%S %p'),
         Time.Closed = as.POSIXct(Date...Time.Closed, format = '%m/%d/%Y %I:%M:%S %p'),
         State.Issue = grepl('T', State.Issue),
         Closed.When.Created = grepl('T', Closed.When.Created),
         Address = as.character(Address),
         City = as.character(City),
         ZIP = as.character(ZIP),
         ZIP = case_when(City == '37214' ~ 37214,
                         ZIP == '0' ~ NA_real_,
                         ZIP == '00000' ~ NA_real_,
                         ZIP == 'TN' ~ NA_real_,
                         ZIP == '' ~ NA_real_,
                         T ~ as.numeric(ZIP)),
         City = toupper(City),
         City = case_when(City == '37214' ~ '',
                          grepl('ANTIOCH', City) ~ 'ANTIOCH',
                          grepl('ASHLAND CITY', City) ~ 'ASHLAND CITY',
                          grepl('BRIGHTON', City) ~ 'BRIGHTON',
                          grepl('FRANKLIN', City) ~ 'FRANKLIN',
                          grepl('^HERM', City) ~ 'HERMITAGE',
                          grepl('^LA', City) ~ 'LA VERGNE',
                          grepl('^MAD', City) ~ 'MADISON',
                          City == 'MASHVILLE TN' ~ 'NASHVILLE',
                          grepl('JULIET$', City) ~ 'MT. JULIET',
                          grepl('^NASH', City) ~ 'NASHVILLE',
                          grepl('OLD HICKORY', City) ~ 'OLD HICKORY',
                          City == 'UNKNOWN' ~ '',
                          T ~ City)) %>%
  select(Request.Num:Additional.Subrequest.Type, Time.Opened, Time.Closed, Request.Origin, 
         Contact.Type, State.Issue:Longitude)


#Nashville Budget ----
Nashville_Budget <- read.csv('Data/Nashville_Budget.csv') %>%
  select(Fund.Description, Department.Description, Business.Unit.Description, 
         Object.Account.Description:FY19.Actual.Expenses) %>%
  pivot_longer(names_to = 'Column', values_to = 'Budget', cols = FY14.Budgeted.Expenses:FY19.Actual.Expenses) %>%
  separate(Column, into = c('Year', 'Budget_Type'), sep = '\\.', extra = 'drop') %>%
  mutate(Year = gsub('FY', '20', Year),
         Year = as.numeric(Year))


#Nashville Government Demographics ----
Nashville_Government_Demographics <- read.csv('Data/Nashville_Gov_Demographics.csv') %>%
  mutate(Ethnic.Code.Description = as.character(Ethnic.Code.Description),
         Ethnic.Code.Description = case_when(grepl('Indian', Ethnic.Code.Description) ~ 'Native American',
                                             grepl('^Black', Ethnic.Code.Description) ~ 'Black',
                                             grepl('^Hispanic', Ethnic.Code.Description) ~ 'Hispanic',
                                             grepl('Hawaiian', Ethnic.Code.Description) ~ 'Pacific Islander',
                                             T ~ Ethnic.Code.Description),
         Data.Started = as.character(Date.Started),
         Date.Started = as.Date(Date.Started, format = '%m/%d/%Y')) %>%
  select(-Pay.Grade...Step, -Class)


#Property Violations ----
Property_Standard_Violations


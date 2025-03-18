##Packages----------------------------------------------------------------------

library(httr)

##Functions---------------------------------------------------------------------

extract_by_id <- function(html, id) {
  pattern <- paste0('(?s)<span[^>]*id="', id, '".*?>(.*?)</span>')
  match <- regmatches(html, regexpr(pattern, html, perl = TRUE))
  return(gsub("<.*?>", "", match))
}

##Path--------------------------------------------------------------------------

data_txt             = "data/Scrapping_CEOS_database/mission_database_20022025.txt"
url_base_missions    = "https://database.eohandbook.com/database/missionsummary.aspx?missionID="
url_base_instruments = "https://database.eohandbook.com/database/instrumentsummary.aspx?instrumentID="

##Variables---------------------------------------------------------------------

vegetation_test          = "../timeline/timeline.aspx?measurementCategoryID=13"

id_mission_launch_date   = "MainContent_lblLaunchDate"
id_mission_name_full     = "MainContent_lblMissionNameFull"
id_mission_name_short    = "MainContent_lblMissionNameShort"
id_mission_agencies      = "MainContent_lblMissionAgencies"
id_mission_orbit         = "MainContent_lblOrbitType"

id_instrument_resolution = "MainContent_lblInstrumentResolutionSummary"
id_instrument_swath      = "MainContent_lblInstrumentSwathSummary"
id_instrument_name_full  = "MainContent_lblInstrumentNameFull"
id_instrument_name_short = "MainContent_lblInstrumentNameShort"
id_instrument_technology = "MainContent_lblInstrumentTechnology"
id_instrument_agencies   = "MainContent_lblInstrumentAgencies"
id_instrument_wb         = "MainContent_lblInstrumentWavebandSummary"

##Script------------------------------------------------------------------------

#Mission ID
lines_txt                = readLines(data_txt)
links                    = regmatches(lines_txt, regexpr('../database/missionsummary.aspx\\?missionID=[0-9]{1,4}', lines_txt, perl=TRUE))
MISSION_ID               = sort(as.integer(substr(links,43,nchar(links))))

#Columns table
mission_id                = c()
mission_launch_date       = c()
mission_name_full         = c()
mission_name_short        = c()
mission_agencies          = c()
mission_orbit             = c()

instrument_id             = c()
instrument_resolution     = c()
instrument_swath          = c()
instrument_name_full      = c()
instrument_name_short     = c()
instrument_technology     = c()
instrument_agencies       = c()
instrument_wb             = c()

instrument_resolution_int = c()

for(ID in MISSION_ID){
  print(ID)
  url          = paste(url_base_missions,ID,sep = "")
  response     = GET(url)
  page_content = content(response, as = "text", encoding = "UTF-8")
  if(grepl(vegetation_test, page_content, fixed = TRUE)){
    #No and id instruments
    page_content_lines           = strsplit(page_content, "\n")[[1]]
    ind_vegetation               = which(grepl(vegetation_test,page_content_lines,fixed=T))
    line_vegetation              = page_content_lines[ind_vegetation]
    pattern                      = "href=\"[^\"]*instrumentID=[0-9]+\""
    links_instruments            = regmatches(line_vegetation, gregexpr(pattern, line_vegetation))[[1]]
    
    instrument_id                = append(instrument_id,as.integer(substr(links_instruments,55,nchar(links_instruments)-1)))
    no_instrument                = length(as.integer(substr(links_instruments,55,nchar(links_instruments)-1)))
    
    #Fill columns
    mission_id            = append(mission_id            ,ID)
    mission_id            = append(mission_id            ,rep("",no_instrument-1))
    mission_launch_date   = append(mission_launch_date   ,extract_by_id(page_content, id_mission_launch_date))
    mission_launch_date   = append(mission_launch_date   ,rep(""  ,no_instrument-1))
    mission_name_full     = append(mission_name_full     ,extract_by_id(page_content, id_mission_name_full))
    mission_name_full     = append(mission_name_full     ,rep(""  ,no_instrument-1))
    mission_name_short    = append(mission_name_short    ,extract_by_id(page_content, id_mission_name_short))
    mission_name_short    = append(mission_name_short    ,rep(""  ,no_instrument-1))
    mission_agencies      = append(mission_agencies      ,extract_by_id(page_content, id_mission_agencies))
    mission_agencies      = append(mission_agencies      ,rep(""  ,no_instrument-1))
    mission_orbit         = append(mission_orbit         ,extract_by_id(page_content, id_mission_orbit))
    mission_orbit         = append(mission_orbit         ,rep(""  ,no_instrument-1))
  }
}

for(ID in instrument_id){
  print(ID)
  url          = paste(url_base_instruments,ID,sep = "")
  response     = GET(url)
  page_content = content(response, as = "text", encoding = "UTF-8")
  
  instrument_resolution = append(instrument_resolution,extract_by_id(page_content, id_instrument_resolution))
  instrument_swath      = append(instrument_swath     ,extract_by_id(page_content, id_instrument_swath))
  instrument_name_full  = append(instrument_name_full ,extract_by_id(page_content, id_instrument_name_full))
  instrument_name_short = append(instrument_name_short,extract_by_id(page_content, id_instrument_name_short))
  instrument_technology = append(instrument_technology,extract_by_id(page_content, id_instrument_technology))
  instrument_agencies   = append(instrument_agencies  ,extract_by_id(page_content, id_instrument_agencies))
  instrument_wb         = append(instrument_wb        ,extract_by_id(page_content, id_instrument_wb))
}

mission_name_short    = substr(mission_name_short,4,nchar(mission_name_short))
instrument_name_short = substr(instrument_name_short,4,nchar(instrument_name_short))

for(i in seq(length(instrument_resolution))){
  r_str                     = instrument_resolution[i]
  wb_str                    = instrument_wb[i]
  r_int                     = as.numeric(readline(prompt = cat(wb_str,"\n",r_str," :",sep = "")))
  instrument_resolution_int = append(instrument_resolution_int,r_int)
}

table_output = data.frame("Mission CEOS ID"        = mission_id
                          ,"Short Name"            = mission_name_short
                          ,"Full Name"             = mission_name_full
                          ,"Mission agencies"      = mission_agencies
                          ,"Launch year"           = mission_launch_date
                          ,"Orbit"                 = mission_orbit
                          ,"Instrument CEOS ID"    = instrument_id
                          ,"Resolution"            = instrument_resolution
                          ,"Swath"                 = instrument_swath
                          ,"Short name"            = instrument_name_short
                          ,"Full name"             = instrument_name_full
                          ,"Technology"            = instrument_technology
                          ,"Instrument agencies"   = instrument_agencies)

write.table(table_output,file = "data/Scrapping_CEOS_database/satellites.csv",sep = ";",dec = ",",row.names = F,fileEncoding = "UTF-8")


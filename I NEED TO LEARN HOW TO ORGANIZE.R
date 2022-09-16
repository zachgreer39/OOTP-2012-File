library(tidyverse);

##point to OOTP IMPORT/EXPORT DIRECTORY
setwd("~/Github/OOTP-2012-File");

ootpRead=function(file){
  data.table::fread(paste0("csv/", file))
};

leagues=function(){
  
  ootpRead("leagues.csv") %>% 
    left_join(cbind(league_id=c(100:116, 201),
                    levelDESC=c("MLB", "AAA", "AAA", "AA", "AA", "AA", 
                                "A", "A", "A+", "A+", "A+", "A-", "A-", 
                                "Rk", "Rk", "HiRk", "HiRk", "FRk")) %>%
                as.data.frame() %>%
                mutate(league_id=as.numeric(league_id)), 
              by="league_id") %>%
    add_row(name="Free Agent", abbr="FA", levelDESC="FA", league_level=0,
            season_year=lubridate::year(ootpRead("leagues.csv") %>% 
                                          pull(current_date) %>% unique()), 
            current_date=ootpRead("leagues.csv") %>% 
              pull(current_date) %>% unique(), league_id=0) %>%
    transmute(season_year, 
              leagueID=league_id, 
              leagueDESC=factor(abbr, 
                                levels=c("MLB", "PCL", "IL", "TL", 
                                         "SOUL", "EL", "FLOR", "CALL", 
                                         "CARL", "MIDW", "SALL", "NORW", 
                                         "NYPL", "PION", "APPY", "GULF", 
                                         "ARIZ", "DSL", "FA")), 
              leagueNAME=factor(name, 
                                levels=c("Major League Baseball", 
                                         "Pacific Coast League", 
                                         "International League", 
                                         "Texas League", 
                                         "Southern League", 
                                         "Eastern League", 
                                         "Florida State League", 
                                         "California League", 
                                         "Carolina League", 
                                         "Midwest League", 
                                         "South Atlantic League", 
                                         "Northwest League", 
                                         "New York-Pennsylvania League", 
                                         "Pioneer League", 
                                         "Appalachian League", 
                                         "Gulf Coast League", 
                                         "Arizona League", 
                                         "Dominican Summer League", 
                                         "Free Agent")), 
              levelID=league_level, 
              levelDESC=factor(levelDESC, 
                               levels=c("MLB", "AAA", "AA", "A+", "A", 
                                        "A-", "HiRk", "Rk", "FRk", "FA")),
              roster_limit=rules_active_roster_limit, 
              rosterSEC_limit=rules_secondary_roster_limit, 
              rosterEXP_limit=rules_expanded_roster_limit,
              service_daysYEAR=rules_min_service_days, 
              service_arbMIN=rules_salary_arbitration_minimum_years, 
              service_faMIN=rules_fa_minimum_years, 
              season_games=rules_schedule_games_per_team,
              start_date=lubridate::ymd(start_date),
              file_date=lubridate::ymd(current_date)) %>%
    arrange(levelDESC, leagueDESC)
  
};
df_leagues=leagues(); 
rm(leagues);

nations=function(){
  
  ootpRead("nations.csv") %>% 
    left_join(ootpRead("cities.csv") %>% 
                transmute(capital_id=city_id, 
                          capitalNAME=name), 
              by=c("capital_id")) %>%
    transmute(nationID=nation_id, 
              nationDESC=factor(abbreviation),
              nationNAME=factor(short_name), 
              nationPOP=population, 
              capitalID=capital_id, 
              capitalNAME, 
              baseball_quality, 
              season_year=df_leagues %>% filter(leagueID==100) %>% 
                pull(season_year) %>% unique(),
              file_date=df_leagues %>% pull(file_date) %>% unique())
  
};
cities=function(){
  
  ootpRead("cities.csv") %>% 
    left_join(nations() %>% 
                select(nationID, nationDESC), 
              by=c("nation_id"="nationID")) %>%
    left_join(ootpRead("states.csv") %>% 
                transmute(state_id, 
                          nation_id, 
                          stateDESC=factor(abbreviation), 
                          stateNAME=factor(name)), 
              by=c("state_id", "nation_id")) %>% 
    transmute(cityID=city_id, 
              cityNAME=ifelse(nation_id==206, 
                              paste0(name, ", ", stateDESC), 
                              paste0(name, ", ", nationDESC)), 
              stateID=state_id, 
              stateDESC, 
              stateNAME,
              nationID=nation_id, 
              nationDESC, 
              lat=latitude, 
              lon=longitude, 
              cityPOP=population, 
              season_year=df_leagues %>% filter(leagueID==100) %>% 
                pull(season_year) %>% unique(),
              file_date=df_leagues %>% pull(file_date) %>% unique())
  
};
df_nations=nations();
df_cities=cities();
rm(nations, cities);

divisions=function(){
  
  ootpRead("divisions.csv") %>%
    left_join(df_leagues %>% 
                select(leagueID, leagueDESC, levelID,
                       levelDESC, season_year, file_date), 
              by=c("league_id"="leagueID")) %>% 
    left_join(ootpRead("sub_leagues.csv") %>% 
                transmute(league_id, 
                          sub_league_id, 
                          subleagueDESC=abbr), 
              by=c("league_id", "sub_league_id")) %>% 
    transmute(leagueID=league_id, 
              leagueDESC, 
              levelID, 
              subleagueID=sub_league_id, 
              subleagueDESC, 
              divisionID=division_id, 
              divisionDESC=paste(str_trim(subleagueDESC), str_trim(name)), 
              season_year, 
              file_date)
  
};
df_divisions=divisions();
rm(divisions);

parks=function(){
  
  ootpRead("parks.csv") %>% 
    transmute(parkID=park_id, 
              parkNAME=name, 
              turf, 
              type, 
              capacity, 
              season_year=df_leagues %>% filter(leagueID==100) %>% 
                pull(season_year) %>% unique(),
              file_date=df_leagues %>% pull(file_date) %>% unique())
  
};
df_parks=parks();
rm(parks)

users=function(){
  
  ootpRead("human_managers.csv") %>% 
    left_join(df_leagues %>% 
                select(leagueID, levelDESC, season_year, file_date), 
              by=c("league_id"="leagueID")) %>% 
    transmute(userID=human_manager_id, 
              userNAME=paste(first_name, last_name),
              leagueID=league_id,
              levelDESC=replace_na(levelDESC,"FA"),
              teamID=team_id, 
              orgID=organization_id,
              is_commish, 
              season_year,
              file_date)
  
};
df_users=users();
rm(users);

teams=function(){
  
  orgs=function(){
    
    ootpRead("teams.csv") %>% 
      filter(parent_team_id==0 & allstar_team==0) %>% 
      transmute(orgID=ifelse(parent_team_id==0, 
                             team_id, 
                             parent_team_id), 
                orgDESC=abbr,
                user_controlled=human_team, 
                userID=human_id)
    
  }
  
  ootpRead("teams.csv") %>%
    filter(allstar_team==0) %>%
    mutate(orgID=ifelse(parent_team_id==0, 
                        team_id, 
                        parent_team_id)) %>%
    left_join(orgs(), 
              by="orgID") %>%
    add_row(team_id=0, orgID=0, orgDESC="-FA", league_id=0, 
            level=0, division_id=0, sub_league_id=0, abbr="FA", 
            name="Free", nickname="Agent") %>%
    left_join(df_leagues %>%
                select(leagueID, leagueDESC, levelDESC), 
              by=c("league_id"="leagueID")) %>%
    left_join(df_divisions %>% 
                select(-c("leagueDESC")), 
              by=c("league_id"="leagueID", 
                   "level"="levelID", 
                   "sub_league_id"="subleagueID", 
                   "division_id"="divisionID")) %>%
    left_join(df_cities %>%
                select(cityID, cityNAME), 
              by=c("city_id"="cityID")) %>% 
    left_join(df_parks %>% 
                select(parkID, parkNAME), 
              by=c("park_id"="parkID")) %>%
    transmute(teamID=team_id, 
              teamDESC=factor(abbr), 
              teamNAME=factor(paste(name, nickname)), 
              leagueID=league_id, 
              leagueDESC,
              levelID=level,
              levelDESC,
              subleagueID=sub_league_id, 
              subleagueDESC, 
              divisionID=division_id, 
              divisionDESC,
              orgID,
              orgDESC,
              cityID=city_id, 
              cityNAME,
              parkID=park_id, 
              parkNAME,
              user_controlled, 
              userID,
              season_year,
              file_date=df_leagues %>% pull(file_date) %>% unique())
  
};
df_teams=teams();
rm(teams);

players=function(){
  
  ootpRead("players.csv") %>% 
    filter(retired==0 & hidden==0) %>% 
    mutate(role=ifelse(role==0, position, role)) %>%
    left_join(as.data.frame(cbind(role=1:13, 
                                  posDESC=c("P", "C", "1B", "2B", "3B", 
                                            "SS", "LF", "CF", "RF", 
                                            "DH", "SP", "RP", "CL"))) %>% 
                mutate(role=as.numeric(role), 
                       posDESC=factor(posDESC, 
                                      levels=c("P", "SP", "CL", "RP", "C", 
                                               "1B", "2B", "3B", "SS", 
                                               "LF", "CF", "RF", "DH"))), 
              by="role") %>%
    left_join(df_leagues %>% 
                select(leagueID, leagueDESC, levelID, levelDESC, 
                       season_year, file_date), 
              by=c("league_id"="leagueID")) %>%
    left_join(df_cities %>% 
                transmute(city_of_birth_id=cityID, 
                          birthCity=cityNAME), 
              by="city_of_birth_id") %>%
    left_join(df_teams %>%
                select(teamID, teamDESC, orgID, orgDESC), 
              by=c("team_id"="teamID")) %>%
    left_join(df_nations %>% 
                transmute(nation_id=nationID, 
                          homeNationDESC=nationDESC), 
              by="nation_id") %>%
    transmute(playerID=player_id, 
              playerNAME=paste(first_name, last_name), 
              posID=position, 
              posDESC, 
              leagueID=league_id,
              leagueDESC,
              levelID, 
              levelDESC,
              orgID=organization_id,
              orgDESC,
              teamID=team_id, 
              teamDESC,
              bats=ifelse(bats==1, 
                          "R", 
                          ifelse(bats==2, 
                                 "L", 
                                 ifelse(bats==3, 
                                        "S", 
                                        NA))), 
              bats=factor(bats, 
                          levels=c("R", "L", "S")),
              throws=ifelse(throws==1, 
                            "R", 
                            ifelse(throws==2, 
                                   "L", 
                                   NA)),
              throws=factor(throws, 
                            levels=c("R", "L")),
              date_of_birth=lubridate::ymd(date_of_birth),
              age=floor(as.numeric(difftime(
                lubridate::ymd(paste0(season_year, "06-30")),
                date_of_birth))/365.25),
              birthCityID=city_of_birth_id, 
              birthCity,
              homeNationID=nation_id, 
              homeNationDESC,
              season_year=ifelse(is.na(season_year), 
                                 unique(df_leagues %>% 
                                          filter(leagueID==100) %>% 
                                          pull(season_year)), 
                                 season_year),
              file_date=df_leagues %>% pull(file_date) %>% unique())
  
};
df_players=players();
rm(players);



batStats=function(){
  
  ootpRead("players_career_batting_stats.csv") %>% 
    inner_join(df_players %>%
                 select(playerID, playerNAME, posDESC, bats, age, season_year), 
               by=c("player_id"="playerID")) %>%
    left_join(df_teams %>% 
                select(teamID, teamDESC, leagueDESC, levelDESC), 
              by=c("team_id"="teamID")) %>% 
    transmute(playerID=player_id, 
              playerNAME,
              year,
              age=age-(season_year-year),
              teamID=team_id,
              teamDESC,
              leagueID=league_id, 
              leagueDESC,
              levelDESC,
              split=ifelse(split_id==1, 
                           "FS", 
                           ifelse(split_id==2, 
                                  "vL", 
                                  ifelse(split_id==3, 
                                         "vR", NA))),
              split=factor(split, 
                           levels=c("FS", "vR", "vL")),
              bats, 
              g, 
              gs, 
              pa, 
              ab, 
              avg=round(h/ab, 3), 
              obp=round((h+bb+hp)/(ab+bb+hp+sf), 3),
              slg=round((h-(d+t+hr) + 2*d + 3*t + 4*hr)/ab, 3),
              ops=obp+slg,
              h, 
              db=d, 
              tr=t, 
              hr, 
              r,
              rbi, 
              k, 
              bb,
              ibb, 
              hbp=hp, 
              sf, 
              sh, 
              gdp, 
              sb, 
              cs,
              pitchesPA=round(pitches_seen/pa, 1),
              war=round(war, 1)) %>% 
    arrange(playerID, split, desc(year), leagueDESC)
  
};
df_batStats=batStats();
rm(batStats);

batCareer=function(){
  
  df_batStats %>%
    mutate(pitches=round(pitchesPA*pa, 0)) %>%
    group_by(playerID, playerNAME, levelDESC, split, bats) %>%
    summarise(g=sum(g), 
              gs=sum(gs), 
              pa=sum(pa), 
              ab=sum(ab), 
              avg=round(sum(h)/sum(ab), 3), 
              obp=round((sum(h)+sum(bb)+sum(hbp))/(sum(ab)+sum(bb)+sum(hbp)+sum(sf)), 3), 
              slg=round(((sum(h) - (sum(db) + sum(tr) + sum(hr))) + 2*sum(db) + 3*sum(tr) + 4*sum(hr))/(sum(ab)), 3),
              ops=obp+slg, 
              h=sum(h), 
              db=sum(db), 
              tr=sum(tr), 
              hr=sum(hr), 
              r=sum(r), 
              rbi=sum(rbi), 
              k=sum(k), 
              bb=sum(bb), 
              ibb=sum(ibb), 
              hbp=sum(hbp), 
              sf=sum(sf), 
              sh=sum(sh), 
              gdp=sum(gdp), 
              sb=sum(sb), 
              cs=sum(cs), 
              pitchesPA=round(sum(pitches)/sum(pa), 1), 
              war=sum(war)) %>%
    arrange(playerID, levelDESC, split)
  
};
df_batCareer=batCareer();
rm(batCareer);
batPrev3yr=function(){
  
  df_batStats %>%
    filter(year %in% seq(unique(df_leagues %>% filter(leagueID==100) %>% 
                                  pull(season_year))-3, 
                         unique(df_leagues %>% filter(leagueID==100) %>% 
                                  pull(season_year))-1)) %>%
    mutate(pitches=round(pitchesPA*pa, 0)) %>%
    group_by(playerID, playerNAME, levelDESC, split, bats) %>%
    summarise(g=sum(g), 
              gs=sum(gs), 
              pa=sum(pa), 
              ab=sum(ab), 
              avg=round(sum(h)/sum(ab), 3), 
              obp=round((sum(h)+sum(bb)+sum(hbp))/(sum(ab)+sum(bb)+sum(hbp)+sum(sf)), 3), 
              slg=round(((sum(h) - (sum(db) + sum(tr) + sum(hr))) + 2*sum(db) + 3*sum(tr) + 4*sum(hr))/(sum(ab)), 3),
              ops=obp+slg, 
              h=sum(h), 
              db=sum(db), 
              tr=sum(tr), 
              hr=sum(hr), 
              r=sum(r), 
              rbi=sum(rbi), 
              k=sum(k), 
              bb=sum(bb), 
              ibb=sum(ibb), 
              hbp=sum(hbp), 
              sf=sum(sf), 
              sh=sum(sh), 
              gdp=sum(gdp), 
              sb=sum(sb), 
              cs=sum(cs), 
              pitchesPA=round(sum(pitches)/sum(pa), 1), 
              war=sum(war)) %>%
    arrange(playerID, levelDESC, split)
  
};
batPrev5yr=function(){
  
  df_batStats %>%
    filter(year %in% seq(unique(df_leagues %>% filter(leagueID==100) %>% 
                                  pull(season_year))-5, 
                         unique(df_leagues %>% filter(leagueID==100) %>% 
                                  pull(season_year))-1)) %>%
    mutate(pitches=round(pitchesPA*pa, 0)) %>%
    group_by(playerID, playerNAME, levelDESC, split, bats) %>%
    summarise(g=sum(g), 
              gs=sum(gs), 
              pa=sum(pa), 
              ab=sum(ab), 
              avg=round(sum(h)/sum(ab), 3), 
              obp=round((sum(h)+sum(bb)+sum(hbp))/(sum(ab)+sum(bb)+sum(hbp)+sum(sf)), 3), 
              slg=round(((sum(h) - (sum(db) + sum(tr) + sum(hr))) + 2*sum(db) + 3*sum(tr) + 4*sum(hr))/(sum(ab)), 3),
              ops=obp+slg, 
              h=sum(h), 
              db=sum(db), 
              tr=sum(tr), 
              hr=sum(hr), 
              r=sum(r), 
              rbi=sum(rbi), 
              k=sum(k), 
              bb=sum(bb), 
              ibb=sum(ibb), 
              hbp=sum(hbp), 
              sf=sum(sf), 
              sh=sum(sh), 
              gdp=sum(gdp), 
              sb=sum(sb), 
              cs=sum(cs), 
              pitchesPA=round(sum(pitches)/sum(pa), 1), 
              war=sum(war)) %>%
    arrange(playerID, levelDESC, split)
  
};



fieldStats=function(){
  
  ootpRead("players_career_fielding_stats.csv") %>% 
    inner_join(df_players %>%
                 select(playerID, playerNAME, throws, age, season_year), 
               by=c("player_id"="playerID")) %>%
    left_join(df_teams %>%
                select(teamID, teamDESC, leagueID, leagueDESC, levelDESC), 
              by=c("team_id"="teamID", "league_id"="leagueID")) %>% 
    left_join(as.data.frame(cbind(position=1:9, 
                                  posCODE=c("P", "C", "1B", "2B", "3B", 
                                            "SS", "LF", "CF", "RF"))) %>% 
                mutate(position=as.numeric(position), 
                       posCODE=factor(posCODE, 
                                      levels=c("P", "C", "1B", "2B", "3B", 
                                               "SS", "LF", "CF", "RF"))), 
              by="position") %>%
    transmute(playerID=player_id, 
              playerNAME,
              year,
              age=age-(season_year-year),
              throws, 
              teamID=team_id,
              teamDESC,
              leagueID=league_id, 
              leagueDESC,
              levelDESC,
              posCODE, 
              g, 
              gs, 
              inn=ip, 
              tc, 
              po, 
              a, 
              e, 
              dp, 
              fldPERC=round((tc-e)/tc, 3), 
              zr=round(zr, 1),
              pb, 
              sba, 
              csPERC=round(rto/sba, 3), 
              csPERC=ifelse(is.nan(csPERC), 0, csPERC)) %>%
    arrange(playerID, desc(year), leagueDESC, posCODE)
  
};
df_fieldStats=fieldStats();
rm(fieldStats);

fieldCareer=function(){
  
  df_fieldStats %>%
    mutate(cs=round(csPERC*sba, 0)) %>%
    group_by(playerID, playerNAME, throws, levelDESC, posCODE) %>%
    summarise(g=sum(g), 
              gs=sum(gs), 
              inn=sum(inn), 
              tc=sum(tc), 
              po=sum(po), 
              a=sum(a), 
              e=sum(e), 
              dp=sum(dp), 
              fldPERC=round((sum(tc)-sum(e))/sum(tc), 3),
              fldPERC=ifelse(is.nan(fldPERC), 
                             0, 
                             fldPERC),
              zr=sum(zr), 
              pb=sum(pb), 
              sba=sum(sba), 
              csPERC=round(sum(cs)/sum(sba), 2), 
              csPERC=ifelse(is.nan(csPERC), 
                            0, 
                            csPERC)) %>%
    arrange(playerID, levelDESC, posCODE)
  
}
df_fieldCareer=fieldCareer();
rm(fieldCareer);
fieldPrev3yr=function(){
  
  df_fieldStats %>%
    filter(year %in% seq(unique(df_leagues %>% filter(leagueID==100) %>% 
                                  pull(season_year))-3, 
                         unique(df_leagues %>% filter(leagueID==100) %>% 
                                  pull(season_year))-1)) %>%
    mutate(cs=round(csPERC*sba, 0)) %>%
    group_by(playerID, playerNAME, throws, levelDESC, posCODE) %>%
    summarise(g=sum(g), 
              gs=sum(gs), 
              inn=sum(inn), 
              tc=sum(tc), 
              po=sum(po), 
              a=sum(a), 
              e=sum(e), 
              dp=sum(dp), 
              fldPERC=round((sum(tc)-sum(e))/sum(tc), 3),
              fldPERC=ifelse(is.nan(fldPERC), 
                             0, 
                             fldPERC),
              zr=sum(zr), 
              pb=sum(pb), 
              sba=sum(sba), 
              csPERC=round(sum(cs)/sum(sba), 2), 
              csPERC=ifelse(is.nan(csPERC), 
                            0, 
                            csPERC)) %>%
    arrange(playerID, levelDESC, posCODE)
  
};
fieldPrev5yr=function(){
  
  df_fieldStats %>%
    filter(year %in% seq(unique(df_leagues %>% filter(leagueID==100) %>% 
                                  pull(season_year))-5, 
                         unique(df_leagues %>% filter(leagueID==100) %>% 
                                  pull(season_year))-1)) %>%
    mutate(cs=round(csPERC*sba, 0)) %>%
    group_by(playerID, playerNAME, throws, levelDESC, posCODE) %>%
    summarise(g=sum(g), 
              gs=sum(gs), 
              inn=sum(inn), 
              tc=sum(tc), 
              po=sum(po), 
              a=sum(a), 
              e=sum(e), 
              dp=sum(dp), 
              fldPERC=round((sum(tc)-sum(e))/sum(tc), 3),
              fldPERC=ifelse(is.nan(fldPERC), 
                             0, 
                             fldPERC),
              zr=sum(zr), 
              pb=sum(pb), 
              sba=sum(sba), 
              csPERC=round(sum(cs)/sum(sba), 2), 
              csPERC=ifelse(is.nan(csPERC), 
                            0, 
                            csPERC)) %>%
    arrange(playerID, levelDESC, posCODE)
  
};
fieldSummary=function(){
  
  df_fieldStats %>%
    rbind(df_fieldCareer %>%
            mutate(year=1), 
          fieldPrev3yr() %>%
            mutate(year=3), 
          fill=TRUE) %>%
    arrange(playerID, levelDESC, desc(year), posCODE) %>%
    mutate(teamDESC=ifelse(year==3, 
                           "*3 Yr", 
                           ifelse(year==1, 
                                  "*Career", 
                                  as.character(teamDESC))), 
           teamDESC=factor(teamDESC),
           leagueDESC=ifelse(year==3, 
                             "*3 Yr", 
                             ifelse(year==1, 
                                    "*Career", 
                                    as.character(leagueDESC))),
           leagueDESC=factor(leagueDESC, 
                             levels=c("MLB", "PCL", "IL", "TL", 
                                      "SOUL", "EL", "FLOR", "CALL", 
                                      "CARL", "MIDW", "SALL", "NORW", 
                                      "NYPL", "PION", "APPY", "GULF", 
                                      "ARIZ", "DSL", "FA", 
                                      "*3 Yr", "*Career")),
           year=ifelse(year %in% 1:3, NA, year))
  
};











injuries=function(){
  
  ootpCSV("players.csv") %>% 
    filter(retired==0 & hidden==0) %>%
    filter(injury_is_injured > 0) %>%
    transmute(
      player_id,
      injury_is_injured,
      injury_dl_left, 
      injury_dl_playoff_round, 
      injury_id, 
      injury_left, 
      injury_id2, 
      injury_dtd_injury2, 
      injury_left2, 
      injury_dtd_injury, 
      injury_dtd_effect=dtd_injury_effect, 
      injury_dtd_effect_hit=dtd_injury_effect_hit, 
      injury_dtd_effect_throw=dtd_injury_effect_throw, 
      injury_dtd_effect_run=dtd_injury_effect_run, 
      injury_dtd_injury2, 
      injury_dtd_effect2=dtd_injury_effect2, 
      injury_dtd_effect_hit2=dtd_injury_effect_hit2, 
      injury_dtd_effect_throw2=dtd_injury_effect_throw2, 
      injury_dtd_effect_run2=dtd_injury_effect_run2, 
      injury_career_ending
      file_date)
  
};
ratings=function(){
  
  rtg_convert=function(x){
    
    ifelse(x>=192, 
           80, 
           ifelse(x>=175, 
                  75, 
                  ifelse(x>=159, 
                         70, 
                         ifelse(x>=142, 
                                65, 
                                ifelse(x>=125, 
                                       60, 
                                       ifelse(x>=109, 
                                              55, 
                                              ifelse(x>=92, 
                                                     50, 
                                                     ifelse(x>=75, 
                                                            45, 
                                                            ifelse(x>=59, 
                                                                   40, 
                                                                   ifelse(x>=42, 
                                                                          35, 
                                                                          ifelse(x>=25, 
                                                                                 30, 
                                                                                 ifelse(x>=9, 
                                                                                        25, 
                                                                                        ifelse(x>=1, 
                                                                                               20, 
                                                                                               0)
                                                                                 )
                                                                          )
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
                                )
                         )
                  )
           )
    )
    
  };
  
  ootpCSV("players_scouted_ratings.csv") %>% 
    left_join(ootpCSV("players.csv") %>%
                transmute(player_id,
                          actualAGE=round(as.numeric(difftime(
                            file_date, date_of_birth))/365.25, 2),
                          season_year=leagues() %>% filter(leagueID==100) %>% pull(season_year),
                          height=round(height*0.393701), 
                          weight,
                          injury_prone_ovr=rtg_convert(prone_overall), 
                          injury_prone_leg=rtg_convert(prone_leg), 
                          injury_prone_back=rtg_convert(prone_back), 
                          injury_prone_arm=rtg_convert(prone_arm), 
                          person_type, 
                          personality_greed=rtg_convert(personality_greed), 
                          personality_loyalty=rtg_convert(personality_loyalty), 
                          personality_play_for_winner=rtg_convert(personality_play_for_winner), 
                          personality_work_ethic=rtg_convert(personality_work_ethic), 
                          personality_intelligence=rtg_convert(personality_intelligence), 
                          personality_leader=rtg_convert(personality_leader)), 
              by="player_id") %>%
    transmute(
      playerID=player_id,
      actualAGE,
      scouting_coach_id, 
      scouting_accuracy, 
      overall=rtg_convert(overall_rating), 
      potential=rtg_convert(talent_rating),
      height, 
      weight,
      contact_overall=rtg_convert(batting_ratings_overall_contact), 
      contact_potential=rtg_convert(batting_ratings_talent_contact), 
      contact_left=rtg_convert(batting_ratings_vsl_contact), 
      contact_right=rtg_convert(batting_ratings_vsr_contact), 
      power_overall=rtg_convert(batting_ratings_overall_power), 
      power_potential=rtg_convert(batting_ratings_talent_power), 
      power_left=rtg_convert(batting_ratings_vsl_power), 
      power_right=rtg_convert(batting_ratings_vsr_power), 
      gap_overall=rtg_convert(batting_ratings_overall_gap), 
      gap_potential=rtg_convert(batting_ratings_talent_gap), 
      gap_left=rtg_convert(batting_ratings_vsl_gap), 
      gap_right=rtg_convert(batting_ratings_vsr_gap), 
      eye_overall=rtg_convert(batting_ratings_overall_eye), 
      eye_potential=rtg_convert(batting_ratings_talent_eye), 
      eye_left=rtg_convert(batting_ratings_vsl_eye), 
      eye_right=rtg_convert(batting_ratings_vsr_eye), 
      avoidK_overall=rtg_convert(batting_ratings_overall_strikeouts), 
      avoidK_potential=rtg_convert(batting_ratings_talent_strikeouts), 
      avoidK_left=rtg_convert(batting_ratings_vsl_strikeouts), 
      avoidK_right=rtg_convert(batting_ratings_vsr_strikeouts), 
      bunt_sac=rtg_convert(batting_ratings_misc_bunt), 
      bunt_hit=rtg_convert(batting_ratings_misc_bunt_for_hit), 
      gb_hitter=batting_ratings_misc_gb_hitter_type, 
      fb_hitter=batting_ratings_misc_fb_hitter_type, 
      speed=rtg_convert(running_ratings_speed), 
      steal_ability=rtg_convert(running_ratings_stealing), 
      baserunning=rtg_convert(running_ratings_baserunning),
      if_range=rtg_convert(fielding_ratings_infield_range), 
      if_arm=rtg_convert(fielding_ratings_infield_arm), 
      if_error=rtg_convert(fielding_ratings_infield_error), 
      if_tdp=rtg_convert(fielding_ratings_turn_doubleplay), 
      of_range=rtg_convert(fielding_ratings_outfield_range), 
      of_arm=rtg_convert(fielding_ratings_outfield_arm), 
      of_error=rtg_convert(fielding_ratings_outfield_error), 
      c_ability=rtg_convert(fielding_ratings_catcher_ability), 
      c_arm=rtg_convert(fielding_ratings_catcher_arm), 
      stamina=rtg_convert(pitching_ratings_misc_stamina), 
      velocity=pitching_ratings_misc_velocity, 
      armslot=pitching_ratings_misc_arm_slot, 
      ground_fly=pitching_ratings_misc_ground_fly, 
      hold_runners=rtg_convert(pitching_ratings_misc_hold),
      stuff_overall=rtg_convert(pitching_ratings_overall_stuff), 
      stuff_potential=rtg_convert(pitching_ratings_talent_stuff), 
      stuff_left=rtg_convert(pitching_ratings_vsl_stuff), 
      stuff_right=rtg_convert(pitching_ratings_vsr_stuff), 
      control_overall=rtg_convert(pitching_ratings_overall_control), 
      control_potential=rtg_convert(pitching_ratings_talent_control), 
      control_left=rtg_convert(pitching_ratings_vsl_control), 
      control_right=rtg_convert(pitching_ratings_vsr_control), 
      movement_overall=rtg_convert(pitching_ratings_overall_movement), 
      movement_potential=rtg_convert(pitching_ratings_talent_movement), 
      movement_left=rtg_convert(pitching_ratings_vsl_movement), 
      movement_right=rtg_convert(pitching_ratings_vsr_movement), 
      fastball=ifelse(pitching_ratings_pitches_fastball>0, 1, 0), 
      fastball_rating=rtg_convert(pitching_ratings_pitches_fastball), 
      fastball_potential=rtg_convert(pitching_ratings_pitches_talent_fastball),
      splitter=ifelse(pitching_ratings_pitches_splitter>0, 1, 0), 
      splitter_rating=rtg_convert(pitching_ratings_pitches_splitter), 
      splitter_potential=rtg_convert(pitching_ratings_pitches_talent_splitter), 
      sinker=ifelse(pitching_ratings_pitches_sinker>0, 1, 0), 
      sinker_rating=rtg_convert(pitching_ratings_pitches_sinker), 
      sinker_potential=rtg_convert(pitching_ratings_pitches_talent_sinker), 
      cutter=ifelse(pitching_ratings_pitches_cutter>0, 1, 0), 
      cutter_rating=rtg_convert(pitching_ratings_pitches_cutter), 
      cutter_potential=rtg_convert(pitching_ratings_pitches_talent_cutter),
      changeup=ifelse(pitching_ratings_pitches_changeup>0, 1, 0), 
      changeup_rating=rtg_convert(pitching_ratings_pitches_changeup), 
      changeup_potential=rtg_convert(pitching_ratings_pitches_talent_changeup), 
      circlechange=ifelse(pitching_ratings_pitches_circlechange>0, 1, 0), 
      circlechange_rating=rtg_convert(pitching_ratings_pitches_circlechange), 
      circlechange_potential=rtg_convert(pitching_ratings_pitches_talent_circlechange),
      knuckleball=ifelse(pitching_ratings_pitches_knuckleball>0, 1, 0), 
      knuckleball_rating=rtg_convert(pitching_ratings_pitches_knuckleball), 
      knuckleball_potential=rtg_convert(pitching_ratings_pitches_talent_knuckleball), 
      slider=ifelse(pitching_ratings_pitches_slider>0, 1, 0), 
      slider_rating=rtg_convert(pitching_ratings_pitches_slider), 
      slider_potential=rtg_convert(pitching_ratings_pitches_talent_slider), 
      curveball=ifelse(pitching_ratings_pitches_curveball>0, 1, 0), 
      curveball_rating=rtg_convert(pitching_ratings_pitches_curveball), 
      curveball_potential=rtg_convert(pitching_ratings_pitches_talent_curveball), 
      knucklecurve=ifelse(pitching_ratings_pitches_knucklecurve>0, 1, 0), 
      knucklecurve_rating=rtg_convert(pitching_ratings_pitches_knucklecurve), 
      knucklecurve_potential=rtg_convert(pitching_ratings_pitches_talent_knucklecurve), 
      screwball=ifelse(pitching_ratings_pitches_screwball>0, 1, 0), 
      screwball_rating=rtg_convert(pitching_ratings_pitches_screwball), 
      screwball_potential=rtg_convert(pitching_ratings_pitches_talent_screwball), 
      forkball=ifelse(pitching_ratings_pitches_forkball>0, 1, 0), 
      forkball_rating=rtg_convert(pitching_ratings_pitches_forkball), 
      forkball_potential=rtg_convert(pitching_ratings_pitches_talent_forkball), 
      fastball_all_cnt=(fastball + splitter + sinker + cutter), 
      fastball_all_avg=round((fastball_rating + splitter_rating + sinker_rating + cutter_rating)/fastball_all_cnt, 0), 
      fastball_all_pot=round((fastball_potential + splitter_potential + sinker_potential + cutter_potential)/fastball_all_cnt, 0),
      fastball_all_avg=ifelse(fastball_all_cnt==0, 0, fastball_all_avg),
      fastball_all_pot=ifelse(fastball_all_cnt==0, 0, fastball_all_pot),
      fastball_all_max_rating=pmax(fastball_rating, splitter_rating, sinker_rating, cutter_rating),
      fastball_all_max_potential=pmax(fastball_potential, splitter_potential, sinker_potential, cutter_potential),
      offspeed_all_cnt=(changeup + circlechange + knuckleball), 
      offspeed_all_avg=round((changeup_rating + circlechange_rating + knuckleball_rating)/offspeed_all_cnt, 0), 
      offspeed_all_pot=round((changeup_potential + circlechange_potential + knuckleball_potential)/offspeed_all_cnt, 0),
      offspeed_all_avg=ifelse(offspeed_all_cnt==0, 0, offspeed_all_avg),
      offspeed_all_pot=ifelse(offspeed_all_cnt==0, 0, offspeed_all_pot),
      offspeed_all_max_rating=pmax(changeup_rating, circlechange_rating, knuckleball_rating),
      offspeed_all_max_potential=pmax(changeup_potential, circlechange_potential, knuckleball_potential),
      breaking_all_cnt=(slider + curveball + knucklecurve + screwball + forkball), 
      breaking_all_avg=round((slider_rating + curveball_rating + knucklecurve_rating + screwball_rating + forkball_rating)/breaking_all_cnt, 0),
      breaking_all_pot=round((slider_potential + curveball_potential + knucklecurve_potential + screwball_potential + forkball_potential)/breaking_all_cnt, 0),
      breaking_all_avg=ifelse(breaking_all_cnt==0, 0, breaking_all_avg),
      breaking_all_pot=ifelse(breaking_all_cnt==0, 0, breaking_all_pot),
      breaking_all_max_rating=pmax(slider_rating, curveball_rating, knucklecurve_rating, screwball_rating, forkball_rating),
      breaking_all_max_potential=pmax(slider_potential, curveball_potential, knucklecurve_potential, screwball_potential, forkball_potential), 
      pitch_cnt=fastball_all_cnt+offspeed_all_cnt+breaking_all_cnt, 
      pitch_avg=round((fastball_rating + splitter_rating + sinker_rating + cutter_rating + changeup_rating + circlechange_rating + knuckleball_rating + slider_rating + curveball_rating + knucklecurve_rating + screwball_rating + forkball_rating)/pitch_cnt, 0),
      pitch_pot=round((fastball_potential + splitter_potential + sinker_potential + cutter_potential + changeup_potential + circlechange_potential + knuckleball_potential + slider_potential + curveball_potential + knucklecurve_potential + screwball_potential + forkball_potential)/pitch_cnt, 0),
      injury_prone_ovr, 
      injury_prone_arm, 
      injury_prone_leg, 
      injury_prone_back,
      personality_greed, 
      personality_loyalty, 
      personality_play_for_winner, 
      personality_work_ethic, 
      personality_intelligence, 
      personality_leader,
      season_year,
      file_name, 
      file_date)
  
};
player_status=function(){
  
  players() %>% 
    select(playerID, 
           playerNAME, 
           posID, 
           leagueID, 
           levelID, 
           orgID, 
           teamID, 
           baseballAGE, 
           season_year, 
           file_name, 
           file_date) %>% 
    left_join(ootpCSV("players_roster_status.csv") %>% 
                transmute(playerID=player_id, 
                          uniform_number, 
                          is_active, 
                          is_on_secondary, 
                          is_on_dl, 
                          is_on_dl60, 
                          is_on_waivers,
                          is_on_irrevocable_waivers=irrevocable_waivers,
                          is_on_dfa=designated_for_assignment, 
                          days_on_waivers, 
                          days_on_waivers_left, 
                          days_on_dfa_left, 
                          claimed_team_id, 
                          options_used, 
                          options_used_this_year, 
                          has_received_arbitration, 
                          trade_status, 
                          must_be_active, 
                          mlb_service_days_ytd=mlb_service_days_this_year,
                          mlb_service_years, 
                          mlb_service_days,
                          pro_service_days_ytd=pro_service_days_this_year,
                          pro_service_years, 
                          pro_service_days, 
                          dl_days_this_year, 
                          years_protected_from_rule_5, 
                          file_name, 
                          file_date), 
              by=c("playerID", "file_name", "file_date")) %>% 
    left_join(ootpCSV("players_contract.csv") %>% 
                transmute(playerID=player_id, 
                          mlb_contract=is_major, 
                          years,
                          salary_y1=salary0,
                          salary_y2=salary1,
                          salary_y3=salary2,
                          salary_y4=salary3,
                          salary_y5=salary4,
                          salary_y6=salary5,
                          salary_y7=salary6,
                          salary_y8=salary7,
                          salary_y9=salary8,
                          salary_y10=salary9,
                          ntc=no_trade,
                          last_year_team_option, 
                          last_year_player_option, 
                          last_year_vesting_option, 
                          last_year_option_buyout,
                          next_last_year_team_option, 
                          next_last_year_player_option, 
                          next_last_year_vesting_option,
                          next_last_year_option_buyout,
                          opt_out, 
                          opt_out_relegation,
                          retained,
                          minimum_pa, 
                          minimum_pa_bonus, 
                          minimum_ip, 
                          minimum_ip_bonus, 
                          mvp_bonus, 
                          cyyoung_bonus, 
                          allstar_bonus, 
                          file_name, 
                          file_date), 
              by=c("playerID", "file_name", "file_date")) %>%
    transmute(playerID, 
              playerNAME, 
              posID, 
              leagueID, 
              levelID, 
              orgID, 
              teamID, 
              baseballAGE, 
              uniform_number, 
              mlb_contract, 
              years, 
              salary_y1, 
              salary_y2, 
              salary_y3, 
              salary_y4, 
              salary_y5, 
              salary_y6, 
              salary_y7, 
              salary_y8, 
              salary_y9, 
              salary_y10, 
              ntc, 
              last_year_team_option, 
              last_year_player_option, 
              last_year_vesting_option, 
              last_year_option_buyout,
              next_last_year_team_option, 
              next_last_year_player_option, 
              next_last_year_vesting_option,
              next_last_year_option_buyout,
              opt_out, 
              opt_out_relegation,
              retained,
              minimum_pa, 
              minimum_pa_bonus, 
              minimum_ip, 
              minimum_ip_bonus, 
              mvp_bonus, 
              cyyoung_bonus, 
              allstar_bonus,
              is_active, 
              is_on_secondary, 
              is_on_dl, 
              is_on_dl60, 
              is_on_waivers, 
              is_on_irrevocable_waivers, 
              is_on_dfa, 
              days_on_waivers, 
              days_on_waivers_left, 
              days_on_dfa_left, 
              claimed_team_id, 
              options_used, 
              options_used_this_year, 
              has_received_arbitration, 
              trade_status, 
              must_be_active, 
              mlb_service_days_ytd, 
              mlb_service_years, 
              mlb_service_days, 
              pro_service_days_ytd, 
              pro_service_years, 
              pro_service_days, 
              dl_days_this_year, 
              years_protected_from_rule_5,
              season_year, 
              file_name, 
              file_date)
  
};
coaches=function(){
  
  rtg_convert=function(x){
    
    ifelse(x>=192, 
           80, 
           ifelse(x>=175, 
                  75, 
                  ifelse(x>=159, 
                         70, 
                         ifelse(x>=142, 
                                65, 
                                ifelse(x>=125, 
                                       60, 
                                       ifelse(x>=109, 
                                              55, 
                                              ifelse(x>=92, 
                                                     50, 
                                                     ifelse(x>=75, 
                                                            45, 
                                                            ifelse(x>=59, 
                                                                   40, 
                                                                   ifelse(x>=42, 
                                                                          35, 
                                                                          ifelse(x>=25, 
                                                                                 30, 
                                                                                 ifelse(x>=9, 
                                                                                        25, 
                                                                                        ifelse(x>=1, 
                                                                                               20, 
                                                                                               0)
                                                                                 )
                                                                          )
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
                                )
                         )
                  )
           )
    )
    
  };
  
  ootpCSV("coaches.csv") %>% 
    left_join(nations() %>% 
                transmute(nation_id=nationID, nationDESC), 
              by="nation_id") %>%
    left_join(cities() %>% 
                transmute(city_of_birth_id=cityID, 
                          birthcityDESC=ifelse(nationID==206, 
                                               paste0(cityNAME, ", ", stateDESC), 
                                               paste0(cityNAME, ", ", nationDESC))), 
              by="city_of_birth_id") %>%
    left_join(teams() %>% 
                transmute(teamID, 
                          teamDESC, 
                          leagueID, 
                          leagueDESC,
                          levelID, 
                          levelDESC, 
                          orgID, 
                          orgDESC, 
                          season_year=leagues() %>% filter(leagueID==100) %>% pull(season_year), 
                          file_name, 
                          file_date), 
              by=c("team_id"="teamID", "file_name", "file_date")) %>%
    transmute(coachID=coach_id, 
              coachNAME=paste(first_name, last_name), 
              posID=position, 
              occID=occupation, 
              teamID=team_id,
              teamDESC=ifelse(teamID==0, "FA", teamDESC), 
              orgID=ifelse(teamID==0, 0, orgID), 
              orgDESC=ifelse(teamID==0, "FA", orgDESC),
              leagueID=ifelse(teamID==0, 0, leagueID), 
              leagueDESC=ifelse(teamID==0, "FA", leagueDESC), 
              levelID=ifelse(teamID==0, 0, levelID), 
              levelDESC=ifelse(teamID==0, "FA", levelDESC), 
              playerID=former_player_id,
              date_of_birth=lubridate::ymd(date_of_birth),
              baseballAGE=floor(as.numeric(difftime(
                lubridate::ymd(paste0(ifelse(is.na(season_year), 
                                             leagues() %>% 
                                               filter(leagueID==100) %>% 
                                               pull(season_year),
                                             season_year), 
                                      "06-30")),
                date_of_birth))/365.25), 
              birthcityID=city_of_birth_id, 
              birthcityDESC,
              nationID=nation_id,
              nationDESC,
              years_exp=experience,
              personality, 
              positive_relation, 
              negative_relation, 
              contract_years, 
              contract_salary, 
              contract_extension_years, 
              contract_extension_salary, 
              rating_manager=rtg_convert(manager_value),
              rating_base_coach=rtg_convert(basecoach_value),
              rating_pitching_coach=rtg_convert(pitching_coach_value),
              rating_hitting_coach=rtg_convert(hitting_coach_value),
              rating_scout=rtg_convert(scout_value),
              rating_doctor=rtg_convert(doctor_value),
              scout_amateur_preference, 
              scout_major=rtg_convert(scout_major),
              scout_minor=rtg_convert(scout_minor),
              scout_amateur=rtg_convert(scout_amateur),
              scout_international=rtg_convert(scout_international),
              hitting_focus, 
              pitching_focus, 
              training_focus, 
              teach_hitting=rtg_convert(teach_hitting),
              teach_pitching=rtg_convert(teach_pitching),
              teach_catcher=rtg_convert(teach_c),
              teach_infield=rtg_convert(teach_if),
              teach_outfield=rtg_convert(teach_of),
              teach_running=rtg_convert(teach_running),
              teach_ovr=rtg_convert(handle_players), 
              teach_veterans=rtg_convert(handle_veterans), 
              teach_prospects=rtg_convert(handle_rookies),
              management_style,
              lr_matchup,
              stealing, 
              running, 
              pinchrun, 
              pinchhit_pos, 
              pinchhit_pitch, 
              bunt_hit, 
              bunt, 
              hit_run, 
              run_hit, 
              squeeze, 
              hook_start, 
              hook_relief, 
              quick_left, 
              closer, 
              opener, 
              pitch_around, 
              intentional_walk, 
              hold_runner, 
              guard_lines, 
              infield_in, 
              outfield_in, 
              corners_in, 
              shift_if, 
              shift_of, 
              num_pitchers, 
              num_hitters,
              favor_speed_to_power, 
              favor_avg_to_obp, 
              favor_defense_to_offense, 
              favor_pitching_to_hitting, 
              favor_veterans_to_prospects, 
              trade_aggressiveness, 
              player_loyalty, 
              trade_frequency, 
              trade_preference, 
              value_stats, 
              value_this_year, 
              value_last_year, 
              value_two_years, 
              draft_value, 
              intl_fa_value, 
              develop_value, 
              ratings_value, 
              heal_legs=rtg_convert(heal_legs), 
              heal_arms=rtg_convert(heal_arms), 
              heal_back=rtg_convert(heal_back), 
              heal_other=rtg_convert(heal_other), 
              heal_rest=rtg_convert(heal_rest),
              prevent_legs=rtg_convert(prevent_legs),
              prevent_arms=rtg_convert(prevent_arms),
              prevent_back=rtg_convert(prevent_back),
              prevent_other=rtg_convert(prevent_other),
              season_year=ifelse(is.na(season_year), 
                                 leagues() %>% 
                                   filter(leagueID==100) %>% 
                                   pull(season_year),
                                 season_year),
              file_name, 
              file_date) %>% 
    filter(teamID==0)
  
};

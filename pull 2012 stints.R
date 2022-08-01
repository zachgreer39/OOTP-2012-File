library(tidyverse);
library(rvest);
library(readxl);


url="https://www.baseball-reference.com/leagues/majors/2012-standings.shtml"


df_mlb=cbind(season=2012, 
         rbind(cbind("AL East", c("NYY", "BAL", "TBR", "TOR", "BOS")),
               cbind("AL Central", c("DET", "CHW", "KCR", "CLE", "MIN")),
               cbind("AL West", c("OAK", "TEX", "LAA", "SEA")),
               cbind("NL East", c("WSN", "ATL", "PHI", "NYM", "MIA")),
               cbind("NL Central", c("CIN", "STL", "MIL", "PIT", "CHC", "HOU")),
               cbind("NL West", c("SFG", "LAD", "ARI", "SDP", "COL"))), 
         url %>% 
           read_html() %>% 
           html_element("body") %>% 
           html_table() %>% 
           as.data.frame() %>% 
           filter(Tm!="Tm")) %>% 
  rename(division=2, teamDESC=3) %>%
           transmute(season=as.numeric(season), 
                     division, 
                     teamDESC, 
                     teamNAME=Tm, 
                     w=W, 
                     l=L, 
                     url=paste0("https://www.baseball-reference.com/register/affiliate.cgi?id=", 
                                teamDESC,
                                "&year=",
                                season))



milb_standings=function(num){
  
  df=df_mlb[num,]
  
  df$url %>% 
    read_html() %>%
    html_nodes(xpath='//comment()') %>%
    html_text() %>%
    paste(collapse="") %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]] %>% 
    as.data.frame() %>% 
    transmute(season=df$season, 
              org=df$teamDESC, 
              teamNAME=Tm, 
              lg=Lg, 
              lev=Lev, 
              w=W, 
              l=L)
  
  
}



df_milb=rbind(milb_standings(1),
              milb_standings(2),
              ##milb_standings(3),
              milb_standings(4), 
              milb_standings(5), 
              milb_standings(6), 
              milb_standings(7), 
              milb_standings(8), 
              milb_standings(9), 
              milb_standings(10), 
              milb_standings(11),
              milb_standings(12),
              ##milb_standings(13),
              milb_standings(14), 
              milb_standings(15), 
              milb_standings(16), 
              milb_standings(17), 
              milb_standings(18), 
              ##milb_standings(19), 
              milb_standings(20), 
              milb_standings(21),
              milb_standings(22),
              milb_standings(23),
              milb_standings(24), 
              milb_standings(25), 
              milb_standings(26), 
              milb_standings(27), 
              milb_standings(28), 
              milb_standings(29), 
              milb_standings(30))


write.csv(df_milb, "df_milb.csv", row.names=FALSE)





df_milb=read_excel("cant stay organized.xlsx")
##pulled in url link manually and added ootp team ID





team_tables=function(link){
  
  paste0("https://www.baseball-reference.com/register/team.cgi?id=",
         link) %>%
    read_html() %>%
    html_nodes(xpath='//comment()') %>%
    html_text() %>%
    paste(collapse="") %>%
    read_html() %>%
    html_table() %>% 
    .[[12]] %>% 
    cbind(link, .) %>% 
    as.data.frame() %>% 
    left_join(df_milb %>% 
                select(org, ootpTEAM, teamNAME, lg, lev, link), 
              by="link") %>% 
    transmute(ootpTEAM, 
              teamNAME, 
              org,
              lg, 
              lev, 
              player_name=Name, 
              age=as.numeric(Age), 
              bats=B, 
              throws=T, 
              height=Ht, 
              weight=as.numeric(Wt), 
              dob=lubridate::mdy(DoB), 
              birthplace=Birthplace, 
              stint=Stint,
              from=lubridate::ymd(From), 
              to=lubridate::ymd(To))
  
};




##df_stints=map_df(.x=df_milb$link[1:10], .f=team_tables)
##df_stints2=map_df(.x=df_milb$link[11:20], .f=team_tables)
##df_stints3=map_df(.x=df_milb$link[21:30], .f=team_tables)
df_stints4=map_df(.x=df_milb$link[31:40], .f=team_tables)
##df_stints5=map_df(.x=df_milb$link[41:50], .f=team_tables)
##df_stints6=map_df(.x=df_milb$link[51:60], .f=team_tables)
##df_stints7=map_df(.x=df_milb$link[61:70], .f=team_tables)
##df_stints8=map_df(.x=df_milb$link[71:80], .f=team_tables)
##df_stints9=map_df(.x=df_milb$link[91:100], .f=team_tables)
##df_stints10=map_df(.x=df_milb$link[101:110], .f=team_tables)
##df_stints11=map_df(.x=df_milb$link[81:90], .f=team_tables)
##df_stints12=map_df(.x=df_milb$link[111:120], .f=team_tables)
##df_stints13=map_df(.x=df_milb$link[121:130], .f=team_tables)
##df_stints14=map_df(.x=df_milb$link[131:140], .f=team_tables)
##df_stints15=map_df(.x=df_milb$link[141:150], .f=team_tables)
##df_stints16=map_df(.x=df_milb$link[151:160], .f=team_tables)
##df_stints17=map_df(.x=df_milb$link[161:170], .f=team_tables)
##df_stints18=map_df(.x=df_milb$link[171:180], .f=team_tables)
##df_stints19=map_df(.x=df_milb$link[180:200], .f=team_tables)


df_stints=rbind(
  df_stints, 
  df_stints10, 
  df_stints11, 
  df_stints12, 
  df_stints13, 
  df_stints14, 
  df_stints15, 
  df_stints16, 
  df_stints17, 
  df_stints18, 
  df_stints19, 
  df_stints2, 
  df_stints3, 
  df_stints5, 
  df_stints6, 
  df_stints7, 
  df_stints8, 
  df_stints9
)



##df_stints1=map_df(.x=df_milb$link[31:35], .f=team_tables)
df_stints2=map_df(.x=df_milb$link[36:38], .f=team_tables)
##df_stints3=map_df(.x=df_milb$link[39:40], .f=team_tables)
##df_stints4=map_df(.x=df_milb$link[201:227], .f=team_tables)
##df_stints2=team_tables(df_milb$link[36])
##df_stints5=team_tables("01883910") - leading 0 in link got rounded
##f_stints6=team_tables(df_milb$link[38])



df_stints=rbind(
  df_stints, 
  df_stints1, 
  df_stints2, 
  df_stints3, 
  df_stints4, 
  df_stints5, 
  df_stints6
)


write.csv(df_stints, "df_stints.csv", row.names=FALSE)

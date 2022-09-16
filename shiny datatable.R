library(tidyverse);
library(shiny);
library(DT);

df_catchers=df_players %>% 
  inner_join(read.csv("catcher_ratings.csv") %>% 
               rename(playerID=1), 
             by="playerID") %>%
  transmute(org=orgDESC, 
            level=levelDESC, 
            league=leagueDESC, 
            pos=posDESC, 
            id=playerID, 
            name=playerNAME, 
            bats, 
            throws, 
            age, 
            cExp=ifelse(cExp==0, NA, cExp), 
            cAbi=ifelse(cAbi==0, NA, cAbi), 
            cArm=ifelse(cArm==0, NA, cArm)) %>% 
  arrange(org, level, league, pos, id);


catching_stats=fieldSummary() %>% 
  semi_join(df_catchers %>% 
              transmute(playerID=id), 
            by="playerID") %>%
  filter(posCODE=="C") %>% 
  left_join(fieldSummary() %>% 
               select(playerID, teamDESC, year, levelDESC, inn) %>% 
               group_by(playerID, teamDESC, levelDESC, year) %>% 
               summarise(innTotal=sum(inn)), 
            by=c("playerID", "teamDESC", "year", "levelDESC")) %>%
  transmute(id=playerID, 
            year, 
            team=teamDESC, 
            tmLevel=levelDESC, 
            g, 
            gs, 
            inn, 
            cUsage=round(inn/innTotal, 2),
            tc, po, a, e, dp, fldPERC, zr, pb, sba, csPERC)





list_stats=split(unname(split(catching_stats, seq_len(nrow(catching_stats)))), catching_stats$id)

stat_merge=function(num){
  
  x=unname(which(sapply(names(list_stats), 
                        function(x) as.character(num) %in% x), 
                 TRUE))
  
  if(is_empty(x)) {
    
    cbind(" " = "expand", df_catchers %>% filter(id==num), edited=0, stats=I(list()))
    
  } else {
    
    cbind(" " = "expand", df_catchers %>% filter(id==num), edited=0, stats=I(list_stats[x]))
    
  }
  
}



Dat=map_df(.x=9, .f=stat_merge)








df_catchers %>% 
  left_join(catching_stats, 
            by="id")




















datatable(df_catchers %>% 
            mutate(edited=0, 
                   id=as.character(id)), 
          editable=list(target="cell", 
                        disable=list(columns=c(0:8))), 
          rownames=FALSE, 
          filter="top", 
          extensions="Buttons",
          options=list(paging=TRUE, 
                       pageLength=25, 
                       scrollX=TRUE, 
                       scrollY=TRUE, 
                       autowidth=TRUE, 
                       buttons=c("csv"), 
                       server=FALSE, 
                       dom="Bfrtip")) %>%
  formatStyle("edited", 
              backgroundColor=styleEqual(c(0,1), 
                                         c("white", "yellow"))) %>%
  formatStyle(c("cAbi", "cArm"), 
              backgroundColor=styleInterval(seq(50, 200, 1), 
                                            colorRampPalette(c("white", 
                                                               "#E39FF6"))(length(seq(50, 200, 1))+1)))




brks=seq(50, 200, 1)
clrs=colorRampPalette(c("white", "#E39FF6"))(length(brks)+1)

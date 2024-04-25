library(tidyverse)
library(gmodels)
library(stringr)
library(readxl)
library(writexl)
library(devtools)
library(lubridate)
library(DataCombine)

original<-read_excel('/Users/patryk/Desktop/phone_reference/original(deleted_names).xlsx')
main <- read_excel('/Users/patryk/Desktop/phone_reference/main.xlsx')
main$EXT<-as.numeric(main$EXT)
lukes <- read_excel('/Users/patryk/Desktop/phone_reference/lukes.xlsx')
kings <- read_excel('/Users/patryk/Desktop/phone_reference/kings.xls')
roosevelt <- read_excel('/Users/patryk/Desktop/phone_reference/roosevelt.xlsx')
bimc <- read_excel('/Users/patryk/Desktop/phone_reference/bimc.xlsx')
reference_numbers<-bind_rows(main,lukes,kings,roosevelt, bimc) #reference

original %>%
  select(MRN, Org, `Panel Type`, `Drug Name`, `Drug Result`, `Drug Interp`, Hospital, `Hospital Add`, `Hospital Zip`, Room, `Hospital Phone`, `Cdate/Ctime`, ADM.DT) %>% 
    mutate(`Hospital Zip` = as.numeric(`Hospital Zip`)) %>% 
           filter(!is.na(Org)) %>%
  mutate(Location = case_when(`Hospital Zip` == 10029 & Room == 'PICU' ~ Hospital,
                              `Hospital Zip` == 10029 & Room == 'TICU' ~ Hospital,
                              `Hospital Zip` == 10029 & Room == 'SICU' ~ Hospital,
                              `Hospital Zip` == 10029 & Room == 'NCVU' ~ Hospital,
                              `Hospital Zip` == 10029 & !is.na(Room) ~ Room,
                              `Hospital Zip` == 10029 & is.na(Room) & !is.na(`Hospital Phone`) ~ Hospital,
                              `Hospital Zip` == 10029 & is.na(Room) & is.na(`Hospital Phone`) ~ Hospital,
                              
                              `Hospital Zip` == 10003 & !is.na(Room) ~ Hospital,
                              `Hospital Zip` == 10003 & !is.na(Room) & is.na(`Hospital Phone`) ~ Hospital,
                              `Hospital Zip` == 10003 & is.na(Room) & !is.na(`Hospital Phone`) ~ Hospital,
                              `Hospital Zip` == 10003 & is.na(Room) & is.na(`Hospital Phone`) ~ Hospital,

                              `Hospital Zip` == 10009 & !is.na(Room) ~ Hospital,
                              `Hospital Zip` == 10009 & !is.na(Room) & is.na(`Hospital Phone`) ~ Hospital,
                              `Hospital Zip` == 10009 & is.na(Room) & !is.na(`Hospital Phone`) ~ Hospital,
                              `Hospital Zip` == 10009 & is.na(Room) & is.na(`Hospital Phone`) ~ Hospital,
                              
                              `Hospital Zip` == 10019 & Room == 'RVT' ~ Hospital,
                              `Hospital Zip` == 10019 & !is.na(Room) ~ Room,
                              `Hospital Zip` == 10019 & is.na(Room) & !is.na(`Hospital Phone`) ~ Hospital,
                              `Hospital Zip` == 10019 & is.na(Room) & is.na(`Hospital Phone`) ~ Hospital,
                              
                              `Hospital Zip` == 10025 & Room == 'STL' ~ Hospital,
                              `Hospital Zip` == 10025 & !is.na(Room) ~ Room,
                              `Hospital Zip` == 10025 & is.na(Room) & !is.na(`Hospital Phone`) ~ Hospital,
                              `Hospital Zip` == 10025 & is.na(Room) & is.na(`Hospital Phone`) ~ Hospital,
                              
                              `Hospital Zip` == 10128 & !is.na(Room) ~ Room,
                              `Hospital Zip` == 10128 & is.na(Room) & !is.na(`Hospital Phone`) ~ Hospital,
                              `Hospital Zip` == 10128 & is.na(Room) & is.na(`Hospital Phone`) ~ Hospital,
                              
                              `Hospital Zip` == 10129 & !is.na(Room) ~ Room,
                              `Hospital Zip` == 10129 & is.na(Room) & !is.na(`Hospital Phone`) ~ Hospital,
                              `Hospital Zip` == 10129 & is.na(Room) & is.na(`Hospital Phone`) ~ Hospital,
                        
                              `Hospital Zip` == 11102 & !is.na(Room) ~ Room,
                              `Hospital Zip` == 11102 & is.na(Room) & !is.na(`Hospital Phone`) ~ Hospital,
                              `Hospital Zip` == 11102 & is.na(Room) & is.na(`Hospital Phone`) ~ Hospital,
                              
                              `Hospital Zip` == 11106 & !is.na(Room) ~ Room,
                              `Hospital Zip` == 11106 & is.na(Room) & !is.na(`Hospital Phone`) ~ Hospital,
                              `Hospital Zip` == 11106 & is.na(Room) & is.na(`Hospital Phone`) ~ Hospital,
                              
                              `Hospital Zip` == 11234 & Room == 'BIKH' ~ Hospital,
                              `Hospital Zip` == 11234 & !is.na(Room) ~ Room,
                              `Hospital Zip` == 11234 & is.na(Room) & !is.na(`Hospital Phone`) ~ Hospital,
                              `Hospital Zip` == 11234 & is.na(Room) & is.na(`Hospital Phone`) ~ Hospital,
                              
                              `Hospital Zip` == 11572 & !is.na(Room) ~ Room,
                              `Hospital Zip` == 11572 & is.na(Room) & !is.na(`Hospital Phone`) ~ Hospital,
                              `Hospital Zip` == 11572 & is.na(Room) & is.na(`Hospital Phone`) ~ Hospital)) -> locations

locations%>%
  mutate(Category = case_when(`Hospital Zip` == 10029 & Room == 'PICU' ~ 'Floor',
                              `Hospital Zip` == 10029 & Room == 'TICU' ~ 'Floor',
                              `Hospital Zip` == 10029 & Room == 'SICU' ~ 'Floor',
                              `Hospital Zip` == 10029 & Room == 'NCVU' ~ 'Floor',
                              `Hospital Zip` == 10029 & !is.na(Room) ~ 'Room',
                              `Hospital Zip` == 10029 & is.na(Room) & !is.na(`Hospital Phone`) ~ 'Floor',
                              `Hospital Zip` == 10029 & is.na(Room) & is.na(`Hospital Phone`) ~ 'Floor',
                              
                              `Hospital Zip` == 10003 & !is.na(Room) ~ 'Floor',
                              `Hospital Zip` == 10003 & !is.na(Room) & is.na(`Hospital Phone`) ~ 'Floor',
                              `Hospital Zip` == 10003 & is.na(Room) & !is.na(`Hospital Phone`) ~ 'Floor',
                              `Hospital Zip` == 10003 & is.na(Room) & is.na(`Hospital Phone`) ~ 'Floor',
                              
                              `Hospital Zip` == 10009 & !is.na(Room) ~ 'Floor',
                              `Hospital Zip` == 10009 & !is.na(Room) & is.na(`Hospital Phone`) ~ 'Floor',
                              `Hospital Zip` == 10009 & is.na(Room) & !is.na(`Hospital Phone`) ~ 'Floor',
                              `Hospital Zip` == 10009 & is.na(Room) & is.na(`Hospital Phone`) ~ 'Floor',
                              
                              `Hospital Zip` == 10019 & Room == 'RVT' ~ 'Floor',
                              `Hospital Zip` == 10019 & !is.na(Room) ~ 'Room',
                              `Hospital Zip` == 10019 & is.na(Room) & !is.na(`Hospital Phone`) ~ 'Floor',
                              `Hospital Zip` == 10019 & is.na(Room) & is.na(`Hospital Phone`) ~ 'Floor',
                              
                              `Hospital Zip` == 10025 & Room == 'STL' ~ 'Floor',
                              `Hospital Zip` == 10025 & !is.na(Room) ~ 'Room',
                              `Hospital Zip` == 10025 & is.na(Room) & !is.na(`Hospital Phone`) ~ 'Floor',
                              `Hospital Zip` == 10025 & is.na(Room) & is.na(`Hospital Phone`) ~ 'Floor',
                              
                              `Hospital Zip` == 10128 & !is.na(Room) ~ 'Room',
                              `Hospital Zip` == 10128 & is.na(Room) & !is.na(`Hospital Phone`) ~ 'Floor',
                              `Hospital Zip` == 10128 & is.na(Room) & is.na(`Hospital Phone`) ~ 'Floor',
                              
                              `Hospital Zip` == 10129 & !is.na(Room) ~ 'Room',
                              `Hospital Zip` == 10129 & is.na(Room) & !is.na(`Hospital Phone`) ~ 'Floor',
                              `Hospital Zip` == 10129 & is.na(Room) & is.na(`Hospital Phone`) ~ 'Floor',
                              
                              `Hospital Zip` == 11102 & !is.na(Room) ~ 'Room',
                              `Hospital Zip` == 11102 & is.na(Room) & !is.na(`Hospital Phone`) ~ 'Floor',
                              `Hospital Zip` == 11102 & is.na(Room) & is.na(`Hospital Phone`) ~ 'Floor',
                              
                              `Hospital Zip` == 11106 & !is.na(Room) ~ 'Room',
                              `Hospital Zip` == 11106 & is.na(Room) & !is.na(`Hospital Phone`) ~ 'Floor',
                              `Hospital Zip` == 11106 & is.na(Room) & is.na(`Hospital Phone`) ~ 'Floor',
                              
                              `Hospital Zip` == 11234 & Room == 'BIKH' ~ 'Floor',
                              `Hospital Zip` == 11234 & !is.na(Room) ~ 'Room',
                              `Hospital Zip` == 11234 & is.na(Room) & !is.na(`Hospital Phone`) ~ 'Floor',
                              `Hospital Zip` == 11234 & is.na(Room) & is.na(`Hospital Phone`) ~ 'Floor',
                              
                              `Hospital Zip` == 11572 & !is.na(Room) ~ 'Room',
                              `Hospital Zip` == 11572 & is.na(Room) & !is.na(`Hospital Phone`) ~ 'Floor',
                              `Hospital Zip` == 11572 & is.na(Room) & is.na(`Hospital Phone`) ~ 'Floor')) %>%
  mutate(Time = c(difftime(mdy_hm(`Cdate/Ctime`), mdy(ADM.DT), units = 'hours'))) %>% 
  mutate(Time = round(Time ,2)) %>% 
  filter(Time>48) %>%
  mutate(Institution = case_when(`Hospital Zip` == 10029 ~ 'Sinai - Main Campus',
                                 `Hospital Zip` == 10003 ~ 'Mount Sinai Beth Israel',
                                 `Hospital Zip` == 11234 ~'Mount Sinai Brooklyn',
                                 `Hospital Zip` == 10025 ~ 'Mount Sinai Morningside',
                                 `Hospital Zip` == 11102 ~ 'Mount Sinai Queens',
                                 `Hospital Zip` == 10019 ~ 'Mount Sinai West')) %>%
  filter(!is.na(Institution)) %>%
  select(-c(Hospital,`Hospital Add`, `Hospital Zip`, Room, `Hospital Phone`, `Cdate/Ctime`, ADM.DT)) %>% mutate(Time=round(Time ,2))-> locations








#location split
locations_split<-split(locations, f=locations$Institution)


#beth israel
  Sinai_Beth_Israel<-locations_split$`Mount Sinai Beth Israel`
    Sinai_Beth_Israel_split<-split(Sinai_Beth_Israel, f=Sinai_Beth_Israel$Category)
      
#      Sinai_Beth_Israel_Room<-as.matrix(Sinai_Beth_Israel_split$Room)
#        Sinai_Beth_Israel_Room<-matrix(Sinai_Beth_Israel_Room, ncol = ncol(Sinai_Beth_Israel_Room), dimnames = NULL)
          Type_Room<- cbind('Rooms','','','','','','','','','')
            Header_title<- cbind('MRN','Org','Panel Type','Drug Name','Drug Result','Drug Interp','Location','Category','Time', 'Institution')
  
      Sinai_Beth_Israel_Floor<-as.matrix(Sinai_Beth_Israel_split$Floor)
        Sinai_Beth_Israel_Floor<-matrix(Sinai_Beth_Israel_Floor, ncol = ncol(Sinai_Beth_Israel_Floor), dimnames = NULL)
          Type_Floor<- cbind('Floor','','','','','','','','','')
            Header_title<- cbind('MRN','Org','Panel Type','Drug Name','Drug Result','Drug Interp','Location','Category','Time', 'Institution')
            space<- cbind('','','','','','','','','','')
             Beth_Israel<-as.data.frame(rbind(Type_Room, Header_title, space, Type_Floor, Header_title, Sinai_Beth_Israel_Floor))
  
  
             
             
#brooklyn            
  Sinai_Brooklyn<-locations_split$`Mount Sinai Brooklyn`
    Sinai_Brooklyn_split<-split(Sinai_Brooklyn, f=Sinai_Brooklyn$Category)
    
      #Sinai_Brooklyn_Room<-as.matrix(Sinai_Brooklyn_split$Room)
      #Sinai_Brooklyn_Room<-matrix(Sinai_Brooklyn_Room, ncol = ncol(Sinai_Brooklyn_Room), dimnames = NULL)
          Type_Room<- cbind('Rooms','','','','','','','','','')
            Header_title<- cbind('MRN','Org','Panel Type','Drug Name','Drug Result','Drug Interp','Location','Category','Time', 'Institution')

      Sinai_Brooklyn_Floor<-as.matrix(Sinai_Brooklyn_split$Floor)
        Sinai_Brooklyn_Floor<-matrix(Sinai_Brooklyn_Floor, ncol = ncol(Sinai_Brooklyn_Floor), dimnames = NULL)
          Type_Floor<- cbind('Floor','','','','','','','','','')
            Header_title<- cbind('MRN','Org','Panel Type','Drug Name','Drug Result','Drug Interp','Location','Category','Time', 'Institution')
              space<- cbind('','','','','','','','','','')
                Brooklyn<-as.data.frame(rbind(Type_Room, Header_title, space, Type_Floor, Header_title, Sinai_Brooklyn_Floor))

#Morningside
  Sinai_Morningside<-locations_split$`Mount Sinai Morningside`
    Sinai_Morningside_split<-split(Sinai_Morningside, f=Sinai_Morningside$Category)

      #Sinai_Morningside_Room<-as.matrix(Sinai_Morningside_split$Room)
        #Sinai_Morningside_Room<-matrix(Sinai_Morningside_Room, ncol = ncol(Sinai_Morningside_Room), dimnames = NULL)
          Type_Room<- cbind('Rooms','','','','','','','','','')
            Header_title<- cbind('MRN','Org','Panel Type','Drug Name','Drug Result','Drug Interp','Location','Category','Time', 'Institution')

      Sinai_Morningside_Floor<-as.matrix(Sinai_Morningside_split$Floor)
        Sinai_Morningside_Floor<-matrix(Sinai_Morningside_Floor, ncol = ncol(Sinai_Morningside_Floor), dimnames = NULL)
          Type_Floor<- cbind('Floor','','','','','','','','','')
            Header_title<- cbind('MRN','Org','Panel Type','Drug Name','Drug Result','Drug Interp','Location','Category','Time', 'Institution')
              space<- cbind('','','','','','','','','','')
                Morningside<-as.data.frame(rbind(Type_Room, Header_title, space, Type_Floor, Header_title, Sinai_Morningside_Floor))

#Sinai West
  Sinai_West<-locations_split$`Mount Sinai West`
    Sinai_West_split<-split(Sinai_West, f=Sinai_West$Category)

      #Sinai_West_Room<-as.matrix(Sinai_West_split$Room)
        #Sinai_West_Room<-matrix(Sinai_West_Room, ncol = ncol(Sinai_West_Room), dimnames = NULL)
          Type_Room<- cbind('Rooms','','','','','','','','','')
            Header_title<- cbind('MRN','Org','Panel Type','Drug Name','Drug Result','Drug Interp','Location','Category','Time', 'Institution')

      Sinai_West_Floor<-as.matrix(Sinai_West_split$Floor)
        Sinai_West_Floor<-matrix(Sinai_West_Floor, ncol = ncol(Sinai_West_Floor), dimnames = NULL)
          Type_Floor<- cbind('Floor','','','','','','','','','')
            Header_title<- cbind('MRN','Org','Panel Type','Drug Name','Drug Result','Drug Interp','Location','Category','Time', 'Institution')
              space<- cbind('','','','','','','','','','')
                West<-as.data.frame(rbind(Type_Room, Header_title, space, Type_Floor, Header_title, Sinai_West_Floor))

#Sinai West
Sinai_Queens<-locations_split$`Mount Sinai Queens`
Sinai_Queens_split<-split(Sinai_Queens, f=Sinai_Queens$Category)
                
Sinai_Queens_Room<-as.matrix(Sinai_Queens_split$Room)
Sinai_Queens_Room<-matrix(Sinai_Queens_Room, ncol = ncol(Sinai_Queens_Room), dimnames = NULL)
Type_Room<- cbind('Rooms','','','','','','','','','')
Header_title<- cbind('MRN','Org','Panel Type','Drug Name','Drug Result','Drug Interp','Location','Category','Time', 'Institution')
                
#Sinai_Queens_Floor<-as.matrix(Sinai_Queens_split$Floor)
#Sinai_Queens_Floor<-matrix(Sinai_Queens_Floor, ncol = ncol(Sinai_Queens_Floor), dimnames = NULL)
Type_Floor<- cbind('Floor','','','','','','','','','')
Header_title<- cbind('MRN','Org','Panel Type','Drug Name','Drug Result','Drug Interp','Location','Category','Time', 'Institution')
space<- cbind('','','','','','','','','','')
Queens<-as.data.frame(rbind(Type_Room, Header_title, Sinai_Queens_Room, space, Type_Floor, Header_title))
                
                

#Sinai Main Campus
  Sinai_Harlem_Icahn<-locations_split$`Sinai - Main Campus`
    Sinai_Harlem_split<-split(Sinai_Harlem_Icahn, f=Sinai_Harlem_Icahn$Category)
            
    Sinai_Harlem_Room<-as.matrix(Sinai_Harlem_split$Room)
      Sinai_Harlem_Room<-matrix(Sinai_Harlem_Room, ncol = ncol(Sinai_Harlem_Room), dimnames = NULL)
        Type_Room<- cbind('Rooms','','','','','','','','','')
          Header_title<- cbind('MRN','Org','Panel Type','Drug Name','Drug Result','Drug Interp','Location','Category','Time', 'Institution')
            
    Sinai_Harlem_Floor<-as.matrix(Sinai_Harlem_split$Floor)
      Sinai_Harlem_Floor<-matrix(Sinai_Harlem_Floor, ncol = ncol(Sinai_Harlem_Floor), dimnames = NULL)
        Type_Floor<- cbind('Floor','','','','','','','','','')
          Header_title<- cbind('MRN','Org','Panel Type','Drug Name','Drug Result','Drug Interp','Location','Category','Time', 'Institution')
            space<- cbind('','','','','','','','','','')
              Sinai_Harlem<-as.data.frame(rbind(Type_Room, Header_title, Sinai_Harlem_Room, space, Type_Floor, Header_title, Sinai_Harlem_Floor))
            
write_xlsx(locations, '/Users/patryk/Desktop/phone_reference/locations_time(48)_Institution.xlsx')
              

Beth_Israel %>%
  select(V1,V2,V3,V4,V5,V6,V7,V9,V10) -> Beth_Israel_excel
Brooklyn %>%
  select(V1,V2,V3,V4,V5,V6,V7,V9,V10) -> Brooklyn_excel
Morningside %>%
  select(V1,V2,V3,V4,V5,V6,V7,V9,V10) -> Morningside_excel
West %>%
  select(V1,V2,V3,V4,V5,V6,V7,V9,V10) -> West_excel
Queens %>%
  select(V1,V2,V3,V4,V5,V6,V7,V9,V10) -> Queens_excel
Sinai_Harlem %>%
  select(V1,V2,V3,V4,V5,V6,V7,V9,V10) -> Harlem_excel

write_xlsx(Beth_Israel_excel, '/Users/patryk/Desktop/ape submission/Beth_Israel_excel.xlsx')
write_xlsx(Brooklyn_excel, '/Users/patryk/Desktop/ape submission/Brooklyn_excel.xlsx')
write_xlsx(Morningside_excel, '/Users/patryk/Desktop/ape submission/Morningside_excel.xlsx')
write_xlsx(West_excel, '/Users/patryk/Desktop/ape submission/West_excel.xlsx')
write_xlsx(Queens_excel, '/Users/patryk/Desktop/ape submission/Queens_excel.xlsx')
write_xlsx(Harlem_excel, '/Users/patryk/Desktop/ape submission/Harlem_excel.xlsx')

             






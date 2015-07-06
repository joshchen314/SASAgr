LIBNAME  prog2 'D:\SASLIB';     
data  prog2.agr_service941;                                                             /*  ==99年農事服務業格式檔==  */     
INFILE 'D:\D\大電腦(SAS)程式及備份\sas程式\99年農事服務業中間表(電腦程式及原始資料部分)\raw data\ed2後第20檢檔尾NEW-HHLEE代入新村里7碼'   lrecl=511;  
INPUT     @1      HUKEY   $12.
                 @123	    a 	1.@;
            do i=1 to 15;
  input	      D082011	1. 
                  D082012	1.
                  D082013	1.
                  D083011	8.
                  D084011	2.@;
        if  D084011>0 then output;
    end;


 data  prog2.agr_service942;    
 set prog2.agr_service941;    
ARRAY COL{1:50} COL1-COL50;
     DO  IX=1 TO 50;
     COL{IX}=0;
     END;
if  D084011=1  then COL1=1;
if  D084011=2  then COL2=1;
if  D084011=3  then COL3=1;
if  D084011=4  then COL4=1;
if  D084011=5  then COL5=1;
if  D084011=6  then COL6=1;
if  D084011=7  then COL7=1;
if  D084011=8  then COL8=1;
if  D084011=9  then COL9=1;
if  D084011=10  then COL10=1;
if  D084011=11 then COL11=1;
if  D084011=12 then COL12=1;
if  D084011=13  then COL13=1;
if  D084011=14  then COL14=1;
if  D084011=15  then COL15=1;
if  D084011=16  then COL16=1;

PROC  SUMMARY DATA=prog2.agr_service942;  
              CLASS    i;
              VAR COL1-COL16 ;
			  OUTPUT  OUT=prog2.agr_service943   SUM=ACOL1-ACOL16 ;


DATA prog2.agr_service944;                           
ARRAY ARR1(16)  ACOL1-ACOL16;                                         
SET  prog2.agr_service943;
LENGTH MN1 $20;      
Q1=0; Q2=0; Q3=0;  MN1=''; MN2=0; 
do  i=1 to 16;
IF Q1<ARR1(i) then  DO; Q1=ARR1(i);MN1=put(i,2.);end;
END;      

do  i=1 to 16;
IF Q2<ARR1(i)<Q1 then DO; Q2=ARR1(i);MN2=i;end; 
if i=16  then mn1=trim(mn1)!!''!!put(MN2,2.);
END;     

do  i=1 to 16;
IF Q3<ARR1(i)<Q2 then DO; Q3=ARR1(i);MN2=i;end; 
 if i=16  then mn1=trim(mn1)!!''!!put(MN2,2.);
END;     


PROC PRINT DATA=prog2.agr_service944  NOOBS LABEL; 
TITLE  '  --表16--' ;
  var    i   ACOL1-ACOL16  MN1;
  run;
 

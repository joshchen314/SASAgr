/*定義資料名稱及來源*/
libname D104 '\\Saseg\D54\D54B\1_lib\104CENSUS_LIB\';
data T01;/*-------99年 表側------*/
set D104.HUS_temp;
if hs in ('71','72') then do;  /*將資料按地區及縣市分類：area1, area2*/
      area1=2;
      area2=0;
end;
else do;
       area1=1;
	   if hs in ('65','63','17','18','02','03','04') then area2=1;
	   if hs in ('66','05','07','08','09') then area2=2;
	   if hs in ('67','64','20','10','13','16') then area2=3;
	   if hs in ('14','15') then area2=4;
end;

IF D055001 IN (1,2,3,4,5,6,7,8,9,10,11,12) THEN D055001_1=1; /*主要服務項目：D055001_1*/
IF D055001 IN (13,14,15,16) THEN D055001_1=2;

   IF D020001<34 THEN yr=1; /*開業年份：yr*/  /*34年以前*/
   IF D020001=34 THEN yr=2; /*34*/
   DO i=1 TO 4; /*45,55,65,75*/
     	IF 35+(I-1)*10<=D020001<45+(I-1)*10 THEN yr=1+i;
   END; /*80,85,90,95*/
   DO i=1 TO 5; 
     IF 75+(I-1)*5<=D020001<80+(I-1)*5 THEN yr=5+i;
   END; 

PL=D072011+D072012+D072021+D072022+D072031+D072032;/*從業員工人數= 常僱+臨時+不支薪：PL*/
CPL=0;/*從業員工人數*/
   IF PL IN (1,2,3,4,5,6,7,8,9) THEN CPL=PL;
   IF 1<=PL<5 THEN CPL_2=1;/* 1-4 */
   IF 5<=PL<10 THEN CPL_2=2;
IF 10<=PL<=39 THEN DO;/* 10-39 */
   IF 10<=PL<=14 THEN CPL_2=3;
   IF 15<=PL<=19 THEN CPL_2=4;
   IF 20<=PL<=29 THEN CPL_2=5;
   IF 30<=PL<=39 THEN CPL_2=6;
END;
   IF 40<=PL THEN CPL_2=7; /* 11 */

   YY=104-D032001;/*管理者年齡：YY*/
DO I=1 TO 11;
   IF 15+(I-1)*5<=YY<=19+(I-1)*5 THEN YOY=I;
END;
IF 70<=YY THEN YOY=12;

MMY=0;
IF 20<=D080001<50 THEN MMY=1;/*revenues*/
IF 50<=D080001<100 THEN MMY=2;
IF 100<=D080001<500 THEN DO;
  DO i=1 TO 4; 
     IF 100+(I-1)*100<=D080001<200+(I-1)*100 THEN MMY=2+I;
  END; END;
IF 500<=D080001<2000 THEN DO;
  DO j=1 TO 3; 
     IF 500*j<=D080001<500*(j+1) THEN MMY=6+j;
  END;END; 
IF 2000<=D080001<3000 THEN MMY=10;
IF 3000<=D080001<5000 THEN MMY=11;
IF 5000<=D080001 THEN MMY=12;RUN;


PROC SORT NODUPKEY; BY hukey; run;


/*----------------------------------------------------   列印巨集  ------------------------------------------------------*/
%MACRO A1(B,S,N,O);
PROC SUMMARY DATA=T01;
/*地區*/
   CLASS AREA1 AREA2 HS;
   VAR COL&B - COL&S;
 OUTPUT OUT=T&N._S&O  SUM=SCOL&B - SCOL&S;%MEND A1;

%MACRO A2(B,S,N,O);
PROC SUMMARY DATA=T01;
/*組織*/
   CLASS D010001;
   VAR COL&B - COL&S;
 OUTPUT OUT=T&N._S&O  SUM=SCOL&B - SCOL&S;%MEND A2;

%MACRO A3(B,S,N,O);
PROC SUMMARY DATA=T01;
/*性別 年齡*/
   CLASS D031001 yr;
   VAR COL&B - COL&S;
 OUTPUT OUT=T&N._S&O  SUM=SCOL&B - SCOL&S;%MEND A3;

%MACRO A4(B,S,N,O);
PROC SUMMARY DATA=T01;
/*主要服務*/
   CLASS D055001 D055001_1;
   VAR COL&B - COL&S;
 OUTPUT OUT=T&N._S&O  SUM=SCOL&B - SCOL&S;%MEND A4;

%MACRO A5(B,S,N,O);
PROC SUMMARY DATA=T01;
/*員工*/
   CLASS CPL CPL_2;
   VAR COL&B - COL&S;
 OUTPUT OUT=T&N._S&O  SUM=SCOL&B - SCOL&S;%MEND A5;

%MACRO A6(B,S,N,O);
PROC SUMMARY DATA=T01;
/*金額*/
   CLASS MMY;
   VAR COL&B - COL&S;
 OUTPUT OUT=T&N._S&O  SUM=SCOL&B - SCOL&S;%MEND A6;
 
%MACRO A7(B,S,N,O);
PROC SUMMARY DATA=T01;
/*開業年份*/
   CLASS YR;
   VAR COL&B - COL&S;
 OUTPUT OUT=T&N._S&O  SUM=SCOL&B - SCOL&S;%MEND A7;
 
 %MACRO A8(B,S,N,O);
PROC SUMMARY DATA=T01;
/*性別 教育*/
   CLASS D031001 D033001;
   VAR COL&B - COL&S;
 OUTPUT OUT=T&N._S&O  SUM=SCOL&B - SCOL&S;%MEND A8;
 
 %MACRO A9(B,S,N,O);
PROC SUMMARY DATA=T01;
/*年齡*/
   CLASS yr;
   VAR COL&B - COL&S;
 OUTPUT OUT=T&N._S&O  SUM=SCOL&B - SCOL&S;%MEND A9;
/*----------------------------------------------------   列印巨集  ------------------------------------------------------*/

 
data T01;/*-------40101 農事及畜牧服務業家數按經營組織型態分  ---99年 表 1------*/
set T01;

 ARRAY dummys01{6} COL2 - COL7; 
 DO i=1 TO 6; 
 dummys01(i) = 0; 
 END; 
if 0<D010001<7 then dummys01(D010001) = 1;
COL1=COL2+COL3+COL4+COL5+COL6+COL7; 
%A1(1,7,01,1);
%A4(1,7,01,2);
run;

data T01;/*-------40102 農事及畜牧服務業之從業員工人數按月份及性別分  ---99年 表 2------*/
set T01;
IF D020001<34 THEN D020001_1=1;
IF D020001=34 THEN D020001_1=2;
DO i=1 TO 4; 
	IF 35+(I-1)*10<=D020001<45+(I-1)*10 THEN D020001_1=1+i;
END; 
DO j=1 TO 6; 
	IF 75+(j-1)*5<=D020001<80+(j-1)*5 THEN D020001_1=5+j;
END; 
ARRAY dummys02{11} COL9 - COL19; 
	DO h=1 TO 11; 
		dummys02(h) = 0; 
	END; 
dummys02(D020001_1) = 1;
	COL8=SUM(OF COL9 - COL19);
%A1(8,19,02,1);
%A2(8,19,02,2);
%A3(8,19,02,3);
%A8(8,19,02,4);
%A4(8,19,02,5);


/*-------40103  各服務項目家數按服務對象地區分  ---99年 表 11------*/
%MACRO H03(startnum,stopnum);
%DO N=&startnum %TO &stopnum;
  data T01;    set T01;
    col85=0;col86=0;col87=0;t=0;
    if 0<&N<10 then do;
        if D0520&N.1=1 then col85=1;if D0520&N.2=1 then col86=1;if D0520&N.3=1 then col87=1;
        if D0520&N.1+D0520&N.2+D0520&N.3>0 then t=1;    end;
    else do;
        if D052&N.1=1 then col85=1;if D052&N.2=1 then col86=1;if D052&N.3=1 then col87=1;
        if D052&N.1+D052&N.2+D052&N.3>0 then t=1;    end;

    PROC SUMMARY DATA=T01;
    VAR t COL85-col87;
    OUTPUT OUT=zT03_S&N  SUM= COL84 COL85-col87;
%END;      
%MEND H03;  
     
%h03(1,16);

data T03_S1;    
set zT03_S1 - zT03_s16;    
run;


/*-------40104  各服務項目家數按主要服務農畜種類分   ---99年 表 6------------------------------------------*/
%MACRO H04(startnum,stopnum);
%DO N=&startnum %TO &stopnum;
    data T01;    set T01;
	ARRAY dummys03{14} COL70 - COL83; 
		DO i=1 TO 14; 
			dummys03(i) = 0; 
		END;
    IF &N=1 THEN DO;
		if D054011=1 then col70=1; 
		t=sum(of col70-col83); END;
    IF &N=2 THEN DO;
		if D054021=4 & D053021>0 then col73=1; 
		if D054021=7 & D053021>0  then col76=1; 
		t=sum(of col70-col83); END;
    IF &N=3 THEN DO;
		if D053031>0 then col75=1; 
		t=sum(of col70-col83); END;
    IF &N=4 THEN DO;
		if D054041=2 & D053041>0 then col71=1; 
		if D054041=3 & D053041>0  then col72=1; 
		if D054041=5 & D053041>0  then col74=1; 
		if D054041=8 & D053041>0  then col77=1; 
		t=sum(of col70-col83); END;
    if &N=5 then do;
		if D0540&N.1=1 then col70=1;
		if D0540&N.1=2 then col71=1;
		if D0540&N.1=3 then col72=1;
		if D0540&N.1=4 then col73=1;
		if D0540&N.1=5 then col74=1;
		if D0540&N.1=7 then col76=1;
		if D0540&N.1=8 then col77=1;
		t=sum(of col70-col83); END;
    if &N=6 then do;
		if D054061=6 then col75=1; 
		t=sum(of col70-col83); END;
    if &N=7 then do;
		if D0540&N.1=1 then col70=1;
		if D0540&N.1=2 then col71=1;
		if D0540&N.1=3 then col72=1;
		if D0540&N.1=4 then col73=1;
		if D0540&N.1=5 then col74=1;
		if D0540&N.1=7 then col76=1;
		if D0540&N.1=8 then col77=1;
		t=sum(of col70-col83); END;
    if 8<=&N<=12 then do;
		IF 8<=&N<=9 THEN DO;
			if D0540&N.1=1 then col70=1;
			if D0540&N.1=2 then col71=1;
			if D0540&N.1=3 then col72=1;
			if D0540&N.1=4 then col73=1;
			if D0540&N.1=5 then col74=1;
			if D0540&N.1=6 then col75=1;
			if D0540&N.1=7 then col76=1;
			if D0540&N.1=8 then col77=1;
			t=sum(of col70-col83); END;	
		ELSE DO;
			if D054&N.1=1 then col70=1;
			if D054&N.1=2 then col71=1;
			if D054&N.1=3 then col72=1;
			if D054&N.1=4 then col73=1;
			if D054&N.1=5 then col74=1;
			if D054&N.1=6 then col75=1;
			if D054&N.1=7 then col76=1;
			if D054&N.1=8 then col77=1;
			t=sum(of col70-col83); END;	END;
    if &N=13 then do;
		if D054&N.1=9 then col78=1;
		if D054&N.1=10 then col79=1;
		if D054&N.1=11 then col80=1;
		if D054&N.1=12 then col81=1;
		if D054&N.1=13 then col82=1;
		if D054&N.1=14 then col83=1;
		t=sum(of col70-col83); END;
    if &N=14 then do;
		if D054&N.1=9 then col78=1;
		if D054&N.1=10 then col79=1;
		if D054&N.1=11 then col80=1;
		t=sum(of col70-col83); END;
    if 15<=&N<=16 then do;
		if D054&N.1=12 then col81=1;
		if D054&N.1=13 then col82=1;
		if D054&N.1=14 then col83=1;
		t=sum(of col70-col83); END;
PROC SUMMARY DATA=T01;
VAR t COL70-col83;
OUTPUT OUT=zT04_S&N  SUM=COL69 COL70-col83;
   %END;   
%MEND H04;       
%h04(1,16);
data T04_S1;
set zT04_S1 - zT04_s16
;run;


data T01;/*-------40105  農事及畜牧服務業家數按主要服務項目分   ---99年 表 5------*/
set T01;
if 1<=D055001<=12 then do;
 	ARRAY dummys04{12} COL52 - COL63; 
 		DO i=1 TO 12; 
 			dummys04(i) = 0; 
 		END;
		dummys04(D055001) = 1;
end;
else if 13<=D055001<=16 then do;
 	ARRAY dummys05{4} COL65 - COL68; 
 		DO j=13 TO 16; 
 			dummys05(j) = 0; 
 		END; 
		dummys05(D055001) = 1; 
end;
COL50=0; COL51=0; COL64=0; 
COL51=SUM(OF COL52-COL63);
COL64=SUM(OF COL65-COL68); 
COL50=SUM(COL51,COL64); 

%A1(50,68,05,1);
%A3(50,68,05,2);
%A8(50,68,05,3);
%A5(50,68,05,4);
%A6(50,68,05,5);

run;


data T01;/*-------40106 農事及畜牧服務業家數按年底從業員工人數分   ---99年 表 3------*/
set T01;
ppl=0;
IF 1<=pl<5 THEN COL21=1;
	IF pl=1 THEN COL22=1;
	IF pl=2 THEN COL23=1;
	IF pl=3 THEN COL24=1;
	IF pl=4 THEN COL25=1;
IF 5<=pl<10 THEN COL26=1;
	IF pl=5 THEN COL27=1;
	IF pl=6 THEN COL28=1;
	IF pl=7 THEN COL29=1;
	IF pl=8 THEN COL30=1;
	IF pl=9 THEN COL31=1;
IF 10<=pl<15 THEN COL32=1;
IF 15<=pl<20 THEN COL33=1;
IF 20<=pl<30 THEN COL34=1;
IF 30<=pl<40 THEN COL35=1;
IF 40<=pl THEN COL36=1;

COL20=SUM(COL21,COL26,SUM(OF COL32 - COL36));

%A1(20,36,06,1);
%A2(20,36,06,2);
%A7(20,36,06,3);
%A3(20,36,06,4);
%A8(20,36,06,5);
%A4(20,36,06,6);
RUN;


data T01;/*-------40107  農事及畜牧服務業家數按農事及畜牧服務總金額分   ---99年 表 4------*/
set T01;
COL37=0;

  ARRAY dummys07{12} COL38 - COL49; 
  DO K=1 TO 12; 
  	dummys07(K) = 0; 
  END; 

if  0<MMY<13 then  dummys07(MMY) = 1;

COL37=SUM(OF COL38 - COL49);
  
%A1(37,49,07,1);
%A2(37,49,07,2);
%A7(37,49,07,3);
%A3(37,49,07,4);
%A8(37,49,07,5);
%A4(37,49,07,6);
%A5(37,49,07,7);
RUN;

data T01;/*-------40201  農事及畜牧服務業之全年作業數量按服務項目分   ---99年 表 10------*/
set T01;
%MACRO H08(startnum,stopnum);
%DO i=&startnum %TO &stopnum;
    %LET J=%EVAL(2*(&I-1)+102);                                         /* IMPORTANT*/
    %LET K=%EVAL(2*(&I-1)+103);                                        /* IMPORTANT*/
    IF 1<=&i<=9 then do;
		IF D0530&i.1>0 THEN COL&J=1;	ELSE COL&J=0;
		COL&K=D0530&i.1;    
	end;
    else do;
		IF D053&i.1>0 THEN COL&J=1;   ELSE COL&J=0;
		COL&K=D053&i.1;
	end;
%END;
COL101=1;
%MEND H08;    

%h08(1,16);

%A1(101,133,08,1);
%A2(101,133,08,2);
%A3(101,133,08,3);
%A8(101,133,08,4);
%A5(101,133,08,5);
%A6(101,133,08,6);
RUN;

data T01;/*-------40301農事及畜牧服務用地及房舍面積    ---99年 表 8------*/
set T01;
IF D040001>0 THEN COL142=1;ELSE COL142=0;
COL143=D040001*0.01;
IF D040002>0 THEN COL144=1;ELSE COL144=0;
COL145=D040002*0.01;
IF D040003>0 THEN COL146=1;ELSE COL146=0;
COL147=D040003*0.01;
IF D040004>0 THEN COL148=1;ELSE COL148=0;
COL149=D040004;
IF D040005>0 THEN COL150=1;ELSE COL150=0;
COL151=D040005;
IF D040006>0 THEN COL152=1;ELSE COL152=0;
COL153=D040006;
IF D040007=1 THEN COL154=1;
IF SUM(COL142, COL144, COL146, COL148, COL150, COL152)>0 THEN COL141=1;
COL140=COL141+COL154;
%A1(140,154,09,1);
%A2(140,154,09,2);
%A4(140,154,09,3);
%A5(140,154,09,4);
%A6(140,154,09,5);
RUN;
 
data T01;/*-------40401 農事及畜牧服務業之農業機械數量   ---99年 表 9------*/
set T01;
ARRAY dm40401{61} COL160 - COL220; 
	DO M=1 TO 61; 
		dm40401(M) = 0; 
	END;
%MACRO H10(startnum,stopnum);
%DO i=&startnum %TO &stopnum;
    %LET J=%EVAL(2*(&I-1)+161);                                         /* IMPORTANT*/
    %LET K=%EVAL(2*(&I-1)+162);                                        /* IMPORTANT*/
    IF 1<=&i<=9 then do;
		IF D06000&i>0 THEN COL&J=1;ELSE COL&J=0;
		COL&K=D06000&i;    end;
    else do;
		IF D0600&i>0 THEN COL&J=1;ELSE COL&J=0;
		COL&K=D0600&i;    end;
%END;
%MEND H10;       

%h10(1,30);

IF D060031=0 THEN COL160=1;ELSE COL160=0;

%A1(160,220,10,1);
%A2(160,220,10,2);
%A7(160,220,10,3);
%A4(160,220,10,4);
%A5(160,220,10,5);
%A6(160,220,10,6);
RUN;


data T01;/*-------40501農事及畜牧服務業經營管理者按性別及教育程度分    ---        ------*/
set T01;
if D033001>0 then COL201=1;
ARRAY dm40501{5} COL222 - COL226; 
	DO i=1 TO 5; 
		dm40501(i) = 0; 
	END;
 if 0<D033001<6 then dm40501(D033001) = 1; 
 COL221=SUM(OF COL222-COL226);
 
IF D031001=1 THEN DO;
	if D033001>0 then COL207=1;
	ARRAY dm40501M{5} COL228 - COL232; 
		DO J=1 TO 5; 
			dm40501M(J) = 0; 
		END;
 	if 0<D033001<6 then dm40501M(D033001) = 1; 
	COL227=SUM(OF COL228-COL232);
	END;
ELSE IF D031001=2 THEN DO;
	if D033001>0 then COL213=1;
	ARRAY dm40501F{5} COL234 - COL238; 
		DO K=1 TO 5; 
			dm40501F(K) = 0; 
		END;
 	if 0<D033001<6 then dm40501F(D033001) = 1; 
	COL233=SUM(OF COL234-COL238);
	END;
	
%A1(221,238,11,1);
%A2(221,238,11,2);
%A4(221,238,11,3);
%A9(221,238,11,4);
RUN;


data T01;/*-------40502農事及畜牧服務業之從業員工按月份及性別分     ---         ------*/
set T01;
ARRAY dm40502{39} COL241 - COL279; 
	DO i=1 TO 39; 
		dm40502(i) = 0; 
	END;
	
%MACRO H40502(startnum,stopnum);
%DO i=&startnum %TO &stopnum;
		%LET J=%EVAL(3*(&I-1)+245);                                        /* IMPORTANT*/
		%LET K=%EVAL(3*(&I-1)+246);                                        /* IMPORTANT*/
		%LET L=%EVAL(3*(&I-1)+244);                                        /* IMPORTANT*/
IF 1<=&i<=9 then do;
	COL&J=D07110&i;
	COL&K=D07120&i;
	COL&L=COL&J+COL&K;
END;
ELSE DO;
	COL&J=D0711&i;
	COL&K=D0712&i;
	COL&L=COL&J+COL&K;
END;
%END;
%MEND H40502;
%H40502(1,12);

COL242=COL245+COL248+COL251+COL254+COL257+COL260+COL263+COL266+COL269+COL272+COL275+COL278;
COL243=COL246+COL249+COL252+COL255+COL258+COL261+COL264+COL267+COL270+COL273+COL276+COL279;
COL241=COL242+COL243;

%A1(241,279,12,1);
%A2(241,279,12,2);
%A4(241,279,12,3);
%A6(241,279,12,4);
RUN;



data T01;/*-------40503 農事及畜牧服務業之年底從業員工按僱用性質及性別分    ---99年 表 7------*/
set T01;
%MACRO H13(startnum,stopnum);
ARRAY dm40503{12} COL281 - COL292;	
	DO i=1 TO 12;		
			dm40503(i) = 0;	
	END;

%DO i=&startnum %TO &stopnum;
		%LET J=%EVAL(3*(&I-1)+285);                                         /* IMPORTANT*/
		%LET K=%EVAL(3*(&I-1)+286);                                        /* IMPORTANT*/
		%LET L=%EVAL(3*(&I-1)+284);                                        /* IMPORTANT*/
	COL&J=D0720&i.1;
	COL&K=D0720&i.2;
	COL&L=COL2&J+COL&K;
	COL282=COL282+D0720&i.1;
	COL283=COL283+D0720&i.2;
	COL281=COL281+D0720&i.1+D0720&i.2;
%END;
%MEND H13;
%h13(1,3);

%A1(281,292,13,1);
%A2(281,292,13,2);
%A4(281,292,13,3);
%A6(281,292,13,4);

RUN;

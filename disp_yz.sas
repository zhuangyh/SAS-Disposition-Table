/*====================================================================
| COMPANY           Bancova LLC
| PROJECT:          BANCOVA SAS TRAINING
|                   HOMEWORK#3
|
| PROGRAM:          Disposition_zyh.SAS
| PROGRAMMER(S):    Yonghua Zhuang
| DATE:             07/12/2016
| PURPOSE:          Generate Disposition Table
|                        
| QC PROGRAMMER(S): 
| QC DATE:          
|
| INPUT FILE DIRECTORY(S): C:\bancova2016summer\data\raw      
| OUTPUT FILE DIRECTORY(S):C:\bancova2016summer\data\output       
| OUTPUT AND PRINT SPECIFICATIONS: Disposition.RTF    
|
|
| REVISION HISTORY
| DATE     BY        COMMENTS
|
|
=====================================================================*/

libname raw "C:\bancova2016summer\data\raw" access=readonly;
libname derived "C:\bancova2016summer\data\derived";
%let outdir=C:\bancova2016summer\data\output;

*****************************************;
*              Import Data              *;
*****************************************;
%macro import(xlsfile);
proc import out= work.&xlsfile 
            datafile= "C:\bancova2016summer\data\raw\&xlsfile..xls" 
            dbms=XLS replace;
run;

%mend import;
%import(termination); quit;

%import(Demog_data); quit;

proc contents data= termination;
run;
data termination;
	set termination;
	where subjid^=.;
run;
proc print data= termination;
run;

** Find duplicate observations **;
%macro dupfind (source,subjdups);
Proc sort data=&source out=derived.&source;
		by subjid;
run;
data &source &subjdups;
     set derived.&source;
     by subjid;
     if not (first.subjid and last.subjid) then output &subjdups;
     if last.subjid then output &source;
run;
%mend dupfind;
%dupfind(Demog_data, subjdup);
%dupfind(termination, temdup);

*** add enroll to keep up with the data set - John 03 14 09***;
data Demog_data;
  set Demog_data;
	call streaminit(123);
  	enroll= ifn(rand("uniform") <= 0.1, 0, 1);
  	call streaminit(456);
 	 ppp = ifn(rand("uniform") <= 0.1, 0, 1);
  output;
run;
proc contents data=temdup;
run;
proc print data=temdup;
run;
proc print data=Demog_data;
run;


*** Find missing values for the analysis variables  ***;
*** Find illogical values (inconsistent termdate)   ***;
data illogic;
	set termination;
	termdate1=input(termdate, yymmdd8.);
	termdate2=mdy(termdt_mm,termdt_dd,termdt_yy);
	if complete=1 and termdate1 ne termdate2 then output illogic;
	else if complete=0 and termdate1 lt termdate2 then output illogic;
 	else;
run;

proc print data = illogic;
run;
data final1 missing;
   merge Demog_data (in=in1) termination (keep=subjid complete disrsn in=in2);
   by subjid;
   if in1 and in2 then output final1;
   array check1{*} _NUMERIC_;
   do i1 = 1 to dim(check1);
      if missing(check1{i1}) then output missing;
   end;
   drop i1;
   array check2{*} _CHARACTER_;
   do i2 = 1 to dim(check2);
      if missing(check2{i2}) then output missing;
   end;
run ;


proc print data=final1;
run;

*************************************;
* Caculate Pvalue results ;
*************************************;

%macro pvalue(input, variable, output);
	Proc freq data=&input noprint;
		tables treatmnt*&variable /FISHER expected norow nocol nopercent;
		output out =&output FISHER;
	run;
%mend;


Proc freq data=final1 ;
		tables treatmnt*enroll /chisq expected norow nocol nopercent;
	run;


%pvalue(final1, enroll, enroll0);
%pvalue(final1, ppp, ppp0);
%pvalue(final1, itt, itt0);
%pvalue(final1, safety, safety0);
%pvalue(final1, complete, complete0);
%pvalue(final1, disrsn, disrsn0);


**************************************;
* Part A                              ;
* Normalize the data.                 ;
**************************************;
  
 proc transpose data=final1 out=final2 (drop=_label_ rename=(col1=result)) ;
   by subjid treatmnt;
   var enroll ppp itt safety complete disrsn;
 run;
proc print data=final2; 
title "after transpose";
run;
title;
**************************************;
* Create a numeric ordering variable. ; 
* for table presention.               ;
**************************************;
        
proc format;
         invalue orderi
          'enroll'= 1
          'ppp'= 2
          'itt'= 3
          'safety'= 4
	   	  'complete'= 5
          'disrsn'= 6
 ;
run;
     
data final3 ;
 set final2 ;
 order = input(_name_,orderi.) ;
run ;
 
proc print data=final3 ;
title2 'Normalized Data' ;
run;
*** duplicate the data set for total column;
data final3;
	set final3;
	output;
	treatmnt=3;
	output;
run;
proc sort data=final3;
       by order treatmnt;
run; 

Proc print data=final3;
run;
 
************************************************; 
* Part B                                        ;
* Categorical Data                              ;
************************************************;

proc summary data=final3 missing;
   by order treatmnt;
   class result;
   output out=cat1(rename= (_freq_=count)) ;
 run ;  

proc print data = cat1;
run; 
*********************************;
* Format results into N (%) form ;
*********************************;
     
data cat2 ;
	set cat1 ;
 	by order treatmnt;
retain denom 0;
length pctcol $15. ;
 if _type_=0 then do ;
    denom=count;
    delete;
    end ;
  ncount=put(count,3.) ;
  npercent=put(100*count/denom,3.) ;
 if count=0 then pctcol=ncount;
 else if count ^=0 then pctcol=right(ncount)||' ('||npercent||'%)' ;
run;

proc print data=cat2;
run;
*************************************;
* Put treatment results into columns ;
*************************************;
     
proc sort data=cat2;
  by order result;
run ;

proc transpose data=cat2 out=cat3 (drop= _name_ ) prefix=trt ;
 by order result;
 var pctcol ;
 id treatmnt;
run;
proc print data=cat3;
run;

*********************************;
* Format row labels for the table;
*********************************;

proc format;
     value disrsn
	     1='^R"    "Patient withdrew consent'
		 2='^R"    "Protocol violation'
		 3='^R"    "Lost to follow up'
		 4='^R"    "Adverse Event'
		 ;
run;
proc format;
	value order
		1='Enrolled Population     (a)  n (%)' 
		2='Per-Protocol Population  (b)  n (%)'
		3='ITT Population (c)'
		4='Safety Population (d)';
	value compl
		1='Patients Completed'
		0='Patients Discontinued'
		;
run;

data cat4;
	set cat3(where=(order<5 and result=1));
	length value $80;
	value=put(order,order.);
run;

data pvalue;
	set enroll0 (in = a)
		ppp0 	(in = b)
		itt0	(in = c)
		safety0 (in = d)
		complete0 (in = e)
		disrsn0 (in = f);
	keep order v3;
	order = (a * 1)+ (b * 2) + (c * 3) +(d * 4) +(e * 5) + (f * 6);
	if a then do;
		v3 = XPL_FISH;  *XP2_FISH will be better than XPL_FISH;
	end;				*Not choose XP2_FISH due to every p is eqal to 1;

	if b then do;
		v3 = XPL_FISH;
	end;

	if c then do;
		v3 = XPL_FISH;
	end;

	if d then do;
		v3 = XPL_FISH;
	end;

	if e then do;
		v3 = XPL_FISH;
	end;

	if f then do;
		v3 = XP2_FISH;
	end;
		
run;

Proc print data = pvalue; run;

data cat4;
	 merge cat4(in=c1) pvalue(rename=(v3=pvalue) in=p1);
	 by order;
	 if c1 and p1;
run;

proc print data=cat4;
run;



data cat5;
	set cat3(where=(order=5));
	value=put(result,compl.);
run;
proc sort data=cat5;
	by decending result;
run;
data cat5;
	 merge cat5(in=c1) pvalue(rename=(v3=pvalue) in=p1);
	 by order;
	 if c1 and p1;
run;

proc print data=cat5;
run;
data cat6;
	set cat3(where=(order=6));
	value=put(result,disrsn.) ;
	if result ne .;
run;
proc print data=cat6;
run;
data cat6;
	set cat6;
	array ckmiss{3} trt1 trt2 trt3;
    do i = 1 to 3;
      if ckmiss{i} = " " then ckmiss{i} = "0";
    end;
	drop i;
run;
data fstrow_cat6;
	order=input('disrsn', orderi.);
	value='Primary Reason for Discontinuation of Study Dose';
run;

data fstrow_cat6;
	 merge fstrow_cat6(in=c1) pvalue(rename=(v3=pvalue) in=p1);
	 by order;
	 if c1 and p1;
run;
proc print data=fstrow_cat6;
run;

data final4 (drop=result);
	set cat4 cat5 fstrow_cat6 cat6;
run;

proc print data=final4;
run;

* blank row after order (1-4) change;
Data final4; 
    Set final4; 
    Output; 
   	if order <5 then do;
        Call missing( of _all_ ) ; 
        Output ; 
   	end; 
Run ; 

proc print data=final4;
run;

*** create styles used for RTF-output ***;
libname formats "C:\bancova2016summer\data\formats";
ods path formats.myRTFstyles(update) 
         sashelp.tmplmst(read); 

proc template;
    DEFINE STYLE PANDA;
	PARENT= Styles.sasdocprinter;
	style fonts FROM FONTS /
		'titleFont' = ("courier new",12pt)
		'titleFont2'= ("courier new", 8pt)
		'headingFont' = ("times roman",10pt, bold)                            
        'docFont' = ("times roman",10pt);	
	style SystemFooter from systemfooter/
		font=fonts('titleFont2');
	replace body/
		bottommargin = 1in                                                
        topmargin = 1.5in                                                   
        rightmargin = 1in                                                 
        leftmargin = 1in; 	
	style TABLE from table/
		cellpadding=0
		cellspacing=0
		OUTPUTWIDTH=95%
		BORDERWIDTH=2PT;
	END;
run;


***** write table to RTF using proc report*****;
ods listing close;
options nodate nonumber orientation=landscape missing='';
ods escapechar='^';
ods rtf file = "C:\bancova2016summer\data\derived\disposition.rtf" style=PANDA; 
proc report data=final4 nowindows missing headline headskip split='|';
	column value trt1-trt3 pvalue;
	
	define value/ ' ';
	define trt1/ center 'Anticancer000| | ';
	define trt2/ center 'Anticancer001| | ';
	define trt3/ center 'Total| | ';
	define pvalue/ Format = 6.3 center 'p-Value (e)| | ';
	
	title1  h=8pt j=l "Bancova_disposition_Homework" j=r "page^{pageof}";
	title2 "Table 2";
  	title3 "Patient Disposition";
  	title4 "(All Patients)";
	footnote1 "^R'\brdrb\brdrs\brdrw1";
	footnote2 "Disposition_yz.sas  submitted  &sysdate9. at  &systime by Yonghua Zhuang";
run ; 
%let text= %str(^S={just=l font=('courier new',8pt)}); 
	ods rtf text=' ';
	ods rtf text="&text Notes:";
	ods rtf text="&text a)  Enrolled population includes all patients who signed the informed consent.";
	ods rtf text="&text b)  Per Protocol population is defined as a subset of the ITT population with a list of criteria met.";
	ods rtf text="&text c)  ITT is defined as including all patients who have the baseline value and at least one post-baseline value for the primary efficacy measure.";
	ods rtf text="&text d)  Safety population is defined as including all patients who took any study medication.";
	ods rtf text="&text e)	P values were obtained using Fisher exact test.";

ods rtf close;
ods listing;
ods path reset;

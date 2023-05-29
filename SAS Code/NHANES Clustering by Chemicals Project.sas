libname DATA 'C:\NHANES\DATA';
libname WORKING 'C:\NHANES\WORKING DATA\SVI Project';

***Retained Variables and Sorted by Sequence Number***;
data bmx_i; set data.bmx_i;
keep SEQN BMDBMIC;
proc sort; by seqn;
run;

data huq_i; set data.huq_i;
keep SEQN HUQ010 HUQ030;
proc sort; by seqn;
run;

data svi; set data.svi;
keep SEQN svi;
proc sort; by seqn;
run;
data demo_i; set data.demo_i;
keep SEQN SDDSRVYR WTINT2YR SDMVPSU SDMVSTRA RIDSTATR RIDAGEYR RIDRETH3 RIAGENDR DMDHHSIZ DMDHREDU INDHHIN2;
proc sort; by seqn;
run;
data alb_cr_i; set data.alb_cr_i;
proc sort; by seqn;
run;
data amdgyd_i; set data.amdgyd_i;
proc sort; by seqn;
run;
data ecq_i; set data.ecq_i;
keep SEQN ECQ080 ECQ020;
proc sort; by seqn;
run;
data ethox_i; set data.ethox_i;
proc sort; by seqn;
run;
data formal_i; set data.formal_i;
proc sort; by seqn;
run;
data hiq_i; set data.hiq_i;
keep SEQN HIQ011;
proc sort; by seqn;
run;
data hoq_i; set data.hoq_i;
keep SEQN HOQ065;
proc sort; by seqn;
run;
data hsq_i; set data.hsq_i;
keep SEQN HSD010;
proc sort; by seqn;
run;
data dbq_i; set data.dbq_i;
keep seqn DBQ700;
proc sort; by seqn;
run;
data inq_i; set data.inq_i;
keep SEQN INDFMMPC ;
proc sort; by seqn;
run;
data mcq_i; set data.mcq_i;
keep SEQN MCQ010 MCQ300B MCQ050 MCQ040 MCQ220;
proc sort; by seqn;
run;
data pah_i; set data.pah_i;
proc sort; by seqn;
run;
data pernt_i; set data.pernt_i;
proc sort; by seqn;
run;
data uadm_i; set data.uadm_i;
proc sort; by seqn;
run;
data ucot_i; set data.ucot_i;
proc sort; by seqn;
run;
data uvoc_i; set data.uvoc_i;
proc sort; by seqn;
run;
data smqfam_i; set data.smqfam_i;
keep SEQN SMD470 SMD460;
proc sort; by seqn;
run;
data smqrtu_i; set data.smqrtu_i;
keep SEQN SMDANY;
proc sort; by seqn;
run;
*Merge Files 2015-2016;

data workdata1; merge bmx_i huq_i svi demo_i alb_cr_i amdgyd_i dbq_i ecq_i ethox_i formal_i hiq_i hoq_i hsq_i inq_i mcq_i pah_i pernt_i uadm_i ucot_i uvoc_i smqrtu_i smqfam_i; by seqn; run;

proc contents data=workdata1;
run;

***RECODE AND CLEAN DATA***;

***BMI - Child***;
***This had to be recoded because the expected cell count was too low for the chi-square. I re-coded underweight as "missing";
***And I collapsted "Obese" and "overweight";
data workdata1; set workdata1;
if BMDBMIC=1 then bmi4=0;
if BMDBMIC=2 then bmi4=0;
if BMDBMIC=3 then bmi4=1;
if BMDBMIC=4 then bmi4=1;
if BMDBMIC=. then bmi4=.;
*check to see if recoding worked correctly;
proc freq; tables BMDBMIC*bmi4/list;
run;


***General Health Condition for Use***;
***Collapsted "poor" and "fair" health because of expected cell counts less than 5 for the chi-sq;
data workdata1; set workdata1;
if HUQ010=1 then generalhealth5=0;
if HUQ010=2 then generalhealth5=1;
if HUQ010=3 then generalhealth5=2;
if HUQ010=4 then generalhealth5=3;
if HUQ010=5 then generalhealth5=3;
if HUQ010=7 then generalhealth5=.;
if HUQ010=9 then generalhealth5=.;
if HUQ010=. then generalhealth5=.;
proc freq; tables HUQ010*generalhealth5/list;
run;

***Routine Place to go for Healthcare***;
***This had to be recoded to collapse "yes" and "more than 1 place" because of "more than one place" had too few for the chi-square;
data workdata1; set workdata1;
if HUQ030=1 then routinecare3=0;
if HUQ030=2 then routinecare3=1;
if HUQ030=3 then routinecare3=0;
if HUQ030=7 then routinecare3=.;
if HUQ030=9 then routinecare3=.;
if HUQ030=. then routinecare3=.;
proc freq; tables HUQ030*routinecare3/list;
run;

***Sex***;
data workdata1; set workdata1;
if RIAGENDR=1 then sex2=0;
if RIAGENDR=2 then sex2=1;

if sex2=0 then sex2_1=0;
if sex2=1 then sex2_1=1;
*check to see if recoding worked correctly* *proc freq; *tables RIAGENDR*sex2/list;
run;

***Race***;
data workdata1; set workdata1;
*check to make sure sample of each category is enough;* proc freq; *tables RIDRETH3;
race6=RIDRETH3-1;

 if race6=0 then do; race6_1=0; race6_2=0; race6_3=0; race6_4=0; race6_5=0;end;
 if race6=1 then do; race6_1=1; race6_2=0; race6_3=0; race6_4=0; race6_5=0;end;
 if race6=2 then do; race6_1=0; race6_2=1; race6_3=0; race6_4=0; race6_5=0;end;
 if race6=3 then do; race6_1=0; race6_2=0; race6_3=1; race6_4=0; race6_5=0;end;
 if race6=5 then do; race6_1=0; race6_2=0; race6_3=0; race6_4=1; race6_5=0;end;
 if race6=6 then do; race6_1=0; race6_2=0; race6_3=0; race6_4=0; race6_5=1;end;
 run;

 *proc freq; *tables race6*race6_1*race6_2*race6_3*race6_4*race6_5/list;*run*;
*proc freq; *tables RIDRETH3*race6/list;
*run; 

***Smoking***;
data workdata1; set workdata1;
proc freq; tables SMD470;
run;

data workdata1; set workdata1;
If SMD460 in (777 999) then SMD460=.;
If SMD460 in (0) then chronicsmk2=0;
if SMD460 in (1 2 3) then chronicsmk2=1;

if chronicsmk2=0 then chronicsmk2_1=0;
if chronicsmk2=1 then chronicsmk2_1=1;

*proc freq; *tables SMD460*chronicsmk2;
*run;

data workdata1; set workdata1;
If SMDANY in (7 9) then SMDANY=.;
proc freq; tables SMDANY1;
run;

***Education***;
data workdata1; set workdata1;
if DMDHREDU in (7 9) then DMDHREDU=.;
if DMDHREDU=1 then edu5=0;
if DMDHREDU=2 then edu5=1;
if DMDHREDU=3 then edu5=2;
if DMDHREDU=4 then edu5=3;
if DMDHREDU=5 then edu5=4;

if edu5=0 then do; edu5_1=0; edu5_2=0; edu5_3=0; edu5_4=0;end;
if edu5=1 then do; edu5_1=1; edu5_2=0; edu5_3=0; edu5_4=0;end;
if edu5=2 then do; edu5_1=0; edu5_2=1; edu5_3=0; edu5_4=0;end;
if edu5=3 then do; edu5_1=0; edu5_2=0; edu5_3=1; edu5_4=0;end;
if edu5=4 then do; edu5_1=0; edu5_2=0; edu5_3=0; edu5_4=1;end;
*proc freq; *tables edu5*edu5_1*edu5_2*edu5_3*edu5_4/list;*run;

run;

*proc freq; *tables DMDHREDU*edu5/list;
*run;

***Income***;
data workdata1; set workdata1;
if INDFMMPC in (7 9) then INDFMMPC=.;
if INDFMMPC=1 then poverty3=0;
if INDFMMPC=2 then poverty3=1;
if INDFMMPC=3 then poverty3=2;

if poverty3=0 then do; poverty3_1=0; poverty3_2=0;end;
if poverty3=1 then do; poverty3_1=1; poverty3_2=0;end;
if poverty3=2 then do; poverty3_1=0; poverty3_2=1;end;
*proc freq; *tables poverty3*poverty3_1*poverty3_2/list;*run;
*proc freq; *tables INDFMMPC*poverty3/list;
run;

***Health Insurance***;
data workdata1; set workdata1;
if HIQ011 in (7 9) then HIQ011=.;
if HIQ011=1 then insurance2=0;
if HIQ011=2 then insurance2=1;

if insurance2=0 then insurance2_1=0;
if insurance2=1 then insurance2_1=1;
*proc freq; *tables HIQ011*insurance2/list;
run;

***Home Ownership***;
data workdata1; set workdata1;
if HOQ065 in (7 9) then HOQ065=.;
if HOQ065=1 then homeown3=0;
if HOQ065=2 then homeown3=1;
if HOQ065=3 then homeown3=2;

if homeown3=0 then do; homeown3_1=0; homeown3_2=0;end;
if homeown3=1 then do; homeown3_1=1; homeown3_2=0;end;
if homeown3=2 then do; homeown3_1=0; homeown3_2=1;end;

*proc freq; *tables HOQ065*homeown3/list;
run;

***General Health Status***;
data workdata1; set workdata1;
if HSD010 in (7 9) then HSD010=.;
if HSD010=1 then genhealth5=0;
if HSD010=2 then genhealth5=1;
if HSD010=3 then genhealth5=2;
if HSD010=4 then genhealth5=3;
if HSD010=5 then genhealth5=4;
*proc freq; *tables HSD010*genhealth5/list;
run;

***Genetics***;
data workdata1; set workdata1;
if MCQ300B in (7 9) then MCQ300B=.;
if MCQ300B=1 then genetics2=0;
if MCQ300B=2 then genetics2=1;

if genetics2=0 then genetics2_1=0;
if genetics2=1 then genetics2_1=1;
*proc freq; *tables MCQ300B*genetics2/list;
run;

***mom smoked when pregnant***;
data workdata1; set workdata1;
if ECQ020 in (7 9) then ECQ020=.;
if ECQ020=1 then smokepregn2=0;
if ECQ020=2 then smokepregn2=1;

if smokepregn2=0 then smokepregn2_1=0;
if smokepregn2=1 then smokepregn2_1=1;
*proc freq; *tables ECQ020*smokepregn2/list;
run;

***Asthma***;
data workdata1; set workdata1;
if MCQ010 in (7 9) then MCQ010=.;
if MCQ010=1 then asthma2=0;
if MCQ010=2 then asthma2=1;

if asthma2=0 then asthma2_1=0;
if asthma2=1 then asthma2_1=1;
*proc freq; *tables MCQ010*asthma2/list;
run;

***Asthma Attack***;
data workdata1; set workdata1;
if MCQ040 in (7 9) then MCQ040=.;
if MCQ040=1 then attack2=0;
if MCQ040=2 then attack2=1;

if attack2=0 then attack2_1=0;
if attack2=1 then attack2_1=1;
*proc freq; *tables MCQ040*attack2/list;
run;

***Asthma Emergency***;
data workdata1; set workdata1;
if MCQ050 in (7 9) then MCQ050=.;
if MCQ050=1 then emergency2=0;
if MCQ050=2 then emergency2=1;

if emergency2=0 then emergency2_1=0;
if emergency2=1 then emergency2_1=1;
*proc freq; *tables MCQ050*attack2/list;
run;

***Cancer***;
data workdata1; set workdata1;
if MCQ220 in (7 9) then MCQ220=.;
if MCQ220=1 then cancer2=0;
if MCQ220=2 then cancer2=1;

if cancer2=0 then cancer2_1=0;
if cancer2=1 then cancer2_1=1;
*proc freq; *tables MCQ220*cancer2/list;
run;

***Age***;
data workdata1; set workdata1;
proc univariate; var ridageyr;
run;

***CREATE A DOMAIN FOR COMPLEX SURVEY ANALYSIS CALLED 'INCLUDE'. Do not include people younger than 6, older than 19, and with a subsample A weight of zero.
Those with zero-value weights do not have laboratory tests according to NHANES, and their data can not be imputed with an output***;
data workdata1; set workdata1; 
if WTSA2YR>0 and ridageyr >=6 and ridageyr <=19 
then include=1; else include=0;
run;

proc means data = workdata1 n nmiss;
  *var _numeric_;
  var include;
RUN ;

***CREATE A PROJECT SUBSET FOR PRE-ANALYSIS OF LABORATORY DATA FOR VARIABLE SELECTION***
***Retain only the variables where <20% of the data is missing, and where <40% of the laboratory values were undetectable***;;
data prelaboratorysubdata1; set workdata1; 
keep include RIDAGEYR WTSA2YR sdmvstra sdmvpsu URDUCRLC LBDACRLC LBDGLYLC LBDEOALC LBDFORLC URDP01LC URDP02LC URDP03LC URDP04LC URDP06LC URDP10LC URDP25LC URDUP8LC URDNO3LC URDSCNLC URD4DALC URD6DALC URD4MALC URD5NALC URDPDALC
URDCOTLC URDHCTLC URDAAMLC URDAMCLC URDATCLC URDBMALC URDBPMLC URDCEMLC URDCYALC URDCYMLC URD1DCLC URD2DCLC URDDHBLC URDDPMLC URDGAMLC URDHEMLC
URDHPMLC URDHP2LC URDPM1LC URDPM3LC URDMADLC URD2MHLC URD34MLC URDMB1LC URDMB2LC URDMB3LC URDPHELC URDPHGLC URDPMALC URDPMMLC URDTCVLC;
run;

proc means data = prelaboratorysubdata1 n nmiss;
  *var _numeric_;
  var include;
RUN ;

proc freq data=prelaboratorysubdata1;
where include=1; 
run;

*dm 'odsresults; *clear';

***CREATE A PROJECT SUBSET OF THE FINAL VARIABLES CHOSEN FROM THE LABORATORY TABLES***;
data laboratorysubdata1; set workdata1; 
keep include SEQN RIDAGEYR WTSA2YR sdmvstra sdmvpsu URXCRS URXUCR URX2MH URX34M URXAAM URXAMC URXATC URXBMA URXBPM URXCEM URXCYM URXDHB URXHPM URXHP2
URXIPM1 URXIPM3 URXMAD URXMB3 URXPHG URXPMM URXUP8 URXNO3 URXSCN URXP01 
URXP02 URXP03 URXP04 URXP06 URXP10 URXP25 URXCOTT URXHCTT URX4MDA LBXFOR LBXEOA LBXACR; 
run;

proc means data = laboratorysubdata1 n nmiss;
  var _numeric_;
  where include=1;
RUN ;

***IMPUTE MISSING VALUES FOR LABORATORY DATA - k-means clustering relies on a complete dataset***;
proc surveyimpute data=laboratorysubdata1 seed=08192016; output out=imputed2;
var URXCRS URXUCR URX2MH URX34M URXAAM URXAMC URXATC URXBMA URXBPM URXCEM URXCYM URXDHB URXHPM URXHP2
URXIPM1 URXIPM3 URXMAD URXMB3 URXPHG URXPMM URXUP8 URXNO3 URXSCN URXP01 
URXP02 URXP03 URXP04 URXP06 URXP10 URXP25 URXCOTT URXHCTT URX4MDA LBXFOR LBXEOA LBXACR;
Strata sdmvstra;
Cluster sdmvpsu;
Weight WTSA2YR;
Run;

***confirm imputation worked***;
proc means data = imputed2 n nmiss;
  var _numeric_;
  where include=1;
RUN ;

*dm 'odsresults; *clear';

***Save laboratory data to the hard drive***;
libname WORKING 'C:\NHANES\WORKING DATA\SVI Project';
data working.imputed2; set imputed2; run;

proc contents data=imputed2;
run;
*proc contents data=workdata;
*run;

***LOG-TRANSFORM THE IMPUTED DATA TO AMELIORATE THE LEFT-SKEWNESS OF LABORATORY VALUES***;
***K-means is sensitive to outliers so this is a necessary step***;
data work.logchange2;
	set imputed2;
	ln10_URXUCR = log10(URXUCR);
	ln10_URXCRS = log10 (URXCRS);
	ln10_URX2MH = log10(URX2MH);
	ln10_URX34M = log10(URX34M);
	ln10_URXAAM = log10(URXAAM);
	ln10_URXAMC = log10(URXAMC);
	ln10_URXATC = log10(URXATC);
	ln10_URXBMA = log10(URXBMA);
	ln10_URXBPM = log10(URXBPM);
	ln10_URXCEM = log10(URXCEM);
	ln10_URXCYM = log10(URXCYM);
	ln10_URXDHB = log10(URXDHB);
	ln10_URXHPM = log10(URXHPM);
	ln10_URXHP2 = log10(URXHP2);
	ln10_URXIPM1 = log10(URXIPM1);
	ln10_URXIPM3 = log10(URXIPM3);
	ln10_URXMAD = log10(URXMAD);
	ln10_URXMB3 = log10(URXMB3);
	ln10_URXPHG = log10(URXPHG);
	ln10_URXPMM = log10(URXPMM);
	ln10_URXUP8 = log10(URXUP8);
	ln10_URXNO3 = log10(URXNO3);
	ln10_URXSCN = log10(URXSCN);
	ln10_URXP01 = log10(URXP01);
	ln10_URXP02 = log10(URXP02);
	ln10_URXP03 = log10(URXP03);
	ln10_URXP04 = log10(URXP04);
	ln10_URXP06 = log10(URXP06);
	ln10_URXP10 = log10(URXP10);
	ln10_URXP25 = log10(URXP25);
	ln10_URXCOTT = log10(URXCOTT);
	ln10_URXHCTT = log10(URXHCTT);
	ln10_URX4MDA = log10(URX4MDA);
	ln10_LBXEOA = log10(LBXEOA);
	ln10_LBXACR = log10(LBXACR);
	ln10_LBXFOR = log10(LBXFOR);
	run;

***confirm that the log-transform enabled normal distributions***;
	proc univariate data=work.logchange2 plots;
	var ln10_URXUCR	ln10_URXCRS ln10_URX2MH	ln10_URX34M	ln10_URXAAM	ln10_URXAMC	ln10_URXATC	ln10_URXBMA	ln10_URXBPM	ln10_URXCEM	ln10_URXCYM	ln10_URXDHB
	ln10_URXHPM	ln10_URXHP2	ln10_URXIPM1 ln10_URXIPM3 ln10_URXMAD ln10_URXMB3 ln10_URXPHG ln10_URXPMM ln10_URXUP8 ln10_URXNO3
	ln10_URXSCN	ln10_URXP01	ln10_URXP02	ln10_URXP03	ln10_URXP04	ln10_URXP06	ln10_URXP10	ln10_URXP25	ln10_URXCOTT ln10_URXHCTT ln10_URX4MDA
	ln10_LBXEOA	ln10_LBXACR	ln10_LBXFOR;
	run;

***STANDARDIZE THE DATA BECAUSE SOME VARIABLES HAVE DIFFERENT MEASUREMENTS***;
	proc stdize data=work.logchange2 out=ScaleSample2 method=std;
	var ln10_URXUCR	ln10_URXCRS ln10_URX2MH	ln10_URX34M	ln10_URXAAM	ln10_URXAMC	ln10_URXATC	ln10_URXBMA	ln10_URXBPM	ln10_URXCEM	ln10_URXCYM	ln10_URXDHB	
	ln10_URXHPM	ln10_URXHP2	ln10_URXIPM1 ln10_URXIPM3 ln10_URXMAD ln10_URXMB3 ln10_URXPHG ln10_URXPMM ln10_URXUP8 ln10_URXNO3
	ln10_URXSCN	ln10_URXP01	ln10_URXP02	ln10_URXP03	ln10_URXP04	ln10_URXP06	ln10_URXP10	ln10_URXP25	ln10_URXCOTT ln10_URXHCTT ln10_URX4MDA
	ln10_LBXEOA	ln10_LBXACR	ln10_LBXFOR;
	run;

	***confirm standardization worked - all data have a mean of 0 and a standard deviation of 1***;
	proc means data=ScaleSample2 mean stddev min max ndec=2;
	var ln10_URXUCR	ln10_URXCRS ln10_URX2MH	ln10_URX34M	ln10_URXAAM	ln10_URXAMC	ln10_URXATC	ln10_URXBMA	ln10_URXBPM	ln10_URXCEM	ln10_URXCYM	ln10_URXDHB	
	ln10_URXHPM	ln10_URXHP2	ln10_URXIPM1 ln10_URXIPM3 ln10_URXMAD ln10_URXMB3 ln10_URXPHG ln10_URXPMM ln10_URXUP8 ln10_URXNO3
	ln10_URXSCN	ln10_URXP01	ln10_URXP02	ln10_URXP03	ln10_URXP04	ln10_URXP06	ln10_URXP10	ln10_URXP25	ln10_URXCOTT ln10_URXHCTT ln10_URX4MDA
	ln10_LBXEOA	ln10_LBXACR	ln10_LBXFOR;
	run;

	proc contents data=scalesample2;
	run;



*write data to hard drive in 'C:\NHANES\WORKING DATA\SVI Project' and call it 'finaltransformed710b';
libname WORKING 'C:\NHANES\WORKING DATA\SVI Project';
data working.finaltransformed710b; set scalesample2; run;


proc contents data=working.finaltransformed710b;
run;

proc means data=working.finaltransformed710b mean stddev min max ndec=2;
run;


***BEGIN CLUSTERING AND CLUSTER ANALYSIS***
***Create an elbow plot - see supplemental code for elbow plot***
***testing to get the CCC, pseudo-F, and pseudo-T squared - see supplemental code for testing***;
***The above steps are in a different SAS program. They were exploratory to see what "k" we should be using for the actual clustering**
If you want to see those, let me know***;

***PERFORM SURVEY-WEIGHTED K-MEANS CLUSTERING WITH k=3, and the radius is 7, PER THE SUPPLEMENTAL TESTS ABOVE***;

proc fastclus data=working.finaltransformed710b
outseed=clustmeans1
  out=clust2
  maxc=3
radius=7 maxiter=100 
seed=working.finaltransformed710b;
 var ln10_URX2MH ln10_URX34M	ln10_URXAAM	ln10_URXAMC	ln10_URXATC	ln10_URXBMA	ln10_URXBPM	ln10_URXCEM	ln10_URXCYM	ln10_URXDHB	
	ln10_URXHPM	ln10_URXHP2	ln10_URXIPM1 ln10_URXIPM3 ln10_URXMAD ln10_URXMB3 ln10_URXPHG ln10_URXPMM ln10_URXUP8 ln10_URXNO3
	ln10_URXSCN	ln10_URXP01	ln10_URXP02	ln10_URXP03	ln10_URXP04	ln10_URXP06	ln10_URXP10	ln10_URXP25	ln10_URXCOTT ln10_URXHCTT ln10_URX4MDA
	ln10_LBXEOA	ln10_LBXACR	ln10_LBXFOR;
	where include=1;
Weight WTSA2YR;
  run;

  ***Perform canonical discrimination analysis to look at quality of clusters, and to help investigate their characteristics***;
proc candisc anova out=can3;
class cluster;
var ln10_URX2MH	ln10_URX34M	ln10_URXAAM	ln10_URXAMC	ln10_URXATC	ln10_URXBMA	ln10_URXBPM	ln10_URXCEM	ln10_URXCYM	ln10_URXDHB	
	ln10_URXHPM	ln10_URXHP2	ln10_URXIPM1 ln10_URXIPM3 ln10_URXMAD ln10_URXMB3 ln10_URXPHG ln10_URXPMM ln10_URXUP8 ln10_URXNO3
	ln10_URXSCN	ln10_URXP01	ln10_URXP02	ln10_URXP03	ln10_URXP04	ln10_URXP06	ln10_URXP10	ln10_URXP25	ln10_URXCOTT ln10_URXHCTT ln10_URX4MDA
	ln10_LBXEOA	ln10_LBXACR	ln10_LBXFOR;
title2 'Canonical Discriminant Analysis of Chemical Clusters';
run;


**** create the sg plot to visualize the clusters***;
proc format;
  value tmp
    1="1 - High Chemical Cluster"
    3="2 - Medium Chemical Cluster"
	2="3 - Low Chemical Cluster";
run;

data can3;
  set can3;
  format cluster tmp.;
run; 

proc sgplot data = can3;
	title "Plot of Canonical Variables and Cluster Value";
	xaxis label="Canonical Discriminate Analysis Variable 1";
	yaxis label="Canonical Discriminate Analysis Variable 2";
	scatter y = can2 x = can1/ group = cluster;
run;



proc contents data=clust2;
run;

***save cluster assignments to the hard drive***;
libname WORKING 'C:\NHANES\WORKING DATA\SVI Project';
data working.cluster3datanoasthma; set clust2; run;

proc contents data=working.cluster3datanoasthma;
run;
	

***Merge cluster data with workdata in a dataset called 'FinalDataNoAsthma', retaining only the final variables for analysis***;
***It is called FinalDataNoAsthma because it was modified from my original project where asthma outcome was an inclusion critera***;
libname WORKING 'C:\NHANES\WORKING DATA\SVI Project';
data workdata1; set workdata1;
keep include SEQN svi generalhealth5 routinecare3 bmi4 sex2 sex2_1 race6 race6_1 race6_2 race6_3 race6_4 race6_5 chronicsmk2 chronicsmk2_1 SMDANY edu5 edu5_1 edu5_2 edu5_3 edu5_4 poverty3 poverty3_1 poverty3_2 insurance2 insurance2_1 homeown3 homeown3_1 homeown3_2 genhealth5 genetics2 genetics2_1 smokepregn2 smokepregn2_1 asthma2 asthma2_1 attack2 attack2_1 emergency2 emergency2_1 cancer2 cancer2_1 ridageyr WTSA2YR sdmvstra sdmvpsu sddsrvyr;
proc sort; by seqn;
run;
data cluster3datanoasthma; set working.cluster3datanoasthma;
keep include SEQN CLUSTER INCLUDE;
proc sort; by seqn;
run;

data FinalDataNoAsthma; merge workdata1 cluster3datanoasthma; by seqn; run;


proc contents data=FinalDataNoAsthma;
run;


proc means data = FinalDataNoAsthma n nmiss;
  var _numeric_;
  where include=1;
RUN ;


***write data to hard drive in 'C:\NHANES\WORKING DATA\SVI Project' and call it 'FinalData'***;
libname WORKING 'C:\NHANES\WORKING DATA\SVI Project';
data working.FinalDataNoAsthma; set FinalDataNoAsthma; run;

***Code data so we can look at age demographics with more detail***;
data FinalDataNoAsthma; set FinalDataNoAsthma;
if RIDAGEYR >=6 and RIDAGEYR <= 12 then agegrp=1;
if RIDAGEYR > 12 and RIDAGEYR <= 19 then agegrp=2;
run;

proc means data=FinalDataNoAsthma;
var ridageyr;
class agegrp;
run;

***Re-Code Cluster Data for a Binary Outcome***;
data FinalDataNoAsthma; set working.FinalDataNoAsthma;
if cluster=1 then cluster3=0;
if cluster=2 then cluster3=1;
if cluster=3 then cluster3=1;
run;
proc freq; tables cluster3*cluster; run;

proc means data=FinalDataNoAsthma;
where include=1;
run;

proc means data=FinalDataNoAsthma;
class cluster3;
var ridageyr;
run;

***Run descriptive statistics of demographics and measures - overall and by cluster***;
***Descriptive statistics on age for Table1***;
***when cluster is used as a "domain", it retains all 3 levels. When it is used as a variable, it has been recoded to be only two levels for the purposes of logistic regression***;

***Begin descriptive data analysis for Table 1***;

proc surveymeans data=FinalDataNoAsthma;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
domain include;
domain cluster;
var RIDAGEYR;
run;

proc surveymeans data=FinalDataNoAsthma;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
domain include;
domain cluster;
var svi;
run;

*Overall group difference testing;
proc surveyreg data=FinalDataNoAsthma;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
domain include;
class cluster;
model ridageyr=cluster/ANOVA;
run;

proc surveyreg data=FinalDataNoAsthma;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
domain include;
class cluster;
model svi=cluster/ANOVA;
run;

*** Table 1 cont.***;
proc surveyfreq data=FinalDataNoAsthma;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
  table include*sex2*cluster /cl chisq;
  run;

proc surveyfreq data=FinalDataNoAsthma;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
  table include*bmi4*cluster /cl chisq;
  run;

proc surveyfreq data=FinalDataNoAsthma;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
  table include*generalhealth5*cluster /cl chisq;
  run;

  
proc surveyfreq data=FinalDataNoAsthma;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
  table include*routinecare3*cluster /cl chisq;
  run;


proc surveyfreq data=FinalDataNoAsthma;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
  table include*race6*cluster /cl chisq;

  run;

     proc surveyfreq data=FinalDataNoAsthma;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
  table include*svi*cluster /cl chisq;
  run;


***Run the crude model with event 0, which is cluster 1(the high chem cluster)***;

  proc surveylogistic data=FinalDataNoAsthma;
	class cluster3 (param=ref ref='0');
	strata sdmvstra;
	cluster sdmvpsu;
	weight WTSA2YR;
	domain include;
	model cluster3(event='0')= svi / link=glogit DF=INFINITY CLPARM VADJUST=none COVB CORRB RSQUARE STB;
	run;

***Run the full model, contolling for Age and Sex, which are not factors in the SVI***;
proc surveylogistic data=FinalDataNoAsthma;
	strata sdmvstra;
	cluster sdmvpsu;
	weight WTSA2YR;
	domain include;
	model cluster3(event='0')= svi sex2 ridageyr bmi4/ DF=INFINITY CLPARM VADJUST=none COVB CORRB RSQUARE STB;
	run;


	*For every point increase in family social vulnerability, children and adolescents are 1.15 times more likely to be in the high chemical exposure group than either of the other exposure groups;

	*dm 'odsresults; *clear';


*Look at between-group differences for amount of missing data by creating a new dataset for just the missing data;
data missing_svi;
set FinalDataNoAsthma;
where missing(svi);
run;

proc surveymeans data=missing_svi;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
domain include;
domain cluster;
var RIDAGEYR;
run;

proc surveymeans data=missing_svi;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
domain include;
domain cluster;
var svi;
run;

*Overall group difference testing;
proc surveyreg data=missing_svi;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
domain include;
class cluster;
model ridageyr=cluster/ANOVA;
run;

proc surveyreg data=missing_svi;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
domain include;
class cluster;
model svi=cluster/ANOVA;
run;

*** Table 1 cont. for missing data***;
proc surveyfreq data=missing_svi;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
  table include*sex2*cluster /cl chisq;
  run;

proc surveyfreq data=missing_svi;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
  table include*bmi4*cluster /cl chisq;
  run;

proc surveyfreq data=missing_svi;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
  table include*generalhealth5*cluster /cl chisq;
  run;

  
proc surveyfreq data=missing_svi;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
  table include*routinecare3*cluster /cl chisq;
  run;


proc surveyfreq data=missing_svi;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
  table include*race6*cluster /cl chisq;

  run;

     proc surveyfreq data=missing_svi;
strata sdmvstra;
cluster sdmvpsu;
weight WTSA2YR;
  table include*svi*cluster /cl chisq;
  run;


*-------------------------------------------------------------------------;
*---------------------CREATION D'UNE LIBRAIRIE----------------------------;
*-------------------------------------------------------------------------;


libname b "C:\SAS\Master2\Analyse de données et Scoring\bank\data-society-bank-marketing-data";



*--------------------------------------------------------------------------;
*------------------------IMPORTATION DES DONNEES---------------------------;
*--------------------------------------------------------------------------;


%LET libname=C:\SAS\Master2\Analyse de données et Scoring\bank\data-society-bank-marketing-data;
%LET table=test_dummy;
%LET lib=b;

%MACRO IMPORT;
PROC IMPORT OUT=&lib..&table
			DATAFILE="&libname.\&table..csv";
			GETNAMES=yes;
			DELIMITER=';';			
RUN;
%MEND;

%IMPORT;

%MACRO IMPORT_XLSX;
proc import datafile="&libname.\&table..xlsx" replace
        DBMS=xlsx
        out=&lib..&table;
        getnames=yes;
run;
%MEND;
%IMPORT_XLSX;


%MACRO EXPORT;
PROC EXPORT DATA = &lib..&table
		OUTFILE = "&libname.\&table..xlsx"
		DBMS = excel replace;
		SHEET = "&table";
RUN;
%MEND;

%EXPORT;

*------------------------Verifions les valeurs manquantes---------------------;


DATA missing  ; /*(DROP=miss_n)*/
SET &lib..&table;
miss_n=cmiss(of age--y); 
*IF miss_n gt 0 THEN DELETE;
RUN;

title "Valeurs manquantes";
PROC FREQ DATA = missing ;
  TABLE miss_n;
RUN;
title;	

/*Nous n'avons pas de valeurs manquantes*/



*----------------------------------------------------------------------------------;
*----------------------------REGRESSION LOGISTIQUE---------------------------------;
*----------------------------------------------------------------------------------;



*----------------------------------------------------------------------------------;
*------------------------ETAPE 1 : ECHANTILLONNAGE---------------------------------;
*----------------------------------------------------------------------------------;



PROC SORT DATA=b.bankfull OUT=b.bank_sort; BY y; RUN;
		 
PROC SURVEYSELECT DATA=b.bank_sort
	OUTALL SAMPRATE=70 METHOD=SRS SEED=435 OUT=echantillon;
	STRATA y;
run;

DATA b.train b.test;
	SET echantillon;
	IF selected=1 THEN OUTPUT b.train;
	ELSE OUTPUT b.test;
RUN;


*-------------Verifions le poids de survie dans les deux tables-----------;
title "Base train";
PROC FREQ DATA=b.train; TABLE y; RUN; title;
title "Base test";
PROC FREQ DATA=b.test; TABLE y; RUN; title;

data tit;
set b.bankfull;
if duration=0 then delete;
run;


*------------------------------------------------------------------------------;
*------------------------ETAPE 2 : ENTRAINEMENT DU MODELE----------------------;
*------------------------------------------------------------------------------;



*----------Création du premier modèle "sans interaction"-----------------------;

%LET var = age balance campaign contact day default duration
		education  housing job loan marital month pdays poutcome previous; 
 ;
PROC LOGISTIC DATA=b.train  OUTMODEL = score1 PLOTS(MAXPOINTS=NONE); /*PLOTS(ONLY)=(effect oddsratio)*/
	CLASS  contact default day month
		education  housing job loan marital  poutcome  / PARAM = glm ;
	BACKWARD : MODEL y (event="yes") = &var /SELECTION=backward slentry=0.01 OUTROC=b.roc1 CTABLE;
	ODS output Association = b.performance1;
	OUTPUT OUT =estimated predicted =estprob  predprobs=I l = lower95 u =upper95;
	SCORE OUT =b.score11;	
RUN;

DATA b.score1; SET score1; RUN;

title "Matrice de confusion apprentissage Modèle 1";
PROC FREQ DATA = b.score11;
	TABLE y*i_y / OUT = Z;
run;
title;


*-----Extraction des fréquences pour construire le taux de faux positifs, faux négatifs etc------;

PROC SQL NOPRINT;
	SELECT COUNT
	INTO :COUNT5-:COUNT8	 
	FROM Z;
QUIT;


%LET AA = &COUNT5; %LET BB = &COUNT6; %LET CC = &COUNT7; %LET DD = &COUNT8; 
%PUT &AA;



*----------Création du second modèle "avec interaction"-----------------------;


PROC LOGISTIC DATA=b.train  OUTMODEL = b.score2 PLOTS(MAXPOINTS=NONE); /*PLOTS = ALL*/
	CLASS  contact default day month
		education  housing job loan marital  poutcome  / PARAM = glm ;
	
	BACKWARD : MODEL y (event="yes") = &var age|balance|campaign|contact|day|default|duration|
							education|housing|job|loan|marital|month|pdays|poutcome|previous @2 /
							SELECTION=backward slentry=0.01 OUTROC=b.roc2;
	ODS output Association = b.performance2;
	OUTPUT OUT =estimated2 predicted =estprob  predprobs=I l = lower95 u =upper95;
	SCORE OUT = b.score22;
RUN;

title "Matrice de confusion apprentissage Modèle 2";
PROC FREQ DATA = b.score22;
	TABLE y*i_y / OUT = H;
run;
title;


*-----Extraction des fréquences pour construire le taux de faux positifs, faux négatifs etc------;


PROC SQL NOPRINT;
	SELECT COUNT
	INTO :COUNT9-:COUNT12	 
	FROM H;
QUIT;

%LET AAA = &COUNT9; %LET BBB = &COUNT10; %LET CCC = &COUNT11; %LET DDD = &COUNT12; 
%PUT &AAA;


*------------------------------------------------------------------------------;
*--------------------------ETAPE 2 : TEST DU MODELE----------------------------;
*------------------------------------------------------------------------------;



*---------------Test du premier modèle "sans interaction"----------------------;


PROC LOGISTIC INMODEL = score1;
	SCORE DATA = b.test OUT = test1 ;
RUN;

DATA b.test1; SET test1; RUN;

title "Matrice de confusion test Modèle 1";
PROC FREQ DATA = b.test1;
	TABLE y*i_y / OUT = M ;
run;
TITLE;


*-----Extraction des fréquences pour construire le taux de faux positifs, faux négatifs etc------;


PROC SQL NOPRINT;
	SELECT COUNT
	INTO :COUNT1-:COUNT4	 
	FROM M;
QUIT;

%PUT &COUNT1;

%LET A = &COUNT1; %LET B = &COUNT2; %LET C = &COUNT3; %LET D = &COUNT4; 
%PUT &A;

%LET Fn = %sysevalf( &C / (&A + &C)); %PUT &Fn; /*Fn = faux positifs*/


*-----------------Test du second modèle "avec interaction"----------------------;


PROC LOGISTIC INMODEL = b.score2;
	SCORE DATA = b.test OUT = test2;
RUN;

DATA b.test2; SET test2; RUN;

title "Matrice de confusion test Modèle 2";
PROC FREQ DATA = b.test2;
	TABLE y*i_y / OUT = F;
run;
title;


*-----Extraction des fréquences pour construire le taux de faux positifs, faux négatifs etc------;



PROC SQL NOPRINT;
	SELECT COUNT
	INTO :COUNT13-:COUNT16	 
	FROM F;
QUIT;

%LET AAAA = &COUNT13; %LET BBBB = &COUNT14; %LET CCCC = &COUNT15; %LET DDDD = &COUNT16; 
%PUT &AAAA;






*----------------------------------------------------------------------------------;
*----------------------------ANALYSE DISCRIMINANTE---------------------------------;
*----------------------------------------------------------------------------------;




*---------------------MACRO PROGRAMME POUR EXTRAIRE LE NOM DES VARIABLES----------------------;


PROC CONTENTS DATA = b.train_dummy OUT=OUT;
RUN;
 
PROC SQL;
	SELECT NAME INTO: NAME SEPARATED BY " " FROM OUT;
	SELECT COUNT(*) INTO: loop  FROM OUT;
QUIT;
 
%PUT &name;


%LET all = age balance campaign contact_cellular contact_telephone contact_unknown day_1 day_2 day_3 day_4 day_5 day_6 day_7 day_8 day_9
day_10 day_11 day_12 day_13 day_14 day_15 day_16 day_17 day_18 day_19 day_20 day_21 day_22 day_23 day_24 day_25 day_26 day_27
day_28 day_29 day_30 day_31 default_no default_yes duration education_primary education_secondary education_tertiary
education_unknown housing_no housing_yes job_admin_ job_blue_collar job_entrepreneur job_housemaid job_management job_retired
job_self_employed job_services job_student job_technician job_unemployed job_unknown loan_no loan_yes marital_divorced
marital_married marital_single month_apr month_aug month_dec month_feb month_jan month_jul month_jun month_mar month_may
month_nov month_oct month_sep pdays poutcome_failure poutcome_other poutcome_success poutcome_unknown previous;



*---------------------Procédure de selection automatique des variables------------------------;


proc stepdisc data = b.train_dummy method = backward slentry = 0.01;
class y;
var &all;
run;


%LET step = campaign  
contact_cellular 
contact_telephone 
day_2 
day_5 
day_6 
day_7 
day_11  
day_17 
day_18  
day_19 
day_20 
day_27 
day_30 
duration duration 
education_tertiary 
housing_no 
job_admin_ 
job_retired  
job_student 
loan_no 
marital_married 
month_apr 
month_aug 
month_feb 
month_jan 
month_jul 
month_jun 
month_mar 
month_may 
month_nov 
poutcome_success 
previous;


DATA test_dummy2;
SET b.test_dummy;
KEEP &step y ;
RUN; 


*-----------------------------Réalisation de la classifiaction--------------------------------;


proc discrim data = b.train_dummy crossvalidate pool = test  testdata = test_dummy2 testout = pred MANOVA;
class y;
var &step;
priors proportional;
run;

/* crossvalidate permet de calculer l’erreur de validation croisée*/
/* pool = test permet ici de faire un test d'homogeneite des variances des deux groupes*/
/* la p-value du test ne permet pas de rejetter l’hypothèse de variances  homogènes */
/*testout renvoie le résultat de la prediction dans la table pred*/

/*Wilks' Lambda hypothese nulle : les classes sont superposées
On rejette cette hypoyhese p value tres petites*/


proc discrim data = b.train_dummy crossvalidate pool = test  testdata = b.test_dummy testout = pred MANOVA;
class y;
var &all;
priors proportional;
run;

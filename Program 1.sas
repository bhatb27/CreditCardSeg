libname fincs1 "/folders/myfolders/New folder/FINALCS1";

proc import datafile="/folders/myfolders/New folder/FINALCS1/CC GENERAL.csv" dbms=csv out=fincs1.ccraw;
guessingrows=100;
run;

proc contents data=fincs1.ccraw varnum;run;

proc means data=fincs1.ccraw2 n nmiss mean stddev min p1 p10 p25 p50 p75 p90 p95 p99 QRANGE max;
var BALANCE PURCHASES ONEOFF_PURCHASES INSTALLMENTS_PURCHASES CASH_ADVANCE CASH_ADVANCE_TRX PURCHASES_TRX CREDIT_LIMIT PAYMENTS MINIMUM_PAYMENTS; 
run;

DATA fincs1.ccraw1;
set fincs1.ccraw;
IF BALANCE>5000 THEN BALANCE = 5000;
IF PURCHASES>4000 THEN PURCHASES = 4000;
IF ONEOFF_PURCHASES>2500 THEN ONEOFF_PURCHASES = 2500;
IF INSTALLMENTS_PURCHASES>1700 THEN INSTALLMENTS_PURCHASES = 1700;
IF CASH_ADVANCE>4000 THEN CASH_ADVANCE = 4000;
IF CASH_ADVANCE_TRX>15 THEN CASH_ADVANCE_TRX = 15;
IF PURCHASES_TRX>40 THEN PURCHASES_TRX = 40;
IF CREDIT_LIMIT>10000 THEN CREDIT_LIMIT = 10000;
IF PAYMENTS>3500 THEN PAYMENTS = 3500;
IF MINIMUM_PAYMENTS>2000 THEN MINIMUM_PAYMENTS = 2000;
RUN;

DATA fincs1.ccraw2;
set fincs1.ccraw1;
IF MINIMUM_PAYMENTS=. THEN MINIMUM_PAYMENTS = BALANCE/3;
IF CREDIT_LIMIT=. THEN CREDIT_LIMIT = 4228;
RUN;

DATA fincs1.ccdata1;
set fincs1.ccraw2;
MONTH_AVG_PURCHASE = PURCHASES/TENURE;
MONTH_AVG_CASH = CASH_ADVANCE/TENURE;
LIMIT_USAGE = BALANCE/CREDIT_LIMIT;
AVG_AMT_PURCHASE = PURCHASES/PURCHASES_TRX;
AVG_AMT_CASH = CASH_ADVANCE/CASH_ADVANCE_TRX;
PAY_TO_MINPAY = PAYMENTS/MINIMUM_PAYMENTS;
RUN;

proc means data=fincs1.ccDATA1 n nmiss mean stddev min p1 p10 p25 p50 p75 p90 p95 p99 QRANGE max;
*var BALANCE PURCHASES ONEOFF_PURCHASES INSTALLMENTS_PURCHASES CASH_ADVANCE CASH_ADVANCE_TRX PURCHASES_TRX CREDIT_LIMIT PAYMENTS MINIMUM_PAYMENTS; 
run;

DATA fincs1.ccdata1;
set fincs1.ccdata1;
IF AVG_AMT_PURCHASE=. THEN AVG_AMT_PURCHASE = 0;
IF AVG_AMT_CASH=. THEN AVG_AMT_CASH = 0;
IF MINIMUM_PAYMENTS=0 THEN PAY_TO_MINPAY = 0;
RUN;

DATA fincs1.ccdata1;
set fincs1.ccdata1;
IF LIMIT_USAGE>1 THEN LIMIT_USAGE = 1;
IF AVG_AMT_PURCHASE>500 THEN AVG_AMT_PURCHASE = 500;
IF AVG_AMT_CASH>600 THEN AVG_AMT_CASH = 600;
IF PAY_TO_MINPAY>50 THEN PAY_TO_MINPAY = 50;
RUN;


PROC factor data= fincs1.ccdata1 method=principal
scree mineigen=0 nfactors= 6 rotate=varimax reorder;
run;

data fincs1.ccdata2;
set fincs1.ccdata1;
z_CASH_ADVANCE = CASH_ADVANCE;
z_AVG_AMT_CASH = AVG_AMT_CASH;
z_ONEOFF_PURCHASES = ONEOFF_PURCHASES;
z_MONTH_AVG_PURCHASE = MONTH_AVG_PURCHASE;
z_PURCHASES_INSTALLMENTS_FREQ = PURCHASES_INSTALLMENTS_FREQUENCY;
z_PURCHASES_FREQUENCY = PURCHASES_FREQUENCY;
z_LIMIT_USAGE = LIMIT_USAGE;
z_BALANCE = BALANCE;
z_PAY_TO_MINPAY = PAY_TO_MINPAY;
z_CREDIT_LIMIT = CREDIT_LIMIT;
z_AVG_AMT_PURCHASE = AVG_AMT_PURCHASE;
run;

PROC STANDARD DATA=fincs1.ccdata2 MEAN=0 STD=1 out=fincs1.CCDATA2;
VAR z_CASH_ADVANCE z_AVG_AMT_CASH z_ONEOFF_PURCHASES z_MONTH_AVG_PURCHASE z_PURCHASES_INSTALLMENTS_FREQ
	z_PURCHASES_FREQUENCY z_LIMIT_USAGE z_BALANCE z_PAY_TO_MINPAY z_CREDIT_LIMIT z_AVG_AMT_PURCHASE;
RUN;

proc fastclus data=fincs1.ccdata2 cluster=clust_4 out=fincs1.ccdata2 maxclusters=4 maxiter=100;
Var
z_CASH_ADVANCE
z_AVG_AMT_CASH
z_ONEOFF_PURCHASES
z_MONTH_AVG_PURCHASE
z_PURCHASES_INSTALLMENTS_FREQ
z_PURCHASES_FREQUENCY
z_LIMIT_USAGE
z_BALANCE
z_PAY_TO_MINPAY
z_CREDIT_LIMIT
z_AVG_AMT_PURCHASE;
run;

proc fastclus data=fincs1.ccdata2 cluster=clust_5 out=fincs1.ccdata2 maxclusters=5 maxiter=100;
Var
z_CASH_ADVANCE
z_AVG_AMT_CASH
z_ONEOFF_PURCHASES
z_MONTH_AVG_PURCHASE
z_PURCHASES_INSTALLMENTS_FREQ
z_PURCHASES_FREQUENCY
z_LIMIT_USAGE
z_BALANCE
z_PAY_TO_MINPAY
z_CREDIT_LIMIT
z_AVG_AMT_PURCHASE;
run;

proc fastclus data=fincs1.ccdata2 cluster=clust_6 out=fincs1.ccdata2 maxclusters=6 maxiter=100;
Var
z_CASH_ADVANCE
z_AVG_AMT_CASH
z_ONEOFF_PURCHASES
z_MONTH_AVG_PURCHASE
z_PURCHASES_INSTALLMENTS_FREQ
z_PURCHASES_FREQUENCY
z_LIMIT_USAGE
z_BALANCE
z_PAY_TO_MINPAY
z_CREDIT_LIMIT
z_AVG_AMT_PURCHASE;
run;

proc freq data=fincs1.ccdata2;
table clust_4 clust_5 clust_6;
run;

ods excel file="/folders/myfolders/New folder/FINALCS1/Profile.xlsx";

proc tabulate data=fincs1.ccdata2;
var
BALANCE PURCHASES ONEOFF_PURCHASES INSTALLMENTS_PURCHASES CASH_ADVANCE CASH_ADVANCE_TRX PURCHASES_TRX CREDIT_LIMIT PAYMENTS MINIMUM_PAYMENTS
LIMIT_USAGE AVG_AMT_PURCHASE AVG_AMT_CASH PAY_TO_MINPAY;

class clust_4 clust_5 clust_6;

table 
(BALANCE PURCHASES ONEOFF_PURCHASES INSTALLMENTS_PURCHASES CASH_ADVANCE CASH_ADVANCE_TRX PURCHASES_TRX CREDIT_LIMIT PAYMENTS MINIMUM_PAYMENTS
LIMIT_USAGE AVG_AMT_PURCHASE AVG_AMT_CASH PAY_TO_MINPAY)*mean, all clust_4 clust_5 clust_6 ;

run;

ods excel close;








***************************
***** PHP 2516 HW # 2 *****
******  Blain Morin  ******
***************************

*1

*** Import data
import delimited C:\Users\blain\Documents\skewl\longitudinal-hw2\calcium_allL.csv, clear


*** Declare data longitudinal
xtset person visit


*** Describe patterns of missingness
xtdescribe 

*** Create Spaghetti Plot
xtline bmd if person <= 108, overlay title(Trajectories for the First 8 Individuals)

*** Create boxplot by group
graph box bmd, over(visit) box(1, fcolor(red)) by(group)


*** Change group to factor
gen tx = 1 if group=="C"
replace tx=0 if group=="P"
label define tx 1 "Treatment" 0 "Placebo"
label values tx tx

*** Response Profile (model1)
mixed bmd visit##tx || person: , noconst residuals(unstructured, t(visit)) reml
estimate store model1



*** Create visit^2
generate visit2 = visit^2

*** Create tx*visit
generate txvisit = tx * visit

*** Create tx*visit^2
generate txvisit2 = tx * visit2

*** Create tx * age
generate txage = tx * age

*** Full model (model2)
mixed bmd visit visit2 tx age txvisit txvisit2 txage  || person: , noconst residuals(unstructured, t(visit)) reml nolog
estimate store model2

*** Remove interaction with visit^2 (model3)
mixed bmd visit visit2 tx age txvisit txage  || person: , noconst residuals(unstructured, t(visit)) reml nolog
estimate store model3

*** Remove visit^2 (model4)
mixed bmd visit tx age txvisit txage  || person: , noconst residuals(unstructured, t(visit)) reml nolog
estimate store model4

*** Remove txage (model5)
mixed bmd visit tx age txvisit  || person: , noconst residuals(unstructured, t(visit)) reml nolog
estimate store model5

*** Information Criteria for all models
estimates stats model1 model2 model3 model4 model5 


* 2

*** Unstructured
mixed bmd visit tx age txvisit || person: , noconst residuals(unstructured, t(visit)) reml nolog
estimate store unstr

*** Exchangable
mixed bmd visit tx age txvisit  || person: , noconst residuals(exchangeable, t(visit)) reml nolog
estimate store exch

*** Autoregressive 1
mixed bmd visit tx age txvisit || person: , noconst residuals(ar 1, t(visit)) reml nolog
estimate store auto1

*** Autoregressive 3
 mixed bmd visit tx age txvisit || person: , noconst residuals(ar 3, t(visit)) reml nolog
estimate store auto3

*** Toeplitz 1
 mixed bmd visit tx age txvisit || person: , noconst residuals(toeplitz 1, t(visit)) reml nolog
estimate store toe1

*** Toeplitz 3
 mixed bmd visit tx age txvisit || person: , noconst residuals(toeplitz 3, t(visit)) reml nolog
estimate store toe3

*** Toeplitz 4
 mixed bmd visit tx age txvisit || person: , noconst residuals(toeplitz 3, t(visit)) reml nolog
estimate store toe4

estimates stats unstr exch auto1 auto3 toe1 toe3 toe4 


*3

*** Unstructured
mixed bmd visit tx  || person: , noconst residuals(unstructured, t(visit)) reml nolog
estimate store unstr

*** Exchangable
mixed bmd visit tx  || person: , noconst residuals(exchangeable, t(visit)) reml nolog
estimate store exch

*** Autoregressive 1
mixed bmd visit tx  || person: , noconst residuals(ar 1, t(visit)) reml nolog
estimate store auto1

*** Autoregressive 3
 mixed bmd visit tx  || person: , noconst residuals(ar 3, t(visit)) reml nolog
estimate store auto3

*** Toeplitz 1
 mixed bmd visit tx  || person: , noconst residuals(toeplitz 1, t(visit)) reml nolog
estimate store toe1

*** Toeplitz 3
 mixed bmd visit tx  || person: , noconst residuals(toeplitz 3, t(visit)) reml nolog
estimate store toe3

*** Toeplitz 4
 mixed bmd visit tx  || person: , noconst residuals(toeplitz 3, t(visit)) reml nolog
estimate store toe4

*** Mean response
mixed bmd i.visit tx || person: , noconst residuals(unstructured, t(visit)) reml nolog
estimate store meanresp

estimates stats unstr exch auto1 auto3 toe1 toe3 toe4 meanresp



*5
*** Import data
import delimited C:\Users\blain\Documents\skewl\longitudinal-hw2\cd4.csv, clear

gen trt = 0
replace trt = 1 if treatment == 4
label define trt 1 "Triple" 0 "Other"
label values trt trt

*** Spaghetti Plot

xtline logcd4 if trt == 0 & id <= 50, i(id) t(week) overlay
xtline logcd4 if trt == 1 & id <= 100, i(id) t(week) overlay


*6
gen int intweek = ceil(week)
gen trtintweek = trt*intweek

***  Increase matrix option for stata, so that unstructured can run
*set matsize 800

*** Unstructured ( takes too long to run )
*mixed logcd4 trt intweek trtintweek || id: ,  noconst residuals(unstructured, t(intweek)) reml nolog 
*estimate store unstr

*** Exchangeable
mixed logcd4 trt intweek trtintweek || id: ,  noconst residuals(exchangeable, t(intweek)) reml nolog 
estimate store exchange

*** Autoregressive
mixed logcd4 trt intweek trtintweek || id: ,  noconst residuals(ar 1, t(intweek)) reml nolog 
estimate store ar1

*** Toeplitz
mixed logcd4 trt intweek trtintweek || id: ,  noconst residuals(toeplitz 1, t(intweek)) reml nolog 
estimate store toeplitz


estimates stats exchange ar1 toeplitz


*7

gen week2 = week^2
gen weektrt = week * trt
gen week2trt = week2 * trt
gen agetrt = age * trt
gen gendertrt = gender * trt


*** Random Intercepts (model1)
mixed logcd4 week week2 trt age gender weektrt week2trt agetrt gendertrt || id: , reml nolog
estimate store randint
predict xbrandint, xb
predict fitrandint, fit

*** Random slope on time (model2)
mixed logcd4 week week2 trt age gender weektrt week2trt agetrt gendertrt || id: week , reml nolog
estimate store rand1
predict xbrand1, xb
predict fitrand1, fit

*** Random slope on time and time^2 (model3)
mixed logcd4 week week2 trt age gender weektrt week2trt agetrt gendertrt || id: week week2 , reml nolog
estimate store rand2
predict xbrand2, xb
predict fitrand2, fit

*** Random slope on time time^2 and age (model4)
mixed logcd4 week week2 trt age gender weektrt week2trt agetrt gendertrt || id: week week2 age, reml nolog
estimate store rand3
predict xbrand3, xb
predict fitrand3, fit

estimates stats randint rand1 rand2 rand3


*** Random intercepts graphs (model1)
#delimit ;
twoway (line logcd4 week if id == 154, sort lcolor(blue)) 
	(line xbrandint week if id == 154, sort lpattern(dot) lcolor(blue))
	(line fitrandint week if id == 154, sort lpattern(dash) lcolor(blue))
	(line logcd4 week if id == 162, sort lcolor(orange)) 
	(line fitrandint week if id == 162, sort lpattern(dash) lcolor(orange))
	(line xbrandint week if id == 162, sort lpattern(dot) lcolor(orange)),
	ylabel(0(1)5)
	legend(order(1 "Obs: id=154" 2 "Xb id=154" 3 "Predicted id=154" 4 "Obs: id=162" 5 "Predicted id=162" 6 "Xb id=162"))
	title("Random Intercepts Model 1: Triple Therapy");
#delimit cr

#delimit ;
twoway (line logcd4 week if id == 161, sort lcolor(blue)) 
	(line xbrandint week if id == 161, sort lpattern(dot) lcolor(blue))
	(line fitrandint week if id == 161, sort lpattern(dash) lcolor(blue))
	(line logcd4 week if id == 160, sort lcolor(orange)) 
	(line fitrandint week if id == 160, sort lpattern(dash) lcolor(orange))
	(line xbrandint week if id == 160, sort lpattern(dot) lcolor(orange)),
	ylabel(0(1)5)
	legend(order(1 "Obs: id=161" 2 "Xb id=161" 3 "Predicted id=161" 4 "Obs: id=160" 5 "Predicted id=160" 6 "Xb id=160"))
	title("Random Intercepts Model 1: Dual Therapy");
#delimit cr	

*** Random Ints and Slopes (model2)

#delimit ;
twoway (line logcd4 week if id == 154, sort lcolor(blue)) 
	(line xbrand1 week if id == 154, sort lpattern(dot) lcolor(blue))
	(line fitrand1 week if id == 154, sort lpattern(dash) lcolor(blue))
	(line logcd4 week if id == 162, sort lcolor(orange)) 
	(line fitrand1 week if id == 162, sort lpattern(dash) lcolor(orange))
	(line xbrand1 week if id == 162, sort lpattern(dot) lcolor(orange)),
	ylabel(0(1)5)
	legend(order(1 "Obs: id=154" 2 "Xb id=154" 3 "Predicted id=154" 4 "Obs: id=162" 5 "Predicted id=162" 6 "Xb id=162"))
	title("Random Ints and Slopes Model 2: Triple Therapy");
#delimit cr

#delimit ;
twoway (line logcd4 week if id == 161, sort lcolor(blue)) 
	(line xbrand1 week if id == 161, sort lpattern(dot) lcolor(blue))
	(line fitrand1 week if id == 161, sort lpattern(dash) lcolor(blue))
	(line logcd4 week if id == 160, sort lcolor(orange)) 
	(line fitrand1 week if id == 160, sort lpattern(dash) lcolor(orange))
	(line xbrand1 week if id == 160, sort lpattern(dot) lcolor(orange)),
	ylabel(0(1)5)
	legend(order(1 "Obs: id=161" 2 "Xb id=161" 3 "Predicted id=161" 4 "Obs: id=160" 5 "Predicted id=160" 6 "Xb id=160"))
	title("Random Ints and Slopes Model 2: Dual Therapy");
#delimitcr

	
*** Random Ints and Slopes (model3)

#delimit ;
twoway (line logcd4 week if id == 154, sort lcolor(blue)) 
	(line xbrand2 week if id == 154, sort lpattern(dot) lcolor(blue))
	(line fitrand2 week if id == 154, sort lpattern(dash) lcolor(blue))
	(line logcd4 week if id == 162, sort lcolor(orange)) 
	(line fitrand2 week if id == 162, sort lpattern(dash) lcolor(orange))
	(line xbrand2 week if id == 162, sort lpattern(dot) lcolor(orange)),
	ylabel(0(1)5)
	legend(order(1 "Obs: id=154" 2 "Xb id=154" 3 "Predicted id=154" 4 "Obs: id=162" 5 "Predicted id=162" 6 "Xb id=162"))
	title("Random Ints and Slopes Model 3: Triple Therapy");
#delimit cr

#delimit ;
twoway (line logcd4 week if id == 161, sort lcolor(blue)) 
	(line xbrand2 week if id == 161, sort lpattern(dot) lcolor(blue))
	(line fitrand2 week if id == 161, sort lpattern(dash) lcolor(blue))
	(line logcd4 week if id == 160, sort lcolor(orange)) 
	(line fitrand2 week if id == 160, sort lpattern(dash) lcolor(orange))
	(line xbrand2 week if id == 160, sort lpattern(dot) lcolor(orange)),
	ylabel(0(1)5)
	legend(order(1 "Obs: id=161" 2 "Xb id=161" 3 "Predicted id=161" 4 "Obs: id=160" 5 "Predicted id=160" 6 "Xb id=160"))
	title("Random Ints and Slopes Model 3: Dual Therapy");
#delimitcr

*** Random Ints and Slopes (model4)

#delimit ;
twoway (line logcd4 week if id == 154, sort lcolor(blue)) 
	(line xbrand3 week if id == 154, sort lpattern(dot) lcolor(blue))
	(line fitrand3 week if id == 154, sort lpattern(dash) lcolor(blue))
	(line logcd4 week if id == 162, sort lcolor(orange)) 
	(line fitrand3 week if id == 162, sort lpattern(dash) lcolor(orange))
	(line xbrand3 week if id == 162, sort lpattern(dot) lcolor(orange)),
	ylabel(0(1)5)
	legend(order(1 "Obs: id=154" 2 "Xb id=154" 3 "Predicted id=154" 4 "Obs: id=162" 5 "Predicted id=162" 6 "Xb id=162"))
	title("Random Ints and Slopes Model 4: Triple Therapy");
#delimit cr

#delimit ;
twoway (line logcd4 week if id == 161, sort lcolor(blue)) 
	(line xbrand3 week if id == 161, sort lpattern(dot) lcolor(blue))
	(line fitrand3 week if id == 161, sort lpattern(dash) lcolor(blue))
	(line logcd4 week if id == 160, sort lcolor(orange)) 
	(line fitrand3 week if id == 160, sort lpattern(dash) lcolor(orange))
	(line xbrand3 week if id == 160, sort lpattern(dot) lcolor(orange)),
	ylabel(0(1)5)
	legend(order(1 "Obs: id=161" 2 "Xb id=161" 3 "Predicted id=161" 4 "Obs: id=160" 5 "Predicted id=160" 6 "Xb id=160"))
	title("Random Ints and Slopes Model 4: Dual Therapy");
#delimitcr





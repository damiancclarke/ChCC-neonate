/* motherAnalysis.do             damiancclarke             yyyy-mm-dd:2016-01-17
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

Codigo para replicar de manera breve los resultados de diferencias en diferenci-
as de la tesis, y estimar modelos de efectos fijos a nivel de madre (condicional
en edad y orden de nacimiento).


*/

vers 11
clear all
set more off
cap log close
set matsize 2000

*--------------------------------------------------------------------------------
*--- (0) Globales (cambiar para ubicacion en tu maquina)
*--------------------------------------------------------------------------------
global DAT "~/investigacion/2016/ChCC-neonate/data"
global OUT "~/investigacion/2016/ChCC-neonate/results"
global LOG "~/investigacion/2016/ChCC-neonate/log"

log using "$LOG/motherAnalysis.txt", text replace

*--------------------------------------------------------------------------------
*--- (1) Abrir y hacer una limpieza basica
*--------------------------------------------------------------------------------
use "$DAT/nacimientos2000-2010"
destring ano_nac mes_nac comuna peso talla edad_m hij_tot semanas, replace
gen anio  = ano_nac

merge m:1 comuna anio using $DAT/crosswalk
drop if _merge==2
drop _merge

merge m:1 ccode using $DAT/early-MDS
gen early = _merge==3
drop _merge
gen     chcc = chcc_gestante=="SI" if chcc_gestante!=""
replace chcc = . if dn_sexo_padre=="MASCULINO"
gen     post = 0 if early==1&anio<2007
replace post = 0 if early==1&anio==2007&mes_nac<=6
replace post = 0 if early==0&anio<2008
replace post = 1 if post==.

gen postChCC = post*chcc
gen teen     = edad_m <20 if edad_m!=.

replace peso      = . if peso<500|peso>5000
replace talla     = . if talla<30|talla>60
replace semanas   = . if semanas<25|semanas>45
replace edad_m    = . if edad_m<14|edad_m>=50
replace hij_total = . if hij_total>15

gen everChCC = chcc==1
*--------------------------------------------------------------------------------
*--- (2) Descriptives/Sum Stats
*--------------------------------------------------------------------------------
preserve
collapse peso talla semanas, by(ano_nac chcc)

#delimit ;
twoway line peso ano_nac if chcc==1, lcolor(black) lwidth(thick) scheme(s1mono)
    || line peso ano_nac if chcc==0, lpattern(dash) lcolor(black) lwidth(thick)
ytitle("Mean Birth Weight (grams)") xtitle("Year of Birth")
legend(lab(1 "Ever Participated in ChCC") lab(2 "Never Participated in ChCC"))
xline(2007.5, lcolor(red));
graph export "$OUT/MotherTrend-weight.eps", as(eps) replace;

twoway line talla ano_nac if chcc==1, lcolor(black) lwidth(thick) scheme(s1mono)
    || line talla ano_nac if chcc==0, lpattern(dash) lcolor(black) lwidth(thick)
ytitle("Mean Birth Size (cms)") xtitle("Year of Birth")
legend(lab(1 "Ever Participated in ChCC") lab(2 "Never Participated in ChCC"))
xline(2007.5, lcolor(red));
graph export "$OUT/MotherTrend-size.eps", as(eps) replace;

twoway line semanas ano_nac if chcc==1, lcolor(black) lwidth(thick) scheme(s1mono)
    || line semanas ano_nac if chcc==0, lpattern(dash) lcolor(black) lwidth(thick)
ytitle("Mean Gestational Length (weeks)") xtitle("Year of Birth")
legend(lab(1 "Ever Participated in ChCC") lab(2 "Never Participated in ChCC"))
xline(2007.5, lcolor(red));
graph export "$OUT/MotherTrend-gestation.eps", as(eps) replace;
restore;


twoway kdensity peso if chcc==1&post==0, lcolor(red) lwidth(thick) scheme(s1mono)
 ||   kdensity peso if chcc==0&post==0, lcolor(blue) lwidth(thick) lpattern(dash)
legend(lab(1 "Ever Participated in ChCC") lab(2 "Never Participated in ChCC"))
xtitle("Birth Weight (grams)") ytitle("Density") bwidth(25);
graph export "$OUT/Density_weightPre.eps", as(eps) replace;

twoway kdensity peso if chcc==1&post==1, lcolor(red) lwidth(thick) scheme(s1mono)
 ||   kdensity peso if chcc==0&post==1, lcolor(blue) lwidth(thick) lpattern(dash)
legend(lab(1 "Ever Participated in ChCC") lab(2 "Never Participated in ChCC"))
xtitle("Birth Weight (grams)") ytitle("Density") bwidth(25);
graph export "$OUT/Density_weightPost.eps", as(eps) replace;

twoway kdensity sema if chcc==1&post==0, lcolor(red) lwidth(thick) scheme(s1mono)
 ||   kdensity sema if chcc==0&post==0, lcolor(blue) lwidth(thick) lpattern(dash)
legend(lab(1 "Ever Participated in ChCC") lab(2 "Never Participated in ChCC"))
xtitle("Gestation (weeks)") ytitle("Density") bwidth(1);
graph export "$OUT/Density_gestationPre.eps", as(eps) replace;

twoway kdensity sema if chcc==1&post==1, lcolor(red) lwidth(thick) scheme(s1mono)
 ||   kdensity sema if chcc==0&post==1, lcolor(blue) lwidth(thick) lpattern(dash)
legend(lab(1 "Ever Participated in ChCC") lab(2 "Never Participated in ChCC"))
xtitle("Gestation (weeks)") ytitle("Density") bwidth(1);
graph export "$OUT/Density_gestationPost.eps", as(eps) replace;
#delimit cr


gen lbw = peso<2500 if peso!=.
gen premature = semanas<37 if semanas!=.
gen gestation = semanas
keep if ano_nac>2002
lab var peso      "Birth Weight (grams)"
lab var lbw       "Low Birth Weight $< 2500$ grams"
lab var talla     "Length (cm)"
lab var gestation "Gestation (weeks)"
lab var premature "Premature $< 37$ weeks"
lab var chcc      "Proportion Ever Enrolled in ChCC"
lab var ano_nac   "Year of Birth"
lab var edad_m    "Mother's Age"
lab var teen      "Proportion Teen Births"
lab var hij_total "Number of Children"

#delimit ;
estpost sum chcc peso lbw gestation premat talla ano_nac edad_m teen hij_total
            if chcc!=.;
estout using "$OUT/SummaryMother-update.tex", replace label style(tex)
cells("count mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))")
collabels(, none) mlabels(, none);
#delimit cr


*--------------------------------------------------------------------------------
*--- (3) Regresiones FE
*--------------------------------------------------------------------------------
destring run_padre_falso, replace
xtset run_padre_falso
bys run_padre_falso (ano_nac): gen bord = _n
bys run_padre_falso: gen fert = _N

gen twoa = ano_nac==2006|ano_nac==2007
gen twob = ano_nac==2008|ano_nac==2009
bys run_padre_falso: egen twoafter  = max(twoa)
bys run_padre_falso: egen twobefore = max(twob)
gen beforeafter = twoafter==1&twobefore==1


local fse fe vce(cluster run_padre_falso)
local controls i.edad_m i.hij_total i.ano_nac#i.mes_nac i.ccode

foreach yvar in peso lbw talla semanas premature {
    *eststo: xtreg `yvar' `controls' postChCC if beforeafter==1, `fse'
    *eststo: xtreg `yvar' `controls' postChCC if beforeafter==1&hij_tot<=2, `fse'
    eststo: xtreg `yvar' `controls' postChCC, `fse'
}
lab var peso      "Birth Weight"
lab var lbw       "LBW"
lab var talla     "Size"
lab var semanas   "Gestation"
lab var premature "Premature"
lab var postChCC  "Participated in ChCC"

#delimit ;
esttab est1 est2 est3 est4 est5 using "$OUT/motherFE.tex", booktabs
b(%-9.3f) se(%-9.3f) brackets starlevel("*" 0.10 "**" 0.05 "***" 0.01) 
stats(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
keep(postChCC _cons) label replace
mtitles("Weight" "LBW" "Size" "Gestation" "Premature")
title("Estimated Program Effects with Mother Fixed Effects"\label{mFE})
postfoot("\bottomrule\multicolumn{6}{p{14.2cm}}{\begin{footnotesize}   "
         "Estimation sample consists of all births where the data      "
         "link exists between the child and the mother's participation "
         "in social programs, including ChCC.  Additional details      "
         "regarding this procedure are provided in Appendix \ref{MFE}. "
         "In each case mother's fixed effects are included, and full   "
         "fixed effects for mother's age at birth, child birth order,  "
         "and child's month and municipality of birth are included.    "
         "Standard errors are clustered by mother."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr


*--------------------------------------------------------------------------------
*--- (4) Multiple hypothesis testing
*--------------------------------------------------------------------------------
***[A -- ROMANO WOLF]
local fse fe vce(cluster run_padre_falso)
local controls i.edad_m i.hij_total i.ano_nac

do rwolf/rwolf.ado
#delimit ;
rwolf peso lbw talla semanas premature, indepvar(postChCC)
      method(xtreg) `fse' verbose reps(50) controls(`controls');
#delimit cr

***[B -- ANDERSON INDEX]
local yvars peso lbw talla semanas premature
foreach var of varlist lbw premature {
    replace `var'=-1*`var'
}
local j=1
foreach var of varlist `yvars' {
    sum `var' if postChCC==0
    local stdev = r(sd)
    sum `var'
    local mean  = r(mean)
    gen z_`j'=(`var'-`mean')/`stdev'
    local ++j
}
foreach var of varlist lbw vlbw premature {
    replace `var'=-1*`var'
}
corr z_1 z_2 z_3 z_4 z_5, covariance
mat def SIGMA = r(C)
mat def I     = J(5,1,1)
mat def sbarpartial = inv((I'*inv(SIGMA)*I))*(I'*inv(SIGMA))
svmat sbarpartial, names(wts)

gen sbar=0
foreach num of numlist 1(1)5 {
    sum wts`num'
    replace wts`num'=r(mean)
    gen temp = wts`num'*z_`num'
    replace sbar=sbar+temp if temp!=.
    drop temp
}
sum sbar
replace sbar=. if sbar==0
sum sbar

xtreg sbar `controls' postChCC, `fse'
xtreg sbar `controls' postChCC if edad_m<=45, `fse'
xtreg sbar `controls' postChCC if beforeafter==1, `fse'

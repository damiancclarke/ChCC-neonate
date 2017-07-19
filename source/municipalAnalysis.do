/* municipalAnalysis.do          damiancclarke             yyyy-mm-dd:2017-07-18
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

 Municipal level descriptives and analysis of the impacts of Chile Crece Contigo
on health at birth.  

 To replicate this file the globals on lines 20-22 should be replaced with the
folder location.

*/

vers 11
clear all
set more off
cap log close
set matsize 1000
*--------------------------------------------------------------------------------
*--- (0) Globals, Log
*--------------------------------------------------------------------------------
global DAT "~/investigacion/2016/ChCC-neonate/data"
global OUT "~/investigacion/2016/ChCC-neonate/results"
global LOG "~/investigacion/2016/ChCC-neonate/log"

log using "$LOG/municipalAnalysis.txt", text replace
cap mkdir "$OUT/comunaTrends"
cap mkdir "$OUT/comunaTrends/bwt"

*--------------------------------------------------------------------------------
*--- (1) Open data, basic clean
*--------------------------------------------------------------------------------
use "$DAT/Defunciones_2002_2010"
rename ano_def anio

merge m:1 comuna anio using $DAT/crosswalk
keep if anio>2002
drop if _merge==2
gen fetalDeath=1
collapse (sum) fetalDeath, by(mes_def anio ccode)
rename mes_def mes_nac
rename anio    ano_nac

tempfile fetalDeaths
save `fetalDeaths'

gen trimester_nac=ceil(mes_nac/3)
collapse (sum) fetalDeath, by(trimester_nac ano_nac ccode)
tempfile fetalDeathsTri
save `fetalDeathsTri'


use "$DAT/nacimientos2000-2010", clear
destring ano_nac mes_nac comuna peso talla edad_m hij_tot, replace
keep if ano_nac>2002
gen anio  = ano_nac
gen month = mes_nac


merge m:1 comuna anio using $DAT/crosswalk
drop if _merge==2
drop _merge

merge m:1 ccode using $DAT/early-MDS
gen early = _merge==3
drop _merge
gen     chcc = chcc_gestante=="SI" if chcc_gestante!=""
replace chcc = . if dn_sexo_padre=="MASCULINO"
replace chcc = 0 if early==1&anio<2007
replace chcc = 0 if early==1&anio==2007&mes_nac<=6
replace chcc = 0 if early==0&anio<2008

merge m:1 ccode using $DAT/controls/census2002baseline.dta
#delimit ;
local controls Sin_discapacidad Pueblo_indigena edu_basica edu_media
               edu_superior Vivienda_particular Agua_c;
#delimit cr
sum `controls'
drop _merge

replace peso     = . if peso<500|peso>6000
replace talla    = . if talla<20|talla==99
replace edad_m   = . if edad_m<12 | edad_m>=50
replace hij_total= . if hij_total > 15

destring semanas, gen(gestation)
replace gestation=. if gestation>45

gen nbirth    = 1
gen lbw       = peso<2500    if peso      != .
gen premature = gestation<37 if gestation != .


*--------------------------------------------------------------------------------
*--- (2) Descriptive Plot of ChCC rollout
*--------------------------------------------------------------------------------
preserve
collapse chcc, by(anio mes_nac)

gen    time = (anio-2003)*12+mes_nac
format chcc     %03.1f

#delimit ;
twoway line chcc time, scheme(s1mono) lcolor(ebblue) lwidth(medthick)
ytitle("Proportion of Births Covered by ChCC") ylabel(, angle(0  ))
xlabel(1 "Jan 2003" 13 "Jan 2004" 25 "Jan 2005" 37 "Jan 2006" 49 "Jan 2007"
       61 "Jan 2008" 73 "Jan 2009" 85 "Jan 2010", angle(45))
xtitle("Month of Birth");
#delimit cr
graph export "$OUT/ChCCtime.eps", replace
restore


*--------------------------------------------------------------------------------
*--- (3) Collapse to Municipal*month level
*--------------------------------------------------------------------------------
preserve
local yvars peso lbw talla gestation premature
collapse chcc `yvars' `controls' (sum) nbirth, by(comunaname ccode ano_nac mes_nac)

merge 1:1 mes_nac ano_nac ccode using `fetalDeaths'
replace fetalDeath=0 if fetalDeath==.
gen     fDeathRate=fetalDeath/nbirth*1000

gen time = (ano_nac-2000)*12+mes_nac
local tcontrols
foreach var of varlist `controls' {
    local tcontrols `tcontrols' c.`var'#c.time
}

*--------------------------------------------------------------------------------
*--- (4) Municipal-level summary statistics
*--------------------------------------------------------------------------------
lab var peso      "Birth Weight (grams)"
lab var lbw       "Low Birth Weight $< 2500$ grams"
*lab var vlbw      "Very Low Birth Weight $< 1500$ grams"
lab var talla     "Length (cm)"
lab var gestation "Gestation (weeks)"
lab var premature "Premature $< 37$ weeks"
lab var nbirth    "Number of Births"
lab var chcc      "Proportion Enrolled in ChCC"
lab var fDeathRat "Rate of Fetal Deaths/1000 Births"

#delimit ;
estpost sum chcc peso lbw gestation premature talla fDeathRate nbirth;
estout using "$OUT/SummaryMunicipal-update.tex", replace label style(tex)
cells("count(label(N)) mean(fmt(2) label(Mean)) sd(fmt(2) label(Std.\ Dev.))
min(fmt(2) label(Min)) max(fmt(2) label(Max))");
#delimit cr


*--------------------------------------------------------------------------------
*--- (5) Main regressions
*--------------------------------------------------------------------------------    
foreach var of varlist `yvars' fDeathRate {
    eststo: areg `var' i.time             chcc [aw=nbirth], abs(comunaname) `se'
    eststo: areg `var' i.time `tcontrols' chcc [aw=nbirth], abs(comunaname) `se'
    eststo: areg `var' i.time             chcc            , abs(comunaname) `se'
}
lab var chcc "Proportion of ChCC coverage"
#delimit ;
esttab est1 est4 est7 est10 est13 est16 using "$OUT/comunaDD.tex",
booktabs b(%-9.3f) se(%-9.3f) brackets stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(chcc _cons) label replace
mtitles("Weight" "LBW" "Size" "Gestation" "Premature" "Fetal Death") 
title("Difference-in-Difference Estimates using Municipal Variation in Coverage"
      \label{mDD})
postfoot("\bottomrule\multicolumn{7}{p{16.8cm}}{\begin{footnotesize} Estimation sample "
         "consists of all municipal-level averages for each month between 2003 and 2010"
         "for all women. Low birth weight refers to the proportion of births under     "
         "2,500 grams, and premature refers to the proportion of births ocurring before"
         "37 weeks of gestation.  Fetal deaths are measured as the number of fetal     "
         "deaths per 1,000 live births.  Each cell is weighted using the number of     "
         "births in the municipality and month, and all specifications include         "
         "municipality and time (Year $\times$ Month) fixed effects. "
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);

esttab est2 est5 est8 est11 est14 est17 using "$OUT/comunaDDtrends.tex",
booktabs b(%-9.3f) se(%-9.3f) brackets stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(chcc _cons) label replace
mtitles("Weight" "LBW" "Size" "Gestation" "Premature" "Fetal Death") 
title("Difference-in-Difference Estimates with Linear Time Trends"
      \label{mDD})
postfoot("\bottomrule\multicolumn{7}{p{16.8cm}}{\begin{footnotesize} Estimation sample "
         "consists of all municipal-level averages for each month between 2003 and 2010"
         "for all women. Low birth weight refers to the proportion of births under     "
         "2,500 grams, and premature refers to the proportion of births ocurring before"
         "37 weeks of gestation.  Each cell is weighted using the number of births in  "
         "the municipality and month, and all specifications include municipality and  "
         "time (Year $\times$ Month) fixed effects. "
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);

esttab est3 est6 est9 est12 est15 est18 using "$OUT/comunaDDweight.tex",
booktabs b(%-9.3f) se(%-9.3f) brackets stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(chcc _cons) label replace
mtitles("Weight" "LBW" "Size" "Gestation" "Premature" "Fetal Death") 
title("Difference-in-Difference Estimates Without Municipality Weights"
      \label{mDD})
postfoot("\bottomrule\multicolumn{7}{p{16.8cm}}{\begin{footnotesize} Estimation sample "
         "consists of all municipal-level averages for each month between 2003 and 2010"
         "for all women. Low birth weight refers to the proportion of births under     "
         "2,500 grams, and premature refers to the proportion of births ocurring before"
         "37 weeks of gestation.  Each cell is weighted using the number of births in  "
         "the municipality and month, and all specifications include municipality and  "
         "time (Year $\times$ Month) fixed effects. "
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

restore

*--------------------------------------------------------------------------------
*--- (6) Distributional Impacts (birth weight)
*--------------------------------------------------------------------------------    
***ADD GESTATION HERE AND PLOT OUTPUT
preserve
local yvarsp
local yvarso
foreach num of numlist 5(5)95 {
    local yvarsp `yvarsp' (p`num') p`num'=peso
    local yvarso `yvarso' p`num'
}

collapse chcc `yvarsp' (sum) nbirth, by(comunaname ano_nac mes_nac)
sum p*

gen time = (ano_nac-2000)*12+mes_nac
egen ccode = group(comunaname)

foreach var of varlist `yvarso' {
    eststo: areg `var' i.time chcc [aw=nbirth], abs(comunaname) `se'
    replace `var'=log(`var')
    eststo: areg `var' i.time chcc [aw=nbirth], abs(comunaname) `se'
}
estimates clear
restore

*--------------------------------------------------------------------------------
*--- (7a) FPS
*--------------------------------------------------------------------------------
destring puntaje_fps_sept2013, replace
histogram puntaje_fps_sept2013, bcolor(blue) scheme(s1mono) freq
graph export ficha.eps, replace
xtile ficha=puntaje_fps_sept2013, nquantiles(10)
gen quintile=_n in 1/10

replace chcc=. if dn_sexo_padre=="MASCULINO"
foreach var of varlist peso lbw talla gestation premature {
    gen impact = .
    gen UB     = .
    gen LB     = .
    foreach num of numlist 1(1)10 {
        preserve
        keep if ficha==`num'
        collapse `var' chcc (sum) nbirth, by(comunaname ano_nac mes_nac)

        gen time = (ano_nac-2000)*12+mes_nac
        egen ccode = group(comunaname)
        local se cluster(comunaname)
        areg `var' i.time chcc [aw=nbirth], abs(comunaname) `se'
        restore
        replace impact = _b[chcc] in `num'
        replace UB     = _b[chcc]+1.96*_se[chcc] in `num'
        replace LB     = _b[chcc]-1.96*_se[chcc] in `num'
        if `num'==10 {
            #delimit ;
            twoway scatter impact quintile, msymbol(O) mcolor(red)
            ||     rcap UB LB quintile, lcolor(black) lpattern(dash)
            scheme(s1mono) xtitle("Quintile of Social Protection Score")
            ytitle("Impact of ChCC") xlabel(1(1)10) yline(0, lcolor(gs14))
            legend(lab(1 "Point Estimate") lab(2 "95% CI"));
            graph export "$OUT/FPS_`var'.eps", replace;
            #delimit cr
            drop impact UB LB
        }
    }
}

*--------------------------------------------------------------------------------
*--- (7b) Subgroups
*--------------------------------------------------------------------------------
destring edad_m, replace
destring urb_rural, replace
destring est_civ_m, replace
destring nivel_m, replace

#delimit ;
gen urban = (urb_rural==1&(ano_nac==2003|ano_nac==2005|ano_nac==2006|
                           ano_nac==2008|ano_nac==2009|ano_nac==2010))|
            (urb_rural==0&(ano_nac==2004|ano_nac==2007));
gen married=est_civ_m==1;
local groups nivel_m<4 nivel_m>=4 urban==1 urban==0 married==1 married==0
             edad_m<20 edad_m>19;                
local names  loweduc higheduc urban rural married unmarried teen nonteen;
#delimit cr

tokenize `groups'
foreach subgroup of local names {
    dis "`subgroup'"
    preserve
    keep if `1'
    
    local yvars peso lbw talla gestation premature
    collapse chcc early `yvars' (sum) nbirth, by(comunaname ano_nac mes_nac)

    gen time = (ano_nac-2000)*12+mes_nac
    egen ccode = group(comunaname)

    local se cluster(comunaname)
    foreach var of varlist `yvars' {
        eststo: areg `var' i.time chcc [aw=nbirth], abs(comunaname) `se'
    }
    lab var chcc "Proportion of ChCC coverage"
    #delimit ;
    esttab est1 est2 est3 est4 est5 using "$OUT/comunaDD_`subgroup'.tex",
    booktabs b(%-9.3f) se(%-9.3f) brackets stats
    (N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
    starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(chcc _cons)
    mtitles("Weight" "LBW" "Size" "Gestation" "Premature") label replace
    title("Difference-in-Difference Estimates: `subgroup'"\label{mDD-`subgroup'})
    postfoot("\bottomrule\multicolumn{6}{p{14.8cm}}{\begin{footnotesize} Estimation "
             "sample consists of all municipal-level averages for `subgroup' women  "
             "each month between 2003 and 2010. Refer to notes in table \ref{mDD}   "
             "for additional details."
             "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
             "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
    #delimit cr
    estimates clear
    
    macro shift
    restore
}

**Geography
foreach region of numlist 1(1)15 {
    preserve
    keep if region==`region'
    
    local yvars peso lbw talla gestation premature
    collapse chcc early `yvars' (sum) nbirth, by(comunaname ano_nac mes_nac)

    gen time = (ano_nac-2000)*12+mes_nac
    egen ccode = group(comunaname)

    local se cluster(comunaname)
    foreach var of varlist `yvars' {
        eststo: areg `var' i.time chcc [aw=nbirth], abs(comunaname) `se'
    }
    lab var chcc "Proportion of ChCC coverage"
    #delimit ;
    esttab est1 est2 est3 est4 est5 using "$OUT/comunaDD_region`region'.tex",
    booktabs b(%-9.3f) se(%-9.3f) brackets stats
    (N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
    starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(chcc _cons)
    mtitles("Weight" "LBW" "Size" "Gestation" "Premature") label replace
    title("Difference-in-Difference Estimates: Region `region'"\label{mDD-`region'})
    postfoot("\bottomrule\multicolumn{6}{p{14.8cm}}{\begin{footnotesize} Estimation  "
             "sample consists of all municipal-level averages for birth occurring to "
             "women in region `region' "
             "each month between 2003 and 2010. Refer to notes in table \ref{mDD}   "
             "for additional details."
             "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
             "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
    #delimit cr
    estimates clear
    
    macro shift
    restore
}


**Time
foreach year of numlist 2003(1)2007 {
    preserve
    keep if ano_nac>=`year'&ano_nac<2010
    
    local yvars peso lbw talla gestation premature
    collapse chcc early `yvars' (sum) nbirth, by(comunaname ano_nac mes_nac)

    gen time = (ano_nac-`year')*12+mes_nac
    egen ccode = group(comunaname)

    local se cluster(comunaname)
    foreach var of varlist `yvars' {
        eststo: areg `var' i.time chcc [aw=nbirth], abs(comunaname) `se'
    }
    lab var chcc "Proportion of ChCC coverage"
    #delimit ;
    esttab est1 est2 est3 est4 est5 using "$OUT/comunaDD_`year'P.tex",
    booktabs b(%-9.3f) se(%-9.3f) brackets stats
    (N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
    starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(chcc _cons)
    mtitles("Weight" "LBW" "Size" "Gestation" "Premature") label replace
    title("Difference-in-Difference Estimates: Years $\geq$ `year'"\label{mDD-`year'})
    postfoot("\bottomrule\multicolumn{6}{p{14.6cm}}{\begin{footnotesize} Estimation  "
             "sample consists of all municipal-level averages for birth occurring to "
             "all women each month between `year' and 2010. Refer to notes in table  "
             "\ref{mDD} for additional details."
             "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
             "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
    #delimit cr
    estimates clear
    
    macro shift
    restore
}

*--------------------------------------------------------------------------------
*--- (8) Placebo tests
*--------------------------------------------------------------------------------
preserve
local yvars peso lbw talla gestation premature
collapse chcc `yvars' (sum) nbirth, by(comunaname ccode ano_nac mes_nac)

merge 1:1 mes_nac ano_nac ccode using `fetalDeaths'
replace fetalDeath=0 if fetalDeath==.
gen     fDeathRate=fetalDeath/nbirth*1000
gen time = (ano_nac-2000)*12+mes_nac

local Nplacebo 40
gen placebo     = 0 in 1/`Nplacebo'
foreach var of varlist `yvars' fDeathRate {
    gen `var'placebo = 0 in 1/`Nplacebo'
    gen `var'pLB     = 0 in 1/`Nplacebo'
    gen `var'pUB     = 0 in 1/`Nplacebo'
}
foreach pl of numlist 1(1)`Nplacebo' {
    bys comunaname (ano_nac mes_nac): gen chccplacebo=chcc[_n-`pl']
    local se cluster(comunaname)
    foreach var of varlist `yvars' fDeathRate {
        areg `var' i.time chccplacebo [aw=nbirth], abs(comunaname) `se'
    
        replace `var'placebo = _b[chccplacebo] in `pl'
        replace `var'pLB     = _b[chccplacebo]-1.96*_se[chccplacebo] in `pl'
        replace `var'pUB     = _b[chccplacebo]+1.96*_se[chccplacebo] in `pl'
    }
    replace placebo     = -`pl' in `pl'
    drop chccplacebo
}
foreach var of varlist `yvars' fDeathRate {
    #delimit ;
    twoway rcap `var'pLB `var'pUB placebo, lcolor(black) scheme(s1mono) ||
           scatter `var'placebo placebo, mcolor(blue) yline(0)
    ytitle("`var'") xtitle("Placebo") legend(order(2 "Estimate" 1 "95% CI"));
    #delimit cr
    graph export "$OUT/placebolag_`var'.eps", replace
}


*--------------------------------------------------------------------------------
*--- (9) Multiple hypothesis testing
*--------------------------------------------------------------------------------
***[A -- ROMANO WOLF]
do rwolf/rwolf.ado
#delimit ;
rwolf `yvars' fDeathRate [aw=nbirth], indepvar(chcc) controls(i.time) 
      abs(comunaname) cluster(comunaname) method(areg) v seed(223) reps(25);
#delimit cr
exit

***[B -- ANDERSON INDEX]
local yvars peso lbw talla gestation premature fDeathRate
foreach var of varlist lbw premature fDeathRate {
    replace `var'=-1*`var'
}
local j=1
foreach var of varlist `yvars' fDeathRate {
    sum `var' if chcc==0
    local stdev = r(sd)
    sum `var'
    local mean  = r(mean)
    gen z_`j'=(`var'-`mean')/`stdev'
    local ++j
}
foreach var of varlist lbw premature fDeathRate {
    replace `var'=-1*`var'
}
corr z_1 z_2 z_3 z_4 z_5 z_6, covariance
mat def SIGMA = r(C)
mat def I     = J(6,1,1)
mat def sbarpartial = inv((I'*inv(SIGMA)*I))*(I'*inv(SIGMA))
svmat sbarpartial, names(wts)

gen sbar=0
foreach num of numlist 1(1)6 {
    sum wts`num'
    replace wts`num'=r(mean)
    gen temp = wts`num'*z_`num'
    replace sbar=sbar+temp if temp!=.
    drop temp
}
sum sbar
replace sbar=. if sbar==0
sum sbar
areg sbar i.time chcc [aw=nbirth], abs(comunaname) cluster(comunaname)
restore

*--------------------------------------------------------------------------------
*--- (10) Based on trimester
*--------------------------------------------------------------------------------
preserve
gen trimester_nac=ceil(mes_nac/3)

local yvars peso lbw talla gestation premature
collapse chcc early `yvars' (sum) nbirth, by(comunaname ano_nac trimester_nac ccode)

gen time = (ano_nac-2003)*4+trimester_nac

merge 1:1 trimester_nac ano_nac ccode using `fetalDeathsTri'
replace fetalDeath=0 if fetalDeath==.
gen     fDeathRate=fetalDeath/nbirth*1000


local se cluster(comunaname)
foreach var of varlist `yvars' fDeathRate {
    eststo: areg `var' i.time chcc [aw=nbirth], abs(comunaname) `se'
}
lab var chcc "Proportion of ChCC coverage"
#delimit ;
esttab est1 est2 est3 est4 est5 est6 using "$OUT/comunaDD-trimester.tex",
booktabs b(%-9.3f) se(%-9.3f) brackets stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(chcc _cons) label replace
mtitles("Weight" "LBW" "Size" "Gestation" "Premature" "Fetal Death")
title("Difference-in-Difference Estimates with Data Collapsed by Trimester"
      \label{mDDt})
postfoot("\bottomrule\multicolumn{7}{p{16.2cm}}{\begin{footnotesize} Estimation sample "
         "consists of all municipal-level averages for each quarter between 2003 and   "
         "2010 for all women. Refer to additional notes in table \ref{mDD}. "
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);

#delimit cr
estimates clear
restore


exit
    
*--------------------------------------------------------------------------------
*--- (6) Based on conception dates
*--------------------------------------------------------------------------------
*replace month = "0" + month if length(month)==1
*gen day = dia_nac
*replace day = "0" + day if length(day)==1
*egen birthdate = concat(day month ano_nac)
*gen bdaycode = date(birthdate, "DMY")
*destring semanas, gen(gestation)
*replace gestation=. if gestation>45
*gen daysgest = gestation*7
*gen concepDay = bdaycode - daysgest
*format concepDay %td
*gen concepMonth = month(concepDay)
*gen concepYear  = year(concepDay)

*collapse chcc early peso, by(comunaname concepYear concepMonth)

*keep if concepYear>1998
*gen time = (concepYear-1999)*12+concepMonth

*/
exit

*--------------------------------------------------------------------------------
*--- (8) Make data of comuna trends
*--------------------------------------------------------------------------------
file open myfile1 using "$OUT/comunaTrends/cgraphs.tex", write replace
file open myfile2 using "$OUT/comunaTrends/bwt/cgraphs.tex", write replace
file open myfile3 using "$OUT/comunaTrends/edits/cgraphs.tex", write replace
*keep if time<132&time>10
encode comunaname, gen(ccode)
xtset ccode time
bys ccode: gen movavg = (F1.chcc + chcc + L1.chcc)/3
bys ccode: gen difav = movavg[_n]-movavg[_n-1]
bys comunaname: egen MD = max(difav) if ano_nac >=2007&ano_nac<=2009
gen adopmonth = MD ==difav
replace adopmonth = 0 if difav ==.
sort comunaname ano_nac mes_nac
bys comunaname: gen adopted = sum(adopmonth)


replace comunaname=subinstr(comunaname,"á","a",.)
replace comunaname=subinstr(comunaname,"é","e",.)
replace comunaname=subinstr(comunaname,"í","i",.)
replace comunaname=subinstr(comunaname,"ó","o",.)
replace comunaname=subinstr(comunaname,"ú","u",.)
replace comunaname=subinstr(comunaname,"ñ","n",.)
replace comunaname=subinstr(comunaname,"Á","A",.)
replace comunaname=subinstr(comunaname,"É","E",.)
replace comunaname=subinstr(comunaname,"Í","I",.)
replace comunaname=subinstr(comunaname,"Ó","O",.)
replace comunaname=subinstr(comunaname,"Ú","U",.)
merge m:1 comunaname using "$DAT/adoptionDatesDGD", force
gen adopted2 = ano_nac>adopYear
replace adopted2 = 1 if ano_nac==adopYear&mes_nac>adopMonth

sort time
levelsof comunaname, local(comunas)
local j=1
foreach c of local comunas {
    local el "Late Adopter"
    local line 98
    sum early if comunaname==`"`c'"'
    if r(mean)==1 local el "Early Adopter" 
    if r(mean)==1 local line 92
    
    #delimit ;
    twoway line chcc time if comunaname==`"`c'"' ||
           line adopted time if comunaname==`"`c'"', lpattern(dash) yaxis(2)
    scheme(s1mono) title("`c'")
    xline(`line', lcolor(red)) subtitle("`el'");
    graph export "$OUT/comunaTrends/comuna_`j'.eps", as(eps) replace;

    twoway line chcc time if comunaname==`"`c'"' ||
           line adopted2 time if comunaname==`"`c'"', lpattern(dash) yaxis(2)
    scheme(s1mono) title("`c'")
    xline(`line', lcolor(red)) subtitle("`el'");
    graph export "$OUT/comunaTrends/edits/comuna_`j'.eps", as(eps) replace;

    twoway line chcc time if comunaname==`"`c'"' ||
           line peso time if comunaname==`"`c'"', lpattern(dash) yaxis(2)
    scheme(s1mono) title("`c'")
    xline(`line', lcolor(red)) subtitle("`el'");
    graph export "$OUT/comunaTrends/bwt/comuna_`j'.eps", as(eps) replace;

    file write myfile1 "\begin{figure}[htpb!]" _n "\begin{center}" _n
    "\includegraphics{comuna_`j'.eps}" _n "\end{center}" _n "\end{figure}" _n _n;
    if mod(`j',4)==0 file write myfile1 "\clearpage" _n;

    file write myfile2 "\begin{figure}[htpb!]" _n "\begin{center}" _n
    "\includegraphics{comuna_`j'.eps}" _n "\end{center}" _n "\end{figure}" _n _n;
    if mod(`j',4)==0 file write myfile2 "\clearpage" _n;

    file write myfile3 "\begin{figure}[htpb!]" _n "\begin{center}" _n
    "\includegraphics{comuna_`j'.eps}" _n "\end{center}" _n "\end{figure}" _n _n;
    #delimit cr
    if mod(`j',4)==0 file write myfile3 "\clearpage" _n

    local ++j
}


***MICRO LEVEL SUM-STATS
gen ChCC = chcc_gestante=="SI"
lab var peso      "Birth weight (grams)"
lab var semanas   "Gestation (weeks)"
lab var lbw       "Low Birth Weight ($<$ 2,500 grams)"
lab var vlbw      "Very Low Birth Weight $< 1500$ grams"
lab var premature "Premature ($<$ 37 weeks)"
lab var gestation "Gestation (weeks)"
lab var ChCC      "Mother Ever Participated in ChCC"
lab var edad_m    "Mother's Age (years)"
lab var hij_total "Surviving Children"
lab var talla     "Length (cm)"
/*
preserve
drop if dn_sexo_padre=="MASCULINO"
drop if chcc_gestante==""
keep if anio>2002
#delimit ;
estpost sum peso lbw vlbw talla gestation premature ChCC edad_m hij_total;
estout using "$OUT/SummaryIndividual-update.tex", replace label style(tex)
cells("count(label(N)) mean(fmt(2) label(Mean)) sd(fmt(2) label(Std.\ Dev.))
min(fmt(2) label(Min)) max(fmt(2) label(Max))");
#delimit cr
restore
*/



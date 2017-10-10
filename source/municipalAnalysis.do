/* municipalAnalysis.do          damiancclarke             yyyy-mm-dd:2017-07-18
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

 Municipal level descriptives and analysis of the impacts of Chile Crece Contigo
on health at birth.  

 To replicate this file the globals on lines 20-22 should be replaced with the
folder location.

TODO:
 - Plot proportion of ChCC post treatment (histogram)
 - Plot proportion of ChCC post treatment vs other variables

*/

vers 11
clear all
set more off
cap log close
set matsize 4000
*--------------------------------------------------------------------------------
*--- (0) Globals, Log
*--------------------------------------------------------------------------------
global DAT "~/investigacion/2016/ChCC-neonate/data"
global OUT "~/investigacion/2016/ChCC-neonate/results"
global LOG "~/investigacion/2016/ChCC-neonate/log"
global MAP "~/investigacion/2016/ChCC-neonate/data/maps"

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


merge m:1 comuna anio using "$DAT/crosswalk_servsalud"
drop if _merge==2
drop _merge

merge m:1 anio month servcode using "$DAT/programComponents.dta"
drop _merge

*merge m:1 ccode using $DAT/early-MDS-dates.dta
merge m:1 ccode using $DAT/MDS-dates-complete.dta
drop if _merge==2
*gen early = _merge==3
drop _merge
gen     chcc = chcc_gestante=="SI" if chcc_gestante!=""
replace chcc = . if dn_sexo_padre=="MASCULINO"
replace chcc = 0 if ano_nac<adopYear
replace chcc = 0 if ano_nac==adopYear&mes_nac<adopMonth
gen ChCCTime = (ano_nac-adopYear)*12+(mes_nac-adopMonth)+1

gen chccAvailable = ano_nac>adopYear
replace chccAvailable = 1 if ano_nac==adopYear&mes_nac>=adopMonth


merge m:1 ccode using $DAT/controls/census2002baseline.dta
#delimit ;
local controls Sin_discapacidad Pueblo_indigena edu_basica edu_media
               edu_superior Vivienda_particular Agua_c;
#delimit cr
sum `controls'
drop _merge

merge m:1 anio comunaname using "$DAT/controls/alcaldes2003_2010"
gen LCR = 1 if party=="PC"|party=="PS"|party=="PH"
replace LCR = 3 if party =="UDI"|party=="RN"
replace LCR = 2 if LCR==.
drop if _merge==2
drop _merge

merge m:1 ccode ano_nac using "$DAT/ComunaControls_Full.dta"
gen fonasaPC2064 = poblacionfonasa2064/poblacion
#delimit ;
local tvcontrol ingresosPC aguapotable fonasaPC FPSanioPC subEscPC
capacitacionesPC ingresoSaludPC transferEducPC pobrezacasen fonasaPC2064;
local otvcontrol o_ingresosPC o_aguapotable o_fonasaPC o_FPSanioPC
o_subEscPC o_capacitacionesPC o_ingresoSaludPC o_transferEducPC
o_pobrezacasen;
#delimit cr
drop if _merge==2
drop _merge
drop if n==.

replace peso     = . if peso<500|peso>6000
replace talla    = . if talla<20|talla==99
replace edad_m   = . if edad_m<12 | edad_m>=50
replace hij_total= . if hij_total > 15
gen     teen     = edad_m<20 if edad_m! = .
destring curso_m, replace
destring nivel_m, replace
gen     meduc    = curso_m if nivel_m==4
replace meduc    = 8+curso_m if nivel_m==2|nivel_m==3
replace meduc    = 12+curso_m if nivel_m==1
replace meduc    = 0 if nivel_m==5

destring semanas, gen(gestation)
replace gestation=. if gestation>45

gen nbirth    = 1
gen lbw       = peso<2500    if peso      != .
gen premature = gestation<37 if gestation != .

gen ChileSolidario = chs=="SI"
replace ChileSolidario = . if dn_sexo_padre=="MASCULINO"
bys anio month servcode: egen birthsService  = total(nbirth)
bys anio month servcode: egen ChileSolServ   = total(ChileSolidario)


*replace i_puritaMama_ma9=i_puritaMama_ma9+i_puritaFortificada_ma9

gen mech_controlPrenat = i_controlesPrenatales_ma9/birthsService
gen mech_controlObstet = i_obstetricControls_ma9/birthsService
gen mech_homeVisits    = i_homeVisits_ma9/birthsService
gen mech_asistenciaSoc = i_asistenciaSocial_ma9/birthsService
gen mech_ChileSolidario= ChileSolidario
gen mech_puritaMama    = i_puritaMama_ma9/birthsService
gen mech_puritaFortif  = (i_puritaMama_ma9+i_puritaFortificada_ma9)/birthsService


*--------------------------------------------------------------------------------
*--- (2) Descriptive Plot of ChCC rollout
*--------------------------------------------------------------------------------
preserve
use $DAT/MDS-dates-complete.dta, clear
gen ncomunas = 1
collapse (sum) ncomunas, by(adopMonth adopYear)
set obs 14
replace adopYear=2007 if adopYear==.
replace adopMonth = 11 in 13
replace adopMonth = 12 in 14
replace ncomunas=0 if ncomunas==.
sort adopYear adopMonth
gen numcomunas = sum(ncomunas)
rename adopYear  anio
rename adopMonth mes_nac
keep numcomunas anio mes_nac
tempfile ncoms
save `ncoms'
restore

*replace puritaFortificada=puritaFortificada+puritaMama
#delimit ;
local mvars puritaFortificada obstetricControls homeVisits asistenciaSocial
            controlesPrenatales puritaMama;
local imvars i_puritaFortificada i_obstetricControls   i_homeVisits
             i_asistenciaSocial  i_controlesPrenatales i_puritaMama;
#delimit cr
preserve
collapse chcc chccAvailable ChileSoli (sum) `mvars' `imvars', by(anio mes_nac)
merge 1:1 anio mes_nac using `ncoms'
replace numcomunas = 0   if _merge!=3 & anio<=2007
replace numcomunas = 346 if _merge!=3 & anio>=2008
lab var numcomunas   "Number of Municipalities Participating"
lab var homeVisits   "Number of Home Visits to Pregnant Women"
lab var puritaFortif "Fortified Milk Powder"
lab var puritaMama   "Fortified Milk (Improved Formula)"
lab var obstetricControls "Number of Obstetric Controls"
lab var homeVisits   "Number of Home Visits"
lab var asistenciaSocial "Number of Visits with Social Assistant"
lab var controlesPrenatales "Number of Prenatal Controls"
lab var ChileSolidario "Proportion of Mothers Ever in Chile Solidario"

gen    time = (anio-2003)*12+mes_nac
format chcc     %03.1f

#delimit ;
twoway line chcc time, scheme(s1mono) lcolor(ebblue) lwidth(medthick) ||
       line numcomunas time, lcolor(red) lpattern(dash) yaxis(2)
ytitle("Proportion of Births Covered by ChCC") ylabel(, angle(0  ))
xlabel(1 "Jan 2003" 13 "Jan 2004" 25 "Jan 2005" 37 "Jan 2006" 49 "Jan 2007"
       61 "Jan 2008" 73 "Jan 2009" 85 "Jan 2010", angle(45))
xtitle("Month of Birth")
legend(lab(1 "Proportion of Births") lab(2 "Number of Municipalities"));
graph export "$OUT/ChCCtime.eps", replace;
#delimit cr

keep if time>=25
foreach var of varlist `mvars' {
    replace `var'=`var'/1000
    #delimit ;
    twoway line chcc time, scheme(s1mono) lcolor(ebblue) lwidth(medthick) ||
           line `var' time, lcolor(red) lpattern(dash) lwidth(thick) yaxis(2)
    ytitle("Proportion of Births Covered by ChCC") ylabel(, angle(0  ))
    xlabel(25 "Jan 2005" 37 "Jan 2006" 49 "Jan 2007"
           61 "Jan 2008" 73 "Jan 2009" 85 "Jan 2010", angle(45))
    xtitle("Month of Birth")  ylabel(, angle(0  ) axis(2))
    legend(lab(1 "Proportion of Births") lab(2 "Number of Units/1000"));
    #delimit cr
    graph export "$OUT/ChCCmechanism_`var'.eps", replace
}
#delimit ;
twoway line chcc time, scheme(s1mono) lcolor(ebblue) lwidth(medthick) ||
line ChileSolida time, lcolor(red) lpattern(dash) lwidth(thick) yaxis(2)
    ytitle("Proportion of Births Covered by ChCC") ylabel(, angle(0  ))
    xlabel(25 "Jan 2005" 37 "Jan 2006" 49 "Jan 2007"
           61 "Jan 2008" 73 "Jan 2009" 85 "Jan 2010", angle(45))
    xtitle("Month of Birth")  ylabel(, angle(0  ) axis(2))
    legend(lab(1 "Proportion of Births") lab(2 "Proportion in Chile Solidario"));
    #delimit cr
    graph export "$OUT/ChCCmechanism_ChileSolidario.eps", replace

restore


preserve
insheet using "$DAT/birthPublic.csv", comma names clear
keep if year>2001
gen proportionPublic = public/all
gen time = (year-2002)*12+month

#delimit ;
twoway connected proportionPublic time, lwidth(thin) lcolor(gs10)
msymbol(smsquare) mcolor(emerald) scheme(s1mono)
ytitle("Births in the Public Health System")
xlabel(1  "Jan 2002" 13 "Jan 2003" 25 "Jan 2004" 37 "Jan 2005" 49 "Jan 2006"
       61 "Jan 2007" 73 "Jan 2008" 85 "Jan 2009" 97 "Jan 2010", angle(45))
xtitle("Time") xline(66 78, lpattern(dash));
graph export "$OUT/birthsPublic.eps", replace;
#delimit cr
restore

preserve
use "$DAT/MDS-dates-complete.dta", clear
rename comunaname cname
replace cname="Aysén"       if cname=="Aisén"
replace cname="Coyhaique"   if cname=="Coihaique"
replace cname="Paihuano"    if cname=="Paiguano"
replace cname="Cochamí"     if cname=="Cochamó"
replace cname="Los Alamos"  if cname=="Los Álamos"
replace cname="Los Angeles" if cname=="Los Ángeles"
replace cname="Marchigüe"   if cname=="Marchihue"
replace cname="Maullén"     if cname=="Maullín"
replace cname="Ranquil"     if cname=="Ránquil"
replace cname="Isla de Pascua"      if regexm(cname,"Pascua")
replace cname="Pedro Aguirre Cerda" if regexm(cname,"Cerda")

rename ccode COD_COMUNA
rename cname nom_com
merge 1:1 nom_com using "$MAP/ChileLite"
drop if _merge!=3

gen     adoptTime = 11 if adopYear==2007&adopMonth==6
replace adoptTime = 10 if adopYear==2007&adopMonth==7
replace adoptTime = 9  if adopYear==2007&adopMonth==8
replace adoptTime = 8  if adopYear==2007&adopMonth==9
replace adoptTime = 7  if adopYear==2007&adopMonth==10
replace adoptTime = 6  if adopYear==2008&adopMonth==1
replace adoptTime = 5  if adopYear==2008&adopMonth==2
replace adoptTime = 4  if adopYear==2008&adopMonth==3
replace adoptTime = 3  if adopYear==2008&adopMonth==4
replace adoptTime = 2  if adopYear==2008&adopMonth==5
replace adoptTime = 1  if adopYear==2008&adopMonth>=6


rename _id _ID
drop if _ID==242|_ID==336|_ID==346

#delimit ;
spmap adoptTime using "$MAP/ChileLiteCoords", id(_ID) legstyle(2)
legend(symy(*2.8) symx(*2.8) size(*3.5) rowgap(1) title("Rollout", size(*2.5))
       lab(12 "Jun 2007") lab(11 "Jul 2007") lab(10 "Aug 2007")
       lab(9 "Sep 2007") lab(8 "Oct 2007") lab(7 "Jan 2008")
       lab(6 "Feb 2008") lab(5 "Mar 2008") lab(4 "Apr 2008")
       lab(3 "May 2008") lab(2 "Jun 2008") position(11))
clmethod(custom) clbreaks(0 1.1 2.1 3.1 4.1 5.1 6.1 7.1 8.1 9.1 10.1 11.1)
fcolor(Spectral);
#delimit cr
graph export "$OUT/Rollout_Time.eps", replace

gen RM = regexm(nom_reg , "Santiago")
#delimit ;
spmap adoptTime if RM==1 using "$MAP/Chile_coords", id(_ID) legstyle(2)
legend(symy(*1) symx(*1) size(*1.2) rowgap(1) title("Rollout")
       lab(12 "Jun 2007") lab(11 "Jul 2007") lab(10 "Aug 2007")
       lab(9 "Sep 2007") lab(8 "Oct 2007") lab(7 "Jan 2008")
       lab(6 "Feb 2008") lab(5 "Mar 2008") lab(4 "Apr 2008")
       lab(3 "May 2008") lab(2 "Jun 2008") position(11))
clmethod(custom)  clbreaks(0 1.1 2.1 3.1 4.1 5.1 6.1 7.1 8.1 9.1 10.1 11.1)
fcolor(Spectral);
#delimit cr
graph export "$OUT/Rollout_Time_RM.eps", replace
restore

preserve
keep if chccAvailable==1
collapse chcc teen meduc `tvcontrol' LCR votop (sum) nbirth, by(comuna comunaname)
lab var chcc         "Proportion of Births Enrolled in ChCC"
lab var teen         "Proportion of Births to Teen Mothers"
lab var meduc        "Average Maternal Education"
lab var LCR          "Left/Centre/Right-wing Party of Mayor"
lab var votop        "Vote Share Obtained by Mayor"
lab var aguapotable  "Proportion of Residents with Treated Tap Water"
lab var fonasaPC2064 "Proportion of Adults Enrolled in Public Health"
lab var FPSanioPC    "Proportion of Population Receiving Vulnerability Score"
lab var pobrezacasen "Percent Living in Poverty"
lab var subEscPC     "Average Education Subvention per Resident"

format chcc %5.2f
#delimit ;
hist chcc, scheme(s1mono) xtitle("Chile Crece Contigo Coverage")
ytitle("Density") fcolor(gs11) lcolor(black) lwidth(vvthin);
#delimit cr
graph export "$OUT/chccUsage.eps", replace

foreach var of varlist teen meduc `tvcontrol' LCR votop {
    scatter `var' chcc [w=nbirth], msymbol(circle_hollow) scheme(s1mono)
    graph export "$OUT/comunaLevel/chcc_`var'.eps", replace
}
restore


*--------------------------------------------------------------------------------
*--- (3) Collapse to Municipal*month level
*--------------------------------------------------------------------------------
preserve
local yvars peso lbw talla gestation premature
local tvars chcc ChCCTime
local mcon  meduc edad_m teen hij_total
local group comunaname ccode ano_nac mes_nac party 
local ocon  votop mujer 
collapse `tvars' `yvars' `controls' `tvcontrol' `mcon' `ocon' (sum) nbirth, by(`group')
replace chcc=0 if chcc==.
gen chccAvailable     = ChCCTime>=1
gen chccAvailableFull = ChCCTime>=9

merge 1:1 mes_nac ano_nac ccode using `fetalDeaths'
replace fetalDeath=0 if fetalDeath==.
gen     fDeathRate=fetalDeath/nbirth*1000

gen time = (ano_nac-2000)*12+mes_nac
gen region = floor(ccode/1000)

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
lab var ano_nac   "Year of Birth"
lab var edad_m    "Mother's Age"
lab var teen      "Proportion Teen Births"
lab var hij_total "Number of Children"
lab var meduc     "Mother's Education"

#delimit ;
estpost sum chcc peso lbw gestation premat talla nbir fDeathRate ano_nac `mcon';
estout using "$OUT/SummaryMunicipal-update.tex", replace label style(tex)
cells("count mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))")
collabels(, none) mlabels(, none);
#delimit cr


*--------------------------------------------------------------------------------
*--- (5a) Main regressions
*--------------------------------------------------------------------------------    
file open pvals using "$OUT/MC_DD_porig.tex", write replace
foreach var of varlist `yvars' fDeathRate {
    eststo: areg `var' i.time             chcc [aw=nbirth], abs(comunaname) `se'
    local p`var'=string(ttail(e(df_r),abs(_b[chcc]/_se[chcc]))*2,"%5.4f") 
    if `"`var'"'!="fDeathRate" file write pvals "& `p`var''"
}
file write pvals "\\" _n
file close pvals

lab var chcc "Proportion of ChCC coverage"

#delimit ;
local Est "Estimation sample consists of all municipal-level averages for each
           month between 2003 and 2010 for all women";
local Def "Low birth weight refers to the proportion of births under 2,500
           grams, and premature refers to the proportion of births occurring
           before 37 weeks of gestation.  Birth weight is measured in grams,
           Size is measured in centimetres, and Gestation is measured in weeks.
           Fetal deaths are measured as the number of fetal deaths per 1,000
           live births.";
local Wts  "Each cell is weighted using the number of births in the municipality
           and month";
local FES  "all specifications include municipality and time (Year $\times$
           Month) fixed effects.";
local Sig  "* p$<$0.10; ** p$<$0.05; *** p$<$0.01.";

esttab est1 est2 est3 est4 est5 est6 using "$OUT/comunaDD.tex",
booktabs b(%-9.3f) se(%-9.3f) brackets stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(chcc _cons) label replace
mtitles("Weight" "LBW" "Size" "Gestation" "Premature" "Fetal Death") 
title("Difference-in-Difference Estimates using Municipal Variation in Coverage"
      \label{mDD})
postfoot("\bottomrule\multicolumn{7}{p{18.1cm}}{\begin{footnotesize}           "
         "\textsc{Notes to Table \ref{mDD}}: `Est' `Def' `Wts', and `FES' `Sig'"
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear




foreach var of varlist `yvars' fDeathRate {
    eststo: areg `var' i.time chccAvailable* [aw=nbirth], abs(ccode) `se'
}
lab var chccAvailable     "ChCC Availability"
lab var chccAvailableFull "ChCC Availability ($ \geq $ 9 months)"
#delimit ;
esttab est1 est2 est3 est4 est5 est6 using "$OUT/comunaDD-Available.tex",
booktabs b(%-9.3f) se(%-9.3f) brackets stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) label replace
keep(chccAvailable chccAvailableFull _cons) 
mtitles("Weight" "LBW" "Size" "Gestation" "Premature" "Fetal Death") 
title("Difference-in-Difference Estimates using Municipal Program Availability"
      \label{mDDavailable})
postfoot("\bottomrule\multicolumn{7}{p{18.8cm}}{\begin{footnotesize}           "
         "\textsc{Notes to Table \ref{mDD}}: `Est' `Def' `Wts', and `FES' `Sig'"
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

/*
*--------------------------------------------------------------------------------
*--- (5b) Alternative Controls
*--------------------------------------------------------------------------------
egen comunaYrFE = concat(ccode ano_nac)
tab party, gen(_party)
local fcontrol `tvcontrol' _party* mujer votop

local f1 i.region#i.ano_nac `fcontrol'
local f2 i.ccode#i.ano_nac  `fcontrol'
local wt [aw=nbirth]


foreach var of varlist `yvars' fDeathRate {
    eststo: areg `var' i.time                     chcc `wt', abs(comunaname) `se'    
    eststo: areg `var' i.time `fcontrol'          chcc `wt', abs(comunaname) `se'
    eststo: areg `var' i.time i.region#c.time     chcc `wt', abs(comunaname) `se'
    eststo: areg `var' i.time i.region#i.ano_nac  chcc `wt', abs(comunaname) `se'
    eststo: areg `var' i.time `f1'                chcc `wt', abs(comunaname) `se'
    eststo: areg `var' i.time i.ccode#c.time      chcc `wt', abs(comunaname) `se'
    eststo: areg `var' i.time i.ccode             chcc `wt', abs(comunaYrFE) `se'
    eststo: areg `var' i.time i.ccode `fcontrol'  chcc `wt', abs(comunaYrFE) `se'

    #delimit ;
    esttab est1 est2 est3 est4 est5 est6 est7 est8 using "$OUT/Alt_`var'.tex",
    b(%-9.3f) se(%-9.3f) brackets noobs keep(chcc) nonotes mlabels(, none)
    nonumbers style(tex) fragment replace noline label
    starlevel ("*" 0.10 "**" 0.05 "***" 0.01) ;
    #delimit cr
    estimates clear
}
*/
restore

*--------------------------------------------------------------------------------
*--- (6) Distributional Impacts (birth weight)
*--------------------------------------------------------------------------------
local yvarsp
foreach num of numlist 0(250)4000 {
    local pnum = 1000+`num'
    gen peso`pnum' = peso>`pnum'&peso!=.
    local yvarsp `yvarsp' peso`pnum'
}
local yvarsg
foreach num of numlist 30(1)41 {
    gen gest`num' = gestation>`num'&gestation!=.
    local yvarsg `yvarsg' gest`num'
}

preserve
collapse chcc `yvarsg' (sum) nbirth, by(comunaname ano_nac mes_nac)
replace chcc=0 if chcc==.
sum `yvarsg'
            
gen time = (ano_nac-2000)*12+mes_nac
egen ccode = group(comunaname)
gen per = .
gen est = .
gen LB  = .
gen UB  = .

local j = 1
foreach num of numlist 30(1)41 {
    areg gest`num' i.time chcc [aw=nbirth], abs(comunaname) `se'
    replace per = `num'    in `j'
    replace est = _b[chcc] in `j'
    replace LB  = _b[chcc]-invttail(e(df_r),0.025)*_se[chcc] in `j'
    replace UB  = _b[chcc]+invttail(e(df_r),0.025)*_se[chcc] in `j'
    local ++j
}
#delimit ;
twoway scatter est per, msymbol(O) mcolor(red)
||     rcap UB LB per, lcolor(black) lpattern(dash)
scheme(s1mono) xtitle("Gestational Weeks Greater than Cut-off")
ytitle("Impact of ChCC") yline(0, lcolor(gs14))
legend(lab(1 "Point Estimate") lab(2 "95% CI"));
graph export "$OUT/Gestation_Cutoffs.eps", replace;
#delimit cr
restore


preserve
collapse chcc `yvarsp' (sum) nbirth, by(comunaname ano_nac mes_nac)
replace chcc=0 if chcc==.
sum `yvarsp'
            
gen time = (ano_nac-2000)*12+mes_nac
egen ccode = group(comunaname)
gen per = .
gen est = .
gen LB  = .
gen UB  = .

local j = 1
foreach num of numlist 0(250)4000 {
    local pnum = 1000+`num'
    areg peso`pnum' i.time chcc [aw=nbirth], abs(comunaname) `se'
    replace per = `pnum'    in `j'
    replace est = _b[chcc] in `j'
    replace LB  = _b[chcc]-invttail(e(df_r),0.025)*_se[chcc] in `j'
    replace UB  = _b[chcc]+invttail(e(df_r),0.025)*_se[chcc] in `j'
    local ++j
}
#delimit ;
twoway scatter est per, msymbol(O) mcolor(red)
||     rcap UB LB per, lcolor(black) lpattern(dash)
scheme(s1mono) xtitle("Birth Weight Greater than Cut-off")
ytitle("Impact of ChCC") yline(0, lcolor(gs14))
legend(lab(1 "Point Estimate") lab(2 "95% CI"));
graph export "$OUT/Birthweight_Cutoffs.eps", replace;
#delimit cr
restore


    
foreach var of varlist peso {     
    preserve
    local yvarsp
    foreach num of numlist 5(5)95 {
        local yvarsp `yvarsp' (p`num') p`num'=`var'
    }
    
    collapse chcc `yvarsp' (sum) nbirth, by(comunaname ano_nac mes_nac)
    replace chcc=0 if chcc==.
    sum p*
            
    gen time = (ano_nac-2000)*12+mes_nac
    egen ccode = group(comunaname)
    gen per = .
    foreach t in Level Log {
        gen est_`t' = .
        gen LB_`t'  = .
        gen UB_`t'  = .
    }

    local j = 1
    foreach num of numlist 5(5)95 {
        areg p`num' i.time chcc [aw=nbirth], abs(comunaname) `se'
        replace per       = `num'    in `j'
        replace est_Level = _b[chcc] in `j'
        replace LB_Level  = _b[chcc]-invttail(e(df_r),0.025)*_se[chcc] in `j'
        replace UB_Level  = _b[chcc]+invttail(e(df_r),0.025)*_se[chcc] in `j'
        
        replace p`num'=log(p`num')
        areg p`num' i.time chcc [aw=nbirth], abs(comunaname) `se'
        replace est_Log = _b[chcc] in `j'
        replace LB_Log  = _b[chcc]-invttail(e(df_r),0.025)*_se[chcc] in `j'
        replace UB_Log  = _b[chcc]+invttail(e(df_r),0.025)*_se[chcc] in `j'
        local ++j
    }
    if `"`var'"'=="peso" local vname "Birth Weight"
    if `"`var'"'=="gestation" local vname "Gestation Length"
    
    foreach type in Level Log {
        #delimit ;
        twoway scatter est_`type' per, msymbol(O) mcolor(red)
        ||     rcap UB_`type' LB_`type' per, lcolor(black) lpattern(dash)
        scheme(s1mono) xtitle("Percentile of `vname' Distribution")
        ytitle("Impact of ChCC (`type')") yline(0, lcolor(gs14))
        legend(lab(1 "Point Estimate") lab(2 "95% CI"));
        graph export "$OUT/Percentiles_`var'_`type'.eps", replace;
        #delimit cr
    }
    restore
}


*--------------------------------------------------------------------------------
*--- (7) FPS
*Primer quintil: de 2.072 a 8.500 puntos.
*Segundo quintil: de 8.501 a 11.734 puntos.
*Tercer quintil: de 11.735 a 13.484 puntos.
*Cuarto quintil: de 13.485 a 14.557 puntos.
*Quinto quintil: 14.558 o más puntos
*--------------------------------------------------------------------------------
local nq 3
    
destring puntaje_fps_sept2013, replace
histogram puntaje_fps_sept2013, bcolor(blue) scheme(s1mono) freq
graph export ficha.eps, replace
*xtile ficha=puntaje_fps_sept2013, nquantiles(`nq')
gen     ficha = 1 if puntaje_fps>=2072 & puntaje_fps<=8500
replace ficha = 2 if puntaje_fps>8500  & puntaje_fps<=11734
replace ficha = 3 if puntaje_fps>11734 & puntaje_fps<=13484
replace ficha = 4 if puntaje_fps>13484 & puntaje_fps<=14557
replace ficha = 5 if puntaje_fps>14557 & puntaje_fps!=.

gen ficha1 = puntaje_fps <= 11734
gen ficha2 = puntaje_fps >  11734 & puntaje_fps <= 13484
gen ficha3 = puntaje_fps >  13484

gen chcc1 = chcc if ficha1==1
gen chcc2 = chcc if ficha2==1
gen chcc3 = chcc if ficha3==1

local avars peso lbw talla gestation premature
preserve
collapse `avars' chcc1 chcc2 chcc3 (sum) nbirth, by(comunaname ano_nac mes_nac)
replace chcc1=0 if chcc1==.
replace chcc2=0 if chcc2==.
replace chcc3=0 if chcc3==.


gen time = (ano_nac-2000)*12+mes_nac
egen ccode = group(comunaname)

local se cluster(comunaname)
foreach var of varlist `avars' {
    eststo: areg `var' i.time chcc* [aw=nbirth], abs(comunaname) `se'
}
#delimit ;
esttab est1 est2 est3 est4 est5 using "$OUT/comunaDD-FPSlevels.tex",
booktabs b(%-9.3f) se(%-9.3f) brackets stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(chcc1 chcc2 chcc3 _cons) label replace
mtitles("Weight" "LBW" "Size" "Gestation" "Premature") 
title("Difference-in-Difference Estimates by Vulnerability Score FPS"
      \label{mDD-FPS})
postfoot("\bottomrule\multicolumn{6}{p{15.8cm}}{\begin{footnotesize}           "
         "\textsc{Notes to Table \ref{mDD}}: `Est' `Def' `Wts', and `FES' `Sig'"
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear
restore



gen quintile=_n in 1/`nq'
replace chcc=. if dn_sexo_padre=="MASCULINO"
foreach var of varlist peso lbw talla gestation premature {
    gen impact = .
    gen UB     = .
    gen LB     = .
    foreach num of numlist 1(1)`nq' {
        preserve
        *keep if 
        replace chcc=. if ficha`num'!=1
        collapse `var' chcc (sum) nbirth, by(comunaname ano_nac mes_nac)
        replace chcc=0 if chcc==.
        
        gen time = (ano_nac-2000)*12+mes_nac
        egen ccode = group(comunaname)
        local se cluster(comunaname)
        areg `var' i.time chcc [aw=nbirth], abs(comunaname) `se'
        estimates store `var'_`num'
        restore
        replace impact = _b[chcc] in `num'
        replace UB     = _b[chcc]+1.96*_se[chcc] in `num'
        replace LB     = _b[chcc]-1.96*_se[chcc] in `num'
        if `num'==`nq' {
            #delimit ;
            twoway scatter impact quintile, msymbol(O) mcolor(red)
            ||     rcap UB LB quintile, lcolor(black) lpattern(dash)
            scheme(s1mono) xtitle("Quintile of Social Protection Score")
            ytitle("Impact of ChCC") xlabel(1(1)`nq') yline(0, lcolor(gs14))
            legend(lab(1 "Point Estimate") lab(2 "95% CI"));
            graph export "$OUT/FPS_`var'.eps", replace;
            #delimit cr
            drop impact UB LB
        }
    }
}
lab var chcc "Proportion ChCC Coverage"
foreach n of numlist 1(1)`nq' {
    #delimit ;
    esttab peso_`n' lbw_`n' talla_`n' gestation_`n' premature_`n'
    using "$OUT/FPS_`n'.tex", b(%-9.3f) se(%-9.3f) brackets keep(chcc)
    nonotes mlabels(, none) nonumbers style(tex) fragment replace noline label
    starlevel ("*" 0.10 "**" 0.05 "***" 0.01) stats
    (N r2, fmt(%9.0g %5.3f) label(Observations R-Squared));
    #delimit cr
}


sum mech*
*--------------------------------------------------------------------------------
*--- (8) Mechanism
*--------------------------------------------------------------------------------
preserve
local yvars peso lbw talla gestation premature
local tvars chcc ChCCTime
local mvars mech_* 
local group comunaname ccode ano_nac mes_nac servcode
collapse `tvars' `yvars' `mvars' (sum) nbirth, by(`group')

replace chcc=0 if chcc==.
replace mech_ChileSolidario=0 if mech_ChileSolidario==.

gen chccAvailable     = ChCCTime>=1
gen chccAvailableFull = ChCCTime>=9

merge 1:1 mes_nac ano_nac ccode using `fetalDeaths'
replace fetalDeath=0 if fetalDeath==.
gen     fDeathRate=fetalDeath/nbirth*1000

gen time = (ano_nac-2000)*12+mes_nac
gen region = floor(ccode/1000)

local c1      mech_controlPrenat
local c2 `c1' mech_puritaMama mech_puritaFortif
local c3 `c2' mech_homeVisits
local c4 `c3' mech_asistenciaSoc mech_ChileSolidario

local se abs(comunaname) cluster(ccode)
foreach var of varlist `c4' { 
    eststo: areg `var' i.time i.servcode chcc [aw=nbirth], `se'
}
lab var chcc "Proportion of ChCC Coverage"

#delimit ;
esttab est1 est2 est3 est4 est5 est6 using "$OUT/ChCC_Inputs.tex",
booktabs b(%-9.3f) se(%-9.3f) brackets stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(chcc _cons) label replace
mtitles("Home Visits" "Food Supplement +" "Food Supplement" "Prenatal Visits"
        "Social Support" "Chile Solidario") 
title("Impact of Chile Crece Contigo on Pregnancy Inputs"
      \label{Inputs})
postfoot("\bottomrule\multicolumn{7}{p{23.8cm}}{\begin{footnotesize}         "
         "\textsc{Notes to Table \ref{Inputs}}: Each regression shows the    "
         "correlation between ChCC useage and different program components.  "
         "Each variable with the exception of Chile Solidario refers to the  "
         "average usage per birth in the 9 months prior to each birth, and is"
         " measured at the level of health service and month.  One health    "
         "service split in two in 2008, and hence lags are not available for "
         "a small number of areas in this period.  Home visits refers to the "
         "number of `integral visits to expecting mothers' by a nurse or     "
         "midwife, Food Supplement and Food Supplement + refer to `Leche     "
         "Purita', a fortified powdered milk drink given to pregnant women   "
         "with an updated formula from 2008 onwards, Prenatal visits refer to"
         " controls with nurses, doctors or midwives at local health centres,"
         " Social support refers to all visits with Social Assistants, and   "
         "Chile Solidario refers to the number of pregnant women giving birth"
         " each month who have at any point participated in Chile Solidario, "
         "a targeted social welfare program including a cash transfer."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

areg peso i.time i.servcode chcc `c1' [aw=nbirth], `se'
eststo: areg peso i.time i.servcode chcc [aw=nbirth] if e(sample)==1, `se'
local beta0 = _b[chcc]
foreach num of numlist 1(1)4 {
    local beta1 = _b[chcc]
    eststo: areg peso i.time i.servcode chcc `c`num'' [aw=nbirth], `se'
    estadd scalar delta  =  (`beta1' - _b[chcc])/`beta1'
    estadd scalar deltaA =  (`beta0' - _b[chcc])/`beta0'
}

lab var mech_controlPrenat  "Prenatal Controls"
lab var mech_puritaMama     "Fortified Milk (New Formula)"
lab var mech_puritaFortif   "Fortified Milk (Original)"
lab var mech_homeVisits     "Home Visits"
lab var mech_asistenciaSoc  "Social Assistance"
lab var mech_ChileSolidario "Chile Solidario Enrolment"
#delimit ;
esttab est1 est2 est3 est4 est5 using "$OUT/ChCC_Mechanism.tex",
booktabs b(%-9.3f) se(%-9.3f) brackets stats
(delta deltaA N r2, fmt(%5.3f %5.3f %9.0g %5.3f)
label("Explained Effect" "Explained Effect (Cumulative)" Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(chcc `c4' _cons) label replace
mtitles("Base" "Home Visits" "Supplements" "Prenatal Care"
        "Social" "Chile Solidario") 
title("Partial Test of ChCC Mechanisms"
      \label{Mechs})
postfoot("\bottomrule\multicolumn{6}{p{17.8cm}}{\begin{footnotesize}            "
         "\textsc{Notes to Table \ref{Mechs}}: Specifications replicate column  "
         "1 of Table \ref{mDD}, where birth weight is the dependent variable.   "
         "All mechanism variables are available for each health service and     "
         "month.  One health service split into two in 2008, meaning that a     "
         "small number of mechanism variables are not available where lagged    "
         "measures are used. We consistently esitmate without these observations"
         " so each column is comparable.  Explained effect refers to the        "
         "proportion of the baseline impact of ChCC which is explained away when"
         " conditioning on a particular mechanism, and the cumulative explained "
         "effect refers to the total explained effect summing all mechanisms.   "
         "Additional details related to mechanisms and measurement are available"
         "in section \ref{scn:Mechanisms}."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

qui reg chcc i.time i.ccode i.servcode [aw=nbirth]
predict chcc_epsilon, residuals
local mech_epsilon
foreach var of varlist `c4' {
    qui reg `var' i.time i.ccode i.servcode [aw=nbirth]
    predict `var'_eps, residuals
    local mech_epsilon `mech_epsilon' `var'_eps
}

foreach var of varlist `yvars' fDeathRate {
    qui reg `var' i.time i.ccode i.servcode [aw=nbirth]
    predict `var'_epsilon, residuals
    #delimit ;
    eststo: b1x2 `var'_epsilon [aw=nbirth], x1all(chcc_epsilon) 
    x2all(`mech_epsilon') x1only(chcc_epsilon) cluster(ccode)
    x2delta(g1 = mech_controlPrenat_eps :
            g2 = mech_puritaMama_eps mech_puritaFortif_eps :
            g3 = mech_homeVisits_eps :
            g4 = mech_asistenciaSoc_eps mech_ChileSolidario_eps);
    #delimit cr
}

foreach num of numlist 1(1)4 {
    gen g`num'=.
}
gen __TC=.
lab var g1 "Prenatal Controls"
lab var g2 "Food Supplementation"
lab var g3 "Home Visits"
lab var g4 "Social Safety Net"
lab var __TC "\textbf{Total Explained Difference}"
lab var chcc_epsilon "Decomposition of $\Delta$ ChCC Coverage"

#delimit ;
esttab est1 est2 est3 est4 est5 est6 using "$OUT/GelbachMechanism.tex",
booktabs b(%-9.3f) se(%-9.3f) brackets stats
(N, fmt(%9.0g) label(Observations)) label replace
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(g1 g2 g3 g4 __TC) 
mtitles("Weight" "LBW" "Size" "Gestation" "Premature" "Fetal Death") 
title("\citet{Gelbach2016} Decomposition of ChCC Mechanism"
      \label{GelbachMech})
postfoot("\bottomrule\multicolumn{7}{p{17.6cm}}{\begin{footnotesize}  "
         "\textsc{Notes to Table \ref{GelbachMech}}: Each column      "
         "displays the coefficient change decompisition developed by  "
         "\citet{Gelbach2016} for a different outcome variable.  This "
         "decomposition considers the change in the estimated effect  "
         "of ChCC from the baseline diff-in-diff model compared with  "
         "that estimated in the full model where all proposed         "
         "mechanisms are accounted for.  The full change is given by  "
         "`Total Explained Difference', and this is decomposed into   "
         "the portion owing to each of the four mechanisms discussed  "
         "in section \ref{scn:Mechanisms}.  Full details of the       "
         "decomposition and estimation of the variance-covariance     "
         "matrix is provided by \citet{Gelbach2016}."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
restore
exit


*--------------------------------------------------------------------------------
*--- (9) Event-study style tests
*--------------------------------------------------------------------------------
local lev1 0.00 0.65
local lev2 0.65 1.00
local mtime 9
local bin   3

preserve
local yvars peso lbw talla gestation premature
local tvars chcc ChCCTime
local group comunaname ccode ano_nac mes_nac
collapse `tvars' `yvars' `tvcontrol' (sum) nbirth, by(`group')

replace chcc=0 if chcc==.
gen ctemp = chcc if ChCCTime>0
bys ccode: egen aveChCC = mean(ctemp)
drop ctemp
gen  time = (ano_nac-2003)*12+mes_nac
gen  ChCCBin = ceil((ChCCTime)/`bin')
egen comunaYrFE = concat(ccode ano_nac)
sum aveChCC, d

tokenize `lev1'
local j=1
local postvars 
foreach l of local lev2 {
    gen post`j' = aveChCC>=``j''&aveChCC<`l'
    local postvars `postvars' post`j'
    local ++j
}

foreach num of numlist 1(1)`mtime' {
    gen plus`num'month = ChCCBin==`num'
    gen less`num'month = ChCCBin==-`num'    
}

local mtime1 = `mtime'+1
local events
gen plus`mtime1'month = ChCCBin>=`mtime1'
gen less`mtime1'month = ChCCBin<=-`mtime1'
foreach cvar of varlist `postvars' {
    foreach num of numlist 1(1)`mtime1' {
        gen `cvar'P`num' = `cvar'==1&plus`num'month==1
        gen `cvar'L`num' = `cvar'==1&less`num'month==1
        local events `events' `cvar'P`num' `cvar'L`num'
    }
}
areg peso i.time `events' `tvcontrol' [aw=nbirth], abs(ccode) cluster(ccode)

gen eventtime=.
foreach cvar of varlist `postvars' {
    local i=1
    gen `cvar'Est=.
    gen `cvar'LB=.
    gen `cvar'UB=.
    foreach num of numlist `mtime1'(-1)1 {
        replace `cvar'Est=_b[`cvar'L`num'] in `i'
        replace `cvar'LB =_b[`cvar'L`num'] - 1.96*_se[`cvar'L`num'] in `i'
        replace `cvar'UB =_b[`cvar'L`num'] + 1.96*_se[`cvar'L`num'] in `i'
        replace eventtime = -`num' in `i'
        local ++i
    }
    replace `cvar'Est=0 in `i'
    replace `cvar'LB=0 in `i'
    replace `cvar'UB=0 in `i'
    replace eventtime = 0 in `i'
    local ++i
    foreach num of numlist 1(1)`mtime1' {
        replace `cvar'Est=_b[`cvar'P`num'] in `i'
        replace `cvar'LB =_b[`cvar'P`num'] - 1.96*_se[`cvar'P`num'] in `i'
        replace `cvar'UB =_b[`cvar'P`num'] + 1.96*_se[`cvar'P`num'] in `i'
        replace eventtime = `num' in `i'
        local ++i
    }
}

foreach cvar of varlist `postvars' {
    #delimit ;
    twoway connected `cvar'Est eventtime || rcap `cvar'LB `cvar'UB eventtime,
    scheme(s1mono) yline(0, lcolor(red))
    xline(0, lcolor(black) lwidth(vthin) lpattern(dash));
    #delimit cr
    graph export "$OUT/event_`cvar'.eps", replace
}

restore
exit


*--------------------------------------------------------------------------------
*--- (10) Placebo tests
*--------------------------------------------------------------------------------
preserve
local yvars peso lbw talla gestation premature
collapse chcc `yvars' (sum) nbirth, by(comunaname ccode ano_nac mes_nac)
replace chcc=0 if chcc==.

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
*--- (11) Multiple hypothesis testing
*--------------------------------------------------------------------------------
***[A -- ROMANO WOLF]
do rwolf/rwolf.ado
*fDeathRate
#delimit ;
rwolf `yvars' [aw=nbirth], indepvar(chcc) controls(i.time) 
      abs(comunaname) cluster(comunaname) method(areg) v seed(27) reps(50);
#delimit cr
local pRW
foreach var of varlist `yvars' {
    local p`var'=string(e(rw_`var'),"%5.4f")
    local pRW "`pRW' & `p`var'' "
}
local pRW "`pRW' \\"

*fDeathRate
***[B -- ANDERSON INDEX]
local yvars peso lbw talla gestation premature 
foreach var of varlist lbw premature {
    replace `var'=-1*`var'
}
local j=1
foreach var of varlist `yvars' {
    sum `var' if chcc==0
    local stdev = r(sd)
    sum `var'
    local mean  = r(mean)
    gen z_`j'=(`var'-`mean')/`stdev'
    local ++j
}
foreach var of varlist lbw premature {
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
areg sbar i.time chcc [aw=nbirth], abs(comunaname) cluster(comunaname)
local psbar=string(ttail(e(df_r),abs(_b[chcc]/_se[chcc]))*2,"%5.4f")
file open pvals using "$OUT/MC_DD_pRW.tex", write replace
file write pvals "\textbf{`psbar'} `pRW' \\" _n 
file close pvals
restore


*--------------------------------------------------------------------------------
*--- (12) Based on trimester
*--------------------------------------------------------------------------------
preserve
gen trimester_nac=ceil(mes_nac/3)

local yvars peso lbw talla gestation premature
local mcon  edad_m teen hij_total 
local group comunaname ccode ano_nac trimester_nac
collapse chcc `yvars' `controls' `mcon' (sum) nbirth, by(`group')
replace chcc=0 if chcc==.

gen time = (ano_nac-2003)*4+trimester_nac

merge 1:1 trimester_nac ano_nac ccode using `fetalDeathsTri'
replace fetalDeath=0 if fetalDeath==.
gen     fDeathRate=fetalDeath/nbirth*1000


lab var peso      "Birth Weight (grams)"
lab var lbw       "Low Birth Weight $< 2500$ grams"
lab var talla     "Length (cm)"
lab var gestation "Gestation (weeks)"
lab var premature "Premature $< 37$ weeks"
lab var nbirth    "Number of Births"
lab var chcc      "Proportion Enrolled in ChCC"
lab var fDeathRat "Rate of Fetal Deaths/1000 Births"
lab var ano_nac   "Year of Birth"
lab var edad_m    "Mother's Age"
lab var teen      "Proportion Teen Births"
lab var hij_total "Number of Children"

#delimit ;
estpost sum chcc peso lbw gestation premat talla nbir fDeathRate ano_nac `mcon';
estout using "$OUT/SummaryMunicipal-trimester.tex", replace label style(tex)
cells("count mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))")
collabels(, none) mlabels(, none);
#delimit cr


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
postfoot("\bottomrule\multicolumn{7}{p{19.2cm}}{\begin{footnotesize}     "
         "Estimation sample consists of all municipal-level averages for "
         "each quarter between 2003 and 2010 for all women. Refer to     "
         "additional notes in table \ref{mDD}, and summary statistics for"
         "each variable at the trimester by municipal level in Table     "
         "\ref{tab:sumstatsTri}. "
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);

#delimit cr
estimates clear
restore


exit

*--------------------------------------------------------------------------------
*--- (13) Make data of comuna trends
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

*--------------------------------------------------------------------------------
*--- (X) Clean
*--------------------------------------------------------------------------------
log close

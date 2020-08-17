
********************************************************************************
********************************************************************************
***** Shared UNWIDER Codes
***** 28 Nov, 2019
********************************************************************************
********************************************************************************

	cls
	set more off
	capture log close
	estimates clear
	
	global dofilename "3B Tunisia_Determinants" // Private sector only
		
// create local for date and version tracking

	local c_date = c(current_date)
	local date_string = subinstr("`c_date'", ":", "_", .)
	local date_string = subinstr("`date_string'", " ", "_", .)
	global track_date = c(current_date)
	global track_time = c(current_time)

// globals to manage users
		
	global user 2 /* put 1 for Michelle and 2 for Phuong */

	if $user == 1 {
	global desktop /Users/michelle/Dropbox/Tunisie_ENPE
	global person "Michelle"
	}

	if $user == 2 {
	global desktop C:/Users/le.minh.phuong/Dropbox/Tunisie_ENPE
	global INS "D:/02_EnqEmp_new/Projet IRD decembre 2019"
	global person "Phuong"

	}
	
// globals for trial or real mode

	global trial 0 /* 
		* 0 if we are not in trial mode
		* 1 if we are in trial mode using individual data
		* 2 if we are in trial mode using household data */

	if $trial ==1 {
		global dofiles $desktop/Dofiles
		global data_imputed $desktop/Data/Combined_Data/ENPE_imputed
		global data_combined $desktop/Data/Combined_Data/sim_combined
		global data_survey $desktop/Data/enquete_des_individus/echantillon_2000_10_17
		global graphs $desktop/Results/PartB_Econometric/Graphs
		global tables $desktop/Results/PartB_Econometric/Tables
		global graphs_private $desktop/Results_private/PartB_Econometric/Graphs
		global tables_private $desktop/Results_private/PartB_Econometric/Tables
		global log $desktop/Log
		version 13
		}
	
	if $trial ==0 {
		global dofiles $INS/Dofiles
		global data_imputed $INS/Data/Combined_Data/ENPE_imputed
		global data_combined $INS/Data/Combined_Data/ENPE_combined
		global data_survey $INS/Data/Survey_Data
		global graphs $INS/Results/PartB_Econometric/Graphs
		global tables $INS/Results/PartB_Econometric/Tables
		global graphs_private $desktop/Results_private/PartB_Econometric/Graphs
		global tables_private $desktop/Results_privatePartB_Econometric/Tables
		global log $INS/Log
		}

// Notes for programs : Need to intall special packages
		foreach package in rif reghdfe {
			capture which `package'
			if _rc==111 ssc install `package', replace 
			}

//run log for commands
	cd
	cd "$log"
	cap log using 3B_Determinants_`date_string', text replace

********************************************************************************
display "This file started on `c(current_date)' at `c(current_time)' " 
********************************************************************************

use "$data_combined", clear

keep if status==1 // for employees only

/*************************************************/
/* E. Aggregated tables for education and public */
/*************************************************/
gen obs = 1

preserve 
keep if position==3 & status==1 
drop if isco88==. | public==. | industry==.
collapse (mean) rti rti_man new_rti_isco2d earn (rawsum) wgt obs [pw=wgt], by(wave isco88 industry public sex educ)
	export delimited using "$tables/agg_em.csv", replace
restore

preserve
drop if  position==. | public==.
collapse (mean) rti rti_man new_rti_isco2d (rawsum) wgt obs [pw=wgt], by(wave isco88 industry public sex educ position)
	export delimited using "$tables/agg_all.csv", replace
restore

preserve
drop if educ==. 
collapse (sum) wgt obs, by(wave educ)
bys wave: egen pop = total(wgt)
gen educ_shr = wgt/pop
	export delimited using "$tables/agg_all_educ.csv", replace
restore

/*****************************************/
/* F. Determinants of polarisation 		 */
/*****************************************/

* Covariates
gen littoral=0
replace littoral = 1 if gouv==11 | gouv==12 | gouv==13 | gouv==15 | gouv==31 | ///
						gouv==32 | gouv==33 | gouv==34 | gouv==51 | gouv==52
recode public (0=2) (1=1)
recode youth (0=2) (1=1)
recode sex (0=2) (1=1), gen(male)
gen married = cond(marital==2, 1, cond(marital!=2 & marital!=., 0, .))
lab var male "Male"
lab var married "Married"
lab var littoral "Littoral region"

*** RIF regression
forval wave=1/3 {
	rifhdreg earn i.isco88_1 i.industry i.educ public male youth littoral married [pw=wgt] if wave==`wave', ///
		rif(gini) scale(1000) robust
		outreg2 using "$tables/rifhdreg.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`wave'", "gini") lab replace
	rifhdreg earn i.isco88_1 i.industry i.educ public male youth littoral married [pw=wgt] if wave==`wave', ///
		rif(mean) robust 
		outreg2 using "$tables/rifhdreg.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`wave'", "mean") lab append
	rifhdreg lnearn i.isco88_1 i.industry i.educ public male youth littoral married [pw=wgt] if wave==`wave', ///
		rif(var) scale(100) robust 
		outreg2 using "$tables/rifhdreg.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`wave'", "var") lab append
	rifhdreg lnearn i.isco88_1 i.industry i.educ public male youth littoral married [pw=wgt] if wave==`wave', ///
		rif(iqr(10 50)) scale(100) robust
		outreg2 using "$tables/rifhdreg.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`wave'", "10-50") lab append
	rifhdreg lnearn i.isco88_1 i.industry i.educ public male youth littoral married [pw=wgt] if wave==`wave', ///
		rif(iqr(10 90)) scale(100) robust
		outreg2 using "$tables/rifhdreg.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`wave'", "10-90") lab append
	rifhdreg lnearn i.isco88_1 i.industry i.educ public male youth littoral married [pw=wgt] if wave==`wave', ///
		rif(iqr(50 90)) scale(100) robust
		outreg2 using "$tables/rifhdreg.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`wave'", "50-90") lab append
	}
	
forval wave=1/3 {
	rifhdreg earn i.isco88_1 i.industry i.educ public male youth littoral married [pw=wgt] if wave==1, ///
		rif(gini) scale(1000) vce(robust) abs(id)
		outreg2 using "$tables\rifhdreg_fe.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`wave'", "gini") lab replace
	rifhdreg earn i.isco88_1 i.industry i.educ public male youth littoral married [pw=wgt] if wave==`wave', ///
		rif(mean) robust abs(id)
		outreg2 using "$tables\rifhdreg_fe.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`wave'", "mean") lab append
	rifhdreg lnearn i.isco88_1 i.industry i.educ public male youth littoral married [pw=wgt] if wave==`wave', ///
		rif(var) scale(100) robust abs(id)
		outreg2 using "$tables\rifhdreg_fe.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`wave'", "var") lab append
	rifhdreg lnearn i.isco88_1 i.industry i.educ public male youth littoral married [pw=wgt] if wave==`wave', ///
		rif(iqr(10 50)) scale(100) robust abs(id)
		outreg2 using "$tables\rifhdreg_fe.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`wave'", "10/50") lab append
	rifhdreg lnearn i.isco88_1 i.industry i.educ public male youth littoral married [pw=wgt] if wave==`wave', ///
		rif(iqr(10 90)) scale(100) robust abs(id)
		outreg2 using "$tables\rifhdreg_fe.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`wave'", "10/90") lab append
	rifhdreg lnearn i.isco88_1 i.industry i.educ public male youth littoral married [pw=wgt] if wave==`wave', ///
		rif(iqr(50 90)) scale(100) robust abs(id)
		outreg2 using "$tables\rifhdreg_fe.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`wave'", "50/90") lab append
	}
	
*** RIF decomposition
gen wave12 = cond(wave==1, 0, cond(wave==2, 1, .))
gen wave13 = cond(wave==1, 0, cond(wave==3, 1, .))
gen wave23 = cond(wave==2, 0, cond(wave==3, 1, .))
	
	// Normal RIF decomposition
foreach i in 12 13 23 {
	oaxaca_rif earn isco88_1 industry educ public male youth littoral married [pw=wgt], ///
		rif(gini) by(wave`i') rwlogit(i.isco88_1 i.industry i.educ public male youth littoral married) scale(1000) swap w(1) relax n
		outreg2 using "$tables\oaxaca_rif.xml", excel dec(3) ctitle("`i'", "gini") lab replace
	oaxaca_rif earn isco88_1 industry educ public male youth littoral married [pw=wgt], ///
		rif(mean) by(wave`i') rwlogit(i.isco88_1 i.industry i.educ public male youth littoral married) swap w(1) relax n
		outreg2 using "$tables\oaxaca_rif.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`i'", "mean") lab append
	oaxaca_rif lnearn i.isco88_1 i.industry i.educ public male youth littoral married [pw=wgt], ///
		rif(var) by(wave`i') rwlogit(i.isco88_1 i.industry i.educ public male youth littoral married) scale(100) swap w(1) relax n
		outreg2 using "$tables\oaxaca_rif.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`i'", "var") lab append
	oaxaca_rif lnearn i.isco88_1 i.industry i.educ public male youth littoral married [pw=wgt], ///
		rif(iqr(10 50)) by(wave`i') rwlogit(i.isco88_1 i.industry i.educ public male youth littoral married) scale(100) swap w(1) relax n
		outreg2 using "$tables\oaxaca_rif.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`i'", "10/50") lab append
	oaxaca_rif lnearn i.isco88_1 i.industry i.educ public male youth littoral married [pw=wgt], ///
		rif(iqr(10 90)) by(wave`i') rwlogit(i.isco88_1 i.industry i.educ public male youth littoral married) scale(100) swap w(1) relax n
		outreg2 using "$tables\oaxaca_rif.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`i'", "10/90") lab append
	oaxaca_rif lnearn i.isco88_1 i.industry i.educ public male youth littoral married [pw=wgt], ///
		rif(iqr(50 90)) by(wave`i') rwlogit(i.isco88_1 i.industry i.educ public male youth littoral married) scale(100) swap w(1) relax n
		outreg2 using "$tables\oaxaca_rif.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`i'", "50/90") lab append
	}
	
	// Bootstrap RIF decomposition
foreach i in 12 13 23 {
	bootstrap:oaxaca_rif earn isco88_1 industry educ public male youth littoral married, ///
		rif(gini) by(wave`i') rwlogit(i.isco88_1 i.industry i.educ public male youth littoral married) scale(1000) swap w(1) relax n
		outreg2 using "$tables\oaxaca_rif_bst.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`i'", "gini") lab replace
	bootstrap:oaxaca_rif earn isco88_1 industry educ public male youth littoral married, ///
		rif(mean) by(wave`i') rwlogit(i.isco88_1 i.industry i.educ public male youth littoral married) swap w(1) relax n
		outreg2 using "$tables\oaxaca_rif_bst.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`i'", "mean") lab append
	bootstrap:oaxaca_rif lnearn i.isco88_1 i.industry i.educ public male youth littoral married, ///
		rif(var) by(wave`i') rwlogit(i.isco88_1 i.industry i.educ public male youth littoral married) scale(100) swap w(1) relax n
		outreg2 using "$tables\oaxaca_rif_bst.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`i'", "var") lab append
	bootstrap:oaxaca_rif lnearn i.isco88_1 i.industry i.educ public male youth littoral married, ///
		rif(iqr(10 50)) by(wave`i') rwlogit(i.isco88_1 i.industry i.educ public male youth littoral married) scale(100) swap w(1) relax n
		outreg2 using "$tables\oaxaca_rif_bst.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`i'", "10/50") lab append
	bootstrap:oaxaca_rif lnearn i.isco88_1 i.industry i.educ public male youth littoral married, ///
		rif(iqr(10 90)) by(wave`i') rwlogit(i.isco88_1 i.industry i.educ public male youth littoral married) scale(100) swap w(1) relax n
		outreg2 using "$tables\oaxaca_rif_bst.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`i'", "10/90") lab append
	bootstrap:oaxaca_rif lnearn i.isco88_1 i.industry i.educ public male youth littoral married, ///
		rif(iqr(50 90)) by(wave`i') rwlogit(i.isco88_1 i.industry i.educ public male youth littoral married) scale(100) swap w(1) relax n
		outreg2 using "$tables\oaxaca_rif_bst.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("`i'", "50/90") lab append
	}
	
	// RIF decomposition by quantile
foreach i in 12 13 23 {
	foreach v of num 1(5)99 {
		oaxaca_rif lnearn isco88_1 industry educ public male youth littoral married [pw=wgt], ///
			rif(q(`v')) by(wave`i') rwlogit(i.isco88_1 i.industry i.educ public male youth littoral married) swap w(1) relax n
		mat wave`i'_`v'_1	= e(b)
		mat wave`i'_`v'		= wave`i'_`v'_1[1,4], wave`i'_`v'_1[1,5], wave`i'_`v'_1[1,6], wave`i'_`v'_1[1,10], wave`i'_`v'_1[1,11], wave`i'_`v'_1[1,12], wave`i'_`v'_1[1,13], wave`i'_`v'_1[1,15], wave`i'_`v'_1[1,16]
		mat wave`i'			= nullmat(wave`i') \ wave`i'_`v'
		mat drop wave`i'_`v' wave`i'_`v'_1
		}
		}
	matrix list wave12, format(%5.4f)
	matrix list wave13, format(%5.4f)
	matrix list wave23, format(%5.4f)
	putexcel set "$tables\decomp.xls", sheet(12, replace) replace
		putexcel A1 = matrix(wave12)
	putexcel set "$tables\decomp.xls", sheet(13, replace) modify
		putexcel A1 = matrix(wave13)
	putexcel set "$tables\decomp.xls", sheet(23, replace) modify
		putexcel A1 = matrix(wave23)
	mat drop wave12 wave13 wave23

/*****************************************/
/* G. Robustness checks			 		 */
/*****************************************/
use "$data_combined", clear
keep if position==2 | position==3 | position==5

* Self-employed imputation
gen isic3_1 = int(isic3/1000)

reg earn age sex i.edu i.isic3_1 i.isco88_1 i.gouv i.year

mi set mlong
mi register imputed earn
mi xtset, clear
mi impute regress earn age sex i.edu i.isic3_1 i.isco88_1 i.gouv i.year, add(20) rseed(1234) force
mi export nhanes1 ENPE_imputed, replace

use ENPE_imputed.dta, clear
gen earnif earn
gen lnearn_im = ln(earn)
replace lnearn = lnearn_im if lnearn==. & lnearn_im!=.

* Employment share, mean earnings and RTI by 4-digit ISCO88 
preserve
	drop if isco88 == .
	gen obs = 1
	collapse (mean) rti rti_man new_rti_isco2d lnearn earn (rawsum) wgt obs [pw=wgt], ///
		by(wave isco88 position) 
	bys wave: egen pop = total(wgt)
	gen empl_sh = (wgt/pop)*100
	export delimited wave isco88 lnearn earn obs wgt /// 
		using "Earn_mean_Tunisia_4d_imputed.csv", replace
	export delimited wave isco88 obs wgt empl_sh rti rti_man new_rti_isco2d ///
		using "RTI_time_comp_Tunisia_4d_imputed.csv", replace
restore


* Summary statistics

/*	Employment by gender */
	* By education group
	tab edu wave [aw=wgt], col
	tab edu wave [aw=wgt] if sex == 1, col nofreq
	tab edu wave [aw=wgt] if sex == 2, col nofreq
	tab edu wave [aw=wgt] if position == 2 | position==5, col nofreq
	tab edu wave [aw=wgt] if position == 3, col nofreq
	tab edu wave [aw=wgt] if (position == 2 | position==5) & sex==1, col nofreq
	tab edu wave [aw=wgt] if position == 3 & sex==1, col nofreq
	tab edu wave [aw=wgt] if (position == 2 | position==5) & sex==2, col nofreq
	tab edu wave [aw=wgt] if position == 3 & sex==2, col nofreq
	* By occupation
	tab isco88_1 wave [aw=wgt], col
	tab isco88_1 wave [aw=wgt] if sex == 1, col nofreq
	tab isco88_1 wave [aw=wgt] if sex == 2, col nofreq
	tab isco88_1 wave [aw=wgt] if position == 2 | position==5, col nofreq
	tab isco88_1 wave [aw=wgt] if position == 3, col nofreq
	tab isco88_1 wave [aw=wgt] if (position == 2 | position==5) & sex==1, col nofreq
	tab isco88_1 wave [aw=wgt] if position == 3 & sex==1, col nofreq
	tab isco88_1 wave [aw=wgt] if (position == 2 | position==5) & sex==2, col nofreq
	tab isco88_1 wave [aw=wgt] if position == 3 & sex==2, col nofreq
	* By skill level
	tab skill wave [aw=wgt], col 
	tab skill wave [aw=wgt] if sex == 1, col nofreq
	tab skill wave [aw=wgt] if sex == 2, col nofreq
	tab skill wave [aw=wgt] if position == 2 | position==5, col nofreq
	tab skill wave [aw=wgt] if position == 3, col nofreq
	tab skill wave [aw=wgt] if (position == 2 | position==5) & sex==1, col nofreq
	tab skill wave [aw=wgt] if position == 3 & sex==1, col nofreq
	tab skill wave [aw=wgt] if (position == 2 | position==5) & sex==2, col nofreq
	tab skill wave [aw=wgt] if position == 3 & sex==2, col nofreq
	* By working status
	tab position wave [aw=wgt], col 
	tab position wave [aw=wgt] if sex == 1, col nofreq
	tab position wave [aw=wgt] if sex == 2, col nofreq

/* 	Real mean earnings by gender */
	* By education group
	table edu wave [aw=wgt], c(mean earn) format(%4.0f) row
	table edu wave sex [aw=wgt], c(mean earn) format(%4.0f) row
	table edu wave [aw=wgt] if position == 2 | position==5, c(mean earn) format(%4.0f) row
	table edu wave [aw=wgt] if position == 3, c(mean earn) format(%4.0f) row	
	table edu wave sex [aw=wgt] if position == 2 | position==5, c(mean earn) format(%4.0f) row
	table edu wave sex [aw=wgt] if position == 3, c(mean earn) format(%4.0f) row
	* By occupation
	table isco88_1 wave [aw=wgt], c(mean earn) format(%4.0f) row 
	table isco88_1 wave sex [aw=wgt], c(mean earn) format(%4.0f) row
	table isco88_1 wave [aw=wgt] if position == 2 | position==5, c(mean earn) format(%4.0f) row
	table isco88_1 wave [aw=wgt] if position == 3, c(mean earn) format(%4.0f) row	
	table isco88_1 wave sex [aw=wgt] if position == 2 | position==5, c(mean earn) format(%4.0f) row
	table isco88_1 wave sex [aw=wgt] if position == 3, c(mean earn) format(%4.0f) row
	* By skill level
	table skill wave [aw=wgt], c(mean earn) format(%4.0f) row 
	table skill wave sex [aw=wgt], c(mean earn) format(%4.0f) row 
	table skill wave [aw=wgt] if position == 2 | position==5, c(mean earn) format(%4.0f) row
	table skill wave [aw=wgt] if position == 3, c(mean earn) format(%4.0f) row	
	table skill wave sex [aw=wgt] if position == 2 | position==5, c(mean earn) format(%4.0f) row
	table skill wave sex [aw=wgt] if position == 3, c(mean earn) format(%4.0f) row

/* 	Changes in employment shares by occupational groups (at the 1-digit level) 
	and skill-level (low/middle/high) (as in Autor 2019, Figures 3-5).*/	
	
	preserve
		drop if isco88_1 == . 
		collapse (mean) skill (rawsum) wgt, by(wave isco88_1)
		lab val skill skill
		bys wave: egen pop = total(wgt)
		gen share = (wgt/pop)*100
		reshape wide share wgt pop, i(isco88_1 skill) j(wave)		
		gen chge12 = share2 - share1
		gen chge23 = share3 - share2		
		gen chge13 = share3 - share1		
		graph bar (asis) share1 share2 share3, over(isco88_1, sort(isco88_1) descending relabel(1 "Managers" 2 "Professionals" ///		 
			3 "Technicians"  4 "Clerks" 5 "Services" 6 "Skilled Agricultural" 7 "Trades Workers" ///
			8 "Machine Operators" 9 "Elementary") label(angle(stdarrow))) ytitle("Share in employment (%)") nofill ///
			graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) name(emplshares, replace) ///
			legend(on order(1 "2000" 2 "2010" 3 "2017") cols(3))
			graph save emplshares "$graphs\Figure_B1a_imputed.gph", replace
			graph export "$graphs\Figure_B1a_imputed.png", as(png) replace
		graph bar (asis) chge12 chge23, over(isco88_1, sort(isco88_1) descending relabel(1 "Managers" 2 "Professionals" ///		 
			3 "Technicians"  4 "Clerks" 5 "Services" 6 "Skilled Agricultural" 7 "Trades Workers" ///
			8 "Machine Operators" 9 "Elementary") label(angle(stdarrow))) ytitle("Change in share in employment (ppts)") nofill ///
			graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) name(emplchange, replace) stack ///
			legend(on order(1 "2000 - 2010" 2 "2010 - 2017") cols(9))
			graph save emplchange "$graphs\Figure_B1b_imputed.gph", replace
			graph export "$graphs\Figure_B1b_imputed.png", as(png) replace
	restore
	preserve
		drop if isco88_1 == . 
		collapse (rawsum) wgt, by(wave skill)
		lab val skill skill
		bys wave: egen pop = total(wgt)
		gen share = (wgt/pop)*100		
		reshape wide share wgt pop, i(skill) j(wave)
		gen chge12 = share2 - share1
		gen chge23 = share3 - share2		
		gen chge13 = share3 - share1		
		graph bar (asis) share1 share2 share3, over(skill, sort(skill) descending label(angle(stdarrow))) ytitle("Share in employment (%)") nofill ///
			graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) name(emplshares_gr, replace) ///
			legend(on order(1 "2000" 2 "2010" 3 "2017") cols(3))
			graph save emplshares_gr "$graphs\Figure_B2a_imputed.gph", replace
			graph export "$graphs\Figure_B2a_imputed.png", as(png) replace			
		graph bar (asis) chge12 chge23, over(skill, sort(skill) descending label(angle(stdarrow))) ytitle("Change in share in employment (ppts)") nofill ///
			graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) name(emplchange_gr, replace) stack ///
			legend(on order(1 "2000 - 2010" 2 "2010 - 2017") cols(2))
			graph save emplchange_gr "$graphs\Figure_B2b_imputed.gph", replace
			graph export "$graphs\Figure_B2b_imputed.png", as(png) replace			
	restore	
	preserve
		drop if isco88_1 == . | edu==.
		collapse (rawsum) wgt, by(wave skill edu)
		bys wave edu: egen pop = total(wgt)
		gen share = (wgt/pop)*100
		reshape wide share wgt pop, i(edu skill) j(wave)		
		gen chge12 = share2 - share1
		gen chge23 = share3 - share2		
		gen chge13 = share3 - share1		
		graph bar (asis) share1 share2 share3, over(skill, sort(skill) descending label(angle(stdarrow))) by(edu, note("") cols(4) legend(on) ///
			graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white))) ytitle("Share in employment (%)") ///
			legend(off order(1 "2000" 2 "2010" 3 "2017") cols(3)) name(emplshares_edu, replace)
			graph save emplshares_edu "$graphs\Figure_B3a_imputed.gph", replace
			graph export "$graphs\Figure_B3a_imputed.png", as(png) replace	
		graph bar (asis) chge12 chge23, over(skill, sort(skill) descending label(angle(stdarrow))) stack by(edu, note("") cols(4) legend(on) ///
			graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white))) ytitle("Change in employment share (ppts)")  ///
			legend(off order(1 "2000 - 2010" 2 "2010 - 2017") cols(2)) name(emplchange_edu, replace) xsize(16) ysize(8)
			graph save emplchange_edu "$graphs\Figure_B3b_imputed.gph", replace
			graph export "$graphs\Figure_B3b_imputed.png", as(png) replace	
	restore
	
/* 	Smoothed changes in employment shares and in earnings over time by skill percentiles 
	(occupations ranked by earnings in the initial or reference year) (as in AD2013, Figure 1). 
	Measures of polarization in the earnings growth pattern. */

	preserve
		local m = "mean"
		drop if isco88_1 == .
		collapse (`m') lnearn (rawsum) wgt [pw=wgt], by(wave isco88_2)
		bys wave: egen pop = total(wgt)
		gen share = (wgt/pop)*100
		reshape wide lnearn share wgt pop, i(isco88_2) j(wave)		
		gen chge12 = share2 - share1	
		gen chge23 = share3 - share2			
		gen chge13 = share3 - share1
		gen lnearn12 = lnearn2 - lnearn1	
		gen lnearn23 = lnearn3 - lnearn2			
		gen lnearn13 = lnearn3 - lnearn1
		sort lnearn1
		gen xt = _n
		tostring isco88_2, gen(aux)	
		gen chge = 0
		set graphics off
		twoway (bar chge12 xt, sort fcolor(bluishgray8) lcolor(bluishgray8)) ///
			   (scatter chge xt, sort mlabel(aux) mlabsize(small) mlabposition(6) mlabcolor(black) mcolor(none) lwidth(medium)), ///
			   graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) xscale(lcolor(white)) ///
			   yline(0, lcolor(black)) legend(off) title("2000 - 2010") ///
			   xlabel(none) xtitle("Occupations (ISCO-88) ranked by `m' log earnings") ///
			   ytitle("Change in employment share (ppts)") name(emplchange_isco_12, replace)
		twoway (bar chge23 xt, sort fcolor(bluishgray8) lcolor(bluishgray8)) ///
			   (scatter chge xt, sort mlabel(aux) mlabsize(small) mlabposition(6) mlabcolor(black) mcolor(none) lwidth(medium)), ///
			   graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) xscale(lcolor(white)) ///
			   yline(0, lcolor(black)) legend(off) title("2010 - 2017") ///
			   xlabel(none) xtitle("Occupations (ISCO-88) ranked by `m' log earnings") ///
			   ytitle("Change in employment share (ppts)") name(emplchange_isco_23, replace)
		twoway (bar chge13 xt, sort fcolor(bluishgray8) lcolor(bluishgray8)) ///
			   (scatter chge xt, sort mlabel(aux) mlabsize(small) mlabposition(6) mlabcolor(black) mcolor(none) lwidth(medium)), ///
			   graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) xscale(lcolor(white)) ///
			   yline(0, lcolor(black)) legend(off) title("2000 - 2017") ///
			   xlabel(none) xtitle("Occupations (ISCO-88) ranked by `m' log earnings") ///
			   ytitle("Change in employment share (ppts)") name(emplchange_isco_13, replace)
		set graphics on	
		graph combine emplchange_isco_12 emplchange_isco_23 emplchange_isco_13, title("Changes in employment (Self-employed included)") cols(1) ///
			xsize(5) ysize(10) graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) name(emplshares_isco, replace)
			graph save emplshares_isco "$graphs\Figure_B5a_imputed.gph", replace
			graph export "$graphs\Figure_B5a_imputed.png", as(png) width(500) height(1100) replace
		set graphics off	
		twoway (bar lnearn12 xt, sort fcolor(bluishgray8) lcolor(bluishgray8)) ///
			   (scatter chge xt, sort mlabel(aux) mlabsize(small) mlabposition(6) mlabcolor(black) mcolor(none) lwidth(medium)), ///
			   graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) xscale(lcolor(white)) ///
			   yline(0, lcolor(black)) legend(off) title("2000 - 2010") ///
			   xlabel(none) xtitle("Occupations (ISCO-88) ranked by `m' log earnings") ///
			   ytitle("Change in log earnings") name(incchange_isco_12, replace)
		twoway (bar lnearn23 xt, sort fcolor(bluishgray8) lcolor(bluishgray8)) ///
			   (scatter chge xt, sort mlabel(aux) mlabsize(small) mlabposition(6) mlabcolor(black) mcolor(none) lwidth(medium)), ///
			   graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) xscale(lcolor(white)) ///
			   yline(0, lcolor(black)) legend(off) title("2010 - 2017") ///
			   xlabel(none) xtitle("Occupations (ISCO-88) ranked by `m' log earnings") ///
			   ytitle("Change in log earnings") name(incchange_isco_23, replace)
		twoway (bar lnearn13 xt, sort  fcolor(bluishgray8) lcolor(bluishgray8)) ///
			   (scatter chge xt, sort mlabel(aux) mlabsize(small) mlabposition(6) mlabcolor(black) mcolor(none) lwidth(medium)), ///
			   graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) xscale(lcolor(white)) ///
			   yline(0, lcolor(black)) legend(off) title("2000 - 2017") ///
			   xlabel(none) xtitle("Occupations (ISCO-88) ranked by `m' log earnings") ///
			   ytitle("Change in log earnings") name(incchange_isco_13, replace)
		set graphics on	
		graph combine incchange_isco_12 incchange_isco_23 incchange_isco_13, title("Changes in log earnings (Self-employed included)") ycommon cols(1) ///
			xsize(5) ysize(10) graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) name(incchange_isco, replace)
			graph save incchange_isco "$graphs\Figure_B5b_imputed.gph", replace
			graph export "$graphs\Figure_B5b_imputed.png", as(png) width(500) height(1100) replace
	restore	
	
	* Skill percentile (ranked by 2000 occupational mean wage)
	preserve
		clear	
		tempfile xt_fix
		set obs 100
		gen xt = _n
		save `xt_fix'
	restore	
	preserve
		local m = "mean"
		drop if isco88_1 == .
		collapse (`m') lnearn (rawsum) wgt [pw=wgt], by(wave isco88)
		export excel using "$tables\Table_B6a_imputed", sheet(B6a) sheetreplace firstrow(variables)
		xtile xt = lnearn if wave==1 [pw=wgt], nq(100)
		bys isco88: egen aux=max(xt)
		replace xt=aux if wave>1
		collapse (`m') lnearn (rawsum) wgt [pw=wgt], by(wave xt)
		drop if xt==.
		bys wave: egen pop = total(wgt)
		gen share = (wgt/pop)*100
		reshape wide lnearn share wgt pop, i(xt) j(wave)		
		gen chge12 = share2 - share1	
		gen chge23 = share3 - share2			
		gen chge13 = share3 - share1
		gen lnearn12 = lnearn2 - lnearn1	
		gen lnearn23 = lnearn3 - lnearn2			
		gen lnearn13 = lnearn3 - lnearn1
		merge 1:1 xt using `xt_fix', nogen
			sort xt
			foreach var in lnearn1 wgt1 pop1 share1 lnearn2 wgt2 pop2 share2 lnearn3 wgt3 pop3 share3 chge12 chge23 chge13 lnearn12 lnearn23 lnearn13 {
				qui replace `var' = `var'[_n-1] if missing(`var')
				}
		set graphics off
		twoway (bar chge12 xt, sort cmissing(n) fcolor(bluishgray8) lcolor(bluishgray8))(lowess chge12 xt, bw(0.4) sort), ///
			ytitle("Change in employment share (ppts)") xtitle("Skill percentile (ranked by 2000 occupational `m' wage)") ///
			legend(on order(1 "2000 - 2010" 2 "smoothed") cols(2)) graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) ///
			xsize(10) ysize(5) yline(0, lcolor(black)) name(empchange_skilpc_12, replace)
		twoway (bar chge23 xt, sort cmissing(n) fcolor(bluishgray8) lcolor(bluishgray8))(lowess chge23 xt, bw(0.4) sort), ///
			ytitle("Change in employment share (ppts)") xtitle("Skill percentile (ranked by 2000 occupational `m' wage)") ///
			legend(on order(1 "2010 - 2017" 2 "smoothed") cols(2)) graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) ///
			xsize(10) ysize(5) yline(0, lcolor(black)) name(empchange_skilpc_23, replace)
		twoway (bar chge13 xt, sort cmissing(n) fcolor(bluishgray8) lcolor(bluishgray8))(lowess chge13 xt, bw(0.4) sort), ///
			ytitle("Change in employment share (ppts)") xtitle("Skill percentile (ranked by 2000 occupational `m' wage)") ///
			legend(on order(1 "2000 - 2017" 2 "smoothed") cols(2)) graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) ///
			xsize(10) ysize(5) yline(0, lcolor(black)) name(empchange_skilpc_13, replace)
		set graphics on	
		graph combine empchange_skilpc_12 empchange_skilpc_23 empchange_skilpc_13, title("Changes in employment (Self-employed included)") ycommon cols(1) ///
			xsize(5) ysize(10) graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) name(empchange_skilpc, replace)
			graph save empchange_skilpc "$graphs\Figure_B6a_imputed.gph", replace
			graph export "$graphs\Figure_B6a_imputed.png", as(png) width(500) height(1100) replace
		set graphics off
		twoway (bar lnearn12 xt, sort cmissing(n) fcolor(bluishgray8) lcolor(bluishgray8)) (lowess lnearn12 xt, bw(0.4) sort), ///
			ytitle("Change in log earnings") xtitle("Skill percentile (ranked by 2000 occupational `m' wage)") ///
			legend(on order(1 "2000 - 2010" 2 "Smoothed") cols(2)) graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) ///
			xsize(10) ysize(5) yline(0, lcolor(black)) name(incchange_skilpc_12, replace)
		twoway (bar lnearn23 xt, sort cmissing(n) fcolor(bluishgray8) lcolor(bluishgray8)) (lowess lnearn23 xt, bw(0.4) sort), ///
			ytitle("Change in log earnings") xtitle("Skill percentile (ranked by 2000 occupational `m' wage)") ///
			legend(on order(1 "2010 - 2017" 2 "Smoothed") cols(2)) graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) ///
			xsize(10) ysize(5) yline(0, lcolor(black)) name(incchange_skilpc_23, replace)
		twoway (bar lnearn13 xt, sort cmissing(n) fcolor(bluishgray8) lcolor(bluishgray8)) (lowess lnearn13 xt, bw(0.4) sort), ///
			ytitle("Change in log earnings") xtitle("Skill percentile (ranked by 2000 occupational `m' wage)") ///
			legend(on order(1 "2000 - 2017" 2 "Smoothed") cols(2)) graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) ///
			xsize(10) ysize(5) yline(0, lcolor(black)) name(incchange_skilpc_13, replace)
		set graphics on	
		graph combine incchange_skilpc_12 incchange_skilpc_23 incchange_skilpc_13, title("Changes in log earnings(Self-employed included)") ycommon cols(1) ///
			xsize(5) ysize(10) graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) name(incchange_skilpc, replace)
			graph save incchange_skilpc "$graphs\Figure_B6b_imputed.gph", replace
			graph export "$graphs\Figure_B6b_imputed.png", as(png) width(500) height(1100) replace
	restore
	
// Polarization tests
preserve
	drop if isco88 == .
	collapse (mean) lnearn (rawsum) wgt [pw=wgt], by(wave isco88)
	bys wave: egen pop = total(wgt)
	gen share = log(wgt/pop)
	reshape wide lnearn share wgt pop, i(isco88) j(wave)		
	gen chge12 = share2 - share1	
	gen chge23 = share3 - share2			
	gen chge13 = share3 - share1
	gen lnearn12 = lnearn2 - lnearn1	
	gen lnearn23 = lnearn3 - lnearn2			
	gen lnearn13 = lnearn3 - lnearn1
	corr chge12 lnearn12
	reg chge12 lnearn12 [pw=wgt1]
	corr chge23 lnearn23
	reg chge23 lnearn23 [pw=wgt2]
	corr chge13 lnearn13
	reg chge13 lnearn13 [pw=wgt1]
	* Fitting an OLS quadratic regression
	reg chge12 c.lnearn1 c.lnearn1#c.lnearn1 [pw=wgt1]
		outreg2 using "$tables\chgn_share_earn_imputed.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("PE, empl", "1-2") lab replace
	reg chge23 c.lnearn2 c.lnearn2#c.lnearn2 [pw=wgt2]
		outreg2 using "$tables\chgn_share_earn_imputed.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("PE, empl", "2-3") lab append
	reg chge13 c.lnearn1 c.lnearn1#c.lnearn1 [pw=wgt1]
		outreg2 using "$tables\chgn_share_earn_imputed.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("PE, empl", "1-3") lab append
	reg lnearn12 c.lnearn1 c.lnearn1#c.lnearn1 [pw=wgt1]
		outreg2 using "$tables\chgn_share_earn_imputed.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("PE, inc", "1-2") lab append
	reg lnearn23 c.lnearn2 c.lnearn2#c.lnearn2 [pw=wgt2]
		outreg2 using "$tables\chgn_share_earn_imputed.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("PE, inc", "2-3") lab append
	reg lnearn13 c.lnearn1 c.lnearn1#c.lnearn1 [pw=wgt1]
		outreg2 using "$tables\chgn_share_earn_imputed.xml", excel addstat(Adj. R-squared, e(r2_a), F-test, e(p)) dec(3) ctitle("PE, inc", "1-3") lab append

* Fitted plot of earnings polarization test
	set graphics off
	twoway (scatter lnearn12 lnearn1 [pw = wgt1], sort) (lfit lnearn12 lnearn1 [pw = wgt1]) ///
		   (qfit lnearn12 lnearn1 [pw = wgt1]), ///
		   graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) ///
		   xtitle("2000 Log earnings") ytitle("Change in log earnings") ///
		   title("2000 - 2010") legend(order(1 "Real values" 2 "Linear fitted values" 3 "Quadratic fitted values")) ///
		   name(polar_earn_12, replace)
	twoway (scatter lnearn23 lnearn2 [pw = wgt2], sort) (lfit lnearn23 lnearn2 [pw = wgt2]) ///
		   (qfit lnearn23 lnearn2 [pw = wgt2]), ///
		   graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) ///
		   xtitle("2010 Log earnings") ytitle("Change in log earnings") ///
		   title("2010 - 2017") legend(order(1 "Real values" 2 "Linear fitted values" 3 "Quadratic fitted values")) ///
		   name(polar_earn_23, replace)
	twoway (scatter lnearn13 lnearn1 [pw = wgt1], sort) (lfit lnearn13 lnearn1 [pw = wgt1]) ///
		   (qfit lnearn13 lnearn1 [pw = wgt1]), ///
		   graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) ///
		   xtitle("2000 Log earnings") ytitle("Change in log earnings") ///
		   title("2000 - 2017") legend(order(1 "Real values" 2 "Linear fitted values" 3 "Quadratic fitted values")) ///
		   name(polar_earn_13, replace)
	set graphics on
	graph combine polar_earn_12 polar_earn_23 polar_earn_13, ///
			xsize(15) ysize(5) graphregion(fcolor(white) lcolor(white)) ycommon rows(1) ///
			plotregion(fcolor(white)) name(polar_earn, replace)
	graph save polar_earn "$graphs/Figure_polar_earn_imputed.gph", replace
	graph export "$graphs/Figure_polar_earn_imputed.png", as(png) width(1200) height(400) replace
	
* Fitted plot of job polarization test
	set graphics off
	twoway (scatter chge12 lnearn1 [pw = wgt1], sort) (lfit chge12 lnearn1 [pw = wgt1]) ///
		  (qfit chge12 lnearn1 [pw = wgt1]), ///
		   graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) ///
		   xtitle("2000 Log earnings") ytitle("Change in employment share") ///
		   title("2000 - 2010") legend(order(1 "Real values" 2 "Linear fitted values" 3 "Quadratic fitted values")) ///
		   name(polar_job_12, replace)
	twoway (scatter chge23 lnearn2 [pw = wgt2], sort) (lfit chge12 lnearn1 [pw = wgt2]) ///
		   (qfit chge23 lnearn2 [pw = wgt2]), ///
		   graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) ///
		   xtitle("2010 Log earnings") ytitle("Change in employment share") ///
		   title("2010 - 2017") legend(order(1 "Real values" 2 "Linear fitted values" 3 "Quadratic fitted values")) ///
		   name(polar_job_23, replace)
	twoway (scatter chge13 lnearn1 [pw = wgt1], sort) (lfit chge12 lnearn1 [pw = wgt1]) ///
		   (qfit chge13 lnearn1 [pw = wgt1]), ///
		   graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white)) ///
		   xtitle("2000 Log earnings") ytitle("Change in employment share") ///
		   title("2000 - 2017") legend(order(1 "Real values" 2 "Linear fitted values" 3 "Quadratic fitted values")) ///
		   name(polar_job_13, replace)
	set graphics on
	graph combine polar_job_12 polar_job_23 polar_job_13, ///
			xsize(15) ysize(5) graphregion(fcolor(white) lcolor(white)) ycommon rows(1) ///
			plotregion(fcolor(white)) name(polar_job, replace)
	graph save polar_job "$graphs/Figure_polar_job_imputed.gph", replace
	graph export "$graphs/Figure_polar_job_imputed.png", as(png) width(1200) height(400) replace
restore


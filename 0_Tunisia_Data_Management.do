
********************************************************************************
********************************************************************************
***** Shared UNWIDER Codes
***** 28 Nov, 2019
********************************************************************************
********************************************************************************

	clear
	capture log close
	estimates clear
	
	global dofiles $desktop/Dofiles

		
// create local for date and version tracking

	local c_date = c(current_date)
	local date_string = subinstr("`c_date'", ":", "_", .)
	local date_string = subinstr("`date_string'", " ", "_", .)
	global track_date = c(current_date)
	global track_time = c(current_time)

// globals to manage users
		
	global user 1 /* put 1 for Michelle and 2 for Phuong */

	if $user == 1 {
	global desktop /Users/michelle/Dropbox/Tunisie_ENPE
	}

	if $user == 2 {
	global desktop C:/Users/Dell/Dropbox/Tunisie_ENPE

	}

// globals for trial or real data 

	
	global data $desktop/Data

	global trial 1 /* 
		* 0 if we are not in trial mode
		* 1 if we are in trial mode using individual data
		* 2 if we are in trial mode using household data */

	if $trial ==1 {
	global data $data/enquete_des_individus
	}


// make folders to organise work  
	cd
	cd $desktop
	
	foreach folder in Log Results {
 	cap mkdir "`folder'"
	global `folder' = "$desktop/`folder'"
	}
	
	foreach folder in PartA_Descriptive PartB_Econometric {
	cd $Results
	cap mkdir "`folder'"
	cd "`folder'"
	cap mkdir Graphs
	cap mkdir Tables
	}



//run log for commands
	 cd $log
	 cap log using Main_`date_string', text replace
	


********************************************************************************
* Github codes
cd $dofiles
! git init .
! git add Tunisia.stpr
! git commit -m "Begining of the dofile"
********************************************************************************


cd $data

use enpe2006fi.dta, clear

gen x = 1

** Two folders with data : "enquete des individus" and "enquete des menages"


********************************************************************************
* Github codes
! git add scatter*
! git commit -m "This is the end of the edits, `date_string' at $track_time"
! git remote add origin "https://github.com/PhuongMinhLe/Tunisia.git"
! git push --set-upstream origin master
********************************************************************************




shell PhuongMinhLe/UNWider

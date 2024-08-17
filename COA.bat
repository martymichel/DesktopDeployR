	@echo off
	setlocal
	
	REM Set the port number where your Shiny app is running
	set PORT=3570
	
	REM Check if the port is already in use
	netstat -ano | findstr :%PORT% > nul
	if %ERRORLEVEL% equ 0 (
	    echo Shiny app is already running on port %PORT%.
	) else (
	    echo Shiny app is not running. Starting the app...
	    wscript dist\script\wsf\run.wsf
	)
	
endlocal
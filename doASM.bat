@echo off
if %errorlevel% neq 0 exit /b %errorlevel%
echo --------------- Variables ---------------
rem name of program. Change it to your own program name
Set PRG=cwGS
rem current folder
Set ProjectFolder=.

Set MyAppleFolder=F:\Bruno\Dev\AppleWin
Set APPLEWIN=%MyAppleFolder%\AppleWin\Applewin.exe
Set MERLIN32ROOT=%MyAppleFolder%\Merlin32_v1.0
Set MERLIN32LIBS=%MERLIN32ROOT%\Library
Set MERLIN32WIN=%MERLIN32ROOT%\Windows
Set MERLIN32EXE=%MERLIN32WIN%\merlin32.exe
Set APPLECOMMANDER=%MyAppleFolder%\Utilitaires\AppleCommander-win64-1.6.0.jar
rem Set ACJAR=java.exe -jar %APPLECOMMANDER%    ; avec ""
Set ACJAR=java.exe -jar %APPLECOMMANDER%
rem echo %ACJAR%
Set DEST="F:\Bruno\Dev\AppleWin\GS\activeGS\Live.Install.po"

echo --------------- debut Merlin ---------------
%MERLIN32EXE% -V %MERLIN32LIBS% %ProjectFolder%\%PRG%.s
if exist %ProjectFolder%\error_output.txt exit
echo --------------- fin Merlin ---------------

echo --------------- Debut Applecommander ---------------
%ACJAR% -d %DEST% %PRG%
%ACJAR% -p %DEST% %PRG% S16 0 < %PRG%
rem %ACJAR% -p %DEST% %PRG% bin 8192 < %PRG%
echo --------------- fin Applecommander ---------------


rem %APPLEWIN% -d1 %PRG%.po
rem echo --------------- Debut ActiveGS ---------------
Set ACTIVEGSPATH=%MyAppleFolder%\GS\activeGS
cd %ACTIVEGSPATH%
rem .\ActiveGS.exe
rem echo --------------- Fin ActiveGS ---------------

echo --------------- Debut Crossrunner ---------------
Set CRPATH="C:\Program Files\Crossrunner\Crossrunner.exe" 
rem %CRPATH% 
echo --------------- fin Crossrunner ---------------
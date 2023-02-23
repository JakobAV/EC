@echo off
EC.exe > assembly.asm
ml64.exe assembly.asm /link /entry:main
assembly.exe
echo %errorlevel%

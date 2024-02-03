@echo off
for /r %%i in (*.hs) do (
    stylish-haskell.exe -i "%%i"
)
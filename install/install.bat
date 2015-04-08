mkdir %HOMEPATH%\AppData\Roaming\.emacs.d\lisp

bitsadmin.exe /transfer "DownloadInit"^
 https://raw.githubusercontent.com/codetech/lisp-tutorial/master/init.el^
 C:\%HOMEPATH%\AppData\Roaming\.emacs.d\init.el

bitsadmin.exe /transfer "DownloadModern"^
 https://raw.githubusercontent.com/jacksonrayhamilton/modern.el/master/modern.el^
 C:\%HOMEPATH%\AppData\Roaming\.emacs.d\lisp\modern.el

bitsadmin.exe /transfer "DownloadEmacs"^
 http://ftp.gnu.org/gnu/emacs/windows/emacs-24.4-bin-i686-pc-mingw32.zip^
 C:\%HOMEPATH%\Downloads\emacs.zip

mkdir Downloads\emacs
cd Downloads\emacs
copy ..\..\j_unzip.vbs
copy ..\emacs.zip
cscript //B j_unzip.vbs emacs.zip

bin\runemacs.exe

Niniejsze repozytorium zawiera szablon projektu w języku C,
korzystającego z gniazd sieciowych oraz biblioteki SDL2
dla środowiska [MSYS2/MinGW](https://www.msys2.org).

Do zbudowania projektu niezbędna jest instalacja pakietów
make, GCC oraz SDL2, co można zrobić w shellu MSYS2
przy pomocy polecenia:

     pacman -S make mingw-w64-x86_64-gcc mingw-w64-x86_64-SDL2

Ważne jest, aby uruchomić powłokę `MSYS2 MinGW 64-bit`.

Ponadto program zawiera prosty symulator środowiska w języku
[Racket](https://racket-lang.org). Aby go uruchomić, należy
zainstalować środowisko, załadować plik `envsim.rkt`
i wcisnąć zielony przycisk "Play" w prawym górnym rogu okna.

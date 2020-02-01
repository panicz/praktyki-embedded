LIBS = -pthread
SDL_LIBS = -lSDL2main -lSDL2
HOST_OS =  $(shell uname -o)
CC = gcc
#mingw-w64-x86_64-gcc


ifeq ($(HOST_OS), Msys)
	LIBS += -mwindows -lws2_32 -DMINGW -lmingw32
endif

all: tcpclient screen

tcpclient: cl.c
	$(CC) $(CFLAGS) $< -o $@ $(LIBS)

screen: screen.c
	$(CC) $(CFLAGS) -DTARGET=SIMULATOR $< -o $@ $(LIBS) $(SDL_LIBS)

clean:
	rm tcpclient screen

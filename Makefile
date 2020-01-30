HOST_OS =  $(shell uname -o)

ifeq ($(HOST_OS), Msys)
	LIBS = -mwindows -lws2_32 -lmingw32 -DMINGW
endif

all: tcpclient

tcpclient: cl.c
	gcc $(CFLAGS) $< -o $@ $(LIBS)

clean:
	rm tcpclient


#include "config.h"

#if TARGET == SIMULATOR

#ifdef MINGW

#define WIN32_LEAN_AND_MEAN

#include <winsock2.h>
#include <Ws2tcpip.h>
#include <errno.h>

#else // !MINGW

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>

#endif // !MINGW

#include <pthread.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <stdint.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>

#include <SDL2/SDL.h>

#endif // TARGET == SIMULATOR

#define INFO(msg, ...) printf(msg"\n", ## __VA_ARGS__); fflush(stdout);

struct {
#if TARGET==SIMULATOR
  int socket;
  SDL_Window *window;
  SDL_Surface *surface;
  pthread_t sync_screen;
  const uint32_t LTDC_CLUT[256];
  uint16_t sync_line;
#endif // TARGET==SIMULATOR
  uint8_t videobuffer[SCREEN_HEIGHT * SCREEN_WIDTH];
} Global = {
  .sync_line = SCREEN_HEIGHT - 1,
  .LTDC_CLUT = {
    0x000000, 0x000008, 0x000010, 0x000018, 0x000020, 0x000028, 0x000030,
    0x000038, 0x000040, 0x000048, 0x000050, 0x000058, 0x000060, 0x000068,
    0x000070, 0x000078, 0x000080, 0x000088, 0x000090, 0x000098, 0x0000A0,
    0x0000A8, 0x0000B0, 0x0000B8, 0x0000C0, 0x0000C8, 0x0000D0, 0x0000D8,
    0x0000E0, 0x0000E8, 0x0000F0, 0x0000F8, 0x0000FF,
    
    0x0800FF, 0x1000FF, 0x1800FF, 0x2000FF, 0x2800FF, 0x3000FF, 0x3800FF,
    0x4000FF, 0x4800FF, 0x5000FF, 0x5800FF, 0x6000FF, 0x6800FF, 0x7000FF,
    0x7800FF, 0x8000FF, 0x8800FF, 0x9000FF, 0x9800FF, 0xA000FF, 0xA800FF,
    0xB000FF, 0xB800FF, 0xC000FF, 0xC800FF, 0xD000FF, 0xD800FF, 0xE000FF,
    0xE800FF, 0xF000FF, 0xF800FF, 0xFF00FF,

    0xFF00F8, 0xFF00F0, 0xFF00E8, 0xFF00E0, 0xFF00D8, 0xFF00D0, 0xFF00C8,
    0xFF00C0, 0xFF00B8, 0xFF00B0, 0xFF00A8, 0xFF00A0, 0xFF0098, 0xFF0090,
    0xFF0088, 0xFF0080, 0xFF0078, 0xFF0070, 0xFF0068, 0xFF0060, 0xFF0058,
    0xFF0050, 0xFF0048, 0xFF0040, 0xFF0038, 0xFF0030, 0xFF0028, 0xFF0020,
    0xFF0018, 0xFF0010, 0xFF0008, 0xFF0000,

    0xFF0800, 0xFF1000, 0xFF1800, 0xFF2000, 0xFF2800, 0xFF3000, 0xFF3800,
    0xFF4000, 0xFF4800, 0xFF5000, 0xFF5800, 0xFF6000, 0xFF6800, 0xFF7000,
    0xFF7800, 0xFF8000, 0xFF8800, 0xFF9000, 0xFF9800, 0xFFA000, 0xFFA800,
    0xFFB000, 0xFFB800, 0xFFC000, 0xFFC800, 0xFFD000, 0xFFD800, 0xFFE000,
    0xFFE800, 0xFFF000, 0xFFF800, 0xFFFF00,

    0xFFFF08, 0xFFFF10, 0xFFFF18, 0xFFFF20, 0xFFFF28, 0xFFFF30, 0xFFFF38,
    0xFFFF40, 0xFFFF48, 0xFFFF50, 0xFFFF58, 0xFFFF60, 0xFFFF68, 0xFFFF70,
    0xFFFF78, 0xFFFF80, 0xFFFF88, 0xFFFF90, 0xFFFF98, 0xFFFFA0, 0xFFFFA8,
    0xFFFFB0, 0xFFFFB8, 0xFFFFC0, 0xFFFFC8, 0xFFFFD0, 0xFFFFD8, 0xFFFFE0,
    0xFFFFE8, 0xFFFFF0, 0xFFFFF8, 0xFFFFFF,

    [161 ... 255] = 0xFFFFFF
  }
};


#if TARGET==SIMULATOR
void LTDC_LineEvent(void) {
  // screen finshed drawing
}

void *LTDC_SDL_sync(void *unused) {
  
  while (1) {
    uint32_t *pixels = (uint32_t *) Global.surface->pixels;
    for (int line = 0; line < SCREEN_HEIGHT; ++line) {
      for (int pixel = 0; pixel < SCREEN_WIDTH; ++pixel) {
	pixels[line*SCREEN_WIDTH + pixel] = 
	  Global.LTDC_CLUT[Global.videobuffer[line*SCREEN_WIDTH + pixel]];
      }
      usleep(6);
      if (line == Global.sync_line) {
        LTDC_LineEvent();
      }
    }
    SDL_UpdateWindowSurface(Global.window);
    usleep(64);
  }
  return NULL;
}
#endif // TARGET==SIMULATOR

int current_temperature_C(void) {
  int temperature_C = 20;
#if TARGET == SIMULATOR
  int n = send(Global.socket, "temperature\n",
               sizeof("temperature\n"), 0);
  assert(n == sizeof("temperature\n"));
  char buffer[16];
  n = recv(Global.socket, buffer, sizeof(buffer), 0);
  assert(n > 0 && n < 16);
  buffer[n] = '\0';
  sscanf(buffer, "%d", &temperature_C);
#elif TARGET == DEVICE
  i2c_read(temperatureSensor, &temperature_C);
#else
#  error "Unknown target"
#endif // TARGET
  return temperature_C;
}



void fill_rect(uint16_t x, uint16_t y,
	       uint16_t w, uint16_t h,
	       uint8_t color) {
  for (int j = 0; j < h; ++j) {
    for (int i = 0; i < w; ++i) {
      int k = (j+y)*SCREEN_WIDTH + x + i;
      if (k < SCREEN_WIDTH*SCREEN_HEIGHT) {
	Global.videobuffer[k] = color;
      }
      else {
	INFO("attempt to render beyond the buffer area");
      }
    }
  }
}


int main(int argc, char *argv[]) {
  
#if TARGET==SIMULATOR
#ifdef MINGW
    WSADATA wsadata;
    if (WSAStartup(MAKEWORD(1,1), &wsadata) == SOCKET_ERROR) {
      INFO("Failed to initialize winsock subsystem");
      return -1;
    }
    INFO("Initialized winsock");
#endif // MINGW

    sleep(1);
    
    Global.socket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    assert(Global.socket >= 0);
    
    struct sockaddr_in address;
    address.sin_addr.s_addr = inet_addr("127.0.0.1");
    address.sin_family = AF_INET;
    address.sin_port = htons(12345);

    int error = connect(Global.socket,
			(struct sockaddr *) &address,
			sizeof(address));
    assert(!error);

    SDL_Init(SDL_INIT_VIDEO);
    Global.window =
      SDL_CreateWindow("embedded device",
		       SDL_WINDOWPOS_UNDEFINED,
		       SDL_WINDOWPOS_UNDEFINED,
		       SCREEN_WIDTH, SCREEN_HEIGHT,
		       0);
    Global.surface = SDL_GetWindowSurface(Global.window);
    assert(Global.surface);


    pthread_create(&Global.sync_screen, NULL,
		   LTDC_SDL_sync, NULL);
    
#elif TARGET==DEVICE
    //inicjalizacja peryferiow
#endif // TARGET

    while (1) {
      int temperature_C = current_temperature_C();
      uint8_t color;
      if (temperature_C < -16) {
	color = 0;
      }
      else if (temperature_C < 145) {
	color = (uint8_t) (temperature_C + 16);
      }
      else {
	color = 255;
      }
      fill_rect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, color);

#if TARGET==SIMULATOR
      SDL_Event event;
      while (SDL_PollEvent(&event)) {
	if (event.type == SDL_QUIT) {
	  exit(0);
	}
      }
#endif // TARGET==SIMULATOR
      
      
    }
    
    return 0;
}

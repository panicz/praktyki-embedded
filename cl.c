#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <stdint.h>
#include <ctype.h>
#include <string.h>

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

int main(int argc, const char *argv[]) {

#ifdef MINGW
    WSADATA wsadata;
    if (WSAStartup(MAKEWORD(1,1), &wsadata) == SOCKET_ERROR) {
        puts("Failed to initialize winsock subsystem");
        return -1;
    }
    puts("Initialized winsock");

#endif // MINGW

    int sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    assert(sock >= 0);
    
    struct sockaddr_in address;
    address.sin_addr.s_addr = inet_addr("127.0.0.1");
    address.sin_family = AF_INET;
    address.sin_port = htons(12345);

    int error = connect(sock, (struct sockaddr *) &address, sizeof(address));
    assert(!error);
    
    char line[255];
    char response[255];
    while (1) {
        fgets(line, sizeof(line), stdin);
        int sent = send(sock, line, strlen(line), 0);
        int received = recv(sock, response, sizeof(response), 0);
        response[received] = '\0';
        printf("%s", response);
        fflush(stdout);
    }

    return 0;
}

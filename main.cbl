IDENTIFICATION DIVISION.
PROGRAM-ID. SimpleHTTPServer.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
    FUNCTION ALL INTRINSIC.

DATA DIVISION.
WORKING-STORAGE SECTION.

77 ErrNo                            USAGE IS POINTER.
77 SocketFunction                   PIC X(08)  USAGE IS POINTER.
77 BindFunction                     PIC X(08)  USAGE IS POINTER.
77 RecvFunction                     PIC X(08)  USAGE IS POINTER.
77 SendFunction                     PIC X(08)  USAGE IS POINTER.

01 Socket-ID                        PIC S9(9) COMP.
01 Server-Address                   USAGE INET-SOCKADDR.
    05 Server-Port                  PIC S9(4) COMP VALUE 8080.
    05 Server-IPAddress             PIC X(4)  VALUE X"00000000".
77 ClientSocketFunction             PIC X(08)  USAGE IS POINTER.

01 ClientSocket-ID                  PIC S9(9) COMP.                                                    
01 Client-Address                   USAGE INET-SOCKADDR.
    05 Client-Port                  PIC S9(4) COMP.
    05 Client-IPAddress             PIC X(4).
01 Address-Length                   PIC S9(9) COMP.

01 Request-Buffer                   PIC X(2048).
01 Request-Content-Length           PIC S9(9) COMP.

01 Request-Line                     PIC X(2048).
01 RequestMethod                    PIC X(4).
01 RequestPath                      PIC X(255).
01 RequestProtocol                  PIC X(10).
01 Response-Buffer                  PIC X(2048).
01 Response-Status-Line             PIC X(15) VALUE "HTTP/1.1 200 OK".
01 Response-Body                    PIC X(255) VALUE "Hello, world!".

LINKAGE SECTION.
01 Socket-Input                     USAGE IS POINTER.
01 Socket-Output                    USAGE IS POINTER.

PROCEDURE DIVISION.
Main-Loop-Section.
    PERFORM Initialize-Server
    PERFORM Accept-Connection
    PERFORM READ-CLIENT-REQUEST
    PERFORM PROCESS-CLIENT-REQUEST
    PERFORM SEND-CLIENT-RESPONSE
    *> Other procedures

Initialize-Server.
    *> Load C functions from libcob library
    CALL 'C$IMPORT_SYM' USING "C$SIN" SocketFunction
                              ON EXCEPTION
                                  DISPLAY "Error: Could not load socket function"
                                  STOP RUN
                              END-CALL
    .
    CALL 'C$IMPORT_SYM' USING "C$BIN" BindFunction
                              ON EXCEPTION
                                  DISPLAY "Error: Could not load bind function"
                                  STOP RUN
                              END-CALL
    .

    *> Create the socket
    CALL SocketFunction USING Socket-Input
                              RETURNING Socket-ID
                              ON EXCEPTION
                                  DISPLAY "Error: Could not create socket"
                                  STOP RUN
                              END-CALL
    
    *> Bind the socket
    SET Address-Length TO LENGTH OF Server-Address
    CALL BindFunction USING BY VALUE Socket-ID
                        BY REFERENCE Server-Address
                        BY VALUE Address-Length
                        RETURNING ErrNo
                        ON EXCEPTION
                            DISPLAY "Error: Could not bind socket"
                            STOP RUN
                        END-CALL
    .

    *> Listen for incoming connections
    CALL 'C$SLI' USING BY VALUE Socket-ID
                BY VALUE 100
                RETURNING ErrNo
                ON EXCEPTION
                    DISPLAY "Error: Could not start listening"
                    STOP RUN
                END-CALL
    .
Accept-Connection.
    *> Load accept function from libcob library
    CALL 'C$IMPORT_SYM' USING "C$ACE" ClientSocketFunction
                              ON EXCEPTION
                                  DISPLAY "Error: Could not load accept function"
                                  STOP RUN
                              END-CALL
    .

    *> Accept incoming connections
    SET Address-Length TO LENGTH OF Client-Address
    CALL ClientSocketFunction USING BY VALUE Socket-ID
                               BY REFERENCE Client-Address
                               BY VALUE Address-Length
                               RETURNING ClientSocket-ID
                               ON EXCEPTION
                                   DISPLAY "Error: Could not accept connection"
                                   STOP RUN
                               END-CALL
    .

    *> Display client address information
    DISPLAY "Client connected from: " Client-IPAddress " Port: " Client-Port
    .
READ-CLIENT-REQUEST.
    *> Load recv function from libcob library
    CALL 'C$IMPORT_SYM' USING "C$REC" RecvFunction
                              ON EXCEPTION
                                  DISPLAY "Error: Could not load recv function"
                                  STOP RUN
                              END-CALL
    .

    *> Read data from client
    CALL RecvFunction USING BY VALUE ClientSocket-ID
                       BY REFERENCE Request-Buffer
                       BY VALUE LENGTH OF Request-Buffer
                       BY VALUE 0
                       RETURNING Request-Content-Length
                       ON EXCEPTION
                           DISPLAY "Error: Could not read data from client"
                           STOP RUN
                       END-CALL
    .

    *> Display received data
    DISPLAY "Received data from client: "
    DISPLAY FUNCTION TRIM(Request-Buffer)
    .
PROCESS-CLIENT-REQUEST.
    *> Parse the request line from the buffer
    UNSTRING Request-Buffer DELIMITED BY X"0D0A" INTO Request-Line

    *> Parse the request method, path and protocol from the request line
    UNSTRING Request-Line DELIMITED BY SPACE 
        INTO RequestMethod RequestPath RequestProtocol

    *> Determine the request type (GET) and generate a response
    IF RequestMethod = 'GET' THEN
        PERFORM GENERATE-RESPONSE

GENERATE-RESPONSE.
    *> Generate the status line and headers for the response
    STRING Response-Status-Line X"0D0A"
           "Content-Type: text/plain" X"0D0A"
           "Content-Length: " FUNCTION LENGTH(Response-Body) X"0D0A"
           X"0D0A"
        INTO Response-Buffer

    *> Append the response body to the buffer
    STRING Response-Buffer Response-Body
        INTO Response-Buffer
    .
SEND-CLIENT-RESPONSE.
    *> Load send function from libcob library
    CALL 'C$IMPORT_SYM' USING "C$SEN" SendFunction
                              ON EXCEPTION
                                  DISPLAY "Error: Could not load send function"
                                  STOP RUN
                              END-CALL
    .

    *> Send response data to the client
    CALL SendFunction USING BY VALUE ClientSocket-ID
                      BY REFERENCE Response-Buffer
                      BY VALUE LENGTH OF FUNCTION TRIM(Response-Buffer)
                      BY VALUE 0
                      RETURNING ErrNo
                      ON EXCEPTION
                          DISPLAY "Error: Could not send response to client"
                          STOP RUN
                      END-CALL
    .

    *> Close the connection
    PERFORM CLOSE-CLIENT-SOCKET
    .

CLOSE-CLIENT-SOCKET.
    CALL 'C$SLO' USING BY VALUE ClientSocket-ID
                RETURNING ErrNo
                ON EXCEPTION
                    DISPLAY "Error: Could not close client connection"
                    STOP RUN
                END-CALL
    .
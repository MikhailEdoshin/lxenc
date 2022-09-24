/* lx.h. GNU AGPL.
Copyright (C) Mikhail Edoshin. Thy will be done. */

# ifndef LX_H
# define LX_H

# include <inttypes.h>

/* Table of names 

  LxBChr         byte when used as an US ASCII character
  LxWChr         a two-byte encoding unit
  LxUChr         Unicode code (any; not necessarily a character category)
  LxUCHR         format code to print the numeric value of 'UChr'.

  Lx_ARRAYSZ     compute the length of a preallocated array
  
  LxEnc          encoding definition type
  LxEncRes       result of 'encToRes'

  LxDec          decoder
  LxDecType      decoder type
  LxDecType_CP   copying decoder code
  LxDecType_NCP  non-copying decoder code
  LxDecRep       decoding report with 0..3 results
  LxDecRes       decoding result
  LxDecRes_CODE  code
  LxDecRes_EBRK  error: incomplete sequence
  LxDecRes_EINV  error: invalid byte
  LxDecRes_ECTX  error: contextually invalid byte
  LxDecRes_ECNT  error: unexpected continuation byte
  LxDecRes_ECODE error: invalid code (not allowed in this encoding)
  LxDecRes_ESURL error: lone high surrogate
  LxDecRes_ESURH error: lone low surrogate
  LxDecResStr    descriptions of result types

  lxEncUtf8      encoding, UTF-8
  lxEncUtf16Be   encoding, UTF-16BE
  lxEncUtf16Le   encoding, UTF-16LE */

/* Byte. */

typedef uint8_t LxBChr;

/* A 16-bit 'character' ('wide'). */

typedef uint16_t LxWChr;

/* A Unicode code, 0x0..10FFFFF and a format code to print it. */

typedef uint_fast32_t LxUChr;

# define LxUCHR PRIuFAST32

/* A macro to compute the length of preallocated array. */

# define Lx_ARRAYSZ(a) (sizeof (a) / sizeof (a[0]))

/* Forward declarations and synonyms. */

typedef struct LxEnc         LxEnc;
typedef struct LxEncRes      LxEncRes;
typedef struct LxDec         LxDec;
typedef struct LxDecType     LxDecType;
typedef struct LxDecRep      LxDecRep;
typedef struct LxDecRes      LxDecRes;
typedef struct LxDecStateUtf LxDecStateUtf; 

/* An encoding is an object that provides a set of functions to work with 
encoded byte sequences. The individual objects are allocated statically in 
the library (for example, 'lxEncUtf8').

  scan(mem): 1..4, 6
    Determine the size in bytes of the code that starts at the specified 
    address. The address is trusted to contain a valid sequence in this 
    encoding.

  scanRev(mem): 1..4, 6
    Determine the size in bytes of the code that ends at the specified 
    address. The address is trusted to contain an end of a valid sequence in 
    this encoding. .

  read(mem, size): uchr
    Read the numeric Unicode value of the code at the specified address. The 
    address is trusted to point to the start of a valid sequence in this 
    encoding of the specified size.

  encToMem(chr, mem): mem
    Encode the specified Unicode character to the specified buffer. The 
    buffer is assumed to have enough space for a longest code in this 
    encoding.

  encToRes(chr): encRes
    Encode the specified Unicode character to the result that contains a
    buffer sufficient to represent any code.

  makeDec(mem, type)
    Initialize a testing decoder in a preallocated piece of memory. The 
    decoder is described further. The 'buf' instructs the decoder to maintain 
    an internal buffer for the data. For the client the buffer is read-ony. 
    If this is not necessary (when the data are in memory or the caller 
    manages copying on its own), pass 'NULL'.

  All functions except 'makeDec' operate only on trusted strings. The only 
  way to work with untrusted string is to check it first via a decoder. */

struct LxEnc {
  const char* name; /* encoding name, US ASCII */
    uint_fast8_t (* const scan    )(const void* mem);
    uint_fast8_t (* const scanRev )(const void* mem);
    LxUChr       (* const read    )(const void* mem, uint_fast8_t);
    LxBChr*      (* const encToMem)(LxUChr, LxBChr*);
    LxEncRes     (* const encToRes)(LxUChr);
    void         (* const makeDec )(LxDec*, uint_fast8_t);
};

/* An encoding result is a small self-contained buffer that stores the result
of encoding. */

struct LxEncRes {
    uint8_t len   ; /* length of the buffer */
    LxBChr  buf[6]; /* the buffer */
};

/* A decoder is an opaque type that stores the decoding state as we go. The 
public properties are 'type' that points to decoder type and, if the decoder
is copying, 'buf' to read the data. The rest are implementation details. */

struct LxDec {
  const LxDecType*   type  ; /* decoder type */
  const LxBChr       buf[7]; /* internal buffer for a copying decoder */
    union {
        struct LxDecStateUtf {
            uint8_t  len   ;
            uint8_t  mod   ;
        }            utf   ; /* UTF-8, UTF-16 */
    }                state ; /* decoder state; see implementation */
};

/* A decoder type identifies the type, stores a link to the encoding and 
provides two methods:

  feed(decoder, byte)  Feed a byte to the decoder.
  stop(decoder)        Signal the decoder there are no more input.

Both methods return a decoding report; see below. */

struct LxDecType {
  const uint8_t type; /* type code, 'LxDecType_CP/NCP'. */
  const LxEnc*  enc ; /* encoding */
    LxDecRep (* const feed)(LxDec*, LxBChr);
    LxDecRep (* const stop)(LxDec*);
};

/* There are two types of decoders: copying and non-copying. A copying 
decoder copies the received bytes in an internal buffer until they are ready
to be reported. A non-copying decoder does not make copies and is more 
efficient when decoding from memory. */

# define LxDecType_CP  0
# define LxDecType_NCP 1

/* NOTE: The logic of decoding is identical. The difference is that a copying 
decoder manipulates the buffer and reports non-negative offsets (from the 
start), while a non-copying variant does not use the buffer and reports 
non-positive offsets (from the end). */

/* A decoder report contains from 0 to 3 results. A single result describes 
the type of a detected sequence of bytes and its location. */

struct LxDecRep {
    uint8_t      cnt    ; /* number of results, 0..3 */
    struct LxDecRes {
        uint8_t  type   ; /* result type, 'LxDecRes_?'. */
        signed   off : 4; /* offset to the first byte, -6..+6; see [OFF]. */
        unsigned len : 4; /* number of bytes, 1..4 */
    }            res[3] ; /* results' data */
};

/* [OFF] The 'LxDecRes.off' field is signed because a copying decoder reports 
offset relative to the start of the internal buffer so the offset is 
non-negative. A non-copying decoder reports the offset relative to the 
current position in the string so the offset is non-positive. This makes the 
usage of the value uniform: '<baseAddress> + off'. */

/* NOTE: The report is returned directly, so the type is tuned to fit, if 
possible, into 8 bytes, that is a single register on 64-bit machines. */

/* Possible values for 'LxDecRes.type' and, in the comment, the range of '
'len' for this type). The only valid result is 'CODE', the rest are 
errors. */

# define LxDecRes_CODE  0 /* valid code .................. 1-4, 6  */
# define LxDecRes_EBRK  1 /* incomlete sequence .......... 1-3     */
# define LxDecRes_EINV  2 /* invalid byte ................ 1       */
# define LxDecRes_ECTX  3 /* contextually invalid byte ... 2       */
# define LxDecRes_ECNT  4 /* unexpected tail byte ........ 1       */
# define LxDecRes_ECODE 5 /* invalid code ................ 3, 4, 6 */
# define LxDecRes_ESURH 6 /* lone hi-surr. in CESU-8 ..... 3       */
# define LxDecRes_ESURL 7 /* lone lo-surr. in CESU-8 ..... 3       */

/* Human-readable strings for 'LxDecRes' names. */

extern const char* lxDecResStr[];

/* Encoding object instances for specific encodings. */

extern const LxEnc
  lxEncUtf8    ,
  lxEncUtf16Be ,
  lxEncUtf16Le ;

# endif /* LX_H */
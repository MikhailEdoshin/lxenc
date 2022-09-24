/* lx-enc-utf16.h. GNU AGPL.
Copyright (C) 2022 Mikhail Edoshin. Thy will be done. */

# include "lx.h"

/* Table of names.

                        n a t i v e   e n d i a n n e s s
  LxEncUtf16_Scan       scan a code
  LxEncUtf16_ScanRev    scan a code in reverse direction
  LxEncUtf16_Read       read a code
  
                        b i g    e n d ia n
  LxEncUtf16Be_Scan     scan a code
  LxEncUtf16Be_ScanRev  scan a code in reverse direction
  LxEncUtf16Be_Read     read a code
  LxEncUtf16Be_EncToMem encode to memory
  LxEncUtf16Be_EncToRes encode to encoding result
  LxDecUtf16Be_FeedCp   feed a byte into a copying decoder
  LxDecUtf16Be_FeedNcp  feed a byte into a non-copying decoder
  LxDecUtf16Be_StopCp   stop a copying decoder
  LxDecUtf16Be_StopNcp  stop a non-copying decoder
  LxEncUtf16Be_MakeDec  make a decoder
  
                        l i t t l e    e n d ia n
  LxEncUtf16Le_Scan     scan a code
  LxEncUtf16Le_ScanRev  scan a code in reverse direction
  LxEncUtf16Le_Read     read a code
  LxEncUtf16Le_EncToMem encode to memory
  LxEncUtf16Le_EncToRes encode to encoding result
  LxDecUtf16Le_FeedCp   feed a byte into a copying decoder
  LxDecUtf16Le_FeedNcp  feed a byte into a non-copying decoder
  LxDecUtf16Le_StopCp   stop a copying decoder
  LxDecUtf16Le_StopNcp  stop a non-copying decoder
  LxEncUtf16Le_MakeDec  make a decoder
  
  lxEncUtf16            UTF-16 in native endianness
  lxEncUtf16Be          UTF-16, big endian
  lxEncUtf16Le          UTF-16, little endian */

/* Implementation of 'LxEnc.scan' for UTF16-BE. */

static uint_fast8_t
LxEncUtf16Be_Scan(
  const void *mem)
{
    LxBChr bchr; uint_fast8_t r;

    bchr = *(LxBChr*)mem;
    if (bchr < 0xD8 || bchr > 0xDF)
        r = 2;
    else
        r = 4;
    return r;
}

/* Implementation of 'LxEnc.scanRev' for UTF16-BE */

static uint_fast8_t
LxEncUtf16Be_ScanRev(
  const void *mem)
{
    LxBChr bchr; uint_fast8_t r;

    bchr = *((LxBChr*)mem - 1);
    if (bchr < 0xD8 || bchr > 0xDF)
        r = 2;
    else
        r = 4;
    return r;
}

/* Implementation of 'LxEnc.scan' for UTF-16LE. */

static uint_fast8_t
LxEncUtf16Le_Scan(
  const void* mem)
{
    LxBChr bchr; uint_fast8_t r;

    bchr = *((LxBChr*)mem + 1);
    if (bchr < 0xD8 || bchr > 0xDF)
        r = 2;
    else
        r = 4;
    return r;
}

/* Implementation of 'LxEnc.scanRev' for UTF16-LE. */

static uint_fast8_t
LxEncUtf16Le_ScanRev(
  const void *mem)
{
    LxBChr bchr; uint_fast8_t r;

    bchr = *(LxBChr*)mem;
    if (bchr < 0xD8 || bchr > 0xDF)
        r = 2;
    else
        r = 4;
    return r;
}

/* Implementation of 'LxEnc.read' for UTF-16BE. */

static LxUChr
LxEncUtf16Be_Read(
  const void*        mem,
    uint_fast8_t len)
{
    const LxBChr *bbuf; LxUChr r;

    bbuf = mem;
    if (len == 2) {
        r  = bbuf[0] << 8;
        r |= bbuf[1];
    }
    else {
        r  = (bbuf[0] & 0x03) << 18;
        r |= (bbuf[1]       ) << 10;
        r |= (bbuf[2] & 0x03) <<  8;
        r |= (bbuf[3]       )      ;
        r += 0x10000;
    }
    return r;
}

/* Implementation of 'LxEnc.read' for UTF-16LE. */

static LxUChr
LxEncUtf16Le_Read(
  const void*    mem,
    uint_fast8_t len)
{
    const LxBChr *bbuf; LxUChr r;

    bbuf = mem;
    if (len == 2) {
        r  = bbuf[0];
        r |= bbuf[1] << 8;
    }
    else {
        r  = (bbuf[0]       ) << 10;
        r |= (bbuf[1] & 0x03) << 18;
        r |= (bbuf[2]       )      ;
        r |= (bbuf[3] & 0x03) <<  8;
        r += 0x10000;
    }
    return r;
}

/* Implementation of 'LxEnc.encToMem' for UTF-16BE. */

static LxBChr*
LxEncUtf16Be_EncToMem(
    LxUChr  chr,
    LxBChr* buf)
{
    if (chr <= 0xFFFF) {
        buf[0] = (chr & 0xFF00) >> 8;
        buf[1] = (chr & 0x00FF)     ;
        buf += 2;
    }
    else {
        chr -= 0x10000;
        buf[0] = 0xD8 | ((chr & (0x03 << 18)) >> 18);
        buf[1] =        ((chr & (0xFF << 10)) >> 10);
        buf[2] = 0xDC | ((chr & (0x03 <<  8)) >>  8);
        buf[3] =         (chr &  0xFF              );
        buf += 4;
    }
    return buf;
}

/* Implementation of 'LxEnc.encToRes' for UTF-16BE. */

static LxEncRes
LxEncUtf16Be_EncToRes(
    LxUChr chr)
    
{
    LxEncRes res;

    if (chr <= 0xFFFF) {
        res.len = 2;
        res.buf[0] = (chr & 0xFF00) >> 8;
        res.buf[1] = (chr & 0x00FF)     ;
    }
    else {
        res.len = 4;
        chr -= 0x10000;
        res.buf[0] = 0xD8 | ((chr & (0x03 << 18)) >> 18);
        res.buf[1] =        ((chr & (0xFF << 10)) >> 10);
        res.buf[2] = 0xDC | ((chr & (0x03 <<  8)) >>  8);
        res.buf[3] =         (chr &  0xFF              );
    }
    return res;
}

/* Implementation of 'LxEnc.encToMem' for UTF-16LE. */

static LxBChr*
LxEncUtf16Le_EncToMem(
    LxUChr  chr,
    LxBChr* buf)
{
    if (chr <= 0xFFFF) {
        buf[0] = (chr & 0x00FF)     ;
        buf[1] = (chr & 0xFF00) >> 8;
        buf += 2;
    }
    else {
        chr -= 0x10000;
        buf[0] =        ((chr & (0xFF << 10)) >> 10);
        buf[1] = 0xD8 | ((chr & (0x03 << 18)) >> 18);
        buf[2] =         (chr &  0xFF              );
        buf[3] = 0xDC | ((chr & (0x03 <<  8)) >>  8);
        buf += 4;
    }
    return buf;
}

/* Implementation of 'LxEnc.encToRes' for UTF-16LE. */

static LxEncRes
LxEncUtf16Le_EncToRes(
    LxUChr chr)
{
    LxEncRes res;

    if (chr <= 0xFFFF) {
        res.len = 2;
        res.buf[0] = (chr & 0x00FF)     ;
        res.buf[1] = (chr & 0xFF00) >> 8;
    }
    else {
        res.len = 4;
        chr -= 0x10000;
        res.buf[0] =        ((chr & (0xFF << 10)) >> 10);
        res.buf[1] = 0xD8 | ((chr & (0x03 << 18)) >> 18);
        res.buf[2] =         (chr &  0xFF              );
        res.buf[3] = 0xDC | ((chr & (0x03 <<  8)) >>  8);
    }
    return res;
}

/* Byte types in UTF-16: head of a high surrogate (D8..DB), of a low 
surrogate (DC..DF), or ordinary byte (all other). */

# define LxDecUtf16_BU 0 /* ordinary/universal */
# define LxDecUtf16_BH 1 /* head of high surrogate, D8..DB */
# define LxDecUtf16_BL 2 /* head of low surrogate, DC, DF */

/* To save space we use the 6 high bytes to detect the byte type; this way 
we only need 64 bytes instead of 256. */

static const uint8_t lxDecUtf16Byte[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 
};

# define LxDecUtf16_BYTE(b) (lxDecUtf16Byte[(b) >> 2])

/* Decoder states in UTF-16BE. */

# define LxDecUtf16Be_M0  0x00 /* expecting byte 1 or stop */
# define LxDecUtf16Be_M22 0x01 /* expecting byte 2 of 2 */
# define LxDecUtf16Be_M24 0x02 /* expecting byte 2 of 4 */
# define LxDecUtf16Be_M3  0x03 /* expecting byte 3 (of 4) */
# define LxDecUtf16Be_M4  0x04 /* expecting byte 4 (of 4) */
# define LxDecUtf16Be_M2L 0x05 /* expecting byte 2 of a low surrogate */

/* Decoding actions in UTF-16BE. */

#define LxEncUtf16Be_AN   0x00 /* no result */
#define LxEncUtf16Be_ACB  0x10 /* base code */
#define LxEncUtf16Be_ACE  0x20 /* supplementary code */
#define LxEncUtf16Be_AEH  0x30 /* lone high surrorgate + next byte */
#define LxEncUtf16Be_AEL  0x40 /* lone low surrogate */

/* Moves in UTF-16BE 

  (0)  -> U   = N  + (22)
  (0)  -> H   = N  + (24)
  (0)  -> L   = N  + (2L)
  (22) -> UHL = CB + (0)
  (24) -> UHL = N  + (3)
  (2L) -> UHL = EL + (0)
  (3)  -> U   = EH + (22)
  (3)  -> H   = EH + (24)
  (3)  -> L   = N  + (4)
  (4)  -> UHL = CE + (0) */

static const uint8_t lxDecUtf16BeMove[3][6] = {
# define a LxDecUtf16Be_M0
# define b LxDecUtf16Be_M22
# define c LxDecUtf16Be_M24
# define d LxDecUtf16Be_M3
# define e LxDecUtf16Be_M4
# define f LxDecUtf16Be_M2L
# define G LxEncUtf16Be_AN
# define H LxEncUtf16Be_ACB
# define I LxEncUtf16Be_ACE
# define J LxEncUtf16Be_AEH
# define K LxEncUtf16Be_AEL
/*        0    22   24   3    4    2L */
/* U */ { G|b, H|a, G|d, J|b, I|a, K|a },
/* H */ { G|c, H|a, G|d, J|c, I|a, K|a },
/* L */ { G|f, H|a, G|d, G|e, I|a, K|a },
# undef a
# undef b
# undef c
# undef d
# undef e
# undef f
# undef G
# undef H
# undef I
# undef J
# undef K
};

/* Get the move from mode (state) and byte (event). */

# define LxDecUtf16Be_MOVE(m, b) \
    (lxDecUtf16BeMove[lxDecUtf16Byte[(b) >> 2]][(m)])

/* Get the action and next mode out of a move. */

# define LxDecUtf16_ACT(m) ((m) & 0xF0)
# define LxDecUtf16_MOD(m) ((m) & 0x0F)

/* Implementation of 'LxDec.feed' for UTF-16BE, copying decoder. */

static LxDecRep
LxDecUtf16Be_FeedCp(
    LxDec *d,
    LxBChr c)
{
    LxDecStateUtf s; uint_fast8_t m; LxDecRep r;
    
    s = d->state.utf;
    m = LxDecUtf16Be_MOVE(s.mod, c);
    switch (LxDecUtf16_ACT(m)) {
      case LxEncUtf16Be_AN:
        r = (LxDecRep){0};
        ((LxBChr*)d->buf)[s.len] = c;
        d->state.utf = (LxDecStateUtf){s.len + 1, LxDecUtf16_MOD(m)};
      break;
      case LxEncUtf16Be_ACB:
        r = (LxDecRep){1, {{LxDecRes_CODE, 0, 2}}};
        ((LxBChr*)d->buf)[1] = c;
        d->state.utf = (LxDecStateUtf){0, 0};
      break;
      case LxEncUtf16Be_ACE:
        r = (LxDecRep){1, {{LxDecRes_CODE, 0, 4}}};
        ((LxBChr*)d->buf)[3] = c;
        d->state.utf = (LxDecStateUtf){0, 0};
      break;
      case LxEncUtf16Be_AEH:
        /* Got a high surrogate followed by a head of a base code or another
        high surrogate. Report the lone surrogate but keep the byte. To do 
        that relocate the surrogate into bytes [2-3]. */
        r = (LxDecRep){1,{{ LxDecRes_ESURH, 2, 2}}};
        ((LxBChr*)d->buf)[2] = d->buf[0];
        ((LxBChr*)d->buf)[3] = d->buf[1];
        ((LxBChr*)d->buf)[0] = c;
        d->state.utf = (LxDecStateUtf){1, LxDecUtf16_MOD(m)};
      break;
      case LxEncUtf16Be_AEL:
        r = (LxDecRep){1, {{LxDecRes_ESURL, 0, 2}}};
        ((LxBChr*)d->buf)[1] = c;
        d->state.utf = (LxDecStateUtf){0, 0};
      break;
    }
    return r;
}

/* Implementation of 'LxDec.feed' for UTF-16, non-copying decoder. */

static LxDecRep
LxDecUtf16Be_FeedNcp(
    LxDec* d,
    LxBChr c)
{
    LxDecStateUtf s; uint_fast8_t m; LxDecRep r;

    s = d->state.utf; m = LxDecUtf16Be_MOVE(s.mod, c);
    switch (LxDecUtf16_ACT(m)) {
      case LxEncUtf16Be_AN:
        r = (LxDecRep){0};
        d->state.utf = (LxDecStateUtf){s.len + 1, LxDecUtf16_MOD(m)};
      break;
      case LxEncUtf16Be_ACB:
        r = (LxDecRep){1, {{LxDecRes_CODE, -1, 2}}};
        d->state.utf = (LxDecStateUtf){0, 0};
      break;
      case LxEncUtf16Be_ACE:
        r = (LxDecRep){1, {{LxDecRes_CODE, -3, 4}}};
        d->state.utf = (LxDecStateUtf){0, 0};
      break;
      case LxEncUtf16Be_AEH:
        r = (LxDecRep){1, {{LxDecRes_ESURH, -2, 2}}};
        d->state.utf = (LxDecStateUtf){1, LxDecUtf16_MOD(m)};
      break;
      case LxEncUtf16Be_AEL:
        r = (LxDecRep){1, {{LxDecRes_ESURL, -1, 2}}};
        d->state.utf = (LxDecStateUtf){0, 0};
      break;
    }
    return r;
}

/* Implementation of 'LxDec.stop' for UTF-16, copying decoder. */

static LxDecRep
LxDecUtf16Be_StopCp(
    LxDec* d)
{
    LxDecRep r;

    if (d->state.utf.len == 0)
        r = (LxDecRep){0};
    else {
        switch (d->state.utf.len) {
          case 1:
            r = (LxDecRep){1, {{LxDecRes_EBRK , 0, 1}}};
          break;
          case 2:
            r = (LxDecRep){1, {{LxDecRes_ESURH, 0, 2}}};
          break;
          case 3:
            r = (LxDecRep){2, {{LxDecRes_ESURH, 0, 2},
                               {LxDecRes_EBRK , 2, 1}}}; 
          break;
        }
        d->state.utf = (LxDecStateUtf){0, 0};
    }
    return r;
}

/* Implementation of 'LxDec.stop' for UTF-16, non-copying decoder. */

static LxDecRep
LxDecUtf16Be_StopNcp(
    LxDec* d)
{
    LxDecRep r;

    if (d->state.utf.len == 0)
        r = (LxDecRep){0};
    else {
        switch (d->state.utf.len) {
          case 1:
            r = (LxDecRep){1, {{LxDecRes_EBRK ,  0, 1}}};
          break;
          case 2:
            r = (LxDecRep){1, {{LxDecRes_ESURH, -1, 2}}};
          break;
          case 3:
            r = (LxDecRep){2, {{LxDecRes_ESURH, -2, 2},
                               {LxDecRes_EBRK ,  0, 1}}}; 
          break;
        }
        d->state.utf = (LxDecStateUtf){0, 0};
    }
    return r;
}

/* Copying and non-copying decoder types, UTF-16BE. */

static const LxDecType
lxDecTypeUtf16BeCp = {
    LxDecType_CP         ,
    &lxEncUtf16Be        ,
    &LxDecUtf16Be_FeedCp ,
    &LxDecUtf16Be_StopCp },
lxDecTypeUtf16BeNcp = {
    LxDecType_NCP        , 
    &lxEncUtf16Be        ,
    &LxDecUtf16Be_FeedNcp,
    &LxDecUtf16Be_StopNcp};

/* Implementation of 'LxEnc.makeDec' for UTF-8. */

static void
LxEncUtf16Be_MakeDec(
    LxDec*       d,
    uint_fast8_t t) /* LxDecType_CP/NCP */
{
    if (t == LxDecType_CP)
        d->type = &lxDecTypeUtf16BeCp;
    else 
        d->type = &lxDecTypeUtf16BeNcp;
    d->state.utf = (LxDecStateUtf){0, 0};
}

/* Encoding object for UTF-16BE. */

const LxEnc lxEncUtf16Be = {
    "UTF-16BE",
    &LxEncUtf16Be_Scan    ,
    &LxEncUtf16Be_ScanRev ,
    &LxEncUtf16Be_Read    ,
    &LxEncUtf16Be_EncToMem,
    &LxEncUtf16Be_EncToRes,
    &LxEncUtf16Be_MakeDec ,
};

/* States in UTF-16LE. */

# define LxDecUtf16Le_M0  0x00 /* expecting byte 1 or stop */
# define LxDecUtf16Le_M2  0x01 /* expecting byte 2 */
# define LxDecUtf16Le_M3  0x02 /* expecting byte 3 */
# define LxDecUtf16Le_M4  0x03 /* expecting byte 4 */

/* Actions in UTF-16LE. */

#define LxEncUtf16Le_AN    0x00 /* no result */
#define LxEncUtf16Le_ACB   0x10 /* base code */
#define LxEncUtf16Le_ACS   0x20 /* supplementary code */
#define LxEncUtf16Le_AEHC  0x30 /* lone high surrogate and base code */
#define LxEncUtf16Le_AEH   0x40 /* lone high surrogate */
#define LxEncUtf16Le_AEL   0x50 /* lone low surrogate */

/* UTF-16LE moves:

  (0) + UHL = AN   (2)
  (2) + U   = ACB  (0)
  (2) + H   = AN   (3)
  (2) + L   = AEL  (0)
  (3) + UHL = AN   (4)
  (4) + U     AEHC (0)
  (4) + H     AEH  (3)
  (4) + L     ACS  (0) */

static const uint8_t lxDecUtf16LeMove[3][4] = {
# define a LxDecUtf16Le_M0
# define b LxDecUtf16Le_M2
# define c LxDecUtf16Le_M3
# define d LxDecUtf16Le_M4
# define E LxEncUtf16Le_AN
# define F LxEncUtf16Le_ACB
# define G LxEncUtf16Le_ACS
# define H LxEncUtf16Le_AEHC
# define I LxEncUtf16Le_AEH
# define J LxEncUtf16Le_AEL
/*        0    2    3    4   */
/* U */ { E|b, F|a, E|d, H|a },
/* H */ { E|b, E|c, E|d, I|c },
/* L */ { E|b, J|a, E|d, G|a },
# undef a
# undef b
# undef c
# undef d
# undef E
# undef F
# undef G
# undef H
# undef I
# undef J
};

/* Get the move from mode (state) and byte (event). */

# define LxDecUtf16Le_MOVE(m, b) \
    (lxDecUtf16LeMove[lxDecUtf16Byte[(b) >> 2]][(m)])

/* Implementation of 'LxDec.feed' for UTF-16LE, copying decoder. */

static LxDecRep
LxDecUtf16Le_FeedCp(
    LxDec *d,
    LxBChr c)
{
    LxDecStateUtf s; uint_fast8_t m; LxDecRep r;
    
    s = d->state.utf;
    m = LxDecUtf16Le_MOVE(s.mod, c);
    switch (LxDecUtf16_ACT(m)) {
      case LxEncUtf16Le_AN:
        r = (LxDecRep){0};
        ((LxBChr*)d->buf)[s.len] = c;
        d->state.utf = (LxDecStateUtf){s.len + 1, LxDecUtf16_MOD(m)};
      break;
      case LxEncUtf16Le_ACB:
        r = (LxDecRep){1, {{LxDecRes_CODE, 0, 2}}};
        ((LxBChr*)d->buf)[1] = c;
        d->state.utf = (LxDecStateUtf){0, LxDecUtf16Le_M0};
      break;
      case LxEncUtf16Le_ACS:
        r = (LxDecRep){1, {{LxDecRes_CODE, 0, 4}}};
        ((LxBChr*)d->buf)[3] = c;
        d->state.utf = (LxDecStateUtf){0, LxDecUtf16Le_M0};
      break;
      case LxEncUtf16Le_AEHC:
        /* Got a high surrogate followed by a base code. */
        r = (LxDecRep){2, {{LxDecRes_ESURH, 0, 2},
                           {LxDecRes_CODE , 2, 2}}};
        ((LxBChr*)d->buf)[3] = c;
        d->state.utf = (LxDecStateUtf){0, LxDecUtf16Le_M0};
      break;
      case LxEncUtf16Le_AEH:
        /* Got a high surrogate followed by another high surrogate. Report 
        the first as a lone surrogate, but keep the next. Rearrange the 
        buffer accordingly. */
        r = (LxDecRep){1, {{LxDecRes_ESURH, 2, 2}}};
        ((LxBChr*)d->buf)[3] = d->buf[1];
        ((LxBChr*)d->buf)[1] = c;
        c = d->buf[2];
        ((LxBChr*)d->buf)[2] = d->buf[0];
        ((LxBChr*)d->buf)[0] = c;
        d->state.utf = (LxDecStateUtf){2, LxDecUtf16Le_M3};
      break;
      case LxEncUtf16Le_AEL:
        r = (LxDecRep){1, {{LxDecRes_ESURL, 0, 2}}};
        ((LxBChr*)d->buf)[1] = c;
        d->state.utf = (LxDecStateUtf){0, LxDecUtf16Le_M0};
      break;
    }
    return r;
}

/* Implementation of 'LxDec.feed' for UTF-16LE, non-copying decoder. */

static LxDecRep
LxDecUtf16Le_FeedNcp(
    LxDec *d,
    LxBChr c)
{
    LxDecStateUtf s; uint_fast8_t m; LxDecRep r;

    s = d->state.utf;
    m = LxDecUtf16Le_MOVE(s.mod, c);
    switch (LxDecUtf16_ACT(m)) {
      case LxEncUtf16Le_AN:
        r = (LxDecRep){0};
        d->state.utf = (LxDecStateUtf){s.len + 1, LxDecUtf16_MOD(m)};
      break;
      case LxEncUtf16Le_ACB:
        r = (LxDecRep){1, {{LxDecRes_CODE, -1, 2}}};
        d->state.utf = (LxDecStateUtf){0, LxDecUtf16Le_M0};
      break;
      case LxEncUtf16Le_ACS:
        r = (LxDecRep){1, {{LxDecRes_CODE, -3, 4}}};
        d->state.utf = (LxDecStateUtf){0, LxDecUtf16Le_M0};
      break;
      case LxEncUtf16Le_AEHC:
        /* Got a high surrogate followed by a base code. */
        r = (LxDecRep){2, {{LxDecRes_ESURH, -3, 2},
                           {LxDecRes_CODE , -1, 2}}};
        d->state.utf = (LxDecStateUtf){0, LxDecUtf16Le_M0};
      break;
      case LxEncUtf16Le_AEH:
        /* Got a high surrogate followed by another high surrogate. Report 
        the first as a lone surrogate, but keep the next. */
        r = (LxDecRep){1, {{LxDecRes_ESURH, -3, 2}}};
        d->state.utf = (LxDecStateUtf){2, LxDecUtf16Le_M3};
      break;
      case LxEncUtf16Le_AEL:
        r = (LxDecRep){1, {{LxDecRes_ESURL, -1, 2}}};
        d->state.utf = (LxDecStateUtf){0, LxDecUtf16Le_M0};
      break;
    }
    return r;
}

/* Implementation of 'LxDec.stop' for UTF-16LE, copying decoder. */

static LxDecRep
LxDecUtf16Le_StopCp(
    LxDec* d)
{
    LxDecRep r;  

    if (d->state.utf.len == 0)
        r = (LxDecRep){ 0 };
    else {
        switch (d->state.utf.len) {
          case 1:
            r = (LxDecRep){1, {{LxDecRes_EBRK , 0, 1}}};
          break;
          case 2:
            r = (LxDecRep){1, {{LxDecRes_ESURH, 0, 2}}};
          break;
          case 3:
            r = (LxDecRep){2, {{LxDecRes_ESURH, 0, 2},
                               {LxDecRes_EBRK , 2, 1}}}; 
          break;
        }
        d->state.utf = (LxDecStateUtf){0, LxDecUtf16Le_M0};
    }
    return r;
}

/* Implementation of 'LxDec.stop' for UTF-16LE, non-copying decoder. */

static LxDecRep
LxDecUtf16Le_StopNcp(
    LxDec* d)
{
    LxDecRep r; uint_fast8_t len;

    if ((len = d->state.utf.len) == 0)
        r = (LxDecRep){0};
    else {
        switch (len) {
          case 1:
            r = (LxDecRep){1, {{LxDecRes_EBRK ,  0, 1}}};
          break;
          case 2:
            r = (LxDecRep){1, {{LxDecRes_ESURH, -1, 2}}};
          break;
          case 3:
            r = (LxDecRep){2, {{LxDecRes_ESURH, -2, 2},
                               {LxDecRes_EBRK ,  0, 1}}}; 
          break;
        }
        d->state.utf = (LxDecStateUtf){0, LxDecUtf16Le_M0};
    }
    return r;
}

/* Copying and non-copying decoder types, UTF-16LE. */

static const LxDecType
lxDecTypeUtf16LeCp = {
    LxDecType_CP         ,
    &lxEncUtf16Le        ,
    &LxDecUtf16Le_FeedCp ,
    &LxDecUtf16Le_StopCp },
lxDecTypeUtf16LeNcp = {
    LxDecType_NCP        , 
    &lxEncUtf16Le        ,
    &LxDecUtf16Le_FeedNcp,
    &LxDecUtf16Le_StopNcp};

/* Implementation of 'LxEnc.makeDec' for UTF-16LE. */

static void
LxEncUtf16Le_MakeDec(
    LxDec*       d,
    uint_fast8_t t) /* LxDecType_CP/NCP */
{
    if (t == LxDecType_CP)
        d->type = &lxDecTypeUtf16LeCp;
    else 
        d->type = &lxDecTypeUtf16LeNcp;
    d->state.utf = (LxDecStateUtf){0, LxDecUtf16Le_M0};
}

/* Encoding object for UTF-16LE. */

const LxEnc lxEncUtf16Le = {
    "UTF-16LE",
    &LxEncUtf16Le_Scan    ,
    &LxEncUtf16Le_ScanRev ,
    &LxEncUtf16Le_Read    ,
    &LxEncUtf16Le_EncToMem,
    &LxEncUtf16Le_EncToRes,
    &LxEncUtf16Le_MakeDec ,
};


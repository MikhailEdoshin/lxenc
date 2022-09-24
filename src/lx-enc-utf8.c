/* lxenc-utf8.h. GNU AGPL.
Copyright (C) 2022 Mikhail Edoshin. Thy will be done. */

# include <stddef.h>
# include "lx.h"

/* Table of names
                       b y t e   t y p e s
   LxDecUtf8_BTA       tail, subtype A (80..8F)
   LxDecUtf8_BTB       tail, subtype B (90..9F)
   LxDecUtf8_BTC       tail, subtype C (A0..AF)
   LxDecUtf8_BTD       tail, subtype D (B0..BF)
   LxDecUtf8_BH1       head of 1-byte code (00..7F)
   LxDecUtf8_BH2       head of 2-byte code (C2..DF)
   LxDecUtf8_BH3       head of 3 byte code (E1..EC, EE..EF)
   LxDecUtf8_BH3A      head of 3 byte code, subtype A (E0)
   LxDecUtf8_BH3B      head of 3-byte code, subtype B (ED)
   LxDecUtf8_BH4       head of 4-byte code (F1..F3)
   LxDecUtf8_BH4A      head of 4-byte code, subtype A (F0)
   LxDecUtf8_BH4B      head of 4-byte code, subtype B (F4)
   LxDecUtf8_BINV      invalid (C0..C1, F5..FF)
                       m o d e s
   LxDecUtf8_M0        expecting a head byte or 'stop'
   LxDecUtf8_M22       expecting byte 2 of 2
   LxDecUtf8_M23       expecting byte 2 of 3, any
   LxDecUtf8_M23A      expecting byte 2 of 3, raise ECTX on AB
   LxDecUtf8_M23B      expecting byte 2 of 3, detect surrogates
   LxDecUtf8_M24       expecting byte 2 of 4, any
   LxDecUtf8_M24A      expecting byte 2 of 4, raise ECTC on A
   LxDecUtf8_M24B      expecting byte 2 of 4, raise ECTX on BCD
   LxDecUtf8_M33       expecting byte 3 of 3
   LxDecUtf8_M33H      expecting byte 3 of 3, high surrogate
   LxDecUtf8_M33L      expecting byte 3 of 3, low surrogate
   LxDecUtf8_M34       expecting byte 3 of 4
   LxDecUtf8_M44       expecting byte 4 of 4
                       a c t i o n s
   LxDecUtf8_ANEXT     wait for the next byte
   LxDecUtf8_ACBAS     code, base plane, non-surrogate
   LxDecUtf8_ACHSU     code, high surrogate
   LxDecUtf8_ACLSU     code, low surrogate
   LxDecUtf8_ACSUP     code, supplementary plane
   LxDecUtf8_AEINV     error, invalid byte 
   LxDecUtf8_AECTX     error, contextually invalid byte
   LxDecUtf8_AECNT     error, unexpected tail byte
   LxDecUtf8_AEBRC     error, interrupted by one-byte code
   LxDecUtf8_AEBRH     error, interrupted by a head of a multi-byte code

   lxDecUtf8BTyp       UTF-8 byte type lookup table
   lxDecUtf8Move       UTF-8 decoder moves lookup table

   LxDecUtf8_ACT       extract action from move
   LxDecUtf8_MOD       extract mode from move or state
   LxDecUtf8_LEN       extract length from state
   LxDecUtf8_SUR       extract surrogate flag from state
   LxDecUtf8_STATE     construct state
   LxDecUtf8_SURON     surrogate on
   LxDecUtf8_SUROFF    surrogate on

                       i m p l e m e n t a t i o n s
   LxEncUtf8_Scan      'scan' for UTF-8
   LxEncUtf8_ScanRev   'scanRev' for UTF-8
   LxEncUtf8_Read      'read' for UTF-8, CESU-8, WTF-8
   LxEncUtf8_EncToMem  'encToMem', UTF-8
   LxEncUtf8_EncToRes  'encToRes', UTF-8
   LxDecUtf8_FeedCp    'feed', UTF-8, copying decoder
   LxDecUtf8_FeedNcp   'feed', UTF-8, non-copying decoder 
   LxDecUtf8_StopCp    'stop', UTF-8, copying decoder
   LxDecUtf8_StopNcp   'stop', UTF-8, non-copying decoder
   LxDecUtf8_MakeDec   'makeDec' for UTF-8 */

/* Implementaction of 'LxEnc.scan' for UTF-8 and WTF-8. */

static uint_fast8_t
LxEncUtf8_Scan(
    const void* mem )
{
    LxBChr b; uint_fast8_t r;

    b = *(const LxBChr*)mem;
    if (b < 0xE0) {
        if (b < 0xC0)
            r = 1;
        else
            r = 2;
    }
    else {
        if (b < 0xF0)
            r = 3;
        else
            r = 4;
    }
    return r;
}

/* Implementation of 'LxEnc.scanRev` for UTF-8 and WTF-8. */

static uint_fast8_t
LxEncUtf8_ScanRev(
    const void* mem)
{
    const LxBChr *p; LxBChr b; uint_fast8_t r;

    p = mem;
    r = 1;
a:  b = *p; 
    if (b < 0x80 || b > 0xBF)
        goto b;
    --p; ++r;
    goto a;
b:  return r;
};

/* Implementation of 'LxEnc.read' for UTF-8, CESU-8, and WTF-8. */

static uint_fast32_t
LxEncUtf8_Read(
    const void*        mem  ,
    uint_fast8_t size ) /* code size, 1..4, 6 */
{
    uint_fast32_t r; const LxBChr* b;

    b = mem;
    switch (size) {
        case 1:
            r  = b[0];
            break;
        case 2:
            r  = (b[0] & 0x1F) << 6;
            r |= (b[1] & 0x3F);
            break;
        case 3:
            r  = (b[0] & 0x0F) << 12;
            r |= (b[1] & 0x3F) << 6;
            r |= (b[2] & 0x3F);
            break;
        case 4:
            r  = (b[0] & 0x0F) << 18;
            r |= (b[1] & 0x3F) << 12;
            r |= (b[2] & 0x3F) << 6;
            r |= (b[3] & 0x3F);
            break;
        case 6: /* CESU-8 surrogate pair */
            r  = (b[1] & 0x0F) << 16;
            r |= (b[2] & 0x3F) << 10;
            r |= (b[4] & 0x0F) <<  6;
            r |= (b[5] & 0x3F)      ;
            r += 0x10000;
            break;
    }
    return r;
}

/* Implementation of 'LxEnc.encToMem' for UTF-8. */

static LxBChr*
LxEncUtf8_EncToMem(
    LxUChr  chr,
    LxBChr* buf)
{
    if (chr <= 0x7FF) {
        if (chr <= 0x7F) {
            buf[0] = chr;
            buf += 1;
        }            
        else {
            buf[0] = 0xC0 | ((chr & (0x1F <<  6)) >> 6);
            buf[1] = 0x80 |  (chr &  0x3F             );
            buf += 2;
        }
    }
    else if (chr <= 0xFFFF) {
        buf[0] = 0xE0 | ((chr & (0x0F << 12)) >> 12);
        buf[1] = 0x80 | ((chr & (0x3F <<  6)) >>  6);
        buf[2] = 0x80 |  (chr &  0x3F              );
        buf += 3;
    }
    else {
        buf[0] = 0xF0 | ((chr & (0x0F << 18)) >> 18);
        buf[1] = 0x80 | ((chr & (0x3F << 12)) >> 12);
        buf[2] = 0x80 | ((chr & (0x3F <<  6)) >>  6);
        buf[3] = 0x80 |  (chr &  0x3F              );
        buf += 4;
    }
    return buf;
}

/* Implementation of 'LxEnc.encToRes' for UTF-8. */

static LxEncRes
LxEncUtf8_EncToRes(
    LxUChr chr)
{
    LxEncRes res;

    if (chr <= 0x7FF) {
        if (chr <= 0x7F) {
            res.len = 1;
            res.buf[0] = chr;
        }
        else {
            res.len = 2;
            res.buf[0] = 0xC0 | ((chr & (0x1F <<  6)) >> 6);
            res.buf[1] = 0x80 |  (chr &  0x3F             );
        }
    }
    else if (chr <= 0xFFFF) {
        res.len = 3;
        res.buf[0] = 0xE0 | ((chr & (0x0F << 12)) >> 12);
        res.buf[1] = 0x80 | ((chr & (0x3F <<  6)) >>  6);
        res.buf[2] = 0x80 |  (chr &  0x3F              );
    }
    else {
        res.len = 4;
        res.buf[0] = 0xF0 | ((chr & (0x0F << 18)) >> 18);
        res.buf[1] = 0x80 | ((chr & (0x3F << 12)) >> 12);
        res.buf[2] = 0x80 | ((chr & (0x3F <<  6)) >>  6);
        res.buf[3] = 0x80 |  (chr &  0x3F              );
    }
    return res;
}

/* UTF-8 byte types: 0 (single-byte character), 1, 2, 3, 4 (continuation byte), 5 (start of a two-byte sequence), 6, 7, 8, 9 (start of a three-byte sequence), 10, 11, 12 (start of a four-byte sequence), 13 (invalid byte). */

# define LxDecUtf8_BTA  0x00 /* tail byte, type A */
# define LxDecUtf8_BTB  0x01 /* tail byte, type B */
# define LxDecUtf8_BTC  0x02 /* tail byte, type C */
# define LxDecUtf8_BTD  0x03 /* tail byte, type D */
# define LxDecUtf8_BH1  0x04 /* head byte 1 of 1 */
# define LxDecUtf8_BH2  0x05 /* head byte 1 of 2 */
# define LxDecUtf8_BH3  0x06 /* head byte 1 of 3 */
# define LxDecUtf8_BH3A 0x07 /* head byte 1 of 3, type A */
# define LxDecUtf8_BH3B 0x08 /* head byte 1 of 3, type B */
# define LxDecUtf8_BH4  0x09 /* head byte 1 of 4 */
# define LxDecUtf8_BH4A 0x0A /* head byte 1 of 3, type A */
# define LxDecUtf8_BH4B 0x0B /* head byte 1 of 3, type B */
# define LxDecUtf8_INV  0x0C /* invalid byte */

# define a LxDecUtf8_BH1 
# define b LxDecUtf8_BTA 
# define c LxDecUtf8_BTB 
# define d LxDecUtf8_BTC 
# define e LxDecUtf8_BTD 
# define f LxDecUtf8_BH2
# define g LxDecUtf8_BH3A
# define h LxDecUtf8_BH3
# define i LxDecUtf8_BH3B
# define j LxDecUtf8_BH4A
# define k LxDecUtf8_BH4
# define l LxDecUtf8_BH4B
# define m LxDecUtf8_INV
static const uint8_t lxUtf8ByteType[] = {
/*       x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF */
/* 0x */ a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a,
/* 1x */ a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a,
/* 2x */ a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a,
/* 3x */ a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a,
/* 4x */ a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a,
/* 5x */ a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a,
/* 6x */ a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a,
/* 7x */ a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a,
/* 8x */ b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b,
/* 9x */ c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c,
/* Ax */ d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
/* Bx */ e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e,
/* Cx */ m, m, f, f, f, f, f, f, f, f, f, f, f, f, f, f,
/* Dx */ f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f,
/* Ex */ g, h, h, h, h, h, h, h, h, h, h, h, h, i, h, h,
/* Fx */ j, k, k, k, l, m, m, m, m, m, m, m, m, m, m, m,
};
# undef a
# undef b
# undef c
# undef d
# undef e
# undef f
# undef g
# undef h
# undef i
# undef j
# undef k
# undef l
# undef m

/* The UTF-8 decoder can be in the following states: head byte or EOF (0), byte 2 of 2 (1), byte 2 of 3 (2, 3, 4), byte 2 of 4 (5, 6, 7), byte 3 of 3 (8, 9, 10), byte 3 of 4 (11), byte 4 of 4 (12). */

# define LxDecUtf8_M0   0x00 /* expect head byte or EOF */
# define LxDecUtf8_M22  0x01 /* expect byte 2 of 2 */
# define LxDecUtf8_M23  0x02 /* expect byte 2 of 3, any */
# define LxDecUtf8_M23A 0x03 /* expect byte 2 of 3, c or d */
# define LxDecUtf8_M23B 0x04 /* expect byte 2 of 3, any, but detect surrogates */
# define LxDecUtf8_M24  0x05 /* expect byte 2 of 4, any */
# define LxDecUtf8_M24A 0x06 /* expect byte 2 of 4, c or d */
# define LxDecUtf8_M24B 0x07 /* expect byte 2 of 4, only a */
# define LxDecUtf8_M33  0x08 /* expect byte 3 of 3, end as base */
# define LxDecUtf8_M33H 0x09 /* expect byte 3 of 3, end as hi-sur. */
# define LxDecUtf8_M33L 0x0A /* expect byte 3 of 3, end as lo-sur */
# define LxDecUtf8_M34  0x0B /* expect byte 3 of 4 */
# define LxDecUtf8_M44  0x0C /* expect byte 4 of 4 */

/* When processing a byte an UTF-8 decoder may get the following status: expect more bytes (0), got a code (1, 2, 3, 4: base, high surrogate, low surrogate, extended), got an error (5, 6, 7: invalid byte, incorrect tail, unexpected tail), unexpected lead (8, 9: with code, without code). */

# define LxDecUtf8_ANEXT  0x00 /* wait */
# define LxDecUtf8_ACBAS  0x10 /* code, basic */
# define LxDecUtf8_ACHSU  0x20 /* code, high surrogate */
# define LxDecUtf8_ACLSU  0x30 /* code, low surrogate */
# define LxDecUtf8_ACSUP  0x40 /* code, extended */
# define LxDecUtf8_AEINV  0x50 /* error, invalid byte */
# define LxDecUtf8_AECTX  0x60 /* error, contextually incorrect byte */
# define LxDecUtf8_AECNT  0x70 /* error, unexpected tail */
# define LxDecUtf8_AEBRC  0x80 /* error, unexpected head with full code */
# define LxDecUtf8_AEBRH  0x90 /* error, unexpected head without full code  */

/* A table of moves is a state machine that maps a combination of byte type 
(see 'lxUtf8BTyp') and mode ('LxDecUtf8_M0..M44') to the action ('LxDecUtf8_ANEXT..AEBRH') and next mode. The next mode occupies the low 
four bits, the action the high four bits. */

/* states */
# define a LxDecUtf8_M0
# define b LxDecUtf8_M22
# define c LxDecUtf8_M23
# define d LxDecUtf8_M23A
# define e LxDecUtf8_M23B
# define f LxDecUtf8_M24
# define g LxDecUtf8_M24A
# define h LxDecUtf8_M24B
# define i LxDecUtf8_M33
# define j LxDecUtf8_M33H
# define k LxDecUtf8_M33L
# define l LxDecUtf8_M34
# define m LxDecUtf8_M44

/* statuses */
# define N LxDecUtf8_ANEXT
# define O LxDecUtf8_ACBAS
# define P LxDecUtf8_ACHSU
# define Q LxDecUtf8_ACLSU
# define R LxDecUtf8_ACSUP
# define S LxDecUtf8_AEINV
# define T LxDecUtf8_AECTX
# define U LxDecUtf8_AECNT
# define V LxDecUtf8_AEBRC
# define W LxDecUtf8_AEBRH

static uint8_t lxUtf8Move[13][13] = {
/*          0    22   23   23a  23b  24   24a  24b  33   33h  33l  34   44 */
/* ta  */ { U|a, O|a, N|i, T|a, N|i, N|l, T|a, N|l, O|a, P|a, Q|a, N|m, R|a },
/* tb  */ { U|a, O|a, N|i, T|a, N|i, N|l, N|l, T|a, O|a, P|a, Q|a, N|m, R|a },
/* tc  */ { U|a, O|a, N|i, N|i, N|j, N|l, N|l, T|a, O|a, P|a, Q|a, N|m, R|a },
/* td  */ { U|a, O|a, N|i, N|i, N|k, N|l, N|l, T|a, O|a, P|a, Q|a, N|m, R|a },
/* h1  */ { O|a, V|a, V|a, V|a, V|a, V|a, V|a, V|a, V|a, V|a, V|a, V|a, V|a },
/* h2  */ { N|b, W|b, W|b, W|b, W|b, W|b, W|b, W|b, W|b, W|b, W|b, W|b, W|b },
/* h3  */ { N|c, W|c, W|c, W|c, W|c, W|c, W|c, W|c, W|c, W|c, W|c, W|c, W|c },
/* h3a */ { N|d, W|d, W|d, W|d, W|d, W|d, W|d, W|d, W|d, W|d, W|d, W|d, W|d },
/* h3b */ { N|e, W|e, W|e, W|e, W|e, W|e, W|e, W|e, W|e, W|e, W|e, W|e, W|e },
/* h4  */ { N|f, W|f, W|f, W|f, W|f, W|f, W|f, W|f, W|f, W|f, W|f, W|f, W|f },
/* h4a */ { N|g, W|g, W|g, W|g, W|g, W|g, W|g, W|g, W|g, W|g, W|g, W|g, W|g },
/* h4b */ { N|h, W|h, W|h, W|h, W|h, W|h, W|h, W|h, W|h, W|h, W|h, W|h, W|h },
/* inv */ { S|a, S|a, S|a, S|a, S|a, S|a, S|a, S|a, S|a, S|a, S|a, S|a, S|a },
};
# undef a
# undef b
# undef c
# undef d
# undef e
# undef f
# undef g
# undef h
# undef i
# undef j
# undef k
# undef l
# undef m
# undef N
# undef O
# undef P
# undef Q
# undef R
# undef S
# undef T
# undef U
# undef V
# undef W

/* Macro to get the move by state and byte. */

# define LxDecUtf8_MOVE(s, b) lxUtf8Move[lxUtf8ByteType[(b)]][(s)]

/* The UTF-8 decoder state stores a surrogate flag, the length, and the 
current mode (decoding phase):

   7 6 5 4 3 2 1 0  bits
  +-+-+-+-+-+-+-+-+
  |S|  L  |   M   | surrogate flag, buffer length, current mode
  +-+-+-+-+-+-+-+-+

A move in the table of moves stores action and the next mode:

   7 6 5 4 3 2 1 0  bits
  +-+-+-+-+-+-+-+-+
  |   A   |   M   | action, next mode
  +-+-+-+-+-+-+-+-+

We need to read these fields. The 'act' and  'sur' fields are not used as 
numbers, only as distinct values, but 'mod' and 'len' are. */

# define LxDecUtf8_ACT(v)  ((v) & 0xF0)       /* move */
# define LxDecUtf8_MOD(v)  ((v) & 0x0F)       /* move, state */
# define LxDecUtf8_LEN(v) (((v) & 0x70) >> 4) /* state */
# define LxDecUtf8_SUR(v)  ((v) & 0x80)       /* state */

/* We also need to construct a new state from a surrogate flag, a length, and 
a new mode. To simplify this we have constants for the surrogate flag. */

# define LxDecUtf8_SurON  0x80
# define LxDecUtf8_SurOFF 0x00

/* The state construction macro is that. */

# define LxDecUtf8_STATE(s, l, m) ((s) | ((l) << 4) | (m))

/* UTF-8 does not use the surrogate flag and it should be way more frequent than CESU-8 and WTF-8, so we provide a separate setter for it. */

# define LxDecUtf8_MkStateUtf8(l, m) (((l) << 4) | (m))

/* Design choice: There are three encodings (UTF-8, CESU-8, and WTF-8), each needs two functions ('feed' and 'stop'), and each function comes in two variants (copying and non-copying). This gives 4 functions for each encoding, 12 functions total. The structure of these functions is similar and they indeed have a lot of common, but it's hard to express this in C in a way that would be both efficient and understandable. So instead of extracting the common logic each function is written as a monolith that handles each move type separately. This makes each case easy to verify.

Common invariant: in every state the 'len' field equals the number of bytes that haven't yet been reported and, if we use an internal buffer, points to the cell for the next byte. On 'feed' we have three patterns: we keep what we received and do not report (Wait), report what we have and then keep what we received (EHeadWait), or add what we received and report all we now have (all other cases). On 'stop' we always report all we have. */

/* Implementation of 'LxDecType.feed', non-copying decoder, UTF-8. */ 

static LxDecRep
LxDecUtf8_FeedCp(
    LxDec* d,
    LxBChr c)
{
    LxDecStateUtf s; uint_fast8_t m; LxDecRep r; 

    s = d->state.utf; m = LxDecUtf8_MOVE(s.mod, c);
    switch (LxDecUtf8_ACT(m)) {
      case LxDecUtf8_ANEXT:
        r = (LxDecRep){0};
        ((LxBChr*)d->buf)[s.len] = c;
        d->state.utf = (LxDecStateUtf){s.len + 1, LxDecUtf8_MOD(m)};
      break;
      case LxDecUtf8_ACBAS:
      case LxDecUtf8_ACSUP:
        r = (LxDecRep){1, {{LxDecRes_CODE, 0, s.len + 1 }}};
        ((LxBChr*)d->buf)[s.len] = c;
        d->state.utf = (LxDecStateUtf){0, 0};
      break;
      case LxDecUtf8_ACHSU: /* surrogates, length 3, invalid in UTF-8 */
      case LxDecUtf8_ACLSU:
        r = (LxDecRep){ 1, { { LxDecRes_ECODE, 0, 3 } } };
        ((LxBChr*)d->buf)[2] = c;
        d->state.utf = (LxDecStateUtf){0, 0};
      break;
      case LxDecUtf8_AEBRC:
        r = (LxDecRep){2, {{LxDecRes_EBRK, 0    , s.len},
                           {LxDecRes_CODE, s.len, 1    }}};
        ((LxBChr*)d->buf)[s.len] = c;
        d->state.utf = (LxDecStateUtf){0, 0};
      break;
      case LxDecUtf8_AEBRH: {
        /* An incomplete sequence followed by the first byte of another 
        multi-byte sequence. Move the incomplete sequence down one byte and 
        report it. Keep the new byte. */
        uint_fast8_t i;
        
        r = (LxDecRep){1, {{LxDecRes_EBRK, 1, s.len}}};
        for (i = s.len; i > 0; --i)
            ((LxBChr*)d->buf)[i] = d->buf[i - 1];
        ((LxBChr*)d->buf)[0] = c;
        d->state.utf = (LxDecStateUtf){1, LxDecUtf8_MOD(m)};
      }
      break;
      case LxDecUtf8_AEINV:
        if (s.len == 0)
            r = (LxDecRep){1, {{LxDecRes_EINV, 0    , 1    }}};
        else
            r = (LxDecRep){2, {{LxDecRes_EBRK, 0    , s.len},
                               {LxDecRes_EINV, s.len, 1    }}};
        ((LxBChr*)d->buf)[s.len] = c;
        d->state.utf = (LxDecStateUtf){0, 0};
      break;
      case LxDecUtf8_AECTX: /* length 2 */
        r = (LxDecRep){1, {{LxDecRes_ECTX, 0, 2}}};
        ((LxBChr*)d->buf)[1] = c;
        d->state.utf = (LxDecStateUtf){0, 0};
      break;
      case LxDecUtf8_AECNT: /* length 1 */
        r = (LxDecRep){1, {{LxDecRes_ECNT, 0, 1}}};
        ((LxBChr*)d->buf)[0] = c;
        d->state.utf = (LxDecStateUtf){0, 0};
      break;
    }
    return r;
}

/* Implementation of 'LxDecType.feed' for a non-copying decoder, UTF-8. */ 

static LxDecRep
LxDecUtf8_FeedNcp(
    LxDec* d,
    LxBChr c)
{
    LxDecStateUtf s; uint_fast8_t m; LxDecRep r;

    s = d->state.utf; m = LxDecUtf8_MOVE(s.mod, c);
    switch (LxDecUtf8_ACT(m)) {
      case LxDecUtf8_ANEXT:
        r = (LxDecRep){0};
        d->state.utf = (LxDecStateUtf){s.len + 1, LxDecUtf8_MOD(m)};
      break;
      case LxDecUtf8_ACBAS:
      case LxDecUtf8_ACSUP:
        r = (LxDecRep){1, {{LxDecRes_CODE, -s.len, s.len + 1}}};
        d->state.utf = (LxDecStateUtf){0, 0};
      break;
      case LxDecUtf8_ACHSU:
      case LxDecUtf8_ACLSU:
        r = (LxDecRep){1, {{LxDecRes_ECODE, -2, 3 }}};
        d->state.utf = (LxDecStateUtf){0, 0};
      break;
      case LxDecUtf8_AEBRC:
        r = (LxDecRep){2, {{LxDecRes_EBRK, -s.len, s.len},
                           {LxDecRes_CODE, 0     , 1    }}};
        d->state.utf = (LxDecStateUtf){0, 0};
      break;
      case LxDecUtf8_AEBRH:
        r = (LxDecRep){1, {{LxDecRes_EBRK, -s.len, s.len}}};
        d->state.utf = (LxDecStateUtf){1, LxDecUtf8_MOD(m)};
      break;
      case LxDecUtf8_AEINV:
        if (s.len == 0)
            r = (LxDecRep){1, {{LxDecRes_EINV, 0     , 1    }}};
        else
            r = (LxDecRep){2, {{LxDecRes_EBRK, -s.len, s.len},
                               {LxDecRes_EINV, 0     , 1    }}};
        d->state.utf = (LxDecStateUtf){0, 0};
      break;
      case LxDecUtf8_AECTX: /* length 2 */
        r = (LxDecRep){1, {{LxDecRes_ECTX, -1, 2}}};
        d->state.utf = (LxDecStateUtf){0, 0};
      break;
      case LxDecUtf8_AECNT: /* length 1 */
        r = (LxDecRep){1, {{LxDecRes_ECNT, 0, 1}}};
        d->state.utf = (LxDecStateUtf){0, 0};
      break;
    }
    return r;
}

/* Implementation of 'LxDecType.stop' for a copying decoder, UTF-8. */

static LxDecRep
LxDecUtf8_StopCp(
    LxDec* d)
{
    LxDecStateUtf s; LxDecRep r;

    s = d->state.utf;
    if (s.len == 0)
        r = (LxDecRep){0}; /* no need to reset the state */
    else {
        r = (LxDecRep){1, {{LxDecRes_EBRK, 0, s.len}}};
        d->state.utf = (LxDecStateUtf){0, 0};
    }
    return r;
}

/* Implementation of 'LxDecType.stop' for a non-copying decoder, UTF-8. */

static LxDecRep
LxDecUtf8_StopNcp(
    LxDec* d)
{
    LxDecStateUtf s; LxDecRep r;

    s = d->state.utf;
    if (s.len == 0)
        r = (LxDecRep){0};
    else {
        r = (LxDecRep){1, {{LxDecRes_EBRK, -(s.len - 1), s.len}}};
        d->state.utf = (LxDecStateUtf){0, 0};
    }
    return r;
}

/* Copying and non-copying decoder types, UTF-8. */

static const LxDecType
lxDecTypeUtf8Cp = {
    LxDecType_CP      ,
    &lxEncUtf8        ,
    &LxDecUtf8_FeedCp ,
    &LxDecUtf8_StopCp },
lxDecTypeUtf8Ncp = {
    LxDecType_NCP     , 
    &lxEncUtf8        ,
    &LxDecUtf8_FeedNcp,
    &LxDecUtf8_StopNcp};

/* Implementation of 'LxEnc.makeDec' for UTF-8. */

static void
LxEncUtf8_MakeDec(
    LxDec*       d,
    uint_fast8_t t) /* LxDecType_CP/NCP */
{
    if (t == LxDecType_CP)
        d->type = &lxDecTypeUtf8Cp;
    else 
        d->type = &lxDecTypeUtf8Ncp;
    d->state.utf = (LxDecStateUtf){0, 0};
}

/* The 'LxEnc' variable for the UTF-8 encoding. */

const LxEnc lxEncUtf8 = {
    "UTF-8"            ,
    &LxEncUtf8_Scan    ,
    &LxEncUtf8_ScanRev ,
    &LxEncUtf8_Read    ,
    &LxEncUtf8_EncToMem,
    &LxEncUtf8_EncToRes,
    &LxEncUtf8_MakeDec ,
};


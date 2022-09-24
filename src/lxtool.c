/* lx-test.c */

/* GNU AGPL. Copyright (C) Mikhail Edoshin. Thy will be done. */

# include <stdarg.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include "lx.h"

/* Table of names:

LxTest_SUCCESS         exit status, completed all tests, none failed
LxTest_FAILURE         exit status, completed all tests, some failed
LxTest_ERROR           exit status, internal error 

LxTestRes              number of failures in a test
LxTestRES              format string to print 'FCount'

LxTest_OutStr          output a string
LxTest_OutFmt          output a string with formatted values
LxTest_OutFmtV           "  "
LxTest_OutBuf          output byte values
LxTest_OutDecRes       output an 'LxDecRes' value

LxTestEncPhase         testing phase
LxTestEncPhase_CODE      code
LxTestEncPhase_EBRK      interrupt (end of input)
LxTestEncPhase_EBRKI     interrupt (invalid byte)
LxTestEncPhase_EBRKC     interrupt (one-byte code)
LxTestEncPhase_EBRKH     interrupt (head of a multi-byte code)
LxTestEncPhase_EINV      invalid byte
LxTestEncPhase_ECTX      contextually invalid byte
LxTestEncPhase_ECMT      unexpected tail (continuation) byte 
LxTestEncPhase_ECODE     invalid code
LxTestEncPhaseStr      description of the testing phase

LxTestEncOp            operations being tested
LxTestEncOp_INT          internal (for safe strings)
LxTestEncOp_DEC          decoding
LxTestEncOpStr         description of the operation

LxTestEncStep          decoding test dstep
LxTestEncStep_INTR       'feed' of an intermediate byte
LxTestEncStep_LAST       'feed' of the last byte
LxTestEncStep_STOP       final 'stop'
LxTestEncStepStr       description of the decoding test dstep

LxTestEncUtf8EInv      an array of invalid bytes

LxTestEncECtxRec       type to describe context dependencies
LxTestEncECtxRecs      context dependency records

LxTestEnc              encoding test data
LxTestEnc_Utf8Len1     encode a 1-byte code
LxTestEnc_Utf8Len2     encode a 2-byte code
LxTestEnc_Utf8Len3Buf  encode a 3-byte code (a helper)
LxTestEnc_Utf8Len3     encode a 3-byte code
LxTestEnc_Utf8Len4     encode a 4-byte code
LxTestEnc_Utf8Len6     encode a 6-byte code (CESU-8/WTF-8)
LxTestEnc_Out          output test information
LxTestEnc_Err          output an error message and test information
LxTestEnc_CmpRes       compare 'LxDecRes' values
LxTestEnc_DecOp        check decoding with a single decoder
LxTestEnc_DecOps       check decoding with both decoder types
LxTestEnc_IntOps       check internal operations (for safe strings)
LxTestEnc_EncOps       check encoding
LxTestEnc_Utf8One      check a one-byte UTF-8 code
LxTestEnc_Utf8Mul      check a multi-byte UTF-8 code

LxTest_EncUtf8         test UTF-8 encoding
LxTest_Enc             test all encodings */

/* The internal name of the tool for messages. */

const char* lxToolName = "lxtool";

/* A testing session may complete all requested tests, successfully or not, 
or it may fail to do that due to an unrelated reason (e.g. it may fail to 
write to 'stdout'). We make a distinction between these two cases. */

# define LxTest_SUCCESS 0 /* run all tests, none failed */
# define LxTest_FAILURE 1 /* run all tests, some failed */
# define LxTest_ERROR   2 /* internal error */

/* During testing we count testing errors (failurs). Each test counts 
failures internally and reports the overall number of it encountered. To 
decide whether to continue with the next test or it is meaningless after a 
failure we use the standard control flow constructs. */

typedef uint_fast64_t LxTestRes;
# define LxTestRES PRIuFAST64

/* All output is done via a few output routines that check for errors and if
they detect one, exit with 'LxTest_ERROR'. */

/* Print a string to a stream.' */

void
LxTest_OutStr(
    FILE*     file,
  const char* str)
{
    int r;

    /* Use 'fputs' to avoid a newline. */
    if ((r = fputs(str, file)) < 0 && ferror(file) != 0 )
        exit(LxTest_ERROR);
}

/* Print a formatted string to a stream. */

void
LxTest_OutFmtV(
    FILE*     file,
  const char* fmt ,
    va_list   args)
{
    int r;

    if ((r = vfprintf(file, fmt, args)) < 0 && ferror(file) != 0)
        exit(LxTest_ERROR);
}

void
LxTest_OutFmt(
    FILE*     file,
  const char* fmt ,
              ... )
{
    va_list args;
    
    va_start(args, fmt);
    LxTest_OutFmtV(file, fmt, args);
    va_end(args);
}

/* Print bytes in a buffer to a stream in uppercase hexadecimal with spaces:
'AB CD'. For small buffers (up to 7 bytes currently, unlikely to grow). */

static void
LxTest_OutBuf(
    FILE*         file,
  const LxBChr*   buf ,
    uint_fast32_t len )
{
    if (len > 0) {
        uint_fast32_t i;

        for (i = 0; i < len - 1; ++i) {
            LxTest_OutFmt(file, "%02" PRIX8, buf[i]);
            LxTest_OutStr(file, " ");
        }
        LxTest_OutFmt(file, "%02" PRIX8, buf[i]);
    }
}

/* Encodings */

/* In decoding tests we test two decoders, non-copying (0), and copying (1). 
We also test internal operations that do not use a decoder. To tell these 
cases apart we use constants for the decoder number. NCP and CP are used as 
indexes into 'LxTestEnc.dec'. */

typedef enum {
    LxTestDecNo_NCP  = 0, /* non-copying decoder */
    LxTestDecNo_CP   = 1, /* copying decoder */
    LxTestDecNo_NONE = 2, /* no decoder (internal operations) */
} LxTestDecNo;

/* To test decoding we give the decoder a sequence and a list of what it must 
read out of the sequence. The tester feeds all bytes of a sequence to a 
decoder and then sends 'stop'. If any of these actions returns a result, it 
compares it with the next expected item. */

typedef struct LxTestEnc     LxTestEnc;
typedef struct LxTestEncRes  LxTestEncRes;

struct LxTestEnc {
  const LxEnc*      enc   ; /* encoding */
    LxBChr          buf[7]; /* buffer with the tested sample */
    uint8_t         bufLen; /* buffer length */
    LxUChr          chr   ; /* Unicode code, if applicable, or LxUChrERR. */
    char*           phase ; /* current phase name, US ASCII. */
    LxDec           dec[2]; /* decoders, non-copying (0), copying (1) */
    LxTestDecNo     decNo ; /* current decoder */
    uint8_t         bufPos; /* decoding position, 0..bufLen (see [POS]) */
    struct LxTestEncRes {
        uint8_t     type  ; /* result type, 'LxDecRes_?' */
        uint8_t     len   ; /* result length */
    }               res[3]; /* expected results */
    uint8_t         resCnt; /* number of expected results */
    uint8_t         resNo ; /* next expected result */
    uint8_t         resLen; /* number of bytes covered by results so far */
};

/* [POS] The 'bufPos' field in the buffer may range from 0 to 'bufLen' 
inclusively. If it is less than 'bufLen', then it indicates the byte used for 
'feed'. If it equals 'bufLen', then we are testing 'stop' after the last 
byte. */

/* In some tests the buffer contains a valid Unicode code, which we store and
output. In other tests the buffer contains an invalid sequence and thus there 
is no code. We need a special value for 'chr' to indicate such cases. */

# define LxUChrERR UINT32_MAX 

/* When a test fails, we output the test state to 'stdout'. */

static void
LxTestEnc_Out(
    LxTestEnc* t)
{
    LxTest_OutFmt(stdout, " (%s, %s", t->enc->name, t->phase);
    if (t->chr != LxUChrERR)
        LxTest_OutFmt(stdout, ", U-%" PRIX32, t->chr);
    LxTest_OutStr(stdout, ", [");
    LxTest_OutBuf(stdout, t->buf, t->bufLen);
    LxTest_OutStr(stdout, "]");
    if (t->decNo != LxTestDecNo_NONE) {
        LxTest_OutStr(stdout, ", ");
        if (t->decNo == 0)
            LxTest_OutStr(stdout, "non-");
        LxTest_OutFmt(stdout, "copying, byte %" PRIu8, t->bufPos);
        if (t->resCnt > 0)
            LxTest_OutFmt(stdout, ", expected %" PRIu8 " result(s), next %"
                    PRIu8 ": (%s %" PRIu8 ")", t->resCnt, t->resNo, 
                    lxDecResStr[t->res[t->resNo].type], 
                    t->res[t->resNo].len);
    }
    LxTest_OutStr(stdout, ".)\n");
}

/* If a message is complex (e.g. involves printing memory buffers), we 
construct it manually, and then print the test information. In most tests, 
however, a message is a single formatted string. In this case we can wrap it
all into a single function that prints the message and the test. */

static void
LxTestEnc_Err(
    LxTestEnc* t,
  const char*  fmt,
               ...)
{
    va_list args;
    
    va_start(args, fmt);
    LxTest_OutFmtV(stdout, fmt, args);
    va_end(args);
    LxTestEnc_Out(t);
}

/* To test decoding we loop over decoders. To each decoder we feed all bytes
in the test buffer and then send 'stop'. If a step produces one or more 
results we compare them with expected results: type, length, data (or 
offset). In the end we check if we received all expected results.

The procedure always sends 'stop' (even if buffer length is 0) and that 
should reset the decoder even if there was an error (if 'stop' does not reset 
the decoder, then it's a bug to eradicate anyway). Another way would be to 
make a new decoder for each test but it is more expensive. */

static LxTestRes
LxTestEnc_DecOps(
    LxTestEnc* t)
{
    LxTestRes r;

    r = 0;
    for (t->decNo = 0 /* NCP */; t->decNo <= 1 /* CP */; ++(t->decNo)) {
        LxDec* dec;
        
        dec = t->dec + t->decNo; t->resNo = 0; t->resLen = 0;
        for (t->bufPos = 0; t->bufPos <= t->bufLen; ++t->bufPos) {
            LxDecRep decRep;

            if (t->bufPos < t->bufLen)
                decRep = dec->type->feed(dec, t->buf[t->bufPos]);
            else
                decRep = dec->type->stop(dec);
            if (decRep.cnt) {
                uint_fast8_t rno;
                
                for (rno = 0; rno < decRep.cnt; ++rno) {
                    LxDecRes ar;
                    
                    ar = decRep.res[rno];
                    if (t->resNo == t->resCnt) {
                        LxTestEnc_Err(t, "Extra result: type %s, offset %u, "
                                "length %u.", lxDecResStr[ar.type], ar.off,
                                ar.len);
                        ++r;
                    }
                    else {
                        LxTestEncRes er;

                        er = t->res[t->resNo];
                        if (er.type != ar.type) {
                            LxTestEnc_Err(t, "Item type mismatch: expected "
                                    "%s, got %s.", lxDecResStr[er.type],
                                    lxDecResStr[ar.type]);
                            ++r;
                        }
                        if (er.len != ar.len) {
                            LxTestEnc_Err(t, "Item length mismatch: expected "
                                    "%u, got %u.", er.len, ar.len);
                            ++r;
                        }
                        else if (t->decNo == 0 /* NCP */) {
                            uint_fast8_t base;
                            
                            if (t->bufPos != t->bufLen)
                                base = t->bufPos;
                            else
                                base = t->bufPos - 1;
                            if (t->resLen != base + ar.off) {
                                LxTestEnc_Err(t, "Result offset mismatch: "
                                        "start %d" PRIu8 ", current %"
                                        PRIuFAST8 ", result offset %d.", 
                                        t->resLen, base, ar.off);
                                ++r;
                            }
                        }
                        else if (ar.off + ar.len > 7) {
                            LxTestEnc_Err(t, "Unsafe offset/length values: "
                                    "offset %u, length %u.", ar.off, ar.len);
                            ++r;
                        }
                        else {
                            const void *em, *am;
                            
                            em = t->buf + t->resLen;
                            am = dec->buf + ar.off;
                            if (memcmp(em, am, ar.len) != 0) {
                                LxTest_OutStr(stdout, "Data mismatch: "
                                        "expected ");
                                LxTest_OutBuf(stdout, em, er.len);
                                LxTest_OutStr(stdout, ", got ");
                                LxTest_OutBuf(stdout, am, ar.len);
                                LxTest_OutStr(stdout, ".");
                                LxTestEnc_Out(t);
                                ++r;
                            }
                        }
                        ++t->resNo; t->resLen += er.len;
                    }
                }
            }
        }
        if (t->resNo < t->resCnt) {
            LxTestEnc_Err(t, "Unused result.");
            ++r;
        }
    }
    return r;
}

/* To start testing an encoding we set up a test record. */

static void
LxTestEnc_Init(
    LxTestEnc* t,
  const LxEnc* e)
{
    t->enc = e;
    e->makeDec(&(t->dec[0]), LxDecType_NCP);
    e->makeDec(&(t->dec[1]), LxDecType_CP);
}

/* Test internal operations for safe strings. */

static LxTestRes
LxTestEnc_IntOps(
    LxTestEnc* t)
{
    uint_fast8_t len; LxUChr chr; LxBChr buf[sizeof t->buf], *next;
    LxEncRes res; LxTestRes r;

    r = 0;
    t->decNo = LxTestDecNo_NONE;
    /* scan */
    len = t->enc->scan(t->buf);
    if (len != t->bufLen) {
        LxTestEnc_Err(t, "Scan mismatch: expected %" PRIuFAST8 ", got %"
                PRIuFAST8 ".", t->bufLen, len);
        ++r;
    }
    /* scanRev */
    len = t->enc->scanRev(t->buf + (t->bufLen - 1));
    if (len != t->bufLen) {
        LxTestEnc_Err(t, "ScanRev mismatch: expected %" PRIuFAST8 ", got %"
                PRIuFAST8 ".", t->bufLen, len);
        ++r;
    }
    /* read */
    chr = t->enc->read(t->buf, t->bufLen);
    if (chr != t->chr) {
        LxTestEnc_Err(t, "Read mismatch: expected %" LxUCHR ", got %" LxUCHR
                ".", t->chr, chr);
        ++r;
    }
    /* encToMem */
    next = t->enc->encToMem(t->chr, buf);
    if (next - buf != t->bufLen) {
        LxTestEnc_Err(t, "EncToMem size mismatch; expected %" PRIu8 ", "
                "got %zu.", t->bufLen, next - buf);
        ++r;
    }
    else if (memcmp(buf, t->buf, t->bufLen) != 0) {
        LxTest_OutStr(stdout, "EncToMem result mismatch, expected ");
        LxTest_OutBuf(stdout, t->buf, t->bufLen);
        LxTest_OutStr(stdout, ", got ");
        LxTest_OutBuf(stdout, buf, t->bufLen);
        LxTest_OutStr(stdout, ".");
        LxTestEnc_Out(t);
        ++r;
    }
    /* encToRes */
    res = t->enc->encToRes(t->chr);
    if (res.len != t->bufLen) {
        LxTestEnc_Err(t, "EncToRes size mismatch; expected %" PRIu8 ", "
                "got %" PRIu8 ".", t->bufLen, res.len);
        ++r;
    }
    else if (memcmp(res.buf, t->buf, t->bufLen) != 0) {
        LxTest_OutStr(stdout, "EncToRes result mismatch, expected ");
        LxTest_OutBuf(stdout, t->buf, t->bufLen);
        LxTest_OutStr(stdout, ", got ");
        LxTest_OutBuf(stdout, res.buf, t->bufLen);
        LxTest_OutStr(stdout, ".");
        LxTestEnc_Out(t);
        ++r;
    }
    return r;
}

/* UTF-8 */

/* Encode a code of known encoding length 1 as UTF-8. */

static void
LxTestEnc_Utf8Len1(
    LxBChr* buf ,
    LxUChr  chr)
{
    buf[0] = chr;
}

/* Encode a code of known encoding length 2 as UTF-8. */

static void
LxTestEnc_Utf8Len2(
    LxBChr* buf ,
    LxUChr  chr)
{
    buf[0] = 0xC0 | ((chr & (0x1F << 6)) >> 6);
    buf[1] = 0x80 |  (chr &  0x3F);
}

/* Encode a code of known encoding length 3 as UTF-8 */

static void
LxTestEnc_Utf8Len3(
    LxBChr* buf ,
    LxUChr  chr)
{
    buf[0] = 0xE0 | ((chr & (0x0F << 12)) >> 12);
    buf[1] = 0x80 | ((chr & (0x3F <<  6)) >>  6);
    buf[2] = 0x80 |  (chr &  0x3F);
}

/* Encode a code of known encoding length 4 as UTF-8. */

static void
LxTestEnc_Utf8Len4(
    LxBChr* buf ,
    LxUChr  chr)
{
    buf[0] = 0xF0 | ((chr & (0x0F << 18)) >> 18);
    buf[1] = 0x80 | ((chr & (0x3F << 12)) >> 12);
    buf[2] = 0x80 | ((chr & (0x3F <<  6)) >>  6);
    buf[3] = 0x80 |  (chr &  0x3F);
}

/* When testing UTF-8 we need to test incomplete sequences: multi-byte codes 
interrupted by the end of input, an invalid byte, or a head byte of another 
sequence. The design choice is to write specific code for each interrupt 
cause and then use a subroutine that will try this cause with all possible 
incomplete sequences. The subroutine receives a partially prepared test and 
then generate all possible valid but incomplete sequences and test each. */

static LxTestRes
LxTestEnc_Utf8Brk(
    LxTestEnc* t)
{
    /* Definitions for all possible incomplete valid codes. */
    static const struct Def {
        uint8_t  len     ; /* sequence length */
        struct {
          LxBChr start   , /* start byte */
                 end     ; /* end byte */
        }        range[3]; /* byte ranges to generate */
    } defs[] = {
        { 1, { { 0xC2, 0xDF } } },
        { 2, { { 0xE0, 0xE0 }, { 0xA0, 0xBF } } },
        { 2, { { 0xE1, 0xEC }, { 0x80, 0xBF } } },
        { 2, { { 0xED, 0xED }, { 0x80, 0x9F } } },
        { 2, { { 0xEE, 0xEF }, { 0x80, 0xBF } } },
        { 3, { { 0xF0, 0xF0 }, { 0x90, 0xBF }, { 0x80, 0xBF } } },
        { 3, { { 0xF1, 0xF3 }, { 0x80, 0xBF }, { 0x80, 0xBF } } },
        { 3, { { 0xF4, 0xF4 }, { 0x80, 0x8F }, { 0x80, 0xBF } } },
    };
    LxTestRes r; uint8_t origLen, len; /* current length */

    r = 0;
    origLen = t->bufLen;
    for (len = 1; len <= 3; ++len) {
        uint_fast8_t defNo;

        t->bufLen = origLen + len; t->res[0].len = len;
        for (defNo = 0; defNo < Lx_ARRAYSZ(defs); ++defNo) {
            struct Def def;
            
            def = defs[defNo];
            if (def.len >= len) {
                uint_fast8_t chrNo, lastChrNo; /* the byte to alter */ 

                lastChrNo = len - 1;
                /* Write initial values for each byte. */
                for (chrNo = 0; chrNo <= lastChrNo; ++chrNo)
                    t->buf[chrNo] = def.range[chrNo].start;
                /* Test this sequence */
            a:  r += LxTestEnc_DecOps(t);
                chrNo = lastChrNo;
            b:  if (t->buf[chrNo] < def.range[chrNo].end) {
                    /* Test next value for this byte */
                    t->buf[chrNo] += 1;
                    goto a;
                }
                else if (chrNo > 0) {
                    /* This byte has reached the end of the range; reset it 
                    to the start and try to increment the value for the 
                    previous byte */
                    t->buf[chrNo] = def.range[chrNo].start;
                    --chrNo;
                    goto b;
                }
                /* all bytes reached their end; go to next record */
            }
        }
    }
    t->bufLen = origLen;
    return r;
}


/* Test the UTF-8 encoding. */

static LxTestRes
LxTest_EncUtf8(void)
{
    /* Invalid bytes. */
    static const LxBChr invChrs[] = {
        0xC0, 0xC1,
        0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF
    };
    /* Restricted continuation byte ranges. */
    const static struct CtxDef{
        LxBChr byte , /* byte that restricts the next byte */
               start, /* start byte of disallowed range */
               end  ; /* end byte of disallowed range */        
    } ctxDefs[] = {
        { 0xE0, 0x80, 0x9F },
        { 0xF0, 0x80, 0x8F },
        { 0xF4, 0xB0, 0xBF },
    };
    
    LxTestRes r; LxTestEnc test; LxUChr chr; uint_fast8_t invChrNo, ctxDefNo;

    LxTestEnc_Init(&test, &lxEncUtf8);
    r = 0;
    /* Valid codes */
    test.phase = "CODE";
    test.resCnt = 1;
    test.res[0].type = LxDecRes_CODE;
    test.bufLen = test.res[0].len = 1;
    for (chr = 0x0; chr <= 0x7F; ++chr) {
        test.chr = chr;
        LxTestEnc_Utf8Len1(test.buf, chr);
        r += LxTestEnc_IntOps(&test);
        r += LxTestEnc_DecOps(&test);
    }
    test.bufLen = test.res[0].len = 2;
    for (chr = 0x80; chr <= 0x7FF; ++chr) {
        test.chr = chr;
        LxTestEnc_Utf8Len2(test.buf, chr);
        r += LxTestEnc_IntOps(&test);
        r += LxTestEnc_DecOps(&test);
    }
    test.bufLen = test.res[0].len = 3;
    for (chr = 0x800; chr <= 0xD7FF; ++chr) {
        test.chr = chr;
        LxTestEnc_Utf8Len3(test.buf, chr);
        r += LxTestEnc_IntOps(&test);
        r += LxTestEnc_DecOps(&test);
    }
    for (chr = 0xE000; chr <= 0xFFFF; ++chr) {
        test.chr = chr;
        LxTestEnc_Utf8Len3(test.buf, chr);
        r += LxTestEnc_IntOps(&test);
        r += LxTestEnc_DecOps(&test);
    }
    test.bufLen = test.res[0].len = 4;
    for (chr = 0x10000; chr <= 0x10FFFF; ++chr) {
        test.chr = chr;
        LxTestEnc_Utf8Len4(test.buf, chr);
        r += LxTestEnc_IntOps(&test);
        r += LxTestEnc_DecOps(&test);
    }

    /* Non-code sequences. */
    test.chr = LxUChrERR;

    /* Interrupted by end of input. */
    test.phase = "BRK";
    test.resCnt = 1;
    test.bufLen = 0;
    test.res[0].type = LxDecRes_EBRK;
    r += LxTestEnc_Utf8Brk(&test);

    /* Interrupted by an invalid byte. */
    test.phase = "BRK+INV";
    test.resCnt = 2;
    test.bufLen = 1;
    test.res[0].type = LxDecRes_EBRK;
    test.res[1].type = LxDecRes_EINV;
    test.res[1].len = 1;
    for (invChrNo = 0; invChrNo < Lx_ARRAYSZ(invChrs); ++invChrNo) {
        test.buf[1] = test.buf[2] = test.buf[3] = invChrs[invChrNo];
        r += LxTestEnc_Utf8Brk(&test);
    }

    /* Interrupted by a single-byte code. */
    test.phase = "BRK+CODE";
    test.resCnt = 2;
    test.bufLen = 1;
    test.res[0].type = LxDecRes_EBRK;
    test.res[1].type = LxDecRes_CODE;
    test.res[1].len = 1;
    for (chr = 0x00; chr <= 0x7F; ++chr) {
        test.buf[1] = test.buf[2] = test.buf[3] = chr;
        r += LxTestEnc_Utf8Brk(&test);
    }

    /* Interrupted by a head of a multi-byte code. */
    test.phase = "BRK+BRK";
    test.resCnt = 2;
    test.bufLen = 1;
    test.res[0].type = LxDecRes_EBRK;
    test.res[1].type = LxDecRes_EBRK;
    test.res[1].len = 1;
    for (chr = 0xC2; chr <= 0xF4; ++chr) {
        test.buf[1] = test.buf[2] = test.buf[3] = chr;
        r += LxTestEnc_Utf8Brk(&test);
    }

    /* Invalid byte when expecting a head byte. */
    test.phase = "INV";
    test.resCnt = 1;
    test.bufLen = 1;
    test.res[0] = (LxTestEncRes){ LxDecRes_EINV, 1 };
    for (invChrNo = 0; invChrNo < Lx_ARRAYSZ(invChrs); ++invChrNo) {
        test.buf[0] = invChrs[invChrNo];
        r += LxTestEnc_DecOps(&test);
    }
    /* Unexpected continuation byte (0x80..BF) when expecting a head byte. */
    test.phase = "CNT";
    test.resCnt = 1;
    test.bufLen = 1;
    test.res[0] = (LxTestEncRes){ LxDecRes_ECNT, 1 };
    for (chr = 0x80; chr <= 0xBF; ++chr) {
        test.buf[0] = chr;
        r += LxTestEnc_DecOps(&test);
    }
    /* Contextually invalid byte. */
    test.phase = "CTX";
    test.resCnt = 1;
    test.bufLen = 2;
    test.res[0] = (LxTestEncRes){ LxDecRes_ECTX, 2 };
    for (ctxDefNo = 0; ctxDefNo < Lx_ARRAYSZ(ctxDefs); ++ctxDefNo) {
        struct CtxDef ctxDef; uint_fast8_t ctxChr;
        
        ctxDef = ctxDefs[ctxDefNo];
        test.buf[0] = ctxDef.byte;
        for (ctxChr = ctxDef.start; ctxChr <= ctxDef.end; ++ctxChr) {
            test.buf[1] = ctxChr;
            r += LxTestEnc_DecOps(&test);
        }
    }
    /* Surrogates. */
    test.phase = "SUR";
    test.resCnt = 1;
    test.bufLen = 3;
    test.res[0] = (LxTestEncRes){ LxDecRes_ECODE, 3 };
    for (chr = 0xD800; chr <= 0xDFFF; ++chr) {
        LxTestEnc_Utf8Len3(test.buf, chr);
        r += LxTestEnc_DecOps(&test); 
    }
    return r;
}

/* UTF-16 */

/* Encode a base code into UTF-16BE. */
static void
LxTestEnc_Utf16Be2(
    LxBChr* buf,
    LxUChr  chr)
{
    buf[0] = (chr & 0xFF00) >> 8;
    buf[1] = (chr & 0x00FF);
}

/* Encode a base code into UTF-16LE. */
static void
LxTestEnc_Utf16Le2(
    LxBChr* buf,
    LxUChr  chr)
{
    buf[0] = (chr & 0x00FF);
    buf[1] = (chr & 0xFF00) >> 8;
}

/* Encode a supplementary code into UTF-16 parametrized by a base coder. */
static void
LxTestEnc_Utf164(
    LxBChr* buf,
    LxUChr  chr,
    void  (*enc2)(LxBChr*, LxUChr))
{
    chr -= 0x10000;
    enc2(buf    , 0xD800 | ((chr & (0x3FF << 10)) >> 10));
    enc2(buf + 2, 0xDC00 |  (chr &  0x3FF)              );
}

/* Encode a supplementary code into UTF-16BE. */
static void
LxTestEnc_Utf16Be4(
    LxBChr* buf,
    LxUChr  chr)
{
    LxTestEnc_Utf164(buf, chr, &LxTestEnc_Utf16Be2);
}

/* Encode a supplementary code into UTF-16LE. */
void
LxTestEnc_Utf16Le4(
    LxBChr* buf,
    LxUChr  chr)
{
    LxTestEnc_Utf164(buf, chr, &LxTestEnc_Utf16Le2);
}

/* Test an UTF-16 encoding, BE or LE; parametrized by encoding and coding
functions. */
static LxTestRes
LxTest_EncUtf16(
  const LxEnc* enc                   ,
    void     (*enc2)(LxBChr*, LxUChr),
    void     (*enc4)(LxBChr*, LxUChr))
{
    LxTestEnc test; LxUChr chr; LxTestRes r;

    LxTestEnc_Init(&test, enc);
    r = 0;
    test.phase = "CODE";
    /* Valid base codes. */
    test.bufLen = 2;
    test.resCnt = 1;
    test.res[0] = (LxTestEncRes){ LxDecRes_CODE, 2 };
    for (chr = 0x0; chr < 0xD800; ++chr) {
        test.chr = chr;
        enc2(test.buf, chr);
        r += LxTestEnc_IntOps(&test);
        r += LxTestEnc_DecOps(&test);
    }
    for (chr = 0xE000; chr <= 0xFFFF; ++chr) {
        test.chr = chr;
        enc2(test.buf, chr);
        r += LxTestEnc_IntOps(&test);
        r += LxTestEnc_DecOps(&test);
    }
    /* Valid extended codes. */
    test.bufLen = 4;
    test.res[0] = (LxTestEncRes){ LxDecRes_CODE, 4 };
    for (chr = 0x10000; chr <= 0x10FFFF; ++chr) {
        test.chr = chr;
        enc4(test.buf, chr);
        r += LxTestEnc_IntOps(&test);
        r += LxTestEnc_DecOps(&test);
    }
    /* All the rest are not valid codes. */
    test.chr = LxUChrERR;
    /* Interrupted base codes. */
    test.phase = "BRK";
    test.bufLen = 1;
    test.resCnt = 1;
    test.res[0] = (LxTestEncRes){ LxDecRes_EBRK, 1 };
    for (chr = 0x00; chr <= 0xFF; ++chr) {
        test.buf[0] = chr;
        r += LxTestEnc_DecOps(&test);
    }
    /* Lone high surrogate. */
    test.phase = "SURH";
    test.bufLen = 2;
    test.resCnt = 1;
    test.res[0] = (LxTestEncRes){ LxDecRes_ESURH, 2 };
    for (chr = 0xD800; chr <= 0xDBFF; ++chr) {
        enc2(test.buf, chr);
        r += LxTestEnc_DecOps(&test);
    }
    /* Lone low surrogate. */
    test.phase = "SURL";
    test.bufLen = 2;
    test.resCnt = 1;
    test.res[0] = (LxTestEncRes){ LxDecRes_ESURL, 2 };
    for (chr = 0xDC00; chr <= 0xDFFF; ++chr) {
        enc2(test.buf, chr);
        r += LxTestEnc_DecOps(&test);
    }
    /* Extended code interrupted after 3rd byte. */
    test.phase = "SURH+BRK";
    test.bufLen = 3;
    test.resCnt = 2;
    test.res[0] = (LxTestEncRes){ LxDecRes_ESURH, 2 };
    test.res[1] = (LxTestEncRes){ LxDecRes_EBRK , 1 };
    for (chr = 0xD800; chr <= 0xDBFF; ++chr) {
        LxUChr eChr;

        enc2(test.buf, chr);
        for (eChr = 0x00; eChr <= 0xFF; ++eChr) {
            test.buf[2] = eChr;
            r += LxTestEnc_DecOps(&test);
        }
    }
    /* High surrogate followed by another high surrogate. */
    test.phase = "SURH+SURH";
    test.bufLen = 4;
    test.resCnt = 2;
    test.res[0] = (LxTestEncRes){ LxDecRes_ESURH, 2 };
    test.res[1] = (LxTestEncRes){ LxDecRes_ESURH, 2 };
    for (chr = 0xD800; chr <= 0xDBFF; ++chr) {
        LxUChr eChr;

        enc2(test.buf, chr);
        for (eChr = 0xD800; eChr <= 0xDBFF; ++eChr) {
            enc2(test.buf + 2, eChr);
            r += LxTestEnc_DecOps(&test);
        }
    }
    /* High surrogate followed by a base code. */
    test.phase = "SURH+BASE";
    test.bufLen = 4;
    test.resCnt = 2;
    test.res[0] = (LxTestEncRes){ LxDecRes_ESURH, 2 };
    test.res[1] = (LxTestEncRes){ LxDecRes_CODE , 2 };
    for (chr = 0xD800; chr <= 0xDBFF; ++chr) {
        LxUChr eChr;

        enc2(test.buf, chr);
        for (eChr = 0x0000; eChr <= 0xD7FF; ++eChr) {
            enc2(test.buf + 2, eChr);
            r += LxTestEnc_DecOps(&test);
        }
        for (eChr = 0xE000; eChr <= 0xFFFF; ++eChr) {
            enc2(test.buf + 2, eChr);
            r += LxTestEnc_DecOps(&test);
        }
    }
    return r;
}

/* Test UTF-16BE. */
static LxTestRes
LxTest_EncUtf16Be(void)
{
    return LxTest_EncUtf16(&lxEncUtf16Be, &LxTestEnc_Utf16Be2,
            &LxTestEnc_Utf16Be4);
}

/* Test UTF-16LE. */
static LxTestRes
LxTest_EncUtf16Le(void)
{
    return LxTest_EncUtf16(&lxEncUtf16Le, &LxTestEnc_Utf16Le2,
            &LxTestEnc_Utf16Le4);
}

/* Encodings and functions to test each. */

static const struct LxTestEncDef {
  const char*  name;
  const LxEnc* enc ;
    LxTestRes (* const func)(void);
} lxTestEncDefs[] = {
    { "utf8"   , &lxEncUtf8   , &LxTest_EncUtf8    },
    { "utf16be", &lxEncUtf16Be, &LxTest_EncUtf16Be },
    { "utf16le", &lxEncUtf16Le, &LxTest_EncUtf16Le },
};

/* Find an encoding in 'lxTestEncDefs'. If not found, print an error message
to 'stderr' and return 'NULL'. */

const struct LxTestEncDef*
LxTestEncDef_Find(
  const char* name)
{
    size_t i; const struct LxTestEncDef* def;
    
    for (i = 0; i < Lx_ARRAYSZ(lxTestEncDefs); ++i) {
        def = lxTestEncDefs + i;
        if (strcmp(def->name, name) == 0) /* found */
            break;
    }
    if (i == Lx_ARRAYSZ(lxTestEncDefs)) {
        LxTest_OutFmt(stderr, "%s: failed to find encoding '%s'.\n",
                lxToolName, name);
        def = NULL;
    }
    return def;
}

/* (Command) Print names of all encodings to 'stdout'. */

static LxTestRes
LxTestCmd_EncList(int /* unused */, char** /* unused */)
{
    size_t i;

    LxTest_OutStr(stdout,"Encoding names: ");
    for (i = 0; i < Lx_ARRAYSZ(lxTestEncDefs); ++i) {
        LxTest_OutStr(stdout, lxTestEncDefs[i].name);
        if (i < Lx_ARRAYSZ(lxTestEncDefs) - 1)
            LxTest_OutStr(stdout, ", ");
    }
    LxTest_OutStr(stdout, ".\n");
    return 0;
}

/* Print a single Unicode code in the specified encoding to 'stdout'. */

static void
LxTestCmd_EncPrintOne(
  const LxEnc* enc,
    LxUChr     chr)
{
    LxEncRes res; uint_fast8_t i;

    res = enc->encToRes(chr);
    for (i = 0; i < res.len; ++i)
        if (putc(res.buf[i], stdout) != EOF)
            continue;
        else 
            exit(LxTest_ERROR);
}

/* (Command) Print all Unicode codes in the specified encoding to 'stdout'. */

static LxTestRes
LxTestCmd_EncPrint(int argc, char* argv[])
{
    LxTestRes r; const struct LxTestEncDef* def;
    
    r = 0;
    if ((def = LxTestEncDef_Find(argv[0])) == NULL)
        ++r;
    else {
        /* TODO: later decide how to handle non-Unicode encodings. */
        LxUChr chr;

        for (chr = 0x0; chr <= 0xD7FF; ++chr) {
            LxTestCmd_EncPrintOne(def->enc, chr);
        }
        for (chr = 0xE000; chr <= 0x10FFFF; ++chr)
            LxTestCmd_EncPrintOne(def->enc, chr);
    }
    return r;
}

/* (Command) Read 'stdin' in one encoding and print to 'stdout' in another. 
On error print an error message to 'stderr' with error type, position, and 
bytes. */

static LxTestRes
LxTestCmd_EncConv(
    int        argc  , /* checked to be 2 */
    char*      argv[]) /* names of inEnc, outEnc */
{
    LxTestRes r; const struct LxTestEncDef *inEncDef, *outEncDef;

    r = 0;
    if (!(inEncDef  = LxTestEncDef_Find(argv[0]))
     || !(outEncDef = LxTestEncDef_Find(argv[1])))
        ++r;
    else {
        const LxEnc *inEnc, *outEnc; LxDec dec;
        LxDecRep decRep; int chr; uint64_t off; uint_fast8_t rno;
        
        inEnc = inEncDef->enc; outEnc = outEncDef->enc; off = 0;
        inEnc->makeDec(&dec, LxDecType_CP);
a:      chr = fgetc(stdin);
        if (chr != EOF)
            decRep = dec.type->feed(&dec, chr);
        else if (feof(stdin))
            decRep = dec.type->stop(&dec);
        else 
            exit(LxTest_ERROR);
        for (rno = 0; rno < decRep.cnt; ++rno) {
            LxDecRes res;
            
            res = decRep.res[rno];
            if (res.type == LxDecRes_CODE) {
                LxUChr uchr; LxEncRes eres; uint_fast8_t bno;
                
                uchr = inEnc->read(dec.buf + res.off, res.len);
                eres = outEnc->encToRes(uchr);
                for (bno = 0; bno < eres.len; ++bno)
                    if (putc(eres.buf[bno], stdout) != EOF)
                        continue;
                    else
                        exit(LxTest_ERROR);
            }
            else {
                LxTest_OutFmt(stderr, ":%s: decoding error '%s' at %" PRIu64
                        ", bytes ", lxToolName, lxDecResStr[res.type], off);
                LxTest_OutBuf(stderr, dec.buf + res.off, res.len);
                LxTest_OutStr(stderr, ".\n");
                ++r;
            }
        }
        if (chr != EOF) {
            ++off;
            goto a;
        }
    }
    return r;
}

/* Test all encodings or a selected encoding. */

static LxTestRes
LxTestCmd_TestEnc(
    int   argc  ,
    char* argv[])
{
    LxTestRes r; const struct LxTestEncDef* def;
    
    r = 0;
    if (argc == 0) {
        size_t i;
        
        for (i = 0; i < Lx_ARRAYSZ(lxTestEncDefs); ++i) {
            def = lxTestEncDefs + 1;
            r += def->func();
        }
    }
    else if ((def = LxTestEncDef_Find(argv[0])))
        r += def->func();
    else
        ++r;
    return r;
}

/* Test definitions. */

const static struct LxTestDef {
  const char   *name,
               *args;
    LxTestRes (*func)(int, char**);
  const char   *desc;
} lxTestDefs[] = {
    { "enc", "[<enc-name>]", &LxTestCmd_TestEnc,
      "Test all encodings or a selected one." },
};

/* (Command) Print test descriptions to 'stdout'. */

static LxTestRes
LxTestCmd_TestList(int /* unused */, char** /* unused */)
{
    size_t i;

    LxTest_OutStr(stdout,
        "Usage:\n\n"
        "  lxtool test [<test-name>]\n\n"
        "where '<test-name>' is one of the following:\n\n");
    for (i = 0; i < Lx_ARRAYSZ(lxTestDefs); ++i) {
        const struct LxTestDef* def;
        
        def = lxTestDefs + i;
        LxTest_OutFmt(stdout, "  %s", def->name);
        if (def->args)
            LxTest_OutFmt(stdout, " %s", def->args);
        LxTest_OutFmt(stdout, "\n    %s\n", def->desc);
    }
    LxTest_OutStr(stdout, "\nIf no test name is given, run all tests.\n");
    return 0;
}

/* Find a test definition. If not found, return 'NULL'. */

static const struct LxTestDef*
LxTestDef_Find(
  const char* name)
{
    size_t i; const struct LxTestDef* def;
    
    for (i = 0; i < Lx_ARRAYSZ(lxTestDefs); ++i) {
        def = lxTestDefs + i;
        if (strcmp(def->name, name) == 0)
            break;
    }
    if (i == Lx_ARRAYSZ(lxTestDefs)) {
        LxTest_OutFmt(stderr, "%s: failed to find test '%s'.\n", lxToolName,
                name);
        def = NULL;
    }
    return def;
}

/* (Command) Run all tests or a selected test. */

static LxTestRes
LxTestCmd_Test(
    int   argc  ,
    char* argv[])
{
    LxTestRes r;

    r = 0;
    if (argc == 0) {
        size_t i;

        for (i = 0; i < Lx_ARRAYSZ(lxTestDefs); ++i)
            r += lxTestDefs[i].func(argc, argv);
    }
    else {
        const struct LxTestDef * def;

        if ((def = LxTestDef_Find(argv[0])))
            r += def->func(argc - 1, argv + 1);
    }
    return r;
}

/* We list commands in an array. The function that prints the array has to 
use it and to be a part of it too, so we need a forward declaration. */

static LxTestRes
LxTestCmd_Help(int, char**);

/* Definitions of commands. */

static const struct LxTestCmd {
  const char   *name   ,
               *args   ;
    int         minArgc,
                maxArgc;
    LxTestRes (*func)(int, char**);
  const char   *desc   ;
} lxTestCmds[] = {
    { "test", "[<test>]", 0, 0, &LxTestCmd_Test,
      "Run all tests or a specified test, print errors to 'stdout'." },
    { "test-list", NULL, 0, 0, &LxTestCmd_TestList,
      "Print test names to 'stdout'." },
    { "enc-list", NULL, 0, 0, &LxTestCmd_EncList,
      "Print encoding names to 'stdout'." },
    { "enc-print", "<enc>", 1, 1, &LxTestCmd_EncPrint,
      "Print all codes in this encoding to 'stdout'." },
    { "enc-conv", "<in-enc> <out-enc>", 2, 2, &LxTestCmd_EncConv,
      "Read 'stdin' in '<in-enc>' and print to 'stdout' in '<out-enc>'." },
    { "help", NULL, 0, 0, &LxTestCmd_Help,
      "Print help (this screen)." },
};

/* (Command) Print help to 'stdout'. */

static LxTestRes
LxTestCmd_Help(int /* unused */, char** /* unused */)
{
    size_t i;

    LxTest_OutStr(stdout,
        "Usage:\n\n"
        "  lxtool <command>\n\n"
        "where '<command>' is one of the following:\n\n");
    for (i = 0; i < Lx_ARRAYSZ(lxTestCmds); ++i) {
        const struct LxTestCmd *cmd;
        
        cmd = lxTestCmds + i;
        LxTest_OutFmt(stdout, "  %s", cmd->name);
        if (cmd->args != NULL)
            LxTest_OutFmt(stdout, " %s", cmd->args);
        LxTest_OutFmt(stdout, "\n    %s\n", cmd->desc);
    }
    return 0;
}

/* Read command-line arguments; if none, print help, else find and execute
the specified command. */

int
main(int argc, char* argv[])
{
    int ec; 

    if (argc == 1 /* no arguments, only the executable name */) {
        (void)LxTestCmd_Help(argc, argv);
        ec = LxTest_SUCCESS;
    }
    else {
        size_t i; 

        for (i = 0; i < Lx_ARRAYSZ(lxTestCmds); ++i) {
            const struct LxTestCmd* cmd; 
            
            cmd = lxTestCmds + i;
            if (strcmp(cmd->name, argv[1]) == 0) {
                if ((argc - 2) < cmd->minArgc
                    || (cmd->maxArgc != 0 && (argc - 2) > cmd->maxArgc)) {
                    LxTest_OutFmt(stderr, "%s: Incorrect number of "
                            "arguments for '%s'; expected %" PRIu8 "..%"
                            PRIu8 ", got %d.", lxToolName, argv[1],
                            cmd->minArgc, cmd->maxArgc, (argc - 2));
                    ec = LxTest_FAILURE;
                }
                else {
                    LxTestRes r;
                    
                    r = cmd->func(argc - 2, argv + 2);
                    if (r == 0)
                        ec = LxTest_SUCCESS;
                    else {
                        LxTest_OutFmt(stdout, "Failed tests: %" LxTestRES
                                ".\n", r);
                        ec = LxTest_FAILURE;
                    }
                }
                break;
            }
        }
        if (i == Lx_ARRAYSZ(lxTestCmds)) {
            LxTest_OutFmt(stderr, "%s: Failed to find command '%s'\n",
                    lxToolName, argv[1]);
            ec = LxTest_FAILURE;
        }
    }
    return ec;
}

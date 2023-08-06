#include <stdint.h>
#include <signal.h>
#include <assert.h>
#include <cmath>

#include <cstdio>
#include <vector>

/* NESEMU1 : EMULATOR FOR THE NINTENDO ENTERTAINMENT SYSTEM (R) ARCHITECTURE  */
/* Written by and copyright (C) 2011  Joel Yliluoma - http://iki.fi/bisqwit/  */
/* Trademarks are owned by their respective owners. Lawyers love tautologies. */

static const char* inputfn = "input.fmv";

#define BYTE_TO_BINARY_PATTERN "%c %c %c %c %c %c"
#define BYTE_TO_BINARY(byte)  \
  ((byte) & 0x01 ? '1' : '0'), \
  ((byte) & 0x02 ? '1' : '0'), \
  ((byte) & 0x04 ? '1' : '0'), \
  ((byte) & 0x08 ? '1' : '0'), \
  ((byte) & 0x40 ? '1' : '0'), \
  ((byte) & 0x80 ? '1' : '0')


static int address_len_table[256] = {
/*       |0|1|2|3|4|5|6|7|8|9|A|B|C|D|E|F|*/
  /* 0 */ 0,1,0,1,1,1,1,1,0,1,1,1,2,2,2,2,/* 0 */
  /* 1 */ 1,1,0,1,1,1,1,1,0,2,0,2,2,2,2,2,/* 1 */
  /* 2 */ 2,1,0,1,1,1,1,1,0,1,1,1,2,2,2,2,/* 2 */
  /* 3 */ 1,1,0,1,1,1,1,1,0,2,0,2,2,2,2,2,/* 3 */
  /* 4 */ 0,1,0,1,1,1,1,1,0,1,1,1,2,2,2,2,/* 4 */
  /* 5 */ 1,1,0,1,1,1,1,1,0,2,0,2,2,2,2,2,/* 5 */
  /* 6 */ 0,1,0,1,1,1,1,1,0,1,1,1,1,2,2,2,/* 6 */
  /* 7 */ 1,1,0,1,1,1,1,1,0,2,0,2,2,2,2,2,/* 7 */
  /* 8 */ 1,1,1,1,1,1,1,1,0,1,0,1,2,2,2,2,/* 8 */
  /* 9 */ 1,1,0,1,1,1,1,1,0,2,0,2,2,2,2,2,/* 9 */
  /* A */ 1,1,1,1,1,1,1,1,0,1,0,1,2,2,2,2,/* A */
  /* B */ 1,1,0,1,1,1,1,1,0,2,0,2,2,2,2,2,/* B */
  /* C */ 1,1,1,1,1,1,1,1,0,1,0,1,2,2,2,2,/* C */
  /* D */ 1,1,0,1,1,1,1,1,0,2,0,2,2,2,2,2,/* D */
  /* E */ 1,1,1,1,1,1,1,1,0,1,0,1,2,2,2,2,/* E */
  /* F */ 1,1,0,1,1,1,1,1,0,2,0,2,2,2,2,2  /* F */
};

static const char* op_name_table[256] = {
  /*       |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  A  |  B  |  C  |  D  |  E  |  F  |      */
  /* 0 */   "BRK","ORA","NOP","SLO","NOP","ORA","ASL","SLO","PHP","ORA","ASL","NOP","NOP","ORA","ASL","SLO", /* 0 */
  /* 1 */   "BPL","ORA","NOP","SLO","NOP","ORA","ASL","SLO","CLC","ORA","NOP","SLO","NOP","ORA","ASL","SLO", /* 1 */
  /* 2 */   "JSR","AND","NOP","RLA","BIT","AND","ROL","RLA","PLP","AND","ROL","NOP","BIT","AND","ROL","RLA", /* 2 */
  /* 3 */   "BMI","AND","NOP","RLA","NOP","AND","ROL","RLA","SEC","AND","NOP","RLA","NOP","AND","ROL","RLA", /* 3 */
  /* 4 */   "RTI","EOR","NOP","SRE","NOP","EOR","LSR","SRE","PHA","EOR","LSR","NOP","JMP","EOR","LSR","SRE", /* 4 */
  /* 5 */   "BVC","EOR","NOP","SRE","NOP","EOR","LSR","SRE","CLI","EOR","NOP","SRE","NOP","EOR","LSR","SRE", /* 5 */
  /* 6 */   "RTS","ADC","NOP","RRA","NOP","ADC","ROR","RRA","PLA","ADC","ROR","NOP","JMP","ADC","ROR","RRA", /* 6 */
  /* 7 */   "BVS","ADC","NOP","RRA","NOP","ADC","ROR","RRA","SEI","ADC","NOP","RRA","NOP","ADC","ROR","RRA", /* 7 */
  /* 8 */   "NOP","STA","NOP","SAX","STY","STA","STX","SAX","DEY","NOP","TXA","NOP","STY","STA","STX","SAX", /* 8 */
  /* 9 */   "BCC","STA","NOP","NOP","STY","STA","STX","SAX","TYA","STA","TXS","NOP","NOP","STA","NOP","NOP", /* 9 */
  /* A */   "LDY","LDA","LDX","LAX","LDY","LDA","LDX","LAX","TAY","LDA","TAX","NOP","LDY","LDA","LDX","LAX", /* A */
  /* B */   "BCS","LDA","NOP","LAX","LDY","LDA","LDX","LAX","CLV","LDA","TSX","LAX","LDY","LDA","LDX","LAX", /* B */
  /* C */   "CPY","CMP","NOP","DCP","CPY","CMP","DEC","DCP","INY","CMP","DEX","NOP","CPY","CMP","DEC","DCP", /* C */
  /* D */   "BNE","CMP","NOP","DCP","NOP","CMP","DEC","DCP","CLD","CMP","NOP","DCP","NOP","CMP","DEC","DCP", /* D */
  /* E */   "CPX","SBC","NOP","ISB","CPX","SBC","INC","ISB","INX","SBC","NOP","SBC","CPX","SBC","INC","ISB", /* E */
  /* F */   "BEQ","SBC","NOP","ISB","NOP","SBC","INC","ISB","SED","SBC","NOP","ISB","NOP","SBC","INC","ISB"  /* F */
};


// Integer types
typedef uint_least32_t u32;
typedef uint_least16_t u16;
typedef uint_least8_t   u8;
typedef  int_least8_t   s8;

// Bitfield utilities
template<unsigned bitno, unsigned nbits=1, typename T=u8>
struct RegBit
{
    T data;
    enum { mask = (1u << nbits) - 1u };
    template<typename T2>
    RegBit& operator=(T2 val)
    {
        data = (data & ~(mask << bitno)) | ((nbits > 1 ? val & mask : !!val) << bitno);
        return *this;
    }
    operator unsigned() const { return (data >> bitno) & mask; }
    RegBit& operator++ ()     { return *this = *this + 1; }
    unsigned operator++ (int) { unsigned r = *this; ++*this; return r; }
};

namespace GamePak
{
    std::vector<u8> ROM, VRAM(0x2000);
    const unsigned VROM_Granularity = 0x0400, VROM_Pages = 0x2000 / VROM_Granularity;
    const unsigned ROM_Granularity  = 0x2000, ROM_Pages = 0x10000 / ROM_Granularity;
    unsigned char NRAM[0x1000], PRAM[0x2000];
    unsigned char* banks[ROM_Pages]  = {};
    unsigned char* Vbanks[VROM_Pages] = {};
    unsigned char *Nta[4] = { NRAM+0x0000, NRAM+0x0400, NRAM+0x0000, NRAM+0x0400 };

    template<unsigned npages,unsigned char*(&b)[npages], std::vector<u8>& r, unsigned granu>
    static void SetPages(unsigned size, unsigned baseaddr, unsigned index)
    {
        for(unsigned v = r.size() + index * size,
                     p = baseaddr / granu;
                     p < (baseaddr + size) / granu && p < npages;
                     ++p, v += granu)
            b[p] = &r[v % r.size()];
    }
    auto& SetROM  = SetPages< ROM_Pages, banks, ROM, ROM_Granularity>;
    auto& SetVROM = SetPages<VROM_Pages,Vbanks,VRAM,VROM_Granularity>;

    u8 Access(unsigned addr)
    {
        if( (addr >> 13) == 3 ) return PRAM[addr & 0x1FFF ];
        return banks[ (addr / ROM_Granularity) % ROM_Pages] [addr % ROM_Granularity];
    }
    void Init()
    {
        SetVROM(0x2000, 0x0000, 0);
        for(unsigned v=0; v<4; ++v) SetROM(0x4000, v*0x4000, v==3 ? -1 : 0);
    }
}

namespace CPU /* CPU: Ricoh RP2A03 (based on MOS6502, almost the same as in Commodore 64) */
{
    u8 RAM[0x800];
    bool reset=false, nmi=false, nmi_edge_detected=false, intr=false;

    template<bool write> u8 MemAccess(u16 addr, u8 v=0);
    u8 RB(u16 addr)      { return MemAccess<0>(addr); }
    u8 WB(u16 addr,u8 v) { return MemAccess<1>(addr, v); }
    void tick();
}

namespace CPU
{
    void tick()
    {
    }

    template<bool write> u8 MemAccess(u16 addr, u8 v)
    {
        // Memory writes are turned into reads while reset is being signalled
        if(reset && write) return MemAccess<0>(addr);

        tick();
        // Map the memory from CPU's viewpoint.
        /**/ if(addr < 0x2000) { u8& r = RAM[addr & 0x7FF]; if(!write)return r; r=v; }
        else return GamePak::Access(addr);
        return 0;
    }

    // CPU registers:
    u16 PC=0xC000;
    u8 A=0,X=0,Y=0,S=0;
    union /* Status flags: */
    {
        u8 raw;
        RegBit<0> C; // carry
        RegBit<1> Z; // zero
        RegBit<2> I; // interrupt enable/disable
        RegBit<3> D; // decimal mode (unsupported on NES, but flag exists)
        // 4,5 (0x10,0x20) don't exist
        RegBit<6> V; // overflow
        RegBit<7> N; // negative
    } P;

    u16 wrap(u16 oldaddr, u16 newaddr)  { return (oldaddr & 0xFF00) + u8(newaddr); }
    void Misfire(u16 old, u16 addr) { u16 q = wrap(old, addr); if(q != addr) RB(q); }
    u8   Pop()        { return RB(0x100 | u8(++S)); }
    void Push(u8 v)   { WB(0x100 | u8(S--), v); }

    template<u16 op> // Execute a single CPU instruction, defined by opcode "op".
    void Ins()       // With template magic, the compiler will literally synthesize >256 different functions.
    {
        // Note: op 0x100 means "NMI", 0x101 means "Reset", 0x102 means "IRQ". They are implemented in terms of "BRK".
        // User is responsible for ensuring that WB() will not store into memory while Reset is being processed.
        unsigned addr=0, d=0, t=0xFF, c=0, sb=0, pbits = op<0x100 ? 0x30 : 0x20;

        // Define the opcode decoding matrix, which decides which micro-operations constitute
        // any particular opcode. (Note: The PLA of 6502 works on a slightly different principle.)
        enum { o8 = op/8, o8m = 1 << (op%8) };
        // Fetch op'th item from a bitstring encoded in a data-specific variant of base64,
        // where each character transmits 8 bits of information rather than 6.
        // This peculiar encoding was chosen to reduce the source code size.
        // Enum temporaries are used in order to ensure compile-time evaluation.
        #define t(s,code) { enum { \
            i=o8m & (s[o8]>90 ? (130+" (),-089<>?BCFGHJLSVWZ[^hlmnxy|}"[s[o8]-94]) \
                              : (s[o8]-" (("[s[o8]/39])) }; if(i>0) { code; } }

        /* Decode address operand */
        t("                                !", addr = 0xFFFA) // NMI vector location
        t("                                *", addr = 0xFFFC) // Reset vector location
        t("!                               ,", addr = 0xFFFE) // Interrupt vector location
        t("zy}z{y}zzy}zzy}zzy}zzy}zzy}zzy}z ", addr = RB(PC++))
        t("2 yy2 yy2 yy2 yy2 XX2 XX2 yy2 yy ", d = X) // register index
        t("  62  62  62  62  om  om  62  62 ", d = Y)
        t("2 y 2 y 2 y 2 y 2 y 2 y 2 y 2 y  ", addr=u8(addr+d); d=0; tick())              // add zeropage-index
        t(" y z!y z y z y z y z y z y z y z ", addr=u8(addr);   addr+=256*RB(PC++))       // absolute address
        t("3 6 2 6 2 6 286 2 6 2 6 2 6 2 6 /", addr=RB(c=addr); addr+=256*RB(wrap(c,c+1)))// indirect w/ page wrap
        t("  *Z  *Z  *Z  *Z      6z  *Z  *Z ", Misfire(addr, addr+d)) // abs. load: extra misread when cross-page
        t("  4k  4k  4k  4k  6z      4k  4k ", RB(wrap(addr, addr+d)))// abs. store: always issue a misread
        /* Load source operand */
        t("aa__ff__ab__,4  ____ -  ____     ", t &= A) // Many operations take A or X as operand. Some try in
        t("                knnn     4  99   ", t &= X) // error to take both; the outcome is an AND operation.
        t("                9989    99       ", t &= Y) // sty,dey,iny,tya,cpy
        t("                       4         ", t &= S) // tsx, las
        t("!!!!  !!  !!  !!  !   !!  !!  !!/", t &= P.raw|pbits; c = t)// php, flag test/set/clear, interrupts
        t("_^__dc___^__            ed__98   ", c = t; t = 0xFF)        // save as second operand
        t("vuwvzywvvuwvvuwv    zy|zzywvzywv ", t &= RB(addr+d)) // memory operand
        t(",2  ,2  ,2  ,2  -2  -2  -2  -2   ", t &= RB(PC++))   // immediate operand
        /* Operations that mogrify memory operands directly */
        t("    88                           ", P.V = t & 0x40; P.N = t & 0x80) // bit
        t("    nink    nnnk                 ", sb = P.C)       // rol,rla, ror,rra,arr
        t("nnnknnnk     0                   ", P.C = t & 0x80) // rol,rla, asl,slo,[arr,anc]
        t("        nnnknink                 ", P.C = t & 0x01) // lsr,sre, ror,rra,asr
        t("ninknink                         ", t = (t << 1) | (sb * 0x01))
        t("        nnnknnnk                 ", t = (t >> 1) | (sb * 0x80))
        t("                 !      kink     ", t = u8(t - 1))  // dec,dex,dey,dcp
        t("                         !  khnk ", t = u8(t + 1))  // inc,inx,iny,isb
        /* Store modified value (memory) */
        t("kgnkkgnkkgnkkgnkzy|J    kgnkkgnk ", WB(addr+d, t))
        t("                   q             ", WB(wrap(addr, addr+d), t &= ((addr+d) >> 8))) // [shx,shy,shs,sha?]
        /* Some operations used up one clock cycle that we did not account for yet */
        t("rpstljstqjstrjst - - - -kjstkjst/", tick()) // nop,flag ops,inc,dec,shifts,stack,transregister,interrupts
        /* Stack operations and unconditional jumps */
        t("     !  !    !                   ", tick(); t = Pop())                        // pla,plp,rti
        t("        !   !                    ", RB(PC++); PC = Pop(); PC |= (Pop() << 8)) // rti,rts
        t("            !                    ", RB(PC++))  // rts
        t("!   !                           /", d=PC+(op?-1:1); Push(d>>8); Push(d))      // jsr, interrupts
        t("!   !    8   8                  /", PC = addr) // jmp, jsr, interrupts
        t("!!       !                      /", Push(t))   // pha, php, interrupts
        /* Bitmasks */
        t("! !!  !!  !!  !!  !   !!  !!  !!/", t = 1)
        t("  !   !                   !!  !! ", t <<= 1)
        t("! !   !   !!  !!       !   !   !/", t <<= 2)
        t("  !   !   !   !        !         ", t <<= 4)
        t("   !       !           !   !____ ", t = u8(~t)) // sbc, isb,      clear flag
        t("`^__   !       !               !/", t = c | t)  // ora, slo,      set flag
        t("  !!dc`_  !!  !   !   !!  !!  !  ", t = c & t)  // and, bit, rla, clear/test flag
        t("        _^__                     ", t = c ^ t)  // eor, sre
        /* Conditional branches */
        t("      !       !       !       !  ", if(t)  { tick(); Misfire(PC, addr = s8(addr) + PC); PC=addr; })
        t("  !       !       !       !      ", if(!t) { tick(); Misfire(PC, addr = s8(addr) + PC); PC=addr; })
        /* Addition and subtraction */
        t("            _^__            ____ ", c = t; t += A + P.C; P.V = (c^t) & (A^t) & 0x80; P.C = t & 0x100)
        t("                        ed__98   ", t = c - t; P.C = ~t & 0x100) // cmp,cpx,cpy, dcp, sbx
        /* Store modified value (register) */
        t("aa__aa__aa__ab__ 4 !____    ____ ", A = t)
        t("                    nnnn 4   !   ", X = t) // ldx, dex, tax, inx, tsx,lax,las,sbx
        t("                 !  9988 !       ", Y = t) // ldy, dey, tay, iny
        t("                   4   0         ", S = t) // txs, las, shs
        t("!  ! ! !!  !   !       !   !   !/", P.raw = t & ~0x30) // plp, rti, flag set/clear
        /* Generic status flag updates */
        t("wwwvwwwvwwwvwxwv 5 !}}||{}wv{{wv ", P.N = t & 0x80)
        t("wwwv||wvwwwvwxwv 5 !}}||{}wv{{wv ", P.Z = u8(t) == 0)
        t("             0                   ", P.V = (((t >> 5)+1)&2))         // [arr]
        /* All implemented opcodes are cycle-accurate and memory-access-accurate.
         * [] means that this particular separate rule exists only to provide the indicated unofficial opcode(s).
         */
    }

    void printState (unsigned op, u16 oPC) {
        printf("op: %s ", op_name_table[op]);
        if (address_len_table[op]) {
          printf("$%02x%02x ", (address_len_table[op] > 1) ? RB(oPC+2) : 0x00, RB(oPC+1));
        } else {
          printf("      ");
        }
        printf("PC: %06u, A:%u , X: %u, Y: %u S: " BYTE_TO_BINARY_PATTERN, PC, A, X, Y, BYTE_TO_BINARY(P.raw));
        printf("\n");
        return;
    }

    void Op()
    {
        /* Check the state of NMI flag */
        bool nmi_now = nmi;

        u16 oPC = PC;
        unsigned op = RB(PC++);

        if(reset)                              { op=0x101; }
        else if(nmi_now && !nmi_edge_detected) { op=0x100; nmi_edge_detected = true; }
        else if(intr && !P.I)                  { op=0x102; }
        if(!nmi_now) nmi_edge_detected=false;

        // Define function pointers for each opcode (00..FF) and each interrupt (100,101,102)
        #define c(n) Ins<0x##n>,Ins<0x##n+1>,
        #define o(n) c(n)c(n+2)c(n+4)c(n+6)
        static void(*const i[0x108])() =
        {
            o(00)o(08)o(10)o(18)o(20)o(28)o(30)o(38)
            o(40)o(48)o(50)o(58)o(60)o(68)o(70)o(78)
            o(80)o(88)o(90)o(98)o(A0)o(A8)o(B0)o(B8)
            o(C0)o(C8)o(D0)o(D8)o(E0)o(E8)o(F0)o(F8) o(100)
        };
        #undef o
        #undef c
        i[op]();

        printState(op, oPC);

        reset = false;
    }
}

int main(int/*argc*/, char** argv)
{
    // Open the ROM file specified on commandline
    FILE* fp = fopen(argv[1], "rb");
    inputfn = argv[2];

    // Read the ROM file header
    assert(fgetc(fp)=='\00' && fgetc(fp)=='\00');
    u8 rom16count = 0x01;
    u8 vrom8count = 0x01;

    // Read the ROM data
    if(rom16count) GamePak::ROM.resize(rom16count * 0x4000);
    if(vrom8count) GamePak::VRAM.resize(vrom8count * 0x2000);
    fread(&GamePak::ROM[0], rom16count, 0x4000, fp);
    fread(&GamePak::VRAM[0], vrom8count, 0x2000, fp);

    fclose(fp);

    // Start emulation
    GamePak::Init();

    // Pre-initialize RAM the same way as FCEUX does, to improve TAS sync.
    for(unsigned a=0; a<0x800; ++a)
        CPU::RAM[a] = (a&4) ? 0xFF : 0x00;

    // Run the CPU until we run out of instructions.
    for(;;) {
      CPU::Op();
      if (!CPU::PC) break;
    }
}





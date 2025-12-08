.section .rodata
.align 2
# 65536 / sqrt(2^n) 查表 (0 到 31)
rsqrt_table:
    .half 65535, 46341, 32768, 23170, 16384, 11585, 8192, 5793, 4096, 2896
    .half 2048, 1448, 1024, 724, 512, 362, 256, 181, 128, 90
    .half 64, 45, 32, 23, 16, 11, 8, 6, 4, 3, 2, 1

test_inputs:
    .word 0, 1, 4, 16, 100, 1024, 65536, 1048576, 10, 42, 12345
test_inputs_end:

.section .text
.global _start

_start:
    la      s0, test_inputs
    la      s1, test_inputs_end
    li      s2, 0x200           
    li      s11, 1              

test_loop:
    beq     s0, s1, test_finish
    lw      a0, 0(s0)

    # --- 效能計數起始 ---
    csrr    s4, cycle           

    # 執行運算
    jal     ra, fast_rsqrt
    
    # --- 效能計數結束 ---
    csrr    t1, cycle
    sub     t2, t1, s4          

    # 握手機制：先寫結果，最後寫 Index
    sw      a0, 0(s2)           # 結果 (0x200)
    sw      t2, 8(s2)           # HW Cycles (0x208)
    sw      s11, 4(s2)          # Index (0x204) 

    addi    s0, s0, 4
    addi    s11, s11, 1         
    j       test_loop
test_finish: ebreak; j .

# --- 完整演算法：fast_rsqrt ---
fast_rsqrt:
    addi    sp, sp, -32
    sw      ra, 28(sp)
    sw      s1, 24(sp)          # 保存原始 x
    sw      s2, 20(sp)          # 保存估計值 y
    sw      s3, 16(sp)          # 保存 exp

    beqz    a0, rsqrt_zero
    li      t0, 1
    beq     a0, t0, rsqrt_one

    mv      s1, a0              # s1 = x
    jal     ra, clz             # a0 = leading zeros (回傳 29 for x=4)
    li      t0, 31
    sub     s3, t0, a0          # s3 = exp = 2 for x=4

    # Step 1: LUT 查表
    la      t2, rsqrt_table
    slli    t3, s3, 1
    add     t2, t2, t3
    lhu     s2, 0(t2)           # s2 = 32768 for x=4

    # --- 優化：判斷是否為 2^n ---
    li      t0, 1
    sll     t0, t0, s3          # t0 = 4 for x=4
    beq     s1, t0, finish_rsqrt # 如果相等，直接跳轉到 epilogue (s2 已是正確結果)

    # Step 2: 線性插值
    sub     t1, s1, t0          
    slli    t1, t1, 16
    srl     t1, t1, s3          

    li      t4, 0
    li      t5, 31
    bge     s3, t5, skip_interp
    lhu     t4, 2(t2)           

    sub     t5, s2, t4          
    mv      a0, t5
    mv      a1, t1
    jal     ra, mul32           
    srli    a0, a0, 16          
    slli    a1, a1, 16          
    or      a0, a0, a1
    sub     s2, s2, a0          

skip_interp:
    jal     ra, newton_step     
    mv      s2, a0
    jal     ra, newton_step     
    mv      s2, a0

finish_rsqrt:
    # 這是標準出口：將 s2 內的結果搬移至 a0
    mv      a0, s2
    
rsqrt_restore:
    # 恢復暫存器並回傳
    lw      ra, 28(sp)
    lw      s1, 24(sp)
    lw      s2, 20(sp)
    lw      s3, 16(sp)
    addi    sp, sp, 32
    ret

rsqrt_zero: 
    li      s2, -1              # 特殊結果放入 s2
    j       finish_rsqrt
rsqrt_one:  
    li      s2, 65536           # 特殊結果放入 s2
    j       finish_rsqrt

# --- Newton Step / clz / mul32 ---
newton_step:
    addi    sp, sp, -8
    sw      ra, 4(sp)
    mv      a0, s2
    mv      a1, s2
    jal     ra, mul32           
    mv      t4, a0              
    mv      a0, s1
    mv      a1, t4
    jal     ra, mul32           
    srli    t5, a0, 16
    slli    t6, a1, 16
    or      t5, t5, t6          
    li      t6, 0x30000
    sub     t6, t6, t5
    mv      a0, s2
    mv      a1, t6
    jal     ra, mul32           
    srli    a0, a0, 17
    slli    a1, a1, 15
    or      a0, a0, a1
    lw      ra, 4(sp)
    addi    sp, sp, 8
    ret

clz:
    li      t0, 0
    li      t2, 0xFFFF0000
    and     t1, a0, t2
    bnez    t1, c8
    addi    t0, t0, 16
    slli    a0, a0, 16
c8: li      t2, 0xFF000000
    and     t1, a0, t2
    bnez    t1, c4
    addi    t0, t0, 8
    slli    a0, a0, 8
c4: li      t2, 0xF0000000
    and     t1, a0, t2
    bnez    t1, c2
    addi    t0, t0, 4
    slli    a0, a0, 4
c2: li      t2, 0xC0000000
    and     t1, a0, t2
    bnez    t1, c1
    addi    t0, t0, 2
    slli    a0, a0, 2
c1: li      t2, 0x80000000
    and     t1, a0, t2
    bnez t1, t_done
    addi    t0, t0, 1
t_done: mv      a0, t0
    ret

mul32:
    li      t0, 0
    li      t1, 0
    li      t2, 0
mul_lp:
    beqz    a1, mul_ed
    andi    t3, a1, 1
    beqz    t3, mul_sh
    add     t0, t0, a0
    sltu    t3, t0, a0
    add     t1, t1, t2
    add     t1, t1, t3
mul_sh:
    srli    t3, a0, 31
    slli    a0, a0, 1
    slli    t2, t2, 1
    or      t2, t2, t3
    srli    a1, a1, 1
    j       mul_lp
mul_ed:
    mv      a0, t0
    mv      a1, t1
    ret
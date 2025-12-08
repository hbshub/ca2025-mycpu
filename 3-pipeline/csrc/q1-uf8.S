.section .text
.global _start
.global run_q1_uf8

# ==========================================
# 1. Entry Point
# ==========================================
_start:
    csrr s5, cycle       # use cycle CSR to get start time
    jal  ra, run_q1_uf8  # call the core function
    mv   s6, a0          # a0 = Pass(1)/Fail(0) 
    csrr t0, cycle       # use cycle CSR to get end time
    sub  t1, t0, s5      # Duration = End - Start, even with overflow

    # --------------------------------------
    # D. 寫入記憶體 (通訊協定)
    # --------------------------------------
    
    # [0x30] 寫入 Cycles (32-bit)
    li   t2, 0x30
    sw   t1, 0(t2)

    # [0x08] 寫入 Result (Pass=1 / Fail=0)
    li   t2, 0x08
    sw   s6, 0(t2)

    # [0x04] 寫入 Magic Number (觸發 Testbench 結束)
    # 你的 MyCPU Testbench 應該監控這個位址，看到 DEADBEEF 就 $finish
    li   t2, 0x04
    li   t3, 0xDEADBEEF
    sw   t3, 0(t2)

finishloop:
    j finishloop

# ==========================================
# 2. 核心函式 (已移除 s 暫存器保存)
# ==========================================
run_q1_uf8:
    # [優化] 移除了 s0-s4 的 sw/lw，因為 _start 不介意它們被改變
    # 但必須保留 ra 的保存，因為這裡會呼叫 test
    addi sp, sp, -16
    sw   ra, 12(sp)
    
    # Initialize variables
    li   s0, 0            # s0 = i
    li   s1, -1           # s1 = previous_value
    li   s2, 256          # s2 = Remaining count
    li   s3, 1            # s3 = passed flag
    
    jal  test             # Execute test loop


    # Restore ra only
    lw   ra, 12(sp)
    mv   a0, s3           # Place result
    addi sp, sp, 16
    ret 

# ==========================================
# 3. Test Loop
# ==========================================
test:
    # 這裡呼叫 uf8_encode，所以必須保存 ra
    addi sp, sp, -16
    sw   ra, 12(sp)

loop:
    beqz s2, test_end     # If count == 0, end
    andi a0, s0, 255      # a0 = i

    # --- Inlined uf8_decode ---
    srli t0, a0, 4        
    andi a0, a0, 0x0F     
    addi a0, a0, 16       
    sll  a0, a0, t0       
    addi a0, a0, -16
    # --------------------------

    addi s4, a0, 0        # s4 = decoded value
    jal  uf8_encode       # Call encode (Clobbers ra, hence we saved it)
    andi a0, a0, 255      

    # --- Verification Logic ---
    sub  t6, s0, a0       
    seqz t6, t6           

    bne  s0, a0, mark_fail

    # Write Result (1) to 0x20 + 8
    li   t5, 0x20         
    sw   t6, 8(t5)        
    
    # Write Index to 0x20
    li   t4, 0x1000       
    add  t4, t4, s0
    sw   t4, 0(t5)        
    
    j    check_mono_inc

mark_fail:
    li   s3, 0
    
    # Write Result (0) to 0x20 + 8
    li   t5, 0x20
    sw   zero, 8(t5)      
    
    # Write Index
    li   t4, 0x1000
    add  t4, t4, s0
    sw   t4, 0(t5)        

check_mono_inc:
    blt  s1, s4, loop_continue 
    li   s3, 0 

loop_continue:
    addi s1, s4, 0
    addi s0, s0, 1
    addi s2, s2, -1
    j    loop

test_end:
    lw   ra, 12(sp)
    addi sp, sp, 16
    ret

# ==========================================
# 4. Encoder
# ==========================================
uf8_encode:
    li  t3, 16
    bge a0, t3, e_not_zero         
    ret
e_not_zero:
    # 這裡呼叫 clz，必須保存 ra
    addi sp, sp, -16      
    sw   ra, 12(sp)       
    mv   t4, a0           
    jal  ra, clz          
    li   t3, 31
    sub  t0, t3, a0       
    mv   a0, t4           
    lw   ra, 12(sp)       
    addi sp, sp, 16       
    
    li t1, 0                
    li t2, 0                
    li   t3, 5
    blt  t0, t3, find_exa_exp         
    addi t1, t0, -4               
    sltiu t3, t1, 16
    beqz  t3, set_exp_15    
    j     build_of_once
set_exp_15:
    li    t1, 15          
build_of_once:
    li   t2, 1
    sll  t2, t2, t1       
    addi t2, t2, -1       
    slli t2, t2, 4        
    j    adj_exp          
adj_exp:
    ble  t1, x0, find_exa_exp        
    bge  a0, t2, find_exa_exp         
    addi t2, t2, -16                
    srli t2, t2, 1                  
    addi t1, t1, -1                 
    j    adj_exp
find_exa_exp:
    li   t6, 15
    bge  t1, t6, calc_m          
    slli t0, t2, 1               
    addi t0, t0, 16              
    blt  a0, t0, calc_m          
    mv   t2, t0                  
    addi t1, t1, 1               
    j    find_exa_exp
calc_m:
    sub t0, a0, t2               
    srl t0, t0, t1              
    li  t6, 15
    ble t0, t6, cmb_num        
    li  t0, 15                   
cmb_num:    
    slli t1, t1, 4               
    or   a0, t1, t0              
    ret

# ==========================================
# 5. Helper (Leaf Function)
# ==========================================
clz:
    # Leaf function (不呼叫其他人)，不需要操作 Stack/sw
    beq   a0, x0, clz_zero
    li    t0, 0
    srli  t1, a0, 16            
    bnez  t1, check_8_bits      
    addi  t0, t0, 16            
    slli  a0, a0, 16            
check_8_bits:
    srli  t1, a0, 24            
    bnez  t1, check_4_bits      
    addi  t0, t0, 8             
    slli  a0, a0, 8             
check_4_bits:
    srli  t1, a0, 28            
    bnez  t1, check_2_bits      
    addi  t0, t0, 4             
    slli  a0, a0, 4             
check_2_bits:
    srli  t1, a0, 30            
    bnez  t1, check_1_bit       
    addi  t0, t0, 2             
    slli  a0, a0, 2             
check_1_bit:
    srli  t1, a0, 31            
    bnez  t1, clz_finish        
    addi  t0, t0, 1             
clz_finish:
    mv    a0, t0                
    ret
clz_zero:
    li    a0, 32                
    ret
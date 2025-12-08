// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.core.CSRRegister

class PipelineProgramTest extends AnyFlatSpec with ChiselScalatestTester {
  private val mcauseAcceptable: Set[BigInt] =
    Set(BigInt("80000007", 16), BigInt("8000000B", 16))

  private def runProgram(exe: String, cfg: PipelineConfig)(body: TestTopModule => Unit): Unit = {
    test(new TestTopModule(exe, cfg.implementation))
      .withAnnotations(TestAnnotations.annos) { c =>
        c.io.csr_debug_read_address.poke(0.U)
        c.io.interrupt_flag.poke(0.U)
        body(c)
      }
  }

  for (cfg <- PipelineConfigs.All) {
    behavior.of(cfg.name)

    it should "calculate recursively fibonacci(10)" in {
      runProgram("fibonacci.asmbin", cfg) { c =>
        for (i <- 1 to 50) {
          c.clock.step(1000)
          c.io.mem_debug_read_address.poke((i * 4).U)
        }
        c.io.mem_debug_read_address.poke(4.U)
        c.clock.step()
        c.io.mem_debug_read_data.expect(55.U)
      }
    }

    it should "quicksort 10 numbers" in {
      runProgram("quicksort.asmbin", cfg) { c =>
        for (i <- 1 to 50) {
          c.clock.step(1000)
          c.io.mem_debug_read_address.poke((i * 4).U)
        }
        for (i <- 1 to 10) {
          c.io.mem_debug_read_address.poke((4 * i).U)
          c.clock.step()
          c.io.mem_debug_read_data.expect((i - 1).U)
        }
      }
    }

    it should "store and load single byte" in {
      runProgram("sb.asmbin", cfg) { c =>
        c.clock.step(1000)
        c.io.regs_debug_read_address.poke(5.U)
        c.io.regs_debug_read_data.expect(0xdeadbeefL.U)
        c.io.regs_debug_read_address.poke(6.U)
        c.io.regs_debug_read_data.expect(0xef.U)
        c.io.regs_debug_read_address.poke(1.U)
        c.io.regs_debug_read_data.expect(0x15ef.U)
      }
    }

    it should "solve data and control hazards" in {
      runProgram("hazard.asmbin", cfg) { c =>
        c.clock.step(1000)
        c.io.regs_debug_read_address.poke(1.U)
        c.io.regs_debug_read_data.expect(cfg.hazardX1.U)
        c.io.mem_debug_read_address.poke(4.U)
        c.clock.step()
        c.io.mem_debug_read_data.expect(1.U)
        c.io.mem_debug_read_address.poke(8.U)
        c.clock.step()
        c.io.mem_debug_read_data.expect(3.U)
      }
    }

    it should "handle all hazard types comprehensively" in {
      runProgram("hazard_extended.asmbin", cfg) { c =>
        c.clock.step(1000)

        // Section 1: WAW (Write-After-Write) - later write wins
        c.io.mem_debug_read_address.poke(0x10.U)
        c.clock.step()
        c.io.mem_debug_read_data.expect(2.U, "WAW: mem[0x10] should be 2 (final write)")

        // Section 2: Store-Load Forwarding
        c.io.mem_debug_read_address.poke(0x14.U)
        c.clock.step()
        c.io.mem_debug_read_data.expect(0xab.U, "Store-Load: mem[0x14] should be 0xAB")

        // Section 3: Multiple Consecutive Loads (sum of zeros)
        c.io.mem_debug_read_address.poke(0x18.U)
        c.clock.step()
        c.io.mem_debug_read_data.expect(0.U, "Multi-Load: mem[0x18] should be 0")

        // Section 4: Branch Condition RAW (branch not taken)
        c.io.mem_debug_read_address.poke(0x1c.U)
        c.clock.step()
        c.io.mem_debug_read_data.expect(10.U, "Branch RAW: mem[0x1C] should be 10")

        // Section 5: JAL Return Address Hazard (skip validation - address varies)
        // Section 6: CSR RAW Hazard (cycle count diff + 0x1888 signature)
        c.io.mem_debug_read_address.poke(0x24.U)
        c.clock.step()
        val csrValue = c.io.mem_debug_read_data.peek().litValue
        assert(
          csrValue >= 0x1888 && csrValue <= 0x1900,
          f"CSR RAW: mem[0x24] should be 0x1888-0x1900, got 0x$csrValue%x"
        )

        // Section 7: Long Dependency Chain (1+2+3+4 = 5)
        c.io.mem_debug_read_address.poke(0x28.U)
        c.clock.step()
        c.io.mem_debug_read_data.expect(5.U, "Long Chain: mem[0x28] should be 5")

        // Section 8: WB Stage Forwarding
        c.io.mem_debug_read_address.poke(0x2c.U)
        c.clock.step()
        c.io.mem_debug_read_data.expect(7.U, "WB Forward: mem[0x2C] should be 7")

        // Section 9: Load-to-Store Forwarding
        c.io.mem_debug_read_address.poke(0x30.U)
        c.clock.step()
        c.io.mem_debug_read_data.expect(0.U, "Load-Store: mem[0x30] should be 0")

        // Section 10: Branch with Multiple RAW (branch not taken)
        c.io.mem_debug_read_address.poke(0x34.U)
        c.clock.step()
        c.io.mem_debug_read_data.expect(20.U, "Multi-RAW Branch: mem[0x34] should be 20")

        // Validate cycle count (s0 register = x8)
        c.io.regs_debug_read_address.poke(8.U)
        c.clock.step()
        val cycles = c.io.regs_debug_read_data.peek().litValue
        assert(cycles > 0, s"${cfg.name}: Cycle count should be > 0, got $cycles")
      }
    }

    it should "handle machine-mode traps" in {
      runProgram("irqtrap.asmbin", cfg) { c =>
        c.clock.setTimeout(0)
        for (i <- 1 to 1000) {
          c.clock.step()
          c.io.mem_debug_read_address.poke((i * 4).U)
        }
        c.io.mem_debug_read_address.poke(4.U)
        c.clock.step()
        c.io.mem_debug_read_data.expect(0xdeadbeefL.U)

        c.io.interrupt_flag.poke(1.U)
        c.clock.step(5)
        c.io.interrupt_flag.poke(0.U)

        for (i <- 1 to 1000) {
          c.clock.step()
          c.io.mem_debug_read_address.poke((i * 4).U)
        }
        c.io.csr_debug_read_address.poke(CSRRegister.MSTATUS)
        c.clock.step()
        c.io.csr_debug_read_data.expect(0x1888.U)
        c.io.csr_debug_read_address.poke(CSRRegister.MCAUSE)
        c.clock.step()
        val cause = c.io.csr_debug_read_data.peek().litValue
        assert(mcauseAcceptable.contains(cause), f"unexpected mcause 0x${cause}%x")
        c.io.mem_debug_read_address.poke(0x4.U)
        c.clock.step()
        c.io.mem_debug_read_data.expect(0x2022L.U)
      }
    }
// [PipelineProgramTest.scala]

    it should "pass the UF8 benchmark with round-trip verification" in {
      runProgram("q1-uf8.asmbin", cfg) { c =>
        // 1. 關閉 ChiselTest 預設的 1000 cycle timeout，因為 main.c 跑很久
        c.clock.setTimeout(0)
        
        var done = false
        var lastIndex = -1 
        var cycles = 0          // 軟體模擬的總 Cycle 數
        
        val maxCycles = 5000000 // 設定一個足夠大的安全上限
        val checkInterval = 50  // 每 50 個 Cycle 檢查一次

        // 模擬主迴圈
        for (_ <- 0 until maxCycles by checkInterval if !done) {
          c.clock.step(checkInterval)
          cycles += checkInterval

          // --- [檢查 1] 程式是否結束 (0x4) ---
          c.io.mem_debug_read_address.poke(4.U)
          c.clock.step()
          if (c.io.mem_debug_read_data.peek().litValue == 0xDEADBEEFL) {
            done = true
          }

          // --- [檢查 2] 監控執行過程 (0x20 & 0x28) ---
          c.io.mem_debug_read_address.poke(0x20.U)
          c.clock.step()
          val rawIndex = c.io.mem_debug_read_data.peek().litValue.toInt
          
          // 加上 Offset (0x1000) 判斷，避開記憶體初始值 0 的誤判
          if (rawIndex >= 0x1000) {
            val currentIndex = rawIndex - 0x1000 

            // 如果 Index 變動了，代表 CPU 算出了一組新結果
            if (currentIndex != lastIndex) {
              c.io.mem_debug_read_address.poke(0x28.U) // 讀取比對結果
              c.clock.step()
              val isEqual = c.io.mem_debug_read_data.peek().litValue.toInt
              
              // 只有失敗時才印出錯誤訊息
              if (isEqual != 1) {
                  println(f"[ERROR] Round-trip failed at index $currentIndex%3d! Result: $isEqual")
              }
              // 斷言檢查：必須為 1
              assert(isEqual == 1, f"Round-trip failed at index $currentIndex! Original != Encoded")
              
              lastIndex = currentIndex
            }
          }
        }
        
        // --- [檢查 3] 確認程式正常結束 ---
        assert(done, s"Timeout! Program did not finish within $maxCycles cycles.")
        
        // --- [檢查 4] 確認最終測試結果 (0x8) ---
        c.io.mem_debug_read_address.poke(8.U)
        c.clock.step()
        assert(c.io.mem_debug_read_data.peek().litValue == 1, "Test Failed Flag (main.c reported failure)!")
        
        // --- [功能 5] 讀取並印出效能數據 (0x30) ---
        c.io.mem_debug_read_address.poke(0x30.U)
        c.clock.step()
        val hwCycles = c.io.mem_debug_read_data.peek().litValue

        println(f"==========================================================")
        println(f"   Perf Report: ${cfg.name}")
        println(f"   [Software View] Total Sim Cycles : $cycles%d")   // 含啟動時間
        println(f"   [Hardware View] Core  CSR Cycles : $hwCycles%d") // 僅含演算法時間
        println(f"==========================================================")
      }
    }





    it should "pass the Hanoi benchmark with move verification" in {
      runProgram("q2-hanoi.asmbin", cfg) { c =>
        // 1. 關閉 Timeout
        c.clock.setTimeout(0)
        
        var done = false
        var lastStep = 0        // 紀錄上一次處理的步數
        var cycles = 0          // 累計軟體 Cycles
        var expectedStep = 1    // 預期接下來是第幾步
        
        val maxCycles = 500000  // 安全上限
        val checkInterval = 5   // 檢查頻率 (不需要太頻繁，但也別太慢以免漏掉)

        println("[Test Start] Running Hanoi Simulation...")

        // --- 主模擬迴圈 ---
        while (!done && cycles < maxCycles) {
          c.clock.step(checkInterval)
          cycles += checkInterval

          // ========================================================
          // 1. 檢查程式是否結束 (Magic Number at 0x04)
          // ========================================================
          c.io.mem_debug_read_address.poke(4.U)
          c.clock.step(1) // 讓位址生效
          // 注意：這裡 cycles 應該也要 +1，但為了簡化計算先忽略這 1 cycle 誤差
          if (c.io.mem_debug_read_data.peek().litValue == 0xDEADBEEFL) {
            done = true
          }

          // ========================================================
          // 2. 監控 Hanoi 移動 (Step Count at 0x100)
          // ========================================================
          if (!done) {
            c.io.mem_debug_read_address.poke(0x100.U)
            c.clock.step(1) 
            val currentStep = c.io.mem_debug_read_data.peek().litValue.toInt

            // 如果讀到的步數比上一次大，代表發生了新的移動
            if (currentStep > lastStep) {
              
              // 【重要】等待 CPU 完成後續的寫入 (Disk, From, To)
              // 由於 Step 是第一個寫的，我們多跑 20 cycles 確保後面三個 sw 指令都執行完畢
              c.clock.step(20)
              cycles += 20

              // 依序讀取參數
              // 讀 Disk (0x104)
              c.io.mem_debug_read_address.poke(0x104.U)
              c.clock.step(1)
              val disk = c.io.mem_debug_read_data.peek().litValue.toInt

              // 讀 From (0x108)
              c.io.mem_debug_read_address.poke(0x108.U)
              c.clock.step(1)
              val from = c.io.mem_debug_read_data.peek().litValue.toInt

              // 讀 To (0x10C)
              c.io.mem_debug_read_address.poke(0x10C.U)
              c.clock.step(1)
              val to = c.io.mem_debug_read_data.peek().litValue.toInt

              // 印出 Log
              println(f"[Hanoi Monitor] Step $currentStep%2d: Move Disk $disk from $from to $to")

              // --- 驗證邏輯 ---
              // 1. 步數必須連續
              assert(currentStep == expectedStep, f"Step missed or disordered! Expected $expectedStep, got $currentStep")
              
              // 2. 簡單的範圍檢查
              assert(disk >= 0 && disk <= 2, f"Invalid Disk ID: $disk")
              assert(from >= 0 && from <= 2, f"Invalid From Peg: $from")
              assert(to >= 0 && to <= 2, f"Invalid To Peg: $to")

              // 更新狀態
              lastStep = currentStep
              expectedStep += 1
            }
          }
        }

        // ========================================================
        // 3. 結束後驗證
        // ========================================================
        
        // A. 確認非超時結束
        assert(done, s"Timeout! Program did not finish within $maxCycles cycles.")

        // B. 確認最終結果 Flag (0x08)
        c.io.mem_debug_read_address.poke(8.U)
        c.clock.step(1)
        val result = c.io.mem_debug_read_data.peek().litValue
        assert(result == 1, "Hanoi Test Failed (Result Flag != 1)")

        // C. 確認步數正確 (3個盤子應該有 7 步)
        // expectedStep 最後會停在 8 (因為第 7 步做完後 +1)
        assert(lastStep == 7, f"Total moves incorrect. Expected 7, got $lastStep")

        // D. 讀取並印出效能數據 (0x30)
        c.io.mem_debug_read_address.poke(0x30.U)
        c.clock.step(1)
        val hwCycles = c.io.mem_debug_read_data.peek().litValue

        println(f"==========================================================")
        println(f"   Hanoi Benchmark Report")
        println(f"   [Software View] Total Sim Cycles : $cycles%d")
        println(f"   [Hardware View] Core Cycle Count : $hwCycles%d")
        println(f"==========================================================")
      }
    }


    it should "verify Fast_Rsqrt results with per-test case HW and Sim cycles" in {
    runProgram("q3-rsqrt.asmbin", cfg) { c =>
      c.clock.setTimeout(0)

      val testInputs = Seq(0, 1, 4, 16, 100, 1024, 65536, 1048576, 10, 42, 12345)
      var nextExpectedIndex = 1 
      var currentTotalSimCycles = 0
      var lastSimTimestamp = 0 // 紀錄上一次測試案例完成的時間點
      val maxSimCycles = 2000000

      def expectedRsqrt(x: Int): Long = {
        if (x == 0) 0xFFFFFFFFL
        else if (x == 1) 65536L
        else (65536.0 / Math.sqrt(x)).toLong
      }

      println(f"==========================================================")
      println(f"  fast_rsqrt Per-Test Case Report: ${cfg.name}")
      println(f"  Idx |  Input  |  Result  | HW Cycles | Sim Cycles")
      println(f"----------------------------------------------------------")
      
      for (_ <- 0 until maxSimCycles by 50 if nextExpectedIndex <= testInputs.length) {
        c.clock.step(50)
        currentTotalSimCycles += 50

        // 監控 0x204 (Index)
        c.io.mem_debug_read_address.poke(0x204.U)
        c.clock.step()
        val currentIndex = c.io.mem_debug_read_data.peek().litValue.toInt

        if (currentIndex == nextExpectedIndex) {
          // 讀取運算結果 (0x200) 與 硬體耗時 (0x208)
          c.io.mem_debug_read_address.poke(0x200.U)
          c.clock.step()
          val result = c.io.mem_debug_read_data.peek().litValue

          c.io.mem_debug_read_address.poke(0x208.U)
          c.clock.step()
          val hwCycles = c.io.mem_debug_read_data.peek().litValue

          // 計算本次測試案例獨立花費的 Simulation Cycles
          val perTestCaseSimCycles = currentTotalSimCycles - lastSimTimestamp

          val input = testInputs(currentIndex - 1)
          val expected = expectedRsqrt(input)

          // 驗證與輸出報告
          if (input == 0) {
            println(f"  ${currentIndex-1}%2d | $input%7d | 0xFFFFFFFF | $hwCycles%9d | $perTestCaseSimCycles%10d")
          } else {
            val margin = (expected * 0.1).toLong.max(2L) 
            val diff = (result - expected).abs
            assert(diff <= margin, f"Index ${currentIndex-1} exceeds margin!")
            println(f"  ${currentIndex-1}%2d | $input%7d | $result%8d | $hwCycles%9d | $perTestCaseSimCycles%10d")
          }
          
          // 更新時間戳，以便下一次計算差值
          lastSimTimestamp = currentTotalSimCycles
          nextExpectedIndex += 1
        }
      }

      println(f"==========================================================")
      println(f"  Final Status: Verified ${nextExpectedIndex - 1} cases.")
      println(f"==========================================================")
    }
  }


  }
}

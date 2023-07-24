package matrices

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class TestMatrices extends AnyFreeSpec with ChiselScalatestTester {
  def print_mat(matrix: Matrix[UInt]) = {
        println("[")
        for(i <- 0 until matrix.rows) {
          for(j <- 0 until matrix.columns) {
            print(matrix(i)(j).peekInt().toString + ", ")
          }
          println()
        }
        println("]")
  }

  "Basic Operation Monitoring" in {
    // command: $ sbt test
    test(new matrixShenanigans).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val iters: Int = 5

      // TODO how to define handling of Bundle type
      // (without simply addressing it's base types)
      for( _ <- 0 until iters) {
        println("---------------------------")
        print("C = ")
        print_mat(dut.io.C)
        print("I = ")
        print_mat(dut.io.I)
      }
      println("---------------------------")
    }
  }
}

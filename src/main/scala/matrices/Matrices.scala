// MaCH - Matrices in CHISEL Framework
package matrices

import chisel3._

class Matrix[T <: Bits with Num[T]](n: Int, m: Int, t: T) extends Bundle {
  val rows: Int = n
  val columns: Int = m
  val M: Vec[Vec[T]] = Vec(n, Vec(m, t))

  def apply(i: Int): Vec[T] = this.M(i)

  // Matrix Product
  // Rule: Overloaded Operators should 
  // 1. Produce valid result within a clock cycle (no registers)
  // 2. No side-effects
  def *(that: Matrix[T]): Matrix[T] = {
    if(this.columns != that.rows)
      throw new ChiselException("for matrix dot product, columns of the multiplicand must match rows of the multiplier")

    // i.e. this "function" has side effects / memory, not just a series of collections or delays
    val result: Matrix[T] = Wire(new Matrix(this.rows, that.columns, t))
    // weird that this wouldn't work with Seqs, instead of Vecs
    val intermediate_results = Wire(Vec(that.columns, Vec(this.columns, t))) // is it possible to size a Seq at compile time? perhaps i should use something else

    for(i <- 0 until this.rows)
      for(j <- 0 until that.columns) {
        for(k <- 0 until this.columns)
          intermediate_results(j)(k) := (this(i)(k) * that(k)(j))
        result(i)(j) := intermediate_results(j).reduce(_ + _) // sum all results
      }
    result
  }


  // scalar multiplication
  def *(scalar: T): Matrix[T] = {
    val result: Matrix[T] = Wire(new Matrix(this.rows, this.columns, t))

    for(i <- 0 until this.rows)
      for(j <- 0 until this.columns)
        result(i)(j) := scalar * this(i)(j)
    result
  }


  def square: Matrix[T] = {
    val result: Matrix[T] = Wire(new Matrix(this.rows, this.columns, t))
    for(i <- 0 until this.rows)
      for(j <- 0 until this.columns)
        result(i)(j) := this(i)(j) * this(i)(j)
    result
  }


  def transpose: Matrix[T] = {
    val result: Matrix[T] = Wire(new Matrix(this.columns, this.rows, t))
    result.M := VecInit(for { row <- this.M.transpose[T] } yield VecInit(row))
    result
  }


  def +(that: Matrix[T]): Matrix[T] = this.elementwise(that, (x, y) => x + y)
  def -(that: Matrix[T]): Matrix[T] = this.elementwise(that, (x, y) => x - y)

  // TODO define implicit to enable this
  // def &(that: Matrix[T]): Matrix[T] = this.elementwise(that, (x, y) => x & y)
  // def |(that: Matrix[T]): Matrix[T] = this.elementwise(that, (x, y) => x | y)

  def elementwise(that: Matrix[T], op: (T, T) => T): Matrix[T] = {
    if((this.columns != that.columns) || (this.rows != that.rows))
      throw new ChiselException("for elementwise operations, dimensions of operands must match")

    val result: Matrix[T] = Wire(new Matrix(this.rows, this.columns, t))
    for(i <- 0 until this.rows)
      for(j <- 0 until this.columns)
        result(i)(j) := op(this(i)(j), that(i)(j))
    result
  }

  def elementwise(op: T => T): Matrix[T] = {
    val result: Matrix[T] = Wire(new Matrix(this.rows, this.columns, t))
    for(i <- 0 until this.rows)
      for(j <- 0 until this.columns)
        result(i)(j) := op(this(i)(j))
    result
  }
}

// Factory methods
object MatrixInit {
  // Returns Matrix with a default value, defined by Seq[T] data
  def apply[T <: Bits with Num[T]](rows: Int, columns: Int, t: T)(data: Seq[T]): Matrix[T] = {
    val sV: Seq[Vec[T]] = (for { row <- data.grouped(columns)} yield VecInit(row)).toSeq
    val matrix = Wire(new Matrix(rows, columns, t))
    matrix.M := VecInit(sV)
    matrix
  }

  // Returns Matrix with a single default value for all t:T in M
  def apply[T <: Bits with Num[T]](rows: Int, columns: Int, t: T, fillValue: T): Matrix[T] = {
    val matrix = Wire(new Matrix(rows, columns, t))
    matrix.M := VecInit.fill(rows, columns)(fillValue)
    matrix
  }

  // Square Matrix wrappers
  def apply[T <: Bits with Num[T]](n: Int, t: T, fillValue: T): Matrix[T] = {
    apply(n, n, t, fillValue)
  }

  def apply[T <: Bits with Num[T]](n: Int, t: T)(data: Seq[T]): Matrix[T] = {
    apply(n, n, t)(data)
  }

  def identity[T <: Bits with Num[T]](n: Int, t: T): Matrix[T] = {
    diagonal(n, 1, t)
  }

  // TODO test with negative values
  def diagonal[T <: Bits with Num[T]](n: Int, x: Int, t: T): Matrix[T] = {
    val matrix = Wire(new Matrix(n, n, t))
    matrix.M := VecInit.tabulate(n, n)( (i, j) => if (i == j) x.S.asTypeOf(t.cloneType) else 0.S.asTypeOf(t.cloneType))
    matrix
  }

  def rand[T <: Bits with Num[T]](n: Int, m: Int, t: T): Matrix[T] = {
    val matrix = Wire(new Matrix(n, m, t))
    val rand = scala.util.Random
    val upper_bound: Int = 10
    for(i <- 0 until matrix.rows)
      for(j <- 0 until matrix.columns)
        matrix(i)(j) := rand.nextInt(upper_bound).S.asTypeOf(t.cloneType)// assign random value to each of these
    matrix
  }
}

class matrixShenanigans extends Module {
  val io = IO(new Bundle {
    // i.e 2D matrix where elements are some numeric hardware type
    val C = Output(new Matrix(1, 2, UInt(8.W)))
    val I = Output(new Matrix(8, 8, UInt(8.W)))
  })

  val incr = RegInit(0.U(8.W))

  incr := incr + 1.U

  // Matrices with Default values
  // Maybe this should be curried so that all the setup is in one () and the initial vlaue is in the nextone
  val W = MatrixInit(2, UInt(8.W))(1.U(8.W) :: 1.U(8.W) ::
                                   2.U(8.W) :: 3.U(8.W) :: 
                                   Nil)
  val B = MatrixInit(1, 2, UInt(8.W))(2.U(8.W) :: 4.U(8.W) :: Nil)
  val X = MatrixInit(1, 2, UInt(8.W), 1.U(8.W))
  
  // A single layer of a neural network can be described in this way
  io.I := MatrixInit.identity(io.I.rows, UInt(8.W))

  io.C := (X * W) + B
}


// command: $ sbt run
object emitMatricesVerilog extends App {
  emitVerilog (new matrixShenanigans, Array("--target-dir", "generated/chisel"))
}

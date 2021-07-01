class Rational(x: Int, y: Int) {
    require(y > 0, "denom must be positive")
    private def gcd(a: Int, b: Int): Int =
      if (b == 0) {println(a); a} else gcd(b, a % b)
    val numer = x
    val denom = y
  
    def add(r: Rational) =
      new Rational(numer * r.denom + r.numer * denom, denom * r.denom)
  
    def mul(r: Rational) =
      new Rational(numer * r.numer, denom * r.denom)
  
    def neg = new Rational(-numer, denom)

    def sub(r: Rational) = add(r.neg)

    override def toString = s"${numer/ gcd(x.abs, y)}/${denom/ gcd(x.abs, y)}"
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x add y mul z
x.neg
y sub x
x.sub(y).sub(z)
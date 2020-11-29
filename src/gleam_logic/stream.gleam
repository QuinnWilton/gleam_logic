pub type Stream(a) {
  Empty
  Mature(a, Stream(a))
  Immature(fn() -> Stream(a))
}

pub fn pull(s: Stream(a)) -> Stream(a) {
  case s {
    Empty -> Empty
    Mature(x, xs) -> Mature(x, xs)
    Immature(f) -> pull(f())
  }
}

pub fn take_all(s: Stream(a)) -> List(a) {
  case pull(s) {
    Empty -> []
    Mature(x, xs) -> [x, ..take_all(xs)]
  }
}

pub fn take(n: Int, s: Stream(a)) -> List(a) {
  case n {
    0 -> []
    n ->
      case pull(s) {
        Empty -> []
        Mature(x, xs) -> [x, ..take(n - 1, xs)]
      }
  }
}

pub fn mzero() -> Stream(a) {
  Empty
}

pub fn unit(x: a) -> Stream(a) {
  Mature(x, Empty)
}

pub fn mplus(s1: Stream(a), s2: Stream(a)) {
  case s1 {
    Empty -> s2
    Mature(x, xs) -> Mature(x, mplus(xs, s2))
    Immature(f) -> Immature(fn() { mplus(s2, f()) })
  }
}

pub fn bind(s1: Stream(a), g: fn(a) -> Stream(a)) -> Stream(a) {
  case s1 {
    Empty -> mzero()
    Mature(x, xs) -> mplus(g(x), bind(xs, g))
    Immature(f) -> Immature(fn() { bind(f(), g) })
  }
}

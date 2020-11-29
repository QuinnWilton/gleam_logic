import gleam/map
import gleam/option

pub type Term {
  Symbol(String)
  Variable(VariableIndex)
  Pair(Term, Term)
}

pub type Stream(a) {
  Empty
  Mature(a, Stream(a))
  Immature(fn() -> Stream(a))
}

pub type VariableIndex =
  Int

pub type Substitution =
  map.Map(VariableIndex, Term)

pub type State {
  State(substitution: Substitution, counter: Int)
}

pub type Goal =
  fn(State) -> Stream(State)

pub fn empty_state() -> State {
  State(map.new(), 0)
}

pub fn walk(u: Term, s: Substitution) -> Term {
  case u {
    Variable(index) ->
      case map.get(s, index) {
        Error(Nil) -> Variable(index)
        Ok(v) -> walk(v, s)
      }
    _ -> u
  }
}

pub fn extend_substitution(
  index: VariableIndex,
  u: Term,
  s: Substitution,
) -> Substitution {
  map.insert(s, index, u)
}

pub fn unify(u: Term, v: Term, s: Substitution) -> option.Option(Substitution) {
  let u = walk(u, s)
  let v = walk(v, s)

  case tuple(u, v) {
    tuple(Variable(u_index), Variable(v_index)) if u_index == v_index ->
      option.Some(s)
    tuple(Variable(index), _) -> option.Some(extend_substitution(index, v, s))
    tuple(_, Variable(index)) -> option.Some(extend_substitution(index, u, s))
    tuple(Pair(u1, u2), Pair(v1, v2)) ->
      case unify(u1, v1, s) {
        option.None -> option.None
        option.Some(s) -> unify(u2, v2, s)
      }
    tuple(Symbol(u_name), Symbol(v_name)) if u_name == v_name -> option.Some(s)
    _ -> option.None
  }
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

pub fn take_n(n: Int, s: Stream(a)) -> List(a) {
  case n {
    0 -> []
    n ->
      case pull(s) {
        Empty -> []
        Mature(x, xs) -> [x, ..take_n(n - 1, xs)]
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

pub fn zzz(g: Goal) -> Goal {
  fn(a) { Immature(fn() { g(a) }) }
}

pub fn equal(u: Term, v: Term) -> Goal {
  fn(state: State) {
    case unify(u, v, state.substitution) {
      option.None -> mzero()
      option.Some(s) -> unit(State(..state, substitution: s))
    }
  }
}

pub fn call_fresh(f: fn(Term) -> Goal) -> Goal {
  fn(state: State) {
    let u = Variable(state.counter)

    f(u)(State(..state, counter: state.counter + 1))
  }
}

pub fn disj(g1: Goal, g2: Goal) -> Goal {
  fn(state) {
    let s1 = g1(state)
    let s2 = g2(state)

    mplus(s1, s2)
  }
}

pub fn conj(g1: Goal, g2: Goal) -> Goal {
  fn(state) {
    let s = g1(state)

    bind(s, g2)
  }
}

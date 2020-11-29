import gleam/int
import gleam/list
import gleam/map
import gleam/option
import gleam/string
import gleam_logic/stream

pub type Term {
  Symbol(String)
  Variable(VariableIndex)
  Pair(Term, Term)
}

pub type VariableIndex =
  Int

pub type Substitution =
  map.Map(VariableIndex, Term)

pub type State {
  State(substitution: Substitution, counter: Int)
}

pub type Goal =
  fn(State) -> stream.Stream(State)

pub fn empty_state() -> State {
  State(map.new(), 0)
}

pub fn call_empty_state(g: Goal) -> stream.Stream(State) {
  g(empty_state())
}

pub fn run(n: Int, g: Goal) {
  mk_reify(stream.take(n, call_empty_state(g)))
}

pub fn run_all(g: Goal) {
  mk_reify(stream.take_all(call_empty_state(g)))
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

pub fn walk_all(u: Term, s: Substitution) -> Term {
  case walk(u, s) {
    Variable(_) -> u
    Pair(u1, u2) -> Pair(walk_all(u1, s), walk_all(u2, s))
    u -> u
  }
}

pub fn extend_substitution(
  index: VariableIndex,
  u: Term,
  s: Substitution,
) -> option.Option(Substitution) {
  case occurs_check(index, u, s) {
    True -> option.None
    False -> option.Some(map.insert(s, index, u))
  }
}

pub fn occurs_check(index: VariableIndex, v: Term, s: Substitution) -> Bool {
  case walk(v, s) {
    Variable(v_index) -> index == v_index
    Pair(v1, v2) -> occurs_check(index, v1, s) || occurs_check(index, v2, s)
    _ -> False
  }
}

pub fn unify(u: Term, v: Term, s: Substitution) -> option.Option(Substitution) {
  let u = walk(u, s)
  let v = walk(v, s)

  case tuple(u, v) {
    tuple(Variable(u_index), Variable(v_index)) if u_index == v_index ->
      option.Some(s)
    tuple(Variable(index), _) -> extend_substitution(index, v, s)
    tuple(_, Variable(index)) -> extend_substitution(index, u, s)
    tuple(Pair(u1, u2), Pair(v1, v2)) ->
      case unify(u1, v1, s) {
        option.None -> option.None
        option.Some(s) -> unify(u2, v2, s)
      }
    tuple(Symbol(u_name), Symbol(v_name)) if u_name == v_name -> option.Some(s)
    _ -> option.None
  }
}

pub fn zzz(g: Goal) -> Goal {
  fn(a) { stream.Immature(fn() { g(a) }) }
}

pub fn conj_all(gs: List(Goal)) -> Goal {
  case gs {
    [g] -> zzz(g)
    [g, ..gs] -> conj(zzz(g), conj_all(gs))
  }
}

pub fn disj_all(gs: List(Goal)) -> Goal {
  case gs {
    [g] -> zzz(g)
    [g, ..gs] -> disj(zzz(g), disj_all(gs))
  }
}

pub fn conde(gs: List(List(Goal))) -> Goal {
  gs
  |> list.map(conj_all)
  |> disj_all()
}

pub fn equal(u: Term, v: Term) -> Goal {
  fn(state: State) {
    case unify(u, v, state.substitution) {
      option.None -> stream.mzero()
      option.Some(s) -> stream.unit(State(..state, substitution: s))
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

    stream.mplus(s1, s2)
  }
}

pub fn conj(g1: Goal, g2: Goal) -> Goal {
  fn(state) {
    let s = g1(state)

    stream.bind(s, g2)
  }
}

pub fn mk_reify(states: List(State)) {
  list.map(states, reify_state_first_variable)
}

pub fn reify_state_first_variable(state: State) {
  let v = walk_all(Variable(0), state.substitution)

  walk_all(v, reify_substitution(v, map.new()))
}

pub fn reify_substitution(u: Term, s: Substitution) {
  case walk(u, s) {
    Variable(index) -> {
      let n = reify_name(map.size(s))
      case extend_substitution(index, n, s) {
        option.None -> s
        option.Some(s) -> s
      }
    }
    Pair(u1, u2) -> reify_substitution(u2, reify_substitution(u1, s))
    _ -> s
  }
}

pub fn reify_name(n: Int) {
  Symbol(string.append("_.", int.to_string(n)))
}

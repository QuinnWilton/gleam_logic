import gleam_logic
import gleam/map
import gleam/should

fn fives(x) {
  gleam_logic.disj(
    gleam_logic.equal(x, gleam_logic.Symbol("5")),
    gleam_logic.zzz(fn(state) { fives(x)(state) }),
  )
}

pub fn fives_test() {
  let stream = gleam_logic.call_fresh(fives)(gleam_logic.empty_state())
  let [state] = gleam_logic.take_n(1, stream)
  let result = map.get(state.substitution, 0)

  should.equal(result, Ok(gleam_logic.Symbol("5")))
}

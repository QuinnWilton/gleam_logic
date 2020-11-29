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
  let result = gleam_logic.run(1, gleam_logic.call_fresh(fives))

  should.equal(result, [gleam_logic.Symbol("5")])
}

pub fn disj_all_test() {
  let result =
    gleam_logic.run_all(gleam_logic.call_fresh(fn(x) {
      gleam_logic.disj_all([
        gleam_logic.equal(x, gleam_logic.Symbol("2")),
        gleam_logic.equal(x, gleam_logic.Symbol("1")),
        gleam_logic.equal(x, gleam_logic.Symbol("8")),
      ])
    }))

  should.equal(
    result,
    [gleam_logic.Symbol("2"), gleam_logic.Symbol("1"), gleam_logic.Symbol("8")],
  )
}

pub fn conde_test() {
  let result =
    gleam_logic.run_all(gleam_logic.call_fresh(fn(x) {
      gleam_logic.call_fresh(fn(y) {
        gleam_logic.conde([
          [
            gleam_logic.equal(x, gleam_logic.Symbol("5")),
            gleam_logic.equal(y, gleam_logic.Symbol("3")),
          ],
          [
            gleam_logic.equal(x, gleam_logic.Symbol("8")),
            gleam_logic.equal(y, gleam_logic.Symbol("2")),
          ],
        ])
      })
    }))

  should.equal(result, [gleam_logic.Symbol("5"), gleam_logic.Symbol("8")])
}

#[cfg(test)]
mod tests {
  use float_cmp::approx_eq;
  use math_parser::generate_lambda;
  use std::f64::consts::{E, PI};

  const EPSILON: f64 = 1e-10;

  fn assert_float_eq(a: f64, b: f64) {
    assert!(
      approx_eq!(f64, a, b, epsilon = EPSILON),
      "Expected {}, got {}",
      b,
      a
    );
  }

  #[test]
  fn test_basic_operations() {
    let add_lambda = generate_lambda("x + 5").unwrap();
    assert_float_eq(add_lambda(3.0).unwrap(), 8.0);
    let sub_lambda = generate_lambda("x - 7").unwrap();
    assert_float_eq(sub_lambda(10.0).unwrap(), 3.0);
    let mul_lambda = generate_lambda("x * 2").unwrap();
    assert_float_eq(mul_lambda(4.0).unwrap(), 8.0);
    let div_lambda = generate_lambda("x / 4").unwrap();
    assert_float_eq(div_lambda(12.0).unwrap(), 3.0);
    let pow_lambda = generate_lambda("x ^ 2").unwrap();
    assert_float_eq(pow_lambda(3.0).unwrap(), 9.0);
  }

  #[test]
  fn test_complex_expressions() {
    let nested_lambda = generate_lambda("(x + 2) * (x - 3)").unwrap();
    assert_float_eq(nested_lambda(5.0).unwrap(), 14.0);
    let complex_lambda = generate_lambda("x/10+2*x-5").unwrap();
    assert_float_eq(complex_lambda(3.0).unwrap(), 3.0 / 10.0 + 2.0 * 3.0 - 5.0);
    let order_lambda = generate_lambda("2 + 3 * x").unwrap();
    assert_float_eq(order_lambda(4.0).unwrap(), 14.0);
  }

  #[test]
  fn test_variable_replacement() {
    let lambda = generate_lambda("x + x * x").unwrap();
    assert_float_eq(lambda(2.0).unwrap(), 2.0 + 2.0 * 2.0);
    let const_lambda = generate_lambda("5 * 3 + 1").unwrap();
    assert_float_eq(const_lambda(100.0).unwrap(), 16.0);
  }

  #[test]
  fn test_edge_cases() {
    assert!(generate_lambda("").is_err());
    let identity_lambda = generate_lambda("x").unwrap();
    assert_float_eq(identity_lambda(42.0).unwrap(), 42.0);
    let constant_lambda = generate_lambda("5").unwrap();
    assert_float_eq(constant_lambda(100.0).unwrap(), 5.0);
    let whitespace_lambda = generate_lambda("  x  +  5  ").unwrap();
    assert_float_eq(whitespace_lambda(3.0).unwrap(), 8.0);
    let large_lambda = generate_lambda("x * 10000000000000.0").unwrap();
    assert_float_eq(large_lambda(2.0).unwrap(), 20000000000000.0);
    let small_lambda = generate_lambda("x * 0.00000000000001").unwrap();
    assert_float_eq(small_lambda(2.0).unwrap(), 0.00000000000002);
  }

  #[test]
  fn test_invalid_expression_syntax_handling() {
    assert!(generate_lambda("x +* 2").is_err());
    assert!(generate_lambda("(x + 2").is_err());
  }

  #[test]
  fn test_division_by_zero_handling() {
    let div_zero_lambda = generate_lambda("1/x").unwrap();
    assert!(div_zero_lambda(0.0).is_err());
  }

  #[test]
  fn test_mathematical_functions() {
    let sin_lambda = generate_lambda("sin(x)").unwrap();
    assert_float_eq(sin_lambda(PI / 2.0).unwrap(), 1.0);
    let cos_lambda = generate_lambda("cos(x)").unwrap();
    assert_float_eq(cos_lambda(0.0).unwrap(), 1.0);
    let tan_lambda = generate_lambda("tan(x)").unwrap();
    assert_float_eq(tan_lambda(PI / 4.0).unwrap(), 1.0);
    let ln_lambda = generate_lambda("ln(x)").unwrap();
    assert_float_eq(ln_lambda(E).unwrap(), 1.0);
    let log10_lambda = generate_lambda("log10(x)").unwrap();
    assert_float_eq(log10_lambda(100.0).unwrap(), 2.0);
    let sqrt_lambda = generate_lambda("sqrt(x)").unwrap();
    assert_float_eq(sqrt_lambda(16.0).unwrap(), 4.0);
    let abs_lambda = generate_lambda("abs(x)").unwrap();
    assert_float_eq(abs_lambda(-5.0).unwrap(), 5.0);
  }

  #[test]
  fn test_combined_functions() {
    let combined_lambda = generate_lambda("sin(x)^2 + cos(x)^2").unwrap();
    assert_float_eq(combined_lambda(PI / 3.0).unwrap(), 1.0);
    let complex_lambda = generate_lambda("2 * sin(x) + log10(x^2)").unwrap();
    assert_float_eq(complex_lambda(10.0).unwrap(), 2.0 * (10.0_f64.sin()) + 2.0);
  }

  #[test]
  fn test_basic_constants() {
    let pi_lambda = generate_lambda("pi").unwrap();
    assert_float_eq(pi_lambda(0.0).unwrap(), PI);
    let pi_lambda = generate_lambda("x + pi").unwrap();
    assert_float_eq(pi_lambda(5.0).unwrap(), 5.0 + PI);
    let pi_lambda = generate_lambda("pi + x").unwrap();
    assert_float_eq(pi_lambda(5.0).unwrap(), 5.0 + PI);
    let pi_lambda = generate_lambda("pi + 5").unwrap();
    assert_float_eq(pi_lambda(5.0).unwrap(), 5.0 + PI);
    let pi_lambda = generate_lambda("5 + pi").unwrap();
    assert_float_eq(pi_lambda(5.0).unwrap(), 5.0 + PI);
    let e_lambda = generate_lambda("e").unwrap();
    assert_float_eq(e_lambda(0.0).unwrap(), E);
    let e_lambda = generate_lambda("x + e").unwrap();
    assert_float_eq(e_lambda(5.0).unwrap(), 5.0 + E);
    let e_lambda = generate_lambda("e + x").unwrap();
    assert_float_eq(e_lambda(5.0).unwrap(), 5.0 + E);
    let e_lambda = generate_lambda("e + 5").unwrap();
    assert_float_eq(e_lambda(0.0).unwrap(), 5.0 + E);
    let e_lambda = generate_lambda("5 + e").unwrap();
    assert_float_eq(e_lambda(0.0).unwrap(), 5.0 + E);
  }

  #[test]
  fn test_complex_constants() {
    let pi_lambda = generate_lambda("sin(pi/2)").unwrap();
    assert_float_eq(pi_lambda(0.0).unwrap(), 1.0);
    let e_lambda = generate_lambda("ln(e)").unwrap();
    assert_float_eq(e_lambda(0.0).unwrap(), 1.0);
  }

  #[test]
  fn test_undefined_operations_handling() {
    let sqrt_neg_lambda = generate_lambda("sqrt(x)").unwrap();
    assert!(sqrt_neg_lambda(-1.0).unwrap().is_nan());
    let log_neg_lambda = generate_lambda("ln(x)").unwrap();
    assert!(log_neg_lambda(-1.0).unwrap().is_nan());
  }
}

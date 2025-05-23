#[cfg(test)]
mod tests {
  use math_parser::MathExpression;
  use std::f64::consts::{E, PI};

  const EPSILON: f64 = 1e-10;

  fn assert_float_eq(actual: f64, expected: f64, msg: &str) {
    assert!(
      (actual - expected).abs() < EPSILON,
      "{}: expected {}, got {} (diff: {})",
      msg,
      expected,
      actual,
      (actual - expected).abs()
    );
  }

  fn assert_calculation_eq(expr: &MathExpression, x: f64, expected: f64, msg: &str) {
    match expr.calculate(x) {
      Ok(result) => assert_float_eq(result, expected, msg),
      Err(e) => panic!("{}: calculation failed with error: {}", msg, e),
    }
  }

  #[test]
  fn test_basic_arithmetic_operations() {
    let add_expr = MathExpression::new("x + 5").unwrap();
    assert_calculation_eq(&add_expr, 3.0, 8.0, "Addition");

    let sub_expr = MathExpression::new("x - 7").unwrap();
    assert_calculation_eq(&sub_expr, 10.0, 3.0, "Subtraction");

    let mul_expr = MathExpression::new("x * 2").unwrap();
    assert_calculation_eq(&mul_expr, 4.0, 8.0, "Multiplication");

    let div_expr = MathExpression::new("x / 4").unwrap();
    assert_calculation_eq(&div_expr, 12.0, 3.0, "Division");

    let pow_expr = MathExpression::new("x ^ 2").unwrap();
    assert_calculation_eq(&pow_expr, 3.0, 9.0, "Exponentiation");
  }

  #[test]
  fn test_complex_expressions() {
    let nested_expr = MathExpression::new("(x + 2) * (x - 3)").unwrap();
    assert_calculation_eq(&nested_expr, 5.0, 14.0, "Nested expression");

    let complex_expr = MathExpression::new("x/10+2*x-5").unwrap();
    let expected = 3.0 / 10.0 + 2.0 * 3.0 - 5.0;
    assert_calculation_eq(&complex_expr, 3.0, expected, "Complex expression");

    let order_expr = MathExpression::new("2 + 3 * x").unwrap();
    assert_calculation_eq(&order_expr, 4.0, 14.0, "Order of operations");

    let mixed_expr = MathExpression::new("2 * x + 5").unwrap();
    assert_calculation_eq(&mixed_expr, 0.0, 5.0, "Mixed operations");
  }

  #[test]
  fn test_variable_and_constants() {
    let var_expr = MathExpression::new("x + x * x").unwrap();
    assert_calculation_eq(&var_expr, 2.0, 6.0, "Multiple variable usage");

    let identity_expr = MathExpression::new("x").unwrap();
    assert_calculation_eq(&identity_expr, 42.0, 42.0, "Identity");

    let constant_expr = MathExpression::new("5").unwrap();
    assert_calculation_eq(&constant_expr, 100.0, 5.0, "Constant");

    let no_var_expr = MathExpression::new("5 * 3 + 1").unwrap();
    assert_calculation_eq(&no_var_expr, 100.0, 16.0, "No variable expression");
  }

  #[test]
  fn test_mathematical_constants() {
    let pi_expr = MathExpression::new("pi").unwrap();
    assert_calculation_eq(&pi_expr, 0.0, PI, "Pi constant");

    let pi_add_expr = MathExpression::new("x + pi").unwrap();
    assert_calculation_eq(&pi_add_expr, 5.0, 5.0 + PI, "Pi addition");

    let pi_mul_expr = MathExpression::new("pi * x").unwrap();
    assert_calculation_eq(&pi_mul_expr, 2.0, PI * 2.0, "Pi multiplication");

    let e_expr = MathExpression::new("e").unwrap();
    assert_calculation_eq(&e_expr, 0.0, E, "E constant");

    let e_add_expr = MathExpression::new("x + e").unwrap();
    assert_calculation_eq(&e_add_expr, 5.0, 5.0 + E, "E addition");

    let both_constants = MathExpression::new("pi + e").unwrap();
    assert_calculation_eq(&both_constants, 0.0, PI + E, "Both constants");
  }

  #[test]
  fn test_trigonometric_functions() {
    let sin_expr = MathExpression::new("sin(x)").unwrap();
    assert_calculation_eq(&sin_expr, PI / 2.0, 1.0, "Sine function");
    assert_calculation_eq(&sin_expr, 0.0, 0.0, "Sine at zero");

    let cos_expr = MathExpression::new("cos(x)").unwrap();
    assert_calculation_eq(&cos_expr, 0.0, 1.0, "Cosine function");
    assert_calculation_eq(&cos_expr, PI, -1.0, "Cosine at pi");

    let tan_expr = MathExpression::new("tan(x)").unwrap();
    assert_calculation_eq(&tan_expr, PI / 4.0, 1.0, "Tangent function");

    let identity_expr = MathExpression::new("sin(x)^2 + cos(x)^2").unwrap();
    assert_calculation_eq(&identity_expr, PI / 3.0, 1.0, "Trigonometric identity");
    assert_calculation_eq(&identity_expr, 1.5, 1.0, "Trigonometric identity at 1.5");
  }

  #[test]
  fn test_logarithmic_functions() {
    let ln_expr = MathExpression::new("ln(x)").unwrap();
    assert_calculation_eq(&ln_expr, 1.0, 0.0, "Natural log of 1");
    assert_calculation_eq(&ln_expr, E, 1.0, "Natural logarithm");
    assert_calculation_eq(
      &ln_expr,
      10.0,
      (10.0 as f64).ln(),
      "Natural logarithm of 10",
    );

    let log10_expr = MathExpression::new("log10(x)").unwrap();
    assert_calculation_eq(&log10_expr, 1.0, 0.0, "Log10 of 1");
    assert_calculation_eq(&log10_expr, 10.0, 1.0, "Log10 of 10");
    assert_calculation_eq(&log10_expr, 100.0, 2.0, "Log10 of 100");

    let log1_5_expr = MathExpression::new("log1.5(x)").unwrap();
    assert_calculation_eq(&log1_5_expr, 1.0, 0.0, "Log1.5 of 1");
    assert_calculation_eq(&log1_5_expr, 1.5, 1.0, "Log1.5 of 1.5");
    assert_calculation_eq(&log1_5_expr, 10.0, (10.0 as f64).log(1.5), "Log1.5 of 10");

    let logx_expr = MathExpression::new("logx(1)").unwrap();
    assert_calculation_eq(&logx_expr, 5.0, 0.0, "LogX5 of 1");
    let logx_expr = MathExpression::new("logx(x)").unwrap();
    assert_calculation_eq(&logx_expr, 5.0, 1.0, "LogX5 of 5");
    let logx_expr = MathExpression::new("logx(10)").unwrap();
    assert_calculation_eq(&logx_expr, 5.0, (10.0 as f64).log(5.0), "LogX5 of 10");
  }

  #[test]
  fn test_other_mathematical_functions() {
    let sqrt_expr = MathExpression::new("sqrt(x)").unwrap();
    assert_calculation_eq(&sqrt_expr, 16.0, 4.0, "Square root");
    assert_calculation_eq(&sqrt_expr, 9.0, 3.0, "Square root of 9");
    assert_calculation_eq(&sqrt_expr, 1.0, 1.0, "Square root of 1");

    let abs_expr = MathExpression::new("abs(x)").unwrap();
    assert_calculation_eq(&abs_expr, -5.0, 5.0, "Absolute value of negative");
    assert_calculation_eq(&abs_expr, 5.0, 5.0, "Absolute value of positive");
    assert_calculation_eq(&abs_expr, 0.0, 0.0, "Absolute value of zero");
  }

  #[test]
  fn test_combined_functions() {
    let combined_expr = MathExpression::new("2 * sin(x) + log10(x^2)").unwrap();
    let x: f64 = 10.0;
    let expected = 2.0 * x.sin() + (x * x).log10();
    assert_calculation_eq(&combined_expr, x, expected, "Combined functions");

    let complex_expr = MathExpression::new("sqrt(abs(x)) + ln(e^x)").unwrap();
    let x: f64 = 4.0;
    let expected = x.abs().sqrt() + (E.powf(x)).ln();
    assert_calculation_eq(&complex_expr, x, expected, "Complex combined functions");
  }

  #[test]
  fn test_unary_minus() {
    let neg_expr = MathExpression::new("-x").unwrap();
    assert_calculation_eq(&neg_expr, 5.0, -5.0, "Unary minus");

    let neg_complex = MathExpression::new("-(x + 5)").unwrap();
    assert_calculation_eq(&neg_complex, 3.0, -8.0, "Unary minus with expression");

    let double_neg = MathExpression::new("-(-x)").unwrap();
    assert_calculation_eq(&double_neg, 5.0, 5.0, "Double negative");
  }

  #[test]
  fn test_whitespace_handling() {
    let spaced_expr = MathExpression::new("  x  +  5  ").unwrap();
    assert_calculation_eq(&spaced_expr, 3.0, 8.0, "Whitespace handling");

    let tabs_expr = MathExpression::new("\tx\t*\t2\t").unwrap();
    assert_calculation_eq(&tabs_expr, 4.0, 8.0, "Tab handling");

    let mixed_space = MathExpression::new(" sin( x ) + cos( x ) ").unwrap();
    let x = PI / 4.0;
    let expected = x.sin() + x.cos();
    assert_calculation_eq(&mixed_space, x, expected, "Mixed whitespace");
  }

  #[test]
  fn test_edge_cases() {
    let large_expr = MathExpression::new("x * 10000000000000.0").unwrap();
    assert_calculation_eq(&large_expr, 2.0, 20000000000000.0, "Large numbers");

    let small_expr = MathExpression::new("x * 0.00000000000001").unwrap();
    assert_calculation_eq(&small_expr, 2.0, 0.00000000000002, "Small numbers");

    let zero_expr = MathExpression::new("x * 0").unwrap();
    assert_calculation_eq(&zero_expr, 1000.0, 0.0, "Multiplication by zero");
  }

  #[test]
  fn test_error_cases() {
    assert!(
      MathExpression::new("").is_err(),
      "Empty expression should fail"
    );
    assert!(
      MathExpression::new("   ").is_err(),
      "Whitespace-only expression should fail"
    );

    assert!(
      MathExpression::new("x +* 2").is_err(),
      "Invalid operator sequence"
    );
    assert!(
      MathExpression::new("(x + 2").is_err(),
      "Unmatched parenthesis"
    );
    assert!(
      MathExpression::new("x + 2)").is_err(),
      "Extra closing parenthesis"
    );
    assert!(MathExpression::new("2 3").is_err(), "Missing operator");

    assert!(MathExpression::new("y + 2").is_err(), "Unknown variable");
    assert!(
      MathExpression::new("unknown_func(x)").is_err(),
      "Unknown function"
    );
  }

  #[test]
  fn test_runtime_errors() {
    let div_zero_expr = MathExpression::new("1/x").unwrap();
    assert!(
      div_zero_expr.calculate(0.0).is_err(),
      "Division by zero should fail"
    );

    let complex_div_zero = MathExpression::new("x/(x-5)").unwrap();
    assert!(
      complex_div_zero.calculate(5.0).is_err(),
      "Complex division by zero should fail"
    );
  }

  #[test]
  fn test_special_float_values() {
    let sqrt_neg_expr = MathExpression::new("sqrt(x)").unwrap();
    let result = sqrt_neg_expr.calculate(-1.0).unwrap();
    assert!(result.is_nan(), "Square root of negative should be NaN");

    let ln_neg_expr = MathExpression::new("ln(x)").unwrap();
    let result = ln_neg_expr.calculate(-1.0).unwrap();
    assert!(result.is_nan(), "Natural log of negative should be NaN");

    let log10_neg_expr = MathExpression::new("log10(x)").unwrap();
    let result = log10_neg_expr.calculate(-1.0).unwrap();
    assert!(result.is_nan(), "Log10 of negative should be NaN");
  }

  #[test]
  fn test_display_traits() {
    let expr = MathExpression::new("2*x + 5").unwrap();
    assert_eq!(
      expr.to_string(),
      "2*x + 5",
      "Display trait shows original formula"
    );
    assert_eq!(expr.formula(), "2*x + 5", "Formula method returns original");
    assert_eq!(
      expr.normalized_formula(),
      "((2*x)+5)",
      "Normalized formula method is valid"
    );
  }

  #[test]
  fn test_complex_mathematical_expressions() {
    let poly_expr = MathExpression::new("x^3 - 2*(x^2) + x - 1").unwrap();
    let x: f64 = 2.0;
    let expected = x.powf(3.0) - 2.0 * x.powf(2.0) + x - 1.0;
    assert_calculation_eq(&poly_expr, x, expected, "Polynomial expression");

    let rational_expr = MathExpression::new("(x^2 + 1)/(x - 1)").unwrap();
    let x = 3.0;
    let expected = (x * x + 1.0) / (x - 1.0);
    assert_calculation_eq(&rational_expr, x, expected, "Rational function");

    let exp_log_expr = MathExpression::new("ln(x^2) + sqrt(x)").unwrap();
    let x: f64 = 4.0;
    let expected = (x * x).ln() + x.sqrt();
    assert_calculation_eq(&exp_log_expr, x, expected, "Exponential and logarithmic");
  }

  #[test]
  fn test_trigonometric_combinations() {
    let trig_expr = MathExpression::new("sin(x) + cos(x + pi/2)").unwrap();
    let x = PI / 6.0;
    let expected = x.sin() + (x + PI / 2.0).cos();
    assert_calculation_eq(&trig_expr, x, expected, "Trigonometric combination");

    let complex_trig = MathExpression::new("sin(2*x) - 2*sin(x)*cos(x)").unwrap();
    let x = PI / 8.0;
    let expected = (2.0 * x).sin() - 2.0 * x.sin() * x.cos();
    assert_float_eq(
      complex_trig.calculate(x).unwrap(),
      expected,
      "Complex trigonometric identity",
    );
  }

  #[test]
  fn test_nested_function_calls() {
    let nested_expr = MathExpression::new("sin(cos(x))").unwrap();
    let x = PI / 4.0;
    let expected = x.cos().sin();
    assert_calculation_eq(&nested_expr, x, expected, "Nested function calls");

    let deeply_nested = MathExpression::new("sqrt(abs(sin(x)))").unwrap();
    let x = -PI / 6.0;
    let expected = x.sin().abs().sqrt();
    assert_calculation_eq(&deeply_nested, x, expected, "Deeply nested functions");
  }

  #[test]
  fn test_operator_precedence() {
    let expr1 = MathExpression::new("1 + 2*x").unwrap();
    let expr2 = MathExpression::new("1 + (2*x)").unwrap();
    assert_calculation_eq(&expr1, 3.0, 7.0, "Precedence: 1 + 2*x");
    assert_calculation_eq(&expr2, 3.0, 7.0, "Explicit precedence: 1 + (2*x)");
  }

  #[test]
  fn test_constants_in_expressions() {
    let pi_trig = MathExpression::new("sin(pi*x)").unwrap();
    assert_calculation_eq(
      &pi_trig,
      0.5,
      (PI * 0.5).sin(),
      "Pi in trigonometric function",
    );

    let e_exp = MathExpression::new("ln(e*x)").unwrap();
    let x = 2.0;
    let expected = (E * x).ln();
    assert_calculation_eq(&e_exp, x, expected, "E in logarithmic function");

    let both_const = MathExpression::new("pi*e + x").unwrap();
    let expected = PI * E + x;
    assert_calculation_eq(&both_const, x, expected, "Both constants together");
  }

  #[test]
  fn test_decimal_numbers() {
    let decimal_expr = MathExpression::new("3.14159*x + 2.71828").unwrap();
    let x = 1.0;
    let expected = 3.14159 * x + 2.71828;
    assert_calculation_eq(&decimal_expr, x, expected, "Decimal numbers");

    let small_decimal = MathExpression::new("0.001*x").unwrap();
    assert_calculation_eq(&small_decimal, 1000.0, 1.0, "Small decimal");

    let leading_decimal = MathExpression::new(".5*x").unwrap();
    assert_calculation_eq(&leading_decimal, 4.0, 2.0, "Leading decimal point");
  }

  #[test]
  fn test_expression_cloning() {
    let original = MathExpression::new("x^2 + 2*x + 1").unwrap();
    let cloned = original.clone();
    assert_calculation_eq(
      &cloned,
      3.0,
      16.0,
      "Cloned expression should calculate correctly",
    );
  }

  #[test]
  fn test_debug_formatting() {
    let expr = MathExpression::new("x + 5").unwrap();
    let debug_str = format!("{:?}", expr);
    assert!(
      debug_str.contains("MathExpression"),
      "Debug should contain struct name"
    );
    assert!(
      debug_str.contains("x + 5"),
      "Debug should contain original formula"
    );
  }

  #[test]
  fn test_very_complex_expressions() {
    let complex =
      MathExpression::new("sqrt(sin(x)^2 + cos(x)^2) + ln(e^x) - log10(10^x) + abs(-x)").unwrap();
    assert_calculation_eq(&complex, 2.0, 3.0, "Very complex expression");
  }

  #[test]
  fn test_multiple_parentheses_levels() {
    let nested_parens = MathExpression::new("((x + 1) * (x - 1)) / (x^2 - 1)").unwrap();
    assert_calculation_eq(&nested_parens, 3.0, 1.0, "Multiple parentheses levels");
  }
}

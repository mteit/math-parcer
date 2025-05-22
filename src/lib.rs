use std::f64::consts::{E, PI};
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
enum Token {
  Number(f64),
  Variable,
  ConstPI,
  ConstE,
  Plus,
  Minus,
  Multiply,
  Divide,
  Exponentiation,
  Sin,
  Cos,
  Tan,
  Ln,
  Log(f64),
  Sqrt,
  Abs,
  LeftParen,
  RightParen,
  EOF,
}

#[derive(Debug, Clone)]
enum Expr {
  Number(f64),
  Variable,
  ConstPI,
  ConstE,
  BinaryOp {
    left: Box<Expr>,
    op: Op,
    right: Box<Expr>,
  },
  UnaryOp {
    op: Op,
    expr: Box<Expr>,
  },
}

#[derive(Debug, Clone)]
enum Op {
  Add,
  Subtract,
  Multiply,
  Divide,
  Exponentiation,
  Negate,
  Sin,
  Cos,
  Tan,
  Ln,
  Log(f64),
  Sqrt,
  Abs,
}

struct Lexer {
  input: Vec<char>,
  pointer: usize,
}

impl Lexer {
  fn new(input: &str) -> Self {
    Lexer {
      input: input.chars().collect(), //.filter(|&x| !x.is_whitespace()).collect(),
      pointer: 0,
    }
  }

  fn peek(&self) -> Option<char> {
    if self.pointer < self.input.len() {
      Some(self.input[self.pointer])
    } else {
      None
    }
  }

  fn shift(&mut self) {
    self.pointer += 1;
  }

  fn extract_number(&mut self) -> Result<f64, String> {
    let mut number_str = String::new();
    let mut has_decimal = false;
    while let Some(c) = self.peek() {
      if c.is_digit(10) {
        number_str.push(c);
        self.shift();
      } else if c == '.' && !has_decimal {
        has_decimal = true;
        number_str.push(c);
        self.shift();
      } else {
        break;
      }
    }
    match number_str.parse::<f64>() {
      Ok(num) => Ok(num),
      Err(_) => Err(format!("Invalid number: {}", number_str)),
    }
  }

  fn extract_word(&mut self) -> String {
    let mut word = String::new();
    while let Some(c) = self.peek() {
      if c.is_alphabetic() {
        word.push(c);
        self.shift();
      } else {
        break;
      }
    }
    return word;
  }

  fn tokenize(&mut self) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    while let Some(_) = self.peek() {
      match self.peek() {
        None => break,
        Some(c) => {
          if c.is_digit(10) || c == '.' {
            tokens.push(Token::Number(self.extract_number()?));
          } else if c.is_alphabetic() {
            match self.extract_word().as_str() {
              "x" => tokens.push(Token::Variable),
              "pi" => tokens.push(Token::ConstPI),
              "e" => tokens.push(Token::ConstE),
              "sin" => tokens.push(Token::Sin),
              "cos" => tokens.push(Token::Cos),
              "tan" => tokens.push(Token::Tan),
              "ln" => tokens.push(Token::Ln),
              "log" => tokens.push(Token::Log(self.extract_number()?)),
              "sqrt" => tokens.push(Token::Sqrt),
              "abs" => tokens.push(Token::Abs),
              unknown_keyword => return Err(format!("Unexpected keyword: {}", unknown_keyword)),
            }
          } else if c.is_whitespace() {
            self.shift();
          } else {
            match c {
              '+' => {
                tokens.push(Token::Plus);
                self.shift();
              },
              '-' => {
                tokens.push(Token::Minus);
                self.shift();
              },
              '*' => {
                tokens.push(Token::Multiply);
                self.shift();
              },
              '/' => {
                tokens.push(Token::Divide);
                self.shift();
              },
              '^' => {
                tokens.push(Token::Exponentiation);
                self.shift();
              },
              '(' => {
                tokens.push(Token::LeftParen);
                self.shift();
              },
              ')' => {
                tokens.push(Token::RightParen);
                self.shift();
              },
              _ => return Err(format!("Unexpected character: {}", c)),
            }
          }
        },
      }
    }
    tokens.push(Token::EOF);
    Ok(tokens)
  }
}

struct Parser {
  tokens: Vec<Token>,
  pointer: usize,
}

impl Parser {
  fn new(tokens: Vec<Token>) -> Self {
    Parser { tokens, pointer: 0 }
  }

  fn peek(&self) -> &Token {
    &self.tokens[self.pointer]
  }

  fn shift(&mut self) {
    self.pointer += 1;
  }

  fn factor(&mut self) -> Result<Expr, String> {
    match self.peek() {
      Token::Number(n) => {
        let number = *n;
        self.shift();
        Ok(Expr::Number(number))
      },
      Token::Variable => {
        self.shift();
        Ok(Expr::Variable)
      },
      Token::ConstPI => {
        self.shift();
        Ok(Expr::ConstPI)
      },
      Token::ConstE => {
        self.shift();
        Ok(Expr::ConstE)
      },
      Token::Minus => {
        self.shift();
        let expr = self.factor()?;
        Ok(Expr::UnaryOp {
          op: Op::Negate,
          expr: Box::new(expr),
        })
      },
      Token::Sin => {
        self.shift();
        let expr = self.factor()?;
        Ok(Expr::UnaryOp {
          op: Op::Sin,
          expr: Box::new(expr),
        })
      },
      Token::Cos => {
        self.shift();
        let expr = self.factor()?;
        Ok(Expr::UnaryOp {
          op: Op::Cos,
          expr: Box::new(expr),
        })
      },
      Token::Tan => {
        self.shift();
        let expr = self.factor()?;
        Ok(Expr::UnaryOp {
          op: Op::Tan,
          expr: Box::new(expr),
        })
      },
      Token::Ln => {
        self.shift();
        let expr = self.factor()?;
        Ok(Expr::UnaryOp {
          op: Op::Ln,
          expr: Box::new(expr),
        })
      },
      Token::Log(base) => {
        let base = *base;
        self.shift();
        let expr = self.factor()?;
        Ok(Expr::UnaryOp {
          op: Op::Log(base),
          expr: Box::new(expr),
        })
      },
      Token::Sqrt => {
        self.shift();
        let expr = self.factor()?;
        Ok(Expr::UnaryOp {
          op: Op::Sqrt,
          expr: Box::new(expr),
        })
      },
      Token::Abs => {
        self.shift();
        let expr = self.factor()?;
        Ok(Expr::UnaryOp {
          op: Op::Abs,
          expr: Box::new(expr),
        })
      },
      Token::LeftParen => {
        self.shift();
        let result = self.expression()?;
        if *self.peek() != Token::RightParen {
          return Err("Expected closing parenthesis".to_string());
        }
        self.shift();
        Ok(result)
      },
      _ => Err(format!("Unexpected token in factor: {:?}", self.peek())),
    }
  }

  fn term(&mut self) -> Result<Expr, String> {
    let mut left = self.factor()?;
    loop {
      match self.peek() {
        Token::Multiply => {
          self.shift();
          let right = self.factor()?;
          left = Expr::BinaryOp {
            left: Box::new(left),
            op: Op::Multiply,
            right: Box::new(right),
          };
        },
        Token::Divide => {
          self.shift();
          let right = self.factor()?;
          left = Expr::BinaryOp {
            left: Box::new(left),
            op: Op::Divide,
            right: Box::new(right),
          };
        },
        Token::Exponentiation => {
          self.shift();
          let right = self.factor()?;
          left = Expr::BinaryOp {
            left: Box::new(left),
            op: Op::Exponentiation,
            right: Box::new(right),
          };
        },
        _ => break,
      }
    }
    Ok(left)
  }

  fn expression(&mut self) -> Result<Expr, String> {
    let mut left = self.term()?;
    loop {
      match self.peek() {
        Token::Plus => {
          self.shift();
          let right = self.term()?;
          left = Expr::BinaryOp {
            left: Box::new(left),
            op: Op::Add,
            right: Box::new(right),
          };
        },
        Token::Minus => {
          self.shift();
          let right = self.term()?;
          left = Expr::BinaryOp {
            left: Box::new(left),
            op: Op::Subtract,
            right: Box::new(right),
          };
        },
        _ => break,
      }
    }
    Ok(left)
  }

  fn parse(&mut self) -> Result<Expr, String> {
    let expr = self.expression()?;
    if *self.peek() != Token::EOF {
      return Err(format!(
        "Unexpected tokens after expression: {:?}",
        self.peek()
      ));
    }
    Ok(expr)
  }
}

impl Expr {
  fn evaluate(&self, x: f64) -> Result<f64, String> {
    match self {
      Expr::Number(n) => Ok(*n),
      Expr::Variable => Ok(x),
      Expr::ConstPI => Ok(PI),
      Expr::ConstE => Ok(E),
      Expr::BinaryOp { left, op, right } => {
        let left_val = left.evaluate(x)?;
        let right_val = right.evaluate(x)?;
        match op {
          Op::Add => Ok(left_val + right_val),
          Op::Subtract => Ok(left_val - right_val),
          Op::Multiply => Ok(left_val * right_val),
          Op::Divide => {
            if right_val != 0.0 {
              Ok(left_val / right_val)
            } else {
              Err("Division by zero".to_string())
            }
          },
          Op::Exponentiation => Ok(left_val.powf(right_val)),
          _ => Err("Unexpected binary operator".to_string()),
        }
      },
      Expr::UnaryOp { op, expr } => {
        let result = expr.evaluate(x)?;
        match op {
          Op::Negate => Ok(-result),
          Op::Sin => Ok(result.sin()),
          Op::Cos => Ok(result.cos()),
          Op::Tan => Ok(result.tan()),
          Op::Ln => Ok(result.ln()),
          Op::Log(10.0) => Ok(result.log10()),
          Op::Log(base) => Ok(result.log(*base)),
          Op::Sqrt => Ok(result.sqrt()),
          Op::Abs => Ok(result.abs()),
          _ => Err("Unexpected unary operator".to_string()),
        }
      },
    }
  }
}

impl fmt::Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expr::Number(n) => write!(f, "{}", n),
      Expr::Variable => write!(f, "x"),
      Expr::ConstPI => write!(f, "pi"),
      Expr::ConstE => write!(f, "e"),
      Expr::BinaryOp { left, op, right } => {
        let op_str = match op {
          Op::Add => "+",
          Op::Subtract => "-",
          Op::Multiply => "*",
          Op::Divide => "/",
          Op::Exponentiation => "^",
          _ => panic!("Unexpected binary operator in string conversion"),
        };
        write!(f, "({}{}{})", left, op_str, right)
      },
      Expr::UnaryOp { op, expr } => match op {
        Op::Negate => write!(f, "(-{})", expr),
        Op::Sin => write!(f, "sin({})", expr),
        Op::Cos => write!(f, "cos({})", expr),
        Op::Tan => write!(f, "tan({})", expr),
        Op::Ln => write!(f, "ln({})", expr),
        Op::Log(10.0) => write!(f, "log({})", expr),
        Op::Log(base) => write!(f, "log{}({})", base, expr),
        Op::Sqrt => write!(f, "sqrt({})", expr),
        Op::Abs => write!(f, "abs({})", expr),
        _ => panic!("Unexpected unary operator in string conversion"),
      },
    }
  }
}

#[derive(Debug, Clone)]
pub struct MathExpression {
  original_formula: String,
  ast: Expr,
}

impl MathExpression {
  pub fn new(formula: &str) -> Result<Self, String> {
    if formula.trim().is_empty() {
      return Err("Empty expression".to_string());
    }
    let tokens = Lexer::new(formula).tokenize()?;
    let ast = Parser::new(tokens).parse()?;
    Ok(MathExpression {
      original_formula: formula.to_string(),
      ast,
    })
  }

  pub fn calculate(&self, x: f64) -> Result<f64, String> {
    self.ast.evaluate(x)
  }

  pub fn formula(&self) -> &str {
    &self.original_formula
  }

  pub fn normalized_formula(&self) -> String {
    self.ast.to_string()
  }
}

impl fmt::Display for MathExpression {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.original_formula)
  }
}

pub fn generate_lambda(formula: &str) -> Result<impl Fn(f64) -> Result<f64, String>, String> {
  let expr = MathExpression::new(formula)?;
  Ok(move |x: f64| expr.calculate(x))
}

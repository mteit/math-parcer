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
      input: input.chars().filter(|&x| !x.is_whitespace()).collect(),
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

fn evaluate_expr(expr: &Expr, x: f64) -> Result<f64, String> {
  match expr {
    Expr::Number(n) => Ok(*n),
    Expr::Variable => Ok(x),
    Expr::ConstPI => Ok(std::f64::consts::PI),
    Expr::ConstE => Ok(std::f64::consts::E),
    Expr::BinaryOp { left, op, right } => {
      let left_val = evaluate_expr(left, x)?;
      let right_val = evaluate_expr(right, x)?;
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
      let expr_result = evaluate_expr(expr, x)?;
      match op {
        Op::Negate => Ok(-expr_result),
        Op::Sin => Ok(expr_result.sin()),
        Op::Cos => Ok(expr_result.cos()),
        Op::Tan => Ok(expr_result.tan()),
        Op::Ln => Ok(expr_result.ln()),
        Op::Log(10.0) => Ok(expr_result.log10()),
        Op::Log(base) => Ok(expr_result.log(*base)),
        Op::Sqrt => Ok(expr_result.sqrt()),
        Op::Abs => Ok(expr_result.abs()),
        _ => Err("Unexpected unary operator".to_string()),
      }
    },
  }
}

pub type MathFunction = Box<dyn Fn(f64) -> Result<f64, String>>;

pub fn generate_lambda(expr_str: &str) -> Result<MathFunction, String> {
  let tokens = Lexer::new(expr_str).tokenize()?;
  let ast = Parser::new(tokens).parse()?;
  Ok(Box::new(move |x: f64| evaluate_expr(&ast, x)))
}

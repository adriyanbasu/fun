use std::collections::HashMap;

// Token Definition
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    Number(i64),
    Operator(char),
    Print,
    Semicolon,
    Assignment,
    Plus,
    Minus,
    Multiply,
    Divide,
    LParen,
    RParen,
    EndOfFile,
}

// AST Node Definition
#[derive(Debug, Clone)]
pub enum Expr {
    Variable(String),
    Constant(i64),
    BinOp(Box<Expr>, Operator, Box<Expr>),
    Paren(Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
}

impl From<Token> for Operator {
    fn from(token: Token) -> Self {
        match token {
            Token::Plus => Operator::Plus,
            Token::Minus => Operator::Minus,
            Token::Multiply => Operator::Multiply,
            Token::Divide => Operator::Divide,
            _ => panic!("Unexpected token in operator conversion"),
        }
    }
}

// Statement Definition
#[derive(Debug, Clone)]
pub enum Statement {
    Assignment(String, Expr),
    Print(Expr),
}

// Lexer
pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer { input, position: 0 }
    }

    fn consume_while<F>(&mut self, condition: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut result = String::new();
        while let Some(c) = self.input[self.position..].chars().next() {
            if condition(c) {
                result.push(c);
                self.position += c.len_utf8();
            } else {
                break;
            }
        }
        result
    }

    fn consume_whitespace(&mut self) {
        self.consume_while(char::is_whitespace);
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while self.position < self.input.len() {
            self.consume_whitespace();

            if let Some(c) = self.input[self.position..].chars().next() {
                match c {
                    '0'..='9' => {
                        let number_str = self.consume_while(|c| c.is_digit(10));
                        let number = number_str.parse::<i64>().unwrap();
                        tokens.push(Token::Number(number));
                    }
                    '+' => {
                        tokens.push(Token::Plus);
                        self.position += c.len_utf8();
                    }
                    '-' => {
                        tokens.push(Token::Minus);
                        self.position += c.len_utf8();
                    }
                    '*' => {
                        tokens.push(Token::Multiply);
                        self.position += c.len_utf8();
                    }
                    '/' => {
                        tokens.push(Token::Divide);
                        self.position += c.len_utf8();
                    }
                    '(' => {
                        tokens.push(Token::LParen);
                        self.position += c.len_utf8();
                    }
                    ')' => {
                        tokens.push(Token::RParen);
                        self.position += c.len_utf8();
                    }
                    'a'..='z' | 'A'..='Z' => {
                        let identifier = self.consume_while(|c| c.is_ascii_alphanumeric());
                        if identifier == "print" {
                            tokens.push(Token::Print);
                        } else {
                            tokens.push(Token::Identifier(identifier));
                        }
                    }
                    '=' => {
                        tokens.push(Token::Assignment);
                        self.position += c.len_utf8();
                    }
                    ';' => {
                        tokens.push(Token::Semicolon);
                        self.position += c.len_utf8();
                    }
                    _ => {
                        // Handle unexpected characters
                        self.position += c.len_utf8();
                    }
                }
            }
        }

        tokens.push(Token::EndOfFile);
        tokens
    }
}

// Parser
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            current: 0,
        }
    }

    fn parse_expr(&mut self) -> Expr {
        let mut left_expr = self.parse_primary();

        while let Some(op) = self.peek_token() {
            if let Token::Operator(_) = op {
                let op = self.consume_token().unwrap();
                let right_expr = self.parse_primary();
                left_expr = Expr::BinOp(Box::new(left_expr), op.into(), Box::new(right_expr));
            } else {
                break;
            }
        }

        left_expr
    }

    fn parse_primary(&mut self) -> Expr {
        match self.consume_token().unwrap() {
            Token::Identifier(ident) => Expr::Variable(ident),
            Token::Number(num) => Expr::Constant(num),
            Token::LParen => {
                let expr = self.parse_expr();
                self.consume_token(); // Consume the closing parenthesis
                Expr::Paren(Box::new(expr))
            }
            _ => panic!("Unexpected token in primary expression"),
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, &'static str> {
        match self.consume_token().unwrap() {
            Token::Identifier(ident) => {
                if let Some(Token::Assignment) = self.peek_token() {
                    self.consume_token(); // Consume '='
                    let expr = self.parse_expr();
                    Ok(Statement::Assignment(ident, expr))
                } else {
                    Err("Expected '=' after identifier in assignment statement")
                }
            }
            Token::Print => {
                let expr = self.parse_expr();
                Ok(Statement::Print(expr))
            }
            _ => Err("Unexpected token in statement"),
        }
    }

    fn consume_token(&mut self) -> Option<Token> {
        if self.current < self.tokens.len() {
            let token = self.tokens[self.current].clone();
            self.current += 1;
            Some(token)
        } else {
            None
        }
    }

    fn peek_token(&self) -> Option<&Token> {
        if self.current < self.tokens.len() {
            Some(&self.tokens[self.current])
        } else {
            None
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, &'static str> {
        let mut statements = Vec::new();

        while self.current < self.tokens.len() {
            statements.push(self.parse_statement()?);
            // Consume semicolon if present
            if let Some(Token::Semicolon) = self.peek_token() {
                self.consume_token();
            }
        }

        Ok(statements)
    }
}

// Interpreter
pub struct Interpreter {
    variables: HashMap<String, i64>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            variables: HashMap::new(),
        }
   }

    fn eval_expr(&mut self, expr: &Expr) -> i64 {
        match expr {
            Expr::Variable(ident) => *self.variables.get(ident).unwrap_or(&0),
            Expr::Constant(num) => *num,
            Expr::BinOp(left, op, right) => {
                let left_val = self.eval_expr(left);
                let right_val = self.eval_expr(right);
                match op {
                    Operator::Plus => left_val + right_val,
                    Operator::Minus => left_val - right_val,
                    Operator::Multiply => left_val * right_val,
                    Operator::Divide => left_val / right_val,
                }
            }
            Expr::Paren(inner_expr) => self.eval_expr(inner_expr),
        }
    }

    pub fn execute(&mut self, statements: Vec<Statement>) {
        for statement in statements {
            match statement {
                Statement::Assignment(ident, expr) => {
                    let value = self.eval_expr(&expr);
                    self.variables.insert(ident, value);
                }
                Statement::Print(expr) => {
                    let result = self.eval_expr(&expr);
                    println!("Print: {}", result);
                }
            }
        }
    }
}

// Main Program
fn main() {
    let input = "x = 42 + (8 * 5); print(x);";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();

    let mut parser = Parser::new(tokens);
    match parser.parse() {
        Ok(statements) => {
            let mut interpreter = Interpreter::new();
            interpreter.execute(statements);
        }
        Err(error) => println!("Parsing Error: {}", error),
    }
}


 

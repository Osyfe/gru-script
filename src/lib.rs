mod parse;

use thiserror::Error;
use std::{ops::Range, io, fmt};

#[derive(Error, Clone, Debug, Default)]
pub struct CodePosition
{
    line: usize,
    cols: Range<usize>
}

impl fmt::Display for CodePosition
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        write!(f, "line {}, cols {}-{}", self.line, self.cols.start, self.cols.end)
    }
}

#[derive(Error, Debug)]
pub enum Error
{
    #[error("problem accessing the source files")]
    IO(#[from] io::Error),
    #[error("lexer @ {0}")]
    Lexer(#[from] CodePosition),
    #[error("parser: {0} @ {1}")]
    Parser(String, CodePosition),
}

#[derive(Debug)]
enum OpSingle
{
    Plus,
    Neg,
}

#[derive(Debug)]
enum OpDouble
{
    Add,
    Sub,
    Mul,
    Div,
    Index,
    ObjNav,
    CodeNav,
    Assign,
}

#[derive(Debug)]
enum AST
{
    Unit,
    Int(i32),
    Float(f32),
    Ident(String),
    TypedIdent(Box<ASTExtra>, Box<ASTExtra>),
    OpSingle(OpSingle, Box<ASTExtra>),
    OpDouble(OpDouble, Box<ASTExtra>, Box<ASTExtra>),
    Block(Vec<ASTExtra>),
    FnCall(Box<ASTExtra>, Vec<ASTExtra>),
    Let(Box<ASTExtra>, Box<ASTExtra>),
    FnDef(Box<ASTExtra>, Vec<ASTExtra>, Box<ASTExtra>, Box<ASTExtra>),
    Items(Vec<ASTExtra>),
}

#[derive(Debug)]
struct ASTExtra
{
    ast: AST,
    pos: CodePosition
}

pub fn debug_lex(file: &str)
{
    parse::debug_lex(file)
}

pub fn debug_parse(file: &str)
{
    parse::debug_parse(file)
}

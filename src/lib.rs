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

#[derive(Clone, Debug)]
enum OpSingle
{
    Plus,
    Neg,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
struct Ident { name: String }

#[derive(Clone, Debug)]
struct Path { steps: Vec<Located<Ident>> }

#[derive(Clone, Debug)]
struct TypedIdent { name: Box<Located<Ident>>, ty: Path }

#[derive(Clone, Debug)]
struct SingleOp { op: OpSingle, arg: Box<Located<AST>> }

#[derive(Clone, Debug)]
struct DoubleOp { op: OpDouble, l_arg: Box<Located<AST>>, r_arg: Box<Located<AST>> }

#[derive(Clone, Debug)]
struct Block { exprs: Vec<Located<AST>> }

#[derive(Clone, Debug)]
struct FnCall { name: Box<Located<AST>>, args: Vec<Located<AST>> }

#[derive(Clone, Debug)]
struct Let { name: Located<TypedIdent>, value: Box<Located<AST>> }

#[derive(Clone, Debug)]
struct FnDef { name: Box<Located<Ident>>, args: Vec<Located<TypedIdent>>, ret: Path, body: Block }

#[derive(Clone, Debug)]
struct Items { fns: Vec<Located<FnDef>> }

#[derive(Clone, Debug)]
enum AST
{
    Unit,
    Int(i32),
    Float(f32),
    Ident(Ident),
    TypedIdent(TypedIdent),
    Path(Path),
    OpSingle(SingleOp),
    OpDouble(DoubleOp),
    Block(Block),
    FnCall(FnCall),
    Let(Let),
    FnDef(FnDef),
    Items(Items),
}

#[derive(Clone, Debug)]
struct Located<T>
{
    t: T,
    pos: CodePosition
}

trait Locate: Sized
{
    fn with(self, pos: CodePosition) -> Located<Self>;
}

impl<T> Locate for T
{
    fn with(self, pos: CodePosition) -> Located<Self>
    {
        Located { t: self, pos }
    }
}

pub fn debug_lex(file: &str)
{
    parse::debug_lex(file)
}

pub fn debug_parse(file: &str)
{
    parse::debug_parse(file)
}

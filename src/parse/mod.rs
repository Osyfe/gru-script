mod lex;

use lex::{Atom, Op, Key, Token, TokenIter};
use super::{Error, AST, ASTExtra, CodePosition};

type ResAST = Result<ASTExtra, Error>;

pub fn debug_lex(file: &str)
{
    let code = std::fs::read_to_string(file).unwrap();
    let mut tokens = TokenIter::from(&code);
    loop
    {
        let token = tokens.next();
        println!("{token:?}");
        if matches!(token, Ok((Token::Eof, _))) { break; }
    }
}

pub fn debug_parse(file: &str)
{
    let code = std::fs::read_to_string(file).unwrap();
    let mut tokens = TokenIter::from(&code);
    let items = items(&mut tokens);
    match items
    {
        Ok(items) => items.print(0),
        Err(err) => println!("{err}")
    }
}

impl<'source> TokenIter<'source>
{
    fn expect(&mut self, expected: Token) -> Result<CodePosition, Error>
    {
        let (found, pos) = self.next()?;
        if found == expected { Ok(pos) }
        else { Err(Error::Parser(format!("expected \"{expected:?}\", found \"{found:?}"), pos)) }
    }

    fn expect_ident(&mut self) -> Result<ASTExtra, Error>
    {
        match self.next()?
        {
            (Token::Atom(Atom::Ident(v)), pos) => Ok(AST::Ident(v).with(pos)),
            (t, pos) => Err(Error::Parser(format!("expected an identifier; found \"{t:?}\""), pos))
        }
    }
}

impl From<Op> for super::OpSingle
{
    fn from(op: Op) -> Self
    {
        match op
        {
            Op::Plus => Self::Plus,
            Op::Minus => Self::Neg,
            _ => unreachable!("bad op: {op:?}")
        }
    }
}

impl From<Op> for super::OpDouble
{
    fn from(op: Op) -> Self
    {
        match op
        {
            Op::Plus => Self::Add,
            Op::Minus => Self::Sub,
            Op::Times => Self::Mul,
            Op::By => Self::Div,
            Op::LBracket => Self::Index,
            Op::ObjNav => Self::ObjNav,
            Op::CodeNav => Self::CodeNav,
            Op::Assign => Self::Assign,
            _ => unreachable!("bad op: {op:?}")
        }
    }
}

impl AST
{
    fn with(self, pos: CodePosition) -> ASTExtra
    {
        ASTExtra { ast: self, pos }
    }

    fn print(&self, ident: usize)
    {
        const IDENT_BASE: &str = "   ";
        let ident_str = IDENT_BASE.repeat(ident);
        match self
        {
            Self::Unit => print!("{ident_str}()"),
            Self::Int(v) => print!("{ident_str}Lit: {v}"),
            Self::Float(v) => print!("{ident_str}Lit: {v}"),
            Self::Ident(v) => print!("{ident_str}Ident: {v}"),
            Self::TypedIdent(v, t) =>
            {
                print!("{ident_str}TypedIdent\n{ident_str}(\n{ident_str}{IDENT_BASE}name:\n");
                v.ast.print(ident + 1);
                print!(",\n{ident_str}{IDENT_BASE}type:\n");
                t.ast.print(ident + 1);
                print!("\n{ident_str})");
            },
            Self::OpSingle(op, hs) =>
            {
                print!("{ident_str}OpSingle\n{ident_str}(\n{ident_str}{IDENT_BASE}op: {op:?},\n{ident_str}{IDENT_BASE}hs:\n");
                hs.ast.print(ident + 1);
                print!("\n{ident_str})");
            },
            Self::OpDouble(op, lhs, rhs) =>
            {
                print!("{ident_str}OpDouble\n{ident_str}(\n{ident_str}{IDENT_BASE}op: {op:?},\n{ident_str}{IDENT_BASE}lhs:\n");
                lhs.ast.print(ident + 1);
                print!(",\n{ident_str}{IDENT_BASE}rhs:\n");
                rhs.ast.print(ident + 1);
                print!("\n{ident_str})");
            },
            Self::Block(list) =>
            {
                print!("{ident_str}Block\n{ident_str}(\n");
                for (i, expr) in list.iter().enumerate()
                {
                    print!("{ident_str}{IDENT_BASE}{}:\n", i + 1);
                    expr.ast.print(ident + 1);
                    println!(",");
                }
                print!("{ident_str})");
            },
            Self::FnCall(obj, list) =>
            {
                print!("{ident_str}FnCall\n{ident_str}(\n{ident_str}{IDENT_BASE}obj:\n");
                obj.ast.print(ident + 1);
                print!(",\n");
                for (i, expr) in list.iter().enumerate()
                {
                    print!("{ident_str}{IDENT_BASE}{}:\n", i + 1);
                    expr.ast.print(ident + 1);
                    println!(",");
                }
                print!("{ident_str})");
            },
            Self::Let(name, value) =>
            {
                print!("{ident_str}Let\n{ident_str}{{\n{ident_str}{IDENT_BASE}name:\n");
                name.ast.print(ident + 1);
                print!(",\n{ident_str}{IDENT_BASE}value:\n");
                value.ast.print(ident + 1);
                print!("\n{ident_str}}}");
            },
            Self::FnDef(name, args, ret, body) =>
            {
                print!("{ident_str}FnDef\n{ident_str}{{\n{ident_str}{IDENT_BASE}name:\n");
                name.ast.print(ident + 1);
                print!(",\n{ident_str}{IDENT_BASE}args:\n");
                for (i, arg) in args.iter().enumerate()
                {
                    print!("{ident_str}{IDENT_BASE}{}:\n", i + 1);
                    arg.ast.print(ident + 1);
                    println!(",");
                }
                print!("{ident_str}{IDENT_BASE}ret:\n");
                ret.ast.print(ident + 1);
                print!(",\n{ident_str}{IDENT_BASE}body:\n");
                body.ast.print(ident + 1);
                print!("\n{ident_str}}}");
            },
            Self::Items(items) =>
            {
                for item in items
                {
                    item.ast.print(0);
                    println!("\n");
                }
            }
        }
    }
}

fn items(tokens: &mut TokenIter) -> Result<AST, Error>
{
    let mut items = Vec::new();
    loop
    {
        match tokens.next()?
        {
            (Token::Key(Key::Fn), pos) =>
            {
                let name = tokens.expect_ident()?;
                tokens.expect(Token::Op(Op::LParen))?;
                let args = var_list(tokens, Op::RParen)?;
                tokens.expect(Token::Key(Key::MapsTo))?;
                let ret = tokens.expect_ident()?;
                let body = expr(tokens, 0, true)?;
                if !matches!(body.ast, AST::Block(_)) { return Err(Error::Parser(format!("unexpected expression; expected a block"), pos)); }
                items.push(AST::FnDef(Box::new(name), args, Box::new(ret), Box::new(body)).with(pos));
            },
            (Token::Eof, _) => break,
            (t, pos) => return Err(Error::Parser(format!("unexpected token: {t:?}"), pos))
        }
    }
    Ok(AST::Items(items))
}

fn expr(tokens: &mut TokenIter, min_pc: u8, assign: bool) -> ResAST
{
    let mut lhs = match tokens.next()?
    {
        (Token::Atom(Atom::Int(v)), pos) => AST::Int(v).with(pos),
        (Token::Atom(Atom::Float(v)), pos) => AST::Float(v).with(pos),
        (Token::Atom(Atom::Ident(v)), pos) => AST::Ident(v).with(pos),
        (Token::Op(Op::LParen), _) =>
        {
            let lhs = expr(tokens, 0, false)?;
            tokens.expect(Token::Op(Op::RParen))?;
            lhs
        },
        (Token::Op(Op::LBrace), pos) =>
        {
            let exprs = expr_list(tokens, Op::BlockSep, Op::RBrace, true, true)?;
            AST::Block(exprs).with(pos)
        },
        (Token::Key(Key::Let), pos) => if assign
        {
            let var = var(tokens)?;
            let pos = tokens.expect(Token::Op(Op::Assign))?;
            let value = expr(tokens, 0, false)?;
            return Ok(AST::Let(Box::new(var), Box::new(value)).with(pos));
        } else
        {
            return Err(Error::Parser(format!("unexpected variable binding"), pos))
        },
        (Token::Op(op), pos) =>
        {
            let r_pc = match prefix_precedence(&op)
            {
                Some((_, r_pc)) => r_pc,
                None => return Err(Error::Parser(format!("operator is not prefix: {op:?}"), pos))
            };
            let rhs = expr(tokens, r_pc, false)?;
            AST::OpSingle(op.into(), Box::new(rhs)).with(pos)
        }
        (t, pos) => return Err(Error::Parser(format!("unexpected token: {t:?}"), pos))
    };

    loop
    {
        let (op, op_pos) = match tokens.peek()?
        {
            (Token::Op(op), pos) => (op, pos),
            _ => break
        };

        if let Some((l_pc, ())) = postfix_precedence(&op)
        {
            if l_pc < min_pc { break; }
            tokens.next()?;

            lhs = match op
            {
                Op::LParen =>
                {
                    let exprs = expr_list(tokens, Op::ItemSep, Op::RParen, false, false)?;
                    AST::FnCall(Box::new(lhs), exprs).with(op_pos)
                },
                Op::LBracket =>
                {
                    let rhs = expr(tokens, 0, false)?;
                    tokens.expect(Token::Op(Op::RBracket))?;
                    AST::OpDouble(op.into(), Box::new(lhs), Box::new(rhs)).with(op_pos)
                },
                Op::ObjNav | Op::CodeNav => 
                {
                    let (ident, pos) = tokens.next()?;
                    match ident
                    {
                        Token::Atom(Atom::Ident(v)) =>
                        {
                            let rhs = AST::Ident(v).with(pos);
                            AST::OpDouble(op.into(), Box::new(lhs), Box::new(rhs)).with(op_pos)
                        },
                        _ => return Err(Error::Parser(format!("unexpected token {op:?}; expected identifier instead"), pos))
                    }
                },
                _ =>
                {
                    AST::OpSingle(op.into(), Box::new(lhs)).with(op_pos)
                }
            };

            continue;
        }

        if let Some((l_pc, r_pc)) = infix_precedence(&op)
        {
            if l_pc < min_pc { break; }
            tokens.next()?;

            if matches!(op, Op::Assign) && !assign { return Err(Error::Parser(format!("unexpected variable assign"), op_pos)) }

            let rhs = expr(tokens, r_pc, false)?;
            lhs = AST::OpDouble(op.into(), Box::new(lhs), Box::new(rhs)).with(op_pos);

            continue;
        }

        break;
    }

    Ok(lhs)
}

fn expr_list(tokens: &mut TokenIter, sep: Op, end: Op, assign: bool, trailing_unit: bool) -> Result<Vec<ASTExtra>, Error>
{
    let mut exprs = Vec::new();
    let mut current_expr = None;
    
    loop
    {
        match tokens.peek()?
        {
            (Token::Op(op), pos) if op == sep =>
            {
                tokens.next()?;
                match current_expr.take()
                {
                    None => return Err(Error::Parser(format!("unexpected seperator; expected an expression instead"), pos)),
                    Some(expr) => exprs.push(expr)
                }
            },
            (Token::Op(op), pos) if op == end =>
            {
                tokens.next()?;
                match current_expr
                {
                    None => if trailing_unit { exprs.push(AST::Unit.with(pos)) },
                    Some(expr) => exprs.push(expr)
                }
                break;
            },
            _ =>
            {
                match current_expr
                {
                    None => current_expr = Some(expr(tokens, 0, assign)?),
                    Some(expr) => return Err(Error::Parser(format!("unexpected expression; expected {sep:?} or {end:?} instead"), expr.pos))
                }
            }
        }
    }

    Ok(exprs)
}

fn var(token: &mut TokenIter) -> Result<ASTExtra, Error>
{
    let name = token.expect_ident()?;
    let pos = token.expect(Token::Op(Op::Type))?;
    let typ = token.expect_ident()?;
    Ok(AST::TypedIdent(Box::new(name), Box::new(typ)).with(pos))
}

fn var_list(tokens: &mut TokenIter, end: Op) -> Result<Vec<ASTExtra>, Error>
{
    let mut exprs = Vec::new();
    let mut current_var = None;
    
    loop
    {
        match tokens.peek()?
        {
            (Token::Op(op), pos) if op == Op::ItemSep =>
            {
                tokens.next()?;
                match current_var.take()
                {
                    None => return Err(Error::Parser(format!("unexpected seperator; expected a variable instead"), pos)),
                    Some(var) => exprs.push(var)
                }
            },
            (Token::Op(op), _) if op == end =>
            {
                tokens.next()?;
                if let Some(var) = current_var { exprs.push(var); }
                break;
            },
            _ =>
            {
                match current_var
                {
                    None => current_var = Some(var(tokens)?),
                    Some(var) => return Err(Error::Parser(format!("unexpected expression; expected {:?} or {end:?} instead", Op::ItemSep), var.pos))
                }
            }
        }
    }

    Ok(exprs)
}

fn prefix_precedence(op: &Op) -> Option<((), u8)>
{
    let res = match op
    {
        Op::Plus | Op::Minus => ((), 9),
        _ => return None,
    };
    Some(res)
}

fn infix_precedence(op: &Op) -> Option<(u8, u8)>
{
    let res = match op
    {
        Op::Assign => (2, 1),
        Op::Plus | Op::Minus => (5, 6),
        Op::Times | Op::By => (7, 8),
        _ => return None,

    };
    Some(res)
}

fn postfix_precedence(op: &Op) -> Option<(u8, ())>
{
    let res = match op
    {
        Op::LParen => (11, ()), //function call
        Op::LBracket => (11, ()), //index
        Op::ObjNav | Op::CodeNav => (11, ()),
        _ => return None,
    };
    Some(res)
}

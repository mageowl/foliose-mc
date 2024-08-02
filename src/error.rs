use std::{error::Error, fmt::Display};

#[derive(Debug, Clone, Copy)]
pub struct SourcePos {
    pub ln: usize,
    pub col: usize,
}

impl Display for SourcePos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(line {ln}, col {col})", ln = self.ln, col = self.col)
    }
}

#[derive(Debug)]
pub struct SourceError {
    pub span: Span,
    pub message: String,
}

impl Display for SourceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: Add actual context to error
        write!(
            f,
            "error: {}\n  from {} to {}",
            self.message, self.span.start, self.span.end
        )
    }
}

impl Error for SourceError {}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: SourcePos,
    pub end: SourcePos,
}

impl Span {
    pub fn new(start: SourcePos, end: SourcePos) -> Self {
        Self { start, end }
    }

    pub fn chunk<T>(&self, data: T) -> Chunk<T> {
        Chunk { span: *self, data }
    }

    pub fn error(&self, message: String) -> SourceError {
        SourceError {
            span: *self,
            message,
        }
    }
}

#[derive(Debug)]
pub struct Chunk<T> {
    pub span: Span,
    pub data: T,
}

impl<T> Chunk<T> {
    pub fn map<F, D>(self, mut f: F) -> Chunk<D>
    where
        F: FnMut(T) -> D,
    {
        Chunk {
            span: self.span,
            data: f(self.data),
        }
    }
}

pub use self::error::EmitError;
use self::funcs::escape_str;
use self::funcs::need_quotes;
use crate::yaml::Hash;
use crate::yaml::Yaml;
use std::fmt;

mod error;
mod funcs;

pub struct YamlEmitter<'a> {
    writer: &'a mut dyn fmt::Write,
    best_indent: usize,
    compact: bool,

    level: isize,
}

pub type EmitResult = Result<(), EmitError>;

impl<'a> YamlEmitter<'a> {
    pub fn new(writer: &'a mut dyn fmt::Write) -> YamlEmitter {
        YamlEmitter {
            writer,
            best_indent: 2,
            compact: true,
            level: -1,
        }
    }

    /// Set 'compact inline notation' on or off, as described for block
    /// [sequences](http://www.yaml.org/spec/1.2/spec.html#id2797382)
    /// and
    /// [mappings](http://www.yaml.org/spec/1.2/spec.html#id2798057).
    ///
    /// In this form, blocks cannot have any properties (such as anchors
    /// or tags), which should be OK, because this emitter doesn't
    /// (currently) emit those anyways.
    pub fn compact(&mut self, compact: bool) {
        self.compact = compact;
    }

    /// Determine if this emitter is using 'compact inline notation'.
    pub fn is_compact(&self) -> bool {
        self.compact
    }

    pub fn dump(&mut self, doc: &Yaml) -> EmitResult {
        writeln!(self.writer, "---")?;
        self.level = -1;
        self.emit_node(doc)
    }

    fn emit_node(&mut self, node: &Yaml) -> EmitResult {
        match *node {
            Yaml::Array(ref v) => self.emit_array(v),
            Yaml::Hash(ref v) => self.emit_hash(v),
            Yaml::String(ref v) => {
                if need_quotes(v) {
                    escape_str(self.writer, v)?;
                } else {
                    write!(self.writer, "{}", v)?;
                }
                Ok(())
            }
            Yaml::Boolean(v) => {
                match v {
                    true => write!(self.writer, "true")?,
                    false => write!(self.writer, "false")?,
                }
                Ok(())
            }
            Yaml::Integer(v) => {
                write!(self.writer, "{}", v)?;
                Ok(())
            }
            Yaml::Real(ref v) => {
                write!(self.writer, "{}", v)?;
                Ok(())
            }
            Yaml::Null | Yaml::BadValue => {
                write!(self.writer, "~")?;
                Ok(())
            }
            Yaml::Alias(_) => Ok(()),
        }
    }

    fn emit_array(&mut self, arr: &[Yaml]) -> EmitResult {
        if arr.is_empty() {
            write!(self.writer, "[]")?;
            return Ok(());
        }

        self.level += 1;
        for (idx, entry) in arr.iter().enumerate() {
            if idx > 0 {
                self.emit_line_begin()?;
            }
            write!(self.writer, "-")?;
            self.emit_value(true, entry)?;
        }
        self.level -= 1;
        Ok(())
    }

    fn emit_hash(&mut self, hash: &Hash) -> EmitResult {
        if hash.is_empty() {
            self.writer.write_str("{}")?;
            return Ok(());
        }

        self.level += 1;
        for (idx, (key, value)) in hash.iter().enumerate() {
            if idx > 0 {
                self.emit_line_begin()?;
            }
            let is_complex_key = matches!(*key, Yaml::Hash(_) | Yaml::Array(_));
            if is_complex_key {
                write!(self.writer, "?")?;
                self.emit_value(true, key)?;
                self.emit_line_begin()?;
                write!(self.writer, ":")?;
                self.emit_value(true, value)?;
            } else {
                self.emit_node(key)?;
                write!(self.writer, ":")?;
                self.emit_value(false, value)?;
            }
        }
        self.level -= 1;
        Ok(())
    }

    /// Emit a yaml as a hash or array value: i.e., which should appear
    /// following a ":" or "-", either after a space, or on a new line.
    /// If `inline` is true, then the preceding characters are distinct
    /// and short enough to respect the compact flag.
    fn emit_value(&mut self, inline: bool, value: &Yaml) -> EmitResult {
        match *value {
            Yaml::Array(ref arr) => {
                if (inline && self.compact) || arr.is_empty() {
                    write!(self.writer, " ")?;
                } else {
                    writeln!(self.writer)?;
                    self.level += 1;
                    self.emit_indent()?;
                    self.level -= 1;
                }
                self.emit_array(arr)
            }
            Yaml::Hash(ref hash) => {
                if (inline && self.compact) || hash.is_empty() {
                    write!(self.writer, " ")?;
                } else {
                    writeln!(self.writer)?;
                    self.level += 1;
                    self.emit_indent()?;
                    self.level -= 1;
                }
                self.emit_hash(hash)
            }
            _ => {
                write!(self.writer, " ")?;
                self.emit_node(value)
            }
        }
    }

    fn emit_line_begin(&mut self) -> EmitResult {
        writeln!(self.writer)?;
        self.emit_indent()?;
        Ok(())
    }

    fn emit_indent(&mut self) -> EmitResult {
        if self.level <= 0 {
            return Ok(());
        }
        for _ in 0..(self.level * self.best_indent as isize) {
            write!(self.writer, " ")?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::YamlLoader;
    use pretty_assertions::assert_eq;
    use std::env;
    use std::fs;

    macro_rules! fixture_test {
        ($test_name:ident, $fixture:expr) => {
            #[test]
            fn $test_name() -> Result<(), std::io::Error> {
                let record_fixtures = env::var("RECORD_FIXTURES").is_ok();

                let input = format!("tests/fixtures/{}.input.yaml", $fixture);
                let expected = format!("tests/fixtures/{}.expected.yaml", $fixture);
                fixture_roundtrip(&input, &expected, record_fixtures);
                Ok(())
            }
        };
    }

    #[test]
    fn test_empty_and_nested() {
        let input = r#"---
a:
  b:
    c: hello
  d: {}
e:
  - f
  - g
  - h: []"#;

        let noncompact_input = r#"---
a:
  b:
    c: hello
  d: {}
e:
  - f
  - g
  -
    h: []"#;

        assert_roundtrip(input);
        assert_roundtrip_noncompact(noncompact_input);
    }

    #[test]
    fn test_nested_arrays() {
        let input = r#"---
a:
  - b
  - - c
    - d
    - - e
      - f"#;

        assert_roundtrip(input);
    }

    #[test]
    fn test_deeply_nested_arrays() {
        let input = r#"---
a:
  - b
  - - c
    - d
    - - e
      - - f
      - - e"#;

        assert_roundtrip(input);
    }

    #[test]
    fn test_nested_hashes() {
        let input = r#"---
a:
  b:
    c:
      d:
        e: f"#;

        assert_roundtrip(input);
    }

    fixture_test!(test_emit_simple, "emitter/simple");

    fixture_test!(test_emit_complex, "emitter/complex");

    fixture_test!(test_emit_avoid_quotes, "emitter/avoid-quotes");

    fixture_test!(test_emit_quoted_bools, "emitter/quoted-bools");

    fixture_test!(test_comments_001, "emitter/comments-001");

    fixture_test!(test_comments_002, "emitter/comments-002");

    fixture_test!(test_comments_hash, "emitter/comments-hash");
    fixture_test!(test_comments_hash_deep, "emitter/comments-hash-deep");

    fixture_test!(test_comments_array, "emitter/comments-array");
    fixture_test!(test_comments_array_deep, "emitter/comments-array-deep");

    // Asserts the roundtrip result is the same than the input
    fn assert_roundtrip(input: &str) {
        assert_formatted(input, input, true)
    }

    fn assert_roundtrip_noncompact(input: &str) {
        assert_formatted(input, input, false)
    }

    // Asserts the input is formatted to the expected output
    fn assert_formatted(expected: &str, input: &str, compact: bool) {
        let docs = YamlLoader::load_from_str(input).unwrap();
        let first_doc = &docs[0];

        let mut output = String::new();
        let mut emitter = YamlEmitter::new(&mut output);
        emitter.compact(compact);
        emitter.dump(first_doc).unwrap();

        assert_eq!(expected, output)
    }

    fn fixture_roundtrip(input: &str, expected: &str, record: bool) {
        let input = fs::read_to_string(input).expect("cannot read input fixture");
        let loaded = YamlLoader::load_from_str(&input).expect("cannot load input fixture");
        let mut actual = String::new();
        YamlEmitter::new(&mut actual).dump(&loaded[0]).unwrap();

        if record {
            fs::write(expected, actual).expect("cannot record fixture");
        } else {
            let expected = fs::read_to_string(expected).expect("cannot read expected fixture");
            assert_eq!(expected, actual);
        }
    }
}

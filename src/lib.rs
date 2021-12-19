use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take},
    character::complete::{alpha1, alphanumeric1, one_of},
    combinator::opt,
    error::{context, ErrorKind, VerboseError},
    multi::{many1, many_m_n},
    sequence::{separated_pair, terminated, tuple},
    AsChar, Err as NomErr, IResult, InputTakeAtPosition,
};

#[allow(dead_code)]
type Res<T, U> = IResult<T, U, VerboseError<T>>;
type Authority<'a> = (&'a str, Option<&'a str>);
#[allow(dead_code)]
type QueryParam<'a> = (&'a str, &'a str);
#[allow(dead_code)]
type QueryParams<'a> = Vec<QueryParam<'a>>;

#[derive(Debug, PartialEq, Eq)]
pub struct URI<'a> {
    schema: Scheme,
    authority: Option<Authority<'a>>,
    host: Host,
    port: Option<u16>,
    path: Option<Vec<&'a str>>,
    query: Option<QueryParams<'a>>,
    fragment: Option<&'a str>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Scheme {
    HTTP,
    HTTPS,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Host {
    HOST(String),
    IP([u8; 4]),
}

impl From<&str> for Scheme {
    fn from(i: &str) -> Self {
        match i.to_lowercase().as_str() {
            "http://" => Scheme::HTTP,
            "https://" => Scheme::HTTPS,
            _ => unimplemented!("we do not support the given scheme"),
        }
    }
}

#[allow(dead_code)]
fn scheme(input: &str) -> Res<&str, Scheme> {
    context(
        "scheme",
        alt((tag_no_case("HTTP://"), tag_no_case("HTTPS://"))),
    )(input)
    .map(|(next_input, res)| (next_input, res.into()))
}

#[allow(dead_code)]
fn authority(input: &str) -> Res<&str, (&str, Option<&str>)> {
    context(
        "authority",
        terminated(
            separated_pair(alphanumeric1, opt(tag(":")), opt(alphanumeric1)),
            tag("@"),
        ),
    )(input)
}

#[allow(dead_code)]
fn host(input: &str) -> Res<&str, Host> {
    context(
        "host",
        alt((
            tuple((many1(terminated(alphanumerichyphen1, tag("."))), alpha1)),
            tuple((many_m_n(1, 1, alphanumerichyphen1), take(0 as usize))),
        )),
    )(input)
    .map(|(next_input, (mut list, single))| {
        if !single.is_empty() {
            list.push(single);
        }
        (next_input, Host::HOST(list.join(".")))
    })
}

#[allow(dead_code)]
fn alphanumerichyphen1<T>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-') && !char_item.is_alphanum()
        },
        ErrorKind::AlphaNumeric,
    )
}

fn ip_num(input: &str) -> Res<&str, u8> {
    context("ip number", n_to_m_digits(1, 3))(input).and_then(|(next_input, result)| {
        match result.parse::<u8>() {
            Ok(n) => Ok((next_input, n)),
            Err(_) => Err(NomErr::Error(VerboseError { errors: vec![] })),
        }
    })
}

fn n_to_m_digits<'a>(n: usize, m: usize) -> impl FnMut(&'a str) -> Res<&str, String> {
    move |input| {
        many_m_n(n, m, one_of("0123456789"))(input)
            .map(|(next_input, result)| (next_input, result.into_iter().collect()))
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use nom::{
        error::{ErrorKind, VerboseError, VerboseErrorKind},
        Err as NomErr,
    };

    #[test]
    fn test_n_to_m_digits() {
        let mut one_digit = n_to_m_digits(1, 1);
        assert_eq!(one_digit("123"), Ok(("23", String::from("1"))));
    }

    #[test]
    fn test_scheme() {
        assert_eq!(scheme("https://foobar"), Ok(("foobar", Scheme::HTTPS)));
        assert_eq!(scheme("http://foobar"), Ok(("foobar", Scheme::HTTP)));
        assert_eq!(
            scheme("baz://foobar"),
            Err(NomErr::Error(VerboseError {
                errors: vec![
                    ("baz://foobar", VerboseErrorKind::Nom(ErrorKind::Tag)),
                    ("baz://foobar", VerboseErrorKind::Nom(ErrorKind::Alt)),
                    ("baz://foobar", VerboseErrorKind::Context("scheme")),
                ]
            }))
        );
    }

    #[test]
    fn test_authority() {
        assert_eq!(
            authority("username:password@foobar.org"),
            Ok(("foobar.org", ("username", Some("password"))))
        );
        assert_eq!(
            authority("username@foobar.org"),
            Ok(("foobar.org", ("username", None)))
        );
        assert_eq!(
            authority("foobar.org"),
            Err(NomErr::Error(VerboseError {
                errors: vec![
                    (".org", VerboseErrorKind::Nom(ErrorKind::Tag)),
                    ("foobar.org", VerboseErrorKind::Context("authority")),
                ]
            }))
        );
    }

    #[test]
    fn test_host() {
        assert_eq!(
            host("localhost:8080"),
            Ok((":8080", Host::HOST(String::from("localhost")))),
        );
        assert_eq!(
            host("example.org:8080"),
            Ok((":8080", Host::HOST(String::from("example.org")))),
        );
        assert_eq!(
            host("some-subsite.example.org:8080"),
            Ok((
                ":8080",
                Host::HOST(String::from("some-subsite.example.org"))
            )),
        );
        assert_eq!(
            host("example.123"),
            Ok((".123", Host::HOST(String::from("example")))),
        );
        assert_eq!(
            host("$$$.com"),
            Err(NomErr::Error(VerboseError {
                errors: vec![
                    ("$$$.com", VerboseErrorKind::Nom(ErrorKind::AlphaNumeric)),
                    ("$$$.com", VerboseErrorKind::Nom(ErrorKind::ManyMN)),
                    ("$$$.com", VerboseErrorKind::Nom(ErrorKind::Alt)),
                    ("$$$.com", VerboseErrorKind::Context("host")),
                ]
            })),
        );
        assert_eq!(
            host(".com"),
            Err(NomErr::Error(VerboseError {
                errors: vec![
                    (".com", VerboseErrorKind::Nom(ErrorKind::AlphaNumeric)),
                    (".com", VerboseErrorKind::Nom(ErrorKind::ManyMN)),
                    (".com", VerboseErrorKind::Nom(ErrorKind::Alt)),
                    (".com", VerboseErrorKind::Context("host")),
                ]
            })),
        );
    }
}

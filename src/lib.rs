use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take},
    character::complete::{alpha1, alphanumeric1, one_of},
    combinator::opt,
    error::{context, ErrorKind, VerboseError},
    multi::{count, many0, many1, many_m_n},
    sequence::{preceded, separated_pair, terminated, tuple},
    AsChar, Err as NomErr, IResult, InputTakeAtPosition,
};

type Res<T, U> = IResult<T, U, VerboseError<T>>;
type Authority<'a> = (&'a str, Option<&'a str>);
type QueryParam<'a> = (&'a str, &'a str);
type QueryParams<'a> = Vec<QueryParam<'a>>;

#[derive(Debug, PartialEq, Eq)]
pub struct URI<'a> {
    scheme: Scheme,
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

fn scheme(input: &str) -> Res<&str, Scheme> {
    context(
        "scheme",
        alt((tag_no_case("HTTP://"), tag_no_case("HTTPS://"))),
    )(input)
    .map(|(next_input, res)| (next_input, res.into()))
}

fn authority(input: &str) -> Res<&str, (&str, Option<&str>)> {
    context(
        "authority",
        terminated(
            separated_pair(alphanumeric1, opt(tag(":")), opt(alphanumeric1)),
            tag("@"),
        ),
    )(input)
}

fn ip_or_host(input: &str) -> Res<&str, Host> {
    context("ip or host", alt((ip, host)))(input)
}

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

fn ip(input: &str) -> Res<&str, Host> {
    context(
        "ip",
        tuple((count(terminated(ip_num, tag(".")), 3), ip_num)),
    )(input)
    .map(|(next_input, (values, last_number))| {
        let mut result: [u8; 4] = [0, 0, 0, 0];
        values
            .into_iter()
            .enumerate()
            .for_each(|(i, v)| result[i] = v);
        result[3] = last_number;
        (next_input, Host::IP(result))
    })
}

fn ip_num(input: &str) -> Res<&str, u8> {
    context("ip number", n_to_m_digits(1, 3))(input).and_then(|(next_input, result)| {
        match result.parse::<u8>() {
            Ok(n) => Ok((next_input, n)),
            Err(_) => Err(NomErr::Error(VerboseError { errors: vec![] })),
        }
    })
}

fn port(input: &str) -> Res<&str, u16> {
    context("port", preceded(tag(":"), n_to_m_digits(2, 4)))(input).and_then(
        |(next_result, res)| match res.parse::<u16>() {
            Ok(n) => Ok((next_result, n)),
            Err(_) => Err(NomErr::Error(VerboseError { errors: vec![] })),
        },
    )
}

fn n_to_m_digits<'a>(n: usize, m: usize) -> impl FnMut(&'a str) -> Res<&str, String> {
    move |input| {
        many_m_n(n, m, one_of("0123456789"))(input)
            .map(|(next_input, result)| (next_input, result.into_iter().collect()))
    }
}

fn path(input: &str) -> Res<&str, Vec<&str>> {
    context(
        "path",
        tuple((
            tag("/"),
            many0(terminated(url_code_points, tag("/"))),
            opt(url_code_points),
        )),
    )(input)
    .map(|(next_input, res)| {
        let mut path: Vec<&str> = res.1.iter().map(|p| p.to_owned()).collect();
        if let Some(last) = res.2 {
            path.push(last);
        }
        (next_input, path)
    })
}

fn url_code_points<T>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            char_item != '-' && !char_item.is_alphanum() && char_item != '.'
        },
        ErrorKind::AlphaNumeric,
    )
}

fn query_params(input: &str) -> Res<&str, QueryParams> {
    context(
        "query_params",
        tuple((
            tag("?"),
            url_code_points,
            tag("="),
            url_code_points,
            many0(tuple((
                tag("&"),
                url_code_points,
                tag("="),
                url_code_points,
            ))),
        )),
    )(input)
    .map(
        |(next_input, (_, first_param, _, first_value, remaning_query_params))| {
            let mut qps = Vec::new();
            qps.push((first_param, first_value));
            for (_, param, _, value) in remaning_query_params {
                qps.push((param, value))
            }
            (next_input, qps)
        },
    )
}

fn fragment(input: &str) -> Res<&str, &str> {
    context("fragment", tuple((tag("#"), url_code_points)))(input)
        .map(|(next_input, res)| (next_input, res.1))
}

pub fn uri(input: &str) -> Res<&str, URI> {
    context(
        "uri",
        tuple((
            scheme,
            opt(authority),
            ip_or_host,
            opt(port),
            opt(path),
            opt(query_params),
            opt(fragment),
        )),
    )(input)
    .map(|(next_input, res)| {
        let (scheme, authority, host, port, path, query, fragment) = res;
        (
            next_input,
            URI {
                scheme,
                authority,
                host,
                port,
                path,
                query,
                fragment,
            },
        )
    })
}

#[cfg(test)]
mod tests {

    use super::*;
    use nom::{
        error::{ErrorKind, VerboseError, VerboseErrorKind},
        Err as NomErr,
    };

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

    #[test]
    fn test_n_to_m_digits() {
        let mut one_digit = n_to_m_digits(1, 1);
        assert_eq!(one_digit("123"), Ok(("23", String::from("1"))));
    }

    #[test]
    fn test_ipv4() {
        assert_eq!(
            ip("192.168.0.1:8080"),
            Ok((":8080", Host::IP([192, 168, 0, 1])))
        );

        assert_eq!(ip("0.0.0.0:8080"), Ok((":8080", Host::IP([0, 0, 0, 0]))));

        assert_eq!(
            ip("1924.168.0.1:8080"),
            Err(NomErr::Error(VerboseError {
                errors: vec![
                    ("4.168.0.1:8080", VerboseErrorKind::Nom(ErrorKind::Tag)),
                    ("1924.168.0.1:8080", VerboseErrorKind::Nom(ErrorKind::Count)),
                    ("1924.168.0.1:8080", VerboseErrorKind::Context("ip")),
                ]
            }))
        );
    }

    #[test]
    fn test_port() {
        assert_eq!(port(":8080"), Ok(("", 8080)));
    }

    #[test]
    fn test_path() {
        assert_eq!(path("/a/b/c?d"), Ok(("?d", vec!["a", "b", "c"])));
        assert_eq!(path("/a/b/c/?d"), Ok(("?d", vec!["a", "b", "c"])));
        assert_eq!(path("/a/b-c-d/c?d"), Ok(("?d", vec!["a", "b-c-d", "c"])));
        assert_eq!(path("/a/1234/c?d"), Ok(("?d", vec!["a", "1234", "c"])));
        assert_eq!(
            path("/a/1234/c.txt?d"),
            Ok(("?d", vec!["a", "1234", "c.txt"]))
        );
    }

    #[test]
    fn test_query_params() {
        assert_eq!(
            query_params("?foo=5&bar=foobar#baz"),
            Ok(("#baz", vec![("foo", "5"), ("bar", "foobar")]))
        );
        assert_eq!(
            query_params("?foo-bar=bar-foo#baz"),
            Ok(("#baz", vec![("foo-bar", "bar-foo")]))
        );
    }

    #[test]
    fn test_fragment() {
        assert_eq!(fragment("#foobar"), Ok(("", "foobar")));
        assert_eq!(fragment("#foo-bar"), Ok(("", "foo-bar")));
    }

    #[test]
    fn test_uri() {
        assert_eq!(
            uri("https://www.github.com/tandolf"),
            Ok((
                "",
                URI {
                    scheme: Scheme::HTTPS,
                    authority: None,
                    host: Host::HOST("www.github.com".to_string()),
                    port: None,
                    path: Some(vec!["tandolf"]),
                    query: None,
                    fragment: None
                }
            ))
        )
    }

    #[test]
    fn test_complex_uri() {
        assert_eq!(
            uri("http://thomas:password123@localhost:8080/tandolf?foobar=foo&bar=baz#home"),
            Ok((
                "",
                URI {
                    scheme: Scheme::HTTP,
                    authority: Some(("thomas", Some("password123"))),
                    host: Host::HOST("localhost".to_string()),
                    port: Some(8080),
                    path: Some(vec!["tandolf"]),
                    query: Some(vec![("foobar", "foo"), ("bar", "baz")]),
                    fragment: Some("home")
                }
            ))
        )
    }
}

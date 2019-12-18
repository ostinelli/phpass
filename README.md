[![Build Status](https://travis-ci.org/ostinelli/phpass.svg?branch=master)](https://travis-ci.org/ostinelli/phpass) [![Hex pm](https://img.shields.io/hexpm/v/phpass.svg)](https://hex.pm/packages/phpass)

# PHPass
PHPass is an Erlang port of the [Portable PHP password hashing framework](https://www.openwall.com/phpass/). Only the `md5` hashing method is currently supported.

## Setup

### For Elixir
Add it to your deps:

```elixir
defp deps do
  [{:phpass, "0.1.0"}]
end
```

Ensure that `phpass` is started with your application, for example by adding it to the list of your application's `extra_applications`:

```elixir
def application do
  [
    extra_applications: [:logger, :phpass]
  ]
end
```

### For Erlang
If you're using [rebar3](https://github.com/erlang/rebar3), add `phpass` as a dependency in your project's `rebar.config` file:

```erlang
{deps, [
  {phpass, {git, "git://github.com/ostinelli/phpass.git", {tag, "0.1.0"}}}
]}.
```
Or, if you're using [Hex.pm](https://hex.pm/) as package manager (with the [rebar3_hex](https://github.com/hexpm/rebar3_hex) plugin):

```erlang
{deps, [
  {phpass, "0.1.0"}
]}.
```

Ensure that `phpass` is started with your application, for example by adding it in your `.app` file to the list of `applications`:

```erlang
{application, my_app, [
    %% ...
    {applications, [
        kernel,
        stdlib,
        sasl,
        phpass,
        %% ...
    ]},
    %% ...
]}.
```

## API
Example code here below is in Erlang. Thanks to Elixir interoperability with Erlang, the equivalent code in Elixir is straightforward.

To hash a password:

```erlang
phpass:hash(Password) ->
    phpass:hash(Password, 13).
```

```erlang
phpass:hash(Password, Rounds) -> Hash.

Types:
    Password = binary()
    Rounds = non_neg_integer()
    Hash = binary()
```

To check a password:

```erlang
phpass:check(Password, Hash) -> boolean().

Types:
    Password = binary()
    Hash = binary()
```

## Implementation details
This is a port of the ruby implementation [phpass-ruby](https://github.com/uu59/phpass-ruby). See original PHPass at [Openwall](https://www.openwall.com/phpass/).


## Contributing
So you want to contribute? That's great! Please follow the guidelines below. It will make it easier to get merged in.

Before implementing a new feature, please submit a ticket to discuss what you intend to do. Your feature might already be in the works, or an alternative implementation might have already been discussed.

Do not commit to master in your fork. Provide a clean branch without merge commits. Every pull request should have its own topic branch. In this way, every additional adjustments to the original pull request might be done easily, and squashed with `git rebase -i`. The updated branch will be visible in the same pull request, so there will be no need to open new pull requests when there are changes to be applied.

Ensure that proper testing is included. To run tests you simply have to be in the project's root directory and run:

```bash
$ make test
```

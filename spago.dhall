{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "splitmix"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "int64"
  , "integers"
  , "maybe"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "quickcheck"
  , "spec"
  , "tailrec"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MPL-2.0"
, repository = "https://github.com/the-dr-lazy/purescript-splitmix"
}

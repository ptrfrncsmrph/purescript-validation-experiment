**NOTE:** This repo is a fork of [jkachmar/purescript-validation-experiment](https://github.com/jkachmar/purescript-validation-experiment). Notable changes:

1. Migrated from `bower` to `psc-package`
2. Updated from PureScript `0.11.7` to `0.12.0`

To do:

3. Replace regular expression validation with parsers (using `string-parsers` library)
4. Add simple HTML form to try in browser

# purescript-validation-experiment

## What Is this
This repo is just a little test of the `purescript-validation` library, mainly
for my own edification, but also hopefully to give an example of how both the
`Semigroup` and `Semiring` versions of `Data.Validation`'s `V` type can prove
useful when validating input.

## How Do I Use This?
First, ensure that you have `node` and `npm` installed.

Then, clone this repo...

    git clone git@github.com:ptrfrncsmrph/purescript-validation-experiment.git

...install the dependencies and build the project...

    npm install

...and run the example...

    npm run validate

...which should print the following to the console:

    Semigroup Validation:
    invalid ([(BadEmail [EmptyField,InvalidEmailAddress]),(BadPassword [EmptyField,NoSpecialCharacter,LessThanMinLength])])
    invalid ([(BadEmail [InvalidEmailAddress]),(BadPassword [NoSpecialCharacter])])
    invalid ([(BadPassword [NoSpecialCharacter])])
    invalid ([(BadPassword [LessThanMinLength])])


    Semiring Validation:
    invalid ([(BadContact [EmptyField,InvalidEmailAddress,InvalidPhoneNumber]),(BadPassword [EmptyField,NoSpecialCharacter,LessThanMinLength])])
    invalid ([(BadContact [InvalidEmailAddress,InvalidPhoneNumber]),(BadPassword [NoSpecialCharacter])])
    invalid ([(BadPassword [NoSpecialCharacter])])
    invalid ([(BadPassword [LessThanMinLength])])
    invalid ([(BadContact [InvalidEmailAddress,InvalidPhoneNumber])])
    pure ("{\"contact\":{\"value0\":\"good@email.com\"},\"password\":\"abc123+-=\"}")
    pure ("{\"contact\":{\"value0\":\"+1 (555) 555-5555\"},\"password\":\"abc123+-=\"}")

Note that the `Semiring` validator has a record with a `value0` key in its
`Valid` results. This is due to the unsafe stringification of `Contact`, which
is a sum type.

## What's Going On Here?
To see a relatively straightforward validator whose `Applicative` instance uses
`Semigroup` to accumulate errors, check out
[`src/Semigroup`](src/Semigroup.purs).

To see a relatively straightforward validator that uses `Semigroup` to
accumulate errors while performing validation with the `Applicative` instance,
`Semiring` to accumulate errors while validation with the `Alt` instance, check
out [`src/Semiring`](src/Semiring.purs).

module Semigroup where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Bifunctor as Bifunctor
import Data.Either as Either
import Data.Foldable as Foldable
import Data.Generic.Rep as Generic
import Data.Generic.Rep.Show as Generic.Show
import Data.List.NonEmpty as NonEmptyList
import Data.String as String
import Data.String.CodeUnits as String.CodeUnits
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.Validation.Semigroup as Validation
import Effect (Effect)
import Effect.Console as Console
import Partial.Unsafe as Partial
import Text.Parsing.StringParser as StringParser
import Text.Parsing.StringParser.CodeUnits as CodeUnits
import Text.Parsing.StringParser.Combinators as Combinators

--------------------------------------------------------------------------------
-- | Utility function to unsafely construct a regular expression from a pattern
-- | string.
-- |
-- | This will fail at runtime with an error if the pattern string is invalid.
unsafeRegexFromString :: String -> Regex.Regex
unsafeRegexFromString str =
  let regex = Regex.regex str Regex.Flags.noFlags
  in Partial.unsafePartial (Either.fromRight regex)

--------------------------------------------------------------------------------
-- | Regular expression for email address validation.
emailRegex :: Regex.Regex
emailRegex =
  unsafeRegexFromString "^\\w+([.-]?\\w+)*@\\w+([.-]?\\w+)*(\\.\\w{2,3})+$"

-- | Regular expression for special symbols.
passwordRegex :: Regex.Regex
passwordRegex = unsafeRegexFromString "\\W"

-- | Minimum password length.
passwordMinLength :: Int
passwordMinLength = 8

--------------------------------------------------------------------------------
-- | Email parser
parseEmail :: StringParser.Parser String
parseEmail = StringParser.try $ do
  firstLetter <- CodeUnits.alphaNum
  restOfAddr <- Combinators.many $ CodeUnits.alphaNum <|> CodeUnits.char '.'
  _ <- CodeUnits.char '@'
  domain <- Combinators.many1 $ CodeUnits.alphaNum <|> CodeUnits.char '.'
  let full' = 
        pure firstLetter 
        <> restOfAddr 
        <> pure '@' 
        <> NonEmptyList.toList domain
  let full = Foldable.foldMap String.CodeUnits.singleton full'
  pure full

--------------------------------------------------------------------------------
data ValidationError
  = EmptyField
  | InvalidEmailAddress
  | NoSpecialCharacter
  | LessThanMinLength

-- | Derive a `Generic` instance for `ValidationError` so we can get a
-- | `Show` instance to print to the console.
derive instance genericValidationError :: Generic.Generic ValidationError _

-- | Derive `show` for `ValidationError` using the `Generic` instance.
instance showValidationError :: Show ValidationError where
  show = Generic.Show.genericShow

type ValidationErrors = NonEmptyList.NonEmptyList ValidationError

--------------------------------------------------------------------------------
-- | Validate that the field of a form is non-empty.
validateNonEmpty :: String -> Validation.V ValidationErrors String
validateNonEmpty str
  | String.null str = Validation.invalid $ NonEmptyList.singleton EmptyField
  | otherwise = pure str

-- | Validate that the field of a form is a valid email address.
validateEmailRegex :: String -> Validation.V ValidationErrors String
validateEmailRegex email
  | Regex.test emailRegex email = pure email
  | otherwise = Validation.invalid $ NonEmptyList.singleton InvalidEmailAddress

-- | Validate that the field of a form is a valid email address.
-- | TODO: Make this actually do something instead of just Left/Right
-- |   check on parser result
validateEmailWithParser :: String -> Validation.V ValidationErrors String
validateEmailWithParser email =
  case (StringParser.runParser parseEmail email) of
    Either.Left _ -> Validation.invalid $ NonEmptyList.singleton InvalidEmailAddress
    Either.Right _ -> pure email

-- | Validate that the field of a form has at least one special character.
validatePasswordRegex :: String -> Validation.V ValidationErrors String
validatePasswordRegex password
  | Regex.test passwordRegex password = pure password
  | otherwise = Validation.invalid $ NonEmptyList.singleton NoSpecialCharacter

-- | Validate that the field of a form is longer than `passwordMinLength`.
validatePasswordMinLength :: String -> Validation.V ValidationErrors String
validatePasswordMinLength password
  | String.length password > passwordMinLength = pure password
  | otherwise = Validation.invalid $ NonEmptyList.singleton LessThanMinLength

--------------------------------------------------------------------------------
-- | Sum type containing errors we could potentially encounter while validating
-- | the form.
data FormErrorF a
  = BadEmail a
  | BadPassword a

-- | Derive a `Functor` instance for `FormErrorF` so we can `map` into it.
derive instance functorFormErrorF :: Functor FormErrorF

-- | Derive a `Generic` instance for `FormErrorF` so we can get a
-- | `Show` instance to print to the console.
derive instance genericFormErrorF :: Generic.Generic (FormErrorF a) _

-- | Derive `show` for `FormErrorF` using the `Generic` instance.
instance showFormErrorF :: Show a => Show (FormErrorF a) where
  show = Generic.Show.genericShow

-- | Type alias for a simple `FormError`, containing only `ValidationErrors`.
type FormError = FormErrorF ValidationErrors

-- | Type alias for a non-empty list of `FormError`s.
type FormErrors = NonEmptyList.NonEmptyList FormError

--------------------------------------------------------------------------------
-- | Newtype wrapper for a form's email field
newtype Email = Email String

-- | Derive a `Generic` instance for `Email` so we can get a
-- | `Show` instance to print to the console.
derive instance genericEmail :: Generic.Generic Email _

-- | Derive `show` for `Email` using the `Generic` instance.
instance showEmail :: Show Email where
  show = Generic.Show.genericShow

-- | Validate that the field of a form is non-empty and has a valid email
-- | address.
validateEmail :: String -> Validation.V FormError Email
validateEmail email =
  Bifunctor.bimap BadEmail Email
  $  validateNonEmpty email
  *> validateEmailWithParser email

-- | Newtype wrapper for a form's password field
newtype Password = Password String

-- | Derive a `Generic` instance for `Password` so we can get a
-- | `Show` instance to print to the console.
derive instance genericPassword :: Generic.Generic Password _

-- | Derive `show` for `Password` using the `Generic` instance.
instance showPassword :: Show Password where
  show = Generic.Show.genericShow

-- | Validate that the field of a form is non-empty, has at least one special
-- | character, and is longer than `passwordMinLength`.
validatePassword :: String -> Validation.V FormError Password
validatePassword password =
  Bifunctor.bimap BadPassword Password
  $  validateNonEmpty password
  *> validatePasswordRegex password
  *> validatePasswordMinLength password

--------------------------------------------------------------------------------
-- | Type alias for an unvalidated version of our simple form, note how the
-- | email and password fields are simple strings.
type UnvalidatedForm =
  { email    :: String
  , password :: String
  }

-- | Type alias for a validated version of our simple form, note how the email
-- | and password fields are wrapped in newtypes.
type ValidatedForm =
  { email    :: Email
  , password :: Password
  }

-- | Validate that a form contains a valid email and a valid password.
validateForm :: UnvalidatedForm -> Validation.V FormErrors ValidatedForm
validateForm {email, password} = {email: _, password: _}
  <$> (Bifunctor.lmap NonEmptyList.singleton $ validateEmail email)
  <*> (Bifunctor.lmap NonEmptyList.singleton $ validatePassword password)

--------------------------------------------------------------------------------
-- | An empty form; this will parse as invalid.
testForm1 :: UnvalidatedForm
testForm1 = {email: "", password: ""}

-- | A form with a bad email and a bad password; invalid.
testForm2 :: UnvalidatedForm
testForm2 = {email: "bademail", password: "badpassword"}

-- | A form with a good email and a bad password; invalid.
testForm3 :: UnvalidatedForm
testForm3 = {email: "good@email.com", password: "badpassword"}

-- | A form with a good email and a password that is too short; invalid.
testForm4 :: UnvalidatedForm
testForm4 = {email: "good@email.com", password: "abc123+"}

-- | A form with a good email and a good password; valid.
testForm5 :: UnvalidatedForm
testForm5 = {email: "good@email.com", password: "abc123+-="}

--------------------------------------------------------------------------------
-- | Run a form validation against all of the test forms we created, formatting
-- | the output and printing it to the console.
main :: Effect Unit
main = 
  mapM_ (Console.logShow <<< formatValidationOutput <<< validateForm) 
    $ [ testForm1
      , testForm2
      , testForm3
      , testForm4
      , testForm5
      ]
  where
    mapM_ f = Foldable.sequence_ <<< map f
    formatValidationOutput =
      Bifunctor.bimap
      (Array.fromFoldable <<< ((map <<< map) (Array.fromFoldable)))
      show
module Semiring where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Bifunctor as Bifunctor
import Data.Either as Either
import Data.Generic.Rep as Generic
import Data.Generic.Rep.Eq as Generic.Eq
import Data.Generic.Rep.Ord as Generic.Ord
import Data.Generic.Rep.Show as Generic.Show
import Data.Semiring.Free as Semiring.Free
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.Validation.Semiring as Validation
import Effect (Effect)
import Effect.Console as Console
import Partial.Unsafe as Partial

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

phoneNumberRegex :: Regex.Regex
phoneNumberRegex =
  unsafeRegexFromString """^(?:(?:\+?1\s*(?:[.-]\s*)?)?(?:\(\s*([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9])\s*\)|([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9]))\s*(?:[.-]\s*)?)?([2-9]1[02-9]|[2-9][02-9]1|[2-9][02-9]{2})\s*(?:[.-]\s*)?([0-9]{4})(?:\s*(?:#|x\.?|ext\.?|extension)\s*(\d+))?$"""

-- | Regular expression for special symbols.
passwordRegex :: Regex.Regex
passwordRegex = unsafeRegexFromString "\\W"

-- | Minimum password length.
passwordMinLength :: Int
passwordMinLength = 8

--------------------------------------------------------------------------------
-- | Sum type representing individual string validations that could fail.
data ValidationError
  = EmptyField
  | InvalidEmailAddress
  | InvalidPhoneNumber
  | NoSpecialCharacter
  | LessThanMinLength

-- | Derive a `Generic` instance for `ValidationError` so we can get a
-- | `Show` instance to print to the console and an `Eq` instance to eliminate
-- | duplicate `ValidationError`s for the `Semiring` instance of `Validation`.
derive instance genericValidationError :: Generic.Generic ValidationError _

-- | Derive an `Eq` instance for `ValidationError` using the `Generic` instance.
instance eqValidationError :: Eq ValidationError where
  eq = Generic.Eq.genericEq

-- | Derive an `Ord` instance for `ValidationError` using the `Generic` instance.
instance ordValidationError :: Ord ValidationError where
  compare = Generic.Ord.genericCompare

-- | Derive `show` for `ValidationError` using the `Generic` instance.
instance showValidationError :: Show ValidationError where
  show = Generic.Show.genericShow

type ValidationErrors = Semiring.Free.Free ValidationError

--------------------------------------------------------------------------------
-- | Validate that the field of a form is non-empty.
validateNonEmpty :: String -> Validation.V ValidationErrors String
validateNonEmpty str
  | String.null str = Validation.invalid $ Semiring.Free.free EmptyField
  | otherwise = pure str

-- | Validate that the field of a form is a valid email address.
validateEmailRegex :: String -> Validation.V ValidationErrors String
validateEmailRegex email
  | Regex.test emailRegex email = pure email
  | otherwise = Validation.invalid $ Semiring.Free.free InvalidEmailAddress

-- | Validate that the field of a form has at least one special character.
validatePasswordRegex :: String -> Validation.V ValidationErrors String
validatePasswordRegex password
  | Regex.test passwordRegex password = pure password
  | otherwise = Validation.invalid $ Semiring.Free.free NoSpecialCharacter

-- | Validate that the field of a form is longer than `passwordMinLength`.
validatePasswordMinLength :: String -> Validation.V ValidationErrors String
validatePasswordMinLength password
  | String.length password > passwordMinLength = pure password
  | otherwise = Validation.invalid $ Semiring.Free.free LessThanMinLength

validatePhoneNumberRegex :: String -> Validation.V ValidationErrors String
validatePhoneNumberRegex phoneNumber
  | Regex.test phoneNumberRegex phoneNumber = pure phoneNumber
  | otherwise = Validation.invalid $ Semiring.Free.free InvalidPhoneNumber

--------------------------------------------------------------------------------
-- | Sum type containing errors we could potentially encounter while validating
-- | the form.
data FormErrorF a
  = BadContact a
  | BadPassword a

-- | Derive a `Functor` instance for `FormErrorF` so we can `map` into it.
derive instance functorFormErrorF :: Functor FormErrorF

-- | Derive a `Generic` instance for `FormErrorF` so we can get a
-- | `Show` instance to print to the console.
derive instance genericFormErrorF :: Generic.Generic (FormErrorF a) _

-- | Derive `show` for `FormError'` using the `Generic` instance.
instance showFormErrorF :: Show a => Show (FormErrorF a) where
  show = Generic.Show.genericShow

-- | Type alias for a simple `FormError`, containing only `ValidationErrors`.
type FormError = FormErrorF ValidationErrors

-- | Type alias for a free semiring of `FormError`s, giving us both
-- | `Applicative` and `Alternative` instances for validation.
type FormErrors = Semiring.Free.Free (FormErrorF ValidationErrors)

--------------------------------------------------------------------------------
-- | Sum type representing a form's possible contact information.
data Contact
  = Email String
  | PhoneNumber String

-- | Derive a `Generic` instance for `Contact` so we can get a
-- | `Show` instance to print to the console.
derive instance genericContact :: Generic.Generic Contact _

-- | Derive `show` for `Contact` using the `Generic` instance.
instance showContact :: Show Contact where
  show = Generic.Show.genericShow

-- | Validate that the field of a form is non-empty and has a valid email
-- | address.
validateEmail :: String -> Validation.V ValidationErrors Contact
validateEmail email =
  map Email
  $  validateNonEmpty email
  *> validateEmailRegex email

-- | Validate that the field of a form is non-empty and has a valid phone
-- | number.
validatePhoneNumber :: String -> Validation.V ValidationErrors Contact
validatePhoneNumber phoneNumber =
  map PhoneNumber
  $  validateNonEmpty phoneNumber
  *> validatePhoneNumberRegex phoneNumber

-- | Validate that the field of a form is non-empty and has EITHER a valid email
-- | address OR a valid phone number.
validateContact :: String -> Validation.V FormError Contact
validateContact contact = Bifunctor.lmap BadContact $
  (validateEmail contact <|> validatePhoneNumber contact)

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
-- | contact and password fields are simple strings.
type UnvalidatedForm =
  { contact  :: String
  , password :: String
  }

-- | Type alias for a validated version of our simple form, note how the contact
-- | and password fields are wrapped in newtypes.
type ValidatedForm =
  { contact  :: Contact
  , password :: Password
  }

-- | Validate that a form contains a valid contact and a valid password.
validateForm :: UnvalidatedForm -> Validation.V FormErrors ValidatedForm
validateForm {contact, password} = {contact: _, password: _}
  <$> (Bifunctor.lmap Semiring.Free.free $ validateContact contact)
  <*> (Bifunctor.lmap Semiring.Free.free $ validatePassword password)

--------------------------------------------------------------------------------
-- | An empty form; this will parse as invalid.
testForm1 :: UnvalidatedForm
testForm1 = {contact: "", password: ""}

-- | A form with a bad email and a bad password; invalid.
testForm2 :: UnvalidatedForm
testForm2 = {contact: "bademail", password: "badpassword"}

-- | A form with a good email and a bad password; invalid.
testForm3 :: UnvalidatedForm
testForm3 = {contact: "good@email.com", password: "badpassword"}

-- | A form with a good email and a password that is too short; invalid.
testForm4 :: UnvalidatedForm
testForm4 = {contact: "good@email.com", password: "abc123+"}

-- | A form with a bad phone number and a good password; invalid.
testForm5 :: UnvalidatedForm
testForm5 = {contact: "55-5555", password: "abc123+-="}

-- | A form with a good email and a good password; valid.
testForm6 :: UnvalidatedForm
testForm6 = {contact: "good@email.com", password: "abc123+-="}

-- | A form with a good phone number and a good password; valid.
testForm7 :: UnvalidatedForm
testForm7 = {contact: "+1 (555) 555-5555", password: "abc123+-="}

--------------------------------------------------------------------------------
-- | Run a form validation against all of the test forms we created, formatting
-- | the output and printing it to the console.
main :: Effect Unit
main = do
  Console.logShow $ formatValidationOutput $ validateForm testForm1
  -- > Invalid ([(BadContact [EmptyField,InvalidEmailAddress,InvalidPhoneNumber]),(BadPassword [EmptyField,NoSpecialCharacter,LessThanMinLength])])

  Console.logShow $ formatValidationOutput $ validateForm testForm2
  -- > Invalid ([(BadContact [InvalidEmailAddress,InvalidPhoneNumber]),(BadPassword [NoSpecialCharacter])])

  Console.logShow $ formatValidationOutput $ validateForm testForm3
  -- > Invalid ([(BadPassword [NoSpecialCharacter])])

  Console.logShow $ formatValidationOutput $ validateForm testForm4
  -- > Invalid ([(BadPassword [LessThanMinLength])])

  Console.logShow $ formatValidationOutput $ validateForm testForm5
  -- > Invalid ([(BadContact [InvalidEmailAddress,InvalidPhoneNumber])])

  Console.logShow $ formatValidationOutput $ validateForm testForm6
  -- > Valid ("{\"contact\":{\"value0\":\"good@email.com\"},\"password\":\"abc123+-=\"}")
  -- NOTE: The `value0` here is an unsafe stringification of the `Contact` type

  Console.logShow $ formatValidationOutput $ validateForm testForm7
  -- > Valid ("{\"contact\":{\"value0\":\"+1 (555) 555-5555\"},\"password\":\"abc123+-=\"}")
  -- NOTE: The `value0` here is an unsafe stringification of the `Contact` type

  where
    -- Format the output of our validator.
    formatValidationOutput =
      Bifunctor.bimap
      -- Convert the Free Semiring of `ValidationError` to an `Array`, eliminate
      -- any duplicate validation errors, and convert the `NonEmptyList` of
      -- `FormError`s to an `Array` too for easier printing
      (Array.fromFoldable <<< ((map <<< map) (Array.nub <<< Array.fromFoldable)))
      show

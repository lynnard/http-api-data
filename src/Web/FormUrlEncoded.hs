-- |
-- Convert Haskell values to and from @application/xxx-form-urlencoded@ format.
module Web.FormUrlEncoded (
  -- * Classes
  ToForm (..),
  FromForm (..),

  -- ** Keys for 'Form' entries
  ToFormKey(..),
  FromFormKey(..),

  -- * Encoding and decoding @'Form'@s
  urlEncodeAsForm,
  urlDecodeAsForm,

  urlEncodeForm,
  urlDecodeForm,

  -- * 'Generic's
  genericToForm,
  genericFromForm,

  -- * Helpers
  toEntriesByKey,
  fromEntriesByKey,

  lookupKey,
  lookupUnique,
  parseUnique,
  parseAll,
) where

import Web.Internal.FormUrlEncoded


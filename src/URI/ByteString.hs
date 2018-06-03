module URI.ByteString
  ( urlEncodeQuery
  , urlDecodeQuery
  ) where

import           Blaze.ByteString.Builder           (Builder)
import qualified Blaze.ByteString.Builder           as BB
import           Data.Bits
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as BS
import           Data.Char                          (ord)
import           Data.Maybe
import           Data.Word

urlEncodeQuery :: ByteString -> Builder
urlEncodeQuery = urlEncode unreserved8

urlDecodeQuery :: ByteString -> ByteString
urlDecodeQuery = urlDecode plusToSpace
  where
    plusToSpace = True

-- | Percent-encoding for URLs. Specify a list of additional
-- unreserved characters to permit.
urlEncode :: [Word8] -> ByteString -> Builder
urlEncode extraUnreserved = mconcat . map encodeChar . BS.unpack
    where
      encodeChar ch | unreserved' ch = BB.fromWord8 ch
                    | otherwise     = h2 ch

      unreserved' ch | ch >= 65 && ch <= 90  = True -- A-Z
                    | ch >= 97 && ch <= 122 = True -- a-z
                    | ch >= 48 && ch <= 57  = True -- 0-9
      unreserved' c = c `elem` extraUnreserved

      h2 v = let (a, b) = v `divMod` 16 in bs $ BS.pack [37, h a, h b] -- percent (%)
      h i | i < 10    = 48 + i -- zero (0)
          | otherwise = 65 + i - 10 -- 65: A


unreserved8 :: [Word8]
unreserved8 = map ord8 unreserved


-- | This function was extracted from the @http-types@ package. The
-- license can be found in licenses/http-types/LICENSE
urlDecode
    :: Bool
    -- ^ Whether to decode '+' to ' '
    -> BS.ByteString
    -> BS.ByteString
urlDecode replacePlus z = fst $ BS.unfoldrN (BS.length z) go z
  where
    go bs' =
        case BS.uncons bs' of
            Nothing -> Nothing
            Just (43, ws) | replacePlus -> Just (32, ws) -- plus to space
            Just (37, ws) -> Just $ fromMaybe (37, ws) $ do -- percent
                (x, xs) <- BS.uncons ws
                x' <- hexVal x
                (y, ys) <- BS.uncons xs
                y' <- hexVal y
                Just (combine x' y', ys)
            Just (w, ws) -> Just (w, ws)
    hexVal w
        | 48 <= w && w <= 57  = Just $ w - 48 -- 0 - 9
        | 65 <= w && w <= 70  = Just $ w - 55 -- A - F
        | 97 <= w && w <= 102 = Just $ w - 87 -- a - f
        | otherwise = Nothing
    combine :: Word8 -> Word8 -> Word8
    combine a b = shiftL a 4 .|. b

bs :: ByteString -> Builder
bs = BB.fromByteString

ord8 :: Char -> Word8
ord8 = fromIntegral . ord

-------------------------------------------------------------------------------
-- Very important!  When concatenating this to other strings to make larger
-- character classes, you must put this at the end because the '-' character
-- is treated as a range unless it's at the beginning or end.
unreserved :: String
unreserved = alphaNum ++ "~._-"

-------------------------------------------------------------------------------
alphaNum :: String
alphaNum = alpha ++ digit


-------------------------------------------------------------------------------
alpha :: String
alpha = "a-zA-Z"


-------------------------------------------------------------------------------
digit :: String
digit = "0-9"

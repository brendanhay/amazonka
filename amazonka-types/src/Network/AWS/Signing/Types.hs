{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Signing.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Signing.Types where

import           Data.ByteString.Builder
import           Data.ByteString.Builder.Extra
import           Data.ByteString.Char8         (ByteString)
import qualified Data.ByteString.Char8         as BS
import           Data.ByteString.Internal      (c2w)
import qualified Data.ByteString.Lazy          as LBS
import           Data.Char
import qualified Data.Foldable                 as Fold
import           Data.Monoid
import           Data.Time
import           Data.Word                     (Word8)
import           Network.AWS.Types

data Signed v = Signed (Request ())

type Signer v = Auth -> Region -> UTCTime -> Signed v

-- How to deal with additional context such as md5s?

class SigningAlgorithm v where
    finalise :: Service a v -> Request a -> Signer v

sign :: (AWSRequest a, AWSService (Sv a), SigningAlgorithm (Sg (Sv a)))
     => a
     -> Signer (Sg (Sv a))
sign = finalise service . request

-- | URI encode every byte. Uri-Encode() must enforce the following rules:
--
-- URI encode every byte except the unreserved characters: 'A'-'Z', 'a'-'z',
-- '0'-'9', '-', '.', '_', and '~'.
--
-- The space character is a reserved character and must be encoded as "%20"
-- (and not as "+").
--
-- Each Uri-encoded byte is formed by a '%' and the two-digit hexadecimal
-- value of the byte.
--
-- Letters in the hexadecimal value must be uppercase, for example "%1A".
-- Encode the forward slash character, '/', everywhere except in the object key name.
-- For example, if- the object key name is photos/Jan/sample.jpg, the forward
-- slash in the key name is not encoded.
--
-- The following is an example uri-encode() function in Java.
--
-- public static String uri-encode(CharSequence input, boolean encodeSlash) {
--   StringBuilder result = new StringBuilder();
--   for (int i = 0; i < input.length(); i++) {
--     char ch = input.charAt(i);
--     if ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9') || ch == '_' || ch == '-' || ch == '~' || ch == '.') {
--         result.append(ch);
--     } else if (ch == '/') {
--         result.append(encodeSlash ? "%2F" : ch);
--     } else {
--         result.append(toHexUTF8(ch));
--     }
--   }
--   return result.toString();
-- }
encodeURI :: Bool -> ByteString -> ByteString
encodeURI p = LBS.toStrict . build . BS.foldr' (mappend . enc) mempty
  where
    build = toLazyByteStringWith (untrimmedStrategy 128 smallChunkSize) mempty

    enc ' '              = "%20"
    enc '/' | p          = "%2F"
    enc  c  | reserved c = char8 c
    enc  c               = char2hex c

    reserved c =
            isAsciiUpper c
         || isAsciiLower c
         || isDigit c
         || c `elem` "-_.~/"

    char2hex c = let (a, b) = c2w c `divMod` 16
                  in word8 37 <> hex a <> hex b

    hex i | i < 10    = word8 (48 + i)
          | otherwise = word8 (65 + i - 10)

-- -- | Convert the string to lowercase.
-- lowercase = undefined

-- -- | Lowercase base 16 encoding.
-- hex = undefined

-- -- | Secure Hash Algorithm (SHA) cryptographic hash function.
-- hashSHA256 = undefined

-- -- | Calculate HMAC hash.
-- hmacSHA256 = undefined

-- -- | Remove any leading or trailing whitespace.
-- trim = undefined

-- sign :: Raw -> AWS Request
-- sign raw@Raw{..} = do
--     auth <- getAuth
--     reg  <- region rqService
--     time <- liftIO getCurrentTime

--     let sig = svcSigner rqService
--         hs  = hHost (endpoint rqService reg) : rqHeaders

--     return $! sig (raw { rqHeaders = hs }) auth reg time

-- common :: Raw -> Region -> Common
-- common Raw{..} reg = Common
--     { _service = svcName rqService
--     , _version = svcVersion rqService
--     , _host    = endpoint rqService reg
--     , _query   = sort rqQuery
--     }

-- data Common = Common
--     { _service :: !ByteString
--     , _version :: !ByteString
--     , _host    :: !ByteString
--     , _query   :: [(ByteString, Maybe ByteString)]
--     }


--     return . svcSigner $ Signee
--         { sigAccess  = Text.encodeUtf8 authAccessKeyId
--         , sigSecret  = Text.encodeUtf8 authSecretAccessKey
--         , sigToken   = Text.encodeUtf8 <$> authSecurityToken
--         , sigTime    = time
--         , sigRegion  = reg
--         , sigService = svcName
--         , sigVersion = svcVersion
--         , sigMethod  = BS.pack $ show rawMethod
--         , sigHost    = host
--         , sigPath    = Text.encodeUtf8 rawPath
--         , sigQuery   = HTTP.queryTextToQuery $ sort rawQuery
--         , sigHeaders = hHost host : rawHeaders
--         , sigBody    = rawBody
--         }
--   where
--     Service{..} = rawService


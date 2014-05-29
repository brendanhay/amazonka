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

import qualified Crypto.Hash.SHA1        as SHA1
import qualified Crypto.Hash.SHA256      as SHA256
import qualified Crypto.MAC.HMAC         as HMAC
import           Data.ByteString         (ByteString)
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Char8   as BS
import           Data.Char
import           Data.Monoid
import           Data.Time
import           Network.AWS.Data
import           Network.AWS.Types

data family Meta v :: *

data Signed v = Signed (Meta v) Host (Request ())

class SigningAlgorithm v where
    finalise :: Service a v
             -> Request a
             -> Auth
             -> Region
             -> UTCTime
             -> Signed v

sign :: (AWSRequest a, AWSService (Sv a), SigningAlgorithm (Sg (Sv a)))
     => a
     -> Auth
     -> Region
     -> UTCTime
     -> Signed (Sg (Sv a))
sign = finalise service . request

hmacSHA1 :: ByteString -> ByteString -> ByteString
hmacSHA1 key msg = HMAC.hmac SHA1.hash 64 key msg

hmacSHA256 :: ByteString -> ByteString -> ByteString
hmacSHA256 key msg = HMAC.hmac SHA256.hash 64 key msg

encodeURI :: Bool -> ByteString -> Builder
encodeURI p = BS.foldr (mappend . enc) mempty
  where
    enc ' '          = "%20"
    enc c@'/'
        | p          = "%2F"
        | otherwise  = build c
    enc c
        | reserved c = build c
    enc c            = char2hex c

    reserved c =
           isAsciiUpper c
        || isAsciiLower c
        || isDigit c
        || c `elem` "-_.~"

    char2hex c =
        let (a, b) = fromEnum c `divMod` 16
         in build ['%', hex a, hex b]

    hex i | i < 10    = toEnum (48 + i)
          | otherwise = toEnum (65 + i - 10)

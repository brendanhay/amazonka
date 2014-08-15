{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Data.Body
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Body where
    -- (
    -- -- * Types
    --   BodySource (..)
    -- , Body
    -- , clientBody
    -- , payloadHash

    -- -- * Classes
    -- , ToBody (..)
    -- ) where

import qualified           Crypto.Hash.SHA256 as SHA256
import           Data.Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base16     as Base16
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.String
import           Network.HTTP.Client

data RsBody where
    RsBody :: Monad m => m ByteString -> RsBody

instance Show RsBody where
    show = const "RsBody <body>"

data RqBody = RqBody
    { _bdyHash :: ByteString
    , _bdyBody :: RequestBody
    }

instance Show RqBody where
    show (RqBody h _) = "RqBody " ++ show h ++ " <body>"

instance IsString RqBody where
    fromString = toBody . LBS8.pack

class ToBody a where
    toBody :: a -> RqBody
    toBody = const (RqBody (base16SHA256 "") (RequestBodyLBS ""))

instance ToBody RqBody where
    toBody = id

instance ToBody LBS.ByteString where
    toBody lbs = RqBody (base16SHA256 lbs) (RequestBodyLBS lbs)

instance ToBody ByteString where
    toBody = toBody . LBS.fromStrict

instance ToBody Value where
    toBody = toBody . encode

base16SHA256 :: LBS8.ByteString -> ByteString
base16SHA256 = Base16.encode . SHA256.hashlazy
